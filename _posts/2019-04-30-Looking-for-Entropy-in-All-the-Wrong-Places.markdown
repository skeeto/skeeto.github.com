---
title: Looking for Entropy in All the Wrong Places
layout: post
date: 2019-04-30T22:50:09Z
tags: [c, lua, crypto]
uuid: 67da1a72-1103-4e12-a646-8a57443619eb
---

Imagine we're writing a C program and we need some random numbers. Maybe
it's for a game, or for a Monte Carlo simulation, or for cryptography.
The standard library has a `rand()` function for some of these purposes.

```c
int r = rand();
```

There are some problems with this. Typically the implementation is a
rather poor PRNG, and [we can do much better][prng]. It's a poor choice
for Monte Carlo simulations, and outright dangerous for cryptography.
Furthermore, it's usually a dynamic function call, which [has a high
overhead][dyn] compared to how little the function actually does. In
glibc, it's also synchronized, adding even more overhead.

But, more importantly, this function returns the same sequences of
values each time the program runs. If we want different numbers each
time the program runs, it needs to be seeded — but seeded with *what*?
Regardless of what PRNG we ultimately use, we need inputs unique to this
particular execution.

### The right places

On any modern unix-like system, the classical approach is to open
`/dev/urandom` and read some bytes. It's not part of POSIX but it is a
*de facto* standard. These random bits are seeded from the physical
world by the operating system, making them highly unpredictable and
uncorrelated. They're are suitable for keying a CSPRNG and, from
there, [generating all the secure random bits you will ever
need][entropy] (perhaps with [fast-key-erasure][fke]). Why not
`/dev/random`? Because on Linux [it's pointlessly
superstitious][myths], which has basically ruined that path for
everyone.

```c
/* Returns zero on failure. */
int
getbits(void *buf, size_t len)
{
    int result = 0;
    FILE *f = fopen("/dev/urandom", "rb");
    if (f) {
        result = fread(buf, len, 1, f);
        fclose(f);
    }
    return result;
}

int
main(void)
{
    unsigned seed;
    if (getbits(&seed, sizeof(seed))) {
        srand(seed);
    } else {
        die();
    }

    /* ... */
}
```

Note how there are two different places `getbits()` could fail, with
multiple potential causes.

* It could fail to open the file. Perhaps the program isn't running on a
  modern unix-like system. Perhaps it's running in a chroot and
  `/dev/urandom` wasn't created. Perhaps there are too many file
  descriptors already open. Perhaps there isn't enough memory available
  to open a file. Perhaps the file permissions disallow it or it's
  blocked by Mandatory Access Control (MAC).

* It could fail to read the file. This essentially can't happen unless
  the system is severely misconfigured, in which case a successful
  read would be suspect anyway. In this case it's probably still a
  good idea to check the result.

The need for creating a file descriptor a serious issue for libraries.
Libraries that quietly create and close file descriptors can interfere
with the main program, especially if its asynchronous. The main program
might rely on file descriptors being consecutive, predictable, or
monotonic ([example][sd]). File descriptors are also a limited resource,
so it may exhaust a file descriptor slot needed for the main program.
For a network service, a remote attacker could perhaps open enough
sockets to deny a file descriptor to `getbits()`, blocking the program
from gathering entropy.

`/dev/urandom` is simple, but it's not an ideal API.

#### getentropy(2)

Wouldn't it be nicer if our program could just directly ask the
operating system to fill a buffer with random bits? That's what the
OpenBSD folks thought, so they introduced a [`getentropy(2)`][bsdge]
system call. When called correctly *it cannot fail*!

```c
int getentropy(void *buf, size_t buflen);
```

Other operating systems followed suit, [including Linux][road], though
on Linux `getentropy(2)` is a library function implemented using
[`getrandom(2)`][gr], the actual system call. It's been in the Linux
kernel since version 3.17 (October 2014), but the libc wrapper didn't
appear in glibc until version 2.25 (February 2017). So as of this
writing, there are still many systems where it's still not practical
to use even if their kernel is new enough.

For now on Linux you may still want to check, and have a strategy in
place, for an `ENOSYS` result. Some systems are still running kernels
that are 5 years old, or older.

OpenBSD also has another trick up its trick-filled sleeves: the
[`.openbsd.randomdata`][rd] section. Just as the `.bss` section is
filled with zeros, the `.openbsd.randomdata` section is filled with
securely-generated random bits. You could put your PRNG state in this
section and it will be seeded as part of loading the program. Cool!

#### RtlGenRandom()

Windows doesn't have `/dev/urandom`. Instead it has:

* `CryptGenRandom()`
* `CryptAcquireContext()`
* `CryptReleaseContext()`

Though in typical Win32 fashion, the API is ugly, overly-complicated,
and has multiple possible failure points. It's essentially impossible
to use without referencing documentation. Ugh.

However, [Windows 98 and later][blast] has [`RtlGenRandom()`][rtl],
which has a much more reasonable interface. Looks an awful lot like
`getentropy(2)`, eh?

```c
BOOLEAN RtlGenRandom(
  PVOID RandomBuffer,
  ULONG RandomBufferLength
);
```

The problem is that it's not quite an official API, and no promises
are made about it. In practice, far too much software now depends on
it that the API is unlikely to ever break. Despite the prototype
above, this function is *actually* named `SystemFunction036()`, and
you have to supply your own prototype. Here's my little drop-in
snippet that turns it nearly into `getentropy(2)`:

```c
#ifdef _WIN32
#  define WIN32_LEAN_AND_MEAN
#  include <windows.h>
#  pragma comment(lib, "advapi32.lib")
   BOOLEAN NTAPI SystemFunction036(PVOID, ULONG);
#  define getentropy(buf, len) (SystemFunction036(buf, len) ? 0 : -1)
#endif
```

It works in Wine, too, where, at least in my version, it reads from
`/dev/urandom`.

### The wrong places

That's all well and good, but suppose we're masochists. We want our
program to be [maximally portable][port] so we're sticking strictly to
functionality found in the standard C library. That means no
`getentropy(2)` and no `RtlGenRandom()`. We can still try to open
`/dev/urandom`, but it might fail, or it might not actually be useful,
so we'll want a backup.

The usual approach found in a thousand tutorials is `time(3)`:

```c
srand(time(NULL));
```

It would be better to [use an integer hash function][hash] to mix up the
result from `time(0)` before using it as a seed. Otherwise two programs
started close in time may have similar initial sequences.

```c
srand(triple32(time(NULL)));
```

The more pressing issue is that `time(3)` has a resolution of one
second. If the program is run twice inside of a second, they'll both
have the same sequence of numbers. It would be better to use a higher
resolution clock, but, **standard C doesn't provide a clock with greater
than one second resolution**. That normally requires calling into POSIX
or Win32.

So, we need to find some other sources of entropy unique to each
execution of the program.

#### Quick and dirty "string" hash function

Before we get into that, we need a way to mix these different sources
together. Here's a [small][min], 32-bit "string" hash function. The loop
is the same algorithm as Java's `hashCode()`, and I appended [my own
integer hash][hash] as a finalizer for much better diffusion.

```c
uint32_t
hash32s(const void *buf, size_t len, uint32_t h)
{
    const unsigned char *p = buf;
    for (size_t i = 0; i < len; i++)
        h = h * 31 + p[i];
    h ^= h >> 17;
    h *= UINT32_C(0xed5ad4bb);
    h ^= h >> 11;
    h *= UINT32_C(0xac4c1b51);
    h ^= h >> 15;
    h *= UINT32_C(0x31848bab);
    h ^= h >> 14;
    return h;
}
```

It accepts a starting hash value, which is essentially a "context" for
the digest that allows different inputs to be appended together. The
finalizer acts as an implicit "stop" symbol in between inputs.

I used fixed-width integers, but it could be written nearly as concisely
using only `unsigned long` and some masking to truncate to 32-bits. I
leave this as an exercise to the reader.

Some of the values to be mixed in will be pointers themselves. These
could instead be cast to integers and passed through an integer hash
function, but using string hash avoids [various caveats][ptr]. Besides,
one of the inputs will be a string, so we'll need this function anyway.

#### Randomized pointers (ASLR, random stack gap, etc.)

Attackers can use predictability to their advantage, so modern systems
use unpredictability to improve security. Memory addresses for various
objects and executable code are randomized since some attacks require
an attacker to know their addresses. We can skim entropy from these
pointers to seed our PRNG.

Address Space Layout Randomization (ASLR) is when executable code and
its associated data is loaded to a random offset by the loader. Code
designed for this is called Position Independent Code (PIC). This has
long been used when loading dynamic libraries so that all of the
libraries on a system don't have to coordinate with each other to
avoid overlapping.

To improve security, it has more recently been extended to programs
themselves. On both modern unix-like systems and Windows,
position-independent executables (PIE) are now the default.

To skim entropy from ASLR, we just need the address of one of our
functions. All the functions in our program will have the same relative
offset, so there's no reason to use more than one. An obvious choice is
`main()`:

```c
    uint32_t h = 0;  /* initial hash value */
    int (*mainptr)() = main;
    h = hash32s(&mainptr, sizeof(mainptr), h);
```

Notice I had to store the address of `main()` in a variable, and then
treat *the pointer itself* as a buffer for the hash function? It's not
hashing the machine code behind `main`, just its address. The symbol
`main` doesn't store an address, so it can't be given to the hash
function to represent its address. This is analogous to an array
versus a pointer.

On a typical x86-64 Linux system, and when this is a PIE, that's about
3 bytes worth of entropy. On 32-bit systems, virtual memory is so
tight that it's worth a lot less. We might want more entropy than
that, and we want to cover the case where the program isn't compiled
as a PIE.

On unix-like systems, programs are typically dynamically linked against
the C library, libc. Each shared object gets its own ASLR offset, so we
can skim more entropy from each shared object by picking a function or
variable from each. Let's do `malloc(3)` for libc ASLR:

```c
    void *(*mallocptr)() = malloc;
    h = hash32s(&mallocptr, sizeof(mallocptr), h);
```

Allocators themselves often randomize the addresses they return so that
data objects are stored at unpredictable addresses. In particular, glibc
uses different strategies for small (`brk(2)`) versus big (`mmap(2)`)
allocations. That's two different sources of entropy:

```c
    void *small = malloc(1);        /* 1 byte */
    h = hash32s(&small, sizeof(small), h);
    free(small);

    void *big = malloc(1UL << 20);  /* 1 MB */
    h = hash32s(&big, sizeof(big), h);
    free(big);
```

Finally the stack itself is often mapped at a random address, or at
least started with a random gap, so that local variable addresses are
also randomized.

```c
    void *ptr = &ptr;
    h = hash32s(&ptr, sizeof(ptr), h);
```

#### Time sources

We haven't used `time(3)` yet! Let's still do that, using the full
width of `time_t` this time around:

```c
    time_t t = time(0);
    h = hash32s(&t, sizeof(t), h);
```

We do have another time source to consider: `clock(3)`. It returns an
approximation of the processor time used by the program. There's a
tiny bit of noise and inconsistency between repeated calls. We can use
this to extract a little bit of entropy over many repeated calls.

Naively we might try to use it like this:

```c
    /* Note: don't use this */
    for (int i = 0; i < 1000; i++) {
        clock_t c = clock();
        h = hash32s(&c, sizeof(c), h);
    }
```

The problem is that the resolution for `clock()` is typically rough
enough that modern computers can execute multiple instructions between
ticks. On Windows, where `CLOCKS_PER_SEC` is low, that entire loop
will typically complete before the result from `clock()` increments
even once. With that arrangement we're hardly getting anything from
it! So here's a better version:

```c
    for (int i = 0; i < 1000; i++) {
        unsigned long counter = 0;
        clock_t start = clock();
        while (clock() == start)
            counter++;
        h = hash32s(&start, sizeof(start), h);
        h = hash32s(&counter, sizeof(counter), h);
    }
```

The counter makes the resolution of the clock no longer important. If
it's low resolution, then we'll get lots of noise from the counter. If
it's high resolution, then we get noise from the clock value itself.
Running the hash function an extra time between overall `clock(3)`
samples also helps with noise.

#### A legitimate use of tmpnam(3)

We've got one more source of entropy available: `tmpnam(3)`. This
function generates a unique, temporary file name. It's dangerous to
use as intended because it doesn't actually create the file. There's a
race between generating the name for the file and actually creating
it.

Fortunately we don't actually care about the name as a filename. We're
using this to sample entropy not directly available to us. In attempt to
get a unique name, the standard C library draws on its own sources of
entropy.

```c
    char buf[L_tmpnam] = {0};
    tmpnam(buf);
    h = hash32s(buf, sizeof(buf), h);
```

The rather unfortunately downside is that lots of modern systems produce
a *linker* warning when it sees `tmpnam(3)` being linked, even though in
this case it's completely harmless.

So what goes into a temporary filename? It depends on the
implementation.

##### glibc and musl

Both get a high resolution timestamp and generate the filename directly
from the timestamp (no hashing, etc.). Unfortunately glibc does a very
poor job of also mixing `getpid(2)` into the timestamp before using it,
and probably makes things worse by doing so.

On these platforms, this is is a way to sample a high resolution
timestamp without calling anything non-standard.

##### dietlibc

In the latest release as of this writing it uses `rand(3)`, which makes
this useless. It's also a bug since the C library isn't allowed to
affect the state of `rand(3)` outside of `rand(3)` and `srand(3)`. I
submitted a bug report and this has [since been fixed][fix].

In the next release it will use a generator seeded by the [ELF
`AT_RANDOM`][elfar] value if available, or ASLR otherwise. This makes
it moderately useful.

##### libiberty

Generated from `getpid(2)` alone, with a counter to handle multiple
calls. It's basically a way to sample the process ID without actually
calling `getpid(2)`.

##### BSD libc / Bionic (Android)

Actually gathers real entropy from the operating system (via
`arc4random(2)`), which means we're getting a lot of mileage out of this
one.

##### uclibc

Its implementation is obviously forked from glibc. However, it first
tries to read entropy from `/dev/urandom`, and only if that fails does
it fallback to glibc's original high resolution clock XOR `getpid(2)`
method (still not hashing it).

#### Finishing touches

Finally, still use `/dev/urandom` if it's available. This doesn't
require us to trust that the output is anything useful since it's just
being mixed into the other inputs.

```c
    char rnd[4];
    FILE *f = fopen("/dev/urandom", "rb");
    if (f) {
        if (fread(rnd, sizeof(rnd), 1, f))
            h = hash32s(rnd, sizeof(rnd), h);
        fclose(f);
    }
```

When we're all done gathering entropy, set the seed from the result.

```c
    srand(h);   /* or whatever you're seeding */
```

That's bound to find *some* entropy on just about any host. Though
definitely don't rely on the results for cryptography.

### Lua

I recently tackled this problem in Lua. It has a no-batteries-included
design, demanding very little of its host platform: nothing more than an
ANSI C implementation. Because of this, a Lua program has even fewer
options for gathering entropy than C. But it's still not impossible!

To further complicate things, Lua code is often run in a sandbox with
some features removed. For example, Lua has `os.time()` and `os.clock()`
wrapping the C equivalents, allowing for the same sorts of entropy
sampling. When run in a sandbox, `os` might not be available. Similarly,
`io` might not be available for accessing `/dev/urandom`.

Have you ever printed a table, though? Or a function? It evaluates to
a string containing the object's address.

    $ lua -e 'print(math)'
    table: 0x559577668a30
    $ lua -e 'print(math)'
    table: 0x55e4a3679a30

Since the raw pointer values are leaked to Lua, we can skim allocator
entropy like before. Here's the same hash function in Lua 5.3:

```lua
local function hash32s(buf, h)
    for i = 1, #buf do
        h = h * 31 + buf:byte(i)
    end
    h = h & 0xffffffff
    h = h ~ (h >> 17)
    h = h * 0xed5ad4bb
    h = h & 0xffffffff
    h = h ~ (h >> 11)
    h = h * 0xac4c1b51
    h = h & 0xffffffff
    h = h ~ (h >> 15)
    h = h * 0x31848bab
    h = h & 0xffffffff
    h = h ~ (h >> 14)
    return h
end
```

Now hash a bunch of pointers in the global environment:

```lua
local h = hash32s({}, 0)  -- hash a new table
for varname, value in pairs(_G) do
    h = hash32s(varname, h)
    h = hash32s(tostring(value), h)
    if type(value) == 'table' then
        for k, v in pairs(value) do
            h = hash32s(tostring(k), h)
            h = hash32s(tostring(v), h)
        end
    end
end

math.randomseed(h)
```

Unfortunately this doesn't actually work well on one platform I tested:
Cygwin. Cygwin has few security features, notably lacking ASLR, and
having a largely deterministic allocator.

### When to use it

In practice it's not really necessary to use these sorts of tricks of
gathering entropy from odd places. It's something that comes up more
in coding challenges and exercises than in real programs. I'm probably
already making platform-specific calls in programs substantial enough
to need it anyway.

On a few occasions I have thought about these things when debugging.
ASLR makes return pointers on the stack slightly randomized on each
run, which can change the behavior of some kinds of bugs. Allocator
and stack randomization does similar things to most of your pointers.
GDB tries to disable some of these features during debugging, but it
doesn't get everything.


[blast]: /blog/2018/04/13/
[bsdge]: https://man.openbsd.org/getentropy.2
[dyn]: /blog/2018/05/27/
[elfar]: https://lwn.net/Articles/301798/
[entropy]: https://blog.cr.yp.to/20140205-entropy.html
[fix]: https://github.com/ensc/dietlibc/commit/8c8df9579962dc7449fe1f3205fd19eec461aa23
[fke]: https://blog.cr.yp.to/20170723-random.html
[gr]: http://man7.org/linux/man-pages/man2/getrandom.2.html
[hash]: /blog/2018/07/31/
[min]: /blog/2018/06/10/
[myths]: https://www.2uo.de/myths-about-urandom/
[port]: /blog/2017/03/30/
[prng]: /blog/2017/09/21/
[ptr]: /blog/2016/05/30/
[rd]: https://github.com/openbsd/src/blob/master/libexec/ld.so/SPECS.randomdata
[road]: https://lwn.net/Articles/711013/
[rtl]: https://docs.microsoft.com/en-us/windows/desktop/api/ntsecapi/nf-ntsecapi-rtlgenrandom
[sd]: https://www.freedesktop.org/software/systemd/man/sd_listen_fds.html
