---
title: Manual Control Flow Guard in C
layout: post
date: 2017-01-21T22:44:15Z
tags: [c, linux, netsec]
uuid: f185405a-3e30-3612-7a21-6d4ec450519d
---

Recent versions of Windows have a new exploit mitigation feature
called [*Control Flow Guard*][pdf] (CFG). Before an indirect function
call — e.g. function pointers and virtual functions — the target
address checked against a table of valid call addresses. If the
address isn't the entry point of a known function, then the program is
aborted.

If an application has a buffer overflow vulnerability, an attacker may
use it to overwrite a function pointer and, by the call through that
pointer, control the execution flow of the program. This is one way to
initiate a [*Return Oriented Programming*][rop] (ROP) attack, where
the attacker constructs [a chain of *gadget* addresses][chain] — a
gadget being a couple of instructions followed by a return
instruction, all in the original program — using the indirect call as
the starting point. The execution then flows from gadget to gadget so
that the program does what the attacker wants it to do, all without
the attacker supplying any code.

The two most widely practiced ROP attack mitigation techniques today
are *Address Space Layout Randomization* (ASLR) and *stack
protectors*. The former randomizes the base address of executable
images (programs, shared libraries) so that process memory layout is
unpredictable to the attacker. The addresses in the ROP attack chain
depend on the run-time memory layout, so the attacker must also find
and exploit an [information leak][pk] to bypass ASLR.

For stack protectors, the compiler allocates a *canary* on the stack
above other stack allocations and sets the canary to a per-thread
random value. If a buffer overflows to overwrite the function return
pointer, the canary value will also be overwritten. Before the
function returns by the return pointer, it checks the canary. If the
canary doesn't match the known value, the program is aborted.

![](/img/cfg/canary.svg)

CFG works similarly — performing a check prior to passing control to
the address in a pointer — except that instead of checking a canary,
it checks the target address itself. This is a lot more sophisticated,
and, unlike a stack canary, essentially requires coordination by the
platform. The check must be informed on all valid call targets,
whether from the main program or from shared libraries.

While not (yet?) widely deployed, a worthy mention is [Clang's
SafeStack][ss]. Each thread gets *two* stacks: a "safe stack" for
return pointers and other safely-accessed values, and an "unsafe
stack" for buffers and such. Buffer overflows will corrupt other
buffers but will not overwrite return pointers, limiting the effect of
their damage.

### An exploit example

Consider this trivial C program, `demo.c`:

~~~c
int
main(void)
{
    char name[8];
    gets(name);
    printf("Hello, %s.\n", name);
    return 0;
}
~~~

It reads a name into a buffer and prints it back out with a greeting.
While trivial, it's far from innocent. That naive call to `gets()`
doesn't check the bounds of the buffer, introducing an exploitable
buffer overflow. It's so obvious that both the compiler and linker
will yell about it.

For simplicity, suppose the program also contains a dangerous
function.

~~~c
void
self_destruct(void)
{
    puts("**** GO BOOM! ****");
}
~~~

The attacker can use the buffer overflow to call this dangerous
function.

To make this attack simpler for the sake of the article, assume the
program isn't using ASLR (e.g. without `-fpie`/`-pie`, or with
`-fno-pie`/`-no-pie`). For this particular example, I'll also
explicitly disable buffer overflow protections (e.g. `_FORTIFY_SOURCE`
and stack protectors).

    $ gcc -Os -fno-pie -D_FORTIFY_SOURCE=0 -fno-stack-protector \
          -o demo demo.c

First, find the address of `self_destruct()`.

    $ readelf -a demo | grep self_destruct
    46: 00000000004005c5  10 FUNC  GLOBAL DEFAULT 13 self_destruct

This is on x86-64, so it's a 64-bit address. The size of the `name`
buffer is 8 bytes, and peeking at the assembly I see an extra 8 bytes
allocated above, so there's 16 bytes to fill, then 8 bytes to
overwrite the return pointer with the address of `self_destruct`.

    $ echo -ne 'xxxxxxxxyyyyyyyy\xc5\x05\x40\x00\x00\x00\x00\x00' > boom
    $ ./demo < boom
    Hello, xxxxxxxxyyyyyyyy?@.
    **** GO BOOM! ****
    Segmentation fault

With this input I've successfully exploited the buffer overflow to
divert control to `self_destruct()`. When `main` tries to return into
libc, it instead jumps to the dangerous function, and then crashes
when that function tries to return — though, presumably, the system
would have self-destructed already. Turning on the stack protector
stops this exploit.

    $ gcc -Os -fno-pie -D_FORTIFY_SOURCE=0 -fstack-protector \
          -o demo demo.c
    $ ./demo < boom
    Hello, xxxxxxxxaaaaaaaa?@.
    *** stack smashing detected ***: ./demo terminated
    ======= Backtrace: =========
    ... lots of backtrace stuff ...

The stack protector successfully blocks the exploit. To get around
this, I'd have to either guess the canary value or discover an
information leak that reveals it.

The stack protector transformed the program into something that looks
like the following:

~~~c
int
main(void)
{
    long __canary = __get_thread_canary();
    char name[8];
    gets(name);
    printf("Hello, %s.\n", name);
    if (__canary != __get_thread_canary())
        abort();
    return 0;
}
~~~

However, it's not actually possible to implement the stack protector
within C. Buffer overflows are undefined behavior, and a canary is
only affected by a buffer overflow, allowing the compiler to optimize
it away.

### Function pointers and virtual functions

After the attacker successfully self-destructed the last computer,
upper management has mandated password checks before all
self-destruction procedures. Here's what it looks like now:

~~~c
void
self_destruct(char *password)
{
    if (strcmp(password, "12345") == 0)
        puts("**** GO BOOM! ****");
}
~~~

The password is hardcoded, and it's the kind of thing an idiot would
have on his luggage, but assume it's actually unknown to the attacker.
Especially since, as I'll show shortly, it won't matter. Upper
management has also mandated stack protectors, so assume that's
enabled from here on.

Additionally, the program has evolved a bit, and now [uses a function
pointer for polymorphism][oop].

~~~c
struct greeter {
    char name[8];
    void (*greet)(struct greeter *);
};

void
greet_hello(struct greeter *g)
{
    printf("Hello, %s.\n", g->name);
}

void
greet_aloha(struct greeter *g)
{
    printf("Aloha, %s.\n", g->name);
}
~~~

There's now a greeter object and the function pointer makes its
behavior polymorphic. Think of it as a hand-coded virtual function for
C. Here's the new (contrived) `main`:

~~~c
int
main(void)
{
    struct greeter greeter = {.greet = greet_hello};
    gets(greeter.name);
    greeter.greet(&greeter);
    return 0;
}
~~~

(In a real program, something else provides `greeter` and picks its
own function pointer for `greet`.)

Rather than overwriting the return pointer, the attacker has the
opportunity to overwrite the function pointer on the struct. Let's
reconstruct the exploit like before.

    $ readelf -a demo | grep self_destruct
    54: 00000000004006a5  10 FUNC  GLOBAL DEFAULT  13 self_destruct

We don't know the password, but we *do* know (from peeking at the
disassembly) that the password check is 16 bytes. The attack should
instead jump 16 bytes into the function, skipping over the check
(0x4006a5 + 16 = 0x4006b5).

    $ echo -ne 'xxxxxxxx\xb5\x06\x40\x00\x00\x00\x00\x00' > boom
    $ ./demo < boom
    **** GO BOOM! ****

Neither the stack protector nor the password were of any help. The
stack protector only protects the *return* pointer, not the function
pointer on the struct.

**This is where the Control Flow Guard comes into play.** With CFG
enabled, the compiler inserts a check before calling the `greet()`
function pointer. It must point to the beginning of a known function,
otherwise it will abort just like the stack protector. Since the
middle of `self_destruct()` isn't the *beginning* of a function, it
would abort if this exploit is attempted.

However, I'm on Linux and there's no CFG on Linux (yet?). So I'll
implement it myself, with manual checks.

### Function address bitmap

As described in the PDF linked at the top of this article, CFG on
Windows is implemented using a bitmap. Each bit in the bitmap
represents 8 bytes of memory. If those 8 bytes contains the beginning
of a function, the bit will be set to one. Checking a pointer means
checking its associated bit in the bitmap.

For my CFG, I've decided to keep the same 8-byte resolution: the
bottom three bits of the target address will be dropped. The next 24
bits will be used to index into the bitmap. All other bits in the
pointer will be ignored. A 24-bit bit index means the bitmap will only
be 2MB.

These 24 bits is perfectly sufficient for 32-bit systems, but it means
on 64-bit systems there may be false positives: some addresses will
not represent the start of a function, but will have their bit set
to 1. This is acceptable, especially because only functions known to
be targets of indirect calls will be registered in the table, reducing
the false positive rate.

Note: Relying on [the bits of a pointer cast to an integer is
unspecified][hash] and isn't portable, but this implementation will
work fine anywhere I would care to use it.

Here are the CFG parameters. I've made them macros so that they can
easily be tuned at compile-time. The `cfg_bits` is the integer type
backing the bitmap array. The `CFG_RESOLUTION` is the number of bits
dropped, so "3" is a granularity of 8 bytes.

~~~c
typedef unsigned long cfg_bits;
#define CFG_RESOLUTION  3
#define CFG_BITS        24
~~~

Given a function pointer `f`, this macro extracts the bitmap index.

~~~c
#define CFG_INDEX(f) \
    (((uintptr_t)f >> CFG_RESOLUTION) & ((1UL << CFG_BITS) - 1))
~~~

The CFG bitmap is just an array of integers. Zero it to initialize.

~~~c
struct cfg {
    cfg_bits bitmap[(1UL << CFG_BITS) / (sizeof(cfg_bits) * CHAR_BIT)];
};
~~~

Functions are manually registered in the bitmap using
`cfg_register()`.

~~~c
void
cfg_register(struct cfg *cfg, void *f)
{
    unsigned long i = CFG_INDEX(f);
    size_t z = sizeof(cfg_bits) * CHAR_BIT;
    cfg->bitmap[i / z] |= 1UL << (i % z);
}
~~~

Because functions are registered at run-time, it's fully compatible
with ASLR. If ASLR is enabled, the bitmap will be a little different
each run. On the same note, it may be worth XORing each bitmap element
with a random, run-time value — along the same lines as the stack
canary value — to make it harder for an attacker to manipulate the
bitmap should he get the ability to overwrite it by a vulnerability.
Alternatively the bitmap could be switched to read-only (e.g.
`mprotect()`) once everything is registered.

And finally, the check function, used immediately before indirect
calls. It ensures `f` was previously passed to `cfg_register()`
(except for false positives, as discussed). Since it will be invoked
often, it needs to be fast and simple.

~~~c
void
cfg_check(struct cfg *cfg, void *f)
{
    unsigned long i = CFG_INDEX(f);
    size_t z = sizeof(cfg_bits) * CHAR_BIT;
    if (!((cfg->bitmap[i / z] >> (i % z)) & 1))
        abort();
}
~~~

And that's it! Now augment `main` to make use of it:

~~~c
struct cfg cfg;

int
main(void)
{
    cfg_register(&cfg, self_destruct);  // to prove this works
    cfg_register(&cfg, greet_hello);
    cfg_register(&cfg, greet_aloha);

    struct greeter greeter = {.greet = greet_hello};
    gets(greeter.name);
    cfg_check(&cfg, greeter.greet);
    greeter.greet(&greeter);
    return 0;
}
~~~

And now attempting the exploit:

    $ ./demo < boom
    Aborted

Normally `self_destruct()` wouldn't be registered since it's not a
legitimate target of an indirect call, but the exploit *still* didn't
work because it called into the middle of `self_destruct()`, which
isn't a valid address in the bitmap. The check aborts the program
before it can be exploited.

In a real application I would have a [global `cfg` bitmap][rel] for
the whole program, and define `cfg_check()` in a header as an `inline`
function.

Despite being possible implement in straight C without the help of the
toolchain, it would be far less cumbersome and error-prone to let the
compiler and platform handle Control Flow Guard. That's the right
place to implement it.

*Update*: Ted Unangst pointed out [OpenBSD performing a similar
check][tedu] in its mbuf library. Instead of a bitmap, the function
pointer is replaced with an index into an array of registered function
pointers. That approach is cleaner, more efficient, completely
portable, and has no false positives.


[pdf]: http://sjc1-te-ftp.trendmicro.com/assets/wp/exploring-control-flow-guard-in-windows10.pdf
[rel]: /blog/2016/12/23/
[hash]: /blog/2016/05/30/
[rop]: https://skeeto.s3.amazonaws.com/share/p15-coffman.pdf
[chain]: https://github.com/JonathanSalwan/ROPgadget
[pk]: https://github.com/torvalds/linux/blob/4c9eff7af69c61749b9eb09141f18f35edbf2210/Documentation/sysctl/kernel.txt#L373
[ss]: http://clang.llvm.org/docs/SafeStack.html
[oop]: /blog/2014/10/21/
[tedu]: http://www.tedunangst.com/inks/l/849
