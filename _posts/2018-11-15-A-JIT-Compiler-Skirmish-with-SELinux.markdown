---
title: A JIT Compiler Skirmish with SELinux
layout: post
date: 2018-11-15T18:57:47Z
tags: [c, linux, posix]
uuid: d4fa35ad-05c3-3b86-1083-d533dfacfb15
---

This is a debugging war story.

Once upon a time I wrote a fancy data conversion utility. The input
was a complex binary format defined by a data dictionary supplied at
run time by the user alongside the input data. Since the converter was
typically used to process massive quantities of input, and the nature
of that input wasn't known until run time, I wrote [an x86-64 JIT
compiler][jit] to speed it up. The converter generated a fast, native
binary parser in memory according to the data dictionary
specification. Processing data now took much less time and everyone
rejoiced.

Then along came SELinux, Sheriff of Pedantry. Not liking all the
shenanigans with page protections, SELinux huffed and puffed and made
`mprotect(2)` return `EACCES` ("Permission denied"). Believing I was
following all the rules and so this would never happen, I foolishly
did not check the result and the converter was now crashing for its
users. What made SELinux so unhappy, and could this somehow be
resolved?

### Allocating memory

Before going further, let's back up and review how this works. Suppose I
want to generate code at run time and execute it. In the old days this
was as simple as writing some machine code into a buffer and jumping to
that buffer — e.g. by converting the buffer to a function pointer and
calling it.

```c
typedef int (*jit_func)(void);

/* NOTE: This doesn't work anymore! */
jit_func
jit_compile(int retval)
{
    unsigned char *buf = malloc(6);
    if (buf) {
        /* mov eax, retval */
        buf[0] = 0xb8;
        buf[1] = retval >>  0;
        buf[2] = retval >>  8;
        buf[3] = retval >> 16;
        buf[4] = retval >> 24;
        /* ret */
        buf[5] = 0xc3;
    }
    return (jit_func)buf;
}

int
main(void)
{
    jit_func f = jit_compile(1001);
    printf("f() = %d\n", f());
    free(f);
}
```

This situation was far too easy for malicious actors to abuse. An
attacker could supply instructions of their own choosing — i.e. *shell
code* — as input and exploit a buffer overflow vulnerability to execute
the input buffer. These exploits were trivial to craft.

Modern systems have hardware checks to prevent this from happening.
Memory containing instructions must have their execute protection bit
set before those instructions can be executed. This is useful both for
making attackers work harder and for catching bugs in programs — no more
executing data by accident.

This is further complicated by the fact that memory protections have
page granularity. You can't adjust the protections for a 6-byte
buffer. You do it for the entire surrounding page — typically 4kB, but
sometimes as large as 2MB. This requires replacing that `malloc(3)`
with a more careful allocation strategy. There are a few ways to go
about this.

#### Anonymous memory mapping

The most common and most sensible is to create an anonymous memory
mapping: a file memory map that's not actually backed by a file. The
`mmap(2)` function has a flag specifically for this purpose:
`MAP_ANONYMOUS`.

```c
#include <sys/mman.h>

void *
anon_alloc(size_t len)
{
    int prot = PROT_READ | PROT_WRITE;
    int flags = MAP_ANONYMOUS | MAP_PRIVATE;
    void *p = mmap(0, len, prot, flags, -1, 0);
    return p != MAP_FAILED ? p : 0;
}

void
anon_free(void *p, size_t len)
{
    munmap(p, len);
}
```

Unfortunately, `MAP_ANONYMOUS` not part of POSIX. If you're being super
strict with your includes — [as I tend to be][port] — this flag won't be
defined, even on systems where it's supported.

```c
#define _POSIX_C_SOURCE 200112L
#include <sys/mman.h>
// MAP_ANONYMOUS undefined!
```

To get the flag, you must use the `_BSD_SOURCE`, or, more recently,
the `_DEFAULT_SOURCE` feature test macro to explicitly enable that
feature.

```c
#define _POSIX_C_SOURCE 200112L
#define _DEFAULT_SOURCE /* for MAP_ANONYMOUS */
#include <sys/mman.h>
```

The POSIX way to do this is to instead map `/dev/zero`. **So, wanting to
be Mr. Portable, this is what I did in my tool.** Take careful note of
this.

```c
#define _POSIX_C_SOURCE 200112L
#include <fcntl.h>
#include <unistd.h>
#include <sys/mman.h>

void *
anon_alloc(size_t len)
{
    int fd = open("/dev/zero", O_RDWR);
    if (fd == -1)
        return 0;
    int prot = PROT_READ | PROT_WRITE;
    int flags = MAP_PRIVATE;
    void *p = mmap(0, len, prot, flags, fd, 0);
    close(fd);
    return p != MAP_FAILED ? p : 0;
}
```

#### Aligned allocation

Another, less common (and less portable) strategy is to lean on the
existing C memory allocator, being careful to allocate on page
boundaries so that the page protections don't affect other allocations.
The classic allocation functions, like `malloc(3)`, don't allow for this
kind of control. However, there are a couple of aligned allocation
alternatives.

The first is `posix_memalign(3)`:

```c
int posix_memalign(void **ptr, size_t alignment, size_t size);
```

By choosing page alignment and a size that's a multiple of the page
size, it's guaranteed to return whole pages. When done, pages are freed
with `free(3)`. Though, unlike unmapping, the original page protections
must first be restored since those pages may be reused.

```c
#define _POSIX_C_SOURCE 200112L
#include <stdlib.h>
#include <unistd.h>

void *
anon_alloc(size_t len)
{
    void *p;
    long pagesize = sysconf(_SC_PAGE_SIZE); // TODO: cache this
    size_t roundup = (len + pagesize - 1) / pagesize * pagesize;
    return posix_memalign(&p, pagesize, roundup) ? 0 : p;
}
```

If you're using C11, there's also `aligned_alloc(3)`. This is the most
uncommon of all since most C programmers refuse to switch to a new
standard until it's at least old enough to drive a car.

### Changing page protections

So we've allocated our memory, but it's not going to start in an
executable state. Why? Because a [W^X][wx] (“write xor execute”)
policy is becoming increasingly common. Attempting to set both write
and execute protections at the same time may be denied. (In fact,
there's an SELinux policy for this.)

As a JIT compiler, we need to write to a page *and* execute it. Again,
there are two strategies. The complicated strategy is to [map the same
memory at two different places][map], one with the execute protection,
one with the write protection. This allows the page to be modified as
it's being executed without violating W^X.

The simpler and more secure strategy is to write the machine
instructions, then swap the page over to executable using `mprotect(2)`
once it's ready. This is what I was doing in my tool.

```c
unsigned char *buf = anon_alloc(len);
/* ... write instructions into the buffer ... */
mprotect(buf, len, PROT_EXEC);
jit_func func = (jit_func)buf;
func();
```

At a high level, That's pretty close to what I was actually doing. That
includes neglecting to check the result of `mprotect(2)`. This worked
fine and dandy for several years, when suddenly (shown here in the style
[of strace][strace]):

    mprotect(ptr, len, PROT_EXEC) = -1 EACCES (Permission denied)

Then the program would crash trying to execute the buffer. Suddenly it
wasn't allowed to make this buffer executable. My program hadn't
changed. What *had* changed was the SELinux security policy on this
particular system.

### Asking for help

The problem is that I don't administer this (Red Hat) system. I can't
access the logs and I didn't set the policy. I don't have any insight
on *why* this call was suddenly being denied. To make this more
challenging, the folks that manage this system didn't have the
necessary knowledge to help with this either.

So to figure this out, I need to treat it like a black box and probe
at system calls until I can figure out just what SELinux policy I'm up
against. I only have practical experience administrating Debian
systems (and its derivatives like Ubuntu), which means I've hardly
ever had to deal with SELinux. I'm flying fairly blind here.

Since my real application is large and complicated, I code up a
minimal example, around a dozen lines of code: allocate a single page
of memory, write a single return (`ret`) instruction into it, set it
as executable, and call it. The program checks for errors, and I can
run it under strace if that's not insightful enough. This program is
also something simple I could provide to the system administrators,
since they were willing to turn some of the knobs to help narrow down
the problem.

However, **here's where I made a major mistake**. Assuming the problem
was solely in `mprotect(2)`, and wanting to keep this as absolutely
simple as possible, I used `posix_memalign(3)` to allocate that page. I
saw the same `EACCES` as before, and assumed I was demonstrating the
same problem. Take note of this, too.

### Finding a resolution

Eventually I'd need to figure out what policy was blocking my JIT
compiler, then see if there was an alternative route. The system
loader still worked after all, and I could plainly see that with
strace. So it wasn't a blanket policy that completely blocked the
execute protection. Perhaps the loader was given an exception?

However, the very first order of business was to actually check the
result from `mprotect(2)` and do something more graceful rather than
crash. In my case, that meant falling back to executing a byte-code
virtual machine. I added the check, and now the program ran slower
instead of crashing.

The program runs on both Linux and Windows, and the allocation and
page protection management is abstracted. On Windows it uses
`VirtualAlloc()` and `VirtualProtect()` instead of `mmap(2)` and
`mprotect(2)`. Neither implementation checked that the protection
change succeeded, so I fixed the Windows implementation while I was at
it.

Thanks to Mingw-w64, I actually do most of my [Windows
development][win] on Linux. And, thanks to [Wine][wine], I mean
everything, including running and debugging. Calling
`VirtualProtect()` in Wine would ultimately call `mprotect(2)` in the
background, which I expected would be denied. So running the Windows
version with Wine under this SELinux policy would be the perfect test.
Right?

**Except that `mprotect(2)` succeeded under Wine!** The Windows version
of my JIT compiler was working just fine on Linux. Huh?

This system doesn't have Wine installed. I had built [and packaged it
myself][qpkg]. This Wine build definitely has no SELinux exceptions.
Not only did the Wine loader work correctly, it can change page
protections in ways my own Linux programs could not. What's different?

Debugging this with all these layers is starting to look silly, but
this is exactly why doing Windows development on Linux is so useful. I
run my program under Wine under strace:

    $ strace wine ./mytool.exe

I study the system calls around `mprotect(2)`. Perhaps there's some
stricter alignment issue? No. Perhaps I need to include `PROT_READ`?
No. The only difference I can find is they're using the
`MAP_ANONYMOUS` flag. So, armed with this knowledge, **I modify my
minimal example to allocate 1024 pages instead of just one, and
suddenly it works correctly**. I was most of the way to figuring this
all out.

### Inside glibc allocation

Why did increasing the allocation size change anything? This is a
typical Linux system, so my program is linked against the GNU C
library, glibc. This library allocates memory from two places
depending on the allocation size.

For small allocations, glibc uses `brk(2)` to extend the executable
image — i.e. to extend the `.bss` section. These resources are not
returned to the operating system after they're freed with `free(3)`.
They're reused.

For large allocations, glibc uses `mmap(2)` to create a new, anonymous
mapping for that allocation. When freed with `free(3)`, that memory is
unmapped and its resources are returned to the operating system.

By increasing the allocation size, it became a "large" allocation and
was backed by an anonymous mapping. Even though I didn't use `mmap(2)`,
to the operating system this would be indistinguishable to what Wine was
doing (and succeeding at).

Consider this little example program:

```c
int
main(void)
{
    printf("%p\n", malloc(1));
    printf("%p\n", malloc(1024 * 1024));
}
```

When *not* compiled as a Position Independent Executable (PIE), here's
what the output looks like. The first pointer is near where the program
was loaded, low in memory. The second pointer is a randomly selected
address high in memory.

    0x1077010
    0x7fa9b998e010

And if you run it under strace, you'll see that the first allocation
comes from `brk(2)` and the second comes from `mmap(2)`.

### Two SELinux policies

With a little bit of research, I found the [two SELinux policies][mem]
at play here. In my minimal example, I was blocked by `allow_execheap`.

    /selinux/booleans/allow_execheap

This prohibits programs from setting the execute protection on any
"heap" page.

> The POSIX specification does not permit it, but the Linux
> implementation of `mprotect` allows changing the access protection of
> memory on the heap (e.g., allocated using `malloc`). This error
> indicates that heap memory was supposed to be made executable. Doing
> this is really a bad idea. If anonymous, executable memory is needed
> it should be allocated using `mmap` which is the only portable
> mechanism.

Obviously this is pretty loose since I was still able to do it with
`posix_memalign(3)`, which, technically speaking, allocates from the
heap. So this policy applies to pages mapped by `brk(2)`.

The second policy was `allow_execmod`.

    /selinux/booleans/allow_execmod

> The program mapped from a file with `mmap` and the `MAP_PRIVATE` flag
> and write permission. Then the memory region has been written to,
> resulting in copy-on-write (COW) of the affected page(s). This memory
> region is then made executable [...]. The `mprotect` call will fail
> with `EACCES` in this case.

I don't understand what purpose this policy serves, but this is what
was causing my original problem. Pages mapped to `/dev/zero` are not
*actually* considered anonymous by Linux, at least as far as this
policy is concerned. I think this is a mistake, and that mapping the
special `/dev/zero` device should result in effectively anonymous
pages.

From this I learned a little lesson about baking assumptions — that
`mprotect(2)` was solely at fault — into my minimal debugging examples.
And the fix was ultimately easy: I just had to suck it up and use the
slightly less pure `MAP_ANONYMOUS` flag.


[jit]: /blog/2015/03/19/
[map]: /blog/2016/04/10/
[mem]: https://akkadia.org/drepper/selinux-mem.html
[port]: /blog/2017/03/30/
[qpkg]: https://nullprogram.com/blog/2018/03/27/
[wx]: https://en.wikipedia.org/wiki/W%5EX
[strace]: /blog/2018/06/23/
[win]: /blog/2016/06/13/
[wine]: https://www.winehq.org/
