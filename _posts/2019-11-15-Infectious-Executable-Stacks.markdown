---
title: Infectious Executable Stacks
layout: post
date: 2019-11-15T03:29:37Z
tags: [c, netsec, x86]
uuid: 7266b2ea-f39e-4b9a-87c8-e4480374af41
excerpt_separator: <!--more-->
---

*This article was discussed [on Hacker News][hn]*.

In software development there are many concepts that at first glance
seem useful and sound, but, after considering the consequences of their
implementation and use, are actually horrifying. Examples include
[thread cancellation][thr], [variable length arrays][vla], and [memory
aliasing][alias]. GCC's closure extension to C is another, and this
little feature compromises the entire GNU toolchain.

<!--more-->

### GNU C nested functions

GCC has its own dialect of C called GNU C. One feature unique to GNU C
is *nested functions*, which allow C programs to define functions inside
other functions:

```c
void intsort1(int *base, size_t nmemb)
{
    int cmp(const void *a, const void *b)
    {
        return *(int *)a - *(int *)b;
    }
    qsort(base, nmemb, sizeof(*base), cmp);
}
```

The nested function above is straightforward and harmless. It's nothing
groundbreaking, and it is trivial for the compiler to implement. The
`cmp` function is really just a static function whose scope is limited
to the containing function, no different than a local static variable.

With one slight variation the nested function turns into a closure. This
is where things get interesting:

```c
void intsort2(int *base, size_t nmemb, _Bool invert)
{
    int cmp(const void *a, const void *b)
    {
        int r = *(int *)a - *(int *)b;
        return invert ? -r : r;
    }
    qsort(base, nmemb, sizeof(*base), cmp);
}
```

The `invert` variable from the outer scope is accessed from the inner
scope. This has [clean, proper closure semantics][php] and works
correctly just as you'd expect. It fits quite well with traditional C
semantics. The closure itself is re-entrant and thread-safe. It's
automatically (read: stack) allocated, and so it's automatically freed
when the function returns, including when the stack is unwound via
`longjmp()`. It's a natural progression to support closures like this
via nested functions. The eventual caller, `qsort`, doesn't even know
it's calling a closure!

While this seems so useful and easy, its implementation has serious
consequences that, in general, outweigh its benefits. In fact, in order
to make this work, the whole GNU toolchain has been specially rigged!

How does it work? The function pointer, `cmp`, passed to `qsort` must
somehow be associated with its lexical environment, specifically the
`invert` variable. A static address won't do. When I [implemented
closures as a toy library][lib], I talked about the function address for
each closure instance somehow needing to be unique.

GCC accomplishes this by constructing a trampoline on the stack. That
trampoline has access to the local variables stored adjacent to it, also
on the stack. GCC also generates a normal `cmp` function, like the
simple nested function before, that accepts `invert` as an additional
argument. The trampoline calls this function, passing the local variable
as this additional argument.

### Trampoline illustration

To illustrate this, I've manually implemented `intsort2()` below for
x86-64 ([System V ABI][sysv]) without using GCC's nested function
extension:

```c
int cmp(const void *a, const void *b, _Bool invert)
{
    int r = *(int *)a - *(int *)b;
    return invert ? -r : r;
}

void intsort3(int *base, size_t nmemb, _Bool invert)
{
    unsigned long fp = (unsigned long)cmp;
    volatile unsigned char buf[] = {
        // mov  edx, invert
        0xba, invert, 0x00, 0x00, 0x00,
        // mov  rax, cmp
        0x48, 0xb8, fp >>  0, fp >>  8, fp >> 16, fp >> 24,
                    fp >> 32, fp >> 40, fp >> 48, fp >> 56,
        // jmp  rax
        0xff, 0xe0
    };
    int (*trampoline)(const void *, const void *) = (void *)buf;
    qsort(base, nmemb, sizeof(*base), trampoline);
}
```

Here's a complete example you can try yourself on nearly any x86-64
unix-like system: [**trampoline.c**][dl]. It even works with Clang. The
two notable systems where stack trampolines won't work are
[OpenBSD][tedu] and [WSL][wsl].

(Note: The `volatile` is necessary because C compilers rightfully do
not see the contents of `buf` as being consumed. Execution of the
contents isn't considered.)

In case you hadn't already caught it, there's a catch. The linker needs
to link a binary that asks the loader for an executable stack (`-z
execstack`):

    $ cc -std=c99 -Os -Wl,-z,execstack trampoline.c

That's because `buf` contains x86 code implementing the trampoline:

```nasm
mov  edx, invert    ; assign third argument
mov  rax, cmp       ; store cmp address in RAX register
jmp  rax            ; jump to cmp
```

(Note: The absolute jump through a 64-bit register is necessary because
the trampoline on the stack and the jump target will be very far apart.
Further, these days the program will likely be compiled as a Position
Independent Executable (PIE), so `cmp` [might itself have an high
address][mm] rather than load into the lowest 32 bits of the address
space.)

However, executable stacks were phased out ~15 years ago because it
makes buffer overflows so much more dangerous! Attackers can inject
and execute whatever code they like, typically *shellcode*. That's why
we need this unusual linker option.

You can see that the stack will be executable using our old friend,
`readelf`:

    $ readelf -l a.out
    ...
      GNU_STACK  0x00000000 0x00000000 0x00000000
                 0x00000000 0x00000000 RWE   0x10
    ...

Note the "RWE" at the bottom right, meaning read-write-execute. This is
a really bad sign in a real binary. Do any binaries installed on your
system right now have an executable stack? [I found one on mine][mupdf].
(Update: [A major one was found in the comments by Walter Misar][qt].)

When compiling the original version using a nested function there's no
need for that special linker option. That's because GCC saw that it
would need an executable stack and used this option automatically.

Or, more specifically, GCC *stopped* requesting a non-executable stack
in the object file it produced. For the GNU Binutils linker, **the
default is an executable stack.**

### Fail open design

Since this is the default, the only way to get a non-executable stack is
if *every* object file input to the linker explicitly declares that it
does not need an executable stack. To request a non-executable stack, an
object file [must contain the (empty) section `.note.GNU-stack`][lance].
If even a single object file fails to do this, then the final program
gets an executable stack.

Not only does one contaminated object file infect the binary, everything
dynamically linked with it *also* gets an executable stack. Entire
processes are infected! This occurs even via `dlopen()`, where the stack
is dynamically made executable to accomodate the new shared object.

I've been bit myself. In [*Baking Data with Serialization*][bake] I did
it completely by accident, and I didn't notice my mistake until three
years later. The GNU linker outputs object files without the special
note by default even though the object file only contains data.

    $ echo hello world >hello.txt
    $ ld -r -b binary -o hello.o hello.txt
    $ readelf -S hello.o | grep GNU-stack
    $

This is fixed with `-z noexecstack`:

    $ ld -r -b binary -z noexecstack -o hello.o hello.txt
    $ readelf -S hello.o | grep GNU-stack
      [ 2] .note.GNU-stack  PROGBITS  00000000  0000004c
    $

This may happen any time you link object files not produced by GCC, such
as output [from the NASM assembler][nasm] or [hand-crafted object
files][needle].

Nested C closures are super slick, but they're just not worth the risk
of an executable stack, and they're certainly not worth an entire
toolchain being fail open about it.

Update: A [rebuttal][rebuttal]. My short response is that the issue
discussed in my article isn't really about C the language but rather
about an egregious issue with one particular toolchain. The problem
doesn't even arise if you use only C, but instead when linking in object
files specifically *not* derived from C code.


[alias]: /blog/2018/07/20/#strict-aliasing
[bake]: /blog/2016/11/15/
[dl]: /download/trampoline.c
[hn]: https://news.ycombinator.com/item?id=21553882
[lance]: https://www.airs.com/blog/archives/518
[lib]: /blog/2017/01/08/
[mm]: https://eli.thegreenplace.net/2012/01/03/understanding-the-x64-code-models
[mupdf]: https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=944817
[nasm]: /blog/2015/04/19/
[needle]: /blog/2016/11/17/
[php]: /blog/2019/09/25/
[qt]: https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=944971
[rebuttal]: http://verisimilitudes.net/2019-11-21
[sysv]: https://wiki.osdev.org/System_V_ABI
[tedu]: https://marc.info/?l=openbsd-cvs&m=149606868308439&w=2
[thr]: https://lwn.net/Articles/683118/
[vla]: /blog/2019/10/27/
[wsl]: https://github.com/microsoft/WSL/issues/286
