---
title: A more robust raw OpenBSD syscall demo
layout: post
date: 2025-03-06T02:43:20Z
tags: [c, bsd, x86]
uuid: f7101ee1-a2e6-4895-b763-bd7b2a842280
---

Ted Unangst published [*dude, where are your syscalls?*][orig] on flak
yesterday, with a neat demonstration of OpenBSD's [pinsyscall][und]
security feature, whereby only pre-registered addresses are allowed to
make system calls. Whether it strengthens or weakens security is [up for
debate][crit], but regardless it's an interesting, low-level programming
challenge. The original demo is fragile for multiple reasons, and requires
manually locating and entering addresses for each build. In this article I
show how to fix it. To prove that it's robust, I ported an entire, real
application to use raw system calls on OpenBSD.

The original program uses ARM64 assembly. I'm a lot more comfortable with
x86-64 assembly, plus that's the hardware I have readily on hand. So the
assembly language will be different, but all the concepts apply to both
these architectures. Almost none of these OpenBSD system interfaces are
formally documented (or stable for that matter), and I had to dig around
the OpenBSD source tree to figure it out (along with a [helpful jart
nudge][nudge]). So don't be afraid to get your hands dirty.

There are lots of subtle problems in the original demo, so let's go
through the program piece by piece, starting with the entry point:

```c
void
start()
{
        w("hello\n", 6);
        x();
}
```

This function is registered as the entry point in the ELF image, so it has
no caller. That means no return address on the stack, so the stack is not
aligned for a function. In toy programs that goes unnoticed, but compilers
generate code assuming the stack is aligned. In a real application this is
likely to crash deep on the first SIMD register spill.

We could fix this with a [`force_align_arg_pointer`][align] attribute, at
least for architectures that support it, but I prefer to write the entry
point in assembly. Especially so we can access the command line arguments
and environment variables, which is necessary in a real application. That
happens to work the same as it does on Linux, so here's my old, familiar
entry point:

```c
asm (
    "        .globl _start\n"
    "_start: mov   %rsp, %rdi\n"
    "        call  start\n"
);
```

Per the ABI, the first argument passes through `rdi`, so I pass a copy of
the stack pointer, `rsp`, as it appeared on entry. Entry point arguments
`argc`, `argv`, and `envp` are all pushed on the stack at `rsp`, so the
first real function can retrieve it all from just the stack pointer. The
original demo won't use it, though. Using `call` to pass control pushes a
return address, which will never be used, and aligns the stack for the
first real function. I name it `_start` because that's what the linker
expects and so things will go a little smoother, so it's rather convenient
that the original didn't use this name.

Next up, the "write" function:

```c
int
w(void *what, size_t len) {
        __asm(
"       mov x2, x1;"
"       mov x1, x0;"
"       mov w0, #1;"
"       mov x8, #4;"
"       svc #0;"
        );
        return 0;
}
```

There are two [serious problems with this assembly block][asm]. First, the
function arguments are not necessarily in those registers by the time
control reaches the basic assembly block. The function prologue could move
them around. Even more so if this function was inlined. This is exactly
the problem *extended* inline assembly is intended to solve. Second, it
clobbers a number of registers. Compilers assume this does not happen when
generating their own code. This sort of assembly falls apart the moment it
comes into contact with a non-zero optimization level.

Solving this is just a matter of using inline assembly properly:

```c
long w(void *what, long len)
{
    char err;
    long rax = 4;  // SYS_write
    asm volatile (
        "syscall"
        : "+a"(rax), "+d"(len), "=@ccc"(err)
        : "D"(1), "S"(what)
        : "rcx", "r11", "memory"
    );
    return err ? -rax : rax;
}
```

I've enhanced it a bit, returning a [Linux-style negative errno][linux] on
error. In the BSD ecosystem, syscall errors are indicated using the carry
flag, which here is output into `err` via `=@ccc`. When set, the return
value is an errno. Further, the OpenBSD kernel uses both `rax` and `rdx`
for return values, so I've also listed `rdx` as an input+output despite
not consuming the result. Despite all these changes, this function is not
yet complete! We'll get back to it later.

The "exit" function, `x`, is just fine:

```c
void
x() {
        __asm(
"       mov x8, #1;"
"       svc #0;"
        );
}
```

It doesn't set an exit status, so it passes garbage instead, but otherwise
this works. No inputs, plus clobbers and outputs don't matter when control
never returns. In a real application I might write it:

```c
__attribute((noreturn))
void x(int status)
{
    asm volatile ("syscall" :: "a"(1), "D"(status));
    __builtin_unreachable();
}
```

This function will need a little additional work later, too.

The `ident` section is basically fine as-is:

```c
__asm(" .section \".note.openbsd.ident\", \"a\"\n"
"       .p2align 2\n"
"       .long   8\n"
"       .long   4\n"
"       .long   1\n"
"       .ascii \"OpenBSD\\0\"\n"
"       .long   0\n"
"       .previous\n");
```

The compiler assumes the current section remains the same at the end of
the assembly block, which here is accomplished with `.previous`. Though it
clobbers the assembler's remembered "other" section and so may interfere
with surrounding code using `.previous`. Better to use `.pushsection` and
`.popsection` for good stack discipline. There are many such examples in
the OpenBSD source tree.

```c
asm (
    ".pushsection .note.openbsd.ident, \"a\"\n"
    ".long  8, 4, 1, 0x6e65704f, 0x00445342, 0\n"
    ".popsection\n"
);
```

Now the trickiest part, the pinsyscall table:

```c
struct whats {
        unsigned int offset;
        unsigned int sysno;
} happening[] __attribute__((section(".openbsd.syscalls"))) = {
        { 0x104f4, 4 },
        { 0x10530, 1 },
};
```

Those offsets — offsets from the beginning of the ELF image — were entered
manually, and it kind of ruins the whole demo. We don't have a good way to
get at those offsets from C, or any high level language. However, we can
solve that by tweaking the inline assembly with some labels:

```c
__attribute((noinline))
long w(void *what, long len)
{
    // ...
    asm volatile (
        "_w: syscall"
        // ...
    );
    // ...
}

__attribute((noinline,noreturn))
void x(int status)
{
    asm volatile (
        "_x: syscall"
        // ...
    );
    // ...
}
```

Very importantly I've added `noinline` to prevent these functions from
being inlined into additional copies of the `syscall` instruction, which
of course won't be registered. This also prevents duplicate labels causing
assembler errors. Once we have the labels, we can use them in an assembly
block listing the allowed syscall instructions:

```c
asm (
    ".pushsection .openbsd.syscalls\n"
    ".long  _x, 1\n"
    ".long  _w, 4\n"
    ".popsection\n"
);
```

That lets the linker solve the offsets problem, which is its main job
after all. With these changes the demo works reliably, even under high
optimization levels. I suggest these flags:

    $ cc -static -nostdlib -no-pie -o where where.c

Disabling PIE with `-no-pie` is necessary in real applications or else
strings won't work. You can apply more flags to strip it down further, but
these are the flags generally necessary to compile these sorts of programs
on at least OpenBSD 7.6.

So, how do I know this stuff works in general? Because I ported [my ultra
portable pkg-config clone, u-config][pkg], to use raw OpenBSD syscalls:
**[`openbsd_main.c`][src]**. Everything still works at high optimization
levels.

    $ cc -static -nostartfiles -no-pie -o pkg-config openbsd_main.c libmemory.a
    $ ./pkg-config --cflags --libs libcurl
    -I/usr/local/include -L/usr/local/lib -lcurl

Because the new syscall wrappers behave just like Linux system calls, it
leverages the `linux_noarch.c` platform, and the whole port is ~70 lines
of code. A few more flags (`-fno-stack-protector`, `-Oz`, `-s`, etc.), and
it squeezes into a slim 21.6K static binary.

Despite making no libc calls, it's not possible stop compilers from
fabricating ([hallucinating?][llm]) string function calls, so the build
above depends on external definitions. In the command above, `libmemory.a`
comes from [`libmemory.c`][mem] found [in w64devkit][w64]. Alternatively,
[and on topic][libc], you could link the OpenBSD libc string functions by
omitting `libmemory.a` from the build.

    $ cc -static -nostartfiles -no-pie -o pkg-config openbsd_main.c

Though it pulls in a lot of bloat (~8x size increase), and teasing out the
necessary objects isn't trivial.


[align]: https://gcc.gnu.org/onlinedocs/gcc/x86-Function-Attributes.html#index-force_005falign_005farg_005fpointer-function-attribute_002c-x86
[asm]: /blog/2024/12/20/
[crit]: https://isopenbsdsecu.re/mitigations/pinsyscall/
[libc]: https://flak.tedunangst.com/post/you-dont-link-all-of-libc
[linux]: /blog/2016/09/23/
[llm]: /blog/2024/11/10/
[mem]: https://github.com/skeeto/w64devkit/blob/master/src/libmemory.c
[nudge]: https://news.ycombinator.com/item?id=26290723
[orig]: https://flak.tedunangst.com/post/dude-where-are-your-syscalls
[pkg]: /blog/2023/01/18/
[src]: https://github.com/skeeto/u-config/blob/openbsd/openbsd_main.c
[und]: https://undeadly.org/cgi?action=article;sid=20230222064027
[w64]: /blog/2024/02/05/
