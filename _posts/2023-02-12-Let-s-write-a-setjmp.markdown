---
title: "Let's write a setjmp"
layout: post
date: 2023-02-12T02:23:11Z
tags: [c]
uuid: ab83cc5d-7877-4cba-98e4-d36059297ead
excerpt_separator: <!--more-->
---

*This article was discussed [on Hacker News][hn].*

Yesterday I wrote that [`setjmp` is handy][libc] and that it would be nice
to have without linking the C standard library. It's conceptually simple,
after all. Today let's explore some differently-portable implementation
possibilities with distinct trade-offs. At the very least it should
illuminate why `setjmp` sometimes requires the use of `volatile`.

<!--more-->

First, a quick review: `setjmp` and `longjmp` are a form of *non-local
goto*.

```c
typedef void *jmp_buf[N];
int setjmp(jmp_buf);
void longjmp(jmp_buf, int);
```

Calling `setjmp` saves the execution context in a `jmp_buf`, and `longjmp`
restores this context, returning the thread to this previous point of
execution. This means `setjmp` returns twice: (1) after saving the
context, and (2) from `longjmp`. To distinguish these cases, the first
time it returns zero and the second time it returns the value passed to
`longjmp`.

`jmp_buf` is an array of some platform-specific type and length. I'll be
using void pointers in this article because it's a register-sized type
that isn't behind a typedef. Plus they print nicely in GDB as hexadecimal
addresses which eased in working it out.

### Using GCC intrinsics

Let's start with the easiest option. [GCC has two intrinsics][gcc] doing
all the hard work for us: `__builtin_setjmp` and `__builtin_longjmp`. Its
worst case `jmp_buf` is length 5, but the most popular architectures only
use the first 3 elements. Clang supports these intrinsics as well for GCC
compatibility.

Be mindful that the semantics are slightly different from the standard C
definition, namely that you cannot use `longjmp` from the same function as
`setjmp`. It also doesn't touch the signal mask. However, it's easier to
use and you don't need to worry about `volatile`.

```c
// NOTE to copy-pasters: semantics differ slightly from standard C
typedef void *jmp_buf[5];
#define setjmp __builtin_setjmp
#define longjmp __builtin_longjmp
```

If you only care about GCC and/or Clang, then that's it! It works as-is on
every supported target and nothing more is needed. As a bonus, it will be
more efficient than the libc version, though I should hope that won't
matter in practice. These are so awesome and convenient that I'm already
second-guessing myself: "Do I *really* need to support other compilers…?"

### Using assembly

If I want to support more compilers I'll need to write it myself. It's
also an excuse to dig into the details. The execution context is no more
than an array of saved registers, and `longjmp` is merely restoring those
registers. One of the registers is the instruction pointer, and setting
the instruction pointer is called a jump.

Since we're talking about registers, that means assembly. We'll also need
to know the target's calling convention, so this really narrows things
down. This implementation will target x86-64, a.k.a x64, Windows, *but* it
will support MSVC as an additional compiler. So it's a different kind of
portability. I'll start with GCC via [w64devkit][] then massage it into
something MSVC can use.

I mentioned before that `setjmp` returns twice. So to return a second time
we just need to *simulate* a normal function return. Obviously that
includes restoring the stack pointer like the `ret` instruction, but it
means preserving all the non-volatile registers a callee is supposed to
preserve. These will all go in the execution context.

The [x64 calling convention][x64] specifies 9 non-volatile `rsp`, `rsp`,
`rbx`, `rdi`, `rsi`, `r12`, `r13`, `r14`, and `r15`. We'll also need the
instruction pointer, `rip`, making it 10 total.

```c
typedef void *jmp_buf[10];
```

#### setjmp assembly

The tricky issue is that we need to save the registers immediately inside
`setjmp` before the compiler has manipulated them in a function prologue.
That will take more than mere inline assembly. We'll start with a *naked*
function, which means that GCC will not create a prologue or epilogue.
However, that means no local variables, and the function body will be
limited to inline assembly, including a `ret` instruction for the
epilogue.

```c
__attribute__((naked))
int setjmp(jmp_buf buf)
{
    __asm(
        // ...
    );
}
```

The x64 calling convention uses `rcx` for the first pointer argument, so
that's where we'll find `buf`. I've arbitrarily decided to store `rip`
first, then the other registers in order. However, the current value of
`rip` isn't the one we need. The `rip` we need was just pushed on top of
the stack by the caller. I'll read that off the stack into a scratch
register, `rax`, and then store it in the first element of `buf`.

```nasm
    mov (%rsp), %rax
    mov %rax,  0(%rcx)
```

The stack pointer, `rsp`, is also indirect since I want the pointer just
before `rip` was pushed, as it would be just after a `ret`. I use a `lea`,
*load effective address*, to add 8 bytes (recall: stack grows down),
placing the result in a scratch register, then write it into the second
element of `buf` (i.e. 8 bytes into `%rcx`).

```nasm
    lea 8(%rsp), %rax
    mov %rax,  8(%rcx)
```

Everything else is a matter of elbow grease.

```nasm
    mov %rbp, 16(%rcx)
    mov %rbx, 24(%rcx)
    mov %rdi, 32(%rcx)
    mov %rsi, 40(%rcx)
    mov %r12, 48(%rcx)
    mov %r13, 56(%rcx)
    mov %r14, 64(%rcx)
    mov %r15, 72(%rcx)
```

With all work complete, return zero to the caller.

```nasm
    xor %eax, %eax
    ret
```

Putting it altogether, and avoiding a `-Wunused-variable`:

```c
__attribute__((naked,returns_twice))
int setjmp(jmp_buf buf)
{
    (void)buf;
    __asm(
        "mov (%rsp), %rax\n"
        "mov %rax,  0(%rcx)\n"
        "lea 8(%rsp), %rax\n"
        "mov %rax,  8(%rcx)\n"
        "mov %rbp, 16(%rcx)\n"
        "mov %rbx, 24(%rcx)\n"
        "mov %rdi, 32(%rcx)\n"
        "mov %rsi, 40(%rcx)\n"
        "mov %r12, 48(%rcx)\n"
        "mov %r13, 56(%rcx)\n"
        "mov %r14, 64(%rcx)\n"
        "mov %r15, 72(%rcx)\n"
        "xor %eax, %eax\n"
        "ret\n"
    );
}
```

Also take note of the `returns_twice` attribute. It informs GCC of this
function's unusual nature, saying the function *doesn't* preserve most
non-volatile registers, and induces `-Wclobbered` diagnostics. Technically
this means we could get away with saving only `rip`, `rsp`, and `rbp` —
exactly as `__builtin_setjmp` does — but we'll need the others for MSVC
anyway.

#### longjmp assembly

In `longjmp` we need to restore all those registers. For purely aesthetic
reasons I've decided to do it in reverse order. Everything but `rip` is
easy.

```nasm
    mov 72(%rcx), %r15
    mov 64(%rcx), %r14
    mov 56(%rcx), %r13
    mov 48(%rcx), %r12
    mov 40(%rcx), %rsi
    mov 32(%rcx), %rdi
    mov 24(%rcx), %rbx
    mov 16(%rcx), %rbp
    mov  8(%rcx), %rsp
```

The instruction set doesn't have direct access to `rip`. It will be a
`jmp` instead of `mov`, but before jumping we'll need to prepare the
return value. The x64 calling convention says the second argument is
passed in `rdx`, so move that to `rax`, then `jmp` to the caller. It's
only a 32-bit operand, C `int`, so `edx` instead of `rdx`.

```nasm
    mov %edx, %eax
    jmp *0(%rcx)
```

Putting it all together, and adding the `noreturn` attribute:

```c
__attribute__((naked,noreturn))
void longjmp(jmp_buf buf, int ret)
{
    (void)buf;
    (void)ret;
    __asm(
        "mov 72(%rcx), %r15\n"
        "mov 64(%rcx), %r14\n"
        "mov 56(%rcx), %r13\n"
        "mov 48(%rcx), %r12\n"
        "mov 40(%rcx), %rsi\n"
        "mov 32(%rcx), %rdi\n"
        "mov 24(%rcx), %rbx\n"
        "mov 16(%rcx), %rbp\n"
        "mov  8(%rcx), %rsp\n"
        "mov %edx, %eax\n"
        "jmp *0(%rcx)\n"
    );
}
```

The C standard says that if `ret` is zero then `longjmp` will return 1
from `setjmp` instead. I leave that detail as a reader exercise. Otherwise
this is a complete, working `setjmp`. It works perfectly when I swap it in
for `setjmp.h` in [my u-config test suite][uc].

### Considering volatile

Now that you've seen the guts, let's talk about `volatile` and why it's
necessary. Consider this function, `example`, which calls a `work`
function that may return through `setjmp` (e.g. on failure).

```c
void work(jmp_buf);

int example(void)
{
    int r = 0;
    jmp_buf buf;
    if (!setjmp(buf)) {
        // first return
        r = 1;
        work(buf);
    } else {
        // second return
    }
    return r;
}
```

It stores to `r` after the first `setjmp` return, then loads `r` after the
second `setjmp` return. However, `r` may have been stored in the execution
context. Since it's used across function calls, it would be reasonable to
store this variable in non-volatile register like `ebx`. If so, it will be
restored to its value at the moment of the first call to `setbuf`, in
which case the *old* `r` would be read after restoration by `longjmp`. If
it's not stored in a register, but on the stack, then on the second return
the function will read the latest value out of the stack. In practice, if
`work` returns through `longjmp`, this function may return either 0 or 1,
probably determined by the optimization level.

The solution is to qualify `r` with `volatile`, which forces the compiler
to store the variable on the stack and never cache it in a register.

```c
    volatile int r = 0;
```

Though since our `setbuf` is marked `returns_twice`, GCC will never store
`r` in a register across `setjmp` calls. This potentially hides a bug in
the program that would occur under some other compilers, but GCC will
(usually) warn about it.

### Pure assembly and MSVC

MSVC doesn't understand `__attribute__` nor the inline assembly, so it
cannot compile these functions. I could compile my `setjmp` with GCC and
the rest of the program with MSVC, which means I need two compilers.
Instead, I'll move to pure assembly, assemble with GNU `as` (TODO: port
to MASM?) so we'll only need a tiny piece of the GNU toolchain.

```nasm
	.global setjmp
setjmp:
        mov (%rsp), %rax
	mov %rax,  0(%rcx)
	lea 8(%rsp), %rax
	mov %rax,  8(%rcx)
	mov %rbp, 16(%rcx)
	mov %rbx, 24(%rcx)
	mov %rdi, 32(%rcx)
	mov %rsi, 40(%rcx)
	mov %r12, 48(%rcx)
	mov %r13, 56(%rcx)
	mov %r14, 64(%rcx)
	mov %r15, 72(%rcx)
	xor %eax, %eax
	ret

	.globl longjmp
longjmp:
	mov 72(%rcx), %r15
	mov 64(%rcx), %r14
	mov 56(%rcx), %r13
	mov 48(%rcx), %r12
	mov 40(%rcx), %rsi
	mov 32(%rcx), %rdi
	mov 24(%rcx), %rbx
	mov 16(%rcx), %rbp
	mov  8(%rcx), %rsp
	mov %edx, %eax
	jmp *0(%rcx)
```

Then some declarations in C:

```c
typedef void *jmp_buf[10];
int setjmp(jmp_buf);
_Noreturn void longjmp(jmp_buf, int);
```

I'll need to enable C11 for that `_Noreturn` in MSVC. Assemble, compile,
and link:

    $ as -o setjmp.obj setjmp.s
    $ cl /std:c11 program.c setjmp.obj

That generally works! If I rename to `xsetjmp` and `xlongjmp` to avoid
conflicting with the CRT definitions, drop them into the u-config test
suite in place of `setjmp.h`, then compile with MSVC, it passes all tests
using my alternate implementation in MSVC as well as GCC. Pretty cool!

### Takeaway

I'm not sure if I'll ever use the assembly, but writing this article led
me to try the GCC intrinsics, and I'm so impressed I'm still thinking
about ways I can use them. My main thought is out-of-memory situations in
arena allocators, using a non-local exit to roll back to a savepoint, even
if just to return an error. This is nicer than either terminating the
program or handling OOM errors on every allocation. Very roughly:

```c
typedef struct {
    size_t cap;
    size_t off;
    void *jmp_buf[5];
} Arena;

// Place an arena and savepoint an out-of-memory jump.
#define OOM(a, m, n) __builtin_setjmp((a = place(m, n))->jmp_buf)

// Place a new arena at the front of the buffer.
Arena *place(void *mem, size_t size)
{
    assert(size >= sizeof(Arena));
    Arena *a = mem;
    a->cap = size;
    a->off = sizeof(Arena);
    return a;
}

void *alloc(Arena *a, size_t size)
{
    size_t avail = a->cap - a->off;
    if (avail < size) {
        __builtin_longjmp(a->jmp_buf, 1);
    }
    void *p = (char *)a + a->off;
    a->off += size;
    return p;
}
```

Usage would look like:

```c
int compute(void *workmem, size_t memsize)
{
    Arena *arena;
    if (OOM(arena, workmem, memsize)) {
        // jumps here when out of memory
        return COMPUTE_OOM;
    }

    Thing *t = PUSHSTRUCT(arena, Thing);
    // ...

    return COMPUTE_OK;
}
```

More granular snapshots can be made further down the stack by allocating
subarenas out of the main arena. I have yet to try this out in a practical
program.


[gcc]: https://gcc.gnu.org/onlinedocs/gcc/Nonlocal-Gotos.html
[libc]: /blog/2023/02/11/
[uc]: https://github.com/skeeto/u-config/blob/master/test_main.c
[w64devkit]: https://github.com/skeeto/w64devkit
[x64]: https://learn.microsoft.com/en-us/cpp/build/x64-calling-convention
[hn]: https://news.ycombinator.com/item?id=34760828
