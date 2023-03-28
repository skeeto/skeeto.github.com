---
title: Practical libc-free threading on Linux
layout: post
date: 2023-03-23T05:32:41Z
tags: [c, optimization, linux, x86]
uuid: 631a8107-2eef-420b-9594-752e6f013048
---

Suppose you're [not using a C runtime][crt] on Linux, and instead you're
programming against its system call API. It's long-term and stable after
all. [Memory management][arena] and [buffered I/O][bufio] are easily
solved, but a lot of software benefits from concurrency. It would be nice
to also have thread spawning capability. This article will demonstrate a
simple, practical, and robust approach to spawning and managing threads
using only raw system calls. It only takes about a dozen lines of C,
including a few inline assembly instructions.

The catch is that there's no way to avoid using a bit of assembly. Neither
the `clone` nor `clone3` system calls have threading semantics compatible
with C, so you'll need to paper over it with a bit of inline assembly per
architecture. This article will focus on x86-64, but the basic concept
should work on all architecture supported by Linux. The [glibc `clone(2)`
wrapper][man] fits a C-compatible interface on top of the raw system call,
but we won't be using it here.

Before diving in, the complete, working demo: [**`stack_head.c`**][src]

### The clone system call

On Linux, threads are spawned using the `clone` system call with semantics
like the classic unix `fork(2)`. One process goes in, two processes come
out in nearly the same state. For threads, those processes share almost
everything and differ only by two registers: the return value — zero in
the new thread — and stack pointer. Unlike typical thread spawning APIs,
the application does not supply an entry point. It only provides a stack
for the new thread. The simple form of the raw clone API looks something
like this:

```c
long clone(long flags, void *stack);
```

Sounds kind of elegant, but it has an annoying problem: The new thread
begins life in the *middle* of a function without any established stack
frame. Its stack is a blank slate. It's not ready to do anything except
jump to a function prologue that will set up a stack frame. So besides the
assembly for the system call itself, it also needs more assembly to get
the thread into a C-compatible state. In other words, **a generic system
call wrapper cannot reliably spawn threads**.

```c
void brokenclone(void (*threadentry)(void *), void *arg)
{
    // ...
    long r = syscall(SYS_clone, flags, stack);
    // DANGER: new thread may access non-existant stack frame here
    if (!r) {
        threadentry(arg);
    }
}
```

For odd historical reasons, each architecture's `clone` has a slightly
different interface. The newer `clone3` unifies these differences, but it
suffers from the same thread spawning issue above, so it's not helpful
here.

### The stack "header"

I [figured out a neat trick eight years ago][raw] which I continue to use
today. The parent and child threads are in nearly identical states when
the new thread starts, but the immediate goal is to diverge. As noted, one
difference is their stack pointers. To diverge their execution, we could
make their execution depend on the stack. An obvious choice is to push
different return pointers on their stacks, then let the `ret` instruction
do the work.

Carefully preparing the new stack ahead of time is the key to everything,
and there's a straightforward technique that I like call the `stack_head`,
a structure placed at the high end of the new stack. Its first element
must be the entry point pointer, and this entry point will receive a
pointer to its own `stack_head`.

```c
struct __attribute((aligned(16))) stack_head {
    void (*entry)(struct stack_head *);
    // ...
};
```

The structure must have 16-byte alignment on all architectures. I used an
attribute to help keep this straight, and it can help when using `sizeof`
to place the structure, as I'll demonstrate later.

Now for the cool part: The `...` can be anything you want! Use that area
to seed the new stack with whatever thread-local data is necessary. It's a
neat feature you don't get from standard thread spawning interfaces. If I
plan to "join" a thread later — wait until it's done with its work — I'll
put a join futex in this space:

```c
struct __attribute((aligned(16))) stack_head {
    void (*entry)(struct stack_head *);
    int join_futex;
    // ...
};
```

More details on that futex shortly.

### The clone wrapper

I call the `clone` wrapper `newthread`. It has the inline assembly for the
system call, and since it includes a `ret` to diverge the threads, it's a
"naked" function [just like with `setjmp`][setjmp]. The compiler will
generate no prologue or epilogue, and the function body is limited to
inline assembly without input/output operands. It cannot even reliably
reference its parameters by name. Like `clone`, it doesn't accept a thread
entry point. Instead it accepts a `stack_head` seeded with the entry
point. The whole wrapper is just six instructions:

```c
__attribute((naked))
static long newthread(struct stack_head *stack)
{
    __asm volatile (
        "mov  %%rdi, %%rsi\n"     // arg2 = stack
        "mov  $0x50f00, %%edi\n"  // arg1 = clone flags
        "mov  $56, %%eax\n"       // SYS_clone
        "syscall\n"
        "mov  %%rsp, %%rdi\n"     // entry point argument
        "ret\n"
        : : : "rax", "rcx", "rsi", "rdi", "r11", "memory"
    );
}
```

On x86-64, both function calls and system calls use `rdi` and `rsi` for
their first two parameters. Per the reference `clone(2)` prototype above:
the first system call argument is `flags` and the second argument is the
new `stack`, which will point directly at the `stack_head`. However, the
stack pointer arrives in `rdi`. So I copy `stack` into the second argument
register, `rsi`, then load the flags (`0x50f00`) into the first argument
register, `rdi`. The system call number goes in `rax`.

Where does that `0x50f00` come from? That's the bare minimum thread spawn
flag set in hexadecimal. If any flag is missing then threads will not
spawn reliably — as discovered the hard way by trial and error across
different system configurations, not from documentation. It's computed
normally like so:

```c
    long flags = 0;
    flags |= CLONE_FILES;
    flags |= CLONE_FS;
    flags |= CLONE_SIGHAND;
    flags |= CLONE_SYSVSEM;
    flags |= CLONE_THREAD;
    flags |= CLONE_VM;
```

When the system call returns, it copies the stack pointer into `rdi`, the
first argument for the entry point. In the new thread the stack pointer
will be the same value as `stack`, of course. In the old thread this is a
harmless no-op because `rdi` is a volatile register in this ABI. Finally,
`ret` pops the address at the top of the stack and jumps. In the old
thread this returns to the caller with the system call result, either an
error ([negative errno][signal]) or the new thread ID. In the new thread
**it pops the first element of `stack_head`** which, of course, is the
entry point. That's why it must be first!

The thread has nowhere to return from the entry point, so when it's done
it must either block indefinitely or use the `exit` (*not* `exit_group`)
system call to terminate itself.

### Caller point of view

The caller side looks something like this:

```c
static void threadentry(struct stack_head *stack)
{
    // ... do work ...
    __atomic_store_n(&stack->join_futex, 1, __ATOMIC_SEQ_CST);
    futex_wake(&stack->join_futex);
    exit(0);
}

__attribute((force_align_arg_pointer))
void _start(void)
{
    struct stack_head *stack = newstack(1<<16);
    stack->entry = threadentry;
    // ... assign other thread data ...
    stack->join_futex = 0;
    newthread(stack);

    // ... do work ...

    futex_wait(&stack->join_futex, 0);
    exit_group(0);
}
```

Despite the minimalist, 6-instruction clone wrapper, this is taking the
shape of a conventional threading API. It would only take a bit more to
hide the futex, too. Speaking of which, what's going on there? The [same
principal as a WaitGroup][wg]. The futex, an integer, is zero-initialized,
indicating the thread is running ("not done"). The joiner tells the kernel
to wait until the integer is non-zero, which it may already be since I
don't bother to check first. When the child thread is done, it atomically
sets the futex to non-zero and wakes all waiters, which might be nobody.

Caveat: It's not safe to free/reuse the stack after a successful join. It
only indicates the thread is done with its work, not that it exited. You'd
need to wait for its `SIGCHLD` (or use `CLONE_CHILD_CLEARTID`). If this
sounds like a problem, consider [your context][ctx] more carefully: Why do
you feel the need to free the stack? It will be freed when the process
exits. Worried about leaking stacks? Why are you starting and exiting an
unbounded number of threads? In the worst case park the thread in a thread
pool until you need it again. Only worry about this sort of thing if
you're building a general purpose threading API like pthreads. I know it's
tempting, but avoid doing that unless you absolutely must.

What's with the `force_align_arg_pointer`? Linux doesn't align the stack
for the process entry point like a System V ABI function call. Processes
begin life with an unaligned stack. This attribute tells GCC to fix up the
stack alignment in the entry point prologue, [just like on Windows][i686].
If you want to access `argc`, `argv`, and `envp` you'll need [more
assembly][wild]. (I wish doing *really basic things* without libc on Linux
didn't require so much assembly.)

```c
__asm (
    ".global _start\n"
    "_start:\n"
    "   movl  (%rsp), %edi\n"
    "   lea   8(%rsp), %rsi\n"
    "   lea   8(%rsi,%rdi,8), %rdx\n"
    "   call  main\n"
    "   movl  %eax, %edi\n"
    "   movl  $60, %eax\n"
    "   syscall\n"
);

int main(int argc, char **argv, char **envp)
{
    // ...
}
```

Getting back to the example usage, it has some regular-looking system call
wrappers. Where do those come from? Start with this 6-argument generic
system call wrapper.

```c
long syscall6(long n, long a, long b, long c, long d, long e, long f)
{
    register long ret;
    register long r10 asm("r10") = d;
    register long r8  asm("r8")  = e;
    register long r9  asm("r9")  = f;
    __asm volatile (
        "syscall"
        : "=a"(ret)
        : "a"(n), "D"(a), "S"(b), "d"(c), "r"(r10), "r"(r8), "r"(r9)
        : "rcx", "r11", "memory"
    );
    return ret;
}
```

I could define `syscall5`, `syscall4`, etc. but instead I'll just wrap it
in macros. The former would be more efficient since the latter wastes
instructions zeroing registers for no reason, but for now I'm focused on
compacting the implementation source.

```c
#define SYSCALL1(n, a) \
    syscall6(n,(long)(a),0,0,0,0,0)
#define SYSCALL2(n, a, b) \
    syscall6(n,(long)(a),(long)(b),0,0,0,0)
#define SYSCALL3(n, a, b, c) \
    syscall6(n,(long)(a),(long)(b),(long)(c),0,0,0)
#define SYSCALL4(n, a, b, c, d) \
    syscall6(n,(long)(a),(long)(b),(long)(c),(long)(d),0,0)
#define SYSCALL5(n, a, b, c, d, e) \
    syscall6(n,(long)(a),(long)(b),(long)(c),(long)(d),(long)(e),0)
#define SYSCALL6(n, a, b, c, d, e, f) \
    syscall6(n,(long)(a),(long)(b),(long)(c),(long)(d),(long)(e),(long)(f))
```

Now we can have some exits:

```c
__attribute((noreturn))
static void exit(int status)
{
    SYSCALL1(SYS_exit, status);
    __builtin_unreachable();
}

__attribute((noreturn))
static void exit_group(int status)
{
    SYSCALL1(SYS_exit_group, status);
    __builtin_unreachable();
}
```

Simplified futex wrappers:

```c
static void futex_wait(int *futex, int expect)
{
    SYSCALL4(SYS_futex, futex, FUTEX_WAIT, expect, 0);
}

static void futex_wake(int *futex)
{
    SYSCALL3(SYS_futex, futex, FUTEX_WAKE, 0x7fffffff);
}
```

And so on.

Finally I can talk about that `newstack` function. It's just a wrapper
around an anonymous memory map allocating pages from the kernel. I've
hardcoded the constants for the standard mmap allocation since they're
nothing special or unusual. The return value check is a little tricky
since a large portion of the negative range is valid, so I only want to
check for a small range of negative errnos. (Allocating a arena looks
basically the same.)

```c
static struct stack_head *newstack(long size)
{
    unsigned long p = SYSCALL6(SYS_mmap, 0, size, 3, 0x22, -1, 0);
    if (p > -4096UL) {
        return 0;
    }
    long count = size / sizeof(struct stack_head);
    return (struct stack_head *)p + count - 1;
}
```

The `aligned` attribute comes into play here: I treat the result like an
array of `stack_head` and return the last element. The attribute ensures
each individual elements is aligned.

That's it! There's not much to it other than a few thoughtful assembly
instructions. It took doing this a few times in a few different programs
before I noticed how simple it can be.


[arena]: https://www.rfleury.com/p/untangling-lifetimes-the-arena-allocator
[bufio]: /blog/2023/02/13/
[crt]: /blog/2023/02/15/
[i686]: /blog/2023/02/15/#stack-alignment-on-32-bit-x86
[ctx]: https://vimeo.com/644068002
[man]: https://man7.org/linux/man-pages/man2/clone.2.html
[raw]: /blog/2015/05/15/
[setjmp]: /blog/2023/02/12/
[signal]: /blog/2016/09/23/
[wg]: /blog/2022/10/05/
[wild]: /blog/2022/02/18/
[src]: https://github.com/skeeto/scratch/blob/master/misc/stack_head.c