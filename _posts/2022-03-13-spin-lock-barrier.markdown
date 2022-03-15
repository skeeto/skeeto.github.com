---
title: A flexible, lightweight, spin-lock barrier
layout: post
date: 2022-03-13T23:55:08Z
tags: [c, cpp, go, x86, optimization]
uuid: 5a72d27a-60f4-4b52-a4c2-f1c3b72e6c85
excerpt_separator: <!--more-->
---

*This article was discussed [on Hacker News][hn].*

The other day I wanted try the famous [memory reordering experiment][act]
for myself. It's the double-slit experiment of concurrency, where a
program can observe an ["impossible" result][mm] on common hardware, as
though a thread had time-traveled. While getting thread timing as tight as
possible, I designed a possibly-novel thread barrier. It's purely
spin-locked, the entire footprint is a zero-initialized integer, it
automatically resets, it can be used across processes, and the entire
implementation is just three to four lines of code.

<!--more-->

Here's the entire barrier implementation for two threads in C11.

```c
// Spin-lock barrier for two threads. Initialize *barrier to zero.
void barrier_wait(_Atomic uint32_t *barrier)
{
    uint32_t v = ++*barrier;
    if (v & 1) {
        for (v &= 2; (*barrier&2) == v;);
    }
}
```

Or in Go:

```go
func BarrierWait(barrier *uint32) {
    v := atomic.AddUint32(barrier, 1)
    if v&1 == 1 {
        v &= 2
        for atomic.LoadUint32(barrier)&2 == v {
        }
    }
}
```

Even more, these two implementations are compatible with each other. C
threads and Go goroutines can synchronize on a common barrier using these
functions. Also note how it only uses two bits.

When I was done with my experiment, I did a quick search online for other
spin-lock barriers to see if anyone came up with the same idea. I found a
couple of [subtly-incorrect][bug] spin-lock barriers, and some
straightforward barrier constructions using a mutex spin-lock.

Before diving into how this works, and how to generalize it, let's discuss
the circumstance that let to its design.

### Experiment

Here's the setup for the memory reordering experiment, where `w0` and `w1`
are initialized to zero.

    thread#1    thread#2
    w0 = 1      w1 = 1
    r1 = w1     r0 = w0

Considering all the possible orderings, it would seem that at least one of
`r0` or `r1` is 1. There seems to be no ordering where `r0` and `r1` could
both be 0. However, if raced precisely, this is a frequent or possibly
even majority occurrence on common hardware, including x86 and ARM.

How to go about running this experiment? These are concurrent loads and
stores, so it's tempting to use `volatile` for `w0` and `w1`. However,
this would constitute a data race — undefined behavior in at least C and
C++ — and so we couldn't really reason much about the results, at least
not without first verifying the compiler's assembly. These are variables
in a high-level language, not architecture-level stores/loads, even with
`volatile`.

So my first idea was to use a bit of inline assembly for all accesses that
would otherwise be data races. x86-64:

```c
static int experiment(int *w0, int *w1)
{
    int r1;
    __asm volatile (
        "movl  $1, %1\n"
        "movl  %2, %0\n"
        : "=r"(r1), "=m"(*w0)
        : "m"(*w1)
    );
    return r1;
}
```

ARM64 (to try on my Raspberry Pi):

```c
static int experiment(int *w0, int *w1)
{
    int r1 = 1;
    __asm volatile (
        "str  %w0, %1\n"
        "ldr  %w0, %2\n"
        : "+r"(r1), "=m"(w0)
        : "m"(w1)
    );
    return r1;
}
```

This is from the point-of-view of thread#1, but I can swap the arguments
for thread#2. I'm expecting this to be inlined, and encouraging it with
`static`.

Alternatively, I could use C11 atomics with a relaxed memory order:

```c
static int experiment(_Atomic int *w0, _Atomic int *w1)
{
    atomic_store_explicit(w0, 1, memory_order_relaxed);
    return atomic_load_explicit(w1, memory_order_relaxed);
}
```

Since this is a *race* and I want both threads to run their two experiment
instructions as simultaneously as possible, it would be wise to use some
sort of *starting barrier*… exactly the purpose of a thread barrier! It
will hold the threads back until they're both ready.

```c
int w0, w1, r0, r1;

// thread#1                   // thread#2
w0 = w1 = 0;
BARRIER;                      BARRIER;
r1 = experiment(&w0, &w1);    r0 = experiment(&w1, &w0);
BARRIER;                      BARRIER;

if (!r0 && !r1) {
    puts("impossible!");
}
```

The second thread goes straight into the barrier, but the first thread
does a little more work to initialize the experiment and a little more at
the end to check the result. The second barrier ensures they're both done
before checking.

Running this only once isn't so useful, so each thread loops a few million
times, hence the re-initialization in thread#1. The barriers keep them
lockstep.

### Barrier selection

On my first attempt, I made the obvious decision for the barrier: I used
[`pthread_barrier_t`][pthr]. I was already using pthreads for spawning the
extra thread, including [on Windows][w64], so this was convenient.

However, my initial results were disappointing. I only observed an
"impossible" result around one in a million trials. With some debugging I
determined that the pthreads barrier was just too damn slow, throwing off
the timing. This was especially true with winpthreads, bundled with
Mingw-w64, which in addition to the per-barrier mutex, grabs a *global*
lock *twice* per wait to manage the barrier's reference counter.

All pthreads implementations I used were quick to yield to the system
scheduler. The first thread to arrive at the barrier would go to sleep,
the second thread would wake it up, and it was rare they'd actually race
on the experiment. This is perfectly reasonable for a pthreads barrier
designed for the general case, but I really needed a *spin-lock barrier*.
That is, the first thread to arrive spins in a loop until the second
thread arrives, and it never interacts with the scheduler. This happens so
frequently and quickly that it should only spin for a few iterations.

### Barrier design

Spin locking means atomics. By default, atomics have sequentially
consistent ordering and will provide the necessary synchronization for the
non-atomic experiment variables. Stores (e.g. to `w0`, `w1`) made before
the barrier will be visible to all other threads upon passing through the
barrier. In other words, the initialization will propagate before either
thread exits the first barrier, and results propagate before either thread
exits the second barrier.

I know statically that there are only two threads, simplifying the
implementation. The plan: When threads arrive, they atomically increment a
shared variable to indicate such. The first to arrive will see an odd
number, telling it to atomically read the variable in a loop until the
other thread changes it to an even number.

At first with just two threads this might seem like a single bit would
suffice. If the bit is set, the other thread hasn't arrived. If clear,
both threads have arrived.

```c
void broken_wait1(_Atomic unsigned *barrier)
{
    ++*barrier;
    while (*barrier&1);
}
```
Or to avoid an extra load, use the result directly:

```c
void broken_wait2(_Atomic unsigned *barrier)
{
    if (++*barrier & 1) {
        while (*barrier&1);
    }
}
```

Neither of these work correctly, and the other mutex-free barriers I found
all have the same defect. Consider the broader picture: Between atomic
loads in the first thread spin-lock loop, suppose the second thread
arrives, passes through the barrier, does its work, hits the next barrier,
and increments the counter. Both threads see an odd counter simultaneously
and deadlock. No good.

To fix this, the wait function must also track the *phase*. The first
barrier is the first phase, the second barrier is the second phase, etc.
Conveniently **the rest of the integer acts like a phase counter**!
Writing this out more explicitly:

```c
void barrier_wait(_Atomic unsigned *barrier)
{
    unsigned observed = ++*barrier;
    unsigned thread_count = observed & 1;
    if (thread_count != 0) {
        // not last arrival, watch for phase change
        unsigned init_phase = observed >> 1;
        for (;;) {
            unsigned current_phase = *barrier >> 1;
            if (current_phase != init_phase) {
                break;
            }
        }
    }
}
```

The key: When the last thread arrives, it overflows the thread counter to
zero and increments the phase counter in one operation.

By the way, I'm using `unsigned` since it may eventually overflow, and
even `_Atomic int` overflow is undefined for the `++` operator. However,
if you use `atomic_fetch_add` or C++ `std::atomic` then overflow is
defined and you can use `int`.

Threads can never be more than one phase apart by definition, so only one
bit is needed for the phase counter, making this effectively a two-phase,
two-bit barrier. In my final implementation, rather than shift (`>>`), I
mask (`&`) the phase bit with 2.

With this spin-lock barrier, the experiment observes `r0 = r1 = 0` in ~10%
of trials on my x86 machines and ~75% of trials on my Raspberry Pi 4.

### Generalizing to more threads

Two threads required two bits. This generalizes to `log2(n)+1` bits for
`n` threads, where `n` is a power of two. You may have already figured out
how to support more threads: spend more bits on the thread counter.

```c
// Spin-lock barrier for n threads, where n is a power of two.
// Initialize *barrier to zero.
void barrier_waitn(_Atomic unsigned *barrier, int n)
{
    unsigned v = ++*barrier;
    if (v & (n - 1)) {
        for (v &= n; (*barrier&n) == v;);
    }
}
```

Note: **It never makes sense for `n` to exceed the logical core count!**
If it does, then at least one thread must not be actively running. The
spin-lock ensures it does not get scheduled promptly, and the barrier will
waste lots of resources doing nothing in the meantime.

If the barrier is used little enough that you won't overflow the overall
barrier integer — maybe just use a `uint64_t` — an implementation could
support arbitrary thread counts with the same principle using modular
division instead of the `&` operator. The denominator is ideally a
compile-time constant in order to avoid paying for division in the
spin-lock loop.

While C11 `_Atomic` seems like it would be useful, unsurprisingly it is
not supported by one major, [stubborn][sane] implementation. If you're
using C++11 or later, then go ahead use `std::atomic<int>` since it's
well-supported. In real, practical C programs, I will continue using dual
implementations: interlocked functions on MSVC, and GCC built-ins (also
supported by Clang) everywhere else.

```c
#if __GNUC__
#  define BARRIER_INC(x) __atomic_add_fetch(x, 1, __ATOMIC_SEQ_CST)
#  define BARRIER_GET(x) __atomic_load_n(x, __ATOMIC_SEQ_CST)
#elif _MSC_VER
#  define BARRIER_INC(x) _InterlockedIncrement(x)
#  define BARRIER_GET(x) _InterlockedOr(x, 0)
#endif

// Spin-lock barrier for n threads, where n is a power of two.
// Initialize *barrier to zero.
static void barrier_wait(int *barrier, int n)
{
    int v = BARRIER_INC(barrier);
    if (v & (n - 1)) {
        for (v &= n; (BARRIER_GET(barrier)&n) == v;);
    }
}
```

This has the nice bonus that the interface does not have the `_Atomic`
qualifier, nor `std::atomic` template. It's just a plain old `int`, making
the interface simpler and easier to use. It's something I've grown to
appreciate from Go.

If you'd like to try the experiment yourself: [`reorder.c`][gist]. If
you'd like to see a test of Go and C sharing a thread barrier:
[`coop.go`][coop].

I'm intentionally not providing the spin-lock barrier as a library. First,
it's too trivial and small for that, and second, I believe [context is
everything][ctx]. Now that you understand the principle, you can whip up
your own, custom-tailored implementation when the situation calls for it,
just as the one in my experiment is hard-coded for exactly two threads.


[act]: https://preshing.com/20120515/memory-reordering-caught-in-the-act/
[bug]: https://stackoverflow.com/questions/33598686/spinning-thread-barrier-using-atomic-builtins
[coop]: https://gist.github.com/skeeto/bdb5a0d2aa36b68b6f66ca39989e1444
[ctx]: https://vimeo.com/644068002
[gist]: https://gist.github.com/skeeto/c63b9ddf2c599eeca86356325b93f3a7
[hn]: https://news.ycombinator.com/item?id=30671979
[mm]: https://research.swtch.com/hwmm
[pthr]: https://pubs.opengroup.org/onlinepubs/9699919799/functions/pthread_barrier_wait.html
[sane]: /blog/2021/12/30/
[w64]: /blog/2020/05/15/
