---
title: How to build a WaitGroup from a 32-bit integer
layout: post
date: 2022-10-05T03:19:07Z
tags: [c, linux, win32]
uuid: cc83b101-2d77-42b8-b409-d4ed36831479
---

Go has a nifty synchronization utility called a [WaitGroup][], on which
one or more goroutines can wait for concurrent task completion. In other
languages, the usual task completion convention is *joining* threads doing
the work. In Go, goroutines aren't values and lack handles, so a WaitGroup
replaces joins. Building a WaitGroup using typical, portable primitives is
a messy affair involving constructors and destructors, managing lifetimes.
However, on at least Linux and Windows, we can build a WaitGroup out of a
zero-initialized integer, much like my [32-bit queue][q] and [32-bit
barrier][b].

In case you're not familiar with it, a typical WaitGroup use case in Go:

```go
var wg sync.WaitGroup
for _, task := range tasks {
    wg.Add(1)
    go func(t Task) {
        // ... do task ...
        wg.Done()
    }(task)
}
wg.Wait()
```

I zero-initialize the WaitGroup, the main goroutine increments the counter
before starting each task goroutine, each goroutine decrements the counter
when done, and the main goroutine waits until the counter reaches zero. My
goal is to build the same mechanism in C:

```c
void workfunc(task t, int *wg)
{
    // ... do task ...
    waitgroup_done(wg);
}

int main(void)
{
    // ...
    int wg = 0;
    for (int i = 0; i < ntasks; i++) {
        waitgroup_add(&wg, 1);
        go(workfunc, tasks[i], &wg);
    }
    waitgroup_wait(&wg);
    // ...
}
```

When it's done, the WaitGroup is back to zero, and no cleanup is required.

I'm going to take it a little further than that: Since its meaning and
contents are explicit, you may initialize a WaitGroup to any non-negative
task count! In other words, `waitgroup_add` is optional if the total
number of tasks is known up front.

```c
    int wg = ntasks;
    for (int i = 0; i < ntasks; i++) {
        go(workfunc, tasks[i], &wg);
    }
    waitgroup_wait(&wg);
```

A sneak peek at the full source: **[`waitgroup.c`][c]**

### The four elements (of synchronization)

To build this WaitGroup, we're going to need four primitives from the host
platform, each operating on an `int`. The first two are atomic operations,
and the second two interact with the system scheduler. To port the
WaitGroup to a platform you need only implement these four functions,
typically as one-liners.

```c
static int  load(int *);           // atomic load
static int  addfetch(int *, int);  // atomic add-then-fetch
static void wait(int *, int);      // wait on change at address
static void wake(int *);           // wake all waiters by address
```

The first two should be self-explanatory. The `wait` function waits for
the pointed-at integer to change its value, and the second argument is its
expected current value. The scheduler will double-check the integer before
putting the thread to sleep in case it changes at the last moment — in
other words, an atomic check-then-maybe-sleep. The `wake` function is the
other half. After changing the integer, a thread uses it to wake all
threads waiting for the pointed-at integer to change. Together, this
mechanism is known as a *futex*.

I'm going to simplify the WaitGroup semantics a bit in order to make my
implementation even simpler. Go's WaitGroup allows adding negatives, and
the `Add` method essentially does double-duty. My version forbids adding
negatives. That means the "add" operation is just an atomic increment:

```c
void waitgroup_add(int *wg, int delta)
{
    addfetch(wg, delta);
}
```

Since it cannot bring the counter to zero, there's nothing else to do. The
"done" operation *can* decrement to zero:

```c
void waitgroup_done(int *wg)
{
    if (!addfetch(wg, -1)) {
        wake(wg);
    }
}
```

If the atomic decrement brought the count to zero, we finished the last
task, so we need to wake the waiters. We don't know if anyone is actually
waiting, but that's fine. Some futex use cases will avoid making the
relatively expensive system call if nobody's waiting — i.e. don't waste
time on a system call for each unlock of an uncontended mutex — but in the
typical WaitGroup case we *expect* a waiter when the count finally goes to
zero. That's the common case.

The most complicated of the three is waiting:

```c
void waitgroup_wait(int *wg)
{
    for (;;) {
        int c = load(wg);
        if (!c) {
            break;
        }
        wait(wg, c);
    }
}
```

First check if the count is already zero and return if it is. Otherwise
use the futex to *wait for it to change*. Unfortunately that's not exactly
the semantics we want, which would be to wait for a certain target. This
doesn't break the wait, but it's a potential source of inefficiency. If a
thread finishes a task between our load and wait, we don't go to sleep,
and instead try again. However, in practice, I ran thousands of threads
through this thing concurrently and I couldn't observe such a "miss." As
far as I can tell, it's so rare it doesn't matter.

If this was a concern, the WaitGroup could instead be a pair of integers:
the counter and a "latch" that is either 0 or 1. Waiters wait on the
latch, and the latch is modified (atomically) when the counter transitions
to or from zero. That gives waiters a stable value on which to wait,
proxying the counter. However, since this doesn't seem to matter in
practice, I prefer the elegance and simplicity of the single-integer
WaitGroup.

### Four elements: Linux

With the WaitGroup done at a high level, we now need the per-platform
parts. Both GCC and Clang support [GNU-style atomics][gcc], so I'll just
assume these are available on Linux without worrying about the compiler.
The first two functions wrap these built-ins:

```c
static int load(int *p)
{
    return __atomic_load_n(p, __ATOMIC_SEQ_CST);
}

static int addfetch(int *p, int addend)
{
    return __atomic_add_fetch(p, addend, __ATOMIC_SEQ_CST);
}
```

For `wait` and `wake` we need the [`futex(2)` system call][futex]. In an
attempt to discourage its direct use, glibc doesn't wrap this system call
in a function, so we must make the system call ourselves.

```c
static void wait(int *p, int current)
{
    syscall(SYS_futex, p, FUTEX_WAIT, current, 0, 0, 0);
}

static void wake(int *p)
{
    syscall(SYS_futex, p, FUTEX_WAKE, INT_MAX, 0, 0, 0);
}
```

The `INT_MAX` means "wake as many as possible." The other common value is
1 for waking a single waiter. Also, these system calls can't meaningfully
fail, so there's no need to check the return value. If `wait` wakes up
early (e.g. `EINTR`), it's going to check the counter again anyway. In
fact, if your kernel is more than 20 years old, predating futexes, and
returns `ENOSYS` ("Function not implemented"), it will *still* work
correctly, though it will be incredibly inefficient.

### Four elements: Windows

Windows didn't support futexes until Windows 8 in 2012, and were still
supporting Windows without it into 2020, so they're still relatively "new"
for this platform. Nonetheless, they're now mature enough that we can
count on them being available.

I'd like to support both GCC-ish ([via Mingw-w64][w64]) and MSVC-ish
compilers. Mingw-w64 provides a compatible `intrin.h`, so I can stick to
MSVC-style atomics and cover both at once. On the other hand, MSVC doesn't
define atomics for `int` (or even `int32_t`), strictly `long`, so I have
to sneak in a little cast. (Recall: `sizeof(long) == sizeof(int)` on every
version of Windows supporting futexes.) The other option is to `typedef`
the WaitGroup so that it's `int` on Linux (for the futex) and `long` on
Windows (for atomics).

```c
static int load(int *p)
{
    return _InterlockedOr((long *)p, 0);
}

static int addfetch(int *p, int addend)
{
    return addend + _InterlockedExchangeAdd((long *)p, addend);
}
```

The official, sanctioned futex functions are [WaitOnAddress][] and
[WakeByAddressAll][]. They [used to be in `kernel32.dll`][move], but as of
this writing they live in `API-MS-Win-Core-Synch-l1-2-0.dll`, linked via
`-lsynchronization`. Gross. Since I can't stomach this, I instead call the
low-level RTL functions where it's actually implemented: RtlWaitOnAddress
and RtlWakeAddressAll. These live in the nice neighborhood of `ntdll.dll`.
They're undocumented as far as I can tell, but thankfully [Wine comes to
the rescue][wine], providing both documentation and several different
implementations. Reading through it is educational, and hints at ways to
construct futexes on systems lacking them.

These functions aren't declared in any headers, so I have to do it myself.
On the plus side, so far I haven't paid the substantial compile-time costs
of [including `windows.h`][h], and so I can continue avoiding it. These
functions *are* listed in the `ntdll.dll` import library, so I don't need
to [invent the import library entries][dll].

```c
__declspec(dllimport)
long __stdcall RtlWaitOnAddress(void *, void *, size_t, void *);
__declspec(dllimport)
long __stdcall RtlWakeAddressAll(void *);
```

Rather conveniently, the semantics perfectly line up with Linux futexes!

```c
static void wait(int *p, int current)
{
    RtlWaitOnAddress(p, &current, sizeof(*p), 0);
}

static void wake(int *p)
{
    RtlWakeAddressAll(p);
}
```

Like with Linux, there's no meaningful failure, so the return values don't
matter.

That's the whole implementation. Considering just a single platform, a
flexible, lightweight, and easy-to-use synchronization facility in ~50
lines of relatively simple code is a pretty good deal if you ask me!


[b]: /blog/2022/03/13/
[c]: https://github.com/skeeto/scratch/blob/master/misc/waitgroup.c
[dll]: /blog/2021/05/31/
[futex]: https://man7.org/linux/man-pages/man2/futex.2.html
[gcc]: https://gcc.gnu.org/onlinedocs/gcc-12.2.0/gcc/_005f_005fatomic-Builtins.html
[h]: https://web.archive.org/web/20090912002357/http://www.tilander.org/aurora/2008/01/include-windowsh.html
[move]: https://sourceforge.net/p/mingw-w64/mailman/mingw-w64-public/thread/CALK-3m%2B6tX_ubMVGV7NarAm6VH0AoOp5THyXfEUA%3DTjyu5L%3Dxw%40mail.gmail.com/
[q]: /blog/2022/05/14/ 
[w64]: https://github.com/skeeto/w64devkit
[WaitGroup]: https://godocs.io/sync#WaitGroup
[WaitOnAddress]: https://learn.microsoft.com/en-us/windows/win32/api/synchapi/nf-synchapi-waitonaddress
[WakeByAddressAll]: https://learn.microsoft.com/en-us/windows/win32/api/synchapi/nf-synchapi-wakebyaddressall
[wine]: https://github.com/wine-mirror/wine/blob/master/dlls/ntdll/sync.c
