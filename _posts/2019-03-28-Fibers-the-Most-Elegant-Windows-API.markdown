---
title: 'Fibers: the Most Elegant Windows API'
layout: post
date: 2019-03-28T22:26:05Z
tags: [win32, c, posix]
uuid: abad2340-99e5-4d72-857c-848e37b4af73
excerpt_separator: <!--more-->
---

*This article was discussed [on Hacker News][hn].*

The Windows API — a.k.a. Win32 — is notorious for being clunky, ugly,
and lacking good taste. Microsoft has done a pretty commendable job with
backwards compatibility, but the trade-off is that the API is filled to
the brim with historical cruft. Every hasty, poor design over the
decades is carried forward forever, and, in many cases, even built upon,
which essentially doubles down on past mistakes. POSIX certainly has its
own ugly corners, but those are the exceptions. In the Windows API,
elegance is the exception.

<!--more-->

That's why, when I recently revisited the [Fibers API][fibers], I was
pleasantly surprised. It's one of the exceptions — much cleaner than the
optional, deprecated, and now obsolete [POSIX equivalent][clash]. It's
not quite an apples-to-apples comparison since the POSIX version is
slightly more powerful, and more complicated as a result. I'll cover the
difference in this article.

For the last part of this article, I'll walk through an async/await
framework build on top of fibers. The framework allows coroutines in C
programs to await on arbitrary kernel objects.

[**Fiber Async/await Demo**][demo]

### Fibers

Windows fibers are really just [stackful][stack], symmetric coroutines.
From a different point of view, they're cooperatively scheduled threads,
which is the source of the analogous name, *fibers*. They're symmetric
because all fibers are equal, and no fiber is the "main" fiber. If *any*
fiber returns from its start routine, the program exits. (Older versions
of Wine will crash when this happens, but it was recently fixed.) It's
equivalent to the process' main thread returning from `main()`. The
initial fiber is free to create a second fiber, yield to it, then the
second fiber destroys the first.

For now I'm going to focus on the core set of fiber functions. There are
some additional capabilities I'm going to ignore, including support for
*fiber local storage*. The important functions are just these five:

```c
void *CreateFiber(size_t stack_size, void (*proc)(void *), void *arg);
void  SwitchToFiber(void *fiber);
bool  ConvertFiberToThread(void);
void *ConvertThreadToFiber(void *arg);
void  DeleteFiber(void *fiber);
```

To emphasize its simplicity, I've shown them here with more standard
prototypes than seen in their formal documentation. That documentation
uses the clunky Windows API typedefs still burdened with its 16-bit
heritage — e.g. `LPVOID` being a "long pointer" from the segmented memory
of the 8086:

* [CreateFiber][cf]
* [SwitchToFiber][s2f]
* [ConvertFiberToThread][cf2t]
* [ConvertThreadToFiber][ct2f]
* [DeleteFiber][df]

Fibers are represented using opaque, void pointers. Maybe that's a little
*too* simple since it's easy to misuse in C, but I like it. The return
values for `CreateFiber()` and `ConvertThreadToFiber()` are void pointers
since these both create fibers.

The fiber start routine returns nothing and takes a void "user pointer".
That's nearly what I'd expect, except that it would probably make more
sense for a fiber to return `int`, which is [more in line with][winmain]
`main` / `WinMain` / `mainCRTStartup` / `WinMainCRTStartup`. As I said,
when any fiber returns from its start routine, it's like returning from
the main function, so it should probably have returned an integer.

A fiber may delete itself, which is the same as exiting the thread.
However, a fiber cannot yield (e.g. `SwitchToFiber()`) to itself. That's
undefined behavior.

```c
#include <stdio.h>
#include <stdlib.h>
#include <windows.h>

void
coup(void *king)
{
    puts("Long live the king!");
    DeleteFiber(king);
    ConvertFiberToThread(); /* seize the main thread */
    /* ... */
}

int
main(void)
{
    void *king = ConvertThreadToFiber(0);
    void *pretender = CreateFiber(0, coup, king);
    SwitchToFiber(pretender);
    abort(); /* unreachable */
}
```

Only fibers can yield to fibers, but when the program starts up, there
are no fibers. At least one thread must first convert itself into a
fiber using `ConvertThreadToFiber()`, which returns the fiber object
that represents itself. It takes one argument analogous to the last
argument of `CreateFiber()`, except that there's no start routine to
accept it. The process is reversed with `ConvertFiberToThread()`.

Fibers don't belong to any particular thread and can be scheduled on any
thread *if* properly synchronized. Obviously one should never yield to the
same fiber in two different threads at the same time.

### Contrast with POSIX

The equivalent POSIX systems was context switching. It's also stackful
and symmetric, but it has just three important functions:
[`getcontext(3)`][gc], [`makecontext(3)`][msc], and
[`swapcontext`][msc].

```c
int  getcontext(ucontext_t *ucp);
void makecontext(ucontext_t *ucp, void (*func)(), int argc, ...);
int  swapcontext(ucontext_t *oucp, const ucontext_t *ucp);
```

These are roughly equivalent to [`GetCurrentFiber()`][gcf],
`CreateFiber()`, and `SwitchToFiber()`. There is no need for
`ConvertFiberToThread()` since threads can context switch without
preparation. There's also no `DeleteFiber()` because the resources are
managed by the program itself. That's where POSIX contexts are a little
bit more powerful.

The first argument to `CreateFiber()` is the desired stack size, with
zero indicating the default stack size. The stack is allocated and freed
by the operating system. The downside is that the caller doesn't have a
choice in managing the lifetime of this stack and how it's allocated. If
you're frequently creating and destroying coroutines, those stacks are
constantly being allocated and freed.

In `makecontext(3)`, the caller allocates and supplies the stack. Freeing
that stack is equivalent to destroying the context. A program that
frequently creates and destroys contexts can maintain a stack pool or
otherwise more efficiently manage their allocation. This makes it more
powerful, but it also makes it a little more complicated. It would be hard
to remember how to do all this without a careful reading of the
documentation:

```c
/* Create a context */
ucontext_t ctx;
ctx.uc_stack.ss_sp = malloc(SIGSTKSZ);
ctx.uc_stack.ss_size = SIGSTKSZ;
ctx.uc_link = 0;
getcontext(&ctx);
makecontext(&ctx, proc, 0);

/* Destroy a context */
free(ctx.uc_stack.ss_sp);
```

Note how `makecontext(3)` is variadic (`...`), passing its arguments on
to the start routine of the context. This seems like it might be better
than a user pointer. Unfortunately it's not, since those arguments are
strictly limited to *integers*.

Ultimately I like the fiber API better. The first time I tried it out, I
could guess my way through it without looking closely at the
documentation.

### Async / await with fibers

Why was I looking at the Fiber API? I've known about coroutines for
years but I didn't understand how they could be useful. Sure, the
function can yield, but what other coroutine should it yield to? It
wasn't until I was [recently bit by the async/await bug][aio] that I
finally saw a "killer feature" that justified their use. Generators come
pretty close, though.

Windows fibers are a coroutine primitive suitable for async/await in C
programs, where [it can also be useful][ssh]. To prove that it's
possible, I built async/await on top of fibers in [95 lines of code][c].

The alternatives are to use a [third-party coroutine library][pth] or to
do it myself [with some assembly programming][raw]. However, having it
built into the operating system is quite convenient! It's unfortunate
that it's limited to Windows. Ironically, though, everything I wrote for
this article, including the async/await demonstration, was originally
written on Linux using Mingw-w64 and tested using [Wine][wine]. Only
after I was done did I even try it on Windows.

Before diving into how it works, there's a general concept about the
Windows API that must be understood: **All kernel objects can be in
either a signaled or unsignaled state.** The API provides functions that
block on a kernel object until it is signaled. The two important ones
are [`WaitForSingleObject()`][wfso] and [`WaitForMultipleObjects()`][wfmo].
The latter behaves very much like `poll(2)` in POSIX.

Usually the signal is tied to some useful event, like a process or
thread exiting, the completion of an I/O operation (i.e. asynchronous
overlapped I/O), a semaphore being incremented, etc. It's a generic way
to wait for some event. **However, instead of blocking the thread,
wouldn't it be nice to *await* on the kernel object?** In my `aio`
library for Emacs, the fundamental "wait" object was a promise. For this
API it's a kernel object handle.

So, the await function will take a kernel object, register it with the
scheduler, then yield to the scheduler. The scheduler — which is a
global variable, so there's only one scheduler per process — looks like
this:

```c
struct {
    void *main_fiber;
    HANDLE handles[MAXIMUM_WAIT_OBJECTS];
    void *fibers[MAXIMUM_WAIT_OBJECTS];
    void *dead_fiber;
    int count;
} async_loop;
```

While fibers are symmetric, coroutines in my async/await implementation
are not. One fiber is the scheduler, `main_fiber`, and the other fibers
always yield to it.

There is an array of kernel object handles, `handles`, and an array of
`fibers`. The elements in these arrays are paired with each other, but
it's convenient to store them separately, as I'll show soon. `fibers[0]`
is waiting on `handles[0]`, and so on.

The array is a fixed size, `MAXIMUM_WAIT_OBJECTS` (64), because there's
a hard limit on the number of fibers that can wait at once. This
pathetically small limitation is an unfortunate, hard-coded restriction
of the Windows API. It kills most practical uses of my little library.
Fortunately there's no limit on the number of handles we might want to
wait on, just the number of co-existing fibers.

When a fiber is about to return from its start routine, it yields one
last time and registers itself on the `dead_fiber` member. The scheduler
will delete this fiber as soon as it's given control. Fibers never
*truly* return since that would terminate the program.

With this, the await function, `async_await()`, is pretty simple. It
registers the handle with the scheduler, then yields to the scheduler
fiber.

```c
void
async_await(HANDLE h)
{
    async_loop.handles[async_loop.count] = h;
    async_loop.fibers[async_loop.count] = GetCurrentFiber();
    async_loop.count++;
    SwitchToFiber(async_loop.main_fiber);
}
```

Caveat: The scheduler destroys this handle with `CloseHandle()` after it
signals, so don't try to reuse it. This made my demonstration simpler,
but it might be better to not do this.

A fiber can exit at any time. Such an exit is inserted implicitly before
a fiber actually returns:

```c
void
async_exit(void)
{
    async_loop.dead_fiber = GetCurrentFiber();
    SwitchToFiber(async_loop.main_fiber);
}
```

The start routine given to `async_start()` is actually wrapped in the
real start routine. This is how `async_exit()` is injected:

```c
struct fiber_wrapper {
    void (*func)(void *);
    void *arg;
};

static void
fiber_wrapper(void *arg)
{
    struct fiber_wrapper *fw = arg;
    fw->func(fw->arg);
    async_exit();
}

int
async_start(void (*func)(void *), void *arg)
{
    if (async_loop.count == MAXIMUM_WAIT_OBJECTS) {
        return 0;
    } else {
        struct fiber_wrapper fw = {func, arg};
        SwitchToFiber(CreateFiber(0, fiber_wrapper, &fw));
        return 1;
    }
}
```

The library provides a single awaitable function, `async_sleep()`. It
creates a "waitable timer" object, starts the countdown, and returns it.
(Notice how `SetWaitableTimer()` is a typically-ugly Win32 function with
excessive parameters.)

```c
HANDLE
async_sleep(double seconds)
{
    HANDLE promise = CreateWaitableTimer(0, 0, 0);
    LARGE_INTEGER t;
    t.QuadPart = (long long)(seconds * -10000000.0);
    SetWaitableTimer(promise, &t, 0, 0, 0, 0);
    return promise;
}
```

A more realistic example would be overlapped I/O. For example, you'd
open a file (`CreateFile()`) in overlapped mode, then when you, say,
read from that file (`ReadFile()`) you create an event object
(`CreateEvent()`), populate an overlapped I/O structure with the event,
offset, and length, then finally await on the event object. The fiber
will be resumed when the operation is complete.

Side note: Unfortunately [overlapped I/O doesn't work correctly for
files][thr], and many operations can't be done asynchronously, like
opening files. When it comes to files, you're [better off using
dedicated threads][thr] as [libuv does][uv] instead of overlapped I/O.
You can still await on these operations. You'd just await on the signal
from the thread doing synchronous I/O, not from overlapped I/O.

The most complex part is the scheduler, and it's really not complex at
all:

```c
void
async_run(void)
{
    while (async_loop.count) {
        /* Wait for next event */
        DWORD nhandles = async_loop.count;
        HANDLE *handles = async_loop.handles;
        DWORD r = WaitForMultipleObjects(nhandles, handles, 0, INFINITE);

        /* Remove event and fiber from waiting array */
        void *fiber = async_loop.fibers[r];
        CloseHandle(async_loop.handles[r]);
        async_loop.handles[r] = async_loop.handles[nhandles - 1];
        async_loop.fibers[r] = async_loop.fibers[nhandles - 1];
        async_loop.count--;

        /* Run the fiber */
        SwitchToFiber(fiber);

        /* Destroy the fiber if it exited */
        if (async_loop.dead_fiber) {
            DeleteFiber(async_loop.dead_fiber);
            async_loop.dead_fiber = 0;
        }
    }
}
```

This is why the handles are in their own array. The array can be passed
directly to `WaitForMultipleObjects()`. The return value indicates which
handle was signaled. The handle is closed, the entry removed from the
scheduler, and then the fiber is resumed.

That `WaitForMultipleObjects()` is what limits the number of fibers.
It's not possible to wait on more than 64 handles at once! This is
hard-coded into the API. How? A return value of 64 is an error code, and
changing this would break the API. Remember what I said about being
locked into bad design decisions of the past?

To be fair, `WaitForMultipleObjects()` was a doomed API anyway, just
like `select(2)` and `poll(2)` in POSIX. It scales very poorly since the
entire array of objects being waited on must be traversed on each call.
That's terribly inefficient when waiting on large numbers of objects.
This sort of problem is solved by interfaces like kqueue (BSD), epoll
(Linux), and IOCP (Windows). Unfortunately [IOCP doesn't really fit this
particular problem well][iocp] — awaiting on kernel objects — so I
couldn't use it.

When the awaiting fiber count is zero and the scheduler has control, all
fibers must have completed and there's nothing left to do. However, the
caller can schedule more fibers and then restart the scheduler if
desired.

That's all there is to it. Have a look at [`demo.c`][democ] to see how
the API looks in some trivial examples. On Linux you can see it in
action with `make check`. On Windows, you just [need to compile
it][four], then run it like a normal program. If there was a better
function than `WaitForMultipleObjects()` in the Windows API, I would
have considered turning this demonstration into a real library.


[aio]: /blog/2019/03/10/
[c]: https://github.com/skeeto/fiber-await/blob/master/async.c
[cf2t]: https://docs.microsoft.com/en-us/windows/desktop/api/winbase/nf-winbase-convertfibertothread
[cf]: https://docs.microsoft.com/en-us/windows/desktop/api/WinBase/nf-winbase-createfiber
[clash]: /blog/2017/06/21/#coroutines
[ct2f]: https://docs.microsoft.com/en-us/windows/desktop/api/WinBase/nf-winbase-convertthreadtofiber
[demo]: https://github.com/skeeto/fiber-await
[democ]: https://github.com/skeeto/fiber-await/blob/master/demo.c
[df]: https://docs.microsoft.com/en-us/windows/desktop/api/WinBase/nf-winbase-deletefiber
[fibers]: https://docs.microsoft.com/en-us/windows/desktop/procthread/fibers
[four]: /blog/2016/06/13/
[gc]: http://man7.org/linux/man-pages/man3/setcontext.3.html
[gcf]: https://docs.microsoft.com/en-us/windows/desktop/api/winnt/nf-winnt-getcurrentfiber
[hn]: https://news.ycombinator.com/item?id=19520078
[iocp]: https://news.ycombinator.com/item?id=11866562
[msc]: http://man7.org/linux/man-pages/man3/makecontext.3.html
[pth]: https://www.gnu.org/software/pth/
[raw]: /blog/2015/05/15/
[s2f]: https://docs.microsoft.com/en-us/windows/desktop/api/WinBase/nf-winbase-switchtofiber
[ssh]: /blog/2019/03/22/
[stack]: https://blog.varunramesh.net/posts/stackless-vs-stackful-coroutines/
[sync]: https://support.microsoft.com/en-us/help/156932/asynchronous-disk-i-o-appears-as-synchronous-on-windows
[thr]: https://blog.libtorrent.org/2012/10/asynchronous-disk-io/
[uv]: http://docs.libuv.org/en/v1.x/design.html#file-i-o
[wfmo]: https://docs.microsoft.com/en-us/windows/desktop/api/synchapi/nf-synchapi-waitformultipleobjects
[wfso]: https://docs.microsoft.com/en-us/windows/desktop/api/synchapi/nf-synchapi-waitforsingleobject
[wine]: https://www.winehq.org/
[winmain]: /blog/2016/01/31/
