---
title: Illuminating synchronization edges for ThreadSanitizer
layout: post
date: 2022-10-03T03:09:38Z
tags: [c, linux]
uuid: a008900c-cf6a-46e2-8657-21bded194350
---

[Sanitizers][] are powerful development tools which complement
[debuggers][] and [fuzzing][]. I typically have at least one sanitizer
active during development. They're particularly useful during code review,
where they can identify issues before I've even begun examining the code
carefully — sometimes in mere minutes under fuzzing. Accordingly, it's a
good idea to have your own code in good agreement with sanitizers before
review. For ThreadSanitizer (TSan), that means dealing with false
positives in programs relying on synchronization invisible to TSan.

This article's motivation is multi-threaded [epoll][]. I mitigate TSan
false positives each time it comes up, enough to have gotten the hang of
it, so I ought to document it. [On Windows][w64] I would also run into the
same issue with the Win32 message queue, crossing the synchronization edge
between [PostMessage][] (release) and [GetMessage][] (acquire), *except*
for the general lack of TSan support in Windows tooling. The same
technique would work there as well.

My typical epoll scenario looks like so:

1. Create an epoll file descriptor (`epoll_create1`).
2. Create worker threads, passing the epoll file descriptor.
3. Worker threads loop on `epoll_wait`.
4. Main thread loops on `accept`, adding sockets to epoll (`epoll_ctl`).

Between `accept` and `EPOLL_CTL_ADD`, the main thread allocates and
initializes the client session state, then attaches it to the epoll event.
The client socket is added with [the `EPOLLONESHOT` flag][os], and the
session state is not touched after the call to `epoll_ctl` (note: sans
error checks):

```c
for (;;) {
    int fd = accept(...);
    struct session *session = ...;
    session->fd = fd;
    // ...
    struct epoll_event;
    event.events = EPOLLET | EPOLLONESHOT | ...;
    event.events.data.ptr = session;
    epoll_ctl(epfd, EPOLL_CTL_ADD, fd, &event);
}
```

In this example, `struct session` is defined by the application to contain
all the state for handling a session (file descriptor, buffers, [state
machine][], parser state, [allocation arena][arena], etc.). Everything
else is part of the epoll interface.

When a socket is ready, one of the worker threads receive it. Due to
`EPOLLONESHOT`, it's immediately disabled and no other thread can receive
it. The thread does as much work as possible (i.e. read/write until
`EAGAIN`), then reactivates it with `epoll_ctl`:

```c
for (;;) {
    struct epoll_event event;
    epoll_wait(epfd, &event, 1, -1);
    struct session *session = event.data.ptr;
    int fd = session->fd;
    // ...
    event.events = EPOLLET | EPOLLONESHOT | ...;
    epoll_ctl(epfd, EPOLL_CTL_MOD, fd, &event);
}
```

The shared variables in `session` are passed between threads through
`epoll` using the event's `.user.ptr`. These variables are potentially
read and mutated by every thread, but it's all perfectly safe without any
further synchronization — i.e. no need for mutexes, etc. All the necessary
synchronization is implicit in epoll.

In the initial hand-off, that `EPOLL_CTL_ADD` must *happen before* the
corresponding `epoll_wait` in a worker thread. This establishes that the
main thread and worker thread do not touch session variables concurrently.
After all, how could the worker see an event on the file descriptor before
it's been added to epoll? The synchronization in epoll itself will also
ensure all the architecture-level stores are visible to other threads
before the hand-off. We can call the "add" a *release* and the "wait" an
*acquire*, forming a synchronization edge.

Similarly, in the hand-off between worker threads, the `EPOLL_CTL_MOD`
that reactivates the file descriptor must *happen before* the wait that
observes the next event because, until reactivation, it's disabled. The
`EPOLL_CTL_MOD` is another *release* in relation to the *acquire* wait.

Unfortunately TSan won't see things this way. It can't see into the
kernel, and it doesn't know these subtle epoll semantics, so it can't see
these synchronization edges. As far [as it can tell][vc], threads might be
accessing a session concurrently, and TSan will reliably produce warnings
about it. You could shrug your shoulders and give up on using TSan in this
case, but there's an easy solution: introduce redundant, semantically
identical synchronization edges, but only when TSan is looking.

    WARNING: ThreadSanitizer: data race

### Redundant synchronization

I prefer to solve this by introducing the weakest possible synchronization
so that I'm not synchronizing beyond epoll's semantics. This will help
TSan catch real mistakes that stronger synchronization might hide.

The weakest option is memory fences. These wouldn't introduce extra loads
or stores. At most it would be a fence instruction. I would use [GCC's
built-in `__atomic_thread_fence`][gcc] for the job. However, TSan does not
currently understand thread fences, so that defeats the purpose. Instead,
I introduce a new field to `struct session`:

```c
struct session {
    int fd;
    // ...
    int _sync;
};
```

Then just before `epoll_ctl` I'll do a *release* store on this field,
"releasing" the session. All session stores are ordered before the
release.

```c
    // main thread
    // ...
    __atomic_store_n(&session->_sync, 0, __ATOMIC_RELEASE)
    epoll_ctl(epfd, EPOLL_CTL_ADD, fd, &event);

    // worker thread
    // ...
    __atomic_store_n(&session->_sync, 0, __ATOMIC_RELEASE)
    epoll_ctl(epfd, EPOLL_CTL_MOD, fd, &event);
```

After `epoll_wait` I add an *acquire* load, "acquiring" the session. All
session loads are ordered after the acquire.

```c
    epoll_wait(epfd, &event, 1, -1);
    struct session *session = event.data.ptr;
    __atomic_load_n(&session->_sync, __ATOMIC_ACQUIRE)
    int fd = session->fd;
    // ...
```

For this to work, the thread must not touch session variables in any way
before the acquire or after the release. For example, note how I obtained
the client file descriptor before the release, i.e. no `session->fd`
argument in the `epoll_ctl` call.

That's it! This redundantly establishes the *happens before* relationship
already implicit in epoll, but now it's visible to TSan. However, I don't
want to pay for this unless I'm actually running under TSan, so some
macros are in order. `__SANITIZE_THREAD__` is automatically defined when
running under TSan:

```c
#if __SANITIZE_THREAD__
# define TSAN_SYNCED     int _sync
# define TSAN_ACQUIRE(s) __atomic_load_n(&(s)->_sync, __ATOMIC_ACQUIRE)
# define TSAN_RELEASE(s) __atomic_store_n(&(s)->_sync, 0, __ATOMIC_RELEASE)
#else
# define TSAN_SYNCED
# define TSAN_ACQUIRE(s)
# define TSAN_RELEASE(s)
#endif
```

This also makes it more readable, and intentions clearer:

```c
struct session {
    int fd;
    // ...
    TSAN_SYNCED;
};

    // main thread
    for (;;) {
        // ...
        TSAN_RELEASE(session);
        epoll_ctl(epfd, EPOLL_CTL_ADD, fd, &event);
    }

    // worker thread
    for (;;) {
        epoll_wait(epfd, &event, 1, -1);
        struct session *session = event.data.ptr;
        TSAN_ACQUIRE(session);
        int fd = session->fd;
        // ...
        TSAN_RELEASE(session);
        epoll_ctl(epfd, EPOLL_CTL_MOD, fd, &event);
    }
```

Now I can use TSan again, and it didn't cost anything in normal builds.


[arena]: https://www.rfleury.com/p/untangling-lifetimes-the-arena-allocator
[debuggers]: /blog/2022/06/26/
[epoll]: https://man7.org/linux/man-pages/man7/epoll.7.html
[fuzzing]: /blog/2019/01/25/
[gcc]: https://gcc.gnu.org/onlinedocs/gcc-12.2.0/gcc/_005f_005fatomic-Builtins.html
[GetMessage]: https://learn.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-getmessage
[os]: https://idea.popcount.org/2017-02-20-epoll-is-fundamentally-broken-12/
[PostMessage]: https://learn.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-postmessagea
[Sanitizers]: https://github.com/google/sanitizers/wiki
[state machine]: /blog/2020/12/31/
[tsan]: https://clang.llvm.org/docs/ThreadSanitizer.html
[vc]: https://www.youtube.com/watch?v=5erqWdlhQLA
[w64]: https://github.com/skeeto/w64devkit
