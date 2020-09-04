---
title: Exactly-Once Initialization in Asynchronous Python
layout: post
date: 2020-07-30T23:39:12Z
tags: [python, asyncio]
uuid: c6796958-9178-47be-8411-8f48c2c85d83
excerpt_separator: <!--more-->
---

*This article was discussed [on Hacker News][hn].*

A common situation in [asyncio][asyncio] Python programs is asynchronous
initialization. Some resource must be initialized exactly once before it
can be used, but the initialization itself is asynchronous — such as an
[asyncpg][asyncpg] database. Let's talk about a couple of solutions.

<!--more-->

The naive "solution" would be to track the initialization state in a
variable:

```py
initialized = False

async def one_time_setup():
    "Do not call more than once!"
    ...

async def maybe_initialize():
    global initialized
    if not initialized:
        await one_time_setup()
        initialized = True
```

The reasoning for `initialized` is the expectation of calling the
function more than once. However, if it might be called from concurrent
tasks there's a *race condition*. If the second caller arrives while the
first is awaiting `one_time_setup()`, the function will be called a
second time.

Switching the order of the call and the assignment won't help:

```py
async def maybe_initialize():
    global initialized
    if not initialized:
        initialized = True
        await one_time_setup()
```

Since asyncio is cooperative, the first caller doesn't give up control
until to other tasks until the `await`, meaning `one_time_setup()` will
never be called twice. However, the second caller may return before
`one_time_setup()` has completed. What we want is for `one_time_setup()`
to be called exactly once, but for no caller to return until it has
returned.

### Mutual exclusion

My first thought was to use a [mutex lock][lock]. This will protect the
variable *and* prevent followup callers from progressing too soon. Tasks
arriving while `one_time_setup()` is still running will block on the
lock.

```py
initialized = False
initialized_lock = asyncio.Lock()

async def maybe_initialize():
    global initialized
    async with initialized_lock:
        if not initialized:
            await one_time_setup()
            initialized = True
```

Unfortunately this has a serious downside: **asyncio locks are
associated with the [loop][loop] where they were created**. Since the
lock variable is global, `maybe_initialize()` can only be called from
the same loop that loaded the module. `asyncio.run()` creates a new loop
so it's incompatible.

```py
# create a loop: always an error
asyncio.run(maybe_initialize())

# reuse the loop: maybe an error
loop = asyncio.get_event_loop()
loop.run_until_complete((maybe_initialize()))
```

(IMHO, it was a mistake for the asyncio API to include explicit loop
objects. It's a low-level concept that unavoidably leaks through most
high-level abstractions.)

A workaround is to create the lock lazily. Thank goodness creating a
lock isn't itself asynchronous!

```py
initialized = False
initialized_lock = None

async def maybe_initialize():
    global initialized, initialized_lock
    if not initialized_lock:
        initialized_lock = asyncio.Lock()
    async with initialized_lock:
        if not initialized:
            await one_time_setup()
            initialized = True
```

This is better, but `maybe_initialize()` can still only ever be called
from a single loop.

```py
asyncio.run(maybe_initialize()) # ok
asyncio.run(maybe_initialize()) # error!
```

### Once

The pthreads API provides [`pthread_once`][once] to solve this problem.
C++11 has similarly has [`std::call_once`][cpp]. We can build something
similar using a future-like object.

```py
future = None

async def maybe_initialize():
    global future
    if not future:
        future = asyncio.create_task(one_time_setup())
    await future
```

Awaiting a coroutine more than once is an error, but [tasks][task] are
future-like objects and can be awaited more than once. At least on
CPython, they can also be awaited in other loops! So not only is this
simpler, it also solves the loop problem!

```py
asyncio.run(maybe_initialize()) # ok
asyncio.run(maybe_initialize()) # still ok
```

This can be tidied up nicely in a `@once` decorator:

```py
def once(func):
    future = None
    async def once_wrapper(*args, **kwargs):
        nonlocal future
        if not future:
            future = asyncio.create_task(func(*args, **kwargs))
        return await future
    return once_wrapper
```

No more need for `maybe_initialize()`, just decorate the original
`one_time_setup()`:

```py
@once
async def one_time_setup():
    ...
```


[asyncio]: https://docs.python.org/3/library/asyncio.html
[asyncpg]: https://github.com/MagicStack/asyncpg
[cpp]: https://en.cppreference.com/w/cpp/thread/call_once
[hn]: https://news.ycombinator.com/item?id=24007354
[lock]: https://docs.python.org/3/library/asyncio-sync.html#lock
[loop]: https://docs.python.org/3/library/asyncio-eventloop.html
[once]: https://pubs.opengroup.org/onlinepubs/007908799/xsh/pthread_once.html
[task]: https://docs.python.org/3/library/asyncio-task.html#task-object
