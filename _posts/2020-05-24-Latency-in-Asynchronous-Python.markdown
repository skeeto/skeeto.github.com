---
title: Latency in Asynchronous Python
layout: post
date: 2020-05-24T02:44:50Z
tags: [python, asyncio]
uuid: 529e2382-d4ec-47a9-93a8-f450311e5a05
---

This week I was debugging a misbehaving Python program that makes
significant use of [Python's asyncio][asyncio]. The program would
eventually take very long periods of time to respond to network
requests. My first suspicion was a CPU-heavy coroutine hogging the
thread, preventing the socket coroutines from running, but an
inspection with `pdb` showed this wasn't the case. Instead, the
program's author had made a couple of fundamental mistakes using
asyncio. Let's discuss them using small examples.

Setting the stage: There's a heartbeat coroutine that "beats" once per
second. A real program would send out a packet as the heartbeat, but
here it just prints how late it was scheduled.

```py
async def heartbeat():
    while True:
        start = time.time()
        await asyncio.sleep(1)
        delay = time.time() - start - 1
        print(f'heartbeat delay = {delay:.3f}s')
```

Running this with `asyncio.run(heartbeat())`:

    heartbeat delay = 0.001s
    heartbeat delay = 0.001s
    heartbeat delay = 0.001s

It's consistently 1ms late, but good enough, especially considering
what's to come. A program that *only* sends a heartbeat is pretty
useless, so a real program will be busy working on other things
concurrently. In this example, we have little 10ms payloads of work to
do, which are represented by this `process()` function:

```py
JOB_DURATION = 0.01  # 10ms

async def process():
    time.sleep(JOB_DURATION) # simulate CPU time
```

That's a synchronous sleep because it's standing in for actual CPU work.
Maybe it's parsing JSON in a loop or crunching numbers in NumPy. Use
your imagination. During this 10ms no other coroutines can be scheduled
because this is, after all, still [just a single-threaded program][coop].

```py
JOB_COUNT = 200

async def main():
    asyncio.create_task(heartbeat())

    await asyncio.sleep(2.5)

    print('begin processing')
    count = JOB_COUNT
    for _ in range(JOB_COUNT):
        asyncio.create_task(process())

    await asyncio.sleep(5)
```

This program starts the heartbeat coroutine in a task. A coroutine
doesn't make progress unless someone is waiting on it, and that
something can be a task. So it will continue along independently without
prodding.

The arbitrary 2.5 second sleep simulates waiting, say, for a network
request. In the output we'll see the heartbeat tick a couple of times,
then it will create and process 200 jobs concurrently. In a real program
we'd have some way to collect the results, but we can ignore that part
for now. They're *only* 10ms, so the effect on the heartbeat should be
pretty small right?

    heartbeat delay = 0.001s
    heartbeat delay = 0.001s
    begin processing
    heartbeat delay = 1.534s
    heartbeat delay = 0.001s
    heartbeat delay = 0.001s

The heartbeat was delayed for 1.5 seconds by a mere 200 tasks each doing
only 10ms of work each. What happened?

Python calls the object that schedules tasks a *loop*, and this is no
coincidence. Everything to be scheduled gets put into a loop and is
scheduled round robin, one after another. The 200 tasks got scheduled
ahead of the heartbeat, and so it doesn't get scheduled again until each
of those tasks either yields (`await`) or completes.

It really didn't take much to significantly hamper the heartbeat, and,
with a [dumb bytecode compiler][dumb], 10ms may not be much work at all.
The lesson here is to avoid spawning many tasks if latency is an
important consideration.

### A semaphore is not the answer

My first idea at a solution: What if we used a semaphore to limit the
number of "active" tasks at a time? Then perhaps the heartbeat wouldn't
have to compete with so many other tasks for time.

```py
WORKER_COUNT = 4  # max "active" jobs at a time

async def main_with_semaphore():
    asyncio.create_task(heartbeat())

    await asyncio.sleep(2.5)

    sem = asyncio.Semaphore(WORKER_COUNT)
    async def process():
        await sem.acquire()
        time.sleep(JOB_DURATION)
        sem.release()

    print('begin processing')
    for _ in range(JOB_COUNT):
        asyncio.create_task(process())

    await asyncio.sleep(5)
```

When the heartbeat sleep completes, about half the jobs will be complete
and the other half blocked on the semaphore. So perhaps the heartbeat
gets to skip ahead of all the blocked tasks since they're not yet ready
to run?

    heartbeat delay = 0.001s
    heartbeat delay = 0.001s
    begin processing
    heartbeat delay = 1.537s
    heartbeat delay = 0.001s
    heartbeat delay = 0.001s

It made no difference whatsoever because the tasks each "held their
place" in line in the loop! Even reducing `WORKER_COUNT` to 1 would have
no effect. As soon as a task completes, it frees the task waiting next
in line. The semaphore does practically nothing here.

### Solving it with a job queue

Here's what does work: a [job queue][q]. Create a queue to be populated
with coroutines (not tasks), and have a small number of tasks run jobs
from the queue. Since this is a real solution, I've made this example
more complete.

```py
async def main_with_queue():
    asyncio.create_task(heartbeat())

    await asyncio.sleep(2.5)

    queue = asyncio.Queue(maxsize=1)
    async def worker():
        while True:
            coro = await queue.get()
            await coro  # consider using try/except
            queue.task_done()
    workers = [asyncio.create_task(worker())
                   for _ in range(WORKER_COUNT)]

    print('begin processing')
    for _ in range(JOB_COUNT):
        await queue.put(process())
    await queue.join()
    print('end processing')

    for w in workers:
        w.cancel()

    await asyncio.sleep(2)
```

The `task_done()` and `join()` methods make it trivial synchronize on
full job completion. I also take the time to destroy the worker tasks.
It's harmless to leave them blocked on the queue. They'll be garbage
collected so it's not a resource leak. However, CPython complains about
garbage collecting running tasks because it looks like a mistake — and
it usually is.

If you read carefully you might have noticed the queue's maximum size is
set to 1: not much of a "queue"! [Go][go] developers will recognize this
as being (nearly) an *unbuffered channel*, the default and most common
kind of channel. So it's more a synchronized rendezvous between producer
(`put()`) and consumer (`get()`). The producer waits at the queue with a
job until a task is free to come take it. A task waits at the queue
until a producer arrives with a job for it.

    heartbeat delay = 0.001s
    heartbeat delay = 0.001s
    begin processing
    heartbeat delay = 0.014s
    heartbeat delay = 0.020s
    end processing
    heartbeat delay = 0.002s
    heartbeat delay = 0.001s

The output shows that the impact to the heartbeat was modest — about
the best we could hope for from async/await — and the heartbeat
continued while jobs were running. The more concurrency — the more
worker tasks running on the queue — the greater the latency.

Note: Increasing the `WORKER_COUNT` in this toy example won't have an
impact on latency since the jobs aren't actually concurrent. They start,
run, and complete before another worker task can draw from the queue.
Putting a couple awaits in `process()` allows for concurrency:

```py
WORKER_COUNT = 200

async def process():
    await asyncio.sleep(0.01)
    time.sleep(JOB_DURATION)
    await asyncio.sleep(0.01)
```

Since there are so many worker tasks, this is back to the initial
problem:

    heartbeat delay = 0.001s
    heartbeat delay = 0.001s
    begin processing
    heartbeat delay = 1.655s
    end processing
    heartbeat delay = 0.001s
    heartbeat delay = 0.001s

As `WORKER_COUNT` decreases, so does heartbeat latency.

### Unbounded queues

Here's another defect from the same program. Create an unbounded queue,
a producer, and a consumer. The consumer prints the queue size so we can
see what's happening:

```py
async def producer_consumer():
    queue = asyncio.Queue()
    done = asyncio.Condition()

    async def producer():
        for i in range(100_000):
            await queue.put(i)
        await queue.join()
        async with done:
            done.notify()

    async def consumer():
        while True:
            await queue.get()
            print(f'qsize = {queue.qsize()}')
            queue.task_done()

    asyncio.create_task(producer())
    asyncio.create_task(consumer())

    async with done:
        await done.wait()
```

The output of this program begins:

    qsize = 99999
    qsize = 99998
    qsize = 99997
    qsize = 99996
    ...

So the entire queue is populated before the consumer does anything at
all: tons of latency for whatever is being consumed. Since the queue is
unbounded, the producer never needs to yield. You might be tempted to
use `asyncio.sleep(0)` in the producer to yield explicitly:

```py
    async def producer():
        for i in range(100_000):
            await queue.put(i)
            await asyncio.sleep(0)  # yield
        await queue.join()
        async with done:
            done.notify()
```

This even seems to work! The output looks like this:

    qsize = 0
    qsize = 0
    qsize = 0
    qsize = 0

However, this is fragile and not a real solution. If the consumer yields
just two times in its own loop, its nearly back to where we started:

```py
    async def consumer():
        while True:
            await queue.get()
            print(f'qsize = {queue.qsize()}')
            queue.task_done()
            await asyncio.sleep(0)
            await asyncio.sleep(0)
```

The output shows that the producer gradually creeps ahead of the
consumer. On each consumer iteration, the producer iterates twice:

    qsize = 0
    qsize = 1
    qsize = 2
    qsize = 3
    ...

There's a really simple solution to this: [Never, ever use unbounded
queues.][bp] In fact **every unbounded `asyncio.Queue()` is a bug**.
It's a serious API defect that asyncio allows unbounded queues to be
created at all. The default `maxsize` should have been *actually* zero
(unbuffered), not infinite. Because unbounded is the default, virtually
every example of `asyncio.Queue` — online, offline, and even the
official documentation — is broken in some way.

### Important takeaways

1. The default `asyncio.Queue()` is *always* wrong.
2. `asyncio.sleep(0)` is *nearly always* used incorrectly.
3. Use a `maxsize=1` job queue instead of spawning many identical tasks.

Python linters should be updated to warn about 1 and 2 by default.

Update: A couple of people have pointed out [an argument in the Trio
documentation for unbounded queues][trio]. This argument conflates two
different concepts: data structure queues and concurrent communication
infrastructure queues. To distinguish, the latter is often called a
channel. An unbounded *queue* (`collections.deque`) is necessary, but
and unbounded *channel* (`asyncio.Queue`) is always wrong. The Trio
documentation describes a web crawler, which is fundamentally a
breadth-first search (read: queue-oriented) of a graph. So this is a
plain old BFS queue, not a channel, which is why it's reasonable for it
to be unbounded.


[asyncio]: https://docs.python.org/3/library/asyncio.html
[bp]: https://lucumr.pocoo.org/2020/1/1/async-pressure/
[coop]: https://rachelbythebay.com/w/2020/03/07/costly/
[dumb]: /blog/2019/02/24/
[go]: https://golang.org/
[q]: https://docs.python.org/3/library/asyncio-queue.html
[trio]: https://trio.readthedocs.io/en/stable/reference-core.html#buffering-in-channels
