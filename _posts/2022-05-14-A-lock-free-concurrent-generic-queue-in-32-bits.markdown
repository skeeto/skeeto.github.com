---
title: A lock-free, concurrent, generic queue in 32 bits
layout: post
date: 2022-05-14T04:22:24Z
tags: [c, optimization]
uuid: b5a6b85a-19af-4f2f-8a32-0098f6e87edb
excerpt_separator: <!--more-->
---

*This article was discussed [on Hacker News][hn].*

While considering concurrent queue design I came up with a generic,
lock-free queue that fits in a 32-bit integer. The queue is "generic" in
that a single implementation supports elements of any arbitrary type,
despite an implementation in C. It's lock-free in that there is guaranteed
system-wide progress. It can store up to 32,767 elements at a time — more
than enough for message queues, which [must always be bounded][asyncio]. I
will first present a single-consumer, single-producer queue, then expand
support to multiple consumers at a cost. Like [my lightweight barrier][b],
I'm not presenting this as a packaged solution, but rather as a technique
you can apply when circumstances call.

<!--more-->

How can the queue store so many elements when it's just 32 bits? It only
handles the indexes of a circular buffer. The [caller is responsible][min]
for allocating and manipulating the queue's storage, which, in the
single-consumer case, doesn't require anything fancy. Synchronization is
managed by the queue.

Like a typical circular buffer, it has a head index and a tail index. The
head is the next element to be pushed, and the tail is the next element to
be popped. The queue storage must have a power-of-two length, but the
capacity is one less than the length. If the head and tail are equal then
the queue is empty. This "wastes" one element, which is why the capacity
is one less than the length of the storage. So already there are some
notable constraints imposed by this design, but I believe the main use
case for such a queue — a job queue for CPU-bound jobs — has no problem
with these constraints.

Since this is a concurrent queue it's worth noting "ownership" of storage
elements. The consumer owns elements from the tail up to, but excluding,
the head. The producer owns everything else. Both pushing and popping
involve a "commit" step that transfers ownership of an element to the
other thread. No elements are accessed concurrently, which makes things
easy for either caller.

### Queue usage

Pushing (to the front) and popping (from the back) are each a three-step
process:

1. Obtain the element index
2. Access that element
3. Commit the operation

I'll be using C11 atomics for my implementation, but it should be easy to
translate these into something else no matter the programming language. As
I mentioned, the queue fits in a 32-bit integer, and so it's represented
by an `_Atomic uint32_t`. Here's the entire interface:

```c
int  queue_pop(_Atomic uint32_t *queue, int exp);
void queue_pop_commit(_Atomic uint32_t *queue);

int  queue_push(_Atomic uint32_t *queue, int exp);
void queue_push_commit(_Atomic uint32_t *queue);
```

Both `queue_pop` and `queue_push` return -1 if the queue is empty/full.

To create a queue, initialize an atomic 32-bit integer to zero. Also
choose a size exponent and allocate some storage. Here's a 63-element
queue of jobs:

```c
#define EXP 6  // note; 2**6 == 64
struct job slots[1<<EXP];
_Atomic uint32_t q = 0;
```

Rather than a length, the queue functions accept a base-2 exponent, which
is why I've defined `EXP`. If you don't like this, you can just accept a
length in your own implementation, though remember it's constrained to
powers of two. The producer might look like so:

```c
for (;;) {
    int i;
    do {
        i = queue_push(&q, EXP);
    } while (i < 0);  // note: busy-wait while full
    slots[i] = job_create();
    queue_push_commit(&q);
}
```

This is a busy-wait loop, which makes for a simple illustration but isn't
ideal. In a [real program][watc] I'd have the producer run a job while it
waits for a queue slot, or just have it turn into a consumer (if this
wasn't a single-consumer queue). Similarly, if the queue is empty, then
maybe a consumer turns into the producer. It all depends on the context.

The consumer might look like so:

```c
for (;;) {
    int i;
    do {
        i = queue_pop(&q, EXP);
    } while (i < 0);  // note: busy-wait while empty
    struct job job = slots[i];
    queue_pop_commit(&q);
    job_run(job);
}
```

In either case it's important that neither touches the element after
committing since that transfers ownership away.

### Pop operation

The queue is actually a pair of 16-bit integers, head and tail, each
stored in the low and high halves of the 32-bit integer. So the first
thing to do is atomically load the integer, then extract these "fields."

If for some reason a capacity of 32,767 is insufficient, you can trivially
upgrade your queue to an Enterprise Queue: a 64-bit integer with a
capacity of over 2 billion elements. I'm going to stick with the 32-bit
queue.

Starting with the pop operation since it's simpler:

```c
int queue_pop(_Atomic uint32_t *q, int exp)
{
    uint32_t r = *q;  // consider "acquire"
    int mask = (1u << exp) - 1;
    int head = r     & mask;
    int tail = r>>16 & mask;
    return head == tail ? -1 : tail;
}
```

If the indexes are equal, the queue is empty. Otherwise return the tail
field. The `*q` is an atomic load since it's qualified `_Atomic`. The load
might be more efficient if this were an explicit "acquire" operation,
which is what I used in some of my tests.

To complete the pop, atomically increment the tail index so that the
element falls out of the range of elements owned by the consumer. The tail
is the high half of the integer so add `0x10000` rather than just 1.

```c
void queue_pop_commit(_Atomic uint32_t *q)
{
    *q += 0x10000;  // consider "release"
}
```

It's harmless if this overflows since it's congruent with the power-of-two
storage length, and an overflow won't affect the head index. The increment
might be more efficient if this were an explicit "release" operation,
which, again, is what I used in some of my tests.

### Push operation

Pushing is a little more complex. As is typical with circular buffers,
before doing anything it must ensure the result won't ambiguously create
an empty queue.

```c
int queue_push(_Atomic uint32_t *q, int exp)
{
    uint32_t r = *q;  // consider "acquire"
    int mask = (1u << exp) - 1;
    int head = r     & mask;
    int tail = r>>16 & mask;
    int next = (head + 1u) & mask;
    if (r & 0x8000) {  // avoid overflow on commit
        *q &= ~0x8000;
    }
    return next == tail ? -1 : head;
}
```

It's important that incrementing the head field won't overflow into the
tail field, so it atomically clears the high bit if set, giving the
increment overhead into which it can overflow.

```c
void queue_push_commit(_Atomic uint32_t *q)
{
    *q += 1;  // consider "release"
}
```

### Multiple-consumers

The single producer and single consumer didn't require locks nor atomic
accesses to the storage array since the queue guaranteed that accesses at
the specified index were not concurrent. However, this is not the case
with multiple-consumers. Consumers race when popping. The loser's access
might occur after the winner's commit, making its access concurrent with
the producer. Both producer and consumers must account for this.

```c
_Atomic struct job slots[1<<EXP];
```

To prepare for multiple consumers, the array now has an atomic qualifier:
one of the costs of multiple consumers. Fortunately these new atomic
accesses can use a "relaxed" ordering since there are no required ordering
constraints. Even if it wasn't atomic, and [the load was torn][tear], we'd
detect it when attempting to commit. It's simply against the rules to have
a data race, and I don't know how else to avoid it other than dropping
into assembly.

The next cost is that committing can fail. Another consumer might have won
the race, which means you must start over. Here's my multiple-consumer
interface, which I've uncreatively called `mpop` ("multiple-consumer
pop"). Besides a `_Bool` for indicating failure, the main change is a new
`save` parameter:

```c
int   queue_mpop(_Atomic uint32_t *, int, uint32_t *save);
_Bool queue_mpop_commit(_Atomic uint32_t *, uint32_t save);
```

The caller must carry some temporary state (`save`), which is how failures
are detected, ultimately communicated by that `_Bool` return.

```c
for (;;) {
    int i;
    int32_t save;
    struct job job;
    do {
        do {
            i = queue_mpop(&q, EXP, &save);
        } while (i < 0);  // note: busy-wait while empty
        job = slots[i];
    } while (!queue_mpop_commit(&q, save));
    job_run(job);
}
```

It's important that the consumer doesn't attempt to use `job` until a
successful commit, since it might not be valid. As noted, that load could
be relaxed (what a mouthful):

```c
job = atomic_load_explicit(slots+i, memory_order_relaxed);
```

Here's the pop implementation:

```c
int queue_mpop(_Atomic uint32_t *q, int exp, uint32_t *save)
{
    uint32_t r = *save = *q;
    int mask = (1u << exp) - 1;
    int head = r     & mask;
    int tail = r>>16 & mask;
    return head == tail ? -1 : tail;
}
```

So far it's exactly the same, except it stores a full snapshot of the
queue state in `*save`. This is needed for a compare-and-swap (CAS) in the
commit, which checks that the queue hasn't been modified concurrently
(i.e. by another consumer):

```c
_Bool queue_mpop_commit(_Atomic uint32_t *q, uint32_t save)
{
    return atomic_compare_exchange_strong(q, &save, save+0x10000);
}
```

As always with CAS, we must be wary of [the ABA problem][aba]. Imagine
that between starting to pop and this CAS that the producer and another
consumer looped over the entire queue and ended up back at exactly the
same spot as where we started. The queue would look like we expect, and
the commit would "succeed" despite reading a garbage value.

Fortunately this matches the entire 32-bit state, and so a small queue
capacity is not at a greater risk. The tail counter is always 16 bits, and
the head counter is 15 bits (due to keeping the 16th clear for overflow).
The chance of them landing at exactly the same count is low. Though if
those odds aren't low enough, as mentioned you can always upgrade to the
64-bit Enterprise Queue with larger counters.

There's a notable performance defect with this particular design. If the
producer concurrently pushes a new value, the commit will fail even if
there was no real race since only the head field changed. It would be
better if the head field was isolated from the tail field…

### A less cheeky design

You might have noticed that there's little reason to pack two 16-bit
counters into a 32-bit integer. These could just be fields in a structure:

```c
struct queue {
    _Atomic uint16_t head;
    _Atomic uint16_t tail;
};
```

While this entire structure can be atomically loaded just like the 32-bit
integer, C11 (and later) do not permit non-atomic accesses to these atomic
fields in an unshared copy loaded from an atomic. So I'd either use
compiler-specific built-ins for atomics — much more flexible, and what I
prefer anyway — or just load them individually:

```c
int queue_pop(struct queue *q, int exp, uint16_t *save)
{
    int mask = (1u << exp) - 1;
    int head = q->head & mask;
    int tail = (*save = q->tail) & mask;
    return head == tail ? -1 : tail;
}
```

Technically with two loads this could extract a `head`/`tail` pair that
were never contemporaneous. The worst case is the queue appears empty even
if it was never actually empty.

```c
_Bool queue_mpop_commit(struct queue *q, uint16_t save)
{
    return atomic_compare_exchange_strong(&q->tail, &save, save+1);
}
```

Since the head index isn't part of the CAS, the producer can't interfere
with the commit. (Though there's still certainly false sharing happening.)

### Real implementation and tests

If you want to try it out, especially with my tests: [**queue.c**][src].
It has both single-consumer and multiple-consumer queues, and supports at
least:

* atomics: C11, GNU, MSC
* threads: pthreads, win32
* compilers: GCC, Clang, MSC
* hosts: Linux, Windows, BSD

Since I wanted to test across a variety of implementations, especially
under Thread Sanitizer (TSan). On a similar note, I also implemented a
concurrent queue shared between C and Go: [**queue.go**][go].

[aba]: /blog/2014/09/02/
[asyncio]: /blog/2020/05/24/
[b]: /blog/2022/03/13/
[go]: https://github.com/skeeto/scratch/blob/master/misc/queue.go
[hn]: https://news.ycombinator.com/item?id=31384602
[min]: /blog/2018/06/10/
[src]: https://github.com/skeeto/scratch/blob/master/misc/queue.c
[tear]: https://lwn.net/Articles/793253/
[watc]: /blog/2022/05/22/
