---
title: C11 Lock-free Stack
layout: post
date: 2014-09-02T03:10:01Z
tags: [c, tutorial, optimization]
uuid: 743811a4-aaf7-32e3-8a0c-62f1e8dbaf66
---

C11, the [latest C standard revision][c11], hasn't received anywhere
near the same amount of fanfare as C++11. I'm not sure why this is.
Some of the updates to each language are very similar, such as formal
support for threading and atomic object access. Three years have
passed and some parts of C11 still haven't been implemented by any
compilers or standard libraries yet. Since there's not yet a lot of
discussion online about C11, I'm basing much of this article on my own
understanding of the [C11 draft][draft]. I *may* be under-using the
`_Atomic` type specifier and not paying enough attention to memory
ordering constraints.

Still, this is a good opportunity to break new ground with a
demonstration of C11. I'm going to use the new
[`stdatomic.h`][stdatomic.h] portion of C11 to build a lock-free data
structure. To compile this code you'll need a C compiler and C library
with support for both C11 and the optional `stdatomic.h` features. As
of this writing, as far as I know only [GCC 4.9][gcc], released April
2014, supports this. It's in Debian unstable but not in Wheezy.

If you want to take a look before going further, here's the source.
The test code in the repository uses plain old pthreads because C11
threads haven't been implemented by anyone yet.

* [https://github.com/skeeto/lstack](https://github.com/skeeto/lstack)

I was originally going to write this article a couple weeks ago, but I
was having trouble getting it right. Lock-free data structures are
trickier and nastier than I expected, more so than traditional mutex
locks. Getting it right requires very specific help from the hardware,
too, so it won't run just anywhere. I'll discuss all this below. So
sorry for the long article. It's just a lot more complex a topic than
I had anticipated!

### Lock-free

A lock-free data structure doesn't require the use of mutex locks.
More generally, it's a data structure that can be accessed from
multiple threads without blocking. This is accomplished through the
use of atomic operations — transformations that cannot be
interrupted. Lock-free data structures will generally provide better
throughput than mutex locks. And it's usually safer, because there's
no risk of getting stuck on a lock that will never be freed, such as a
deadlock situation. On the other hand there's additional risk of
starvation (livelock), where a thread is unable to make progress.

As a demonstration, I'll build up a lock-free stack, a sequence with
last-in, first-out (LIFO) behavior. Internally it's going to be
implemented as a linked-list, so pushing and popping is O(1) time,
just a matter of consing a new element on the head of the list. It
also means there's only one value to be updated when pushing and
popping: the pointer to the head of the list.

Here's what the API will look like. I'll define `lstack_t` shortly.
I'm making it an opaque type because its fields should never be
accessed directly. The goal is to completely hide the atomic
semantics from the users of the stack.

~~~c
int     lstack_init(lstack_t *lstack, size_t max_size);
void    lstack_free(lstack_t *lstack);
size_t  lstack_size(lstack_t *lstack);
int     lstack_push(lstack_t *lstack, void *value);
void   *lstack_pop (lstack_t *lstack);
~~~

Users can push void pointers onto the stack, check the size of the
stack, and pop void pointers back off the stack. Except for
initialization and destruction, these operations are all safe to use
from multiple threads. Two different threads will never receive the
same item when popping. No elements will ever be lost if two threads
attempt to push at the same time. Most importantly a thread will never
block on a lock when accessing the stack.

Notice there's a maximum size declared at initialization time. While
[lock-free allocation is possible][malloc] [PDF], C makes no
guarantees that `malloc()` is lock-free, so being truly lock-free
means not calling `malloc()`. An important secondary benefit to
pre-allocating the stack's memory is that this implementation doesn't
require the use of [hazard pointers][hazard], which would be far more
complicated than the stack itself.

The declared maximum size should actually be the desired maximum size
plus the number of threads accessing the stack. This is because a
thread might remove a node from the stack and before the node can
freed for reuse, another thread attempts a push. This other thread
might not find any free nodes, causing it to give up without the stack
actually being "full."

The `int` return value of `lstack_init()` and `lstack_push()` is for
error codes, returning 0 for success. The only way these can fail is
by running out of memory. This is an issue regardless of being
lock-free: systems can simply run out of memory. In the push case it
means the stack is full.

### Structures

Here's the definition for a node in the stack. Neither field needs to
be accessed atomically, so they're not special in any way. In fact,
the fields are *never* updated while on the stack and visible to
multiple threads, so it's effectively immutable (outside of reuse).
Users never need to touch this structure.

~~~c
struct lstack_node {
    void *value;
    struct lstack_node *next;
};
~~~

Internally a `lstack_t` is composed of *two* stacks: the value stack
(`head`) and the free node stack (`free`). These will be handled
identically by the atomic functions, so it's really a matter of
convention which stack is which. All nodes are initially placed on the
free stack and the value stack starts empty. Here's what an internal
stack looks like.

~~~c
struct lstack_head {
    uintptr_t aba;
    struct lstack_node *node;
};
~~~

There's still no atomic declaration here because the struct is going
to be handled as an entire unit. The `aba` field is critically
important for correctness and I'll go over it shortly. It's declared
as a `uintptr_t` because it needs to be the same size as a pointer.
Now, this is not guaranteed by C11 — it's only guaranteed to be large
enough to hold any valid `void *` pointer, so it could be even larger
— but this will be the case on any system that has the required
hardware support for this lock-free stack. This struct is therefore
the size of two pointers. If that's not true for any reason, this code
will not link. Users will never directly access or handle this struct
either.

Finally, here's the actual stack structure.

~~~c
typedef struct {
    struct lstack_node *node_buffer;
    _Atomic struct lstack_head head, free;
    _Atomic size_t size;
} lstack_t;
~~~

Notice the use of the new `_Atomic` qualifier. Atomic values may have
different size, representation, and alignment requirements in order to
satisfy atomic access. These values should never be accessed directly,
even just for reading (use `atomic_load()`).

The `size` field is for convenience to check the number of elements on
the stack. It's accessed separately from the stack nodes themselves,
so it's not safe to read `size` and use the information to make
assumptions about future accesses (e.g. checking if the stack is empty
before popping off an element). Since there's no way to lock the
lock-free stack, there's otherwise no way to estimate the size of the
stack during concurrent access without completely disassembling it via
`lstack_pop()`.

There's [no reason to use `volatile` here][volatile]. That's a
separate issue from atomic operations. The C11 `stdatomic.h` macros
and functions will ensure atomic values are accessed appropriately.

### Stack Functions

As stated before, all nodes are initially placed on the internal free
stack. During initialization they're allocated in one solid chunk,
chained together, and pinned on the `free` pointer. The initial
assignments to atomic values are done through `ATOMIC_VAR_INIT`, which
deals with memory access ordering concerns. The `aba` counters don't
*actually* need to be initialized. Garbage, indeterminate values are
just fine, but not initializing them would probably look like a
mistake.

~~~c
int
lstack_init(lstack_t *lstack, size_t max_size)
{
    struct lstack_head head_init = {0, NULL};
    lstack->head = ATOMIC_VAR_INIT(head_init);
    lstack->size = ATOMIC_VAR_INIT(0);

    /* Pre-allocate all nodes. */
    lstack->node_buffer = malloc(max_size * sizeof(struct lstack_node));
    if (lstack->node_buffer == NULL)
        return ENOMEM;
    for (size_t i = 0; i < max_size - 1; i++)
        lstack->node_buffer[i].next = lstack->node_buffer + i + 1;
    lstack->node_buffer[max_size - 1].next = NULL;
    struct lstack_head free_init = {0, lstack->node_buffer};
    lstack->free = ATOMIC_VAR_INIT(free_init);
    return 0;
}
~~~

The free nodes will not necessarily be used in the same order that
they're placed on the free stack. Several threads may pop off nodes
from the free stack and, as a separate operation, push them onto the
value stack in a different order. Over time with multiple threads
pushing and popping, the nodes are likely to get shuffled around quite
a bit. This is why a linked listed is still necessary even though
allocation is contiguous.

The reverse of `lstack_init()` is simple, and it's assumed concurrent
access has terminated. The stack is no longer valid, at least not
until `lstack_init()` is used again. This one is declared `inline` and
put in the header.

~~~c
static inline void
stack_free(lstack_t *lstack)
{
    free(lstack->node_buffer);
}
~~~

To read an atomic value we need to use `atomic_load()`. Give it a
pointer to an atomic value, it dereferences the pointer and returns
the value. This is used in another inline function for reading the
size of the stack.

~~~c
static inline size_t
lstack_size(lstack_t *lstack)
{
    return atomic_load(&lstack->size);
}
~~~

#### Push and Pop

For operating on the two stacks there will be two internal, static
functions, `push` and `pop`. These deal directly in nodes, accepting
and returning them, so they're not suitable to expose in the API
(users aren't meant to be aware of nodes). This is the most complex
part of lock-free stacks. Here's `pop()`.

~~~c
static struct lstack_node *
pop(_Atomic struct lstack_head *head)
{
    struct lstack_head next, orig = atomic_load(head);
    do {
        if (orig.node == NULL)
            return NULL;  // empty stack
        next.aba = orig.aba + 1;
        next.node = orig.node->next;
    } while (!atomic_compare_exchange_weak(head, &orig, next));
    return orig.node;
}
~~~

It's centered around the new C11 `stdatomic.h` function
`atomic_compare_exchange_weak()`. This is an atomic operation more
generally called [compare-and-swap][cas] (CAS). On x86 there's an
instruction specifically for this, `cmpxchg`. Give it a pointer to the
atomic value to be updated (`head`), a pointer to the value it's
expected to be (`orig`), and a desired new value (`next`). If the
expected and actual values match, it's updated to the new value. If
not, it reports a failure and updates the expected value to the latest
value. In the event of a failure we start all over again, which
requires the `while` loop. This is an *optimistic* strategy.

The "weak" part means it will sometimes spuriously fail where the
"strong" version would otherwise succeed. In exchange for more
failures, calling the weak version is faster. Use the weak version
when the body of your `do ... while` loop is fast and the strong
version when it's slow (when trying again is expensive), or if you
don't need a loop at all. You usually want to use weak.

The alternative to CAS is [load-link/store-conditional][llsc]. It's a
stronger primitive that doesn't suffer from the ABA problem described
next, but it's also not available on x86-64. On other platforms, one
or both of `atomic_compare_exchange_*()` will be implemented using
LL/SC, but we still have to code for the worst case (CAS).

##### The ABA Problem

The `aba` field is here to solve [the ABA problem][aba] by counting
the number of changes that have been made to the stack. It will be
updated atomically alongside the pointer. Reasoning about the ABA
problem is where I got stuck last time writing this article.

Suppose `aba` didn't exist and it was just a pointer being swapped.
Say we have two threads, A and B.

* Thread A copies the current `head` into `orig`, enters the loop body
  to update `next.node` to `orig.node->next`, then gets preempted
  before the CAS. The scheduler pauses the thread.

* Thread B comes along performs a `pop()` changing the value pointed
  to by `head`. At this point A's CAS will fail, which is fine. It
  would reconstruct a new updated value and try again. While A is
  still asleep, B puts the popped node back on the free node stack.

* Some time passes with A still paused. The freed node gets re-used
  and pushed back on top of the stack, which is likely given that
  nodes are allocated FIFO. Now `head` has its original value again,
  but the `head->node->next` pointer is pointing somewhere completely
  new! *This is very bad* because A's CAS will now succeed despite
  `next.node` having the wrong value.

* A wakes up and it's CAS succeeds. At least one stack value has been
  lost and at least one node struct was leaked (it will be on neither
  stack, nor currently being held by a thread). This is the ABA
  problem.

The core problem is that, unlike integral values, pointers have
meaning beyond their intrinsic numeric value. The meaning of a
particular pointer changes when the pointer is reused, making it
suspect when used in CAS. The unfortunate effect is that, **by itself,
atomic pointer manipulation is nearly useless**. They'll work with
append-only data structures, where pointers are never recycled, but
that's it.

The `aba` field solves the problem because it's incremented every time
the pointer is updated. Remember that this internal stack struct is
two pointers wide? That's 16 bytes on a 64-bit system. The entire 16
bytes is compared by CAS and they all have to match for it to succeed.
Since B, or other threads, will increment `aba` at least twice (once
to remove the node, and once to put it back in place), A will never
mistake the recycled pointer for the old one. There's a special
double-width CAS instruction specifically for this purpose,
`cmpxchg16`. This is generally called DWCAS. It's available on most
x86-64 processors. On Linux you can check `/proc/cpuinfo` for support.
It will be listed as `cx16`.

If it's not available at compile-time this program won't link. The
function that wraps `cmpxchg16` won't be there. You can tell GCC to
*assume* it's there with the `-mcx16` flag. The same rule here applies
to C++11's new std::atomic.

There's still a tiny, tiny possibility of the ABA problem still
cropping up. On 32-bit systems A may get preempted for over 4 billion
(2^32) stack operations, such that the ABA counter wraps around to the
same value. There's nothing we can do about this, but if you witness
this in the wild you need to immediately stop what you're doing and go
buy a lottery ticket. Also avoid any lightning storms on the way to
the store.

##### Hazard Pointers and Garbage Collection

Another problem in `pop()` is dereferencing `orig.node` to access its
`next` field. By the time we get to it, the node pointed to by
`orig.node` may have already been removed from the stack and freed. If
the stack was using `malloc()` and `free()` for allocations, it may
even have had `free()` called on it. If so, the dereference would be
undefined behavior — a segmentation fault, or worse.

There are three ways to deal with this.

1. Garbage collection. If memory is automatically managed, the node
   will never be freed as long as we can access it, so this won't be a
   problem. However, if we're interacting with a garbage collector
   we're not really lock-free.

2. Hazard pointers. Each thread keeps track of what nodes it's
   currently accessing and other threads aren't allowed to free nodes
   on this list. This is messy and complicated.

3. Never free nodes. This implementation recycles nodes, but they're
   never truly freed until `lstack_free()`. It's always safe to
   dereference a node pointer because there's always a node behind it.
   It may point to a node that's on the free list or one that was even
   recycled since we got the pointer, but the `aba` field deals with
   any of those issues.

Reference counting on the node won't work here because we can't get to
the counter fast enough (atomically). It too would require
dereferencing in order to increment. The reference counter could
potentially be packed alongside the pointer and accessed by a DWCAS,
but we're already using those bytes for `aba`.

##### Push

Push is a lot like pop.

~~~c
static void
push(_Atomic struct lstack_head *head, struct lstack_node *node)
{
    struct lstack_head next, orig = atomic_load(head);
    do {
        node->next = orig.node;
        next.aba = orig.aba + 1;
        next.node = node;
    } while (!atomic_compare_exchange_weak(head, &orig, next));
}
~~~

It's counter-intuitive, but adding a [few microseconds of
sleep][pitfall] after CAS failures would probably *increase*
throughput. Under high contention, threads wouldn't take turns
clobbering each other as fast as possible. It would be a bit like
exponential backoff.

#### API Push and Pop

The API push and pop functions are built on these internal atomic
functions.

~~~c
int
lstack_push(lstack_t *lstack, void *value)
{
    struct lstack_node *node = pop(&lstack->free);
    if (node == NULL)
        return ENOMEM;
    node->value = value;
    push(&lstack->head, node);
    atomic_fetch_add(&lstack->size, 1);
    return 0;
}
~~~

Push removes a node from the free stack. If the free stack is empty it
reports an out-of-memory error. It assigns the value and pushes it
onto the value stack where it will be visible to other threads.
Finally, the stack size is incremented atomically. This means there's
an instant where the stack size is listed as one shorter than it
actually is. However, since there's no way to access both the stack
size and the stack itself at the same instant, this is fine. The stack
size is really only an estimate.

Popping is the same thing in reverse.

~~~c
void *
lstack_pop(lstack_t *lstack)
{
    struct lstack_node *node = pop(&lstack->head);
    if (node == NULL)
        return NULL;
    atomic_fetch_sub(&lstack->size, 1);
    void *value = node->value;
    push(&lstack->free, node);
    return value;
}
~~~

Remove the top node, subtract the size estimate atomically, put the
node on the free list, and return the pointer. It's really simple with
the primitive push and pop.

### SHA1 Demo

The lstack repository linked at the top of the article includes a demo
that searches for patterns in SHA-1 hashes (sort of like Bitcoin
mining). It fires off one worker thread for each core and the results
are all collected into the same lock-free stack. It's not *really*
exercising the library thoroughly because there are no contended pops,
but I couldn't think of a better example at the time.

The next thing to try would be implementing a C11, bounded, lock-free
queue. It would also be more generally useful than a stack,
particularly for common consumer-producer scenarios.


[c11]: http://en.wikipedia.org/wiki/C11_(C_standard_revision)
[atomic.h]: http://lxr.free-electrons.com/source/include/linux/atomic.h
[stdatomic.h]: http://en.cppreference.com/w/c/atomic
[gcc]: https://gcc.gnu.org/gcc-4.9/changes.html
[draft]: http://stackoverflow.com/a/83763
[cas]: http://en.wikipedia.org/wiki/Compare-and-swap
[volatile]: https://www.kernel.org/doc/Documentation/volatile-considered-harmful.txt
[malloc]: http://www.research.ibm.com/people/m/michael/pldi-2004.pdf
[pitfall]: http://blog.memsql.com/common-pitfalls-in-writing-lock-free-algorithms/
[hazard]: http://en.wikipedia.org/wiki/Hazard_pointer
[aba]: http://en.wikipedia.org/wiki/ABA_problem
[llsc]: http://en.wikipedia.org/wiki/Load-link/store-conditional
