---
title: '"Once" one-time concurrent initialization with an integer'
layout: post
date: 2023-07-31T23:00:41Z
tags: [c]
uuid: 523b07ef-efc5-4d8a-a3e3-682f4c296161
---

We've previously discussed [integer barriers][b], [integer queues][q], and
[integer wait groups][wg] as tiny concurrency utilities. Next let's tackle
"once" initialization, i.e. [`pthread_once`][once], using an integer.
We'll need only three basic atomic operations — store, load, and increment
— and futex wait/wake. It will be zero-initialized and the entire source
small enough to fit on an old-fashioned terminal display. The interface
will also get an overhaul, more to my own tastes.

If you'd like to skip ahead: [**`once.c`**][src]

What's the purpose? Suppose a concurrent program requires initialization,
but has no definite moment to do so. Threads are already in motion, and
it's unpredictable which will arrive first, and when. It might be because
this part of the program is loaded lazily, or initialization is expensive
and only done lazily as needed. A "once" object is a control allowing the
first arrival to initialize, and later arrivals to wait until
initialization done.

The pthread version has this interface:

```c
pthread_once_t once = PTHREAD_ONCE_INIT;
int pthread_once(pthread_once_t *, void (*init)(void));
```

It's deliberately quite limited, and [the specification][spec] refers to
it merely as "dynamic package initialization." That is, it's strictly for
initializing global package data, not individual objects, and a "once"
object must be a static variable, not dynamically allocated. Also note the
lack of context pointer for the callback. No pthread implementation I
examined was actually so restricted, but the specification is written for
the least common denominator, and the interface is clearly designed
against more general use.

An example of lazily static table initialization for [a cipher][bf]:

```c
// Blowfish subkey tables (constants)
static uint32_t blowfish_p[20];
static uint32_t blowfish_s[256];
static pthread_once_t once = PTHREAD_ONCE_INIT;

static void init(void)
{
    // ... populate blowfish_p and blowfish_s with pi ...
}

void blowfish_encrypt(struct blowfish *ctx, void *buf, size_t len)
{
    pthread_once(&once, init);
    // ... lookups into blowfish_p and blowfish_s ...
}
```

The `pthread_once` allows `blowfish_encrypt` to be called concurrently (on
different context objects). The first call populates lookup tables and
others wait as needed. A good `pthread_once` will speculate initialization
has already completed and make that the fast path. The tables do not
require locks or atomics because `pthread_once` establishes a
synchronization edge: initialization *happens-before* the return from
`pthread_once`.

Go's `sync.Once` has a similar interface:

```go
func (o *Once) Do(f func())
```

It's more flexible and not restricted to global data, but retains the
callback interface.

### A new "once" interface

Callbacks are clunky, especially without closures, so in my re-imagining I
wanted to remove it from the interface. Instead I broke out exit and
entry. The in-between takes the place of the callback and it runs in its
original context.

```c
_Bool do_once(int *);
void once_done(int *);
```

This is similar to breaking "push" and "pop" each into two steps in my
concurrent queue. `do_once` returns true if initialization is required,
otherwise it returns false *after* initialization has completed, i.e. it
blocks. The initializing thread signals that initialization is complete by
calling `once_done`. As mentioned, the "once" object would be
zero-initialized. Reworking the above example:

```c
// Blowfish subkey tables (constants)
static uint32_t blowfish_p[20];
static uint32_t blowfish_s[256];
static int once = 0;

void blowfish_encrypt(struct blowfish *ctx, void *buf, size_t len)
{
    if (do_once(&once)) {
        // ... populate blowfish_p and blowfish_s with pi ...
        once_done(&once);
    }
    // ... lookups into blowfish_p and blowfish_s ...
}
```

It gets more interesting when taken beyond global initialization. Here
each object is lazily initialized by the first thread to use it:

```c
typedef struct {
    int once;
    // ...
} Thing;

static void expensive_init(Thing *, ptrdiff_t);

static double compute(Thing *t, ptrdiff_t index)
{
    if (do_once(&t->once)) {
        expensive_init(t, index);
        once_done(&t->once);
    }
    // ...
}

int main(void)
{
    // ...
    Thing *things = calloc(1000000, sizeof(Thing));
    #pragma omp parallel for
    for (int i = 0; i < iterations; i++) {
        ptrdiff_t which = random_access(i);
        double r = compute(&things[which], which);
        // ...
    }
    // ...
}
```

### Implementation details

A "once" object must express at least these three states:

1. Uninitialized
2. Undergoing initialization
3. Initialized

To support zero-initialization, (1) must map into zero. A thread observing
(1) must successfully transition to (2) before attempting to initialize. A
thread observing (2) must wait for a transition to (3). Observing (3) is
the fast path, and the implementation should optimize for it.

The trickiest part is the state transition from (1) to (2). If multiple
threads are attempting the transition concurrently, only one should "win".
The obvious choice is a [compare-and-swap][cas] atomic, which will fail if
another thread has already made the transition. However, with a more
careful selection of state representation, we can do this with just an
atomic increment!

The secret sauce: (2) will be **any positive value** and (3) will be **any
negative value**. The "winner" is the thread that increments from zero to
one. Other threads that also observed zero will increment to a different
value, after which they behave as though they did not observe (1) in the
first place.

I chose shorthand names for the three atomic and two futex operations.
Each can be defined with a single line of code — the atomics with
[compiler intrinsics][gcc] and the futex with system calls, as they
interact with the system scheduler. (See the "four elements" of [the wait
group article][wg].) Technically it will still work correctly if the futex
calls are no-ops, though it would waste time spinning on the slow path. In
a real program you'd probably use less pithy names.

```c
static int  load(int *);
static void store(int *, int);
static int  incr(int *);
static void wait(int *, int);
static void wake(int *);
```

From here it's useful to work backwards, starting with `once_done`,
because there's an important detail, another secret sauce ingredient:

```c
void once_done(int *once)
{
    store(once, INT_MIN);
    wake(once);
}
```

Recall that the "initialized" state (3) is negative. We don't just pick
any arbitrary negative, especially not the obvious -1, but *the most
negative value*. Keep that in mind. Once set, wake up any waiters. Since
this is the slow path, we don't care to avoid the system call if there are
no waiters. Now `do_once`:

```c
_Bool do_once(int *once)
{
    int r = load(once);
    if (r < 0) {
        return 0;
    } else if (r == 0) {
        r = incr(once);
        if (r == 1) {
            return 1;
        }
    }
    while (r > 0) {
        wait(once, r);
        r = load(once);
    }
    return 0;
}
```

First, check for the fast path. If we're already in state (3), return
immediately. If `do_once` will be placed in a separate translation unit
from the caller, we might extract this check such that it can be inlined
at the call site. Once initialization has settled, nobody will be mutating
`*once`, so this will be a fast, uncontended atomic load, though mind your
cache lines for false sharing.

If we're in state (1), try to transition to state (2). If we incremented
to 1, we won so tell the caller to initialize. Otherwise continue as
though we never saw state (1). There's an important subtlety easy to miss:
Initialization may have already completed before the increment. That is,
`*once` may have been negative for the increment! Fortunately since we
chose `INT_MIN` in `once_done`, it will *stay negative*. (Assuming you
have less than 2 billion threads contending `*once`. Ha!) So it's vital to
check `r` again for negative after the increment, hence `while` instead of
`do while`.

Losers continuing to increment `*once` may interfere with the futex wait,
but, again, this is the slow path so that's fine. Eventually we will wake
up and observe (3), then give control back to the caller.

That's all there is to it. If you haven't already, check out the source
including tests for for Windows and Linux: [**`once.c`**][src]. Suggested
experiments to try, particularly under a debugger:

* Change `INT_MIN` to `-1`.
* Change `while (r > 0) { ... }` to `do { ... } while (r > 0);`.
* Comment out the futex system calls. (Note: will be very slow without
  also reducing `NTHREADS`.)


[b]: /blog/2022/03/13/
[bf]: https://github.com/skeeto/prng64-shootout/blob/master/blowfish.c
[cas]: /blog/2014/09/02/
[gcc]: https://gcc.gnu.org/onlinedocs/gcc/_005f_005fatomic-Builtins.html
[once]: https://man7.org/linux/man-pages/man3/pthread_once.3p.html
[q]: /blog/2022/05/14/
[spec]: https://pubs.opengroup.org/onlinepubs/9699919799/functions/pthread_once.html
[src]: https://github.com/skeeto/scratch/blob/master/misc/once.c
[wg]: /blog/2022/10/05/
