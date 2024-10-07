---
title: Slim Reader/Writer Locks are neato
layout: post
date: 2024-10-03T22:40:13Z
tags: [win32, cpp]
uuid: 0bbd925e-c012-4711-b513-b34cd0357bfa
---

I'm 18 years late, but [Slim Reader/Writer Locks][srw] have a fantastic
interface: pointer-sized ("slim"), zero-initialized, and non-allocating.
Lacking cleanup, they compose naturally with [arena allocation][arena].
Sounds like a futex? That's because they're built on futexes introduced at
the same time. They're also complemented by [condition variables][cond]
with the same desirable properties. My only quibble is that slim locks
[could easily have been 32-bit objects][32], but it hardly matters. This
article, while treating [Win32 as a foreign interface][w32], discusses a
paper-thin C++ wrapper interface around lock and condition variables, in
[my own style][c++].

If you'd like to see/try a complete, working demonstration before diving
into the details: [`demo.cpp`][gist]. We're going to build this from the
ground up, so let's establish a few primitive integer definitions:

```c++
using b32 = signed;
using i32 = signed;
using uz  = decltype(0uz);
```

Think of `uz` as like `uintptr_t`. This implementation will support both
32-bit and 64-bit targets, and we'll need it as the basis for locks and
condition variables:

```c++
enum Lock : uz;
enum Cond : uz;
```

Opaque enums provide additional type safety: They have the properties of
an integer, including trivial destruction, but are distinct types which
compilers forbid mixing with other integers. We can't, say, accidentally
cross condition variable and lock parameters — my main concern. Aside from
zero-initialization, we do not actually care about the values of these
variables, so enumerators are unnecessary. (Caveat: GDB cannot display
opaque enums, which is slightly irritating.)

The documentation doesn't explicitly mention zero initialization, but the
official `*_INIT` constants are defined as zero. That locks in zero at the
ABI level, so we can count on it.

All the functions we'll need are exported by `kernel32.dll`. Locks have
two variations on lock/unlock: "exclusive" (write) and "shared" (read).
There are also "try" versions, but I won't be using them.

```c++
#define W32(r, p) extern "C" __declspec(dllimport) r __stdcall p noexcept
W32(void, AcquireSRWLockExclusive(Lock *));
W32(void, AcquireSRWLockShared(Lock *));
W32(void, ReleaseSRWLockExclusive(Lock *));
W32(void, ReleaseSRWLockShared(Lock *));
```

Declaring Win32 functions in C++ is a mouthful, and everything must be
written in just the right order, but it's mostly tucked away in a macro.
Usually there's a stack discipline to these locks, so an RAII scoped guard
is in order:

```c++
struct Guard {
    Lock *l;
    Guard(Lock *l) : l{l} { AcquireSRWLockExclusive(l); }
    ~Guard()              { ReleaseSRWLockExclusive(l); }
};

struct RGuard {
    Lock *l;
    RGuard(Lock *l) : l{l} { AcquireSRWLockShared(l); }
    ~RGuard()              { ReleaseSRWLockShared(l); }
};
```

Dead simple. (What about [rule of three][3]? Instead of working around
this language design flaw, [reach into the distant future][dtor] where
it's been fixed: `-Werror=deprecated-copy-dtor`.) Usage might look like:

```c++
struct Example {
    Lock lock = {};
    i32  value;
};

i32 incr(Example *e)
{
    Guard g(&e->lock);
    return ++e->value;
}
```

Note the `= {}` to guarantee the lock is always ready for use. It gets
more interesting with condition variables in the mix. That's three more
functions:

```c++
W32(b32,  SleepConditionVariableSRW(Cond *, Lock *, i32, b32));
W32(void, WakeAllConditionVariable(Cond *));
W32(void, WakeConditionVariable(Cond *));
```

The last parameter on [SleepConditionVariableSRW][sleep] indicates if the
lock was acquired shared. Why do locks have distinct acquire and release
functions while condition variables use a flag for the same purpose? Beats
me. I'll unfold it into two functions, selected by type, with a default
infinite wait:

```c++
b32 wait(Cond *c, Guard *g, i32 ms = -1)
{
    return SleepConditionVariableSRW(c, g->l, ms, 0);
}

b32 wait(Cond *c, RGuard *g, i32 ms = -1)
{
    return SleepConditionVariableSRW(c, g->l, ms, 1);
}
```

Usage might look like:

```c++
for (RGuard g(&lock); remaining;) {
    wait(&done, &g);
}
```

The other side is nothing more than a rename (but could also be
[accomplished through linking][rename]):

```c++
void signal(Cond *c)
{
    WakeConditionVariable(c);
}

void broadcast(Cond *c)
{
    WakeAllConditionVariable(c);
}
```

And a couple examples of its usage:

```c++
if (Guard g(&lock); !--remaining) {
    signal(&done);
}

// Or:

Guard g(&lock);
ready = true;
broadcast(&init);
while (remaining) {
    wait(&done, &g);
}
```

A satisfying, powerful synchronization interface with hardly any code!


[32]: /blog/2022/10/05/
[3]: https://en.cppreference.com/w/cpp/language/rule_of_three
[arena]: /blog/2023/09/27/
[c++]: /blog/2024/04/14/
[cond]: https://learn.microsoft.com/en-us/windows/win32/sync/condition-variables
[gist]: https://gist.github.com/skeeto/42adc0c90a156d4457422e034be697e8
[dtor]: https://quuxplusone.github.io/blog/2023/05/05/deprecated-copy-with-dtor/
[rename]: /blog/2023/08/27/
[sleep]: https://learn.microsoft.com/en-us/windows/win32/api/synchapi/nf-synchapi-sleepconditionvariablesrw
[srw]: https://learn.microsoft.com/en-us/windows/win32/sync/slim-reader-writer--srw--locks
[w32]: /blog/2023/05/31/
