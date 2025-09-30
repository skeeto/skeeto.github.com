---
title: More speculations on arenas in C++
layout: post
date: 2025-09-30T11:46:16Z
tags: []
uuid: ffce917f-c757-42e7-a4d1-55e8d80c5051
---

Patrice Roy's new book, [*C++ Memory Management*][mm], has made me more
conscious of object lifetimes. C++ is stricter than C about lifetimes, and
common, textbook memory management that's sound in C is less so in C++ —
*more than I realized*. The book also presents a form of arena allocation
so watered down as to enjoy none of the benefits. (Despite its precision
otherwise, the second half is also littered with [integer overflows][io]
lacking [the appropriate checks][z], and near the end has some [pointer
overflows][po] invalidating the check.) However, I'm grateful for the new
insights, and it's made me revisit [my own C++ arena allocation][prev]. In
this new light I see I got it subtly wrong myself!

Surprising to most C++ programmers, but not language lawyers, [idiomatic C
memory allocation was ill-formed in C++ until recently][wg21]:

```c++
int *newint(int v)
{
    int *r = (int *)malloc(sizeof(*r));
    if (r) {
        *r = v;  // <-- undefined behavior before C++20
    }
    return r;
}
```

This program allocates memory for an object but never starts a lifetime.
Assignment without a lifetime is invalid. Pointer casts are that much more
suspicious in C++, and due to lifetime semantics, in many cases indicate
incorrect code. (To be clear, I'm not arguing in favor of these semantics,
but reasoning about the facts on the ground.) C++20 carved out special
exceptions for `malloc` and friends, but addressing this kind of thing in
general is the purpose of the brand new [`start_lifetime_as`][sla] (and
similar), the slightly older [`construct_at`][ca], or a classic placement
new. They all start lifetimes. The last looks like:

```c++
int *newint(int v)
{
    void *r = malloc(sizeof(int));
    if (r) {
        return new(r) int{v};
    }
    return nullptr;
}
```

That's no good as a C/C++ polyglot, though per the differing old semantics
that was impossible anyway without macros. Which is basically cheating. An
important detail: The corrected version has no casts, and it returns the
result of `new`. That's important because only the pointer returned by
`new` is imbued as a pointer to the new lifetime, *not* `r`. There are no
side effects affecting the provenance of `r`, which still points to raw
memory as far as the language is concerned.

With that in mind let's revisit my arena from last time, which does not
necessarily benefit from the recent changes, not being one of the special
case C standard library functions:

```c++
struct Arena {
    char *beg;
    char *end;
};

template<typename T>
T *alloc(Arena *a, ptrdiff_t count = 1)
{
    ptrdiff_t size = sizeof(T);
    ptrdiff_t pad  = -(uintptr_t)a->beg & (alignof(T) - 1);
    assert(count < (a->end - a->beg - pad)/size);  // OOM policy
    T *r = (T *)(a->beg + pad);
    a->beg += pad + count*size;
    for (ptrdiff_t i = 0; i < count; i++) {
        new((void *)&r[i]) T{};
    }
    return r;
}
```

Hey, look, placement new! I did that to produce a nicer interface, but I
lucked out also starting lifetimes appropriately. Except it returns the
wrong pointer. This allocator discards the pointer blessed with the new
lifetime. Both pointers have the same address but different provenance.
That matters. But I'm calling `new` many times, so how do I fix this?
Array new, duh.

```c++
template<typename T>
T *alloc(Arena *a, ptrdiff_t count = 1)
{
    ptrdiff_t size = sizeof(T);
    ptrdiff_t pad  = -(uintptr_t)a->beg & (alignof(T) - 1);
    assert(count < (a->end - a->beg - pad)/size);  // OOM policy
    void *r = a->beg + pad;
    a->beg += pad + count*size;
    return new(r) T[count]{};
}
```

Wow… that's actually much better anyway. No explicit casts, no loop. Why
didn't I think of this in the first place? The catch is I can't forward
constructor arguments, emplace-style — the part that gave me the trouble
with perfect forwarding — but that's for the best. Forwarding more than
once was unsound, made more obvious by the array new.

Since I'm thinking about lifetimes, what about the other end? My arena
does not call destructors, by design, and starts new lifetimes on top of
objects that are technically still alive. Is that undefined behavior? As
far as I can tell [this is allowed][l], even for non-trivial destructors,
with the caveat that it might leak resources. In this case the resource is
memory managed by the arena, so that's fine of course.

So addressing pointer provenance also produced a nicer definition. What a
great result from reading that book! While researching, I noticed Jonathan
Müller, who personally gave me great advice and feedback on my previous
article, [talked about lifetimes][jm] just a couple weeks later. I
recommend both.


[ca]: https://en.cppreference.com/w/cpp/memory/construct_at.html
[io]: https://github.com/PacktPublishing/C-Plus-Plus-Memory-Management/blob/9e4c4ea7/chapter12/Vector-better.cpp#L45
[jm]: https://www.youtube.com/watch?v=oZyhq4D-QL4
[l]: https://en.cppreference.com/w/cpp/language/lifetime.html#Storage_reuse
[mm]: https://www.packtpub.com/en-us/product/c-memory-management-9781805129806
[po]: https://github.com/PacktPublishing/C-Plus-Plus-Memory-Management/blob/9e4c4ea7/chapter14/Vector_with_allocator_cpp23.cpp#L118-L119
[prev]: /blog/2024/04/14/
[sla]: https://en.cppreference.com/w/cpp/memory/start_lifetime_as.html
[sr]: https://en.cppreference.com/w/cpp/language/lifetime.html#Storage_reuse
[wg21]: https://wg21.link/P0593#idiomatic-c-code-as-c
[z]: /blog/2024/05/24/
