---
title: Speculations on arenas and custom strings in C++
layout: post
date: 2024-04-14T00:39:18Z
tags: [c, cpp]
uuid: 6b07a406-b303-4c2b-8afd-3e589b26eaa1
---

My techniques with [arena allocation][arena] and [strings][style] are
oriented around C. I'm always looking for a better way, and lately I've
been experimenting with building them using C++ features. What are the
trade-offs? Are the benefits worth the costs? In this article I lay out my
goals, review implementation possibilities, and discuss my findings.
Following along will require familiarity with those previous two articles.

Some of C++ is beyond my mental capabilities, and so I cannot wield those
parts effectively. Other parts I *can* wrap my head around, but it
requires substantial effort and the inevitable mistakes are difficult to
debug. So a general goal is to minimize contact with that complexity, only
touching a few higher-value features that I can use confidently.

Existing practice is unimportant. I've seen where that goes. [Like the C
standard library][libc], the C++ standard library offers me little. Its
concepts regarding ownership and memory management are irreconcilable
(move semantics, smart pointers, etc.), so I have to build from scratch
anyway. So absolutely no including C++ headers. The most valuable features
are built right into the language, so I won't need to include library
definitions.

No [`public` or `private`][hh]. Still no `const` beyond what is required
to access certain features. This means I can toss out a bunch of keywords
like `class`, `friend`, etc. It eliminates noisy, repetitive code and
interfaces — getters, setters, separate `const` and non-`const` — which in
my experience means fewer defects.

No references beyond mandatory cases. References hide addresses being
taken — or merely implies it, when it's actually an expensive copy — which
is an annoying experience when reading unfamiliar C++. After all, for
arenas the explicit address-taking (permanent) or copying (scratch) is a
critical part of communicating the interfaces.

In theory `constexpr` could be useful, but it keeps falling short when I
try it out, so I'm ignoring it. I'll elaborate in a moment.

Minimal template use. They blow up compile times and code size, they're
noisy, and in practice they make debug builds (i.e. `-O0`) much slower
(typically ~10x) because there's no optimization to clean up the mess.
I'll only use them for a few foundational purposes, such as allocation.
(Though this article *is* about the fundamental stuff.)

No methods aside from limited use of operator overloads. I want to keep a
C style, plus methods just look ugly without references: `obj->func()` vs.
`func(obj)`. (Why are we still writing `->` in the 21st century?) Function
overloading can instead differentiate "methods." Overloads are acceptable
in moderation, especially because I'm paying for it (symbol decoration)
whether or not I take advantage.

Finally, no exceptions of course. I assume `-fno-exceptions`, or the local
equivalent, is active.

### Allocation

Let's start with allocation. Since writing that previous article, I've
streamlined arena allocation in C:

```c
#define new(a, t, n)  (t *)alloc(a, sizeof(t), _Alignof(t), n)

typedef struct {
    byte *beg;
    byte *end;
} arena;

static byte *alloc(arena *a, size objsize, size align, size count)
{
    assert(count >= 0);
    size pad = (uptr)a->end & (align - 1);
    assert(count < (a->end - a->beg - pad)/objsize);  // oom
    return memset(a->end -= objsize*count + pad, 0, objsize*count);
}
```

(As needed, replace the second `assert` with whatever out of memory policy
is appropriate.) Then allocating, say, a [10k-element hash table][msi]
(i.e. to keep it [off the stack][stack]):

```c
    i16 *seen = new(&scratch, i16, 1<<14);
```

With C++, I initially tried [placement new][new] with the arena as the
"place" for the allocation:

```c++
void *operator new(size_t, arena *);  // avoid this
```

Then to create a single object:

```c++
    object *o = new (&scratch) object{};
```

This exposes the constructor, but everything else about it is poor. It
relies on complex, finicky rules governing `new` overloads, especially for
alignment handling. It's difficult to tell what's happening, and it's too
easy to make mistakes that compile. That doesn't even count the mess that
is array `new[]`.

I soon learned it's better to replace the `new` macro with a template,
which can actually see what it's doing. I can't call it `new` in C++, so I
settled on `make` instead:

```c++
template<typename T>
static T *make(arena *a, size count = 1)
{
    assert(count >= 0);
    size objsize = sizeof(T);
    size align   = alignof(T);
    size pad     = (uptr)a->end & (align - 1);
    assert(count < (a->end - a->beg - pad)/objsize);  // oom
    a->end -= objsize*count + pad;
    T *r = (T *)a->end;
    for (size i = 0; i < count; i++) {
        new ((void *)&r[i]) T{};
    }
    return r;
}
```

Then allocating that hash table becomes:

```c++
    i16 *seen = make<i16>(&scratch, 10000);
```

Or a single object, relying on the default argument:

```c++
    object *o = make<object>(&scratch);
```

Due to placement new, merely for invoking the constructor, these objects
aren't just zero-initialized, but value-initialized. It can only construct
objects that define an empty initializer, but in exchange unlocks some
interesting possibilities:

```c++
struct mat3 {
    f32 data[9] = {
        1, 0, 0,
        0, 1, 0,
        0, 0, 1,
    };
};

struct list {
    node  *head = 0;
    node **tail = &head;
};
```

When a zero-initialized state isn't ideal, objects can still initialize to
a more useful state straight out of the arena. The second case is even
self-referencing, which is specifically supported through placement new.
Otherwise you'd need a special-written copy or move constructor.

`make` could accept constructor arguments and perfect forward them to a
constructor. However, that's too far into the dark arts for my comfort,
plus it requires a correct definition of `std::forward`. In practice that
means `#include`-ing it, and whatever comes in with it. Or ask an expert
capable of writing such a definition from scratch, though both are
probably too busy.

**Update 1**: One of those experts, Jonathan Müller, kindly reached out to
say that [a static cast is sufficient][fwd]. This is easy to do:

```c++
template<typename T, typename ...A>
static T *make(arena *a, size count = 1, A &&...args)
{
    // ...
        new ((void *)&r[i]) T{(A &&)args...};
    // ...
}
```

**Update 2**: I later realized that because I do not care about copy or
move semantics, I also don't care about perfect forwarding. I can simply
expand the parameter pack without casting or `&&`. I also don't want the
extra restrictions on braced initializer conversions, so better to use
parentheses with `new`.

```c++
template<typename T, typename ...A>
static T *make(arena *a, size count = 1, A ...args)
{
    // ...
        new ((void *)&r[i]) T(args...);
    // ...
}
```

One small gotcha: placement new doesn't work out of the box, and you need
to provide a definition. That means including `<new>` or writing one out.
Fortunately it's trivial, but the prototype must exactly match, including
`size_t`:

```c++
void *operator new(size_t, void *p) { return p; }
```

Overall I feel the template is a small improvement over the macro.

### Strings

Recall my basic C string type, with a macro to wrap literals:

```c
#define countof(a)  (size)(sizeof(a) / sizeof(*(a)))
#define s8(s)       (s8){(u8 *)s, countof(s)-1}

typedef struct {
    u8  *data;
    size len;
} s8;
```

Since it doesn't own the underlying buffer — region-based allocation has
already solved the ownership problem — this is what C++ long-windedly
calls a `std::string_view`. In C++ we won't need the `countof` macro for
strings, but it's still generally useful. Converting it to a template,
which is *theoretically* more robust (rejects pointers), but comes with [a
non-zero cost][debug]:

```c
template<typename T, size N>
size countof(T (&)[N])
{
    return N;
}
```

The reference — here a reference to an array — is unavoidable, so it's one
of the rare cases. The same concept applies as an `s8` constructor to
replace the macro:

```c
struct s8 {
    u8  *data = 0;
    size len  = 0;

    s8() = default;

    template<size N>
    s8(const char (&s)[N]) : data{(u8 *)s}, len{N-1} {}
};
```

I've explicitly asked to keep a default zero-initialized (empty) string
since it's useful — and necessary to directly allocate strings using
`make`, e.g. an array of strings. `const` is required because string
literals are `const` in C++, but it's immediately stripped off for the
sake of simplicity. The new constructor allows:

```c++
    s8 version = "1.2.3";
```

Or even [more usefully][buf]:

```c++
    void print(bufout *, s8);
    // ...
    print(stdout, "hello world\n");
```

Define `operator==` and it's more useful yet:

```c++
    b32 operator==(s8 s)
    {
        return len==s.len && (!len || !memcmp(data, s.data, len));
    }
```

Now this works, and it's cheap and fast even in debug builds:

```c
    s8 key = ...;
    if (key == "HOME") {
        // ...
    }
```

That's more ergonomic than the macro and comparison function. `operator[]`
also improves ergonomics, to subscript a string without going through the
`data` member:

```c++
    u8 &operator[](size i)
    {
        assert(i >= 0);
        assert(i < len);
        return data[i];
    }
```

The reference is again necessary to make subscripts assignable. Since
`s8span` — make a string spanning two pointers — so often appears in my
programs, a constructor seems appropriate, too:

```c++
    s8(u8 *beg, u8 *end)
    {
        assert(beg <= end);
        data = beg;
        len = end - beg;
    }
```

By the way, these assertions I've been using are great for catching
mistakes quickly and early, and they complement [fuzz testing][fuzz].

I'm not sold on it, but an idea for the future: C++23's multi-index
`operator[]` as a slice operator:

```c++
    s8 operator[](size beg, size end)
    {
        assert(beg >= 0);
        assert(beg <= end);
        assert(end <= len);
        return {data+beg, data+end};
    }
```

Then:

```c++
    s8 msg = "foo bar baz";
    msg = msg[4,7];  // msg = "bar"
```

I could keep going with, say, iterators and such, but each will be more
specialized and less useful. (I don't care about range-based `for` loops.)

### Downside: static initialization

The new string stuff is neat, but I hit a wall trying it out: These fancy
constructors do not reliably construct at compile time, *not even with a
`constexpr` qualifier* in two of the three major C++ implementations. A
static lookup table that contains a string is likely constructed at run
time in at least some builds. For example, this table:

```c++
static s8 keys[] = {"foo", "bar", "baz"};
```

Requires run-time construction in real world cases I care about, requiring
C++ magic and linking runtime gunk. The constructor is therefore a strict
downgrade from the macro, which works perfectly in these lookup tables.
Once a non-default constructor is defined, I've been unable to find an
escape hatch back to the original, dumb, reliable behavior.

**Update**: Jonathan Müller points out the reinterpret cast is forbidden
in a `constexpr` function, so it's not required to happen at compile time.
After some thought, I've figured out a workaround using a union:

```c++
struct s8 {
    union {
        u8         *data = 0;
        const char *cdata;
    };
    size len = 0;

    template<size N>
    constexpr s8(const char (&s)[N]) : cdata{s}, len{N-1} {}

    // ...
}
```

In all three C++ implementations, in all configurations, this reliably
constructs strings at compile time. The other semantics are unchanged.

### Other features

Having a generic dynamic array would be handy, and more ergonomic than [my
dynamic array macro][slice]:

```c
template<typename T>
struct slice {
    T   *data = 0;
    size len  = 0;
    size cap  = 0;

    slice<T> = default;

    template<size N>
    slice<T>(T (&a)[N]) : data{a}, len{N}, cap{N} {}

    T &operator[](size i) { ... }
}

template<typename T>
slice<T> append(arena *, slice<T>, T);
```

On the other hand, [hash maps are mostly solved][map], so I wouldn't
bother with a generic map.

Function overloads would simplify naming. For example, this in C:

```c
prints8(bufout *, s8);
printi32(bufout *, i32);
printf64(bufout *, f64);
printvec3(bufout *, vec3);
```

Would hide that stuff behind the scenes in the symbol decoration:

```c
print(bufout *, s8);
print(bufout *, i32);
print(bufout *, f64);
print(bufout *, vec3);
```

Same goes for a `hash()` function on different types.

C++ has better null pointer semantics than C. Addition or subtraction of
zero with a null pointer produces a null pointer, and subtracting null
pointers results in zero. This eliminates some boneheaded special case
checks required in C, though not all: `memcpy`, for instance, arbitrarily
still does not accept null pointers even in C++.

### Ultimately worth it?

The static data problem is a real bummer, but perhaps it's worth it for
the other features. I still need to put it all to the test in a real,
sizable project.


[arena]: /blog/2023/09/27/
[buf]: /blog/2023/02/13/
[debug]: https://vittorioromeo.info/index/blog/debug_performance_cpp.html
[fuzz]: /blog/2019/01/25/
[fwd]: https://www.foonathan.net/2020/09/move-forward/
[hh]: https://www.youtube.com/watch?v=uHSLHvWFkto&t=4386s
[libc]: /blog/2023/02/11/
[map]: /blog/2023/09/30/
[msi]: /blog/2023/06/26/
[new]: https://en.cppreference.com/w/cpp/language/new#Placement_new
[slice]: /blog/2023/10/05/
[stack]: /blog/2024/02/05/
[style]: /blog/2023/10/08/
