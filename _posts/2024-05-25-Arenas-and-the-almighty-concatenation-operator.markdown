---
title: Arenas and the almighty concatenation operator
layout: post
date: 2024-05-25T00:00:00Z
tags: [c, cpp]
uuid: e88784ce-08fb-40d2-b6ad-c3d9af3cf5bc
---

I continue to streamline [an arena-based paradigm][arena], and stumbled
upon a concise technique for dynamic growth — an efficient, generic
"concatenate anything to anything" within an arena built atop a core of
9-ish lines of code. The key insight originated from a reader suggestion
about [dynamic arrays][da]. The subject of concatenation can be a string,
dynamic array, or even something else. The "system" is extensible, and
especially useful for path handling.

Continuing [from last time][cpp], the examples are in light, C-style C++.
I chose it because templates and function overloading express the concepts
succinctly. It uses no standard library functionality, so converting to C,
or similar, should be straightforward. The core concatenation "operator":

```c++
template<typename T>
T concat(arena *a, T head, T tail)
{
    if ((char *)(head.data+head.len) != a->beg) {
        head = T{a, head};
    }
    head.len += T{a, tail}.len;
    return head;
}
```

This concatenates two objects of the same type in the arena, and does so
*in place* if possible. That is, we can efficiently build a value piece by
piece. The type `T` must have `data` and `len` members, and a "copy"
constructor that makes a copy of the given object at *the front of the
arena*. Size integer overflows and out-of-memory errors are, as usual,
handled by the arena. In particular, note that the `len` addition happens
after allocation.

Since the front-of-the-arena business implicit, consider `assert`ing it if
you're worried. I've also considered declaring a `clone` "operator" where
that behavior is an explicit part of its interface.

```c++
// Make a copy of the object at the front of the arena.
template<typename T> T clone(arena *, T);

// In concat, replace the T{} constructors with clone:
    head = clone(a, head);
    head.len += clone(a, tail).len;
```

Strings are perhaps them most interesting subject of concatenation. Here's
a compatible string, `str`, definition from my previous article:

```c++
struct str {
    union {
        uint8_t    *data = 0;
        char const *cdata;
    };
    ptrdiff_t len = 0;

    str() = default;

    str(uint8_t *beg, uint8_t *end) : data{beg}, len{end-beg} {}

    template<ptrdiff_t N>
    constexpr str(char const (&s)[N]) : cdata{s}, len{N-1} {}

    str(arena *, str);  // TODO

    uint8_t &operator[](ptrdiff_t i) { return data[i]; }
};
```

This has `data`, `len`, and the necessary constructor declaration. Before
showing the constructor definition, here's an arena following the usual
formula, which should be familiar to those who've been following along:

```c++
struct arena {
    char *beg;
    char *end;
};

template<typename T, typename ...A>
T *makefront(ptrdiff_t count, arena *a, A ...args)
{
    ptrdiff_t size  = sizeof(T);
    ptrdiff_t align = -(uintptr_t)a->beg & (alignof(T) - 1);
    assert(count < (a->end - a->beg - align)/size);  // OOM
    T *r = (T *)(a->beg + align);
    a->beg += align + size*count;
    for (ptrdiff_t i = 0; i < count; i++) {
        new (r+i) T(args...);
    }
    return r;
}
```

Note how it bumps `beg`, not `end`, because it's allocated at the front.
That opens the end of the object for concatenation. When it returns, `beg`
points just past the end of the new object, aligned to it. Later, `concat`
inspects `beg` to see if it can *extend in place*. That will be true if
nothing else has been allocated *at the front* in the meantime. That is,
we can allocate objects *at the end* — such as [hash map nodes][map] —
while efficiently growing an object at the front through concatenation. If
it's not true for whatever reason, concatenation still works, just with
reduced efficiency.

With that out of the way, the "copy" constructor is simple:

```c++
str::str(arena *a, str s)
{
    data = makefront<uint8_t>(s.len, a);
    len = s.len;
    for (ptrdiff_t i = 0; i < len; i++) {
        data[i] = s[i];
    }
}
```

That's everything we need to put it into action. For example, a function
that deletes a file at a path following a path template.

```c++
char *tocstr(arena *a, str s)
{
    return (char *)concat(a, s, str{"\0"}).data;
}

bool removeconfig(str home, str program, arena scratch)
{
    str path = {};
    path = concat(&scratch, path, home);
    path = concat(&scratch, path, str{"/.config/"});
    path = concat(&scratch, path, program);
    path = concat(&scratch, path, str{"/rc"});
    return !unlink(tocstr(&scratch, path));
}
```

First, `concat` does all the heavy lifting in a null-terminated "C string"
conversion function that operates in place if possible. In `removeconfig`
I construct a path from path components, starting from a zero-initialized
*null string*. In the first `concat`, this null string is "copied" into
the arena, laying a foundation for additional concatenations. Each path
component is copied in place, so unlike [a dumb `strcat`][dumb], it's not
quadratic.

Even more, notice it supports arbitrary path lengths. No `PATH_MAX`,
`MAX_PATH`, etc., it grows into the arena as needed. No [huge stack
variables][chkstk] necessary, and the scratch arena automatically frees
the path on return. Fancier yet, imagine a variadic function that glues
path components together with the proper path delimiter, and it wouldn't
involve [a single, error-prone size calculation][sizes].

The `str{}` business is unfortunate. The `char` array constructor normally
kicks in in these situations, but compilers can't resolve the template
without an explicit `str` object. Perhaps there's a workaround, but I'm
not yet savvy enough with C++ to figure it out. In the C version you'd
always need to wrap those literals in the string macro.

### Extending concatenation

The "operator" can be extended by defining more overloads. For example, to
concatenate 32-bit integers to a string:

```c++
str concat(arena *a, str s, int32_t x)
{
    uint8_t  buf[16];
    uint8_t *end = buf + countof(buf);
    uint8_t *beg = end;
    int32_t  neg = x<0 ? x : -x;
    do {
        *--beg = '0' - neg%10;
    } while (neg /= 10);
    if (x < 0) {
        *--beg = '-';
    }
    return concat(a, s, {beg, end});
}
```

Now we can, say, construct a randomly-generated temporary path:

```c++
str path = {};
path = concat(&scratch, path, tempdir);
path = concat(&scratch, path, str{"/temp"});
int32_t id = rand32(&rng);
path = concat(&scratch, path, id);
```

Keep adding more definitions like this and you'll have something like, or
complementing, [buffered output][buf]. It doesn't stop there. Code points
concatenated as UTF-8:

```c++
str concat(arena *a, str s, char32_t rune)
{
    enum { REPLACEMENT_CHARACTER = 0xfffd };
    if (rune>=0xd800 && rune<=0xdfff) {
        rune = REPLACEMENT_CHARACTER;
    }

    uint8_t  buf[4];
    uint8_t *end = 0;
    if (rune < 0x80) {
        buf[0] = rune;
        end = buf + 1;
    } else if (rune < 0x800) {
        buf[0] =  (rune >>  6)         | 0xc0;
        buf[1] = ((rune >>  0) & 0x3f) | 0x80;
        end = buf + 2;
    } else if (rune < 0x10000) {
        buf[0] =  (rune >> 12)         | 0xe0;
        buf[1] = ((rune >>  6) & 0x3f) | 0x80;
        buf[2] = ((rune >>  0) & 0x3f) | 0x80;
        end = buf + 3;
    } else {
        buf[0] =  (rune >> 18)         | 0xf0;
        buf[1] = ((rune >> 12) & 0x3f) | 0x80;
        buf[2] = ((rune >>  6) & 0x3f) | 0x80;
        buf[3] = ((rune >>  0) & 0x3f) | 0x80;
        end = buf + 4;
    }
    return concat(a, s, {buf, end});
}
```

That composes well for general UTF-8 handling. For example, to ingest
Win32 strings (arguments, paths, etc.):

```c++
str convert(arena *perm, char16_t *s)
{
    str r = {};
    while (*s) {
        char32_t rune = decode(&s);
        r = concat(perm, r, rune);
    }
    return r;
}
```

### Beyond strings

One of my most useful C++ templates has been a span structure:

```c++
template<typename T>
struct span {
    T        *data = 0;
    ptrdiff_t len  = 0;

    span() = default;

    span(T *beg, T *end) : data{beg}, len{end-beg} {}

    span(arena *, span);  // for concat

    T &operator[](ptrdiff_t i) { return data[i]; }
};
```

The `span::span` definition looks exactly like `str::str`. In fact, we
could nearly define strings as `uint8_t` spans:

```c++
typedef span<uint8_t> str;  // hypothetical
```

Though I've found strings to be just special enough not to be worth it.

This `span` definition is now fleshed out sufficiently to use `concat`
with no additional definitions! However, outside of strings, concatenating
spans is unusual. More often we want to append individual elements. Again,
we can build on that core `concat` template:

```c++
template<typename T>
span<T> concat(arena *a, span<T> s, T v)
{
    return concat(a, s, span{&v, &v+1});
}
```

Now `span` is ready for 99% of its use cases. For example:

```c++
    span<int32_t> squares;
    for (int32_t i = 1; i <= 1000; i++) {
        squares = concat(&scratch, squares, i*i);
    }
```

It's often good enough, but it's not ideal as a general purpose dynamic
array. Each append makes a trip through arena allocation, and this span
cannot efficiently shrink and then grow again. Sometimes we'd like to
track capacity, covering both those cases.

```c++
template<typename T>
struct list {
    T        *data = 0;
    ptrdiff_t len  = 0;
    ptrdiff_t cap  = 0;

    list() = default;

    list(arena *, list);  // for concat

    T &operator[](ptrdiff_t i) { return data[i]; }
};
```

Unfortunately `cap` is a curve ball that the core template can't handle,
requiring a slightly more complex definition. Since concatenating whole
`list` objects is unusual, a definition for appending single elements:

```c++
template<typename T>
list<T> concat(arena *a, list<T> s, T v)
{
    if (s.len == s.cap) {
        if ((char *)(s.data+s.len) != a->beg) {
            s = list<T>{a, s};
        }
        ptrdiff_t extend = s.cap ? s.cap : 4;
        makefront<T>(extend, a);
        s.cap += extend;
    }
    s[s.len++] = v;
    return s;
}
```

Note how inside the `if` it's basically the same core definition. As
before, this definition extends in place if possible, but otherwise
handles it correctly anyway. In addition the above concerns, this `list`
is more suited to having multiple "open" dynamic arrays at once.

This concatenative concept has been a useful way to think about a variety
of situations in order to solve them effectively with arena allocation.

**Update**: NRK [sharply points out][nrk] that "extend in place" as
expressed in `concat` is incompatible with the [`alloc_size` and `malloc`
GCC function attributes][attr], which I've suggested in the past. While
considering how to mitigate this, we've also discovered that `alloc_size`
has always been fundamentally broken in GCC. Correct use is impossible,
and so *it must not be used*.


[attr]: https://gcc.gnu.org/onlinedocs/gcc/Function-Attributes.html
[arena]: /blog/2023/09/27/
[buf]: /blog/2023/02/13/
[chkstk]: /blog/2024/02/05/
[cpp]: /blog/2024/04/14/
[da]: /blog/2023/10/05/
[dumb]: /blog/2021/07/30/
[map]: /blog/2023/09/30/
[nrk]: https://lists.sr.ht/~skeeto/public-inbox/%3Cane2ee7fpnyn3qxslygprmjw2yrvzppxuim25jvf7e6f5jgxbd@p7y6own2j3it%3E#%3C2qzyqky3jtv6w64vicwnkrwa7nb52uohuu625bc3zrkaoor6ml@v57pb72uozpy%3E
[sizes]: /blog/2024/05/24/
