---
title: Parameterized types in C using the new tag compatibility rule
layout: post
date: 2025-06-26T23:49:53Z
tags: [c]
uuid: abb3bf93-074f-4876-8d46-42997edebb34
---

C23 has [a new rule for struct, union, and enum compatibility][n3037]
finally appearing in compilers starting with GCC 15, released this past
April, and Clang later this year. The same struct defined in different
translation units (TU) has always been compatible — essential to how they
work. Until this rule change, each such definition within a TU was a
distinct, incompatible type. The new rule says that, *ackshually*, they
are compatible! This unlocks some type parameterization using macros.

How can a TU have multiple definitions of a struct? Scope. Prior to C23
this wouldn't compile because the compound literal type and the return
type were distinct types:

```c
struct Example { int x, y, z; };

struct Example example(void)
{
    struct Example { int x, y, z; };
    return (struct Example){1, 2, 3};
}
```

Otherwise the definition of `struct Example` within `example` was fine, if
strange. At first this may not seem like a big deal, but let's [revisit my
technique for dynamic arrays][quick]:

```c
typedef struct {
    T        *data;
    ptrdiff_t len;
    ptrdiff_t cap;
} SliceT;
```

Where I write out one of these for each `T` that I might want to put into
a slice. With the new rule we can change it slightly, taking note of the
introduction of a tag (the name after `struct`):

```c
#define Slice(T)        \
    struct Slice##T {   \
        T        *data; \
        ptrdiff_t len;  \
        ptrdiff_t cap;  \
    }
```

This makes the "write it out ahead of time" thing simpler, but with the
new rule we can skip the "ahead of time" part and conjure slice types on
demand. Each declaration with the same `T` is compatible with the others
due to matching tags and fields. So, for example, with this macro we can
declare functions using slices parameterized for different element types.

```c
Slice(int) range(int, Arena *);

float mean(Slice(float));

Slice(Str) split(Str, char delim, Arena *);
Str join(Slice(Str), char delim, Arena *);
```

Or using it with [our model parser][obj]:

```c
typedef struct {
    float x, y, z;
} Vec3;

typedef struct {
    int32_t v[3];
    int32_t n[3];
} Face;

typedef struct {
    Slice(Vec3) verts;
    Slice(Vec3) norms;
    Slice(Face) faces;
} Model;

typedef Slice(Vec3) Polygon;
```

I worried these macros might confuse my tools, particularly [Universal
Ctags][ctags] because [it's important to me][w64]. Everything handles
prototypes better than expected, but ctags doesn't see fields with slice
types. Overall they're like a very limited form of C++ templates. Though
only the types are parameterized, not the functions operating on those
types. Outside of unwarranted macro abuse, this new technique does nothing
regarding generic functions. On the other hand, my generic slice function
complements the new technique, especially with the help of C23's new
`typeof` to mitigate `_Alignof`'s limitations:

```c
typedef struct { char *beg, *end; } Arena;
void *alloc(Arena *, ptrdiff_t count, int size, int align);

#define push(a, s)                          \
  ((s)->len == (s)->cap                     \
    ? (s)->data = push_(                    \
        (a),                                \
        (s)->data,                          \
        &(s)->cap,                          \
        sizeof(*(s)->data),                 \
        _Alignof(typeof(*(s)->data))        \
      ),                                    \
      (s)->data + (s)->len++                \
    : (s)->data + (s)->len++)

void *push_(Arena *a, void *data, ptrdiff_t *pcap, int size, int align)
{
    ptrdiff_t cap = *pcap;

    if (a->beg != (char *)data + cap*size) {
        void *copy = alloc(a, cap, size, align);
        memcpy(copy, data, cap*size);
        data = copy;
    }

    ptrdiff_t extend = cap ? cap : 4;
    alloc(a, extend, size, align);
    *pcap = cap + extend;
    return data;
}
```

This exploits the fact that implementations adopting the new tag rule also
have [the upcoming C2y null pointer rule][n3322] (note: also requires a
cooperating libc). Putting it together, now I can write stuff like this:

```c
Slice(int64_t) generate_primes(int64_t limit, Arena *a)
{
    Slice(int64_t) primes = {};

    if (limit > 2) {
        *push(a, &primes) = 2;
    }

    for (int64_t n = 3; n < limit; n += 2) {
        bool valid = true;
        for (ptrdiff_t i = 0; valid && i<primes.len; i++) {
            valid = n % primes.data[i];
        }
        if (valid) {
            *push(a, &primes) = n;
        }
    }

    return primes;
}
```

But it doesn't take long to run into limitations. It makes little sense to
define, say, a `Map(K, V)` without a generic function to manipulate it.
This also doesn't work:

```c
typedef struct {
    Slice(Str)          names;
    Slice(Slice(float)) edges;
} Graph;
```

Due to `Slice##T` in the macro, required to establish a unique tag for
each element type. The parameter to the macro must be an identifier, so
you have to build up to it (or define another macro), which sort of
defeats the purpose, which was entirely about convenience.

```c
typedef Slice(float) Edges;

typedef struct {
    Slice(Str)   names;
    Slice(Edges) edges;
} Graph;
```

The benefits are small enough that perhaps it's not worth the costs, but
it's been at least worth investigating. I've written a small demo of the
technique if you'd like to see it in action, or test the abilities of your
local C implementation: [`demo.c`][demo]


[ctags]: https://github.com/universal-ctags/ctags
[demo]: https://gist.github.com/skeeto/3fe27cd81ca5bdb4926b12e03bdfbc62
[n3037]: https://www.open-std.org/jtc1/sc22/wg14/www/docs/n3037.pdf
[n3322]: https://www.open-std.org/jtc1/sc22/wg14/www/docs/n3322.pdf
[obj]: /blog/2025/03/02/
[quick]: /blog/2025/01/19/
[w64]: https://github.com/skeeto/w64devkit
