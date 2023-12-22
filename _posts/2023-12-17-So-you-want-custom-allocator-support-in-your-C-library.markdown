---
title: So you want custom allocator support in your C library
layout: post
date: 2023-12-17T17:52:26Z
tags: [c]
uuid: 1ffa33fe-c701-4cf7-b8fb-6c30a14497a3
excerpt_separator: <!--more-->
---

*This article was discussed [on Hacker News][hn] and [on reddit][reddit].*

Users of mature C libraries conventionally get to choose how memory is
allocated — that is, when it [cannot be avoided entirely][mini]. The C
standard never laid down a convention — [perhaps for the better][libc] —
so each library re-invents an allocator interface. Not all are created
equal, and most repeat a few fundamental mistakes. Often the interface is
merely a token effort, to check off that it's "supported" without actual
consideration to its use. This article describes the critical features of
a practical allocator interface, and demonstrates why they're important.

<!--more-->

Before diving into the details, here's the checklist for library authors:

1. All allocation functions accept a user-defined context pointer.
2. The "free" function accepts the original allocation size.
3. The "realloc" function accepts both old and new size.

### Context pointer

The standard library allocator keeps its state in global variables. This
makes for a simple interface, but comes with significant performance and
complexity costs. These costs likely motivate custom allocator use in the
first place, in which case slavishly duplicating the standard interface is
essentially the worst possible option. Unfortunately this is typical:

```c
#define LIB_MALLOC  malloc
#define LIB_FREE    free
```

I could observe the library's allocations, and I could swap in a library
functionality equivalent to the standard library allocator — jemalloc,
mimalloc, etc. — but that's about it. Better than nothing, I suppose, but
only just so. Function pointer callbacks are slightly better:

```c
typedef struct {
    void *(*malloc)(size_t);
    void  (*free)(void *);
} allocator;

session *session_new(..., allocator);
```

At least I could use different allocators at different times, and there
are even [tricks to bind a context pointer][closure] to the callback. It
also works when the library is dynamically linked.

Either case barely qualifies as custom allocator support, and they're
useless when it matters most. Only a small ingredient is needed to make
these interfaces useful: a context pointer.

```c
// NOTE: Better, but still not great
typedef struct {
    void *(*malloc)(size_t, void *ctx);
    void  (*free)(void *, void *ctx);
    void   *ctx;
} allocator;
```

Users can choose *from where* the library will allocate at at given time.
It liberates the allocator from global variables (or janky workarounds),
and multithreading woes. The default can still hook up to the standard
library through stubs that fit these interfaces.

```c
static void *lib_malloc(size_t size, void *ctx)
{
    (void)ctx;
    return malloc(size);
}

static void *lib_free(void *ptr, void *ctx)
{
    (void)ctx;
    free(ptr);
}

static allocator lib_allocator = {lib_malloc, lib_free, 0};
```

Note that the context pointer came after the "standard" arguments. All
things being equal, "extra" arguments should go after standard ones. But
don't sweat it! In the most common calling conventions this allows stub
implementations to be merely an unconditional jump. It's *as though* the
stubs are a kind of subtype of the original functions.

```nasm
lib_malloc:
        jmp malloc
lib_free:
        jmp free
```

Typically the decision is completely arbitrary, and so this minutia tips
the balance.

#### Context pointer example

So what's the big deal? It means we can trivially plug in, say, a [tiny
arena allocator][arena]. To demonstrate, consider this fictional string
set and partial JSON API, each of which supports a custom allocator. For
simplicity — I'm attempting to balance substance and brevity — they share
an allocator interface. (Note: Because [subscripts and sizes should be
signed][sign], and we're now breaking away from the standard library
allocator, I will use `ptrdiff_t` for the rest of the examples.)

```c
typedef struct {
    void *(*malloc)(ptrdiff_t, void *ctx);
    void  (*free)(void *, void *ctx);
    void   *ctx;
} allocator;

typedef struct set set;
set  *set_new(allocator *);
set  *set_free(set *);
bool  set_add(set *, char *);

typdef struct json json;
json     *json_load(char *buf, ptrdiff_t len, allocator *);
json     *json_free(json *);
ptrdiff_t json_length(json *);
json     *json_subscript(json *, ptrdiff_t i);
json     *json_getfield(json *, char *field);
double    json_getnumber(json *);
char     *json_getstring(json *);
```

`set` and `json` objects retain a copy of the `allocator` object for all
allocations made through that object. Given nothing, they default to the
standard library using the pass-through definitions above. Used together
with the standard library allocator:

```c
typedef struct {
    double sum;
    bool   ok;
} sum_result;

sum_result sum_unique(char *json, ptrdiff_t len)
{
    sum_result r = {0};
    json *namevals = json_load(json, len, 0);
    if (!namevals) {
        return r;  // parse error
    }

    ptrdiff_t arraylen = json_length(namevals);
    if (arraylen < 0) {
        json_free(namevals);
        return r;  // not an array
    }

    set *seen = set_new(0);
    for (ptrdiff_t i = 0; i < arraylen; i++) {
        json *element = json_subscript(namevals, i);
        char *name    = json_getfield(element, "name");
        char *value   = json_getfield(element, "value");
        if (!name || !value) {
            set_free(set);
            json_free(namevals);
            return r;  // invalid element
        } else if (set_add(set, name)) {
            r.sum += json_getnumber(value);
        }
    }

    set_free(set);
    json_free(namevals);
    r.ok = 1;
    return r;
}
```

Which given as JSON input:

```json
[
    {"name": "foo", "value":  123},
    {"name": "bar", "value":  456},
    {"name": "foo", "value": 1000}
]
```

Would return `579.0`. Because it's using standard library allocation, it
must carefully clean up before returning. There's also no out-of-memory
handling because, in practice, programs typically do not get to observe
and respond to the standard allocator running out of memory.

We can improve and simplify it with an arena allocator:

```c
typedef struct {
    char    *beg;
    char    *end;
    jmp_buf *oom;
} arena;

void *arena_malloc(ptrdiff_t size, void *ctx)
{
    arena *a = ctx;
    ptrdiff_t available = a->end - a->beg;
    ptrdiff_t alignment = -size & 15;
    if (size > available-alignment) {
        longjmp(*a->oom);
    }
    return a->end -= size + alignment;
}

void arena_free(void *ptr, void *ctx)
{
    // nothing to do (yet!)
}
```

I'm allocating from the end rather than the beginning because it will make
a later change simpler. Applying that to the function:

```c
sum_result sum_unique(char *json, ptrdiff_t len, arena scratch)
{
    sum_result r = {0};

    allocator a = {0};
    a.malloc = arena_malloc;
    a.free = arena_free;
    a.ctx = &scratch;

    json *namevals = json_load(json, len, &a);
    if (!namevals) {
        return r;  // parse error
    }

    ptrdiff_t arraylen = json_length(namevals);
    if (arraylen < 0) {
        return r;  // not an array
    }

    set *seen = set_new(&a);
    for (ptrdiff_t i = 0; i < arraylen; i++) {
        json *element = json_subscript(namevals, i);
        char *name    = json_getfield(element, "name");
        char *value   = json_getfield(element, "value");
        if (!name || !value) {
            return r;  // invalid element
        } else if (set_add(set, name)) {
            r.sum += json_getnumber(value);
        }
    }
    r.ok = 1;
    return r;
}
```

Calls to `set_free` and `json_free` are no longer necessary because the
arena automatically frees these on any return, in O(1). I almost feel bad
the library authors bothered to write them! It also handles allocation
failure without introducing it to `sum_unique`. We may even deliberately
restrict the memory available to this function — perhaps because the input
is untrusted, and we want to quickly abort denial-of-service attacks — by
giving it a small arena, relying on out-of-memory to reject pathological
inputs.

There are so many possibilities unlocked by the context pointer.

### Provide the original allocation size when freeing

When an application frees an object it always has the original, requested
allocation size on hand. After all, it's a necessary condition to use the
object correctly. In the simplest case it's the size of the freed object's
type: a static quantity. If it's an array, then it's a multiple of the
tracked capacity: a dynamic quantity. In any case the size is either known
statically or tracked dynamically by the application.

Yet `free()` does not accept a size, meaning that the allocator must track
the information redundantly! That's a needless burden on custom
allocators, and with a bit of care a library can lift it.

This was noticed in C++, and WG21 added [sized deallocation][sized] in
C++14. It's now the default on two of the three major implementations (and
probably not the two you'd guess). In other words, object size is so
readily available that it can mostly be automated away. Notable exception:
`operator new[]` and `operator delete[]` with trivial destructors. With
non-trivial destructors, `operator new[]` must track the array length for
its its own purposes *on top of libc bookkeeping*. In other words, array
allocations have their size stored in at least three different places!

That means the "free" interface should look like this:

```c
void *lib_free(void *ptr, ptrdiff_t len, void *ctx);
```

And calls inside the library might look like:

```c
lib_free(p, sizeof(*p), ctx);
lib_free(a, sizeof(*a)*len, ctx);
```

Now that `arena_free` has size information, it can free an allocation if
it was the most recent:

```c
void arena_free(void *ptr, ptrdiff_t size, void *ctx)
{
    arena *a = ctx;
    if (ptr == a->end) {
        ptrdiff_t alignment = -size & 15;
        a->end += size + alignment;
    }
}
```

If the library allocates short-lived objects to compute some value, then
discards in reverse order, the memory can be reused. The arena doesn't
have to do anything special. The library merely needs to share its
knowledge with the allocator.

Beyond arena allocation, an allocator could use the size to locate the
allocation's size class and, say, push it onto a freelist of its size
class. [Size-class freelists compose well with arenas][c++], and an
implementation is short and simple when the caller of "free" communicates
object size.

Another idea: During testing, use a debug allocator that tracks object
size and validates the reported size against its own bookkeeping. This can
help catch mistakes sooner.

### Provide the old size when resizing an allocation

Resizing an allocation requires a lot from an allocator, and it should be
avoided if possible. At the very least it cannot be done *at all* without
knowing the original allocation size. An allocator can't simply no-op it
like it can with "free." With the standard library interface, allocators
have no choice but to redundantly track object sizes when "realloc" is
required.

So, just as with "free," the allocator should be given the old object
size!

```c
void *lib_realloc(void *ptr, ptrdiff_t old, ptrdiff_t new, void *ctx);
```

At the very least, an allocator could implement "realloc" with "malloc"
and `memcpy`:

```c
void arena_realloc(void *ptr, ptrdiff_t old, ptrdiff_t new, void *ctx)
{
    assert(new > old);
    void *r = arena_malloc(new, ctx);
    return memcpy(r, ptr, old);
}
```

Of the three checklist items, this is the most neglected. Exercise for the
reader: The last-allocated object *can* be resized in place, instead using
`memmove`. If this is frequently expected, allocate from the front, adjust
`arena_free` as needed, and extend the allocation in place [as discussed a
previous addendum][slice], without any copying.

### Real world examples

Let's examine real world examples to see how well they fit the checklist.
First up is [uthash][], a popular, easy-to-use, intrusive hash table:

```c
#define uthash_malloc(sz) my_malloc(sz)
#define uthash_free(ptr, sz) my_free(ptr)
```

No "realloc" so it trivially checks (3). It optionally provides the old
size to "free" which checks (2). However it misses (1) which is the most
important, greatly limiting its usefulness.

Next is the venerable [zlib][zlib]. It has function pointers with these
prototypes on its `z_stream` object.

```c
void *zlib_malloc(void *ctx, unsigned items, unsigned size);
void  zlib_free(void *ctx, void *ptr);
```

The context pointer checks (1), and I can confirm from experience that
it's genuinely useful with a custom allocator. No "realloc" so it passes
(3) automatically. It misses (2), but in practice this hardly matters: It
allocates everything up front, and frees at the very end, meaning a no-op
"free" is quite sufficient.

Finally there's the [Lua programming language][lua] with this economical,
single-function interface:

```c
void *lua_Alloc(void *ctx, void *ptr, size_t old, size_t new);
```

It packs all three allocator functions into one function. It includes a
context pointer (1), a free size (2), and two realloc sizes (3). It's a
simple allocator's best friend!


[arena]: /blog/2023/09/27/
[c++]: https://www.youtube.com/watch?v=LIb3L4vKZ7U
[closure]: /blog/2017/01/08/
[hn]: https://news.ycombinator.com/item?id=38675379
[libc]: /blog/2023/02/11/
[lua]: https://www.lua.org/manual/5.4/manual.html#lua_Alloc
[mini]: /blog/2018/06/10/
[reddit]: https://old.reddit.com/r/C_Programming/comments/18ks5qg/
[sign]: https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2019/p1428r0.pdf
[sized]: https://isocpp.org/files/papers/n3778.html
[slice]: /blog/2023/10/05/#addendum-extend-the-last-allocation
[uthash]: https://troydhanson.github.io/uthash/userguide.html#_hooks
[zlib]: https://www.zlib.net/manual.html
