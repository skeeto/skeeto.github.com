---
title: Examples of quick hash tables and dynamic arrays in C
layout: post
date: 2025-01-19T04:10:33Z
tags: [c, tutorial]
uuid: d139d0bc-af7b-4e0e-94f2-566312f92290
---

This article durably captures [my reddit comment][r] showing techniques
for `std::unordered_map` and `std::vector` equivalents in C programs. The
core, important features of these data structures require only a dozen or
so lines of code apiece. They compile quickly, and tend to run faster in
debug builds than *release builds* of their C++ equivalents. What they
lack in genericity they compensate in simplicity. Nothing here will be
new. Everything has been covered in greater detail previously, which I
will reference when appropriate.

For a concrete goal, we will build a data structure representing an
process environment, along with related functionality to make it more
interesting. That is, we'll build a string-to-string map.

### Allocator

The foundation is our allocator, a simple [bump allocator][arena], so
we'll start there:

```c
#define new(a, n, t)    (t *)alloc(a, n, sizeof(t), _Alignof(t))

typedef struct {
    char *beg;
    char *end;
} Arena;

void *alloc(Arena *a, ptrdiff_t count, int size, int align)
{
    int pad = -(uintptr_t)a->beg & (align - 1);
    assert(count < (a->end - a->beg)/size);  // TODO: OOM policy
    void *r = a->beg + pad;
    a->beg += pad + count*size;
    return memset(r, 0, count*size);
}
```

Allocating through the `new` macro eliminates several classes of common
defects in C programs. If we get our types mixed up we get errors, or at
least warnings. Our [size calculations cannot overflow][size]. We cannot
accidentally use uninitialized memory. We cannot leak memory; deallocating
is implicit. The main downside is that it doesn't fit some less common
allocator requirements.

### Strings

Next, a string representation. Classic [null-terminated strings are an
error-prone paradigm][cstr], so we'll use [counted strings][str] instead:

```c
#define S(s)    (Str){s, sizeof(s)-1}

typedef struct {
    char     *data;
    ptrdiff_t len;
} Str;
```

This is equivalent to a `std::string_view` in C++. The macro allows us to
efficiently convert string literals into `Str` objects. Because our data
structures are backed by arenas, we won't care whether a particular string
is backed by a static string, arena, memory map, etc. We'll also need a
function to compare strings for equality:

```c
_Bool equals(Str a, Str b)
{
    if (a.len != b.len) {
        return 0;
    }
    return !a.len || !memcmp(a.data, b.data, a.len);
}
```

`!a.len` appears superfluous, but it's necessary: `memcmp` [arbitrarily
forbids null pointers][null], and we may be passed a zero-initialized
`Str`. Though [this is scheduled to be corrected][fix].

We'll need a string hash function, too:

```c
uint64_t hash64(Str s)
{
    uint64_t h = 0x100;
    for (ptrdiff_t i = 0; i < s.len; i++) {
        h ^= s.data[i] & 255;
        h *= 1111111111111111111;
    }
    return h;
}
```

This is an FNV-style hash. The "basis" keeps strings of nulls from getting
stuck at zero, and the multiplier is my favorite prime number. Character
data is fixed to 0–255 rather than allowing the signedness of `char` to
influence the results. As a multiplicative hash, the high bits are mixed
better than the low bits, and our maps will take that into account.

### Flat hash map

We have a couple string-to-string map options. The more restrictive, but
more efficient — in terms of memory use and speed — is a [Mask-Step-Index
(MSI) hash table][msi]. I don't think it fits our problem as well as the
next option, particularly because it puts a hard limit on unique keys, but
it's worth evaluating. Let's call it `FlatEnv`:

```c
enum { ENVEXP = 10 };  // support up to 1,000 unique keys
typedef struct {
    Str keys[1<<ENVEXP];
    Str vals[1<<ENVEXP];
} FlatEnv;
```

It's nothing more than two fixed-length arrays, storing keys and values
separately. Keys with null pointers are empty slots, so a zero-initialized
`FlatEnv` is an empty table. They come out of an arena ready-to-use:

```c
    FlatEnv *env = new(a, 1, FlatEnv);  // new, empty environment
```

Now we leverage `equals` and `hash64` for a double-hashed, open address
search on the keys array:

```c
Str *flatlookup(FlatEnv *env, Str key)
{
    uint64_t hash = hash64(key);
    uint32_t mask = (1<<ENVEXP) - 1;
    uint32_t step = (hash>>(64 - ENVEXP)) | 1;
    for (int32_t i = hash;;) {
        i = (i + step) & mask;
        if (!env->keys[i].data) {
            env->keys[i] = key;
            return env->vals + i;
        } else if (equals(env->keys[i], key)) {
            return env->vals + i;
        }
    }
}
```

By returning a pointer to the unmodified value slot, this function covers
both lookup and insertion. So that's the entire hash table implementation.
To insert, the caller assigns the slot. For mere lookup, check the slot
for a null pointer.

```c
    FlatEnv *env = new(a, 1, FlatEnv);

    // insert
    *flatlookup(env, S("hello")) = S("world");

    // lookup
    Str val = *flatlookup(env, key);
    if (val.data) {
        printf("%.*s = %.*s\n", (int)key.len, key.data,
                                (int)val.len, val.data);
    }
```

To iterate over the map entries, iterate over the arrays, skipping null
entries. Per the `ENVEXP` comment, it's hard-coded to support up to 1,000
unique keys (1,024 slots, leaving some to spare). The table itself doesn't
enforce this limit and will turn into an infinite loop if you insert too
many keys. To support scaling, we could design the map to have dynamic
table sizes, track the number of unique keys, and resize the table
(allocate new arrays) when the load factor crosses a threshold. Resizing
sounds messy and complicated, so fortunately there's another option.

### Hierarchical hash map

If the number of keys is unbounded, [hash tries][trie] work better. Trees
scale well, and we can allocate nodes out of the arena as it grows. We'll
use a 4-ary trie, a good default that balances size and performance:

```c
typedef struct Env Env;
struct Env {
    Env *child[4];
    Str  key;
    Str  value;
};
```

An empty map is just a null pointer, and so, again, these maps come
ready-to-use in their zero state:

```c
    Env *env = 0;  // new, empty environment
```

The implementation is equally as brief:

```c
Str *lookup(Env **env, Str key, Arena *a)
{
    for (uint64_t h = hash64(key); *env; h <<= 2) {
        if (equals(key, (*env)->key)) {
            return &(*env)->value;
        }
        env = &(*env)->child[h>>62];
    }
    if (!a) return 0;
    *env = new(a, 1, Env);
    (*env)->key = key;
    return &(*env)->value;
}
```

Like before, this covers both lookup and insertion, though the mode is
determined explicitly by the arena pointer. Without an arena, it's a
lookup, which doesn't require allocation. With an arena, it creates an
entry if necessary and, like before, returns a pointer into the map so
that the caller can assign it. Usage differs only slightly:

```c
    Env *env = 0;

    // insert
    *lookup(env, S("hello"), &scratch) = S("world");

    // lookup
    Str *val = flatlookup(env, key, 0);
    if (val) {
        printf("%.*s = %.*s\n", (int)key.len, key.data,
                                (int)val->len, val->data);
    }
```

We'll come back around to iteration later.

### String concatenation

Next I'd like a function that takes an `Env` and produces an `envp` data
structure as expected by [`execve(2)`][exec]. Then we can use this map as
the environment in a child process. We'll need some string manipulation,
particularly [string concatenation][concat]. The core is a copy function:

```c
Str copy(Arena *a, Str s)
{
    Str r = s;
    r.data = new(a, s.len, char);
    if (r.len) memcpy(r.data, s.data, r.len);
    return r;
}
```

Like with `memcmp`, because it's `memcpy` we need to handle the arbitrary
special case around null pointers should the input be a zero `Str`. Now we
can easily concatenate strings, *in-place if possible*:

```c
Str concat(Arena *a, Str head, Str tail)
{
    if (!head.data || head.data+head.len != a->beg) {
        head = copy(a, head);
    }
    head.len += copy(a, tail).len;
    return head;
}
```

Yet again, `!head.data` is special check because pointer arithmetic on
null (i.e. adding zero to null) is arbitrarily disallowed. Worrying about
this is exhausting, isn't it? That language fix can't come soon enough.
This one's already fixed in C++.

That's enough to get the ball rolling on `FlatEnv`:

```c
char **flat_to_envp(FlatEnv *env, Arena *a)
{
    int    cap  = 1<<ENVEXP;
    char **envp = new(a, cap, char *);
    int len = 0;
    for (int i = 0; i < cap; i++) {
        if (env->vals[i].data) {
            Str pair = env->keys[i];
            pair = concat(a, pair, S("="));
            pair = concat(a, pair, env->vals[i]);
            pair = concat(a, pair, S("\0"));
            envp[len++] = pair.data;
        }
    }
    return envp;
}
```

Simple, right? Traditional string handling in C is an error-prone pain,
but with a better set of primitives it's a breeze. Plus we're doing this
all with essentially no runtime. In use this might look like:

```c
void shellexec(char *cmd, FlatEnv *env, Arena scratch)
{
    char  *argv[] = {"sh", "-c", cmd, 0};
    char **envp   = flat_to_envp(env, &scratch);
    execve("/bin/sh", argv, envp);
}
```

By virtue of the scratch arena, the `envp` object is automatically freed
should `execve` fail. (If that should even matter.) Considering this, if
you're itching to write the fastest shell ever devised, arena allocation
and the techniques in this article would probably get you most of the way
there. Nobody writes shells this way.

### Dynamic arrays

To implement the `envp` conversion for the hash trie `Env`, let's add one
more tool to our toolbox: dynamic arrays. Our `std::vector` equivalent.
We'll start with [a familiar slice header][slice]:

```c
typedef struct {
    char    **data;
    ptrdiff_t len;
    ptrdiff_t cap;
} EnvpSlice;
```

The bad news is that we don't have templates, and so we'll need to define
one such structure for each type of which we want a dynamic array. This
one is set up to create an `envp` array. The good news that manipulation
occurs through generic code, so everything else is reusable.

I want a `push` macro that creates an empty slot in which to insert a new
value, evaluating to a pointer to this slot. Usually that means
incrementing `len`, but when out of room it will need to expand the
underlying storage. It's clearer to start with example usage. Imagine
using it with the previous `flat_to_envp`:

```c
char **flat_to_envp(FlatEnv *env, Arena *a)
{
    EnvpSlice r = {0};
    for (int i = 0; i < 1<<ENVEXP; i++) {
        if (env->vals[i].data) {
            // ... concat as before ...
            *push(a, &r) = pair.data;
        }
    }
    push(a, &r);  // terminal null pointer
    return r.data;
}
```

Continuing the theme, a zero-initialized slice is a ready-to-use empty
slice, and most begin life this way. The immediate dereference on `push`
is just like those calls to `lookup`. If expansion is needed, the `push`
macro's job is to pull elements off the slice, pass them into a helper
function which agnostically, strict-aliasing-legally, manipulates the
slice header:

```c
void *push_(Arena *, void *data, ptrdiff_t *pcap, ptrdiff_t size);

#define push(a, s) \
  ((s)->len == (s)->cap \
    ? (s)->data = push_((a), (s)->data, &(s)->cap, sizeof(*(s)->data)), \
      (s)->data + (s)->len++ \
    : (s)->data + (s)->len++)
```

The internals of that helper look an awful lot like `concat`, with the
same in-place-if-possible behavior:

```c
enum { SLICE_INITIAL_CAP = 4 };

void *push_(Arena *a, void *data, ptrdiff_t *pcap, ptrdiff_t size)
{
    ptrdiff_t cap   = *pcap;
    ptrdiff_t align = _Alignof(void *);

    if (!data || a->beg != (char *)data + cap*size) {
        void *copy = alloc(a, cap, size, align);
        if (data) memcpy(copy, data, cap*size);
        data = copy;
    }

    ptrdiff_t extend = cap ? cap : SLICE_INITIAL_CAP;
    alloc(a, extend, size, align);
    *pcap = cap + extend;
    return data;
}
```

For unfathomable reasons, standard C does not permit `_Alignof` on
expressions, so slice data is simply pointer-aligned. (The more shrewd
might consider `max_align_t`.) Like concatenation, we copy the object to
the beginning of the arena if necessary, and extend the allocation by
allocating the usual way, being careful not to increment the capacity
until after it succeeds.

We can now use `push` on any structure with `data`, `len`, and `cap`
fields of the appropriate types.

### Putting it all together

With that in place, we can define a simple, recursive version of the
`envp` builder for `Env`:

```c
#define countof(a)  ((ptrdiff_t)(sizeof(a) / sizeof(*(a))))

EnvpSlice env_to_envp_(EnvpSlice r, Env *env, Arena *a)
{
    if (env) {
        Str pair = env->key;
        pair = concat(a, pair, S("="));
        pair = concat(a, pair, env->value);
        pair = concat(a, pair, S("\0"));
        *push(a, &r) = pair.data;
        for (int i = 0; i < countof(env->child); i++) {
            r = env_to_envp_(r, env->child[i], a);
        }
    }
    return r;
}

char **env_to_envp(Env *env, Arena *a)
{
    EnvpSlice r = {0};
    r = env_to_envp_(r, env, a);
    push(a, &r);  // null pointer terminator
    return r.data;
}
```

As is often the case, the recursive part doesn't fit the final interface,
so the core is a helper, and the caller-facing part is an adapter. I'm not
*entirely* comfortable with this function, though. When working with huge
environments — over a ~100k entries — then the recursive implementation
will non-deterministically blow the stack if the trie winds up lopsided.
Or deterministically for chosen pathological inputs, because the hash
function isn't seeded.

Instead we could use a stack data structure backed by the arena to
traverse the trie. If passed a secondary scratch arena, we'd use that
arena for this stack, but I'm sticking to the original interface. Here's
what that looks like, with an extra trick thrown in just to show off:

```c
char **env_to_envp_safe(Env *env, Arena *a)
{
    EnvpSlice r = {0};

    typedef struct {
        Env *env;
        int  index;
    } Frame;
    Frame init[16];  // small size optimization

    struct {
        Frame    *data;
        ptrdiff_t len;
        ptrdiff_t cap;
    } stack = {init, 0, countof(init)};

    *push(a, &stack) = (Frame){env, 0};
    while (stack.len) {
        Frame *top = stack.data + stack.len - 1;

        if (!top->env) {
            stack.len--;

        } else if (top->index == countof(top->env->child)) {
            Str pair = top->env->key;
            pair = concat(a, pair, S("="));
            pair = concat(a, pair, top->env->value);
            pair = concat(a, pair, S("\0"));
            *push(a, &r) = pair.data;
            stack.len--;

        } else {
            int i = top->index++;
            *push(a, &stack) = (Frame){top->env->child[i], 0};
        }
    }

    push(a, &r);
    return r.data;
}
```

The `init` array is a form of [small-size optimization][sso]. It's used at
first, and sufficient for nearly all inputs. So no stack litter in the
arena. If it's not enough, then `push` will *automatically move the stack
into the arena*. I think that's a super duper neato trick!

Alternative to this, and as discussed in the original hash trie article,
we could instead add a `next` field to `Env` as an intrusive linked list
that chains the nodes together in insertion order. Or another way to look
at it, `Env` is a linked list with an *intrusive hash trie* for O(log n)
searches on the list. That's a lot simpler, has other useful properties,
and only costs one extra pointer per entry. And we wouldn't need slices,
which was my motivation for choosing non-linked-list approach above.

### Hash hardening (bonus)

Okay, I lied, this is something new. Think of it as your special treat for
sticking with me so far.

Hash map non-determinism comes with a classic security vulnerability: If
populated with untrusted keys, an attacker could choose colliding keys and
produce worst case behavior in the hash map. That is, MSI hash tables
reduce to linear scans, and hash tries reduce to linked lists. Worse, the
recursive `envp` function blows the stack, though we already solved that
issue.

If we want to foil such attacks, we can seed the hash so that an attacker
cannot devise collisions. They'd need to discover the seed. We might even
call that seed a "key," but this is a non-cryprographic hash so I'm going
to avoid that term. The usual implementation of this concept involves
generating a seed, sometimes per table, and storing it somewhere. However,
we can leverage an existing security mechanism, gaining this feature at
basically no cost: Address Space Layout Randomization (ASLR). First, let's
augment the string hash function:

```c
uint64_t hash64(Str s, uint64_t seed)
{
    uint64_t h = seed;
    for (ptrdiff_t i = 0; i < s.len; i++) {
        h ^= s.data[i] & 255;
        h *= 1111111111111111111;
    }
    return h;
}
```

In `flatlookup` we can use the address of the `FlatEnv` as our seed:

```c
Str *flatlookup(FlatEnv *env, Str key)
{
    uint64_t hash = hash64(key, (uintptr_t)env);
    // ...
}
```

Recall it's allocated out of our arena (via `new`), and ASLR gives our
arena a random offset. On top of that, a `FlatEnv` seed depends precisely
on the amount of memory allocated earlier. An environment variable name or
value being slightly longer or shorter will reshuffle the whole table if
allocated in the arena before the `FlatEnv`.

It's slightly trickier with hash tries. The root pointer isn't required to
be fixed. For example:

```c
    Env *env = 0;
    // ... insert keys ...
    Env *myenv = env;
    // ... lookup keys in myenv ...
```

We could disallow this, but it would be easy to forget (e.g. while you're
refactoring and not thinking about it) and difficult to detect.
Difficult-to-detect bugs keep me awake at night. Instead we can use the
root node to seed the trie:

```c
Str *lookup(Env **env, Str key, Arena *a)
{
    uint64_t seed = env ? (uintptr_t)*env : 0;
    for (uint64_t h = hash64(key, seed); *env; h <<= 2) {
    // ...
}
```

At first this seems like it couldn't work, like a chicken-and-egg problem.
There's no root node at first, so we can't know the seed yet. Though think
about it a little longer and it should be obvious: The hash is unused when
inserting the very first element. It simply becomes the root of the trie.
The seed is irrelevant until the second insert, at which point we've
established a seed. This delay establishing the seed means hash tries are
even more randomized.

With the proper tools and representations, working in C isn't difficult,
even if you need containers and string manipulation. Aside from `memcmp`
and `memcpy` — each easily replaceable — we did all this without runtime
assistance, not even its allocator. What a pleasant way to work!

Source from this article in runnable form, which I used to test my samples:
[`example.c`](https://gist.github.com/skeeto/42d8a23871642696b6b8de30d9222328)


[arena]: /blog/2023/09/27/
[concat]: /blog/2024/05/25/
[cstr]: https://www.symas.com/post/the-sad-state-of-c-strings
[exec]: https://man7.org/linux/man-pages/man2/execve.2.html
[fix]: https://developers.redhat.com/articles/2024/12/11/making-memcpynull-null-0-well-defined
[msi]: /blog/2022/08/08/
[null]: /blog/2023/02/11/#strings
[r]: https://old.reddit.com/r/C_Programming/comments/1hrvhfl/_/m51saq2/
[size]: /blog/2024/05/24/
[slice]: /blog/2023/10/05/
[sso]: /blog/2016/10/07/
[str]: /blog/2024/04/14/
[trie]: /blog/2023/09/30/
