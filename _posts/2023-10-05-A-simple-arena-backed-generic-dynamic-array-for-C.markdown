---
title: A simple, arena-backed, generic dynamic array for C
layout: post
date: 2023-10-05T23:05:57Z
tags: [c]
uuid: 0c5f55d1-ca7c-4897-97ef-8a539e03bf34
---

Previously I presented an [arena-friendly hash map][map] applicable to any
programming language where one might use arena allocation. In this third
article I present a generic, arena-backed dynamic array. The details are
specific to C, as the most appropriate mechanism depends on the language
(e.g. templates, generics). Just as in the previous two articles, the goal
is to demonstrate an idea so simple that a full implementation fits on one
terminal pager screen — a *concept* rather than a *library*.

Unlike a hash map or linked list, a dynamic array — a data buffer with a
size that varies during run time — is more difficult to square with arena
allocation. They're contiguous by definition, and we cannot resize objects
in the middle of an arena, i.e. `realloc`. So while convenient, they come
with trade-offs. At least until they stop growing, dynamic arrays are more
appropriate for shorter-lived, temporary contexts, where you would use a
scratch arena. On average they consume about twice the memory of a fixed
array of the same size.

As before, I begin with a motivating example of its use. The guts of the
generic dynamic array implementation are tucked away in a `push()` macro,
which is essentially the entire interface.

```c
typedef struct {
    int32_t  *data;
    ptrdiff_t len;
    ptrdiff_t cap;
} int32s;

int32s fibonacci(int32_t max, arena *perm)
{
    static int32_t init[] = {0, 1};
    int32s fib = {0};
    fib.data = init;
    fib.len = fib.cap = countof(init);

    for (;;) {
        int32_t a = fib.data[fib.len-2];
        int32_t b = fib.data[fib.len-1];
        if (a+b > max) {
            return fib;
        }
        *push(&fib, perm) = a + b;
    }
}
```

Anyone familiar with Go will quickly notice a pattern: `int32s` looks an
awful lot like a [Go *slice*][slice]. That was indeed my inspiration, and
there is enough context that you could infer similar semantics. I will
even call these "slice headers." Initially I had tried a design based on
[stretchy buffers][stb], but I didn't like the macros nor the ergonomics.

I wouldn't write a `fibonacci` this way in practice, but it's useful for
highlighting certain features. Of particular note:

* The dynamic array initially wraps a static array, yet I can append to it
  as though it were a dynamic allocation. If I don't append at all, it
  still works. (Though of course the caller then shouldn't modify the
  elements.)

* `push()` operates on any object which is *slice-shaped*. That is it has
  a pointer field named `data`, a `ptrdiff_t` length field named `len`, a
  `ptrdiff_t` capacity field named `cap`, and all in that order.

* `push()` evaluates to a pointer to the newly-pushed element. In my
  example I immediately dereference and assign a value.

* An element is zero-initialized the first time it's pushed. I say "first
  time" because you can truncate an array by reducing `len`, and "pushing"
  afterward will simply reveal the original elements.

* The name `int32s` is intended to evoke plurality. I'll use this
  convention again in a moment.

* The arena passed to `push()` is only used if the array needs to grow.
  The new backing array will be allocated out of this arena regardless of
  the original backing array.

* Resizes always change the backing array address, and the old array
  remains valid. This is also just like slices in Go.

* Despite the name `perm`, I expect it points to the caller's scratch
  arena. It's "permanent" only relative to the `fibonacci` call. Otherwise
  I might build the array in a scratch arena, then create a final copy in
  a permanent arena.

For a slightly more realistic example: rendering triangles. Suppose we
need data in array format for OpenGL, but we don't know the number of
vertices ahead of time. A dynamic array is convenient, especially if we
discard the array as soon as OpenGL is done with it. We could build up
entire scenes like this for each display frame.

```c
typedef struct {
     GLfloat x, y, z;
} GLvert;

typedef struct {
    GLvert   *data;
    ptrdiff_t len;
    ptrdiff_t cap;
} GLverts;

void renderobj(char *buf, ptrdiff_t len, arena scratch)
{
    GLverts vs = {0};
    objparser parser = newobjparser(buf, len);
    for (...) {
        *push(&vs, &scratch) = nextvert(&parser);
    }
    glVertexPointer(3, GL_FLOAT, 0, vs.data);
    glDrawArrays(GL_TRIANGLES, 0, vs.len);
}
```

As before, `GLverts` is slice-shaped. This time it's zero-initialized,
which is a valid empty dynamic array. As with maps, that means any object
with such a field comes with a ready-to-use empty dynamic array. Putting
it together, here's an example that gradually appends vertices to named
dynamic arrays, randomly accessed by string name:

```c
typedef struct {
    map    *child[4];
    str     name;
    GLverts verts;
} map;

verts *upsert(map **, str, arena *);  // from the last article

map *example(..., arena *perm)
{
    map *m = 0;
    for (...) {
        str name = ...;
        vert v = ...;
        verts *vs = upsert(&m, name, perm);
        *push(vs, perm) = v;
    }
    return m;
}
```

That's what Go would call `map[str][]vert`, but allocated entirely out of
an arena. Ever thought C could do this so simply and conveniently? The
memory allocator (~15 lines), map (~30 lines), dynamic array (~30 lines),
constructors (0 lines), and destructors (0 lines) that power this total to
~75 lines of zero-dependency code!

### Implementation details

I despise macro abuse, and programs substantially implemented in macros
are annoying. They're difficult to understand and debug. A good dynamic
array implementation will require a macro, and one of my goals was to keep
it as simple and minimal as possible. The macro's job is to:

1. Check the capacity and maybe grow the array via function call.
2. Smuggle type information (i.e. `sizeof`) to that function.
3. Compute a pointer of the proper type to the new element.

Here's what I came up with:

```c
#define push(s, arena) \
    ((s)->len >= (s)->cap \
        ? grow(s, sizeof(*(s)->data), arena), \
          (s)->data + (s)->len++ \
        : (s)->data + (s)->len++)
```

The macro will be used as an expression, so it cannot use statements like
`if`. The condition is therefore a ternary operator. If it's full, it
calls the supporting `grow` function. In either case, it computes the
result from `data`. In particular, note that the `grow` branch uses a
comma operator to *sequence* growth before pointer derivation, as `grow`
will change the value of `data` as a side effect.

To be generic, the `grow` function uses `memcpy`-based type punning:

```c
static void grow(void *slice, ptrdiff_t size, arena *a)
{
    struct {
        void     *data;
        ptrdiff_t len;
        ptrdiff_t cap;
    } replica;
    memcpy(&replica, slice, sizeof(replica));

    replica.cap = replica.cap ? replica.cap : 1;
    ptrdiff_t align = 16;
    void *data = alloc(a, 2*size, align, replica.cap);
    replica.cap *= 2;
    if (replica.len) {
        memcpy(data, replica.data, size*replica.len);
    }
    replica.data = data;

    memcpy(slice, &replica, sizeof(replica));
}
```

The slice header is copied over a local replica, avoiding conflicts with
strict aliasing. It still requires that different pointers have identical
memory representation. That's virtually always true, and certainly true
anywhere I'd use an arena.

If the capacity was zero, it behaves as though it was one, and so, through
doubling, zero-capacity arrays become capacity-2 arrays on the first push.
It's better to let `alloc` — whose definition, you may recall, included an
overflow check — handle size overflow so that it can invoke the out of
memory policy, so instead of doubling `cap`, which would first require an
overflow check, it doubles the *object size*. This is a small constant
(i.e. from `sizeof`), so doubling it is always safe.

Copying over old data includes a special check for zero-length inputs,
because, [quite frustratingly][mem], `memcpy` does not accept null even
when the length is zero. I check for zero length instead of null so that
it's more sensitive to defects. If the pointer is null with a non-zero
length, it will trip Undefined Behavior Sanitizer, or at least crash the
program, rather than silently skip copying.

Finally the updated replica is copied over the original slice header,
updating it with the new `data` pointer and capacity. The original backing
array is untouched but is no longer referenced through this slice header.
Old slice headers will continue to function with the old backing array,
such as when the arena is reset to a point where the dynamic array was
smaller.

```c
    int32s vals = {0};
    *push(&vals, &scratch) = 1;  // resize: cap=2
    *push(&vals, &scratch) = 2;
    *push(&vals, &scratch) = 3;  // resize: cap=4
    {
        arena tmp = scratch;  // scoped arena
        int32s extended = vals;
        *push(&extended, &tmp) = 4;
        *push(&extended, &tmp) = 5;  // resize: cap=8
        example(extended);
    }
    // vals still works, cap=4, extension freed
```

In practice, a dynamic array comes from old backing arrays whose total
size adds up just shy of the current array capacity. For example, if the
current capacity is 16, old arrays are size 2+4+8 = 14.

If you're worried about misuse, such as slice header fields being in the
wrong order, a couple of assertions can quickly catch such mistakes at run
time, typically under the lightest of testing. In fact, I planned for this
by using the more-sensitive `len>=cap` instead of just `len==cap`, so that
it would direct execution towards assertions in `grow`:

```c
    assert(replica.len >= 0);
    assert(replica.cap >= 0);
    assert(replica.len <= replica.cap);
```

This also demonstrates another benefit of signed sizes: Exactly half the
range is invalid and so defects tend to quickly trip these assertions.

### Alignment

Alignment is unfortunately fixed, and I picked a "safe" value of 16. In my
`new` macro I used `_Alignof` to pass type information to `alloc`. [Due to
an oversight][alignof], unlike `sizeof`, `_Alignof` cannot be applied to
expressions, and so it cannot be used in dynamic arrays. GCC and Clang
support `_Alignof` on expressions just like `sizeof`, as it's such an
obvious idea, but Microsoft chose to strictly follow the oversight in the
standard. To support MSVC, I've deliberately limited the capabilities of
`push`. If that doesn't matter, fixing it is easy:

```diff
--- a/example.c
+++ b/example.c
@@ -2,3 +2,3 @@
     ((s)->len >= (s)->cap \
-        ? grow(s, sizeof(*(s)->data), arena), \
+        ? grow(s, sizeof(*(s)->data), _Alignof(*(s)->data), arena), \
           (s)->data + (s)->len++ \
@@ -6,3 +6,3 @@
 
-static void grow(void *slice, ptrdiff_t size, arena *a)
+static void grow(void *slice, ptrdiff_t size, ptrdiff_t align, arena *a)
 {
@@ -16,3 +16,2 @@
     replica.cap = replica.cap ? replica.cap : 1;
-    ptrdiff_t align = 16;
     void *data = alloc(a, 2*size, align, replica.cap);
```

Though while you're at it, if you're already using extensions you might
want to switch `push` to a [statement expression][stmt] so that the slice
header `s` does not get evaluated more than once — i.e. so that `upsert()`
in my example above could be used inside the `push()` expession.

So far this approach to dynamic arrays has been useful on a number of
occasions, and I'm quite happy with the results. As with arena-friendly
hash maps, I've no doubt they'll become a staple in my C programs.


[alignof]: https://groups.google.com/g/comp.std.c/c/v5hsWOu5vSw
[map]: /blog/2023/09/30/
[mem]: /blog/2023/02/11/
[slice]: https://go.dev/blog/slices-intro
[stb]: https://github.com/nothings/stb/blob/master/deprecated/stretchy_buffer.txt
[stmt]: https://gcc.gnu.org/onlinedocs/gcc/Statement-Exprs.html
