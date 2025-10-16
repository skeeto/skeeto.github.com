---
title: Speculations on arenas and non-trivial destructors
layout: post
date: 2025-10-16T20:11:22Z
tags: [cpp]
uuid: 102e0e39-0078-4698-b2d2-b9454dfe5545
---

As I [continue to reflect][2] on arenas and lifetimes in C++, I realized
that dealing with destructors is not so onerous. In fact, it does not even
impact [my established arena usage][api]! That is, implicit RAII-style
deallocation at scope termination, which works even in plain old C. With a
small change we can safely place resource-managing objects in arenas, such
as those owning file handles, sockets, threads, etc. (Though the ideal
remains [resource management avoidance][swr] when possible.) We can also
place traditional, memory-managing C++ objects in arenas, too. Their own
allocations won't come from the arena — either because they [lack the
interfaces][re] to do so, or they're simply ineffective at it ([pmr][]) —
but they will reliably clean up after themselves. It's all exception-safe,
too. In this article I'll update my arena allocator with this new feature.
The change requires one additional arena pointer member, a bit of overhead
for objects with non-trivial destructors, and no impact for other objects.

I continue to title this "speculations" because, unlike arenas in C, I
have not (yet?) put these C++ techniques into practice in real software. I
haven't refined them through use. Even ignoring its standard library as I
do here, C++ is an enormously complex programming language — far more so
than C — and I'm less confident that I'm not breaking a rule by accident.
I only want to break rules with intention!

As a reminder here's where we left things off:

```c++
struct Arena {
    char *beg;
    char *end;
};

template<typename T>
T *raw_alloc(Arena *a, ptrdiff_t count = 1)
{
    ptrdiff_t size = sizeof(T);
    ptrdiff_t pad  = -(uintptr_t)a->beg & (alignof(T) - 1);
    if (count >= (a->end - a->beg - pad)/size) {
        throw std::bad_alloc{};  // OOM policy
    }
    void *r = a->beg + pad;
    a->beg += pad + count*size;
    return new(r) T[count]{};
}
```

I used `throw` when out of memory mainly to emphasize that this works, but
you're free to pick whatever is appropriate for your program. Remember,
that's the entire allocator, including implicit deallocation, sufficient
to fulfill the allocation needs for most programs, though they must be
designed for it. Also note that it's now `raw_alloc`, as we'll be writing
a new, enhanced `alloc` that builds upon this one.

Also a reminder on usage, I'll draw on an old example, updated for C++:

```c
wchar_t   *towidechar(Str, Arena *);   // convert to UTF-16
Str        slurpfile(wchar_t *path);   // read an entire file
Slice<Str> split(Str, char, Arena *);  // split on delimiter

Slice<Str> getlines(Str path, Arena *perm, Arena scratch)
{
    // Use scratch for path conversion, auto-free on return
    wchar_t *wpath = towidechar(path, &scratch);

    // Use perm for file contents, which are returned
    Str buf = slurpfile(wpath, perm);

    // Use perm for the slice, pointing into buf
    return split(buf, '\n', perm);
}
```

Changes to `scratch` do not persist after `getlines` returns, so objects
allocated from that arena are automatically freed on return. So far this
doesn't rely on C++ RAII features, just simple value semantics. It works
well because all the objects in question have trivial destructors. But
suppose there's a resource to manage:

```c++
struct TcpSocket {
    int socket = ::socket(AF_INET, SOCK_STREAM, 0);
    TcpSocket() = default;
    TcpSocket(TcpSocket &) = delete;
    void operator=(TcpSocket &) = delete;
    // TODO: move ctor/operator
    ~TcpSocket() { if (socket >= 0) close(socket); }
    operator int() { return socket; }
};
```

If we allocate a TcpSocket in an arena, including as a member of another
object, the destructor will never run unless we call it manually. To deal
with this we'll need to keep track of objects requiring destruction, which
we'll do with a linked list of destructors, forming a LIFO stack:

```c++
struct Dtor {
    Dtor     *next;
    void     *objects;
    ptrdiff_t count;
    void     (*dtor)(void *objects, ptrdiff_t count);
};
```

Each Dtor points to a homogeneous array, a count (typically one), and a
pointer to a function that knows how to destroy these objects. The linked
list itself is heterogeneous, with dynamic type. The function pointer is
like a kind of type tag. The `dtor` functions will be generated using a
template function:

```c++
template<class T>
void destroy(void *ptr, ptrdiff_t count)
{
    T *objects = (T *)ptr;
    for (ptrdiff_t i = count-1; i >= 0; i--) {
        objects[i].~T();
    }
}
```

Notice it destroys end-to-beginning, in reverse order that these objects
would be instantiated by placement `new[]`. It's essentially a placement
`delete[]`. An arena initializes with an empty list of Dtors as a new
member:

```c++
struct Arena {
    char *beg;
    char *end;
    Dtor *dtors = 0;

    // ...

};
```

There are two different ways to construct an arena: over a block of raw
memory (unowned), or from an existing arena to borrow a scratch arena over
its free space. So that's two constructors:

```c++
struct Arena {
    // ...

    Arena(char *mem, ptrdiff_t len) : beg{mem}, end{mem+len} {}
    Arena(Arena &a) : beg{a.beg}, end{a.end} {}

    // ...
};
```

Finally a destructor that pops the Dtor linked list until empty, which
runs the destructors in reverse order when the arena is destroyed:

```c++
struct Arena {
    // ...

    void operator=(Arena &) = delete;  // rule of three

    ~Arena()
    {
        while (dtors) {
            Dtor *dead = dtors;
            dtors = dead->next;
            dead->dtor(dead->objects, dead->count);
        }
    }
};
```

(Note: This should probably use a local variable instead of manipulating
the `dtors` member directly. Updates to `dtors` are potentially visible to
destructors, inhibiting optimization.) The new, enhanced `alloc` building
upon `raw_alloc`:

```c++
template<typename T>
T *alloc(Arena *a, ptrdiff_t count = 1)
{
    if (__has_trivial_destructor(T) || !count) {
        return raw_alloc<T>(a, count);
    }

    Dtor *dtor    = raw_alloc<Dtor>(a);  // allocate first
    T    *r       = raw_alloc<T>(a, count);
    dtor->next    = a->dtors;
    dtor->objects = r;
    dtor->count   = count;
    dtor->dtor    = destroy<T>;

    a->dtors = dtor;
    return r;
}
```

I'm using the non-standard `__has_trivial_destructor` built-in supported
by all major C++ implementations, meaning we still don't need the C++
standard library, but [`std::is_trivially_destructible`][itd] is the usual
tool here. [LLVM is pushing `__is_trivially_destructible`][llvm] instead,
but it's not supported by GCC [until GCC 16][gcc].

Since it's so simple to do it, if the count is zero then it doesn't care
about non-trivial destruction, as there's nothing to destroy. Things get
more interesting for a non-zero number of non-trivially destructible
objects. First allocate a Dtor, important because failing to allocate it
second would cause a leak (no Dtor entry in place). Then allocate the
array, attach it to the Dtor, attach the Dtor to the arena, registering
the objects for cleanup.

If a constructor throws, placement `new[]` will automatically destroy
objects that have been created so far — i.e. the real placement `delete[]`
— before returning, so that case was already covered at the start.

With a little more cleverness we could omit the `objects` pointer and
discover the array using pointer arithmetic off the Dtor object itself.
That's tricky (consider alignment), and generally unnecessary, so I didn't
worry about it. With arenas, allocator overhead is already well below that
of conventional allocation, so slack is plentiful. Chances are we will
also never need an *array* of non-trivially destructible objects, and so
we could probably omit `count`, then write a single-object allocator that
forwards constructor arguments (e.g. a handles to the resource to be
managed). That involves no new concepts, and I leave it as an exercise for
the reader.

With that in place, we could now allocate an array of TcpSockets:

```c++
void example(Arena scratch)
{
    TcpSocket *sockets = alloc<TcpSocket>(&scratch, 100);
    // ...
}
```

These sockets will all be closed when `example` exits via their singular
Dtor entry on `scratch`. When calling this `example` with an arena:

```c++
void caller(Arena *perm)
{
    example(*perm);  // creates a scratch arena
    // ...
}
```

This invokes the copy constructor, creating a scratch arena with an empty
`dtors` list to be passed into `example`. Objects existing in `*perm` will
not be destroyed by `example` because `dtors` isn't passed in. If we had
passed a *pointer to an arena*, the Arena constructor isn't invoked, so
the callee uses the caller's arena, pushing its Dtors onto the callee's
list.

In other words, the interface hasn't changed! That's the most exciting
part for me. This by-copy, by-pointer interfacing has really grown on me
the past two years.


[2]: /blog/2025/09/30/
[api]: /blog/2025/01/19/
[gcc]: https://gcc.gnu.org/bugzilla/show_bug.cgi?id=107600
[itd]: https://en.cppreference.com/w/cpp/types/is_destructible.html
[llvm]: https://clang.llvm.org/docs/LanguageExtensions.html#:~:text=__has_trivial_destructor
[pmr]: https://en.cppreference.com/w/cpp/memory/polymorphic.html
[re]: /blog/2024/09/04/
[srw]: /blog/2024/10/03/
