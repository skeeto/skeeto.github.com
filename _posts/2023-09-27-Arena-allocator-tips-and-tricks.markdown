---
title: Arena allocator tips and tricks
layout: post
date: 2023-09-27T03:58:59Z
tags: [c]
uuid: 46b2ee54-9169-4070-ad5d-aa0e2700a65e
excerpt_separator: <!--more-->
---

*This article was discussed [on Hacker News][hn].*

Over the past year I've refined my approach to [arena allocation][arena].
With practice, it's effective, simple, and fast; typically as easy to use
as garbage collection but without the costs. Depending on need, an
allocator can weigh just 7–25 lines of code — perfect when [lacking a
runtime][crt]. With the core details of my own technique settled, now is a
good time to document and share lessons learned. This is certainly not the
only way to approach arena allocation, but these are practices I've worked
out to simplify programs and reduce mistakes.

<!--more-->

An arena is a memory buffer and an offset into that buffer, initially
zero. To allocate an object, grab a pointer at the offset, advance the
offset by the size of the object, and return the pointer. There's a little
more to it, such as ensuring alignment and availability. We'll get to
that. Objects are not freed individually. Instead, groups of allocations
are freed at once by restoring the offset to an earlier value. Without
individual lifetimes, you don't need to write destructors, nor do your
programs need to walk data structures at run time to take them apart. You
also no longer need to worry about memory leaks.

A minority of programs inherently require general purpose allocation, at
least in part, that linear allocation cannot fulfill. This includes, for
example, most programming language runtimes. If you like arenas, avoid
accidentally create such a situation through an over-flexible API that
allows callers to assume you have general purpose allocation underneath.

To get warmed up, here's my style of arena allocation in action that shows
off multiple features:

```c
typedef struct {
    uint8_t  *data;
    ptrdiff_t len;
} str;

typedef struct {
    strlist *next;
    str      item;
} strlist;

typedef struct {
    str head;
    str tail;
} strpair;

// Defined elsewhere
void    towidechar(wchar_t *, ptrdiff_t, str);
str     loadfile(wchar_t *, arena *);
strpair cut(str, uint8_t);

strlist *getlines(str path, arena *perm, arena scratch)
{
    int max_path = 1<<15;
    wchar_t *wpath = new(&scratch, wchar_t, max_path);
    towidechar(wpath, max_path, path);

    strpair pair = {0};
    pair.tail = loadfile(wpath, perm);

    strlist *head = 0;
    strlist **tail = &head;
    while (pair.tail.len) {
        pair = cut(pair.tail, '\n');
        *tail = new(perm, strlist, 1);
        (*tail)->item = pair.head;
        tail = &(*tail)->next;
    }
    return head;
}
```

Take note of these details, each to be later discussed in detail:

* `getlines` takes two arenas, "permanent" and "scratch". The former is
  for objects that will be returned to the caller. The latter is for
  temporary objects whose lifetime ends when the function returns. They
  have stack lifetimes just like local variables.

* Objects are not explicitly freed. Instead, **all allocations from a
  scratch arena are implicitly freed upon return**. This would include
  error return paths automatically.

* The **scratch arena is passed by copy** — i.e. a copy of the "header"
  not the *memory region* itself. Allocating only changes the local copy,
  and so cannot survive the return. The semantics are obvious to callers,
  so they're less likely to get mixed up.

* While `wpath` could be an automatic local variable, it's relatively
  large for the stack, so it's allocated out of the scratch arena. A
  scratch arena safely permits large, dynamic allocations that would never
  be safe on the stack. In other words, **a sane [`alloca`][alloca]!**
  Same for variable-length arrays (VLAs). A scratch arena means you'll
  never be tempted to use either of these terrible ideas.

* The second parameter to `new` is a type, so it's obviously a macro. As
  you will see momentarily, this is not some complex macro magic, just a
  convenience one-liner. There is no implicit cast, and you will get a
  compiler diagnostic if the type is incorrect.

* Despite all the allocation, there is not a single `sizeof` operator nor
  size computation. That's because **size computations are a major source
  of defects.** That job is handled by specialized code.

* **Allocation failures are not communicated by a null return**. Lifting
  this burden greatly simplifies programs. Instead such errors are handled
  non-locally by the arena.

* All allocations are **zero-initialized by default**. This makes for
  simpler, less error-prone programs. When that's too expensive, this can
  become an opt-out without changing the default.

See also [u-config][].

### An arena implementation

An arena suitable for most cases can be this simple:

```c
typedef struct {
    char *beg;
    char *end;
} arena;

void *alloc(arena *a, ptrdiff_t size, ptrdiff_t align, ptrdiff_t count)
{
    ptrdiff_t padding = -(uintptr_t)a->beg & (align - 1);
    ptrdiff_t available = a->end - a->beg - padding;
    if (available < 0 || count > available/size) {
        abort();  // one possible out-of-memory policy
    }
    void *p = a->beg + padding;
    a->beg += padding + count*size;
    return memset(p, 0, count*size);
}
```

Yup, just a pair of pointers! When allocating, all sizes are signed [just
as they ought to be][signed]. Unsigned sizes are another historically
common source of defects, and offer no practical advantages in return.

The `align` parameter allows the arena to handle any unusual alignments,
something that's surprisingly difficult to do with libc. It's difficult to
appreciate its usefulness until it's convenient.

The `uintptr_t` business may look unusual if you've never come across it
before. To align `beg`, we need to compute the number of bytes to advance
the address (`padding`) until the alignment evenly divides the address.
The modulo with `align` computes the number of bytes it's since the last
alignment:

    extra = addr % align

We can't operate numerically on an address like this, so in the code we
first convert to `uintptr_t`. Alignment is always a power of two, which
notably excludes zero, so no worrying about division by zero. That also
means we can compute modulo by subtracting one and masking with AND:

    extra = addr & (align - 1)

However, we want the number of bytes to advance to the next alignment,
which is the inverse:

    padding = -addr & (align - 1)

Add the `uintptr_t` cast and you have the code in `alloc`.

The `if` tests if there's enough memory and simultaneously for overflow on
`size*count`. If either fails, it invokes the out-of-memory policy, which
in this case is `abort`. I strongly recommend that, at least when testing,
always having *something* in place to, at minimum, abort when allocation
fails, even when you think it cannot happen. It's easy to use more memory
than you anticipate, and you want a reliable signal when it happens.

An alternative policy is to [longjmp to a "handler"][setjmp], which with
GCC and Clang doesn't even require runtime support. In that case add a
`jmp_buf` to the arena:

```c
typedef struct {
    char  *beg;
    char  *end;
    void **jmp_buf;
} arena;

void *alloc(...)
{
    // ...
    if (/* out of memory */) {
        __builtin_longjmp(a->jmp_buf, 1);
    }
    // ...
}

bool example(..., arena scratch)
{
    void *jmp_buf[5];
    if (__builtin_setjmp(jmp_buf)) {
        return 0;
    }
    scratch.jmp_buf = jmp_buf;
    // ...
    return 1;
}
```

`example` returns failure to the caller if it runs out of memory, without
needing to check individual allocations and, thanks to the implicit free
of scratch arenas, without needing to clean up. If callees receiving the
scratch arena don't set their own `jmp_buf`, they'll return here, too. In
a real program you'd probably wrap the `setjmp` setup in a macro.

Suppose zeroing is too expensive or unnecessary in some cases. Add a flag
to opt out:

```c
void *alloc(..., int flags)
{
    // ...
    return flag&NOZERO ? p : memset(p, 0, total);
}
```

Similarly, perhaps there's a critical moment where you're holding a
non-memory resource (lock, file handle), or you don't want allocation
failure to be fatal. In either case, it's important that the out-of-memory
policy isn't invoked. You could request a "soft" failure with another
flag, and then do the usual null pointer check:

```c
void *alloc(..., int flags)
{
    // ...
    if (/* out of memory */) {
        if (flags & SOFTFAIL) {
            return 0;
        }
        abort();
    }
    // ...
}
```

Most non-trivial programs will probably have at least one of these flags.

In case it wasn't obvious, allocating an arena is simple:

```c
arena newarena(ptrdiff_t cap)
{
    arena a = {0};
    a.beg = malloc(cap);
    a.end = a.beg ? a.beg+cap : 0;
    return a;
}
```

Or make a direct allocation from the operating system, e.g. `mmap`,
`VirtualAlloc`. Typically arena lifetime is the whole program, so you
don't need to worry about freeing it. (Since you're using arenas, you can
also turn off any memory leak checkers while you're at it.)

If you need more arenas then you can always allocate smaller ones out of
the first! In multi-threaded applications, each thread may have at least
its own scratch arena.

### The `new` macro

I've shown `alloc`, but few parts of the program should be calling it
directly. Instead they have a macro to automatically handle the details. I
call mine `new`, though of course if you're writing C++ you'll need to
pick another name (`make`? `PushStruct`?):

```c
#define new(a, t, n)  (t *)alloc(a, sizeof(t), _Alignof(t), n)
```

The cast is an extra compile-time check, especially useful for avoiding
mistakes in levels of indirection. It also keeps normal code from directly
using the `sizeof` operator, which is easy to misuse. If you added a
`flags` parameter, pass in zero for this common case. Keep in mind that
the goal of this macro is to make common allocation simple and robust.

Often you'll allocate single objects, and so the count is 1. If you think
that's ugly, you could make variadic version of `new` that fills in common
defaults. In fact, that's partly why I put `count` last!

```c
#define new(...)            newx(__VA_ARGS__,new4,new3,new2)(__VA_ARGS__)
#define newx(a,b,c,d,e,...) e
#define new2(a, t)          (t *)alloc(a, sizeof(t), alignof(t), 1, 0)
#define new3(a, t, n)       (t *)alloc(a, sizeof(t), alignof(t), n, 0)
#define new4(a, t, n, f)    (t *)alloc(a, sizeof(t), alignof(t), n, f)
```

Not quite so simple, but it optionally makes for more streamlined code:

```c
thing *t   = new(perm, thing);
thing *ts  = new(perm, thing, 1000);
char  *buf = new(perm, char, len, NOZERO);
```

Side note: If `sizeof` should be avoided, what about array lengths? That's
part of the problem! Hardly ever do you want the *size* of an array, but
rather the *number of elements*. That includes `char` arrays where this
happens to be the same number. So instead, define a `countof` macro that
uses `sizeof` to compute the value you actually want. I like to have this
whole collection:

```c
#define sizeof(x)    (ptrdiff_t)sizeof(x)
#define countof(a)   (sizeof(a) / sizeof(*(a)))
#define lengthof(s)  (countof(s) - 1)
```

Yes, you can convert `sizeof` into a macro like this! It won't expand
recursively and bottoms out as an operator. `countof` also, of course,
produces a less error-prone signed count so users don't fumble around with
`size_t`. `lengthof` statically produces null-terminated string length.

```c
char msg[] = "hello world";
write(fd, msg, lengthof(msg));

#define MSG "hello world"
write(fd, MSG, lengthof(MSG));
```

### Enhance `alloc` with attributes

At least for GCC and Clang, we can further improve `alloc` with three
function attributes:

```c
__attribute((malloc, alloc_size(2, 4), alloc_align(3)))
void *alloc(...);
```

`malloc` indicates that the pointer returned by `alloc` does not alias any
existing object. Enables some significant optimizations that are otherwise
blocked, most often by breaking potential loop-carried dependencies.

`alloc_size` tracks the allocation size for compile-time diagnostics and
run-time assertions ([`__builtin_object_size`][objsize]). This generally
requires a non-zero optimization level. In other words, you will get a
compiler warnings about some out bounds accesses of arena objects, and
with Undefined Behavior Sanitizer you'll get run-time bounds checking.
It's a great [complement to fuzzing][fuzz].

**Update June 2024**: I've learned that [`alloc_size` is fundamentally
broken][broken] since its [introduction in GCC 4.3.0 (March 2008)][rel].
Correct use is impossible, and existing instances all rely on luck. In
certain cases, such as function inlining, the pointer information is lost,
and GCC may generate invalid code based on stale data.

In theory `alloc_align` may also allow better code generation, but I've
yet to observe a case. Consider it optional and low-priority. I mention it
only for completeness.

### Arena size and growth

How large an arena should you allocate? The simple answer: As much as is
necessary for the program to successfully complete. Usually the cost of
untouched arena memory is low or even zero. Most programs should probably
have an upper limit, at which point they assume something has gone wrong.
Arenas allow this case to be handled gracefully, simplifying recovery and
paving the way for continued operation.

While a sufficient answer for most cases, it's unsatisfying. There's a
common assumption that programs should increase their memory usage as much
as needed and let the operating system respond if it's too much. However,
if you've ever tried this yourself, you probably noticed that mainstream
operating systems don't handle it well. The typical results are system
instability — thrashing, drivers crashing — possibly necessitating a
reboot.

If you insist on this route, on 64-bit hosts you can reserve a gigantic
virtual address space and gradually commit memory as needed. On Linux that
means leaning on overcommit by allocating the largest arena possible at
startup, which will automatically commit through use. [Use `MADV_FREE` to
decommit.][purge]

On Windows, `VirtualAlloc` handles reserve and commit separately. In
addition to the allocation offset, you need a commit offset. Then expand
the committed region ahead of the allocation offset as it grows. If you
ever manually reset the allocation offset, you could decommit as well, or
at least `MEM_RESET`. At some point commit may fail, which should then
trigger the out-of-memory policy, but the system is probably in poor shape
by that point — i.e. use an abort policy to release it all quickly.

### Pointer laundering (filthy hack)

While allocations out of an arena don't require individual error checks,
allocating the arena itself at startup requires error handling. It would
be nice if the arena could be allocated out of `.bss` and punt that job to
the loader. While you *could* make a big, global `char[]` array to back
your arena, it's technically not permitted (strict aliasing). A "clean"
`.bss` region could be obtained with a bit of assembly — [`.comm`][comm]
plus assembly to get the address into C without involving an array. I
wanted a more portable solution, so I came up with this:

```c
arena getarena(void)
{
    static char mem[1<<28];
    arena r = {0};
    r.beg = mem;
    asm ("" : "+r"(r.beg));  // launder the pointer
    r.end = r.beg + countof(mem);
    return r;
}
```

The `asm` accepts a pointer and returns a pointer (`"+r"`). The compiler
cannot "see" that it's actually empty, and so returns the same pointer.
The arena will be backed by `mem`, but by laundering the address through
`asm`, I've disconnected the pointer from its origin. As far the compiler
is concerned, this is some foreign, assembly-provided pointer, not a
pointer into `mem`. It can't optimize away `mem` because it's been given
to a mysterious assembly black box.

While inappropriate for a real project, I think it's a neat trick.

### Arena-friendly container data structures

In my initial example I used a linked list to stores lines. This data
structure is great with arenas. It only takes a few of lines of code to
implement a linked list on top of an arena, and no "destroy" code is
needed. Simple.

What about [arena-backed associative arrays][hashtrie]? Or [arena-backed
dynamic arrays][array]? See these follow-up articles for details!


[alloca]: https://man7.org/linux/man-pages/man3/alloca.3.html
[arena]: https://www.rfleury.com/p/untangling-lifetimes-the-arena-allocator
[array]: /blog/2023/10/05/
[broken]: https://lists.sr.ht/~skeeto/public-inbox/%3Cane2ee7fpnyn3qxslygprmjw2yrvzppxuim25jvf7e6f5jgxbd@p7y6own2j3it%3E
[comm]: https://sourceware.org/binutils/docs/as/Comm.html
[crt]: /blog/2023/02/15/
[fuzz]: /blog/2019/01/25/
[hashtrie]: /blog/2023/09/30/
[hn]: https://news.ycombinator.com/item?id=37670740
[objsize]: https://gcc.gnu.org/onlinedocs/gcc/Object-Size-Checking.html
[purge]: /blog/2019/12/29/
[rel]: https://gcc.gnu.org/gcc-4.3/changes.html
[setjmp]: /blog/2023/02/12/
[signed]: https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2019/p1428r0.pdf
[u-config]: /blog/2023/01/18/
