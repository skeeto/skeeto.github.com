---
title: C Closures as a Library
layout: post
date: 2017-01-08T22:45:38Z
tags: [c, linux, x86]
uuid: a5f897bc-0510-3164-a949-fcb848d9279b
---

A common idiom is C is the callback function pointer, either to
deliver information (i.e. a *visitor* or *handler*) or to customize
the function's behavior (e.g. a comparator). Examples of the latter in
the C standard library are `qsort()` and `bsearch()`, each requiring a
comparator function in order to operate on arbitrary types.

~~~c
void qsort(void *base, size_t nmemb, size_t size,
           int (*compar)(const void *, const void *));

void *bsearch(const void *key, const void *base,
              size_t nmemb, size_t size,
              int (*compar)(const void *, const void *));
~~~

A problem with these functions is that there's no way to pass context
to the callback. The callback may need information beyond the two
element pointers when making its decision, or to [update a
result][res]. For example, suppose I have a structure representing a
two-dimensional coordinate, and a coordinate distance function.

~~~c
struct coord {
    float x;
    float y;
};

static inline float
distance(const struct coord *a, const struct coord *b)
{
    float dx = a->x - b->x;
    float dy = a->y - b->y;
    return sqrtf(dx * dx + dy * dy);
}
~~~

If I have an array of coordinates and I want to sort them based on
their distance from some target, the comparator needs to know the
target. However, the `qsort()` interface has no way to directly pass
this information. Instead it has to be passed by another means, such
as a global variable.

~~~c
struct coord *target;

int
coord_cmp(const void *a, const void *b)
{
    float dist_a = distance(a, target);
    float dist_b = distance(b, target);
    if (dist_a < dist_b)
        return -1;
    else if (dist_a > dist_b)
        return 1;
    else
        return 0;
}
~~~

And its usage:

~~~c
    size_t ncoords = /* ... */;
    struct coords *coords = /* ... */;
    struct current_target = { /* ... */ };
    // ...
    target = &current_target
    qsort(coords, ncoords, sizeof(coords[0]), coord_cmp);
~~~

Potential problems are that it's neither thread-safe nor re-entrant.
Two different threads cannot use this comparator [at the same
time][gs]. Also, on some platforms and configurations, repeatedly
accessing a global variable in a comparator [may have a significant
cost][reloc]. A common workaround for thread safety is to make the
global variable thread-local by allocating it in thread-local storage
(TLS):

~~~c
_Thread_local struct coord *target;       // C11
__thread struct coord *target;            // GCC and Clang
__declspec(thread) struct coord *target;  // Visual Studio
~~~

This makes the comparator thread-safe. However, it's still not
re-entrant (usually unimportant) and accessing thread-local variables
on some platforms is even more expensive — which is the situation for
Pthreads TLS, though not a problem for native x86-64 TLS.

Modern libraries usually provide some sort of "user data" pointer — a
generic pointer that is passed to the callback function as an
additional argument. For example, the GNU C Library has long had
`qsort_r()`: *re-entrant* qsort.

~~~c
void qsort_r(void *base, size_t nmemb, size_t size,
           int (*compar)(const void *, const void *, void *),
           void *arg);
~~~

The new comparator looks like this:

~~~c
int
coord_cmp_r(const void *a, const void *b, void *target)
{
    float dist_a = distance(a, target);
    float dist_b = distance(b, target);
    if (dist_a < dist_b)
        return -1;
    else if (dist_a > dist_b)
        return 1;
    else
        return 0;
}
~~~

And its usage:

~~~c
    void *arg = &current_target;
    qsort_r(coords, ncoords, sizeof(coords[0]), coord_cmp_r, arg);
~~~

User data arguments are thread-safe, re-entrant, performant, and
perfectly portable. They completely and cleanly solve the entire
problem with virtually no drawbacks. If every library did this, there
would be nothing left to discuss and this article would be boring.

### The closure solution

In order to make things more interesting, suppose you're stuck calling
a function in some old library that takes a callback but doesn't
support a user data argument. A global variable is insufficient, and
the thread-local storage solution isn't viable for one reason or
another. What do you do?

The core problem is that a function pointer is just an address, and
it's the same address no matter the context for any particular
callback. On any particular call, the callback has three ways to
distinguish this call from other calls. These align with the three
solutions above:

1. Inspect some global state: the **global variable solution**. The
   caller will change this state for some other calls.
2. Query its unique thread ID: the **thread-local storage solution**.
   Calls on different threads will have different thread IDs.
3. Examine a context argument: the **user pointer solution**.

A wholly different approach is to **use a unique function pointer for
each callback**. The callback could then inspect its own address to
differentiate itself from other callbacks. Imagine defining multiple
instances of `coord_cmp` each getting their context from a different
global variable. Using a unique copy of `coord_cmp` on each thread for
each usage would be both re-entrant and thread-safe, and wouldn't
require TLS.

Taking this idea further, I'd like to **generate these new functions
on demand at run time** akin to a JIT compiler. This can be done as a
library, mostly agnostic to the implementation of the callback. Here's
an example of what its usage will be like:

~~~c
void *closure_create(void *f, int nargs, void *userdata);
void  closure_destroy(void *);
~~~

The callback to be converted into a closure is `f` and the number of
arguments it takes is `nargs`. A new closure is allocated and returned
as a function pointer. This closure takes `nargs - 1` arguments, and
it will call the original callback with the additional argument
`userdata`.

So, for example, this code uses a closure to convert `coord_cmp_r`
into a function suitable for `qsort()`:

~~~c
int (*closure)(const void *, const void *);
closure = closure_create(coord_cmp_r, 3, &current_target);

qsort(coords, ncoords, sizeof(coords[0]), closure);

closure_destroy(closure);
~~~

**Caveat**: This API is *utterly insufficient* for any sort of
portability. The number of arguments isn't nearly enough information
for the library to generate a closure. For practically every
architecture and ABI, it's going to depend on the types of each of
those arguments. On x86-64 with the System V ABI — where I'll be
implementing this — this argument will only count integer/pointer
arguments. To find out what it takes to do this properly, see the
[libjit][libjit] documentation.

### Memory design

This implementation will be for x86-64 Linux, though the high level
details will be the same for any program running in virtual memory. My
closures will span exactly two consecutive pages (typically 8kB),
though it's possible to use exactly one page depending on the desired
trade-offs. The reason I need two pages are because each page will
have different protections.

![](/img/diagram/closure-pages.svg)

Native code — the *thunk* — lives in the upper page. The user data
pointer and callback function pointer lives at the high end of the
lower page. The two pointers could really be anywhere in the lower
page, and they're only at the end for aesthetic reasons. The thunk
code will be identical for all closures of the same number of
arguments.

The upper page will be executable and the lower page will be writable.
This allows new pointers to be set without writing to executable thunk
memory. In the future I expect operating systems to enforce W^X
("write xor execute"), and this code will already be compliant.
Alternatively, the pointers could be "baked in" with the thunk page
and immutable, but since creating closure requires two system calls, I
figure it's better that the pointers be mutable and the closure object
reusable.

The address for the closure itself will be the upper page, being what
other functions will call. The thunk will load the user data pointer
from the lower page as an additional argument, then jump to the
actual callback function also given by the lower page.

### Thunk assembly

The x86-64 thunk assembly for a 2-argument closure calling a
3-argument callback looks like this:

~~~nasm
user:  dq 0
func:  dq 0
;; --- page boundary here ---
thunk2:
        mov  rdx, [rel user]
        jmp  [rel func]
~~~

As a reminder, the integer/pointer argument register order for the
System V ABI calling convention is: `rdi`, `rsi`, `rdx`, `rcx`, `r8`,
`r9`. The third argument is passed through `rdx`, so the user pointer
is loaded into this register. Then it jumps to the callback address
with the original arguments still in place, plus the new argument. The
`user` and `func` values are loaded *RIP-relative* (`rel`) to the
address of the code. The thunk is using the callback address (its own
address) to determine the context.

The assembled machine code for the thunk is just 13 bytes:

~~~c
unsigned char thunk2[16] = {
    // mov  rdx, [rel user]
    0x48, 0x8b, 0x15, 0xe9, 0xff, 0xff, 0xff,
    // jmp  [rel func]
    0xff, 0x25, 0xeb, 0xff, 0xff, 0xff
}
~~~

All `closure_create()` has to do is allocate two pages, copy this
buffer into the upper page, adjust the protections, and return the
address of the thunk. Since `closure_create()` will work for `nargs`
number of arguments, there will actually be 6 slightly different
thunks, one for each of the possible register arguments (`rdi` through
`r9`).

~~~c
static unsigned char thunk[6][13] = {
    {
        0x48, 0x8b, 0x3d, 0xe9, 0xff, 0xff, 0xff,
        0xff, 0x25, 0xeb, 0xff, 0xff, 0xff
    }, {
        0x48, 0x8b, 0x35, 0xe9, 0xff, 0xff, 0xff,
        0xff, 0x25, 0xeb, 0xff, 0xff, 0xff
    }, {
        0x48, 0x8b, 0x15, 0xe9, 0xff, 0xff, 0xff,
        0xff, 0x25, 0xeb, 0xff, 0xff, 0xff
    }, {
        0x48, 0x8b, 0x0d, 0xe9, 0xff, 0xff, 0xff,
        0xff, 0x25, 0xeb, 0xff, 0xff, 0xff
    }, {
        0x4C, 0x8b, 0x05, 0xe9, 0xff, 0xff, 0xff,
        0xff, 0x25, 0xeb, 0xff, 0xff, 0xff
    }, {
        0x4C, 0x8b, 0x0d, 0xe9, 0xff, 0xff, 0xff,
        0xff, 0x25, 0xeb, 0xff, 0xff, 0xff
    },
};
~~~

Given a closure pointer returned from `closure_create()`, here are the
setter functions for setting the closure's two pointers.

~~~c
void
closure_set_data(void *closure, void *data)
{
    void **p = closure;
    p[-2] = data;
}

void
closure_set_function(void *closure, void *f)
{
    void **p = closure;
    p[-1] = f;
}
~~~

In `closure_create()`, allocation is done with an anonymous `mmap()`,
just like in [my JIT compiler][jit]. It's initially mapped writable in
order to copy the thunk, then the thunk page is set to executable.

~~~c
void *
closure_create(void *f, int nargs, void *userdata)
{
    long page_size = sysconf(_SC_PAGESIZE);
    int prot = PROT_READ | PROT_WRITE;
    int flags = MAP_ANONYMOUS | MAP_PRIVATE;
    char *p = mmap(0, page_size * 2, prot, flags, -1, 0);
    if (p == MAP_FAILED)
        return 0;

    void *closure = p + page_size;
    memcpy(closure, thunk[nargs - 1], sizeof(thunk[0]));
    mprotect(closure, page_size, PROT_READ | PROT_EXEC);

    closure_set_function(closure, f);
    closure_set_data(closure, userdata);
    return closure;
}
~~~

Destroying a closure is done by computing the lower page address and
calling `munmap()` on it:

~~~c
void
closure_destroy(void *closure)
{
    long page_size = sysconf(_SC_PAGESIZE);
    munmap((char *)closure - page_size, page_size * 2);
}
~~~

And that's it! You can see the entire demo here:

* [closure-demo.c][demo]{: .download}

It's a lot simpler for x86-64 than it is for x86, where there's no
RIP-relative addressing and arguments are passed on the stack. The
arguments must all be copied back onto the stack, above the new
argument, and it cannot be a tail call since the stack has to be fixed
before returning. Here's what the thunk looks like for a 2-argument
closure:

~~~nasm
data:	dd 0
func:	dd 0
;; --- page boundary here ---
thunk2:
        call .rip2eax
.rip2eax:
        pop eax
        push dword [eax - 13]
        push dword [esp + 12]
        push dword [esp + 12]
        call [eax - 9]
        add esp, 12
        ret
~~~

Exercise for the reader: Port the closure demo to a different
architecture or to the the Windows x64 ABI.


[reloc]: /blog/2016/12/23/
[jit]: /blog/2015/03/19/
[libjit]: https://www.gnu.org/software/libjit/
[demo]: /download/closure-demo.c
[res]: /blog/2016/09/05/
[gs]: /blog/2014/10/12/
