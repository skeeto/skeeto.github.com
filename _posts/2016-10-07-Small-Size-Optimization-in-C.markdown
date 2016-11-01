---
title: Small-Size Optimization in C
layout: post
date: 2016-10-07T01:43:12Z
tags: [c, optimization]
uuid: 1e249621-40bb-39f9-7e47-17fbe37c9fa4
---

I've worked on many programs that frequently require small,
short-lived buffers for use as a temporary workspace, perhaps to
construct a string or array. In C this is often accomplished with
arrays of [automatic storage duration][auto] (i.e. allocated on the
stack). This is dirt cheap — much cheaper than a heap allocation —
and, unlike a typical general-purpose allocator, involves no thread
contention. However, the catch that there may be no hard bound to the
buffer. For correctness, the scratch space must scale appropriately to
match its input. Whatever arbitrary buffer size I pick may be too small.

A widespread extension to C is the alloca() pseudo-function. It's like
malloc(), but allocates memory on the stack, just like an automatic
variable. The allocation is automatically freed when the function (not
its scope!) exits, even with a longjmp() or other non-local exit.

~~~c
void *alloca(size_t size);
~~~

Besides its portability issues, the most dangerous property is the
**complete lack of error detection**. If `size` is too large, the
program simply crashes, *or worse*.

For example, suppose I have an intern() function that finds or creates
the canonical representation/storage for a particular string. My
program needs to intern a string composed of multiple values, and will
construct a temporary string to do so.

~~~c
const char *intern(const char *);

const char *
intern_identifier(const char *prefix, long id)
{
    size_t size = strlen(prefix) + 32;
    char *buffer = alloca(size);
    sprintf(buffer, "%s%ld", prefix, id);
    return intern(buffer);
}
~~~

I expect the vast majority of these `prefix` strings to be very small,
perhaps on the order of 10 to 80 bytes, and this function will handle
them extremely efficiently. But should this function get passed a huge
`prefix`, perhaps by a malicious actor, the program will misbehave
without warning.

A portable alternative to alloca() is variable-length arrays (VLA),
introduced in C99. Arrays with automatic storage duration need not
have a fixed, compile-time size. It's just like alloca(), having
exactly **the same dangers**, but at least it's properly scoped. It
was rejected for inclusion in C++11 due to this danger.

~~~c
const char *
intern_identifier(const char *prefix, long id)
{
    char buffer[strlen(prefix) + 32];
    sprintf(buffer, "%s%ld", prefix, id);
    return intern(buffer);
}
~~~

There's a middle-ground to this, using neither VLAs nor alloca().
Suppose the function always allocates a small, fixed size buffer —
essentially a free operation — but only uses this buffer if it's large
enough for the job. If it's not, a normal heap allocation is made with
malloc().

~~~c
const char *
intern_identifier(const char *prefix, long id)
{
    char temp[256];
    char *buffer = temp;
    size_t size = strlen(prefix) + 32;
    if (size > sizeof(temp))
        if (!(buffer = malloc(size)))
            return NULL;
    sprintf(buffer, "%s%ld", prefix, id);
    const char *result = intern(buffer);
    if (buffer != temp)
        free(buffer);
    return result;
}
~~~

Since the function can now detect allocation errors, this version has
an error condition. Though, intern() itself would presumably return
NULL for its own allocation errors, so this is probably transparent to
the caller.

We've now entered the realm of *small-size optimization*. The vast
majority of cases are small and will therefore be very fast, but we
haven't given up on the odd large case either. In fact, it's been made
a little bit *worse* (via the unnecessary small allocation), selling
it out to make the common case fast. That's sound engineering.

Visual Studio has a pair of functions that *nearly* automate this
solution: \_malloca() and \_freea(). It's like alloca(), but
allocations beyond a certain threshold go on the heap. This allocation
is freed with \_freea(), which does nothing in the case of a stack
allocation.

~~~c
void *_malloca(size_t);
void _freea(void *);
~~~

I said "nearly" because Microsoft screwed it up: instead of returning
NULL on failure, it generates a stack overflow structured exception
(for a heap allocation failure).

I haven't tried it yet, but I bet something similar to malloca() /
freea() could be implemented using a couple of macros.

### Toward Structured Small-Size Optimization

CppCon 2016 was a couple weeks ago, and I've begun catching up on the
talks. I don't like developing in C++, but I always learn new,
interesting concepts from this conference, many of which apply
directly to C. I look forward to Chandler Carruth's talks the most,
having learned so much from his past talks. I recommend these all:

* [Efficiency with Algorithms, Performance with Data Structures][algo] (2014)
* [Tuning C++: Benchmarks, and CPUs, and Compilers! Oh My!][perf] (2015)
* [High Performance Code 201: Hybrid Data Structures][hybrid] (2016)
* [Understanding Compiler Optimization][optim] (2015)
* [Optimizing the Emergent Structures of C++][emergent] (2013)

After writing this article, I saw Nicholas Ormrod's talk, [The strange
details of std::string at Facebook][string], which is also highly
relevant.

Chandler's talk this year was the one on hybrid data structures. I'd
already been mulling over small-size optimization for months, and the
first 5–10 minutes of his talk showed me I was on the right track. In
his talk he describes LLVM's SmallVector class (among others), which
is basically a small-size-optimized version of std::vector, which, due
to constraints on iterators under std::move() semantics, can't itself
be small-size optimized.

I picked up a new trick from this talk, which I'll explain in C's
terms. Suppose I have a dynamically growing buffer "vector" of `long`
values. I can keep pushing values into the buffer, doubling the
storage in size each time it fills. I'll call this one "simple."

~~~c
struct vec_simple {
    size_t size;
    size_t count;
    long *values;
};
~~~

Initialization is obvious. Though for easy overflow checks, and for
another reason I'll explain later, I'm going to require the starting
size, `hint`, to be a power of two. It returns 1 on success and 0 on
error.

~~~c
int
vec_simple_init(struct vec_simple *v, size_t hint)
{
    assert(hint && (hint & (hint - 1)) == 0);  // power of 2
    v->size = hint;
    v->count = 0;
    v->values = malloc(sizeof(v->values[0]) * v->size);
    return !!v->values;
}
~~~

Pushing is straightforward, using realloc() when the buffer fills,
returning 0 for integer overflow or allocation failure.

~~~c
int
vec_simple_push(struct vec_simple *v, long x)
{
    if (v->count == v->size) {
        size_t value_size = sizeof(v->values[0]);
        size_t new_size = v->size * 2;
        if (!new_size || value_size > (size_t)-1 / new_size)
            return 0; // overflow
        void *new_values = realloc(v->values, new_size * value_size);
        if (!new_values)
            return 0; // out of memory
        v->size = new_size;
        v->values = new_values;
    }
    v->values[v->count++] = x;
    return 1;
}
~~~

And finally, cleaning up. I hadn't thought about this before, but if
the compiler manages to inline vec\_simple\_free(), that NULL pointer
assignment will probably get optimized out, possibly [even in the face
of a use-after-free bug][ub].

~~~c
void
vec_simple_free(struct vec_simple *v)
{
    free(v->values);
    v->values = 0;  // trap use-after-free bugs
}
~~~

And finally an example of its use (without checking for errors).

~~~c
long
example(long (*f)(void *), void *arg)
{
    struct vec_simple v;
    vec_simple_init(&v, 16);
    long n;
    while ((n = f(arg)) > 0)
        vec_simple_push(&v, n);
    // ... process vector ...
    vec_simple_free(&v);
    return result;
}
~~~

If the common case is only a handful of `long` values, and this
function is called frequently, we're doing a lot of heap allocation
that could be avoided. Wouldn't it be nice to put all that on the
stack?

### Applying Small-Size Optimization

Modify the struct to add this `temp` field. It's probably obvious what
I'm getting at here. This is essentially the technique in SmallVector.

~~~c
struct vec_small {
    size_t size;
    size_t count;
    long *values;
    long temp[16];
};
~~~

The `values` field is initially pointed at the small buffer. Notice
that unlike the "simple" vector above, this initialization function
cannot fail. It's one less thing for the caller to check. It also
doesn't take a `hint` since the buffer size is fixed.

~~~c
void
vec_small_init(struct vec_small *v)
{
    v->size = sizeof(v->temp) / sizeof(v->temp[0]);
    v->count = 0;
    v->values = v->temp;
}
~~~

Pushing gets a little more complicated. If it's the first time the
buffer has grown, the realloc() has to be done "manually" with
malloc() and memcpy().

~~~c
int
vec_small_push(struct vec_small *v, long x)
{
    if (v->count == v->size) {
        size_t value_size = sizeof(v->values[0]);
        size_t new_size = v->size * 2;
        if (!new_size || value_size > (size_t)-1 / new_size)
            return 0; // overflow

        void  *new_values;
        if (v->temp == v->values) {
            /* First time heap allocation. */
            new_values = malloc(new_size * value_size);
            if (new_values)
                memcpy(new_values, v->temp, sizeof(v->temp));
        } else {
            new_values = realloc(v->values, new_size * value_size);
        }

        if (!new_values)
            return 0; // out of memory
        v->size = new_size;
        v->values = new_values;
    }
    v->values[v->count++] = x;
    return 1;
}
~~~

Finally, only call free() if the buffer was actually allocated on the
heap.

~~~c
void
vec_small_free(struct vec_small *v)
{
    if (v->values != v->temp)
        free(v->values);
    v->values = 0;
}
~~~

If 99% of these vectors never exceed 16 elements, then 99% of the time
the heap isn't touched. That's *much* better than before. The 1% case
is still covered, too, at what is probably an insignificant cost.

An important difference to SmallVector is that they parameterize the
small buffer's size through the template. In C we're stuck with fixed
sizes or macro hacks. Or are we?

### Using a Caller-Provided Buffer

This time remove the temporary buffer, making it look like the simple
vector from before.

~~~c
struct vec_flex {
    size_t size;
    size_t count;
    long *values;
};
~~~

The user will provide the initial buffer, which will presumably be an
adjacent, stack-allocated array, but whose size is under the user's
control.

~~~c
void
vec_flex_init(struct vec_flex *v, long *init, size_t nmemb)
{
    assert(nmemb > 1); // we need that low bit!
    assert(nmemb && (nmemb & (nmemb - 1)) == 0); // power of 2
    v->size = nmemb | 1;
    v->count = 0;
    v->values = init;
}
~~~

The power of two size, greater than one, means the size will always be
an even number. Why is this important? There's one piece of
information missing from the struct: Is the buffer currently heap
allocated or not? That's just one bit of information, but adding just
one more bit to the struct will typically pad it out another 31 or 63
more bits. What a waste! Since I'm not using the lowest bit of the
size (always being an even number), I can smuggle it in there. Hence
the `nmemb | 1`, the 1 indicating that it's not heap allocated.

When pushing, the `actual_size` is extracted by clearing the bottom
bit (`size & ~1`) and the indicator bit is extracted with a 1 bit mask
(`size & 1`). The bit is cleared by virtue of not intentionally
setting it again.

~~~c
int
vec_flex_push(struct vec_flex *v, long x)
{
    size_t actual_size = v->size & ~(size_t)1; // clear bottom bit
    if (v->count == actual_size) {
        size_t value_size = sizeof(v->values[0]);
        size_t new_size = actual_size * 2;
        if (!new_size || value_size > (size_t)-1 / new_size)
            return 0; /* overflow */

        void *new_values;
        if (v->size & 1) {
            /* First time heap allocation. */
            new_values = malloc(new_size * value_size);
            if (new_values)
                memcpy(new_values, v->values, actual_size * value_size);
        } else {
            new_values = realloc(v->values, new_size * value_size);
        }

        if (!new_values)
            return 0; /* out of memory */
        v->size = new_size;
        v->values = new_values;
    }
    v->values[v->count++] = x;
    return 1;
}
~~~

Only free() when it's been allocated, like before.

~~~c
void
vec_flex_free(struct vec_flex *v)
{
    if (!(v->size & 1))
        free(v->values);
    v->values = 0;
}
~~~

And here's what it looks like in action.

~~~c
long
example(long (*f)(void *), void *arg)
{
    struct vec_flex v;
    long buffer[16];
    vec_flex_init(&v, buffer, sizeof(buffer) / sizeof(buffer[0]));
    long n;
    while ((n = f(arg)) > 0)
        vec_flex_push(&v, n);
    // ... process vector ...
    vec_flex_free(&v);
    return result;
}
~~~

If you were to log all vector sizes as part of profiling, and the
assumption about their typical small number of elements was correct,
you could easily tune the array size in each case to remove the vast
majority of vector heap allocations.

Now that I've learned this optimization trick, I'll be looking out for
good places to apply it. It's also a good reason for me to stop
abusing VLAs.


[auto]: /blog/2016/10/02/
[malloca]: https://msdn.microsoft.com/en-us/library/5471dc8s.aspx
[algo]: https://www.youtube.com/watch?v=fHNmRkzxHWs
[perf]: https://www.youtube.com/watch?v=nXaxk27zwlk
[optim]: https://www.youtube.com/watch?v=FnGCDLhaxKU
[hybrid]: https://www.youtube.com/watch?v=vElZc6zSIXM
[emergent]: https://www.youtube.com/watch?v=eR34r7HOU14
[ub]: http://blog.llvm.org/2011/05/what-every-c-programmer-should-know.html
[string]: https://www.youtube.com/watch?v=kPR8h4-qZdk
