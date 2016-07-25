---
title: Generic C Reference Counting
layout: post
date: 2015-02-17T04:06:11Z
tags: [c]
uuid: 58357076-8f76-3506-0c5e-198bfc711f8d
---

As a result of making regular use of [object-oriented programming in
C][oop], I've discovered a useful reference counting technique for the
occasional dynamically allocated structs that need it. The situation
arises when the same struct instance is shared between an arbitrary
number of other data structures and I need to keep track of it all.

It's *incredibly* simple and lives entirely in a header file, so
without further ado (`ref.h`):

~~~c
#pragma once

struct ref {
    void (*free)(const struct ref *);
    int count;
};

static inline void
ref_inc(const struct ref *ref)
{
    ((struct ref *)ref)->count++;
}

static inline void
ref_dec(const struct ref *ref)
{
    if (--((struct ref *)ref)->count == 0)
        ref->free(ref);
}
~~~

It has only two fields: the reference count and a "method" that knows
how to free the object once the reference count hits 0. Structs using
this reference counter will know how to free themselves, so callers
will never call a specific `*_destroy()`/`*_free()` function. Instead
they call `ref_dec()` to decrement the reference counter and let it
happen on its own.

I decided to go with a signed count because it allows for better error
checking. It may be worth putting an `assert()` in `ref_inc()` and
`ref_dec()` to ensure the count is always non-negative. I chose an
`int` because it's fast, and anything smaller will be padded out to
*at least* that size anyway. On x86-64, `struct ref` is 16 bytes.

This is basically all there is to a C++ [shared_ptr][shared_ptr],
leveraging C++'s destructors and performing all increment/decrement
work automatically.

### Thread Safety

Those increments and decrements aren't thread safe, so this won't work
as-is when data structures are shared between threads. If you're sure
that you're using GCC on a capable platform, you can make use of its
[atomic builtins][gcc], making the reference counter completely thread
safe.

~~~c
static inline void
ref_inc(const struct ref *ref)
{
    __sync_add_and_fetch((int *)&ref->count, 1);
}

static inline void
ref_dec(const struct ref *ref)
{
    if (__sync_sub_and_fetch((int *)&ref->count, 1) == 0)
        ref->free(ref);
}
~~~

Or if you're using C11, [make use of the new stdatomic.h][lf].

~~~c
static inline void
ref_inc(const struct ref *ref)
{
    atomic_fetch_add((int *)&ref->count, 1);
}

static inline void
ref_dec(const struct ref *ref)
{
    if (atomic_fetch_sub((int *)&ref->count, 1) == 1)
        ref->free(ref);
}
~~~

### What's That Const?

There's a very deliberate decision to make all of the function
arguments `const`, for both reference counting functions and the
`free()` method. This may seem wrong because these functions are
*specifically* intended to modify the reference count. There are
dangerous-looking casts in each case to remove the `const`.

The reason for this is that's it's likely for someone holding a
`const` pointer to one of these objects to want to keep their own
reference. Their promise not to modify the object doesn't *really*
apply to the reference count, which is merely embedded metadata. They
would need to cast the `const` away before being permitted to call
`ref_inc()` and `ref_dec()`. Rather than litter the program with
dangerous casts, the casts are all kept in one place — in the
reference counting functions — where they're strictly limited to
mutating the reference counting fields.

On a related note, the `stdlib.h` `free()` function doesn't take a
`const` pointer, so the `free()` method taking a `const` pointer is a
slight departure from the norm. Taking a non-`const` pointer [was a
mistake in the C standard library][const]. The `free()` function
mutates the pointer itself — including all other pointers to that
object — making it invalid. Semantically, it doesn't mutate the
memory *behind* the pointer, so it's not actually violating the
`const`. To compare, the [Linux kernel `kfree()`][kfree] takes a
`const void *`.

Just as users may need to increment and decrement the counters on
`const` objects, they'll also need to be able to `free()` them, so
it's also a `const`.

### Usage Example

So how does one use this generic reference counter? Embed a `struct
ref` in your own structure and use our old friend: the
`container_of()` macro. For anyone who's forgotten, this macro not
part of standard C, but you can define it with `offsetof()`.

~~~c
#define container_of(ptr, type, member) \
    ((type *)((char *)(ptr) - offsetof(type, member)))
~~~

Here's a dumb linked list example where each node is individually
reference counted. Adding an extra 16 bytes to each of your linked
list nodes isn't normally going to help with much, but if the tail of
the linked list is being shared between different data structures
(such as other lists), reference counting makes things a lot simpler.

~~~c
struct node {
    char id[64];
    float value;
    struct node *next;
    struct ref refcount;
};
~~~

I put `refcount` at the end so that we'll have to use `container_of()`
in this example. It conveniently casts away the `const` for us.

~~~c
static void
node_free(const struct ref *ref)
{
    struct node *node = container_of(ref, struct node, refcount);
    struct node *child = node->next;
    free(node);
    if (child)
        ref_dec(&child->refcount);
}
~~~

Notice that it recursively decrements its child's reference count
afterwards (intentionally tail recursive). A whole list will clean
itself up when the head is freed and no part of the list is shared.

The allocation function sets up the `free()` function pointer and
initializes the count to 1.

~~~c
struct node *
node_create(char *id, float value)
{
    struct node *node = malloc(sizeof(*node));
    snprintf(node->id, sizeof(node->id), "%s", id);
    node->value = value;
    node->next = NULL;
    node->refcount = (struct ref){node_free, 1};
    return node;
}
~~~

(Side note: I used `snprintf()` because [`strncpy()` is
broken][strncpy] and `strlcpy()` is non-standard, so it's the most
straightforward way to do this in standard C.);

And to start making some use of the reference counter, here's push and
pop.

~~~c
void
node_push(struct node **nodes, char *id, float value)
{
    struct node *node = node_create(id, value);
    node->next = *nodes;
    *nodes = node;
}

struct node *
node_pop(struct node **nodes)
{
    struct node *node = *nodes;
    *nodes = (*nodes)->next;
    if (*nodes)
        ref_inc(&(*nodes)->refcount);
    return node;
}
~~~

Notice `node_pop()` increments the reference count of the new head
node before returning. That's because the node now has an additional
reference: from `*nodes` *and* from the node that was just popped.
It's up to the caller to free the returned node, which would decrement
the count of the new head node, but not free it. Alternatively
`node_pop()` could set `next` on the returned node to NULL rather than
increment the counter, which would also prevent the returned node from
freeing the new head when it gets freed. But it's probably more useful
for the returned node to keep functioning as a list. That's what the
reference counting is for, after all.

Finally, a simple program to exercise it all. It reads ID/value pairs
from standard input.

~~~c
void
node_print(struct node *node)
{
    for (; node; node = node->next)
        printf("%s = %f\n", node->id, node->value);
}

int main(void)
{
    struct node *nodes = NULL;
    char id[64];
    float value;
    while (scanf(" %63s %f", id, &value) == 2)
        node_push(&nodes, id, value);
    if (nodes != NULL) {
        node_print(nodes);
        struct node *old = node_pop(&nodes);
        node_push(&nodes, "foobar", 0.0f);
        node_print(nodes);
        ref_dec(&old->refcount);
        ref_dec(&nodes->refcount);
    }
    return 0;
}
~~~

I've used this technique several times over the past few months. It's
trivial to remember, so I just code it up from scratch each time I
need it.


[oop]: /blog/2014/10/21/
[gcc]: http://gcc.gnu.org/onlinedocs/gcc-4.1.2/gcc/Atomic-Builtins.html
[lf]: /blog/2014/09/02/
[const]: http://yarchive.net/comp/const.html
[kfree]: http://lxr.free-electrons.com/source/include/linux/slab.h#L144
[shared_ptr]: http://en.cppreference.com/w/cpp/memory/shared_ptr
[strncpy]: https://randomascii.wordpress.com/2013/04/03/stop-using-strncpy-already/
