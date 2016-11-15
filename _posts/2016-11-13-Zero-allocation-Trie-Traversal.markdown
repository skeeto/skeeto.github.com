---
title: Zero-allocation Trie Traversal
layout: post
date: 2016-11-13T06:03:24Z
tags: [c, compsci]
uuid: 38dd798b-9e27-3109-590c-3a8482f634a7
---

As part of a demonstration in [an upcoming article][next], I wrote a
simple [trie][trie] implementation. A trie is a search tree where the
keys are a sequence of symbols (i.e. strings). Strings with a common
prefix share an initial path down the trie, and the keys themselves
are stored implicitly by the structure of the trie. It's commonly used
as a sorted set or, when values are associated with nodes, an
associative array.

This wasn't my first time writing a trie. The curse of programming in
C is rewriting the same data structures and algorithms over and over.
It's the problem C++ templates are intended to solve. This rewriting
isn't always bad since each implementation is typically customized for
its specific use, often resulting in greater performance and a smaller
resource footprint.

Every time I've rewritten a trie, my implementation is a little bit
better than the last. This time around I discovered an approach for
traversing, both depth-first and breadth-first, an arbitrarily-sized
trie without memory allocation. I'm definitely not the first to
discover something like this. There's [Deutsch-Schorr-Waite pointer
reversal][dsw] for binary graphs (1965) — which I originally learned
from reading the [Scheme 9 from Outer Space][s9] garbage collector
source — and [Morris in-order traversal][morris] (1979) for binary
trees. The former requires two extra tag bits per node and the latter
requires no modifications at all.

### What's a trie?

But before I go further, some background. A trie can come in many
shapes and sizes, but in the simple case each node of a trie has as
many pointers as its alphabet. For illustration purposes, imagine a
trie for strings of only four characters: A, B, C, and D. Each node is
essentially four pointers.

~~~c
#define TRIE_ALPHABET_SIZE  4
#define TRIE_STATIC_INIT    {.flags = 0}
#define TRIE_TERMINAL_FLAG  (1U << 0)

struct trie {
    struct trie *next[TRIE_ALPHABET_SIZE];
    unsigned flags;
};
~~~

It includes a `flags` field, where a single bit tracks whether or not
a node is terminal — that is, a key terminates at this node. Terminal
nodes are not necessarily leaf nodes, which is the case when one key
is a prefix of another key. I could instead have used a 1-bit
bit-field (e.g. `int is_terminal : 1;`) but I don't like bit-fields.

A trie with the following keys, inserted in any order:

    AAAAA
    ABCD
    CAA
    CAD
    CDBD

Looks like this (terminal nodes illustrated as small black squares):

![](/img/trie/trie.svg)

The root of the trie is the empty string, and each child represents a
trie prefixed with one of the symbols from the alphabet. This is a
nice recursive definition, and it's tempting to write recursive
functions to process it. For example, here's a recursive insertion
function.

~~~c
int
trie_insert_recursive(struct trie *t, const char *s)
{
    if (!*s) {
        t->flags |= TRIE_TERMINAL_FLAG;
        return 1;
    }

    int i = *s - 'A';
    if (!t->next[i]) {
        t->next[i] = malloc(sizeof(*t->next[i]));
        if (!t->next[i])
            return 0;
        *t->next[i] = (struct trie)TRIE_STATIC_INIT;
    }
    return trie_insert_recursive(t->next[i], s + 1);
}
~~~

If the string is empty (`!*s`), mark the current node as terminal.
Otherwise recursively insert the substring under the appropriate
child. That's a tail call, and any optimizing compiler would optimize
this call into a jump back to the beginning of of the function
(tail-call optimization), reusing the stack frame as if it were a
simple loop.

If that's not good enough, such as when optimization is disabled for
debugging and the recursive definition is blowing the stack, this is
trivial to convert to a safe, iterative function. I prefer this
version anyway.

~~~c
int
trie_insert(struct trie *t, const char *s)
{
    for (; *s; s++) {
        int i = *s - 'A';
        if (!t->next[i]) {
            t->next[i] = malloc(sizeof(*t->next[i]));
            if (!t->next[i])
                return 0;
            *t->next[i] = (struct trie)TRIE_STATIC_INIT;
        }
        t = t->next[i];
    }
    t->flags |= TRIE_TERMINAL_FLAG;
    return 1;
}
~~~

Finding a particular prefix in the trie iteratively is also easy. This
would be used to narrow the trie to a chosen prefix before iterating
over the keys (e.g. find all strings matching a prefix).

~~~c
struct trie *
trie_find(struct trie *t, const char *s)
{
    for (; *s; s++) {
        int i = *s - 'A';
        if (!t->next[i])
            return NULL;
        t = t->next[i];
    }
    return t;
}
~~~

Depth-first traversal is *stack-oriented*. The stack represents the
path through the graph, and each new vertex is pushed into this stack
as it's visited. A recursive traversal function can implicitly use the
call stack for storing this information, so no additional data
structure is needed.

The downside is that the call is no longer tail-recursive, so a large
trie will blow the stack. Also, the caller needs to provide a callback
function because the stack cannot unwind to return a value: The stack
has important state on it. Here's a typedef for the callback.

~~~c
typedef void (*trie_visitor)(const char *key, void *arg);
~~~

And here's the recursive depth-first traversal function. The top-level
caller passes the same buffer for `buf` and `bufend`, which must be at
least as large as the largest key. The visited key will be written to
this buffer and passed to the visitor.

~~~c
void
trie_dfs_recursive(struct trie *t,
                   char *buf,
                   char *bufend,
                   trie_visitor v,
                   void *arg)
{
    if (t->flags & TRIE_TERMINAL_FLAG) {
        *bufend = 0;
        v(buf, arg);
    }

    for (int i = 0; i < TRIE_ALPHABET_SIZE; i++) {
        if (t->next[i]) {
            *bufend = 'A' + i;
            trie_dfs_recursive(t->next[i], buf, bufend + 1, v, arg);
        }
    }
}
~~~

#### Heap-allocated Traversal Stack

Moving the traversal stack to the heap would eliminate the stack
overflow problem and it would allow control to return to the caller.
This is going to be a lot of code for an article, but bear with me.

First define an iterator object. The stack will need two pieces of
information: which node did we come from (`p`) and through which
pointer (`i`). When a node has been exhausted, this will allow return
to the parent. The `root` field tracks when traversal is complete.

~~~c
struct trie_iter {
    struct trie *root;
    char *buf;
    char *bufend;
    struct {
        struct trie *p;
        int i;
    } *stack;
};
~~~

A special value of -1 in `i` means it's the first visit for this node
and it should be visited by the callback if it's terminal.

The iterator is initialized with `trie_iter_init`. The `max` indicates
the maximum length of any key. A more elaborate implementation could
automatically grow the stack to accommodate (e.g. realloc()), but I'm
keeping it as simple as possible.

~~~c
int
trie_iter_init(struct trie_iter *it, struct trie *t, size_t max)
{
    it->root = t;
    it->stack = malloc(sizeof(*it->stack) * max);
    if (!it->stack)
        return 0;
    it->buf = it->bufend = malloc(max);
    if (!it->buf) {
        free(it->stack);
        return 0;
    }
    it->stack->p = t;
    it->stack->i = -1;
    return 1;
}

void
trie_iter_destroy(struct trie_iter *it)
{
    free(it->stack);
    it->stack = NULL;
    free(it->buf);
    it->buf = NULL;
}
~~~

And finally the complicated part. This uses the allocated stack to
explore the trie in a loop until it hits a terminal, at which point it
returns. A further call continues the traversal from where it left
off. It's like a hand-coded [generator][gen]. With the way it's
written, the caller is obligated to follow through with the entire
iteration before destroying the iterator, but this would be easy to
correct.

~~~c
int
trie_iter_next(struct trie_iter *it)
{
    for (;;) {
        struct trie *current = it->stack->p;
        int i = it->stack->i++;

        if (i == -1) {
            /* Return result if terminal node. */
            if (current->flags & TRIE_TERMINAL_FLAG) {
                *it->bufend = 0;
                return 1;
            }
            continue;
        }

        if (i == TRIE_ALPHABET_SIZE) {
            /* End of current node. */
            if (current == it->root)
                return 0;  // back at root, done
            it->stack--;
            it->bufend--;
            continue;
        }

        if (current->next[i]) {
            /* Push on next child node. */
            *it->bufend = 'A' + i;
            it->stack++;
            it->bufend++;
            it->stack->p = current->next[i];
            it->stack->i = -1;
        }
    }
}
~~~

This is *much* nicer for the caller since there's no control inverse.

~~~c
struct trie_iter it;
trie_iter_init(&it, &trie_root, KEY_MAX);
while (trie_iter_next(&it)) {
    // ... do something with it.buf ...
}
trie_iter_destroy(&it);
~~~

There are a few downsides to this:

1. Initialization could fail (not checked in the example) since it
   allocates memory.

2. Either the caller has to keep track of the maximum key length, or
   the iterator grows the stack automatically, which would mean
   iteration could fail at any point in the middle.

3. In order to destroy the trie, it needs to be traversed: Freeing
   memory first requires allocating memory. If the program is out of
   memory, it cannot destroy the trie to clean up before handling the
   situation, nor to make more memory available. It's not good for
   resilience.

Wouldn't it be nice to traverse the trie without memory allocation?

### Modifying the Trie

Rather than allocate a separate stack, the stack can be allocated
across the individual nodes of the trie. Remember those `p` and `i`
fields from before? Put them on the trie.

~~~c
struct trie_v2 {
    struct trie_v2 *next[TRIE_ALPHABET_SIZE];
    struct trie_v2 *p;
    int i;
    unsigned flags;
};
~~~

![](/img/trie/trie_v2.svg)

This automatically scales with the size of the trie, so there will
always be enough of this stack. With the stack "pre-allocated" like
this, traversal requires no additional memory allocation.

The iterator itself becomes a little simpler. It cannot fail and it
doesn't need a destructor.

~~~c
struct trie_v2_iter {
    struct trie_v2 *current;
    char *buf;
};

void
trie_v2_iter_init(struct trie_v2_iter *it, struct trie_v2 *t, char *buf)
{
    t->p = NULL;
    t->i = -1;
    it->current = t;
    it->buf = buf;
}
~~~

The iteration function itself is almost identical to before. Rather
than increment a stack pointer, it uses `p` to chain the nodes as a
linked list.

~~~c
int
trie_v2_iter_next(struct trie_v2_iter *it)
{
    for (;;) {
        struct trie_v2 *current = it->current;
        int i = it->current->i++;

        if (i == -1) {
            /* Return result if terminal node. */
            if (current->flags & TRIE_TERMINAL_FLAG) {
                *it->buf = 0;
                return 1;
            }
            continue;
        }

        if (i == TRIE_ALPHABET_SIZE) {
            /* End of current node. */
            if (!current->p)
                return 0;
            it->current = current->p;
            it->buf--;
            continue;
        }

        if (current->next[i]) {
            /* Push on next child node. */
            *it->buf = 'A' + i;
            it->buf++;
            it->current = current->next[i];
            it->current->p = current;
            it->current->i = -1;
        }

    }
}
~~~

During traversal the iteration pointers look something like this:

![](/img/trie/trie_v2-dfs.svg)

This is not without its downsides:

1. Traversal is not re-entrant nor thread-safe. It's not possible to
   run multiple in-place iterators side by side on the same trie since
   they'll clobber each other.

2. It uses more memory — O(n) rather than O(max-key-length) — and sits
   on this extra memory for its entire lifetime.

#### Breadth-first Traversal

The same technique can be used for breadth-first search, which is
*queue-oriented* rather than stack-oriented. The `p` pointers are
instead chained into a queue, with a `head` and `tail` pointer
variable for each end. As each node is visited, its children are
pushed into the queue linked list.

This isn't good for visiting keys by name. `buf` was itself a stack
and played nicely with depth-first traversal, but there's no easy way
to build up a key in a buffer breadth-first. So instead here's a
function to destroy a trie breadth-first.

~~~c
void
trie_v2_destroy(struct trie_v2 *t)
{
    struct trie_v2 *head = t;
    struct trie_v2 *tail = t;
    while (head) {
        for (int i = 0; i < TRIE_ALPHABET_SIZE; i++) {
            struct trie_v2 *next = head->next[i];
            if (next) {
                next->p = NULL;
                tail->p = next;
                tail = next;
            }
        }
        struct trie_v2 *dead = head;
        head = head->p;
        free(dead);
    }
}
~~~

During its traversal the `p` pointers link up like so:

![](/img/trie/trie_v2-bfs.svg)

### Further Research

In my real code there's also a flag to indicate the node's allocation
type: static or heap. This allows a trie to be composed of nodes from
both kinds of allocations while still safe to destroy. It might also
be useful to pack a reference counter into this space so that a node
could be shared by more than one trie.

For a production implementation it may be worth packing `i` into the
`flags` field since it only needs a few bits, even with larger
alphabets. Also, I bet, as in Deutsch-Schorr-Waite, the `p` field
could be eliminated and instead one of the child pointers is
temporarily reversed. With these changes, this technique would fit
into the original `struct trie` without changes, eliminating the extra
memory usage.

Update: Over on Hacker News, [psi-squared has interesting
suggestions][hn] such as leaving the traversal pointers intact,
particularly in the case of a breadth-first search, which, until the
next trie modification, allows for concurrent follow-up traversals.


[trie]: https://en.wikipedia.org/wiki/Trie
[morris]: http://www.geeksforgeeks.org/morris-traversal-for-preorder/
[dsw]: https://xlinux.nist.gov/dads/HTML/SchorrWaiteGraphMarking.html
[s9]: http://t3x.org/s9fes/
[gen]: https://en.wikipedia.org/wiki/Generator_(computer_programming)
[hn]: https://news.ycombinator.com/item?id=12943339
[next]: /blog/2016/11/15/
