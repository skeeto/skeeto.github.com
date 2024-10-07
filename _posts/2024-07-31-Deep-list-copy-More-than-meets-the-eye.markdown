---
title: 'Deep list copy: More than meets the eye'
layout: post
date: 2024-07-31T18:49:57Z
tags: [c]
uuid: 1eb18920-bb29-4d8a-b9f5-2495d3eab697
---

I recently came across a take-home C programming test which had more depth
and complexity than I suspect the interviewer intended. While considering
it, I also came up with a novel, or at least unconventional, solution. The
problem is to deep copy a linked list where each node references a random
list element in addition to usual linkage — similar to [LeetCode problem
138][leet]. This reference is one of identity rather than value, which has
murky consequences.

```c
typedef struct node node;
struct node {
    node *next;
    node *ref;   // arbitrary node in the list, or null
};

node *deepcopy(node *);
```

In the copy, nodes have individual lifetimes allocated using `malloc`
which the caller is responsible for freeing. [While thickheaded][lt], this
is conventional, and I cannot blame the test's designer for sticking to
familiar textbook concepts. My special solution handles this constraint in
stride. (In a well-written program the whole list would have [a single
lifetime][arena] likely shared with yet more objects.)

Ignoring `ref`, copying the normal list linkage is trivial. Walk the
original list, allocate a new node each iteration, and append it to the
result. The hard part is resolving `ref`. Given an arbitrary node pointer,
we must determine to which of the original list nodes it points, then find
the node at the matching position in the new list. Naively we could scan
the old list to search for a match:

```c
node *old = oldlist;
node *new = newlist;
for (;;) {
    if (old->ref) {
        node *findold = oldlist;
        node *findnew = newlist;
        for (;;) {
            if (old->ref == findold) {
                new->ref = findnew;
                break;
            }
            findold = findold->next;
            findnew = findnew->next;
        }
    }
    old = old->next;
    new = new->next;
}
```

The nested loops are obviously quadratic time. That won't scale well. To
do better we need some way to map, by identity, old list nodes onto new
list nodes. However, [pointers do not necessarily have a value on which we
could key a map][hash]. Other languages do not even expose such a concept,
or at least hide it behind some "unsafe" mechanism. In that case it seems
the best we could do is quadratic time.

### Solution by temporary mutation

If we're free to *temporarily* modify the original list, then we can use
memory as a map. After all, memory itself is a kind of pointer-to-object
map! Since we only get one such map per process, we'll need to commandeer
the original list during the copy. The trick is to interleave the two
lists when constructing the new list:

    old1 -> new1 -> old2 -> new2 -> ... -> null

That might look like (note the double-skip per iteration):

```c
for (node *old = oldlist; old; old = old->next->next) {
    node *new = malloc(sizeof(*new));
    new->ref  = 0;
    new->next = old->next;
    old->next = new;
}
```

When we have a pointer to an old list node, the node itself points to the
matching new list node.

```c
for (node *old = oldlist; old; old = old->next->next) {
    if (old->ref) {
        old->next->ref = old->ref->next;
    }
}
```

Then before returning we'd need to deinterleave the lists, restoring the
old list and separating it from the result. This solution is linear time
and doesn't require dealing with the concept of identity. Though modifying
the original list isn't always possible. That won't work if it's accessed
concurrently — shared with another thread, accessed in a signal handler,
or something else reentrant — or if it's in read-only memory.

### Solution by intrusive hash map

If we can obtain a stable value from a pointer, i.e. `uintptr_t` — in
practice virtually always true — then there's an interesting `O(n log n)`
solution using an [intrusive map][ht] which doesn't modify the original
list. This is my own novel solution. The result will be simultaneously a
linked list and a hash map, and the caller won't even know it! Because the
map is built into the list, with a caller-managed lifetime, we won't free
anything before returning.

To start, linked list nodes are embedded at the front of hash trie nodes.
The caller will see this initial field, but not the hash trie fields.
Being at the front, the caller can still `free` them by this "internal"
pointer, which allows the hash trie to be invisible.

```c
typedef struct map map;
struct map {
    node  new;
    map  *child[4];
    node *old;
};
```

The "key" is `old` and the "value" is `new`. Lookup and insert use the
usual "upsert" construction oriented around zero-initialization:

```c
node *upsert(map **m, node *old)
{
    if (!old) {
        return 0;  // map null to null
    }

    uint64_t hash = (uintptr_t)old * 1111111111111111111ull;
    for (; *m; hash <<= 2) {
        if (old == (*m)->old) {
            return &(*m)->new;
        }
        m = &(*m)->child[hash>>62];
    }

    *m = calloc(1, sizeof(map));
    (*m)->old = old;
    return &(*m)->new;
}
```

If the matching node doesn't yet exist, the function creates it. Also note
how it returns an internal pointer. With "upsert" semantics, loop copying
is trivialized:

```c
node *deepcopy(node *head)
{
    map *m = 0;
    for (node *old = head; old; old = old->next) {
        node *new = upsert(&m, old);
        new->next = upsert(&m, old->next);
        new->ref  = upsert(&m, old->ref);
    }
    return upsert(&m, head);
}
```

These easy-to-implement hash tries continue to be generally useful and
elegant, even with traditional memory management. Cloneable, runnable
source with tests is [available as a gist][gist] if you'd like to play
around with it yourself.


[arena]: /blog/2023/09/27/
[gist]: https://gist.github.com/skeeto/9aedc59629de75c07a9533dcfb83af66
[hash]: /blog/2016/05/30/
[ht]: /blog/2023/09/30/
[leet]: https://leetcode.com/problems/copy-list-with-random-pointer/
[lt]: https://www.youtube.com/watch?v=f4ioc8-lDc0&t=4407s
[map]: /blog/2023/09/30/
