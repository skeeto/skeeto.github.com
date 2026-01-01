---
title: Freestyle linked lists tricks
layout: post
date: 2025-12-31T11:59:59Z
tags: [c]
uuid: 355dfc03-0e7c-4bae-92fe-5b52174de325
---

Linked lists are a data structure basic building block, with especially
flexible allocation behavior. They're not just a useful starting point,
but sometimes a sound foundation for future growth. I'm going to start
with the beginner stuff, then *without disrupting the original linked
list*, enhance it with new capabilities.

### Linked list basics

For the sake of an interesting example, I'm will demonstrate with the same
concept as [last time I talked about data structures][quick]: a collection
of key/value strings, in the form of an environment variables. This time
in linked list form:

```c
typedef struct {
    char     *data;
    ptrdiff_t len;
} Str;

uint64_t hash64(Str);
bool     equals(Str, Str);

typedef struct Env Env;
struct Env {
    Env *next;
    Str  key;
    Str  value;
};
```

It will be sourced from some string, formatted like the `env` program:

```c
    Str input = S(
        "EDITOR=vim\n"
        "HOME=/home/user\n"
        "PATH=/bin:/usr/bin\n"
        "SHELL=/bin/bash\n"
        "TERM=xterm-256color\n"
        "USER=user\n"
        "SHELL=/bin/sh\n"   // <- repeated entry
    );
```

And all the parser heavy lifting will be done by [our ever-handy `cut`
function][obj]:

```c
typedef struct {
    Str tail;
    Str head;
} Cut;

Cut cut(Str, char);
```

The simplest way to build up a linked list is like a stack, pushing
objects into the front. Zero-initialized `head` pointer, point the new
node at it, then make that node the new `head` element:

```c
Env *parse_reversed(Str s, Arena *a)
{
    Env *head = 0;  // 1
    for (Cut line = {s}; line.tail.len;) {
        line = cut(line.tail, '\n');
        Cut  pair  = cut(line.head, '=');
        Env *env   = new(a, 1, Env);
        env->key   = pair.head;
        env->value = pair.tail;
        env->next  = head;  // 2
        head = env;  // 3
    }
    return head;
}
```

That's it, a complete linked list implementation in three lines of code.
No big deal. Because of the bump allocator, nodes are packed in order in
memory, so the usual cache objections for linked lists do not apply. LIFO
semantics mean the linked list is in reverse order from the source order.
If we're doing a linear scan through the linked list, the last entry in
the source wins, which may be what you wanted:

```c
Str lookup_linear(Env *env, Str key)
{
    for (Env *var = env; var; var = var->next) {
        if (equals(key, var->key)) {
            return var->value;
        }
    }
    return (Str){};
}

    // ...
    Env *env  = parse_reversed(input, &scratch);
    Str value = lookup_linear(env, S("SHELL"));  // <- "/bin/sh"
```

It's just one more line of code to maintain the original order, using a
very simple double-pointer technique:

```c
Env *parse_ordered(Str s, Arena *a)
{
    Env  *head = 0;  // 1
    Env **tail = &head;  // 2
    for (Cut line = {s}; line.tail.len;) {
        // ...
        *tail = env;  // 3
        tail = &env->next;  // 4
    }
    return head;
}
```

No branches necessary, nor dummy nodes. A pointer to the last pointer in
the list works even for empty lists. The `tail` pointer is unneeded once
the list is complete. This form has queue behavior.

### Faster look-up with a tree

If you're doing many look-ups, or if the list is long, those linear scans
to find items in the list are not ideal. We can introduce an intrusive
hash map, in the form of [a hash trie][trie], by adding two more pointers
to the linked list:

```c
typedef struct Env Env;
struct Env {
    Env *next;
    Env *child[2];  // <- hash map linkage
    Str  key;
    Str  value;
};
```

I've found it's simplest to construct a node into the hash map, then link
it onto the list tail. That constructor looks like this:

```c
Env *new_env(Arena *a, Env **env, Str key, Str value)
{
    for (uint64_t h = hash64(key); *env; h <<= 1) {
        env = &(*env)->child[h>>63];
    }
    *env = new(a, 1, Env);
    (*env)->key = key;
    (*env)->value = value;
    return *env;
}
```

Then we swap that into the `head`/`tail` version in place of the original
`new` macro call:

```c
Env *parse_mapped(Str s, Arena *a)
{
    Env  *head = 0;
    Env **tail = &head;
    for (Cut line = {s}; line.tail.len;) {
        // ...
        Env *env = new_env(a, &head, pair.head, pair.tail);
        *tail = env;
        tail = &env->next;
    }
    return head;
}
```

This is now a linked list and a hash map at the same time, built-up piece
by piece without any resizing. We still have the original linked list, but
we can now search it in log time. The look-up function resembles the
constructor:

```c
Str lookup_logn(Env *env, Str key)
{
    for (uint64_t h = hash64(key); env; h <<= 1) {
        if (equals(key, env->key)) {
            return env->value;
        }
        env = env->child[h>>63];
    }
    return (Str){};
}
```

Because of the FIFO semantics, it finds the first match in the source:

```c
    Env *env   = parse_mapped(input, &scratch);
    Str  value = lookup_logn(env, S("SHELL"));  // <- /bin/bash
```

The other matches are also in the tree, and we can find those as well by
continuing traversal. That is, it's already a multi-map. This particular
interface can't pick up where it left off, but we can build one that does
using an iterator/cursor:

```c
typedef struct {
    uint64_t hash;
    Str      key;
    Env     *env;
} EnvIter;

EnvIter new_enviter(Env *env, Str key)
{
    return (EnvIter){hash64(key), key, env};
}

Str enviter_next(EnvIter *it)
{
    for (; it->env; it->hash <<= 1) {
        Env *cur = it->env;
        it->env = it->env->child[it->hash>>63];
        if (equals(it->key, cur->key)) {
            return cur->value;
        }
    }
    return (Str){};
}
```

Then we can use a loop to visit every match in source order:

```c
    Env *env = parse_mapped(input, &scratch);
    for (EnvIter it = new_enviter(env, S("SHELL"));;) {
        Str value = enviter_next(&it);
        if (!value.data) break;
        // ...
    }
```

### Faster look-up with an index table

If the list is static once constructed, or if look-ups happen much more
frequently than the list grows, we can find list items even faster by
constructing an index table over the list: [an MSI hash table][msi]. This
table avoids redundancy by *sharing structure with the list*. Because it's
a flat table, if we keep adding to the list then eventually we'll need to
reconstruct a larger table when it becomes overloaded.

The table itself has a very simple structure, just an array and its size,
expressed as a power-of-two exponent:

```c
typedef struct {
    Env **slots;
    int   exp;
} EnvTable;
```

We do not need the `child` nodes, and so linked list nodes are untouched.
That is, it's not intrusive. In fact, we can build any arbitrary number of
tables over a list, perhaps indexing different properties for different
sorts of queries. The idea is that we build the list first, then create
the table:

```c
EnvTable new_table(Arena *a, Env *env)
{
    // Compute list length
    ptrdiff_t len = 0;
    for (Env *var = env; var; var = var->next) {
        len++;
    }

    // Then compute an appropriate table size
    EnvTable table = {};
    table.exp = 3;
    ptrdiff_t one = 1;
    for (; (one<<table.exp) - (one<<(table.exp-3)) < len; table.exp++) {}
    table.slots = new(a, one<<table.exp, Env *);

    // Then insert linked list items into the table
    for (Env *var = env; var; var = var->next) {
        uint64_t hash = hash64(var->key);
        size_t   mask = ((size_t)1 << table.exp) - 1;
        size_t   step = (size_t)(hash >> (64 - table.exp)) | 1;
        for (size_t i = (size_t)hash;;) {
            i = (i + step) & mask;
            if (!table.slots[i]) {
                table.slots[i] = var;
                break;
            }
        }
    }

    return table;
}
```

Note how only searches for an empty slot, not for a matching entry. That's
because this too is a multi-map, also with elements in insertion order.
Look-ups are constant time:

```c
Str lookup_constant(EnvTable table, Str key)
{
    uint64_t hash = hash64(key);
    size_t   mask = ((size_t)1 << table.exp) - 1;
    size_t   step = (size_t)(hash >> (64 - table.exp)) | 1;
    for (size_t i = (size_t)hash;;) {
        i = (i + step) & mask;
        if (!table.slots[i]) {
            return (Str){};
        } else if (equals(table.slots[i]->key, key)) {
            return table.slots[i]->value;
        }
    }
}
```

It finds the earliest match in the list, meaning an index over the
"reverse" list will find the last entry in the source. The indexed-over
property is the input to `hash64` and `equals`. By using a different input
to these functions we could build another table on, say, value length if
that's a property on which we needed to find elements efficiently. Again,
for multi-map iteration we need some kind of iterator or cursor:

```c
typedef struct {
    EnvTable table;
    Str      key;
    size_t   step;
    size_t   i;
} TableIter;

TableIter new_tableiter(EnvTable table, Str key)
{
    uint64_t hash = hash64(key);
    size_t   step = (size_t)(hash >> (64 - table.exp)) | 1;
    size_t   idx  = (size_t)hash;
    return (TableIter){table, key, step, idx};
}

Str table_next(TableIter *it)
{
    size_t mask  = ((size_t)1 << it->table.exp) - 1;
    Env  **slots = it->table.slots;
    for (;;) {
        it->i = (it->i + it->step) & mask;
        if (!slots[it->i]) {
            return (Str){};
        } else if (equals(slots[it->i]->key, it->key)) {
            return slots[it->i]->value;
        }
    }
}
```

Its usage looks just like the other multi-map:

```c
    Env *env = parse_ordered(input, &scratch);
    EnvTable table = new_table(&scratch, env);
    for (TableIter it = new_tableiter(table, S("SHELL"));;) {
        Str value = table_next(&it);
        if (!value.data) break;
        // ...
    }
```

With these techniques at hand, I can start with linked lists when they are
convenient, and later add needed features without fundamentally changing
the underlying data structure. None of this requires runtime support, and
so it fits comfortably on embedded systems, tiny WebAssembly programs,
etc.  All the above code is available ready to run: [`list.c`][src].


[msi]: /blog/2022/08/08/
[obj]: /blog/2025/03/02/
[quick]: /blog/2025/01/19/
[src]: https://gist.github.com/skeeto/493823d5956dfdc1d95d8c390c2b0e1d
[trie]: /blog/2023/09/30/
