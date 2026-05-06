---
title: Concurrent, atomic MSI hash tables
layout: post
date: 2026-05-06T02:01:17Z
tags: [c]
uuid: d877f4c2-b213-4af7-8fb9-269558ee6b86
---

Readers will be familiar with [Mask-Step-Index (MSI) hash tables][msi], a
technique for building fast, open-addressed hash tables in [a dozen lines
of code][flat]. If multiple threads or processes access an MSI table with
at least one still inserting elements, care must be taken to avoid data
races. This article will show how to add atomic operations to MSI tables
in order to support different concurrency constraints.

Let's begin with the simplest case: An integer hash set, no deletions,
only one insert thread (single producer), and consumers do not care about
insert order. That is, the producer inserts A then B, but consumers may
observe B in the table before A. Suppose this is the hash table in the
single-threaded case:

```c
int32_t *lookup(int32_t key, int32_t *table, int exp)
{
    uint64_t hash = ((uint64_t)key * 1111111111111111111u) >> 32;
    uint32_t mask = ((uint32_t)1 << exp) - 1;
    uint32_t step = (hash >> (32 - exp)) | 1;
    for (uint32_t index = hash;;) {
        index = (index + step) & mask;
        if (!table[index] || table[index]==key) {
            return table + index;
        }
    }
}
```

Keys must be non-zero, and tables are zero-initialized. Usage example:

```c
    // Initialization
    enum { exp = 8 };
    int32_t table[1<<8] = {};

    // Producer
    for (int i = 0; i < nkeys; i++) {
        *lookup(keys[i], table, exp) = keys[i];
    }

    // Consumer
    int32_t key = 1234;
    bool present = *lookup(key, table, exp);
```

The only problem is the data race on `table` slots. Since consumers can
tolerate out-of-order insertions, ordering does not matter and relaxed
atomics eliminate the data race. Insert and query now have different
requirements, so it makes sense to distinguish them. Starting with the
latter:

```c
bool contains(int32_t key, int32_t *table, int exp)
{
    uint64_t hash = ((uint64_t)key * 1111111111111111111u) >> 32;
    uint32_t mask = ((uint32_t)1 << exp) - 1;
    uint32_t step = (hash >> (32 - exp)) | 1;
    for (uint32_t index = hash;;) {
        index = (index + step) & mask;
        int32_t k = __atomic_load_n(table+index, __ATOMIC_RELAXED);
        if (!k) {
            return false;
        } else if (k == key) {
            return true;
        }
    }
}
```

Note how all elements are accessed by atomic loads, as a producer may
store to any slot at any time. Now producers:

```c
bool insert(int32_t key, int32_t *table, int exp)
{
    uint64_t hash = ((uint64_t)key * 1111111111111111111u) >> 32;
    uint32_t mask = ((uint32_t)1 << exp) - 1;
    uint32_t step = (hash >> (32 - exp)) | 1;
    for (uint32_t index = hash;;) {
        index = (index + step) & mask;
        if (!table[index]) {
            __atomic_store_n(table+index, key, __ATOMIC_RELAXED);
            return true;
        } else if (table[index] == key) {
            return false;
        }
    }
}
```

This function may load elements non-atomically because there's only one
producer: the current thread. This idea could not be expressed were the
type system involved, e.g. `_Atomic`, but [GCC atomics][gcc] do not
involve require such special qualifiers. Stores on the other hand are
concurrent with consumers, requiring an atomic store. Single-producer,
multiple-consumer (SPMC) usage is nearly identical to the single-threaded
case:

```c
    // Producer
    for (int i = 0; i < nkeys; i++) {
        insert(keys[i], table, exp);
    }

    // Consumer
    int32_t key = 1234;
    bool present = contains(key, table, exp);
```

A concurrent integer hash table is contrived and unrealistic. In a real
program a key likely carries some broader semantic meaning. For example,
if that "integer" is actually a memory offset known as a pointer, then it
*points* at some object, and it is important that stores to that object
happen before consumers observe the pointer in the table:

```c
bool   insert(Thing *thing, Thing **table, int exp)
Thing *lookup(Key key, Thing **table, int exp)
```

Where usage might look like:

```c
    // Producer
    for (int i = 0; i < nthings; i++) {
        things[i].key = ...;  // update/init object
        insert(things+i, table, exp);
    }

    // Consumer
    bool present = !!find((Key){...}, table, exp);
```

In this case relaxed atomics are insufficient. Updates to the inserted
object may be reordered after the insertion, and consumers will race on
those updates. In this case we upgrade to acquire-release:

```c
Thing *lookup(Key key, Thing **table, int exp)
{
    // ...
    for (...) {
        // ...
        Thing *thing = __atomic_load_n(table+index, __ATOMIC_ACQUIRE);
        if (!thing || thing->key==key) {
            return thing;
        }
    }
}

bool insert(Thing *thing, Thing **table, int exp)
{
    // ...
    for (...) {
        // ...
        if (!table[index]) {
            __atomic_store_n(table+index, thing, __ATOMIC_RELEASE);
            return true;
        } else if (table[index]->key == thing->key) {
            return false;
        }
    }
}
```

In this case producer and consumer synchronize on the atomics. Producer
stores are ordered before the release, and consumer loads are ordered
after the acquire. Objects are not modified once in the table, so atomics
are not required for their fields. On some architectures, including x86,
there will be no indication at the ISA level that atomics are in use —
i.e. this likely generates the same code as the single-threaded version —
and these atomics merely constrain the compiler's instruction scheduling.

As a side effect of synchronizing, consumers will now observe insertions
in the same order as the producer. This is a more realistic and practical
situation than an integer hash table.

### Multiple producers

The multiple-producer case (MPMC) is more complicated for producers, but
consumers are unaffected, so we need only modify insertion. Still without
any locks, we will optimistically update the table. We look at the current
slot item, and if nothing is present compare-and-swap the new element in
place. On failure we *acquire* the element that won the race, continuing
as though it's what we saw in the first place.

```c
bool insert(Thing *thing, Thing **table, int exp)
{
    // ...
    for (...) {
        // ...
        Thing *current = __atomic_load_n(table+index, __ATOMIC_ACQUIRE);
        if (!current) {
            int pass = __ATOMIC_RELEASE;
            int fail = __ATOMIC_ACQUIRE;
            if (__atomic_compare_exchange_n(
                    table+index, &current, thing, 0, pass, fail)) {
                return true;
            }
        }
        if (current->key == thing->key) {
            return false;
        }
    }
}
```

This is quite similar [my hash trie concurrency enhancement][trie] a few
years ago.


[flat]: /blog/2025/01/19/#flat-hash-map
[gcc]: https://gcc.gnu.org/onlinedocs/gcc/_005f_005fatomic-Builtins.html
[msi]: /blog/2022/08/08/
[trie]: /blog/2023/09/30/#as-a-concurrent-hash-map
