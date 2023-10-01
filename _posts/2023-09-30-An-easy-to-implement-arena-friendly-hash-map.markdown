---
title: An easy-to-implement, arena-friendly hash map
layout: post
date: 2023-09-30T23:18:40Z
tags: [c, optimization]
uuid: 4a457832-7d23-4dab-80f2-31f683379d7b
---

My last article had [tips for for arena allocation][arena]. This next
article demonstrates a technique for building bespoke hash maps that
compose nicely with arena allocation. In addition, they're fast, simple,
and automatically scale to any problem that could reasonably be solved
with an in-memory hash map. To avoid resizing — both to better support
arenas and to simplify implementation — they have slightly above average
memory requirements. The design, which we're calling a *hash-trie*, is the
result of [fruitful collaboration with NRK][nrk], whose sibling article
includes benchmarks. It's my new favorite data structure, and has proven
incredibly useful. With a couple well-placed acquire/release atomics, we
can even turn it into a *lock-free concurrent hash map*.

I've written before about [MSI hash tables][msi], a simple, *very* fast
map that can be quickly implemented from scratch as needed, tailored to
the problem at hand. The trade off is that one must know the upper bound
*a priori* in order to size the base array. Scaling up requires resizing
the array — an impedance mismatch with arena allocation. Search trees
scale better, as there's no underlying array, but tree balancing tends to
be finicky and complex, unsuitable to rapid, on-demand implementation.
**We want the ease of an MSI hash table with the scaling of a tree.**

I'll motivate the discussion with example usage. Suppose we have an array
of pointer+length strings, as defined last time:

```c
typedef struct {
    uint8_t  *data;
    ptrdiff_t len;
} str;
```

And we need a function that removes duplicates in place, but (for the
moment) we're not worried about preserving order. This could be done
naively in quadratic time. Smarter is to sort, then look for runs.
Instead, I've used a hash map to track seen strings. It maps `str` to
`bool`, and it is represented as type `strmap` and one insert+lookup
function, `upsert`.

```c
// Insert/get bool value for given str key.
bool *upsert(strmap **, str key, arena *);

ptrdiff_t unique(str *strings, ptrdiff_t len, arena scratch)
{
    ptrdiff_t count = 0;
    strmap *seen = 0;
    while (count < len) {
        bool *b = upsert(&seen, strings[count], &scratch);
        if (*b) {
            // previously seen (discard)
            strings[count] = strings[--len];
        } else {
            // newly-seen (keep)
            count++;
            *b = 1;
        }
    }
    return count;
}
```

In particular, note:

* A null pointer is an empty hash map and initialization is trivial. As
  discussed in the last article, one of my arena allocation principles is
  default zero-initializion. Put together, than means any data structure
  containing a map already has a ready-to-use empty map.

* The map is allocated out of the scratch arena so it's automatically
  freed upon any return. It's as care-free as garbage collection.

* The map directly uses strings in the input array as keys, without making
  copies nor worrying about ownership. Arenas own objects, not references.
  If I wanted to carve out some fixed keys ahead of time, I could even
  insert static strings.

* `upsert` returns a pointer to a value. That is, a pointer into the map.
  This is not strictly required, but usually makes for a simple interface.
  When an entry is new, this value will be false (zero-initialized).

So, what is this wonderful data structure? Here's the basic shape:

```c
typedef struct {
    hashmap *child[4];
    keytype  key;
    valtype  value;
} hashmap;
```

They `child` and `key` fields are essential to the map. Adding a `child`
to any data structure turns it into a hash map over whatever field you
choose as the key. In other words, a hash-trie can serve as an *intrusive
hash map*. In several programs I've combined intrusive lists and intrusive
hash maps simultaneous to create an insert-ordered hash map. Going the
other direction, omitting `value` turns it into a hash set. (Which is what
`unique` *really* needs!)

As you probably guessed, this hash-trie is a 4-ary tree. It can easily be
2-ary (leaner but slower) or 8-ary (bigger and usually no faster), but
4-ary strikes a good balance, if a bit bulky. In the example above,
`keytype` would be `str` and `valtype` would be `bool`. The most general
form of `upsert` looks like this:

```c
static valtype *upsert(hashmap **m, keytype key, arena *perm)
{
    for (uint64_t h = hash(key); *m; h <<= 2) {
        if (equals(key, (*m)->key)) {
            return &(*m)->value;
        }
        m = &(*m)->child[h>>62];
    }
    if (!perm) {
        return 0;
    }
    *m = new(perm, hashmap);
    (*m)->key = key;
    return &(*m)->value;
}
```

This will take some unpacking. The first argument is a pointer to a
pointer. That's the destination for any newly-allocated element. As it
travels down the tree, this points into the parent's `child` array. If
it points to null, then it's an empty tree which, by definition, does not
contain the key.

We need two "methods" for keys: `hash` and `equals`. The hash function
should return a uniformly distributed integer. As is usually the case,
less uniform fast hashes generally do better than highly-uniform slow
hashes. For hash maps under ~100K elements, a 32-bit hash is fine, but
larger maps should use a 64-bit hash state and result. Hash collisions
revert to linear, linked list performance and, per the birthday paradox,
that will happen often with 32-bit hashes on large hash maps.

If you're worried about pathological inputs, add a seed parameter to
`upsert` and `hash`. Or maybe even use the address `m` as a seed. The
specifics depend on your security model. It's not an issue for most hash
maps, so I don't demonstrate it here.

The top two bits of the hash are used to select a branch. These tend to be
higher quality for [multiplicative hash functions][hash]. At each level,
two bits are shifted out. This is what gives it its name: a *trie* of the
*hash bits*. Though it's un-trie-like for elements to be deposited at the
first empty spot found. To make it 2-ary or 8-ary, use 1 or 3 bits at a
time.

I initially tried a [Multiplicative Congruential Generator][mcg] (MCG) to
select the next branch at each trie level, instead of bit shifting, but
NRK noticed it was consistently slower than shifting.

While "delete" could be handled using gravestones, many deletes would not
work well. After all, the underlying allocator is an arena. A combination
of uniformly distributed branching and no deletion means that rebalancing
is unnecessary. This is what grants it its simplicity!

If no arena is provided, it reverts to a lookup and returns null when the
key is not found. It allows one function to flexibly serve both modes. In
`unique`, pure lookups are unneeded, so this condition could be skipped in
its `strmap`.

Sometimes it's useful to return the entire `hashmap` object itself rather
than an internal pointer, particularly when it's intrusive. Use whichever
works best for the situation. Regardless, exploit zero-initialization to
detect newly-allocated elements when possible.

In some cases we may deep copy the key in its arena before inserting it
into the map. The provided key may be a temporary (e.g. `sprintf`) which
the map outlives, and the caller doesn't want to allocate a longer-lived
key unless it's needed. It's all part of tailoring the map to the problem,
which we can do because it's so short and simple!

### Fleshing it out

Putting it all together, `unique` could look like the following, with
`strmap`/`upsert` renamed to `strset`/`ismember`:

```c
uint64_t hash(str s)
{
    uint64_t h = 0x100;
    for (ptrdiff_t i = 0; i < s.len; i++) {
        h ^= s.data[i];
        h *= 1111111111111111111u;
    }
    return h;
}

bool equals(str a, str b)
{
    return a.len==b.len && !memcmp(a.data, b.data, a.len);
}

typedef struct {
    strset *child[4];
    str     key;
} strset;

static bool ismember(strset **m, str key, arena *perm)
{
    for (uint64_t h = hash(key); *m; h <<= 2) {
        if (equals(key, (*m)->key)) {
            return 1;
        }
        m = &(*m)->child[h>>62];
    }
    *m = new(perm, strset);
    (*m)->key = key;
    return 0;
}

ptrdiff_t unique(str *strings, ptrdiff_t len, arena scratch)
{
    ptrdiff_t count = 0;
    for (strset *seen = 0; count < len;) {
        if (ismember(&seen, strings[count], &scratch)) {
            strings[count] = strings[--len];
        } else {
            count++;
        }
    }
    return count;
}
```

The FNV hash multiplier is 19 ones, my favorite prime. I don't bother with
an xorshift finalizer because the bits are used most-significant first.

Exercise for the reader: Support retaining the original input order using
an intrusive linked list on `strset`.

### Relative pointers?

As mentioned, four pointers per entry — 32 bytes on 64-bit hosts — makes
these hash-tries a bit heavier than average. It's not an issue for smaller
hash maps, but has practical consequences for huge hash maps.

In attempt to address this, I experimented with [relative pointers][rel]
(example: [`markov.c`][markov]). That is, instead of pointers I use signed
integers whose value indicates an offset *relative to itself*. Because
relative pointers can only refer to nearby memory, a custom allocator is
imperative, and arenas fit the bill perfectly. Range can be extended by
exploiting memory alignment. In particular, 32-bit relative pointers can
reference up to 8GiB in either direction. Zero is reserved to represent a
null pointer, and relative pointers cannot refer to themselves.

As a bonus, data structures built out of relative pointers are *position
independent*. A collection of them — perhaps even a whole arena — can be
dumped out to, say, a file, loaded back at a different position, then
continue to operate as-is. Very cool stuff.

Using 32-bit relative pointers on 64-bit hosts cuts the hash-trie overhead
in half, to 16 bytes. With an arena no larger than 8GiB, such pointers are
guaranteed to work. No object is ever too far away. It's a compounding
effect, too. Smaller map nodes means a larger number of them are in reach
of a relative pointer. Also very cool.

However, as far as I know, no generally available programming language
implementation supports this concept well enough to put into practice. You
could implement relative pointers with language extension facilities, such
as C++ operator overloads, but *no tools will understand them* — a major
bummer. You can no longer use a debugger to examine such structures, and
it's just not worth that cost. If only arena allocation was more popular…

### As a concurrent hash map

For the finale, let's convert `upsert` into a concurrent, lock-free hash
map. That is, multiple threads can call upsert concurrently on the same
map. Each must still have its own arena, probably per-thread arenas, and
so no implicit locking for allocation.

The structure itself requires no changes! Instead we need two atomic
operations: atomic load (acquire), and atomic compare-and-exchange
(acquire/release). They operate only on `child` array elements and the
tree root. To illustrate I will use [GCC atomics][gcc], also supported by
Clang.

```c
static valtype *upsert(map **m, keytype key, arena *perm)
{
    for (uint64_t h = hash(key);; h <<= 2) {
        map *n = __atomic_load_n(m, __ATOMIC_ACQUIRE);
        if (!n) {
            if (!perm) {
                return 0;
            }
            arena rollback = *perm;
            map *new = new(perm, map, 1);
            new->key = key;
            int pass = __ATOMIC_RELEASE;
            int fail = __ATOMIC_ACQUIRE;
            if (__atomic_compare_exchange_n(m, &n, new, 0, pass, fail)) {
                return &new->value;
            }
            *perm = rollback;
        }
        if (equals(n->key, key)) {
            return &n->value;
        }
        m = n->child + (h>>62);
    }
}
```

First an atomic load retrieves the current node. If there is no such node,
then attempt to insert one using atomic compare-and-exchange. The [ABA
problem][aba] is not an issue thanks again to lack of deletion: Once set,
a pointer never changes. Before allocating a node, take a snapshot of the
arena so that the allocation can be reverted on failure. If another thread
got there first, continue tumbling down the tree *as though a null was
never observed*.

On compare-and-swap failure, it turns into an acquire load, just as it
began. On success, it's a release store, synchronizing with acquire loads
on other threads.

The `key` field does not require atomics because it's synchronized by the
compare-and-swap. That is, the assignment will happen before the node is
inserted, and keys do not change after insertion. The same goes for any
zeroing done by the arena.

**Loads and stores through the returned pointer are the caller's
responsibility.** These likely require further synchronization. If
`valtype` is a shared counter then an atomic increment is sufficient. In
other cases, `upsert` should probably be modified to accept an initial
value to be assigned alongside the key so that the entire key/value pair
inserted atomically. Alternatively, [break it into two steps][q]. The
details depend on the needs of the program.

On small trees there will much contention near the root of the tree during
inserts. However, a contentious tree will not stay small for long! The
hash function will spread threads around the tree, generally keeping them
off each other's toes.

A complete demo you can try yourself: **[`concurrent-hash-trie.c`][c]**.
It returns a value pointer like above, and store/load is synchronized by
the thread join. Each thread is given a per-thread subarena allocated out
of the main arena, and the final tree is built from these subarenas.

A complete fast, concurrent, lock-free hash map in under 30 lines of C
sounds like a sweet deal to me!


[aba]: /blog/2014/09/02/
[arena]: /blog/2023/09/27/
[c]: https://github.com/skeeto/scratch/blob/master/misc/concurrent-hash-trie.c
[gcc]: https://gcc.gnu.org/onlinedocs/gcc/_005f_005fatomic-Builtins.html
[hash]: /blog/2018/07/31/
[markov]: https://github.com/skeeto/scratch/blob/master/misc/markov.c
[mcg]: /blog/2019/11/19/
[msi]: /blog/2022/08/08/
[nrk]: https://nrk.neocities.org/articles/hash-trees-and-tries
[q]: /blog/2022/05/14/
[rel]: https://www.youtube.com/watch?v=Z0tsNFZLxSU
