---
title: 'The quick and practical "MSI" hash table'
layout: post
date: 2022-08-08T23:57:08Z
tags: [c, optimization]
uuid: 4a7d8c3d-3bcf-4b10-b50a-64227c02b254
excerpt_separator: <!--more-->
---

*Follow-up: [Solving "Two Sum" in C with a tiny hash table][twosum]*

I [generally prefer C][c], so I'm accustomed to building whatever I need
on the fly, such as heaps, [linked lists][list], and especially hash
tables. Few programs use more than a small subset of a data structure's
features, making their implementation smaller, simpler, and [more
efficient][bench] than the general case, which must handle every edge
case. A typical hash table tutorial will describe a relatively lengthy
program, but in practice, bespoke hash tables are [only a few lines of
code][bs]. Over the years I've worked out some basic principles for hash
table construction that aid in quick and efficient implementation. This
article covers the technique and philosophy behind what I've come to call
the "mask-step-index" (MSI) hash table, which is my standard approach.

<!--more-->

MSI hash tables are nothing novel, just a [double hashed][dh], [open
address][oa] hash table layered generically atop an external array. It's
best regarded as a kind of database index — *a lookup index over an
existing array*. The array exists independently, and the hash table
provides an efficient lookup into that array over some property of its
entries.

The core of the MSI hash table is this iterator function:

```c
// Compute the next candidate index. Initialize idx to the hash.
int32_t ht_lookup(uint64_t hash, int exp, int32_t idx)
{
    uint32_t mask = ((uint32_t)1 << exp) - 1;
    uint32_t step = (hash >> (64 - exp)) | 1;
    return (idx + step) & mask;
}
```

The name should now make sense. I literally sound it out in my head when I
type it, like a mnemonic. Compute a mask, then a step size, finally an
index. The `exp` parameter is a power-of-two exponent for the hash table
size, [which may look familiar][exp]. I've used `int32_t` for the index,
but it's easy to substitute, say, `size_t`. I try to optimize for the
common case, where a 31-bit index is more than sufficient, and a signed
type since [subscripts should be signed][ss]. Internally it uses unsigned
types since overflow is both expected and harmless thanks to the
power-of-two hash table size.

It's the caller's responsibility to compute the hash, and the MSI iterator
tells the caller *where to look next*. For insertion, the caller (maybe)
looks either for an existing entry to override, or an empty slot. For
lookup, the caller looks for a matching entry, giving up as soon as it
find an empty slot. An insertion loop looks like this string intern table:

```c
#define EXP 15

// Initialize all slots to an "empty" value (null)
#define HT_INIT { {0}, 0 }
struct ht {
    char *ht[1<<EXP];
    int32_t len;
};

char *intern(struct ht *t, char *key)
{
    uint64_t h = hash(key, strlen(key)+1);
    for (int32_t i = h;;) {
        i = ht_lookup(h, EXP, i);
        if (!t->ht[i]) {
            // empty, insert here
            if ((uint32_t)t->len+1 == (uint32_t)1<<EXP) {
                return 0;  // out of memory
            }
            t->len++;
            t->ht[i] = key;
            return key;
        } else if (!strcmp(t->ht[i], key)) {
            // found, return canonical instance
            return t->ht[i];
        }
    }
}
```

The caller initializes the iterator to the hash result. This will probably
be out of range, even negative, but that doesn't matter. The iterator
function will turn it into a valid index before use. This detail is key to
*double hashing*: The low bits of the hash tell it where to start, and the
high bits tell it how to step. The hash table size is a power of two, and
the step size is forced to an odd number (via `| 1`), so it's guaranteed
to visit each slot in the table exactly once before restarting. It's
important that the search halts before looping, such as by guaranteeing
the existence of an empty slot (i.e. the "out of memory" check).

Note: The example out of memory check pushes the hash table to the
absolute limit, and in practice you'd want to stop at a smaller load
factor — perhaps even as low as 50% since that's simple and fast.
Otherwise it degrades into a linear search as the table approaches
capacity.

Even if two keys start or land at the same place, they'll quickly diverge
due to differing steps. For awhile I used plain linear probing — i.e.
`step=1` — but double hashing came out ahead every time I benchmarked,
steering me towards this "MSI" construction. Ideally `ht_lookup` would be
placed so that it's inlined — e.g. in the same translation unit — so that
the mask and step are not actually recomputed each iteration.

### Deletion

What about deletion? First, consider how infrequently you delete entries
from a hash table. When was the last time you used `del` on a dictionary
in Python, or `delete` on a `map` in Go? This operation is rarely needed.
However, when you *do* need it, reserve a gravestone value in addition to
the empty value.

```c
static char gravestone[] = "(deleted)";

char *intern(struct ht *t, char *key)
{
    char **dest = 0;
    // ...
        if (!t->ht[i]) {
            // ...
            dest = dest ? dest : &t->ht[i];
            *dest = key;
            return key;
        } else if (t->ht[i] == gravestone) {
            dest = dest ? dest : &t->ht[i];
        } else if (!strcmp(...)) {
            // ...
        }
    // ...
}

char *unintern(struct ht *t, char *key)
{
    // ...
        if (!t->ht[i]) {
            return 0;
        } else if (t->ht[i] == gravestone) {
            // skip over
        } else if (!strcmp(...)) {
            char *old = t->ht[i];
            t->ht[i] = gravestone;
            return old;
        }
    // ...
}
```

When searching, skip over gravestones. Note that gravestones are compared
with `==` (identity), so this does not preclude a string `"(deleted)"`.
When inserting, use the first gravestone found if no entry was found.

### As a database index

Iterating over the example string intern table is simple: Iterate over the
underlying array, skipping empty slots (and maybe gravestones). Entries
will be in a random order rather than, say, insertion order. This is a
useful introductory example, but this isn't where MSI most shines. As
mentioned, it's best when treated like a database index.

Let's take a step back and consider the caller of `intern`. How does it
allocate these strings? Perhaps they're [appended to a buffer][watc], and
`intern` indicates whether or not the string is unique so far.

```c
struct buf {
    // lookup table over the buffer
    struct ht ht;

    // a collection of strings
    int32_t len;
    char buf[BUFLEN];
};
```

Strings are only appended to the buffer when unique, and the hash table
can make that determination in constant time.

```c
char *buf_push(struct buf *b, char *s)
{
    size_t len = strlen(s) + 1;
    if (b->len+len > sizeof(b->buf)) {
        return 0;  // out of memory
    }

    char *candidate = b->buf + buf->len;
    memcpy(candidate, s, len);

    char *result = intern(&b->ht, candidate);
    if (result == candidate) {
        // string is unique, keep it
        b->len += len;
    }
    return result;
}
```

In my first example, `EXP` was fixed. This could be converted into a
dynamic allocation and the hash table resized as needed. Here's a new
constructor, which I'm including since I think it's instructive:

```c
struct ht {
    int32_t len;
    int exp;
    char **ht;
};

static struct ht
ht_new(int exp)
{
    struct ht ht = {0, exp, 0};

    assert(exp >= 0);
    if (exp >= 32) {
        return ht;  // request too large
    }

    ht.ht = calloc((size_t)1<<exp, sizeof(ht.ht[0]));
    return ht;
}
```

If `intern` fails, the hash table can be replaced with a new table twice
as large, and since, like a database index, its contents are entirely
redundant, *the hash table can be discarded and rebuilt from scratch*. The
new and old table don't need to exist simultaneously. Here's a routine to
populate an empty hash table from the buffer:

```c
void buf_rehash(struct buf *b)
{
    assert(b->ht.len == 0);
    for (int32_t off = 0; off < b->len;) {
        char *s = b->buf + off;
        int32_t len = strlen(s) + 1;
        off += len;
        uint64_t h = hash(s, len);
        for (int32_t i = h;;) {
            i = ht_lookup(h, b->ht.exp, i);
            if (!b->ht.ht[i]) {
                b->ht.len++;
                b->ht.ht[i] = s;
                break;
            }
        }
    }
}
```

Note how this iterates in insertion order, which may be useful in other
cases, too. On the rehash it doesn't need to check for existing entries,
as all entries are already known to be unique. Later when `intern` hits
its capacity:

```c
    char *result = intern(&b->ht, candidate);
    if (!result) {
        free(b->ht.ht);
        b->ht = ht_new(ht.exp+1);
        if (!b->ht) {
            return 0;  // out of memory
        }
        buf_rehash(b);
        result = intern(&b->ht, candidate);  // cannot fail
    }
```

I freed and reallocated the table, but it would be trivial to use a
`realloc` instead, unlike the case where the old table *isn't* redundant.

### Multimaps

An MSI hash table is trivially converted into a multimap, a hash table
with multiple values per key. Callers just make one small change: *Don't
stop searching until an empty slot is found*. Each match is an additional
multimap value. The "value array" is stored along the hash table itself,
in insertion order, without additional allocations.

For example, imagine the strings in the string buffer have a namespace
prefix, delimited by a colon, like `city:Austin` and `state:Texas`. We'd
like a fast lookup of all strings under a particular namespace. The
solution is to add another hash table as you would an index to a database
table.

```c
struct buf {
    // ..
    struct ht ns;
    // ..
};
```

When a unique string is appended it's also registered in the namespace
multimap. It doesn't check for an existing key, only for an empty slot,
since it's a multimap:

```c
    // Check outside the loop since it always inserts.
    if (/* ... ns multimap lacks capacity ... */) {
        // ... grow+rehash ns mutilmap ...
    }

    int32_t nslen = strcspn(s, ":") + 1;
    uint64_t h = hash(s, nslen);
    for (int32_t i = h;;) {
        i = ht_lookup(h, b->ns.exp, i);
        if (!b->ns.ht[i]) {
            b->ns.len++;
            b->ns.ht[i] = s;
            break;
        }
    }
```

It includes the `:` as a terminator which simplifies lookups. Here's a
lookup loop to print all strings under a namespace (includes terminal `:`
in the key):

```c
    char *ns = "city:";
    int32_t nslen = strlen(ns);
    // ...

    uint64_t h = hash(ns, nslen);
    for (int32_t i = h;;) {
        i = ht_lookup(h, b->ns.exp, i);
        if (!b->ns.ht[i]) {
            break;
        } else if (!strncmp(b.ns->ht[i], ns, nslen)) {
            puts(b->ns.ht[i]+nslen);
        }
    }
```

An alternative approach to multimaps is to additionally key over a value
subscript. For example, the first city is keyed `{"city", 0}`, the next
`{"city", 1}`, etc. The value subscript could be mixed into the string
hash with an [integer permutation][hp] (more on this below):

```c
uint64_t h = hash64(val_idx ^ hash(s, nslen));
```

The lookup loop would compare both the string and the value subscript, and
stop when it finds a match. The underlying hash table is not truly a
multimap, but rather a plain hash table with a larger key. This requires
extra bookkeeping — tracking individual subscripts and the number of
values per key — but provides constant time random access on the multimap
value array.

### Hash functions

The MSI iterator leaves hashing up to the caller, who has better knowledge
about the input and how to hash it, though this takes a bit of knowledge
of how to build a hash function. The good news is that it's easy, and less
is more. Better to do too little than too much, and a faster, weaker hash
function is worth a few extra collisions.

The first rule is to never lose sight of the goal: The purpose of the hash
function is to uniformly distribute entries over a table. The better you
know and exploit your input, the less you need to do in the hash function.
Sometimes your keys already contain random data, and so your hash function
can be the identity function! For example, if your keys are ["version 4"
UUIDs][v4], don't waste time hashing them, just load a few bytes from the
end as an integer and you're done.

```c
// "Hash" a v4 UUID
uint64_t uuid4_hash(unsigned char uuid[16])
{
    uint64_t h;
    memcpy(&h, uuid+8, 8);
    return h;
}
```

A reasonable start for strings is [FNV-1a][fnv], such as this possible
implementation for my `hash()` function above:

```c
uint64_t hash(char *s, int32_t len)
{
    uint64_t h = 0x100;
    for (int32_t i = 0; i < len; i++) {
        h ^= s[i] & 255;
        h *= 1111111111111111111;
    }
    return h ^ h>>32;
}
```

The hash state is initialized to a *basis*, some arbitrary value. This a
useful place to introduce a seed or hash key. It's best that at least one
bit above the low mix-in bits is set so that it's not trivially stuck at
zero. Above, I've chosen the most trivial basis with reasonable results,
though often I'll use the digits of π.

Next XOR some input into the low bits. This could be a byte, a Unicode
code point, etc. More is better, since otherwise you're stuck doing more
work per unit, the main weakness of FNV-1a. Carefully note the byte mask,
`& 255`, which inhibits sign extension. **Do not mix sign-extended inputs
into FNV-1a** — a widespread implementation mistake.

Multiply by a large, odd random-ish integer. A prime is a reasonable
choice, and I usually pick my favorite prime, shown above: 19 ones in base
10.

Finally, my own touch, an xorshift finalizer. The high bits are much
better mixed than the low bits, so this improves the overall quality.
Though if you take time to benchmark, you might find that this finalizer
isn't necessary. Remember, do *just* enough work to keep the number of
collisions low — not *lowest* — and no more.

If your input is made of integers, or is a short, fixed length, use an
[integer permutation][hp], particularly multiply-xorshift. It takes very
little to get a sufficient distribution. Sometimes one multiplication does
the trick. Fixed-sized, integer-permutation hashes tend to be the fastest,
easily beating fancier SIMD-based hashes, [including AES-NI][bench]. For
example:

```c
// Hash a timestamp-based, version 1 UUID
uint64_t uuid1_hash(unsigned char uuid[16])
{
    uint64_t s[2];
    memcpy(s, uuid, 16);
    s[0] += 0x3243f6a8885a308d;  // digits of pi
    s[0] *= 1111111111111111111;
    s[0] ^= s[0] >> 33;
    s[0] += s[1];
    s[0] *= 1111111111111111111;
    s[0] ^= s[0] >> 33;
    return s[0];
}
```

If I benchmarked this in a real program, I would probably cut it down even
further, deleting hash operations one at a time and measuring the overall
hash table performance. This `memcpy` trick works well with floats, too,
especially packing two single precision floats into one 64-bit integer.

If you ever [hesitate to build a hash table][tar] when the situation
calls, I hope the MSI technique will make the difference next time. I have
more hash table tricks up my sleeve, but since they're not specific to MSI
I'll save them for a future article.

### Benchmarks

There have been objections to my claims about performance, so [I've
assembled some benchmarks][bench]. These demonstrate that:

* AES-NI slower than an integer permutation, at least for short keys.
* A custom, 10-line MSI hash table is easily an order of magnitude faster
  than a typical generic hash table from your language's standard library.
  This isn't because the standard hash table is inferior, but because [it
  wasn't written for your specific problem][context].


[bench]: https://gist.github.com/skeeto/8e7934318560ac739c126749d428a413
[bs]: /blog/2020/10/19/#hash-table-memoization
[c]: https://skeeto.s3.amazonaws.com/share/onward17-essays2.pdf
[context]: https://vimeo.com/644068002
[dh]: https://en.wikipedia.org/wiki/Double_hashing
[exp]: /blog/2022/05/14/
[fnv]: https://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function
[hp]: /blog/2018/07/31/
[list]: /blog/2022/05/22/#inverting-the-tree-links
[oa]: https://en.wikipedia.org/wiki/Open_addressing
[ss]: https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2019/p1428r0.pdf
[tar]: https://mort.coffee/home/tar/
[twosum]: /blog/2023/06/26/
[v4]: https://www.rfc-editor.org/rfc/rfc4122#section-4.4
[watc]: /blog/2022/05/22/
