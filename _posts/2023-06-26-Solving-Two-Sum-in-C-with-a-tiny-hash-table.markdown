---
title: 'Solving "Two Sum" in C with a tiny hash table'
layout: post
date: 2023-06-26T19:38:18Z
tags: [c, go, optimization]
uuid: 5d15318f-6915-4f72-8690-74a84d43d2f7
---

I came across a question: How does one efficiently solve [Two Sum][] in C?
There's a naive quadratic time solution, but also an amortized linear time
solution using a hash table. Without a built-in or standard library hash
table, the latter sounds onerous. However, a [mask-step-index table][msi],
a hash table construction suitable for many problems, requires only a few
lines of code. This approach is useful even when a standard hash table is
available, because by [exploiting the known problem constraints][ctx], it
beats typical generic hash table performance by 1–2 orders of magnitude
([demo][gist]).

The Two Sum exercise, restated:

> Given an integer array and target, return the distinct indices of two
> elements that sum to the target.

In particular, the solution doesn't find elements, but their indices. The
exercise also constrains input ranges — important but easy to overlook:

* 2 <= `count` <= 10<sup>4</sup>
* -10<sup>9</sup> <= `nums[i]` <= 10<sup>9</sup>
* -10<sup>9</sup> <= `target` <= 10<sup>9</sup>

Notably, indices fit in a 16-bit integer with lots of room to spare. In
fact, it will fit in a 14-bit address space (16,384) with still plenty of
overhead. Elements fit in a signed 32-bit integer, and we can add and
subtract elements without overflow, if just barely. The last constraint
isn't redundant, but it's not readily exploitable either.

The naive solution is to linearly search the array for the complement.
With nested loops, it's obviously quadratic time. At 10k elements, we
expect an abysmal 25M comparisons on average.

```c
int16_t count = ...;
int32_t *nums = ...;

for (int16_t i = 0; i < count-1; i++) {
    for (int16_t j = i+1; j < count; j++) {
        if (nums[i]+nums[j] == target) {
            // found
        }
    }
}
```

The `nums` array is "keyed" by index. It would be better to also have the
inverse mapping: key on elements to obtain the `nums` index. Then for each
element we could compute the complement and find its index, if any, using
this second mapping.

The input range is finite, so an inverse map is simple. Allocate an array,
one element per integer in range, and store the index there. However, the
input range is 2 billion, and even with 16-bit indices that's a 4GB array.
Feasible on 64-bit hosts, but wasteful. The exercise is certainly designed
to make it so. This array would be very sparse, at most less than half a
percent of its elements populated. That's a hint: Associative arrays are
far more appropriate for representing such sparse mappings. That is, a
hash table.

Using Go's built-in hash table:

```go
func TwoSumWithMap(nums []int32, target int32) (int, int, bool) {
    seen := make(map[int32]int16)
    for i, num := range nums {
        complement := target - num
        if j, ok := seen[complement]; ok {
            return int(j), i, true
        }
        seen[num] = int16(i)
    }
    return 0, 0, false
}
```

In essence, the hash table folds the sparse 2 billion element array onto a
smaller array, with collision resolution when elements inevitably land in
the same slot. For this exercise, that small array could be as small as
10,000 elements because that's the most we'd ever need to track. For
folding the large key space onto the smaller, we could use modulo. For
collision resolution, we could keep walking the table.

```c
int16_t seen[10000] = {0};

// Find or insert nums[index].
int16_t lookup(int32_t *nums, int16_t index)
{
    int i = nums[index] % 10000;
    for (;;) {
        int16_t j = seen[i] - 1;  // unbias
        if (j < 0) {  // empty slot
            seen[i] = index + 1;  // insert biased index
            return -1;
        } else if (nums[j] == nums[index]) {
            return j;  // match found
        }
        i = (i + 1) % 10000;  // keep looking
    }
}
```

Take note of a few details:

1. An empty slot is zero, and an empty table is a zero-initialized array.
   Since zero is a valid value, and all values are non-negative, it biases
   values by 1 in the table.

2. The `nums` array is part of the table structure, necessary for lookups.
   **The two mappings — element-by-index and index-by-element — share
   structure.**

3. It uses *open addressing* with *linear probing*, and so walks the table
   until it either either finds the element or hits an empty slot.

4. The "hash" function is modulo. If inputs are not random, they'll tend
   to bunch up in the table. Combined with linear probing makes for lots
   of collisions. For the worst case, imagine sequentially ordered inputs.

5. Sometimes the table will almost completely fill, and lookups will be no
   better than the linear scans of the naive solution.

6. Most subtle of all: This hash table is not enough for the exercise. The
   keyed-on element may not even be in `nums`, and when lookup fails, that
   element is not inserted in the table. Instead, a different element is
   inserted. The conventional solution has at least two hash table
   lookups. **In the Go code, it's `seen[complement]` for lookups and
   `seen[num]` for inserts.**

To solve (4) we'll use a hash function to more uniformly distribute
elements in the table. We'll also probe the table in a random-ish order
that depends on the key. In practice there will be little bunching even
for non-random inputs.

To solve (5) we'll use a larger table: 2<sup>14</sup> or 16,384 elements.
This has breathing room, and with a power of two we can use a fast mask
instead of a slow division (though in practice, compilers usually
implement division by a constant denominator with modular multiplication).

To solve (6) we'll key complements together under the same key. It looks
for the complement, but on failure it inserts the current element in the
empty slot. In other words, **this solution will only need a single hash
table lookup per element!**

Laying down some groundwork:

```c
typedef struct {
    int16_t i, j;
    _Bool ok;
} TwoSum;

TwoSum twosum(int32_t *nums, int16_t count, int32_t target)
{
    TwoSum r = {0};
    int16_t seen[1<<14] = {0};
    for (int16_t n = 0; n < count; n++) {
        // ...
    }
    return r;
}
```

The `seen` array is a 32KiB hash table large enough for all inputs, small
enough that it can be a local variable. In the loop:

```c
        int32_t complement = target - nums[n];
        int32_t key = complement>nums[n] ? complement : nums[n];
        uint32_t hash = key * 489183053u;
        unsigned mask = sizeof(seen)/sizeof(*seen) - 1;
        unsigned step = hash>>13 | 1;
```

Compute the complement, then apply a "max" operation to derive a key. Any
commutative operation works, though obviously addition would be a poor
choice. XOR is similar enough to cause many collisions. Multiplication
works well, and is probably better if the ternary produces a branch.

The hash function is multiplication with [a randomly-chosen prime][prime].
As we'll see in a moment, `step` will also add-shift the hash before use.
The initial index will be the bottom 14 bits of this hash. For `step`,
recall from the MSI article that it must be odd so that every slot is
eventually probed. I shift out 13 bits and then override the 14th bit, so
`step` effectively skips over the 14 bits used for the initial table
index.

I used `unsigned` because I don't really care about the width of the hash
table index, but more importantly, I want defined overflow from all the
bit twiddling, even in the face of implicit promotion. As a bonus, it can
help in reasoning about indirection: `seen` indices are `unsigned`, `nums`
indices are `int16_t`.

```c
        for (unsigned i = hash;;) {
            i = (i + step) & mask;
            int16_t j = seen[i] - 1;  // unbias
            if (j < 0) {
                seen[i] = n + 1;  // bias and insert
                break;
            } else if (nums[j] == complement) {
                r.i = j;
                r.j = n;
                r.ok = 1;
                return r;
            }
        }
```

The step is added before using the index the first time, helping to
scatter the start point and reduce collisions. If it's an empty slot,
insert the *current* element, not the complement — which wouldn't be
possible anyway. Unlike conventional solutions, this doesn't require
another hash and lookup. If it finds the complement, problem solved,
otherwise keep going.

Putting it all together, it's only slightly longer than solutions using a
generic hash table:

```c
TwoSum twosum(int32_t *nums, int16_t count, int32_t target)
{
    TwoSum r = {0};
    int16_t seen[1<<14] = {0};
    for (int16_t n = 0; n < count; n++) {
        int32_t complement = target - nums[n];
        int32_t key = complement>nums[n] ? complement : nums[n];
        uint32_t hash = key * 489183053u;
        unsigned mask = sizeof(seen)/sizeof(*seen) - 1;
        unsigned step = hash>>13 | 1;
        for (unsigned i = hash;;) {
            i = (i + step) & mask;
            int16_t j = seen[i] - 1;  // unbias
            if (j < 0) {
                seen[i] = n + 1;  // bias and insert
                break;
            } else if (nums[j] == complement) {
                r.i = j;
                r.j = n;
                r.ok = 1;
                return r;
            }
        }
    }
    return r;
}
```

Applying this technique to Go:

```go
func TwoSumWithBespoke(nums []int32, target int32) (int, int, bool) {
    var seen [1 << 14]int16
    for n, num := range nums {
        complement := target - num
        hash := int(num * complement * 489183053)
        mask := len(seen) - 1
        step := hash>>13 | 1
        for i := hash; ; {
            i = (i + step) & mask
            j := int(seen[i] - 1) // unbias
            if j < 0 {
                seen[i] = int16(n) + 1 // bias
                break
            } else if nums[j] == complement {
                return j, n, true
            }
        }
    }
    return 0, 0, false
}
```

With Go 1.20 this is an order of magnitude faster than `map[int32]int16`,
which isn't surprising. I used multiplication as the key operator because,
in my first take, Go produced a branch for the "max" operation — at a 25%
performance penalty on random inputs.

A full-featured, generic hash table may be overkill for your problem, and
a bit of hashed indexing with collision resolution over a small array
might be sufficient. The problem constraints might open up such shortcuts.


[Two Sum]: https://leetcode.com/problems/two-sum/
[ctx]: https://vimeo.com/644068002
[gist]: https://gist.github.com/skeeto/7119cf683662deae717c0d4e79ebf605
[msi]: /blog/2022/08/08/
[prime]: /blog/2019/11/19/