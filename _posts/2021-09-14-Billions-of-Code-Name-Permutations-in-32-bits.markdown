---
title: Billions of Code Name Permutations in 32 bits
layout: post
date: 2021-09-14T21:06:59Z
tags: [c, crypto, optimization]
uuid: bc17a779-bee1-4a60-80d1-5c5cfd8fd638
---

My friend over at Possibly Wrong [created a code name generator][pw]. By
coincidence I happened to be thinking about code names myself while
recently replaying [*XCOM: Enemy Within*][ew] (2012/2013). The game
generates a random code name for each mission, and I wondered how often it
repeats. The [UFOpaedia page on the topic][ufo] gives the word lists: 53
adjectives and 76 nouns, for a total of 4028 possible code names. A
typical game has around 60 missions, and if code names are generated
naively on the fly, then per the birthday paradox around half of all games
will see a repeated mission code name! Fortunately this is easy to avoid,
and the particular configuration here lends itself to an interesting
implementation.

Mission code names are built using "*adjective* *noun*". Some examples
from the game's word list:

* Fading Hammer
* Fallen Jester
* Hidden Crown

To generate a code name, we could select a random adjective and a random
noun, but as discussed it wouldn't take long for a collision. The naive
approach is to keep a database of previously-generated names, and to
consult this database when generating new names. That works, but there's
an even better solution: use a random permutation. Done well, we don't
need to keep track of previous names, and the generator won't repeat until
it's exhausted all possibilities.

Further, the total number of possible code names, 4028, is suspiciously
shy of 4,096, a power of two (`2**12`). That makes designing and
implementing an efficient permutation that much easier.

### A linear congruential generator

A classic, obvious solution is a [linear congruential generator][lcg]
(LCG). A full-period, 12-bit LCG is nothing more than a permutation of the
numbers 0 to 4,095. When generating names, we can skip over the extra 68
values and pretend it's a permutation of 4,028 elements. An LCG is
constructed like so:

    f(n) = (f(n-1)*A + C) % M

Typically the seed is used for `f(0)`. M is selected based on the problem
space or implementation efficiency, and usually a power of two. In this
case it will be 4,096. Then there are some rules for choosing A and C.

Simply choosing a random `f(0)` per game isn't great. The code name order
will always be the same, and we're only choosing where in the cycle to
start. It would be better to vary the permutation itself, which we can do
by also choosing unique A and C constants per game.

Choosing C is easy: It must be relatively prime with M, i.e. it must be
odd. Since it's addition modulo M, there's no reason to choose `C >= M`
since the results are identical to a smaller C. If we think of C as a
12-bit integer, 1 bit is locked in, and the other 11 bits are free to
vary:

    xxxxxxxxxxx1

Choosing A is more complicated: must be odd, `A-1` must be divisible by 4,
and `A-1` should be divisible by 8 (better results). Again, thinking of
this in terms of a 12-bit number, this locks in 3 bits and leaves 9 bits
free:

    xxxxxxxxx101

This ensures all the *must* and *should* properties of A.

Finally `0 <= f(0) < M`. Because of modular arithmetic larger, values are
redundant, and all possible values are valid since the LCG, being
full-period, will cycle through all of them. This is just choosing the
starting point in a particular permutation cycle. As a 12-bit number, all
12 bits are free:

    xxxxxxxxxxxx

That's `9 + 11 + 12 = 32` free bits to fill randomly: again, how
incredibly convenient! Every 32-bit integer defines some unique code name
permutation… *almost*. Any 32-bit descriptor where `f(0) >= 4028` will
collide with at least one other due to skipping, and so around 1.7% of the
state space is redundant. A small loss that should shrink with slightly
better word list planning. I don't think anyone will notice.

### Slice and dice

[I love compact state machines][sm], and this is an opportunity to put one
to good use. My code name generator will be just one function:

```c
uint32_t codename(uint32_t state, char *buf);
```

This takes one of those 32-bit permutation descriptors, writes the first
code name to `buf`, and returns a descriptor for another permutation that
starts with the next name. All we have to do is keep track of that 32-bit
number and we'll never need to worry about repeating code names until all
have been exhausted.

First, lets extract A, C, and `f(0)`, which I'm calling S. The low bits
are A, middle bits are C, and high bits are S. Note the OR with 1 and 5 to
lock in the hard-set bits.

```c
long a = (state <<  3 | 5) & 0xfff;  //  9 bits
long c = (state >>  8 | 1) & 0xfff;  // 11 bits
long s =  state >> 20;               // 12 bits
```

Next iterate the LCG until we have a number in range:

```c
do {
    s = (s*a + c) & 0xfff;
} while (s >= 4028);
```

Once we have an appropriate LCG state, compute the adjective/noun indexes
and build a code name:

```c
int i = s % 53;
int j = s / 53;
sprintf(buf, "%s %s", adjvs[i], nouns[j]);
```

Finally assemble the next 32-bit state. Since A and C don't change, these
are passed through while the old S is masked out and replaced with the new
S.

```c
return (state & 0xfffff) | (uint32_t)s<<20;
```

Putting it all together:

```c
static const char *adjvs[] = { /* ... */ };
static const char *nouns[] = { /* ... */ };

uint32_t codename(uint32_t state, char *buf)
{
    long a = (state <<  3 | 5) & 0xfff;  //  9 bits
    long c = (state >>  8 | 1) & 0xfff;  // 11 bits
    long s =  state >> 20;               // 12 bits

    do {
        s = (s*a + c) & 0xfff;
    } while (s >= COUNTOF(adjvs)*COUNTOF(nouns));

    int i = s % COUNTOF(adjvs);
    int j = s / COUNTOF(adjvs);
    sprintf(buf, "%s %s", adjvs[i], nouns[j]);
    return (state & 0xfffff) | (uint32_t)s<<20;
}
```

The caller just needs to generate an initial 32-bit integer. Any 32-bit
integer is valid — even zero — so this could just be, say, the unix epoch
(`time(2)`), but adjacent values will have similar-ish permutations. I
intentionally placed S in the high bits, which are least likely to vary,
since it only affects where the cycle begins, while A and C have a much
more dramatic impact and so are placed at more variable locations.

Regardless, it would be better to hash such an input so that adjacent time
values map to distant states. It also helps hide poorer (less random)
choices for A multipliers. I happen to have [designed some great functions
for exactly this purpose][hash]. Here's one of my best:

```c
static uint32_t
hash32(uint32_t x)
{
    x += 0x3243f6a8U; x ^= x >> 15;
    x *= 0xd168aaadU; x ^= x >> 15;
    x *= 0xaf723597U; x ^= x >> 15;
    return x;
}
```

This would be perfectly reasonable for generating all possible names in a
random order:

```c
uint32_t state = hash32(time(0));
for (int i = 0; i < 4028; i++) {
    char buf[32];
    state = codename(state, buf);
    puts(buf);
}
```

To further help cover up poorer A multipliers, it's better for the word
list to be pre-shuffled in its static storage. If that underlying order
happens to show through, at least it will be less obvious (i.e. not in
alphabetical order). Shuffling the string list in my source is just a few
keystrokes in Vim, so this is easy enough.

### Robustness

If you're set on making the `codename` function easier to use such that
consumers don't need to think about hashes, you could "encode" and
"decode" the descriptor going in an out of the function:

```c
uint32_t codename(uint32_t state, char *buf)
{
    state += 0x3243f6a8U; state ^= state >> 17;
    state *= 0x9e485565U; state ^= state >> 16;
    state *= 0xef1d6b47U; state ^= state >> 16;

    // ...

    state = (state & 0xfffff) | (uint32_t)s<<20;
    state ^= state >> 16; state *= 0xeb00ce77U;
    state ^= state >> 16; state *= 0x88ccd46dU;
    state ^= state >> 17; state -= 0x3243f6a8U;
    return state;
}
```

This permutes the state coming in, and reverses that permutation on the
way out (read: inverse hash). This breaks up similar starting points.

### A random-access code name permutation

Of course this isn't the only way to build a permutation. I recently
picked up another trick: [Kensler permutation][kensler]. The key insight
is cycle-walking, allowing for random-access to a permutation of a smaller
domain (e.g. 4,028 elements) through permutation of a larger domain (e.g.
4096 elements).

Here's such a code name generator built around a bespoke 12-bit
xorshift-multiply permutation. I used 4 "rounds" since xorshift-multiply
is less effective the smaller the permutation.

```c
// Generate the nth code name for this seed.
void codename_n(char *buf, uint32_t seed, int n)
{
    uint32_t i = n;
    do {
        i ^= i >> 6; i ^= seed >>  0; i *= 0x325; i &= 0xfff;
        i ^= i >> 6; i ^= seed >>  8; i *= 0x3f5; i &= 0xfff;
        i ^= i >> 6; i ^= seed >> 16; i *= 0xa89; i &= 0xfff;
        i ^= i >> 6; i ^= seed >> 24; i *= 0x85b; i &= 0xfff;
        i ^= i >> 6;
    } while (i >= COUNTOF(adjvs)*COUNTOF(nouns));

    int a = i % COUNTOF(adjvs);
    int b = i / COUNTOF(adjvs);
    snprintf(buf, 22, "%s %s", adjvs[a], nouns[b]);
}
```

While this is more flexible, avoids poorer permutations, and doesn't have
state space collisions, I still have a soft spot for my LCG-based state
machine generator.

### Source code

You can find the complete, working source code with both generators here:
[**`codename.c`**][c]. I used [real US Secret Service code names][ss] for
my word list. Some sample outputs:

* PLASTIC HUMMINGBIRD
* BLACK VENUS
* SILENT SUNBURN
* BRONZE AUTHOR
* FADING MARVEL


[c]: https://github.com/skeeto/scratch/tree/master/misc/codename.c
[ew]: https://en.wikipedia.org/wiki/XCOM:_Enemy_Within
[hash]: /blog/2018/07/31/
[kensler]: https://andrew-helmer.github.io/permute/
[lcg]: /blog/2019/11/19/
[pw]: https://possiblywrong.wordpress.com/2021/09/13/code-name-generator/
[sm]: /blog/2020/12/31/
[ss]: https://en.wikipedia.org/wiki/Secret_Service_code_name
[ufo]: https://www.ufopaedia.org/index.php/Mission_Names_(EU2012)
