---
title: Inspiration from Data-dependent Rotations
layout: post
date: 2018-02-07T23:59:59Z
tags: [c, crypto, meta]
uuid: 917b72f1-3aad-3af3-075a-a4b0a6eb8a4d
---

This article is an expanded email I wrote in response to a question
from Frank Muller. He asked how I arrived at my solution to a
[branchless UTF-8 decoder][utf8]:

> I mean, when you started, I’m pretty the initial solution was using
> branches, right? Then, you’ve applied some techniques to eliminate
> them.

A bottom-up approach that begins with branches and then proceeds to
eliminate them one at a time sounds like a plausible story. However,
this story is the inverse of how it actually played out. It began when I
noticed a branchless decoder could probably be done, then I put together
the pieces one at a time without introducing any branches. But what
sparked that initial idea?

The two prior posts reveal my train of thought at the time: [a look at
the Blowfish cipher][bf] and [a 64-bit PRNG shootout][prng]. My
layman's study of Blowfish was actually part of an examination of a
number of different block ciphers. For example, I also read the NSA's
[Speck and Simon paper][ss] and then [implemented the 128/128 variant
of Speck][speck] — a 128-bit key and 128-bit block. I didn't take the
time to write an article about it, but note how the entire cipher —
key schedule, encryption, and decryption — is just 40 lines of code:

```c
struct speck {
    uint64_t k[32];
};

void
speck_init(struct speck *ctx, uint64_t x, uint64_t y)
{
    ctx->k[0] = y;
    for (uint64_t i = 0; i < 31; i++) {
        x = (x >> 8) | (x << 56);
        x += y;
        x ^= i;
        y = (y << 3) | (y >> 61);
        y ^= x;
        ctx->k[i + 1] = y;
    }
}

void
speck_encrypt(const struct speck *ctx, uint64_t *x, uint64_t *y)
{
    for (int i = 0; i < 32; i++) {
        *x = (*x >> 8) | (*x << 56);
        *x += *y;
        *x ^= ctx->k[i];
        *y = (*y << 3) | (*y >> 61);
        *y ^= *x;
    }
}

static void
speck_decrypt(const struct speck *ctx, uint64_t *x, uint64_t *y)
{
    for (int i = 0; i < 32; i++) {
        *y ^= *x;
        *y = (*y >> 3) | (*y << 61);
        *x ^= ctx->k[31 - i];
        *x -= *y;
        *x = (*x << 8) | (*x >> 56);
    }
}
```

Isn't that just beautiful? It's so tiny and fast. Other than the
not-very-arbitrary selection of 32 rounds, and the use of 3-bit and
8-bit rotations, there are no magic values. One could fairly
reasonably commit this cipher to memory if necessary, similar to the
late RC4. Speck is probably my favorite block cipher right now,
*except* that I couldn't figure out the key schedule for any of the
other key/block size variants.

Another cipher I studied, though in less depth, was [RC5][rc5] (1994),
a block cipher by (obviously) Ron Rivest. The most novel part of RC5
is its use of data dependent rotations. This was a very deliberate
decision, and the paper makes this clear:

> RC5 should *highlight the use of data-dependent rotations*, and
> encourage the assessment of the cryptographic strength data-dependent
> rotations can provide.

What's a data-dependent rotation. In the Speck cipher shown above,
notice how the right-hand side of all the rotation operations is a
constant (3, 8, 56, and 61). Suppose that these operands were not
constant, instead they were based on some part of the value of the
block:

```c
    int r = *y & 0x0f;
    *x = (*x >> r) | (*x << (64 - r));
```

Such "random" rotations "frustrate differential cryptanalysis" according
to the paper, increasing the strength of the cipher.

Another algorithm that uses data-dependent shift is the [PCG family of
PRNGs][pcg]. Honestly, the data-dependent "permutation" shift is *the*
defining characteristic of PCG. As a reminder, here's the simplified PCG
from my shootout:

```c
uint32_t
spcg32(uint64_t s[1])
{
    uint64_t m = 0x9b60933458e17d7d;
    uint64_t a = 0xd737232eeccdf7ed;
    *s = *s * m + a;
    int shift = 29 - (*s >> 61);
    return *s >> shift;
}
```

Notice how the final shift depends on the high order bits of the PRNG
state. (This one weird trick from Melissa O'Neil will significantly
improve your PRNG. Xorshift experts hate her.)

I think this raises a really interesting question: Why did it take until
2014 for someone to apply a data-dependent shift to a PRNG? Similarly,
why are [data-dependent rotations not used in many ciphers][ddr]?

My own theory is that this is because many older instruction set
architectures can't perform data-dependent shift operations efficiently.

Many instruction sets only have a fixed shift (e.g. 1-bit), or can
only shift by an immediate (e.g. a constant). In these cases, a
data-dependent shift would require a loop. These loops would be a ripe
source of side channel attacks in ciphers due to the difficultly of
making them operate in constant time. It would also be relatively slow
for video game PRNGs, which often needed to run on constrained
hardware with limited instruction sets. For example, the 6502 (Atari,
Apple II, NES, Commodore 64) and the Z80 (too many to list) can only
shift/rotate one bit at a time.

Even on an architecture with an instruction for data-dependent shifts,
such as the x86, those shifts will be slower than constant shifts, at
least in part due to the additional data dependency.

It turns out there are also some patent issues (ex. [1][p1], [2][p2]).
Fortunately most of these patents have now expired, and one in
particular is set to expire this June. I still like my theory better.

### To branchless decoding

So I was thinking about data-dependent shifts, and I had also noticed I
could trivially check the length of a UTF-8 code point using a small
lookup table — the first step in my decoder. What if I combined these: a
data-dependent shift based on a table lookup. This would become the last
step in my decoder. The idea for a branchless UTF-8 decoder was
essentially borne out of connecting these two thoughts, and then filling
in the middle.


[bf]: /blog/2017/09/15/
[ddr]: https://crypto.stackexchange.com/q/20325
[p1]: https://www.google.com/patents/US5724428
[p2]: https://www.google.com/patents/US6269163
[pcg]: http://www.pcg-random.org/
[prng]: /blog/2017/09/21/
[rc5]: http://people.csail.mit.edu/rivest/Rivest-rc5rev.pdf
[speck]: https://github.com/skeeto/scratch/tree/master/speck
[ss]: http://eprint.iacr.org/2013/404.pdf
[utf8]: /blog/2017/10/06/
