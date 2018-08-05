---
title: Finding the Best 64-bit Simulation PRNG
layout: post
date: 2017-09-21T21:25:00Z
tags: [c, compsci, x86, crypto, optimization]
uuid: 637af55f-6e33-31e5-25fa-edb590a16d44
---

**August 2018 Update**: *xoroshiro128+ fails [PractRand][practrand] very
badly. Since this article was published, its authors have supplanted it
with **xoshiro256\*\***. It has essentially the same performance, but
better statistical properties. xoshiro256\*\* is now my preferred PRNG.*

I use pseudo-random number generators (PRNGs) a whole lot. They're an
essential component in lots of algorithms and processes.

* **Monte Carlo simulations**, where PRNGs are used to [compute
  numeric estimates][mc] for problems that are difficult or impossible
  to solve analytically.

* [**Monte Carlo tree search AI**][mcts], where massive numbers of games
  are played out randomly in search of an optimal move. This is a
  specific application of the last item.

* [**Genetic algorithms**][ga], where a PRNG creates the initial
  population, and then later guides in mutation and breeding of selected
  solutions.

* [**Cryptography**][crypto], where a cryptographically-secure PRNGs
  (CSPRNGs) produce output that is predictable for recipients who know
  a particular secret, but not for anyone else. This article is only
  concerned with plain PRNGs.

For the first three "simulation" uses, there are two primary factors
that drive the selection of a PRNG. These factors can be at odds with
each other:

1. The PRNG should be *very* fast. The application should spend its
   time running the actual algorithms, not generating random numbers.

2. PRNG output should have robust statistical qualities. Bits should
   appear to be independent and the output should closely follow the
   desired distribution. Poor quality output will negatively effect
   the algorithms using it. Also just as important is [how you use
   it][dbl], but this article will focus only on generating bits.

In other situations, such as in cryptography or online gambling,
another important property is that an observer can't learn anything
meaningful about the PRNG's internal state from its output. For the
three simulation cases I care about, this is not a concern. Only speed
and quality properties matter.

Depending on the programming language, the PRNGs found in various
standard libraries may be of dubious quality. They're slower than they
need to be, or have poorer quality than required. In some cases, such
as `rand()` in C, the algorithm isn't specified, and you can't rely on
it for anything outside of trivial examples. In other cases the
algorithm and behavior *is* specified, but you could easily do better
yourself.

My preference is to BYOPRNG: *Bring Your Own Pseudo-random Number
Generator*. You get reliable, identical output everywhere. Also, in
the case of C and C++ — and if you do it right — by embedding the PRNG
in your project, it will get inlined and unrolled, making it far more
efficient than a [slow call into a dynamic library][plt].

A fast PRNG is going to be small, making it a great candidate for
embedding as, say, a header library. That leaves just one important
question, "Can the PRNG be small *and* have high quality output?" In
the 21st century, the answer to this question is an emphatic "yes!"

For the past few years my main go to for a drop-in PRNG has been
[xorshift\*][xs]. The body of the function is 6 lines of C, and its
entire state is a 64-bit integer, directly seeded. However, there are a
number of choices here, including other variants of Xorshift. How do I
know which one is best? The only way to know is to test it, hence my
64-bit PRNG shootout:

* [**64-bit PRNG Shootout**](https://github.com/skeeto/prng64-shootout)

Sure, there [are other such shootouts][other], but they're all missing
something I want to measure. I also want to test in an environment very
close to how I'd use these PRNGs myself.

### Shootout results

Before getting into the details of the benchmark and each generator,
here are the results. These tests were run on an i7-6700 (Skylake)
running Linux 4.9.0.

                                   Speed (MB/s)
    PRNG           FAIL  WEAK  gcc-6.3.0 clang-3.8.1
    ------------------------------------------------
    baseline          X     X      15000       13100
    blowfishcbc16     0     1        169         157
    blowfishcbc4      0     5        725         676
    blowfishctr16     1     3        187         184
    blowfishctr4      1     5        890        1000
    mt64              1     7       1700        1970
    pcg64             0     4       4150        3290
    rc4               0     5        366         185
    spcg64            0     8       5140        4960
    xoroshiro128+     0     6       8100        7720
    xorshift128+      0     2       7660        6530
    xorshift64*       0     3       4990        5060

And the actual dieharder outputs:

* [shootout-results.zip](/download/shootout-results.zip)

**The clear winner is [xoroshiro128+][xoro]**, with a function body of
just 7 lines of C. It's clearly the fastest, and the output had no
observed statistical failures. However, that's not the whole story. A
couple of the other PRNGS have advantages that situationally makes
them better suited than xoroshiro128+. I'll go over these in the
discussion below.

These two versions of GCC and Clang were chosen because these are the
latest available in Debian 9 "Stretch." It's easy to build and run the
benchmark yourself if you want to try a different version.

### Speed benchmark

In the speed benchmark, the PRNG is initialized, a 1-second `alarm(1)`
is set, then the PRNG fills a large `volatile` buffer of 64-bit unsigned
integers again and again as quickly as possible until the alarm fires.
The amount of memory written is measured as the PRNG's speed.

The baseline "PRNG" writes zeros into the buffer. This represents the
absolute speed limit that no PRNG can exceed.

The purpose for making the buffer `volatile` is to force the entire
output to actually be "consumed" as far as the compiler is concerned.
Otherwise the compiler plays nasty tricks to make the program do as
little work as possible. Another way to deal with this would be to
`write(2)` buffer, but of course I didn't want to introduce
unnecessary I/O into a benchmark.

On Linux, SIGALRM was impressively consistent between runs, meaning it
was perfectly suitable for this benchmark. To account for any process
scheduling wonkiness, the bench mark was run 8 times and only the
fastest time was kept.

The SIGALRM handler sets a `volatile` global variable that tells the
generator to stop. The PRNG call was unrolled 8 times to avoid the
alarm check from significantly impacting the benchmark. You can see
the effect for yourself by changing `UNROLL` to 1 (i.e. "don't
unroll") in the code. Unrolling beyond 8 times had no measurable
effect to my tests.

Due to the PRNGs being inlined, this unrolling makes the benchmark
less realistic, and it shows in the results. Using `volatile` for the
buffer helped to counter this effect and reground the results. This is
a fuzzy problem, and there's not really any way to avoid it, but I
will also discuss this below.

### Statistical benchmark

To measure the statistical quality of each PRNG — mostly as a sanity
check — the raw binary output was run through [dieharder][dh] 3.31.1:

    prng | dieharder -g200 -a -m4

This statistical analysis has no timing characteristics and the
results should be the same everywhere. You would only need to re-run
it to test with a different version of dieharder, or a different
analysis tool.

There's not much information to glean from this part of the shootout.
It mostly confirms that all of these PRNGs would work fine for
simulation purposes. The WEAK results are not very significant and is
only useful for breaking ties. Even a true RNG will get some WEAK
results. For example, the [x86 RDRAND][rdrand] instruction (not
included in actual shootout) got 7 WEAK results in my tests.

The FAIL results are more significant, but a single failure doesn't
mean much. A non-failing PRNG should be preferred to an otherwise
equal PRNG with a failure.

### Individual PRNGs

Admittedly the definition for "64-bit PRNG" is rather vague. My high
performance targets are all 64-bit platforms, so the highest PRNG
throughput will be built on 64-bit operations ([if not wider][simd]).
The original plan was to focus on PRNGs built from 64-bit operations.

Curiosity got the best of me, so I included some PRNGs that don't use
*any* 64-bit operations. I just wanted to see how they stacked up.

#### Blowfish

One of the reasons I [wrote a Blowfish implementation][bf] was to
evaluate its performance and statistical qualities, so naturally I
included it in the benchmark. It only uses 32-bit addition and 32-bit
XOR. It has a 64-bit block size, so it's naturally producing a 64-bit
integer. There are two different properties that combine to make four
variants in the benchmark: number of rounds and block mode.

Blowfish normally uses 16 rounds. This makes it a lot slower than a
non-cryptographic PRNG but gives it a *security margin*. I don't care
about the security margin, so I included a 4-round variant. At
expected, it's about four times faster.

The other feature I tested is the block mode: [Cipher Block
Chaining][cbc] (CBC) versus [Counter][ctr] (CTR) mode. In CBC mode it
encrypts zeros as plaintext. This just means it's encrypting its last
output. The ciphertext is the PRNG's output.

In CTR mode the PRNG is encrypting a 64-bit counter. It's 11% faster
than CBC in the 16-round variant and 23% faster in the 4-round variant.
The reason is simple, and it's in part an artifact of unrolling the
generation loop in the benchmark.

In CBC mode, each output depends on the previous, but in CTR mode all
blocks are independent. Work can begin on the next output before the
previous output is complete. The x86 architecture uses out-of-order
execution to achieve many of its performance gains: Instructions may
be executed in a different order than they appear in the program,
though their observable effects must [generally be ordered
correctly][order]. Breaking dependencies between instructions allows
out-of-order execution to be fully exercised. It also gives the
compiler more freedom in instruction scheduling, though the `volatile`
accesses cannot be reordered with respect to each other (hence it
helping to reground the benchmark).

Statistically, the 4-round cipher was not significantly worse than the
16-round cipher. For simulation purposes the 4-round cipher would be
perfectly sufficient, though xoroshiro128+ is still more than 9 times
faster without sacrificing quality.

On the other hand, CTR mode had a single failure in both the 4-round
(dab\_filltree2) and 16-round (dab\_filltree) variants. At least for
Blowfish, is there something that makes CTR mode less suitable than CBC
mode as a PRNG?

In the end Blowfish is too slow and too complicated to serve as a
simulation PRNG. This was entirely expected, but it's interesting to see
how it stacks up.

#### Mersenne Twister (MT19937-64)

Nobody ever got fired for choosing [Mersenne Twister][mt]. It's the
classical choice for simulations, and is still usually recommended to
this day. However, Mersenne Twister's best days are behind it. I
tested the 64-bit variant, MT19937-64, and there are four problems:

* It's between 1/4 and 1/5 the speed of xoroshiro128+.

* It's got a large state: 2,500 bytes. Versus xoroshiro128+'s 16 bytes.

* Its implementation is three times bigger than xoroshiro128+, and much
  more complicated.

* It had one statistical failure (dab\_filltree2).

Curiously my implementation is 16% faster with Clang than GCC. Since
Mersenne Twister isn't seriously in the running, I didn't take time to
dig into why.

Ultimately I would never choose Mersenne Twister for anything anymore.
This was also not surprising.

#### Permuted Congruential Generator (PCG)

The [Permuted Congruential Generator][pcg] (PCG) has some really
interesting history behind it, particularly with its somewhat [unusual
paper][pcgp], controversial for both its excessive length (58 pages)
and informal style. It's in close competition with Xorshift and
xoroshiro128+. I was really interested in seeing how it stacked up.

PCG is really just a Linear Congruential Generator (LCG) that doesn't
output the lowest bits (too poor quality), and has an extra
permutation step to make up for the LCG's other weaknesses. I included
two variants in my benchmark: the official PCG and a "simplified" PCG
(sPCG) with a simple permutation step. sPCG is just the first PCG
presented in the paper (34 pages in!).

Here's essentially what the simplified version looks like:

~~~c
uint32_t
spcg32(uint64_t s[1])
{
    uint64_t m = 0x9b60933458e17d7d;
    uint64_t a = 0xd737232eeccdf7ed;
    *s = *s * m + a;
    int shift = 29 - (*s >> 61);
    return *s >> shift;
}
~~~

The third line with the modular multiplication and addition is the
LCG. The bit shift is the permutation. This PCG uses the most
significant three bits of the result to determine which 32 bits to
output. That's *the* novel component of PCG.

The two constants are entirely my own devising. It's two 64-bit primes
generated using Emacs' `M-x calc`: `2 64 ^ k r k n k p k p k p`.

Heck, that's so simple that I could easily memorize this and code it
from scratch on demand. Key takeaway: This is **one way that PCG is
situationally better than xoroshiro128+**. In a pinch I could use Emacs
to generate a couple of primes and code the rest from memory. If you
participate in coding competitions, take note.

However, you probably also noticed PCG only generates 32-bit integers
despite using 64-bit operations. To properly generate a 64-bit value
we'd need 128-bit operations, which would need to be implemented in
software.

Instead, I doubled up on everything to run two PRNGs in parallel.
Despite the doubling in state size, the period doesn't get any larger
since the PRNGs don't interact with each other. We get something in
return, though. Remember what I said about out-of-order execution?
Except for the last step combining their results, since the two PRNGs
are independent, doubling up shouldn't *quite* halve the performance,
particularly with the benchmark loop unrolling business.

Here's my doubled-up version:

~~~c
uint64_t
spcg64(uint64_t s[2])
{
    uint64_t m  = 0x9b60933458e17d7d;
    uint64_t a0 = 0xd737232eeccdf7ed;
    uint64_t a1 = 0x8b260b70b8e98891;
    uint64_t p0 = s[0];
    uint64_t p1 = s[1];
    s[0] = p0 * m + a0;
    s[1] = p1 * m + a1;
    int r0 = 29 - (p0 >> 61);
    int r1 = 29 - (p1 >> 61);
    uint64_t high = p0 >> r0;
    uint32_t low  = p1 >> r1;
    return (high << 32) | low;
}
~~~

The "full" PCG has some extra shifts that makes it 25% (GCC) to 50%
(Clang) slower than the "simplified" PCG, but it does halve the WEAK
results.

In this 64-bit form, both are significantly slower than xoroshiro128+.
However, if you find yourself only needing 32 bits at a time (always
throwing away the high 32 bits from a 64-bit PRNG), 32-bit PCG is
faster than using xoroshiro128+ and throwing away half its output.

#### RC4

This is another CSPRNG where I was curious how it would stack up. It
only uses 8-bit operations, and it generates a 64-bit integer one byte
at a time. It's the slowest after 16-round Blowfish and generally not
useful as a simulation PRNG.

#### xoroshiro128+

xoroshiro128+ is the obvious winner in this benchmark and it seems to be
the best 64-bit simulation PRNG available. If you need a fast, quality
PRNG, just drop these 11 lines into your C or C++ program:

~~~c
uint64_t
xoroshiro128plus(uint64_t s[2])
{
    uint64_t s0 = s[0];
    uint64_t s1 = s[1];
    uint64_t result = s0 + s1;
    s1 ^= s0;
    s[0] = ((s0 << 55) | (s0 >> 9)) ^ s1 ^ (s1 << 14);
    s[1] = (s1 << 36) | (s1 >> 28);
    return result;
}
~~~

There's one important caveat: **That 16-byte state must be
well-seeded.** Having lots of zero bytes will lead *terrible* initial
output until the generator mixes it all up. Having all zero bytes will
completely break the generator. If you're going to seed from, say, the
unix epoch, then XOR it with 16 static random bytes.

#### xorshift128+ and xorshift64\*

These generators are closely related and, like I said, xorshift64\* was
what I used for years. Looks like it's time to retire it.

~~~c
uint64_t
xorshift64star(uint64_t s[1])
{
    uint64_t x = s[0];
    x ^= x >> 12;
    x ^= x << 25;
    x ^= x >> 27;
    s[0] = x;
    return x * UINT64_C(0x2545f4914f6cdd1d);
}
~~~

However, unlike both xoroshiro128+ and xorshift128+, xorshift64\* will
tolerate weak seeding so long as it's not literally zero. Zero will also
break this generator.

If it weren't for xoroshiro128+, then xorshift128+ would have been the
winner of the benchmark and my new favorite choice.

~~~c
uint64_t
xorshift128plus(uint64_t s[2])
{
    uint64_t x = s[0];
    uint64_t y = s[1];
    s[0] = y;
    x ^= x << 23;
    s[1] = x ^ y ^ (x >> 17) ^ (y >> 26);
    return s[1] + y;
}
~~~

It's a lot like xoroshiro128+, including the need to be well-seeded,
but it's just slow enough to lose out. There's no reason to use
xorshift128+ instead of xoroshiro128+.

### Conclusion

My own takeaway (until I re-evaluate some years in the future):

* The best 64-bit simulation PRNG is xoroshiro128+.
* "Simplified" PCG can be useful in a pinch.
* When only 32-bit integers are necessary, use PCG.

Things can change significantly between platforms, though. Here's the
shootout on a ARM Cortex-A53:

                        Speed (MB/s)
    PRNG         gcc-5.4.0   clang-3.8.0
    ------------------------------------
    baseline          2560        2400
    blowfishcbc16       36.5        45.4
    blowfishcbc4       135         173
    blowfishctr16       36.4        45.2
    blowfishctr4       133         168
    mt64               207         254
    pcg64              980         712
    rc4                 96.6        44.0
    spcg64            1021         948
    xoroshiro128+     2560        1570
    xorshift128+      2560        1520
    xorshift64*       1360        1080

LLVM is not as mature on this platform, but, with GCC, both
xoroshiro128+ and xorshift128+ matched the baseline! It seems memory
is the bottleneck.

So don't necessarily take my word for it. You can run this shootout in
your own environment — perhaps even tossing in more PRNGs — to find
what's appropriate for your own situation.


[bf]: /blog/2017/09/15/
[cbc]: https://en.wikipedia.org/wiki/Block_cipher_mode_of_operation#CBC
[crypto]: https://blog.cr.yp.to/20140205-entropy.html
[ctr]: https://en.wikipedia.org/wiki/Block_cipher_mode_of_operation#Counter_.28CTR.29
[dbl]: http://mumble.net/~campbell/2014/04/28/uniform-random-float
[dh]: http://webhome.phy.duke.edu/~rgb/General/dieharder.php
[ga]: https://github.com/skeeto/carpet-fractal-genetics
[mc]: https://possiblywrong.wordpress.com/2015/09/15/kanoodle-iq-fit-and-dancing-links/
[mcts]: /blog/2017/04/27/
[mt]: https://en.wikipedia.org/wiki/Mersenne_Twister
[order]: http://preshing.com/20120515/memory-reordering-caught-in-the-act/
[other]: http://xoroshiro.di.unimi.it/
[pcg]: http://www.pcg-random.org/
[pcgp]: http://www.pcg-random.org/paper.html
[plt]: /blog/2016/10/27/
[practrand]: http://pracrand.sourceforge.net/
[rdrand]: https://en.wikipedia.org/wiki/RdRand
[simd]: /blog/2015/07/10/
[xoro]: http://xoroshiro.di.unimi.it/
[xs]: https://en.wikipedia.org/wiki/Xorshift
