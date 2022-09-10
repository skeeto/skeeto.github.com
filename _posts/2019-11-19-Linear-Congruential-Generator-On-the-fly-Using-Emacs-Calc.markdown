---
title: On-the-fly Linear Congruential Generator Using Emacs Calc
layout: post
date: 2019-11-19T01:17:50Z
tags: [emacs, crypto, optimization, c, java, javascript]
uuid: 13e56720-ef3a-4fa4-a4ff-0a6fef914504
---

I regularly make throwaway "projects" and do a surprising amount of
programming in `/tmp`. For Emacs Lisp, the equivalent is the
`*scratch*` buffer. These are places where I can make a mess, and the
mess usually gets cleaned up before it becomes a problem. A lot of my
established projects ([ex][ssh].) start out in volatile storage and
only graduate to more permanent storage once the concept has proven
itself.

Throughout my whole career, this sort of throwaway experimentation has
been an important part of my personal growth, and I try to [encourage it
in others][ment]. Even if the idea I'm trying doesn't pan out, I usually
learn something new, and occasionally it translates into an article here.

I also enjoy small programming challenges. One of the most abused
tools in my mental toolbox is the Monte Carlo method, and I readily
apply it to solve toy problems. Even beyond this, random number
generators are frequently a useful tool ([1][mcts], [2][pgp]), so I
find myself reaching for one all the time.

Nearly every programming language comes with a pseudo-random number
generation function or library. Unfortunately the language's standard
PRNG is usually a poor choice (C, [C++][cpp], [C#][csharp], [Go][go]).
It's probably mediocre quality, [slower than it needs to be][call]
([also][go]), [lacks reliable semantics or behavior between
implementations][bsd], or is missing some other property I want. So I've
long been a fan of *BYOPRNG:* Bring Your Own Pseudo-random Number
Generator. Just embed a generator with the desired properties directly
into the program. The [best non-cryptographic PRNGs today][prng] are
tiny and exceptionally friendly to embedding. Though, depending on what
you're doing, you might [need to be creative about seeding][ent].

### Crafting a PRNG

On occasion I don't have an established, embeddable PRNG in reach, and
I have yet to commit xoshiro256\*\* to memory. Or maybe I want to use
a totally unique PRNG for a particular project. In these cases I make
one up. With just a bit of know-how it's not too difficult.

Probably the easiest decent PRNG to code from scratch is the venerable
[Linear Congruential Generator][lcg] (LCG). It's a simple recurrence
relation:

    x[1] = (x[0] * A + C) % M

That's trivial to remember once you know the details. You only need to
choose appropriate values for `A`, `C`, and `M`. Done correctly, it
will be a *full-period* generator — a generator that visits a
permutation of each of the numbers between 0 and `M - 1`. The seed —
the value of `x[0]` — is chooses a starting position in this (looping)
permutation.

`M` has a natural, obvious choice: a power of two matching the range of
operands, such as 2^32 or 2^64. With this the modulo operation is free
as a natural side effect of the computer architecture.

Choosing `C` also isn't difficult. It must be co-prime with `M`, and
since `M` is a power of two, any odd number is valid. Even 1. In
theory choosing a small value like 1 is faster since the compiler
won't need to embed a large integer in the code, but this difference
doesn't show up in any micro-benchmarks I tried. If you want a cool,
unique generator, then choose a large random integer. More on that
below.

The tricky value is `A`, and getting it right is the linchpin of the
whole LCG. It must be coprime with `M` (i.e. not even), and, for a
full-period generator, `A-1` must be divisible by four. For better
results, `A-1` should not be divisible by 8. A good choice is a prime
number that satisfies these properties.

If your operands are 64-bit integers, or larger, how are you going to
generate a prime number?

#### Primes from Emacs Calc

Emacs Calc can solve this problem. I've [noted before][calc] how
featureful it is. It has arbitrary precision, random number
generation, and primality testing. It's everything we need to choose
`A`. (In fact, this is nearly identical to [the process I used to
implement RSA][rsa].) For this example I'm going to generate a 64-bit
LCG for the C programming language, but it's easy to use whatever
width you like and mostly whatever language you like. If you wanted a
[minimal standard 128-bit LCG][min], this will still work.

Start by opening up Calc with `M-x calc`, then:

1. Push `2` on the stack
2. Push `64` on the stack
3. Press `^`, computing 2^64 and pushing it on the stack
4. Press `k r` to generate a random number in this range
5. Press `d r 16` to switch to hexadecimal display
6. Press `k n` to find the next prime following the random value
7. Repeat step 6 until you get a number that ends with `5` or `D`
8. Press `k p` a few times to avoid false positives.

What's left on the stack is your `A`! If you want a random value for
`C`, you can follow a similar process. Heck, make it prime, too!

The reason for using hexadecimal (step 5) and looking for `5` or `D`
(step 7) is that such numbers satisfy both of the important properties
for `A-1`.

Calc doesn't try to factor your random integer. Instead it uses the
[Miller–Rabin primality test][mr], a probabilistic test that, itself,
requires random numbers. It has false positives but no false negatives.
The false positives can be mitigated by repeating the test multiple
times, hence step 8.

Trying this all out right now, I got this implementation (in C):

```c
uint64_t lcg1(void)
{
    static uint64_t s = 0;
    s = s*UINT64_C(0x7c3c3267d015ceb5) + UINT64_C(0x24bd2d95276253a9);
    return s;
}
```

However, we can still do a little better. Outputting the entire state
doesn't have great results, so instead it's better to create a
*truncated* LCG and only return some portion of the most significant
bits.

```c
uint32_t lcg2(void)
{
    static uint64_t s = 0;
    s = s*UINT64_C(0x7c3c3267d015ceb5) + UINT64_C(0x24bd2d95276253a9);
    return s >> 32;
}
```

This won't quite pass [BigCrush][test] in 64-bit form, but the results
are pretty reasonable for most purposes.

But we can still do better without needing to remember much more than
this.

### Appending permutation

A [Permuted Congruential Generator][pcg] (PCG) is really just a
truncated LCG with a permutation applied to its output. Like LCGs
themselves, there are arbitrarily many variations. The "official"
implementation has a [data-dependent shift][data], for which I can
never remember the details. Fortunately a couple of simple, easy to
remember transformations is sufficient. Basically anything I used
[while prospecting for hash functions][hash]. I love xorshifts, so
lets add one of those:

```c
uint32_t pcg1(void)
{
    static uint64_t s = 0;
    s = s*UINT64_C(0x7c3c3267d015ceb5) + UINT64_C(0x24bd2d95276253a9);
    uint32_t r = s >> 32;
    r ^= r >> 16;
    return r;
}
```

This is a big improvement, but it still fails one BigCrush test. As
they say, when xorshift isn't enough, use xorshift-multiply! Below I
generated a 32-bit prime for the multiply, but any odd integer is a
valid permutation.

```c
uint32_t pcg2(void)
{
    static uint64_t s = 0;
    s = s*UINT64_C(0x7c3c3267d015ceb5) + UINT64_C(0x24bd2d95276253a9);
    uint32_t r = s >> 32;
    r ^= r >> 16;
    r *= UINT32_C(0x60857ba9);
    return r;
}
```

This passes BigCrush, and I can reliably build a new one entirely from
scratch using Calc any time I need it.

### Bonus: Adapting to other languages

Sometimes it's not so straightforward to adapt this technique to other
languages. For example, JavaScript has limited support for 32-bit
integer operations (enough for a poor 32-bit LCG) and no 64-bit
integer operations. Though [BigInt][bi] is now a thing, and should
make a great 96- or 128-bit LCG easy to build.

```js
function lcg(seed) {
    let s = BigInt(seed);
    return function() {
        s *= 0xef725caa331524261b9646cdn;
        s += 0x213734f2c0c27c292d814385n;
        s &= 0xffffffffffffffffffffffffn;
        return Number(s >> 64n);
    }
}
```

Java doesn't have unsigned integers, so how could you build the above
PCG in Java? Easy! First, remember is that Java has two's complement
semantics, including wrap around, and that two's complement doesn't
care about unsigned or signed for multiplication (or addition, or
subtraction). The result is identical. Second, the oft-forgotten `>>>`
operator does an unsigned right shift. With these two tips:

```java
long s = 0;

int pcg2() {
    s = s*0x7c3c3267d015ceb5L + 0x24bd2d95276253a9L;
    int r = (int)(s >>> 32);
    r ^= r >>> 16;
    r *= 0x60857ba9;
    return r;
}
```

So, in addition to the Calc step list above, you may need to know some
of the finer details of your target language.


[bi]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt
[bsd]: https://lists.freebsd.org/pipermail/svn-src-head/2013-July/049068.html
[calc]: /blog/2009/06/23/
[call]: /blog/2018/05/27/
[cpp]: https://arvid.io/2018/06/30/on-cxx-random-number-generator-quality/
[csharp]: https://lowleveldesign.org/2018/08/15/randomness-in-net/
[data]: /blog/2018/02/07/
[ent]: /blog/2019/04/30/
[go]: https://github.com/skeeto/rng-go
[go]: https://grokbase.com/t/gg/golang-nuts/155f6kbb7a/go-nuts-why-are-high-bits-used-by-math-rand-helpers-instead-of-low-ones
[hash]: /blog/2018/07/31/
[lcg]: https://en.wikipedia.org/wiki/Linear_congruential_generator
[mcts]: /blog/2017/04/27/
[ment]: /blog/2016/09/02/
[min]: http://www.pcg-random.org/posts/does-it-beat-the-minimal-standard.html
[mr]: https://en.wikipedia.org/wiki/Miller%E2%80%93Rabin_primality_test
[pcg]: http://www.pcg-random.org/
[pgp]: /blog/2019/07/22/
[prng]: /blog/2017/09/21/
[rsa]: /blog/2015/10/30/
[ssh]: /blog/2019/03/22/
[test]: http://simul.iro.umontreal.ca/testu01/tu01.html
