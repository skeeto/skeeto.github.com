---
title: Prospecting for Hash Functions
layout: post
date: 2018-07-31T22:32:45Z
tags: [c, crypto, optimization]
uuid: e865266a-2896-30c5-3f7d-cfad767b1ae2
---

*Update 2022*: [TheIronBorn has found even better permutations][tib] using
a smarter technique.

I recently got an itch to design my own non-cryptographic integer hash
function. Firstly, I wanted to [better understand][blowpipe] how hash
functions work, and the best way to learn is to do. For years I'd been
treating them like magic, shoving input into it and seeing
[random-looking][rot], but deterministic, output come out the other
end. Just how is the avalanche effect achieved?

Secondly, could I apply my own particular strengths to craft a hash
function better than the handful of functions I could find online?
Especially the classic ones from [Thomas Wang][wang] and [Bob
Jenkins][jenkins]. Instead of struggling with the mathematics, maybe I
could software engineer my way to victory, working from the advantage
of access to the excessive computational power of today.

Suppose, for example, I wrote tool to generate a **random hash
function definition**, then **JIT compile it** to a native function in
memory, then execute that function across various inputs to **evaluate
its properties**. My tool could rapidly repeat this process in a loop
until it stumbled upon an incredible hash function the world had never
seen. That's what I actually did. I call it the **Hash Prospector**:

**<https://github.com/skeeto/hash-prospector>**

It only works on x86-64 because it uses the same [JIT compiling
technique I've discussed before][jit]: allocate a page of memory, write
some machine instructions into it, set the page to executable, cast the
page pointer to a function pointer, then call the generated code through
the function pointer.

### Generating a hash function

My focus is on integer hash functions: a function that accepts an
*n*-bit integer and returns an *n*-bit integer. One of the important
properties of an *integer* hash function is that it maps its inputs to
outputs 1:1. In other words, there are **no collisions**. If there's a
collision, then some outputs aren't possible, and the function isn't
making efficient use of its entropy.

This is actually a lot easier than it sounds. As long as every *n*-bit
integer operation used in the hash function is *reversible*, then the
hash function has this property. An operation is reversible if, given
its output, you can unambiguously compute its input.

For example, XOR with a constant is trivially reversible: XOR the
output with the same constant to reverse it. Addition with a constant
is reversed by subtraction with the same constant. Since the integer
operations are modular arithmetic, modulo 2^n for *n*-bit integers,
multiplication by an *odd* number is reversible. Odd numbers are
coprime with the power-of-two modulus, so there is some *modular
multiplicative inverse* that reverses the operation.

[Bret Mulvey's hash function article][mulvey] provides a convenient list
of some reversible operations available for constructing integer hash
functions. This list was the catalyst for my little project. Here are
the ones used by the hash prospector:

```c
x  = ~x;
x ^= constant;
x *= constant | 1; // e.g. only odd constants
x += constant;
x ^= x >> constant;
x ^= x << constant;
x += x << constant;
x -= x << constant;
x <<<= constant; // left rotation
```

I've come across a couple more useful operations while studying existing
integer hash functions, but I didn't put these in the prospector.

```c
hash += ~(hash << constant);
hash -= ~(hash << constant);
```

The prospector picks some operations at random and fills in their
constants randomly within their proper constraints. For example,
here's an awful hash function I made it generate as an example:

```c
// do NOT use this!
uint32_t
badhash32(uint32_t x)
{
    x *= 0x1eca7d79U;
    x ^= x >> 20;
    x  = (x << 8) | (x >> 24);
    x  = ~x;
    x ^= x << 5;
    x += 0x10afe4e7U;
    return x;
}
```

That function is reversible, and it would be [relatively
straightforward][inv1] to [define its inverse][inv2]. However, it has
awful biases and poor avalanche. How do I know this?

### The measure of a hash function

There are two key properties I'm looking for in randomly generated hash
functions.

1. High avalanche effect. When I flip one input bit, the output bits
   should each flip with a 50% chance.

2. Low bias. Ideally there is no correlation between which output bits
   flip for a particular flipped input bit.

Initially I screwed up and only measured the first property. This lead
to some hash functions that *seemed* to be amazing before close
inspection, since, for a 32-bit hash function, it was flipping over 15
output bits on average. However, the particular bits being flipped
were heavily biased, resulting in obvious patterns in the output.

For example, when hashing a counter starting from zero, the high bits
would follow a regular pattern. 15 to 16 bits were being flipped each
time, but it was always the same bits.

Conveniently it's easy to measure both properties at the same time. For
an *n*-bit integer hash function, create an *n* by *n* table initialized
to zero. The rows are input bits and the columns are output bits. The
*i*th row and *j*th column track the correlation between the *i*th input
bit and *j*th output bit.

Then exhaustively iterate over all 2^n inputs, and flip each bit one at
a time. Increment the appropriate element in the table if the output bit
flips.

When you're done, ideally each element in the table is exactly 2^(n-1).
That is, each output bit was flipped exactly half the time by each input
bit. Therefore the *bias* of the hash function is the distance (the
error) of the computed table from the ideal table.

For example, the ideal bias table for an 8-bit hash function would be:

    128 128 128 128 128 128 128 128
    128 128 128 128 128 128 128 128
    128 128 128 128 128 128 128 128
    128 128 128 128 128 128 128 128
    128 128 128 128 128 128 128 128
    128 128 128 128 128 128 128 128
    128 128 128 128 128 128 128 128
    128 128 128 128 128 128 128 128

The hash prospector computes the standard deviation in order to turn
this into a single, normalized measurement. Lower scores are better.

However, there's still one problem: the input space for a 32-bit hash
function is over 4 billion values. The full test takes my computer about
an hour and a half. Evaluating a 64-bit hash function is right out.

Again, [Monte Carlo to the rescue][prng]! Rather than sample the entire
space, just sample a random subset. This provides a good estimate in
less than a second, allowing lots of terrible hash functions to be
discarded early. The full test can be saved only for the known good
32-bit candidates. 64-bit functions will only ever receive the estimate.

### What did I find?

Once I got the bias issue sorted out, and after hours and hours of
running, followed up with some manual tweaking on my part, the
**prospector stumbled across this little gem**:

```c
// DO use this one!
uint32_t
prospector32(uint32_t x)
{
    x ^= x >> 15;
    x *= 0x2c1b3c6dU;
    x ^= x >> 12;
    x *= 0x297a2d39U;
    x ^= x >> 15;
    return x;
}
```

According to a full (e.g. not estimated) bias evaluation, this function
beats *the snot* out of most of 32-bit hash functions I could find. It
even comes out ahead of this well known hash function that I *believe*
originates from the H2 SQL Database. (Update: Thomas Mueller has
confirmed that, indeed, this is his hash function.)

```c
uint32_t
hash32(uint32_t x)
{
    x = ((x >> 16) ^ x) * 0x45d9f3bU;
    x = ((x >> 16) ^ x) * 0x45d9f3bU;
    x = (x >> 16) ^ x;
    return x;
}
```

It's still an excellent hash function, just slightly more biased than
mine.

Very briefly, `prospector32()` was the best 32-bit hash function I could
find, and I thought I had a major breakthrough. Then I noticed the
finalizer function for [the 32-bit variant of MurmurHash3][m3]. It's
also a 32-bit hash function:

```c
uint32_t
murmurhash32_mix32(uint32_t x)
{
    x ^= x >> 16;
    x *= 0x85ebca6bU;
    x ^= x >> 13;
    x *= 0xc2b2ae35U;
    x ^= x >> 16;
    return x;
}
```

This one is just *barely* less biased than mine. So I still haven't
discovered the best 32-bit hash function, only the *second* best one.
:-)

### A pattern emerges

If you're paying close enough attention, you may have noticed that all
three functions above have the same structure. The prospector had
stumbled upon it all on its own without knowledge of the existing
functions. It may not be so obvious for the second function, but here it
is refactored:

```c
uint32_t
hash32(uint32_t x)
{
    x ^= x >> 16;
    x *= 0x45d9f3bU;
    x ^= x >> 16;
    x *= 0x45d9f3bU;
    x ^= x >> 16;
    return x;
}
```

I hadn't noticed this until after the prospector had come across it on
its own. The pattern for all three is XOR-right-shift, multiply,
XOR-right-shift, multiply, XOR-right-shift. There's something
particularly useful about this [multiply-xorshift construction][mx]
([also][mx2]). The XOR-right-shift diffuses bits rightward and the
multiply diffuses bits leftward. I like to think it's "sloshing" the
bits right, left, right, left.

It seems that multiplication is particularly good at diffusion, so it
makes perfect sense to exploit it in non-cryptographic hash functions,
especially since modern CPUs are so fast at it. Despite this, it's not
used much in cryptography due to [issues with completing it in constant
time][salsa20].

I like to think of this construction in terms of a five-tuple. For the
three functions it's the following:

    (15, 0x2c1b3c6d, 12, 0x297a2d39, 15)  // prospector32()
    (16, 0x045d9f3b, 16, 0x045d9f3b, 16)  // hash32()
    (16, 0x85ebca6b, 13, 0xc2b2ae35, 16)  // murmurhash32_mix32()

The prospector actually found lots of decent functions following this
pattern, especially where the middle shift is smaller than the outer
shift. Thinking of it in terms of this tuple, I specifically directed
it to try different tuple constants. That's what I meant by
"tweaking." Eventually my new function popped out with its really low
bias.

The prospector has a template option (`-p`) if you want to try it
yourself:

    $ ./prospector -p xorr,mul,xorr,mul,xorr

If you really have your heart set on certain constants, such as my
specific selection of shifts, you can lock those in while randomizing
the other constants:

    $ ./prospector -p xorr:15,mul,xorr:12,mul,xorr:15

Or the other way around:

    $ ./prospector -p xorr,mul:2c1b3c6d,xorr,mul:297a2d39,xorr

My function seems a little strange using shifts of 15 bits rather than
a nice, round 16 bits. However, changing those constants to 16
increases the bias. Similarly, neither of the two 32-bit constants is
a prime number, but **nudging those constants to the nearest prime
increases the bias**. These parameters really do seem to be a local
minima in the bias, and using prime numbers isn't important.

### What about 64-bit integer hash functions?

So far I haven't been able to improve on 64-bit hash functions. The main
function to beat is SplittableRandom / [SplitMix64][sr]:

```c
uint64_t
splittable64(uint64_t x)
{
    x ^= x >> 30;
    x *= 0xbf58476d1ce4e5b9U;
    x ^= x >> 27;
    x *= 0x94d049bb133111ebU;
    x ^= x >> 31;
    return x;
}
```

Here's its inverse since it's sometimes useful:

```c
uint64_t
splittable64_r(uint64_t x)
{
    x ^= x >> 31 ^ x >> 62;
    x *= 0x319642b2d24d8ec3U;
    x ^= x >> 27 ^ x >> 54;
    x *= 0x96de1b173f119089U;
    x ^= x >> 30 ^ x >> 60;
    return x;
}
```

I also came across [this function][h2]:

```c
uint64_t
hash64(uint64_t x)
{
    x ^= x >> 32;
    x *= 0xd6e8feb86659fd93U;
    x ^= x >> 32;
    x *= 0xd6e8feb86659fd93U;
    x ^= x >> 32;
    return x;
}
```

Again, these follow the same construction as before. There really is
something special about it, and many other people have noticed, too.

Both functions have about the same bias. (Remember, I can only estimate
the bias for 64-bit hash functions.) The prospector has found lots of
functions with about the same bias, but nothing provably better. Until
it does, I have no new 64-bit integer hash functions to offer.

### Beyond random search

Right now the prospector does a completely random, unstructured search
hoping to stumble upon something good by chance. Perhaps it would be
worth using a genetic algorithm to breed those 5-tuples towards
optimum? Others have had [success in this area with simulated
annealing][sa].

There's probably more to exploit from the multiply-xorshift construction
that keeps popping up. If anything, the prospector is searching too
broadly, looking at constructions that could never really compete no
matter what the constants. In addition to everything above, I've been
looking for good 32-bit hash functions that don't use any 32-bit
constants, but I'm really not finding any with a competitively low bias.

### Update after one week

About one week after publishing this article I found an even better hash
function. I believe **this is the least biased 32-bit integer hash
function *of this form* ever devised**. It's even less biased than the
MurmurHash3 finalizer.

```c
// exact bias: 0.17353355999581582
uint32_t
lowbias32(uint32_t x)
{
    x ^= x >> 16;
    x *= 0x7feb352dU;
    x ^= x >> 15;
    x *= 0x846ca68bU;
    x ^= x >> 16;
    return x;
}

// inverse
uint32_t
lowbias32_r(uint32_t x)
{
    x ^= x >> 16;
    x *= 0x43021123U;
    x ^= x >> 15 ^ x >> 30;
    x *= 0x1d69e2a5U;
    x ^= x >> 16;
    return x;
}
```

If you're willing to use an additional round of multiply-xorshift, this
next function actually reaches the theoretical bias limit (bias =
~0.021) as exhibited by a perfect integer hash function:

```c
// exact bias: 0.020888578919738908
uint32_t
triple32(uint32_t x)
{
    x ^= x >> 17;
    x *= 0xed5ad4bbU;
    x ^= x >> 11;
    x *= 0xac4c1b51U;
    x ^= x >> 15;
    x *= 0x31848babU;
    x ^= x >> 14;
    return x;
}
```

It's statistically indistinguishable from a random permutation of all
32-bit integers.

### Update, February 2020

Some people have been experimenting with using my hash functions in GLSL
shaders, and the results are looking good:

* <https://www.shadertoy.com/view/WttXWX>
* <https://www.shadertoy.com/view/ttVGDV>


[blowpipe]: /blog/2017/09/15/
[h2]: https://gist.github.com/degski/6e2069d6035ae04d5d6f64981c995ec2
[inv1]: https://naml.us/post/inverse-of-a-hash-function/
[inv2]: http://c42f.github.io/2015/09/21/inverting-32-bit-wang-hash.html
[jenkins]: http://burtleburtle.net/bob/hash/integer.html
[jit]: /blog/2015/03/19/
[m3]: https://en.wikipedia.org/wiki/MurmurHash#Algorithm
[mulvey]: http://papa.bretmulvey.com/post/124027987928/hash-functions
[mx]: http://www.pcg-random.org/posts/developing-a-seed_seq-alternative.html#multiplyxorshift
[mx2]: http://ticki.github.io/blog/designing-a-good-non-cryptographic-hash-function/#designing-a-diffusion-function--by-example
[prng]: /blog/2017/09/21/
[rot]: /blog/2018/02/07/
[sa]: https://zimbry.blogspot.com/2011/09/better-bit-mixing-improving-on.html
[salsa20]: http://cr.yp.to/snuffle/design.pdf
[sr]: http://xoshiro.di.unimi.it/splitmix64.c
[tib]: https://github.com/skeeto/hash-prospector/issues/19
[wang]: https://gist.github.com/badboy/6267743
