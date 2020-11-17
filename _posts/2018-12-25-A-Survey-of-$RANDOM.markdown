---
title: A Survey of $RANDOM
layout: post
date: 2018-12-25T00:05:38Z
tags: [linux, bsd, c]
uuid: 071e3ec5-fe1d-309a-3e66-3b590a96ac2c
---

Most Bourne shell clones support a special `RANDOM` environment
variable that evaluates to a random value between 0 and 32,767 (e.g.
15 bits). Assigment to the variable seeds the generator. This variable
is an extension and [did not appear][sh] in the original Unix Bourne
shell. Despite this, the different Bourne-like shells that implement
it have converged to the same interface, but *only* the interface.
Each implementation differs in interesting ways. In this article we'll
explore how `$RANDOM` is implemented in various Bourne-like shells.

~~Unfortunately I was unable to determine the origin of `$RANDOM`.~~
Nobody was doing a good job tracking source code changes before the
mid-1990s, so that history appears to be lost. Bash was first released
in 1989, but the earliest version I could find was 1.14.7, released in 1996.
KornShell was first released in 1983, but the earliest source I could
find [was from 1993][ksh]. In both cases `$RANDOM` already existed. My
guess is that it first appeared in one of these two shells, probably
KornShell.

**Update**: Quentin Barnes has informed me that his 1986 copy of
KornShell (a.k.a. ksh86) implements `$RANDOM`. This predates Bash and
makes it likely that this feature originated in KornShell.

### Bash

Of all the shells I'm going to discuss, Bash has the most interesting
history. It never made use use of `srand(3)` / `rand(3)` and instead
uses its own generator — which is generally [what I prefer][prng]. Prior
to Bash 4.0, it used the crummy linear congruential generator (LCG)
[found in the C89 standard][c89]:

```c
static unsigned long rseed = 1;

static int
brand ()
{
  rseed = rseed * 1103515245 + 12345;
  return ((unsigned int)((rseed >> 16) & 32767));
}
```

For some reason it was naïvely decided that `$RANDOM` should never
produce the same value twice in a row. The caller of `brand()` filters
the output and discards repeats before returning to the shell script.
This actually *reduces* the quality of the generator further since it
increases correlation between separate outputs.

When the shell starts up, `rseed` is seeded from the PID and the current
time in seconds. These values are literally summed and used as the seed.

```c
/* Note: not the literal code, but equivalent. */
rseed = getpid() + time(0);
```

Subshells, which fork and initally share an `rseed`, are given similar
treatment:

```c
rseed = rseed + getpid() + time(0);
```

Notice there's no [hashing][hash] or [mixing][mix] of these values, so
there's no avalanche effect. That would have prevented shells that start
around the same time from having related initial random sequences.

With Bash 4.0, released in 2009, the algorithm was changed to a
[Park–Miller multiplicative LCG][pm] from 1988:

```c
static int
brand ()
{
  long h, l;

  /* can't seed with 0. */
  if (rseed == 0)
    rseed = 123459876;
  h = rseed / 127773;
  l = rseed % 127773;
  rseed = 16807 * l - 2836 * h;
  return ((unsigned int)(rseed & 32767));
}
```

There's actually a subtle mistake in this implementation compared to the
generator described in the paper. This function will generate different
numbers than the paper, and it will generate different numbers on
different hosts! More on that later.

This algorithm is a [much better choice][ms] than the previous LCG.
There were many more options available in 2009 compared to 1989, but,
honestly, this generator is pretty reasonable for this application.
Bash is *so slow* that you're never practically going to generate
enough numbers for the small state to matter. Since the Park–Miller
algorithm is older than Bash, they could have used this in the first
place.

I considered submitting a patch to switch to something more modern.
However, given Bash's constraints, it's harder said than done.
Portability to weird systems is still a concern, and I expect they'd
reject a patch that started making use of `long long` in the PRNG.
They still support pre-ANSI C compilers that don't have 64-bit
arithmetic.

However, what still really *could* be improved is seeding. In Bash 4.x
here's what it looks like:

```c
static void
seedrand ()
{
  struct timeval tv;

  gettimeofday (&tv, NULL);
  sbrand (tv.tv_sec ^ tv.tv_usec ^ getpid ());
}
```

Seeding is both better and worse. It's better that it's seeded from a
higher resolution clock (milliseconds), so two shells started close in
time have more variation. However, it's "mixed" with XOR, which, in
this case, is worse than addition.

For example, imagine two Bash shells started one millsecond apart. Both
`tv_usec` and `getpid()` are incremented by one. Those increments are
likely to cancel each other out by an XOR, and they end up with the same
seed.

Instead, each of those quantities should be hashed before mixing. Here's
a rough example using my [`triple32()` hash][t32] (adapted to glorious
GNU-style pre-ANSI C):

```c
static unsigned long
hash32 (x)
     unsigned long x;
{
  x ^= x >> 17;
  x *= 0xed5ad4bbUL;
  x &= 0xffffffffUL;
  x ^= x >> 11;
  x *= 0xac4c1b51UL;
  x &= 0xffffffffUL;
  x ^= x >> 15;
  x *= 0x31848babUL;
  x &= 0xffffffffUL;
  x ^= x >> 14;
  return x;
}

static void
seedrand ()
{
  struct timeval tv;

  gettimeofday (&tv, NULL);
  sbrand (hash32 (tv.tv_sec) ^
          hash32 (hash32 (tv.tv_usec) ^ getpid ()));
}
```

I had said there's there's a mistake in the Bash implementation of
Park–Miller. Take a closer look at the types and the assignment to
rseed:

```c
  /* The variables */
  long h, l;
  unsigned long rseed;

  /* The assignment */
  rseed = 16807 * l - 2836 * h;
```

The result of the substraction can be negative, and that negative
value is converted to `unsigned long`. The C standard says
`ULONG_MAX + 1` is added to make the value positive. `ULONG_MAX`
varies by platform — typicially `long` is either 32 bits or 64 bits —
so the results also vary. Here's how the paper defined it:

```c
  long test;

  test = 16807 * l - 2836 * h;
  if (test > 0)
    rseed = test;
  else
    rseed = test + 2147483647;
```

As far as I can tell, this mistake doesn't hurt the quality of the
generator.

```
$ 32/bash -c 'RANDOM=127773; echo $RANDOM $RANDOM'
29932 13634

$ 64/bash -c 'RANDOM=127773; echo $RANDOM $RANDOM'
29932 29115
```

### Zsh

In contrast to Bash, Zsh is the most straightforward: defer to
`rand(3)`. Its `$RANDOM` can return the same value twice in a row,
assuming that `rand(3)` does.

```c
zlong
randomgetfn(UNUSED(Param pm))
{
    return rand() & 0x7fff;
}

void
randomsetfn(UNUSED(Param pm), zlong v)
{
    srand((unsigned int)v);
}
```

A cool feature is that means you could override it if you wanted with [a
custom generator][xkcd].

```c
int
rand(void)
{
    return 4; // chosen by fair dice roll.
              // guaranteed to be random.
}
```

Usage:

```
$ gcc -shared -fPIC -o rand.so rand.c
$ LD_PRELOAD=./rand.so zsh -c 'echo $RANDOM $RANDOM $RANDOM'
4 4 4
```

This trick also applies to the rest of the shells below.

### KornShell (ksh)

KornShell originated in 1983, but it was finally released under an open
source license in 2005. There's a clone of KornShell called Public
Domain Korn Shell (pdksh) that's been forked a dozen different ways, but
I'll get to that next.

KornShell defers to `rand(3)`, but it does some additional naïve
filtering on the output. When the shell starts up, it generates 10
values from `rand()`. If any of them are larger than 32,767 then it will
shift right by three all generated numbers.

```c
#define RANDMASK 0x7fff

    for (n = 0; n < 10; n++) {
        // Don't use lower bits when rand() generates large numbers.
        if (rand() > RANDMASK) {
            rand_shift = 3;
            break;
        }
    }
```

Why not just look at `RAND_MAX`? I guess they didn't think of it.

**Update**: Quentin Barnes pointed out that `RAND_MAX` didn't exist
until POSIX standardization in 1988. The constant [first appeared in
Unix in 1990][first]. This KornShell code either predates the standard
or needed to work on systems that predate the standard.

Like Bash, repeated values are not allowed. I suspect one shell got this
idea from the other.

```c
    do {
        cur = (rand() >> rand_shift) & RANDMASK;
    } while (cur == last);
```

Who came up with this strange idea first?

### OpenBSD's Public Domain Korn Shell (pdksh)

I picked the OpenBSD variant of pdksh since it's the only pdksh fork I
ever touch in practice, and its `$RANDOM` is the most interesting of the
pdksh forks — at least since 2014.

Like Zsh, pdksh simply defers to `rand(3)`. However, OpenBSD's `rand(3)`
is [infamously and proudly non-standard][vio]. By default it returns
*non-deterministic*, cryptographic-quality results seeded from system
entropy (via the misnamed [`arc4random(3)`][rc4]), à la `/dev/urandom`.
Its `$RANDOM` inherits this behavior.

```c
    setint(vp, (int64_t) (rand() & 0x7fff));
```

However, if a value is assigned to `$RANDOM` in order to seed it, it
reverts to its old pre-2014 deterministic generation via
[`srand_deterministic(3)`][srand].

```c
    srand_deterministic((unsigned int)intval(vp));
```

OpenBSD's deterministic `rand(3)` is the crummy LCG from the C89
standard, just like Bash 3.x. So if you assign to `$RANDOM`, you'll get
nearly the same results as Bash 3.x and earlier — the only difference
being that it can repeat numbers.

That's a slick upgrade to the old interface without breaking anything,
making it my favorite version `$RANDOM` for any shell.


[c89]: http://port70.net/~nsz/c/c89/c89-draft.html#4.10.2.2
[first]: https://github.com/dspinellis/unix-history-repo/commit/1cc1b02a4361
[hash]: /blog/2018/07/31/
[ksh]: https://web.archive.org/web/20120613182836/http://www.research.att.com/sw/download/man/man1/ksh.html
[mix]: http://www.pcg-random.org/posts/developing-a-seed_seq-alternative.html
[ms]: http://www.pcg-random.org/posts/does-it-beat-the-minimal-standard.html
[pm]: http://www.firstpr.com.au/dsp/rand31/p1192-park.pdf
[prng]: /blog/2017/09/21/
[rc4]: https://man.openbsd.org/arc4random.3
[sh]: http://pubs.opengroup.org/onlinepubs/9699919799.2016edition/utilities/V3_chap02.html
[srand]: https://man.openbsd.org/rand
[t32]: https://github.com/skeeto/hash-prospector#three-round-functions
[vio]: https://marc.info/?l=openbsd-tech&m=141807224826859&w=2
[xkcd]: https://xkcd.com/221/
