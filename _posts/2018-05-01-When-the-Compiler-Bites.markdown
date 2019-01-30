---
title: When the Compiler Bites
layout: post
date: 2018-05-01T23:28:06Z
tags: [c, x86, optimization, ai, netsec]
uuid: 02b974e1-e25b-397d-a16f-c754338e9c1e
---

*Update: There are discussions [on Reddit][reddit] and [on Hacker
News][hn].*

So far this year I've been bitten three times by compiler edge cases
in GCC and Clang, each time catching me totally by surprise. Two were
caused by historical artifacts, where an ambiguous specification lead
to diverging implementations. The third was a compiler optimization
being far more clever than I expected, behaving almost like an
artificial intelligence.

In all examples I'll be using GCC 7.3.0 and Clang 6.0.0 on Linux.

### x86-64 ABI ambiguity

The first time I was bit — or, well, narrowly avoided being bit — was
when I examined a missed floating point optimization in both Clang and
GCC. Consider this function:

```c
double
zero_multiply(double x)
{
    return x * 0.0;
}
```

The function multiplies its argument by zero and returns the result. Any
number multiplied by zero is zero, so this should always return zero,
right? Unfortunately, no. IEEE 754 floating point arithmetic supports
NaN, infinities, and signed zeros. This function can return NaN,
positive zero, or negative zero. (In some cases, the operation could
also potentially produce a hardware exception.)

As a result, both GCC and Clang perform the multiply:

```nasm
zero_multiply:
    xorpd  xmm1, xmm1
    mulsd  xmm0, xmm1
    ret
```

The `-ffast-math` option relaxes the C standard floating point rules,
permitting an optimization at the cost of conformance and
[consistency][prec]:

```nasm
zero_multiply:
    xorps  xmm0, xmm0
    ret
```

Side note: `-ffast-math` doesn't necessarily mean "less precise."
Sometimes it will actually [improve precision][fma].

Here's a modified version of the function that's a little more
interesting. I've changed the argument to a `short`:

```c
double
zero_multiply_short(short x)
{
    return x * 0.0;
}
```

It's no longer possible for the argument to be one of those special
values. The `short` will be promoted to one of 65,535 possible `double`
values, each of which results in 0.0 when multiplied by 0.0. GCC misses
this optimization (`-Os`):

```nasm
zero_multiply_short:
    movsx     edi, di       ; sign-extend 16-bit argument
    xorps     xmm1, xmm1    ; xmm1 = 0.0
    cvtsi2sd  xmm0, edi     ; convert int to double
    mulsd     xmm0, xmm1
    ret
```

Clang also misses this optimization:

```nasm
zero_multiply_short:
    cvtsi2sd xmm1, edi
    xorpd    xmm0, xmm0
    mulsd    xmm0, xmm1
    ret
```

But hang on a minute. This is shorter by one instruction. What
happened to the sign-extension (`movsx`)? Clang is treating that
`short` argument as if it were a 32-bit value. Why do GCC and Clang
differ? Is GCC doing something unnecessary?

It turns out that the [x86-64 ABI][abi] didn't specify what happens with
the upper bits in argument registers. Are they garbage? Are they zeroed?
GCC takes the conservative position of assuming the upper bits are
arbitrary garbage. Clang takes the boldest position of assuming
arguments smaller than 32 bits have been promoted to 32 bits by the
caller. This is what the ABI specification *should* have said, but
currently it does not.

Fortunately GCC also conservative when passing arguments. It promotes
arguments to 32 bits as necessary, so there are no conflicts when
linking against Clang-compiled code. However, this is not true for
Intel's ICC compiler: [**Clang and ICC are not ABI-compatible on
x86-64**][icc].

I don't use ICC, so that particular issue wouldn't bite me, *but* if I
was ever writing assembly routines that called Clang-compiled code, I'd
eventually get bit by this.

### Floating point precision

Without looking it up or trying it, what does this function return?
Think carefully.

```c
int
float_compare(void)
{
    float x = 1.3f;
    return x == 1.3f;
}
```

Confident in your answer? This is a trick question, because it can
return either 0 or 1 depending on the compiler. Boy was I confused when
this comparison returned 0 in my real world code.

    $ gcc   -std=c99 -m32 cmp.c  # float_compare() == 0
    $ clang -std=c99 -m32 cmp.c  # float_compare() == 1

So what's going on here? The original ANSI C specification wasn't
clear about how intermediate floating point values get rounded, and
implementations [all did it differently][hn]. The C99 specification
cleaned this all up and introduced [`FLT_EVAL_METHOD`][wflt].
Implementations can still differ, but at least you can now determine
at compile-time what the compiler would do by inspecting that macro.

Back in the late 1980's or early 1990's when the GCC developers were
deciding how GCC should implement floating point arithmetic, the trend
at the time was to use as much precision as possible. On the x86 this
meant using its support for 80-bit extended precision floating point
arithmetic. Floating point operations are performed in `long double`
precision and truncated afterward (`FLT_EVAL_METHOD == 2`).

In `float_compare()` the left-hand side is truncated to a `float` by the
assignment, but the right-hand side, *despite being a `float` literal*,
is actually "1.3" at 80 bits of precision as far as GCC is concerned.
That's pretty unintuitive!

The remnants of this high precision trend are still in JavaScript, where
all arithmetic is double precision (even if [simulated using
integers][smi]), and great pains have been made [to work around][fround]
the performance consequences of this. [Until recently][mono], Mono had
similar issues.

The trend reversed once SIMD hardware became widely available and
there were huge performance gains to be had. Multiple values could be
computed at once, side by side, at lower precision. So on x86-64, this
became the default (`FLT_EVAL_METHOD == 0`). The young Clang compiler
wasn't around until well after this trend reversed, so it behaves
differently than the [backwards compatible][flt] GCC on the old x86.

I'm a little ashamed that I'm only finding out about this now. However,
by the time I was competent enough to notice and understand this issue,
I was already doing nearly all my programming on the x86-64.

### Built-in Function Elimination

I've saved this one for last since it's my favorite. Suppose we have
this little function, `new_image()`, that allocates a greyscale image
for, say, [some multimedia library][mm].

```c
static unsigned char *
new_image(size_t w, size_t h, int shade)
{
    unsigned char *p = 0;
    if (w == 0 || h <= SIZE_MAX / w) { // overflow?
        p = malloc(w * h);
        if (p) {
            memset(p, shade, w * h);
        }
    }
    return p;
}
```

It's a static function because this would be part of some [slick
header library][stb] (and, secretly, because it's necessary for
illustrating the issue). Being a responsible citizen, the function
even [checks for integer overflow][of] before allocating anything.

I write a unit test to make sure it detects overflow. This function
should return 0.

```c
/* expected return == 0 */
int
test_new_image_overflow(void)
{
    void *p = new_image(2, SIZE_MAX, 0);
    return !!p;
}
```

So far my test passes. Good.

I'd also like to make sure it correctly returns NULL — or, more
specifically, that it doesn't crash — if the allocation fails. But how
can I make `malloc()` fail? As a hack I can pass image dimensions that
I know cannot ever practically be allocated. Essentially I want to
force a `malloc(SIZE_MAX)`, e.g. allocate every available byte in my
virtual address space. For a conventional 64-bit machine, that's 16
exibytes of memory, and it leaves space for nothing else, including
the program itself.

```c
/* expected return == 0 */
int
test_new_image_oom(void)
{
    void *p = new_image(1, SIZE_MAX, 0xff);
    return !!p;
}
```

I compile with GCC, test passes. I compile with Clang and the test
fails. That is, **the test somehow managed to allocate 16 exibytes of
memory, *and* initialize it**. Wat?

Disassembling the test reveals what's going on:

```nasm
test_new_image_overflow:
    xor  eax, eax
    ret

test_new_image_oom:
    mov  eax, 1
    ret
```

The first test is actually being evaluated at compile time by the
compiler. The function being tested was inlined into the unit test
itself. This permits the compiler to collapse the whole thing down to
a single instruction. The path with `malloc()` became dead code and
was trivially eliminated.

In the second test, Clang correctly determined that the image buffer is
not actually being used, despite the `memset()`, so it eliminated the
allocation altogether and then *simulated* a successful allocation
despite it being absurdly large. Allocating memory is not an observable
side effect as far as the language specification is concerned, so it's
allowed to do this. My thinking was wrong, and the compiler outsmarted
me.

I soon realized I can take this further and trick Clang into
performing an invalid optimization, [revealing a bug][bug]. Consider
this slightly-optimized version that uses `calloc()` when the shade is
zero (black). The `calloc()` function does its own overflow check, so
`new_image()` doesn't need to do it.

```c
static void *
new_image(size_t w, size_t h, int shade)
{
    unsigned char *p = 0;
    if (shade == 0) { // shortcut
        p = calloc(w, h);
    } else if (w == 0 || h <= SIZE_MAX / w) { // overflow?
        p = malloc(w * h);
        if (p) {
            memset(p, color, w * h);
        }
    }
    return p;
}
```

With this change, my overflow unit test is now also failing. The
situation is even worse than before. The `calloc()` is being
eliminated *despite the overflow*, and replaced with a simulated
success. This time it's actually a bug in Clang. While failing a unit
test is mostly harmless, **this could introduce a vulnerability in a
real program**. The OpenBSD folks are so worried about this sort of
thing that [they've disabled this optimization][bsd].

Here's a slightly-contrived example of this. Imagine a program that
maintains a table of unsigned integers, and we want to keep track of
how many times the program has accessed each table entry. The "access
counter" table is initialized to zero, but the table of values need
not be initialized, since they'll be written before first access (or
something like that).

```c
struct table {
    unsigned *counter;
    unsigned *values;
};

static int
table_init(struct table *t, size_t n)
{
    t->counter = calloc(n, sizeof(*t->counter));
    if (t->counter) {
        /* Overflow already tested above */
        t->values = malloc(n * sizeof(*t->values));
        if (!t->values) {
            free(t->counter);
            return 0; // fail
        }
        return 1; // success
    }
    return 0; // fail
}
```

This function relies on the overflow test in `calloc()` for the second
`malloc()` allocation. However, this is a static function that's
likely to get inlined, as we saw before. If the program doesn't
actually make use of the `counter` table, and Clang is able to
statically determine this fact, it may eliminate the `calloc()`. This
would also **eliminate the overflow test, introducing a
vulnerability**. If an attacker can control `n`, then they can
overwrite arbitrary memory through that `values` pointer.

### The takeaway

Besides this surprising little bug, the main lesson for me is that I
should probably isolate unit tests from the code being tested. The
easiest solution is to put them in separate translation units and don't
use link-time optimization (LTO). Allowing tested functions to be
inlined into the unit tests is probably a bad idea.

The unit test issues in my *real* program, which was [a bit more
sophisticated][buf] than what was presented here, gave me artificial
intelligence vibes. It's that situation where a computer algorithm did
something really clever and I felt it outsmarted me. It's creepy to
consider [how far that can go][pc]. I've gotten that even from
observing [AI I've written myself][mcts], and I know for sure no human
taught it some particularly clever trick.

My favorite AI story along these lines is about [an AI that learned
how to play games on the Nintendo Entertainment System][nes]. It
didn't understand the games it was playing. It's optimization task was
simply to choose controller inputs that maximized memory values,
because that's generally associated with doing well — higher scores,
more progress, etc. The most unexpected part came when playing Tetris.
Eventually the screen would fill up with blocks, and the AI would face
the inevitable situation of losing the game, with all that memory
being reinitialized to low values. So what did it do?

Just before the end it would pause the game and wait… forever.


[abi]: https://www.uclibc.org/docs/psABI-x86_64.pdf
[bsd]: https://marc.info/?l=openbsd-cvs&m=150125592126437&w=2
[buf]: https://github.com/skeeto/growable-buf
[bug]: https://bugs.llvm.org/show_bug.cgi?id=37304
[flt]: https://gcc.gnu.org/bugzilla/show_bug.cgi?id=323
[fma]: https://en.wikipedia.org/wiki/Multiply–accumulate_operation#Fused_multiply–add
[fround]: https://blog.mozilla.org/javascript/2013/11/07/efficient-float32-arithmetic-in-javascript/
[hn]: https://news.ycombinator.com/item?id=13738880
[hn]: https://news.ycombinator.com/item?id=16974770
[icc]: https://stackoverflow.com/a/36760539
[mcts]: /blog/2017/04/27/
[missed]: https://github.com/gergo-/missed-optimizations
[mm]: /blog/2017/11/03/
[mono]: http://tirania.org/blog/archive/2018/Apr-11.html
[nes]: https://www.youtube.com/watch?v=xOCurBYI_gY
[of]: /blog/2017/07/19/
[pc]: https://wiki.lesswrong.com/wiki/Paperclip_maximizer
[prec]: https://possiblywrong.wordpress.com/2017/09/12/floating-point-agreement-between-matlab-and-c/
[reddit]: https://old.reddit.com/r/cpp/comments/8gfhq3/when_the_compiler_bites/
[smi]: http://thibaultlaurens.github.io/javascript/2013/04/29/how-the-v8-engine-works/#more-example-on-how-v8-optimized-javascript-code
[stb]: https://github.com/nothings/stb
[wflt]: https://en.wikipedia.org/wiki/C99#IEEE_754_floating_point_support
