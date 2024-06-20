---
title: Guidelines for computing sizes and subscripts
layout: post
date: 2024-05-24T22:25:10Z
tags: [c, cpp, go, tutorial]
uuid: df6214e0-e408-4254-bd65-49d64e06a93e
---

Occasionally we need to compute the size of an object that does not yet
exist, or a subscript [that may fall out of bounds][bs]. It's easy to miss
the edge cases where results overflow, creating a nasty, subtle bug, [even
in the presence of type safety][go]. Ideally such computations happen in
specialized code, such as *inside* an allocator (`calloc`, `reallocarray`)
and not *outside* by the allocatee (i.e. `malloc`). Mitigations exist with
different trade-offs: arbitrary precision, or using a wider fixed integer
— i.e. 128-bit integers on 64-bit hosts. In the typical case, working only
with fixed size-type integers, I've come up with a set of guidelines to
avoid overflows in the edge cases.

1. Range check *before* computing a result. No exceptions.
2. Do not cast unless you know *a priori* the operand is in range.
3. Never mix unsigned and signed operands. [Prefer signed.][kalb] If you
   need to convert an operand, see (2).
4. Do not add unless you know *a priori* the result is in range.
5. Do not multiply unless you know *a priori* the result is in range.
6. Do not subtract unless you know *a priori* both signed operands
   are non-negative. For unsigned, that the second operand is not larger
   than the first (treat it like (4)).
7. Do not divide unless you know *a prior* the denominator is positive.
8. Make it correct first. Make it fast later, if needed.

These guidelines are also useful when *reviewing* code, tracking in your
mind whether the invariants are held at each step. If not, you've likely
found a bug. If in doubt, use assertions to document and check invariants.
I compiled this list during code review, so for me that's where it's most
useful.

### Range check, then compute

Not strictly necessary when overflow is well-defined, i.e. wraparound, but
it's like defensive driving. It's simpler and clearer to check with basic
arithmetic rather than reason from a wraparound, i.e. a negative result.
Checked math functions are fine, too, if you check the overflow boolean
before accessing the result.

    // bad
    len++;
    if (len <= 0) error();

    // good
    if (len == MAX) error();
    len++;

### Casting

Casting from signed to unsigned, it's as simple as knowing the value is
non-negative, which is likely if you're following (1). If a negative size
has appeared, there's already been a bug earlier in the program, and the
only reasonable course of action is to abort, not handle it like an error.

### Addition

To check if addition will overflow, subtract one of the operands from the
maximum value.

    if (b > MAX - a) error();
    r = a + b;

In pointer arithmetic addition, it's a common mistake to compute the
result pointer then compare it to the bounds. If the check failed, then
the pointer *already* overflowed, i.e. undefined behavior. Major pieces
software, [like glibc][outend], are riddled with such pointer overflows.
(Now that you're aware of it, you'll start noticing it everywhere. Sorry.)

    // bad: never do this
    beg += size;
    if (beg > end) error();

To do this correctly, **check integers not pointers**. Like before,
subtract before adding.

    available = end - beg;
    if (size > available) error();
    beg += size;

Mind mixing signed and unsigned operands for the comparison operator (3),
e.g. an unsigned size on the left and signed difference on the right.

### Multiplication and division

If you're working this out on your own, multiplication seems tricky until
you've internalized a simple pattern. Just as we subtracted before adding,
we need to divide before multiplying. Divide the maximum value by one of
the operands:

    if (a>0 && b>MAX/a) error();
    r = a * b;

It's often permitted for one or both to be zero, so mind divide-by-zero,
which is handled above by the first condition. Sometimes size must be
positive, e.g. the result of the `sizeof` operator in C, in which case we
should prefer it as the denominator.

    assert(size  >  0);
    assert(count >= 0);
    if (count > MAX/size) error();
    total = count * size;

With [arena allocation][arena] there are usually two concerns. First, will
it overflow when computing the total size, i.e. `count * size`? Second, is
the total size within the arena capacity. Naively that's two checks, but
we can kill two birds with one stone: Check both at once by using the
current arena capacity as the maximum value when considering overflow.

    if (count > (end - beg)/size) error();
    total = count * size;

One condition pulling double duty.

### Subtraction

With signed sizes, the negative range is a long "runway" allowing a single
unchecked subtraction before overflow might occur. In essence, we were
exploiting this in order to check addition. The most common mistake with
unsigned subtraction is not accounting for overflow when going below zero.

    // note: signed "i" only
    for (i = end - stride; i >= beg; i -= stride) ...

This loop will go awry if `i` is unsigned and `beg <= stride`.

In special cases we can get away with a second subtraction without an
overflow check if we know some properties of our operands. For example, my
arena allocators look like this:

    padding = -beg & (align - 1);
    if (count >= (end - beg - padding)/size) error();

That's two subtractions in a row. However, `end - beg` describes the size
of a realized object, and `align` is a small constant (e.g. 2^(0–6)). It
could only overflow if the entirety of memory was occupied by the arena.

Bonus, advanced note: This check is actually pulling *triple duty*. Notice
that I used `>=` instead of `>`. The arena can't fill exactly to the brim,
but it handles the extreme edge case where `count` is zero, the arena is
nearly full, but the bump pointer is unaligned. The result of subtracting
`padding` is negative, which rounds to zero by integer division, and would
pass a `>` check. That wouldn't be a problem except that aligning the bump
pointer would break the invariant `beg <= end`.

### Try it for yourself

Next time you're reviewing code that computes sizes or subscripts, bring
the list up and see how well it follows the guidelines. If it misses one,
try to contrive an input that causes an overflow. If it follows guidelines
and you can still contrive such an input, then perhaps the list could use
another item!


[arena]: /blog/2023/09/27/
[bs]: https://research.google/blog/extra-extra-read-all-about-it-nearly-all-binary-searches-and-mergesorts-are-broken/
[go]: https://blog.carlana.net/post/2024/golang-slices-concat/
[kalb]: https://www.youtube.com/watch?v=wvtFGa6XJDU
[outend]: https://sourcegraph.com/search?q=context:global+%22%3E+outend%22+repo:%5Egithub%5C.com/bminor/glibc%24+&patternType=keyword&sm=0
