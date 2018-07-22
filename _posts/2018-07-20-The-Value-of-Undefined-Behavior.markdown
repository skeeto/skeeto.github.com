---
title: The Value of Undefined Behavior
layout: post
date: 2018-07-20T21:31:18Z
tags: [c, cpp, x86, optimization]
uuid: 9758e9ea-46b6-3904-5166-52c7e6922892
---

In several places, the C and C++ language specifications use a
curious, and fairly controversial, phrase: *undefined behavior*. For
certain program constructs, the specification prescribes no specific
behavior, instead allowing [anything to happen][nd]. Such constructs
are considered erroneous, and so the result depends on the particulars
of the platform and implementation. The original purpose of undefined
behavior was for implementation flexibility. In other words, it's
slack that allows a compiler to produce appropriate and efficient code
for its target platform.

Specifying a particular behavior would have put unnecessary burden on
implementations — especially in the earlier days of computing — making
for inefficient programs on some platforms. For example, if the result
of dereferencing a null pointer was defined to trap — to cause the
program to halt with an error — then platforms that do not have
hardware trapping, such as those without virtual memory, would be
required to instrument, in software, each pointer dereference.

In the 21st century, undefined behavior has taken on a somewhat
different meaning. Optimizers use it — or *ab*use it depending on your
point of view — to lift [constraints][lex] that would otherwise
inhibit more aggressive optimizations. It's not so much a
fundamentally different application of undefined behavior, but it does
take the concept to an extreme.

The reasoning works like this: A program that evaluates a construct
whose behavior is undefined cannot, by definition, have any meaningful
behavior, and so that program would be useless. As a result,
[compilers assume programs never invoke undefined behavior][llvm] and
use those assumptions to prove its optimizations.

Under this newer interpretation, mistakes involving undefined behavior
are more [punishing][func] and [surprising][bite] than before. Programs
that *seem* to make some sense when run on a particular architecture may
actually compile into a binary with a security vulnerability due to
conclusions reached from an analysis of its undefined behavior.

This can be frustrating if your programs are intended to run on a very
specific platform. In this situation, all behavior really *could* be
locked down and specified in a reasonable, predictable way. Such a
language would be like an extended, less portable version of C or C++.
But your toolchain still insists on running your program on the
*abstract machine* rather than the hardware you actually care about.
However, **even in this situation undefined behavior can still be
desirable**. I will provide a couple of examples in this article.

### Signed integer overflow

To start things off, let's look at one of my all time favorite examples
of useful undefined behavior, a situation involving signed integer
overflow. The result of a signed integer overflow isn't just
unspecified, it's undefined behavior. Full stop.

This goes beyond a simple matter of whether or not the underlying
machine uses a two's complement representation. From the perspective of
the abstract machine, just the act a signed integer overflowing is
enough to throw everything out the window, even if the overflowed result
is never actually used in the program.

On the other hand, unsigned integer overflow is defined — or, more
accurately, defined to wrap, *not* overflow. Both the undefined signed
overflow and defined unsigned overflow are useful in different
situations.

For example, here's a fairly common situation, much like what [actually
happened in bzip2][bz2]. Consider this function that does substring
comparison:

```c
int
cmp_signed(int i1, int i2, unsigned char *buf)
{
    for (;;) {
        int c1 = buf[i1];
        int c2 = buf[i2];
        if (c1 != c2)
            return c1 - c2;
        i1++;
        i2++;
    }
}

int
cmp_unsigned(unsigned i1, unsigned i2, unsigned char *buf)
{
    for (;;) {
        int c1 = buf[i1];
        int c2 = buf[i2];
        if (c1 != c2)
            return c1 - c2;
        i1++;
        i2++;
    }
}
```

In this function, the indices `i1` and `i2` will always be some small,
non-negative value. Since it's non-negative, it should be `unsigned`,
right? Not necessarily. That puts an extra constraint on code generation
and, at least on x86-64, makes for a less efficient function. Most of
the time you actually *don't* want overflow to be defined, and instead
allow the compiler to assume it just doesn't happen.

The constraint is that **the behavior of `i1` or `i2` overflowing as an
unsigned integer is defined, and the compiler is obligated to implement
that behavior.** On x86-64, where `int` is 32 bits, the result of the
operation must be truncated to 32 bits one way or another, requiring
extra instructions inside the loop.

In the signed case, incrementing the integers cannot overflow since that
would be undefined behavior. This permits the compiler to perform the
increment only in 64-bit precision without truncation if it would be
more efficient, which, in this case, it is.

Here's the output of Clang 6.0.0 with `-Os` on x86-64. Pay close
attention to the main loop, which I named `.loop`:

```nasm
cmp_signed:
        movsxd rdi, edi             ; use i1 as a 64-bit integer
        mov    al, [rdx + rdi]
        movsxd rsi, esi             ; use i2 as a 64-bit integer
        mov    cl, [rdx + rsi]
        jmp    .check

.loop:  mov    al, [rdx + rdi + 1]
        mov    cl, [rdx + rsi + 1]
        inc    rdx                  ; increment only the base pointer
.check: cmp    al, cl
        je     .loop

        movzx  eax, al
        movzx  ecx, cl
        sub    eax, ecx             ; return c1 - c2
        ret

cmp_unsigned:
        mov    eax, edi
        mov    al, [rdx + rax]
        mov    ecx, esi
        mov    cl, [rdx + rcx]
        cmp    al, cl
        jne    .ret
        inc    edi
        inc    esi

.loop:  mov    eax, edi             ; truncated i1 overflow
        mov    al, [rdx + rax]
        mov    ecx, esi             ; truncated i2 overflow
        mov    cl, [rdx + rcx]
        inc    edi                  ; increment i1
        inc    esi                  ; increment i2
        cmp    al, cl
        je     .loop

.ret:   movzx  eax, al
        movzx  ecx, cl
        sub    eax, ecx
        ret
```

As unsigned values, `i1` and `i2` can overflow independently, so they
have to be handled as independent 32-bit unsigned integers. As signed
values they can't overflow, so they're treated as if they were 64-bit
integers and, instead, the pointer, `buf`, is incremented without
concern for overflow. The signed loop is much more efficient (5
instructions versus 8).

The signed integer helps to communicate the *narrow contract* of the
function — the limited range of `i1` and `i2` — to the compiler. In a
variant of C where signed integer overflow is defined (i.e. `-fwrapv`),
this capability is lost. In fact, using `-fwrapv` deoptimizes the signed
version of this function.

Side note: Using `size_t` (an unsigned integer) is even better on x86-64
for this example since it's already 64 bits and the function doesn't
need the initial sign/zero extension. However, this might simply move
the sign extension out to the caller.

### Strict aliasing

Another controversial undefined behavior is [*strict aliasing*][sa].
This particular term doesn't actually appear anywhere in the C
specification, but it's the popular name for C's aliasing rules. In
short, variables with types that aren't compatible are not allowed to
alias through pointers.

Here's the classic example:

```c
int
foo(int *a, int *b)
{
    *b = 0;    // store
    *a = 1;    // store
    return *b; // load
}
```

Naively one might assume the `return *b` could be optimized to a simple
`return 0`. However, since `a` and `b` have the same type, the compiler
must consider the possibility that they alias — that they point to the
same place in memory — and must generate code that works correctly under
these conditions.

If `foo` has a narrow contract that forbids `a` and `b` to alias, we
have a couple of options for helping our compiler.

First, we could manually resolve the aliasing issue by returning 0
explicitly. In more complicated functions this might mean making local
copies of values, working only with those local copies, then storing the
results back before returning. Then aliasing would no longer matter.

```c
int
foo(int *a, int *b)
{
    *b = 0;
    *a = 1;
    return 0;
}
```

Second, C99 introduced a `restrict` qualifier to communicate to the
compiler that pointers passed to functions cannot alias. For example,
the pointers to `memcpy()` are qualified with `restrict` as of C99.
Passing aliasing pointers through `restrict` parameters is undefined
behavior, e.g. this doesn't ever happen as far as a compiler is
concerned.

```c
int foo(int *restrict a, int *restrict b);
```

The third option is to design an interface that uses incompatible
types, exploiting strict aliasing. This happens all the time, usually
by accident. For example, `int` and `long` are never compatible even
when they have the same representation.

```c
int foo(int *a, long *b);
```

If you use an extended or modified version of C without strict
aliasing (`-fno-strict-aliasing`), then the compiler must assume
everything aliases all the time, generating a lot more precautionary
loads than necessary.

What [irritates][linus] a lot of people is that compilers will still
apply the strict aliasing rule even when it's trivial for the compiler
to prove that aliasing is occurring:

```c
/* note: forbidden */
long a;
int *b = (int *)&a;
```

It's not just a simple matter of making exceptions for these cases.
The language specification would need to define all the rules about
when and where incompatible types are permitted to alias, and
developers would have to understand all these rules if they wanted to
take advantage of the exceptions. It can't just come down to trusting
that the compiler is smart enough to see the aliasing when it's
sufficiently simple. It would need to be carefully defined.

Besides, there are probably [conforming, portable solutions][endian]
that, with contemporary compilers, will safely compile to the efficient
code you actually want anyway.

There *is* one special exception for strict aliasing: `char *` is
allowed to alias with anything. This is important to keep in mind both
when you intentionally want aliasing, but also when you want to avoid
it. Writing through a `char *` pointer could force the compiler to
generate additional, unnecessary loads.

In fact, there's a whole dimension to strict aliasing that, even today,
no compiler yet exploits: `uint8_t` is not necessarily `unsigned char`.
That's just one possible `typedef` definition for it. It could instead
`typedef` to, say, some internal `__byte` type.

In other words, technically speaking, `uint8_t` does not have the strict
aliasing exemption. If you wanted to write bytes to a buffer without
worrying the compiler about aliasing issues with other pointers, this
would be the tool to accomplish it. Unfortunately there's far too much
existing code that violates this part of strict aliasing that no
toolchain is [willing to exploit it][u8] for optimization purposes.

### Other undefined behaviors

Some kinds of undefined behavior don't have performance or portability
benefits. They're only there to make the compiler's job a little
simpler. Today, most of these are caught trivially at compile time as
syntax or semantic issues (i.e. a pointer cast to a float).

Some others are obvious about their performance benefits and don't
require much explanation. For example, it's undefined behavior to
index out of bounds (with some special exceptions for one past the
end), meaning compilers are not obligated to generate those checks,
instead relying on the programmer to arrange, by whatever means, that
it doesn't happen.

Undefined behavior is like nitro, a dangerous, volatile substance that
makes things go really, really fast. You could argue that it's *too*
dangerous to use in practice, but the aggressive use of undefined
behavior is [not without merit][intimacy].


[bite]: /blog/2018/05/01/
[bz2]: https://www.youtube.com/watch?v=yG1OZ69H_-o&t=38m18s
[endian]: https://commandcenter.blogspot.com/2012/04/byte-order-fallacy.html
[func]: https://kristerw.blogspot.com/2017/09/why-undefined-behavior-may-call-never.html
[intimacy]: http://thoughtmesh.net/publish/367.php
[lex]: /blog/2016/12/22/
[linus]: https://lkml.org/lkml/2003/2/26/158
[llvm]: http://blog.llvm.org/2011/05/what-every-c-programmer-should-know.html
[nd]: http://www.catb.org/jargon/html/N/nasal-demons.html
[sa]: https://gist.github.com/shafik/848ae25ee209f698763cffee272a58f8
[u8]: https://gcc.gnu.org/bugzilla/show_bug.cgi?id=66110
