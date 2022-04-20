---
title: 'Chunking Optimizations: Let the Knife Do the Work'
layout: post
date: 2019-12-09T22:37:55Z
tags: [c, cpp, optimization, x86]
uuid: 961086fa-46af-42d4-bd69-6f4a326a1505
---

There's an old saying, [*let the knife do the work*][gr]. Whether
preparing food in the kitchen or whittling a piece of wood, don't push
your weight into the knife. Not only is it tiring, you're much more
likely to hurt yourself. Use the tool properly and little force will be
required.

The same advice also often applies to compilers.

Suppose you need to XOR two, non-overlapping 64-byte (512-bit) blocks of
data. The simplest approach would be to do it a byte at a time:

```c
/* XOR src into dst */
void
xor512a(void *dst, void *src)
{
    unsigned char *pd = dst;
    unsigned char *ps = src;
    for (int i = 0; i < 64; i++) {
        pd[i] ^= ps[i];
    }
}
```

Maybe you benchmark it or you look at the assembly output, and the
results are disappointing. Your compiler did *exactly* what you asked
of it and produced code that performs 64 single-byte XOR operations
(GCC 9.2.0, x86-64, `-Os`):

```nasm
xor512a:
        xor    eax, eax
.L0:    mov    cl, [rsi+rax]
        xor    [rdi+rax], cl
        inc    rax
        cmp    rax, 64
        jne    .L0
        ret
```

The target architecture has wide registers so it could be doing *at
least* 8 bytes at a time. Since your compiler isn't doing it, you
decide to chunk the work into 8 byte blocks yourself in an attempt to
manually implement a *chunking operation*. Here's some [real world
code][reddit] that does so:

```c
/* WARNING: Broken, do not use! */
void
xor512b(void *dst, void *src)
{
    uint64_t *pd = dst;
    uint64_t *ps = src;
    for (int i = 0; i < 8; i++) {
        pd[i] ^= ps[i];
    }
}
```

You check the assembly output of this function, and it looks much
better. It's now processing 8 bytes at a time, so it should be about 8
times faster than before.

```nasm
xor512b:
        xor    eax, eax
.L0:    mov    rcx, [rsi+rax*8]
        xor    [rdi+rax*8], rcx
        inc    rax
        cmp    rax, 8
        jne    .L0
        ret
```

Still, this machine has 16-byte wide registers (SSE2 `xmm`), so there
could be another doubling in speed. Oh well, this is good enough, so you
plug it into your program. But something strange happens: **The output
is now wrong!**

```c
int
main(void)
{
    uint32_t dst[32] = {
        1, 2, 3, 4, 5, 6, 7, 8,
        9, 10, 11, 12, 13, 14, 15, 16
    };
    uint32_t src[32] = {
        1, 4, 9, 16, 25, 36, 49, 64,
        81, 100, 121, 144, 169, 196, 225, 256,
    };
    xor512b(dst, src);
    for (int i = 0; i < 16; i++) {
        printf("%d\n", (int)dst[i]);
    }
}
```

Your program prints 1..16 as if `xor512b()` was never called. You check
over everything a dozen times, and you can't find anything wrong. Even
crazier, if you disable optimizations then the bug goes away. It must be
some kind of compiler bug!

Investigating a bit more, you learn that the `-fno-strict-aliasing`
option also fixes the bug. That's because this program violates C strict
aliasing rules. An array of `uint32_t` was accessed as a `uint64_t`. As
an [important optimization][ub], compilers are allowed to assume such
variables do not alias and generate code accordingly. Otherwise every
memory store could potentially modify any variable, which limits the
compiler's ability to produce decent code.

The original version is fine because `char *`, including both `signed`
and `unsigned`, has a special exemption and may alias with anything. For
the same reason, using `char *` unnecessarily can also make your
programs slower.

What could you do to keep the chunking operation while not running afoul
of strict aliasing? Counter-intuitively, you could use `memcpy()`. Copy
the chunks into legitimate, local `uint64_t` variables, do the work, and
copy the result back out.

```c
void
xor512c(void *dst, void *src)
{
    for (int i = 0; i < 8; i++) {
        uint64_t buf[2];
        memcpy(buf + 0, (char *)dst + i*8, 8);
        memcpy(buf + 1, (char *)src + i*8, 8);
        buf[0] ^= buf[1];
        memcpy((char *)dst + i*8, buf, 8);
    }
}
```

Since `memcpy()` is a built-in function, your compiler knows its
semantics and can ultimately elide all that copying. The assembly
listing for `xor512c` is identical to `xor512b`, but it won't go haywire
when integrated into a real program.

It works and it's correct, but you can still do much better than this!

### Letting your compiler do the work

The problem is you're forcing the knife and not letting it do the work.
There's a constraint on your compiler that hasn't been considered: It
must work correctly for overlapping inputs.

```c
char buf[74] = {...};
xor512a(buf, buf + 10);
```

In this situation, the byte-by-byte and chunked versions of the function
will have different results. That's exactly why your compiler can't do
the chunking operation itself. However, *you don't care about this
situation* because the inputs never overlap.

Let's revisit the first, simple implementation, but this time being
smarter about it. The `restrict` keyword indicates that the inputs
will not overlap, freeing your compiler of this unwanted concern.

```c
void
xor512d(void *restrict dst, void *restrict src)
{
    unsigned char *pd = dst;
    unsigned char *ps = src;
    for (int i = 0; i < 64; i++) {
        pd[i] ^= ps[i];
    }
}
```

(Side note: Adding `restrict` to the manually chunked function,
`xor512b()`, will not fix it. Using `restrict` can never make an
incorrect program correct.)

Compiled with GCC 9.2.0 and `-O3`, the resulting unrolled code
processes 16-byte chunks at a time (`pxor`):

```nasm
xor512d:
        movdqu  xmm0, [rdi+0x00]
        movdqu  xmm1, [rsi+0x00]
        movdqu  xmm2, [rsi+0x10]
        movdqu  xmm3, [rsi+0x20]
        pxor    xmm0, xmm1
        movdqu  xmm4, [rdi+0x30]
        movups  [rdi+0x00], xmm0
        movdqu  xmm0, [rdi+0x10]
        pxor    xmm0, xmm2
        movups  [rdi+0x10], xmm0
        movdqu  xmm0, [rdi+0x20]
        pxor    xmm0, xmm3
        movups  [rdi+0x20], xmm0
        movdqu  xmm0, [rsi+0x30]
        pxor    xmm0, xmm4
        movups  [rdi+0x30], xmm0
        ret
```

Compiled with Clang 9.0.0 with AVX-512 enabled in the target
(`-mavx512bw`), *it does the entire operation in a single, big chunk!*

```nasm
xor512d:
        vmovdqu64   zmm0, [rdi]
        vpxorq      zmm0, zmm0, [rsi]
        vmovdqu64   [rdi], zmm0
        vzeroupper
        ret
```

"Letting the knife do the work" means writing a correct program and
lifting unnecessary constraints so that the compiler can use whatever
chunk size is appropriate for the target.


[gr]: https://www.youtube.com/watch?v=bTee6dKpDB0
[reddit]: https://old.reddit.com/r/C_Programming/comments/e83jzk/strange_gcc_compiler_bug_when_using_o2_or_higher/
[ub]: /blog/2018/07/20/#strict-aliasing
