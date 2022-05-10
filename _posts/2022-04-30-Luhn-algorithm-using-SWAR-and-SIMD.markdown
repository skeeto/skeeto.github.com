---
title: Luhn algorithm using SWAR and SIMD
layout: post
date: 2022-04-30T17:53:05Z
tags: [c, optimization, x86]
uuid: 2bb8fbd6-4197-4799-8258-861d316a7086
---

Ever been so successful that credit card processing was your bottleneck?
Perhaps you've wondered, "If only I could compute check digits three times
faster using the same hardware!" Me neither. But if that ever happens
someday, then this article is for you. I will show how to compute the
[Luhn algorithm][luhn] in parallel using *SIMD within a register*, or
SWAR.

If you want to skip ahead, here's the full source, tests, and benchmark:
[`luhn.c`][src]

The Luhn algorithm isn't just for credit card numbers, but they do make a
nice target for a SWAR approach. The major payment processors use [16
digit numbers][ex] — i.e. 16 ASCII bytes — and typical machines today have
8-byte registers, so the input fits into two machine registers. In this
context, the algorithm works like so:

1. Consider the digits number as an array, and double every other digit
   starting with the first. For example, 6543 becomes 12, 5, 8, 3.

2. Sum individual digits in each element. The example becomes 3 (i.e.
   1+2), 5, 8, 3.

3. Sum the array mod 10. Valid inputs sum to zero. The example sums to 9.

I will implement this algorithm in C with this prototype:

```c
int luhn(const char *s);
```

It assumes the input is 16 bytes and only contains digits, and it will
return the Luhn sum. Callers either validate a number by comparing the
result to zero, or use it to compute a check digit when generating a
number. (Read: You could use SWAR to rapidly generate valid numbers.)

The plan is to process the 16-digit number in two halves, and so first
load the halves into 64-bit registers, which I'm calling `hi` and `lo`:

```c
uint64_t hi =
    (uint64_t)(s[ 0]&255) <<  0 | (uint64_t)(s[ 1]&255) <<  8 |
    (uint64_t)(s[ 2]&255) << 16 | (uint64_t)(s[ 3]&255) << 24 |
    (uint64_t)(s[ 4]&255) << 32 | (uint64_t)(s[ 5]&255) << 40 |
    (uint64_t)(s[ 6]&255) << 48 | (uint64_t)(s[ 7]&255) << 56;
uint64_t lo =
    (uint64_t)(s[ 8]&255) <<  0 | (uint64_t)(s[ 9]&255) <<  8 |
    (uint64_t)(s[10]&255) << 16 | (uint64_t)(s[11]&255) << 24 |
    (uint64_t)(s[12]&255) << 32 | (uint64_t)(s[13]&255) << 40 |
    (uint64_t)(s[14]&255) << 48 | (uint64_t)(s[15]&255) << 56;
```

This looks complicated and possibly expensive, but it's really just an
idiom for loading a little endian 64-bit integer from a buffer. Breaking
it down:

* The input, `*s`, is `char`, which may be signed on some architectures. I
  chose this type since it's the natural type for strings. However, I do
  not want sign extension, so I mask the low byte of the possibly-signed
  result by ANDing with 255. It's as though `*s` was `unsigned char`.

* The shifts assemble the 64-bit result in little endian byte order
  [regardless of the host machine byte order][bof]. In other words, this
  will produce correct results even on big endian hosts.

* I chose little endian since it's the natural byte order for all the
  architectures I care about. Big endian hosts may pay a cost on this load
  (byte swap instruction, etc.). The rest of the function could just as
  easily be computed over a big endian load if I was primarily targeting a
  big endian machine instead.

* I could have used `unsigned long long` (i.e. *at least* 64 bits) since
  no part of this function requires *exactly* 64 bits. I chose `uint64_t`
  since it's succinct, and in practice, every implementation supporting
  `long long` also defines `uint64_t`.

Both GCC and Clang figure this all out and produce perfect code. On
x86-64, just one instruction for each statement:

```nasm
    mov  rax, [rdi+0]
    mov  rdx, [rdi+8]
```

Or, more impressively, loading both using a *single instruction* on ARM64:

```nasm
    ldp  x0, x1, [x0]
```

The next step is to decode ASCII into numeric values. This is [trivial and
common][dl] in SWAR, and only requires subtracting `'0'` (`0x30`). So long
as there is no overflow, this can be done lane-wise.

```c
hi -= 0x3030303030303030;
lo -= 0x3030303030303030;
```

Each byte of the register now contains values in 0–9. Next, double every
other digit. Multiplication in SWAR is not easy, but doubling just means
adding the odd lanes to themselves. I can mask out the lanes that are not
doubled. Regarding the mask, recall that the least significant byte is the
first byte (little endian).

```c
hi += hi & 0x00ff00ff00ff00ff;
lo += lo & 0x00ff00ff00ff00ff;
```

Each byte of the register now contains values in 0–18. Now for the tricky
problem of folding the tens place into the ones place. Unlike 8 or 16, 10
is not a particularly convenient base for computers, especially since SWAR
lacks lane-wide division or modulo. Perhaps a lane-wise [binary-coded
decimal][bcd] could solve this. However, I have a better trick up my
sleeve.

Consider that the tens place is either 0 or 1. In other words, we really
only care if the value in the lane is greater than 9. If I add 6 to each
lane, the 5th bit (value 16) will definitely be set in any lanes that were
previously at least 10. I can use that bit as the tens place.

```c
hi += (hi + 0x0006000600060006)>>4 & 0x0001000100010001;
lo += (lo + 0x0006000600060006)>>4 & 0x0001000100010001;
```

This code adds 6 to the doubled lanes, shifts the 5th bit to the least
significant position in the lane, masks for just that bit, and adds it
lane-wise to the total. Only applying this to doubled lanes is a style
decision, and I could have applied it to all lanes for free.

The astute might notice I've strayed from the stated algorithm. A lane
that was holding, say, 12 now hold 13 rather than 3. Since the final
result of the algorithm is modulo 10, leaving the tens place alone is
harmless, so this is fine.

At this point each lane contains values in 0–19. Now that the tens
processing is done, I can combine the halves into one register with a
lane-wise sum:

```c
hi += lo;
```

Each lane contains values in 0–38. I would have preferred to do this
sooner, but that would have complicated tens place handling. Even if I had
rotated the doubled lanes in one register to even out the sums, some lanes
may still have had a 2 in the tens place.

The final step is a horizontal sum reduction using the typical SWAR
approach. Add the top half of the register to the bottom half, then the
top half of what's left to the bottom half, etc.

```c
hi += hi >> 32;
hi += hi >> 16;
hi += hi >>  8;
```

Before the sum I said each lane was 0–38, so couldn't this sum be as high
as 304 (8x38)? It would overflow the lane, giving an incorrect result.
Fortunately the actual range is 0–18 for normal lanes and 0–38 for doubled
lanes. That's a maximum of 224, which fits in the result lane without
overflow. Whew! I've been tracking the range all along to guard against
overflow like this.

Finally mask the result lane and return it modulo 10:

```c
return (hi&255) % 10;
```

On my machine, SWAR is around 3x faster than a straightforward
digit-by-digit implementation.

### Usage examples

```c
int is_valid(const char *s)
{
    return luhn(s) == 0;
}

void random_credit_card(char *s)
{
    sprintf(s, "%015llu0", rand64()%1000000000000000);
    s[15] = '0' + 10 - luhn(s);
}
```

### SIMD

Conveniently, all the SWAR operations translate directly into SSE2
instructions. If you understand the SWAR version, then this is easy to
follow:

```c
int luhn(const char *s)
{
    __m128i r = _mm_loadu_si128((void *)s);

    // decode ASCII
    r = _mm_sub_epi8(r, _mm_set1_epi8(0x30));

    // double every other digit
    __m128i m = _mm_set1_epi16(0x00ff);
    r = _mm_add_epi8(r, _mm_and_si128(r, m));

    // extract and add tens digit
    __m128i t = _mm_set1_epi16(0x0006);
    t = _mm_add_epi8(r, t);
    t = _mm_srai_epi32(t, 4);
    t = _mm_and_si128(t, _mm_set1_epi8(1));
    r = _mm_add_epi8(r, t);

    // horizontal sum
    r = _mm_sad_epu8(r, _mm_set1_epi32(0));
    r = _mm_add_epi32(r, _mm_shuffle_epi32(r, 2));
    return _mm_cvtsi128_si32(r) % 10;
}
```

On my machine, the SIMD version is around another 3x increase over SWAR,
and so nearly an order of magnitude faster than a digit-by-digit
implementation.

*Update*: Const-me on Hacker News [suggests a better option][cm] for
handling the tens digit in the function above, shaving off 7% of the
function's run time on my machine:

```c
    // if (digit > 9) digit -= 9
    __m128i nine = _mm_set1_epi8(9);
    __m128i gt = _mm_cmpgt_epi8(r, nine);
    r = _mm_sub_epi8(r, _mm_and_si128(gt, nine));
```

*Update*: u/aqrit on reddit has come up with a more optimized SSE2
solution, 12% faster than mine on my machine:

```c
int luhn(const char *s)
{
    __m128i v = _mm_loadu_si128((void *)s);
    __m128i m = _mm_cmpgt_epi8(_mm_set1_epi16('5'), v);
    v = _mm_add_epi8(v, _mm_slli_epi16(v, 8));
    v = _mm_add_epi8(v, m);  // subtract 1 if less than 5
    v = _mm_sad_epu8(v, _mm_setzero_si128());
    v = _mm_add_epi32(v, _mm_shuffle_epi32(v, 2));
    return (_mm_cvtsi128_si32(v) - 4) % 10;
    // (('0' * 24) - 8) % 10 == 4
}
```


[bcd]: https://en.wikipedia.org/wiki/Binary-coded_decimal
[bof]: https://commandcenter.blogspot.com/2012/04/byte-order-fallacy.html
[cm]: https://news.ycombinator.com/item?id=31320853
[dl]: https://lemire.me/blog/2022/01/21/swar-explained-parsing-eight-digits/
[ex]: https://www.paypalobjects.com/en_GB/vhelp/paypalmanager_help/credit_card_numbers.htm
[luhn]: https://en.wikipedia.org/wiki/Luhn_algorithm
[src]: https://github.com/skeeto/scratch/blob/master/misc/luhn.c
