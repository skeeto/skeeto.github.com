---
title: Legitimate Use of Variable Length Arrays
layout: post
date: 2019-10-27T19:58:00Z
tags: [c, optimization]
uuid: acf6af69-f18c-49a6-b3ae-a23ae537da6d
excerpt_separator: <!--more-->
---

*This article was discussed [on Hacker News][hn] and [on reddit][rd]*.

The C99 (ISO/IEC 9899:1999) standard of C introduced a new, powerful
feature called Variable Length Arrays (VLAs). The size of an array with
automatic storage duration (i.e. stack allocated) can be determined at
run time. Each instance of the array may even have a different length.
Unlike `alloca()`, they're a sanctioned form of dynamic stack
allocation.

<!--more-->

At first glance, VLAs seem convenient, useful, and efficient. Heap
allocations have a small cost because the allocator needs to do some
work to find or request some free memory, and typically the operation
must be synchronized since there may be other threads also making
allocations. Stack allocations are trivial and fast by comparison:
Allocation is a matter of bumping the stack pointer, and no
synchronization is needed.

For example, here's a function that non-destructively finds the median
of a buffer of floats:

```c
/* note: nmemb must be non-zero */
float
median(const float *a, size_t nmemb)
{
    float copy[nmemb];
    memcpy(copy, a, sizeof(a[0]) * nmemb);
    qsort(copy, nmemb, sizeof(copy[0]), floatcmp);
    return copy[nmemb / 2];
}
```

It uses a VLA, `copy`, as a temporary copy of the input for sorting. The
function doesn't know at compile time how big the input will be, so it
cannot just use a fixed size. With a VLA, it efficiently allocates
exactly as much memory as needed on the stack.

Well, sort of. If `nmemb` is too large, then the VLA will *silently*
overflow the stack. By silent I mean that the program has no way to
detect it and avoid it. In practice, it can be a lot louder, from a
segmentation fault in the best case, to an exploitable vulnerability in
the worst case: [**stack clashing**][clash]. If an attacker can control
`nmemb`, they might choose a value that causes `copy` to overlap with
other allocations, giving them control over those values as well.

If there's any risk that `nmemb` is too large, it must be guarded.

```c
#define COPY_MAX 4096

float
median(const float *a, size_t nmemb)
{
    if (nmemb > COPY_MAX)
        abort();  /* or whatever */
    float copy[nmemb];
    memcpy(copy, a, sizeof(a[0]) * nmemb);
    qsort(copy, nmemb, sizeof(copy[0]), floatcmp);
    return copy[nmemb / 2];
}
```

However, if `median` is expected to safely accommodate `COPY_MAX`
elements, it may as well *always* allocate an array of this size. If it
can't, then that's not a safe maximum.

```c
float
median(const float *a, size_t nmemb)
{
    if (nmemb > COPY_MAX)
        abort();
    float copy[COPY_MAX];
    memcpy(copy, a, sizeof(a[0]) * nmemb);
    qsort(copy, nmemb, sizeof(copy[0]), floatcmp);
    return copy[nmemb / 2];
}
```

And rather than abort, you might still want to support arbitrary input
sizes:

```c
float
median(const float *a, size_t nmemb)
{
    float buf[COPY_MAX];
    float *copy = buf;
    if (nmemb > COPY_MAX)
        copy = malloc(sizeof(a[0]) * nmemb);
    memcpy(copy, a, sizeof(a[0]) * nmemb);
    qsort(copy, nmemb, sizeof(copy[0]), floatcmp);
    float result = copy[nmemb / 2];
    if (copy != buf)
        free(copy);
    return result;
}
```

Then small inputs are fast, but large inputs still work correctly. This
is called [**small size optimization**][small].

If the correct solution ultimately didn't use a VLA, then what good are
they? In general, VLAs not useful. They're [time bombs][linux]. **VLAs
are nearly always the wrong choice.** You must be careul to check that
they don't exceed some safe maximum, and there's no reason not to always
use the maximum. This problem was realized for the C11 standard (ISO/IEC
9899:2011) where VLAs were made optional. A program containing a VLA
will not necessarily compile on a C11 compiler.

Some purists also object to a special exception required for VLAs: The
`sizeof` operator may evaluate its operand, and so it does not always
evaluate to compile-time constant. If the operand contains a VLA, then
the result depends on a run-time value.

Because they're optional, it's best to avoid even *trivial* VLAs like
this:

```c
float
median(const float *a, size_t nmemb)
{
    int max = 4096;
    if (nmemb > max)
        abort();
    float copy[max];
    memcpy(copy, a, sizeof(a[0]) * nmemb);
    qsort(copy, nmemb, sizeof(copy[0]), floatcmp);
    return copy[nmemb / 2];
}
```

It's easy to prove that the array length is always 4096, but technically
this is still a VLA. That would still be true even if `max` were `const
int`, because the array length still isn't a constant integral
expression.

### VLA overhead

Finally, there's also the problem that VLAs just aren't as efficient as
you might hope. A function that does dynamic stack allocation requires
additional stack management. It must track additional memory addresses
and will require extra instructions.

```c
void
fixed(int n)
{
    if (n <= 1<<14) {
        volatile char buf[1<<14];
        buf[n - 1] = 0;
    }
}

void
dynamic(int n)
{
    if (n <= 1<<14) {
        volatile char buf[n];
        buf[n - 1] = 0;
    }
}
```

Compiled with `gcc -Os` and viewed with `objdump -d -Mintel`:

```
0000000000000000 <fixed>:
   0:	81 ff 00 40 00 00    	cmp    edi,0x4000
   6:	7f 19                	jg     21 <fixed+0x21>
   8:	ff cf                	dec    edi
   a:	48 81 ec 88 3f 00 00 	sub    rsp,0x3f88
  11:	48 63 ff             	movsxd rdi,edi
  14:	c6 44 3c 88 00       	mov    BYTE PTR [rsp+rdi*1-0x78],0x0
  19:	48 81 c4 88 3f 00 00 	add    rsp,0x3f88
  20:	c3                   	ret    
  21:	c3                   	ret    

0000000000000022 <dynamic>:
  22:	81 ff 00 40 00 00    	cmp    edi,0x4000
  28:	7f 23                	jg     4d <dynamic+0x2b>
  2a:	55                   	push   rbp
  2b:	48 63 c7             	movsxd rax,edi
  2e:	ff cf                	dec    edi
  30:	48 83 c0 0f          	add    rax,0xf
  34:	48 63 ff             	movsxd rdi,edi
  37:	48 83 e0 f0          	and    rax,0xfffffffffffffff0
  3b:	48 89 e5             	mov    rbp,rsp
  3e:	48 89 e2             	mov    rdx,rsp
  41:	48 29 c4             	sub    rsp,rax
  44:	c6 04 3c 00          	mov    BYTE PTR [rsp+rdi*1],0x0
  48:	48 89 d4             	mov    rsp,rdx
  4b:	c9                   	leave  
  4c:	c3                   	ret    
  4d:	c3                   	ret    
```

Note the use of a base pointer, `rbp` and `leave`, in the second
function in order to dynamically track the stack frame. (Hmm, in both
cases GCC could easily shave off the extra `ret` at the end of each
function. Missed optimization?)

The story is even worse when stack clash protection is enabled
(`-fstack-clash-protection`). The compiler generates extra code to probe
every page of allocation in case one of those pages is a guard page.
That's also more complex when the allocation is dynamic. The VLA version
more than doubles in size (from 44 bytes to 101 bytes)!

### Safe and useful variable length arrays

There is one convenient, useful, and safe form of VLAs: a pointer to a
VLA. It's convenient and useful because it makes some expressions
simpler. It's safe because there's no arbitrary stack allocation.

Pointers to arrays are a rare sight in C code, whether variable length
or not. That's because, the vast majority of the time, C programmers
implicitly rely on *array decay*: arrays quietly "decay" into pointers
to their first element the moment you do almost anything with them. Also
because they're really awkward to use.

For example, the function `sum3` takes a pointer to an array of exactly
three elements.

```c
int
sum3(int (*array)[3])
{
    return (*array)[0] + (*array)[1] + (*array)[2];
}
```

The parentheses are necessary because, without them, `array` would be an
array of pointers — a type far more common than a pointer to an array.
To index into the array, first the pointer to the array must be
dereferenced to the array value itself, then this intermediate array is
indexed triggering array decay. Conceptually there's quite a bit to it,
but, in practice, it's all as efficient as the conventional approach to
`sum3` that accepts a plain `int *`.

The caller must take the address of an array of exactly the right
length:

```c
int buf[] = {1, 2, 4};
int r = sum3(&buf);
```

Or if dynamically allocating the array:

```c
int (*array)[3] = malloc(sizeof(*array));
(*array)[0] = 1;
(*array)[1] = 2;
(*array)[2] = 4;
int r = sum3(array);
free(array);
```

The mandatory parentheses and strict type requirements make this awkward
and rarely useful. However, with VLAs perhaps it's worth the trouble!
Consider an NxN matrix expressed using a pointer to a VLA:

```c
int n = /* run-time value */;
/* TODO: Check for integer overflow. See note. */
float (*identity)[n][n] = malloc(sizeof(*identity));
if (identity) {
    for (int y = 0; y < n; y++) {
        for (int x = 0; x < n; x++) {
            (*identity)[y][x] = x == y;
        }
    }
}
```

When indexing, the parentheses are weird, but the indices have the
convenient `[y][x]` format. The non-VLA alternative is to compute a 1D
index manually from 2D indices (`y*n+x`):

```c
int n = /* run-time value */;
/* TODO: Check for integer overflow. */
float *identity = malloc(sizeof(*identity) * n * n);
if (identity) {
    for (int y = 0; y < n; y++) {
        for (int x = 0; x < n; x++) {
            identity[y*n + x] = x == y;
        }
    }
}
```

Note: What's the behavior in the VLA version when `n` is so large that
`sizeof(*identity)` doesn't fit in a `size_t`? I couldn't find anything
in the standard about it, though I bet it's undefined behavior. Neither
GCC and Clang check for overflow and, when it occurs, the overflow is
silent. Neither the undefined behavior sanitizer nor address sanitizer
complain when this happens.

**Update**: [bru del pointed out][simple] that these multi-dimensional
VLAs can be simplified such that the parenthesis may be omitted when
indexing. The trick is to omit the first dimension from the VLA
expression:

```c
float (*identity)[n] = malloc(sizeof(*identity) * n);
if (identity) {
    for (int y = 0; y < n; y++) {
        for (int x = 0; x < n; x++) {
            identity[y][x] = x == y;
        }
    }
}
```

So VLAs *might* be worth the trouble when using pointers to
multi-dimensional, dynamically-allocated arrays. However, I'm still
judicious about their use due to reduced portability. As a practical
example, MSVC famously does not, and likely will never will, support
VLAs.


[clash]: /blog/2017/06/21/
[hn]: https://news.ycombinator.com/item?id=21375580
[linux]: https://www.phoronix.com/scan.php?page=news_item&px=Linux-Kills-The-VLA
[rd]: https://old.reddit.com/r/programming/comments/dz1fau/variable_length_arrays_in_c_are_nearly_always_the/
[simple]: https://lists.sr.ht/~skeeto/public-inbox/%3CCAP-ht1CQKVByZt1EXOb3J7TF%3DMcCKi%3DEtzjEH+CaEsPtvY5%3Djg%40mail.gmail.com%3E
[small]: /blog/2016/10/07/
