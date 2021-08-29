---
title: Minimalist C Libraries
layout: post
date: 2018-06-10T01:40:16Z
tags: [c]
uuid: bb1ed0bd-ef15-3710-ad47-2365bda0822b
---

In the past year I've written a number of minimalist C libraries,
particularly header libraries. The distinction for "minimalist" is, of
course, completely arbitrary and subjective. My definition in this
context isn't about the library's functionality being [stupidly
trivial][stupid] or even necessarily simple. I'm talking about
interface (API) complexity and the library's run time requirements.
Complex functionality can, in some cases, be tucked behind a simple
interface.

In this article I'll give my definition for minimalist C API, then take
you through some of my own recent examples.

### Minimalist properties

A minimalist C library would generally have these properties.

#### (1) Small number of functions, perhaps even as little as one.

This one's pretty obvious. More functions means more surface area in
the interface. Since these functions typically interact, the
relationship between complexity and number of functions will be
superlinear.

#### (2) No dynamic memory allocations.

The library mustn't call `malloc()` internally. It's up to the caller
to allocate memory for the library. What's nice about this is that
it's completely up to the application exactly how memory is allocated.
Maybe it's using a custom allocator, or it's not [linked against the
standard library][free].

A common approach is for the application to provide allocation functions
to the library — e.g. function pointers at run time, or define functions
with specific, expected names. The library would call these instead of
`malloc()` and `free()`. While that's perfectly reasonable, it's not
really *minimalist*, so I'm not including this technique.

Instead a minimalist API is designed such that it's natural for the
application to make the allocations itself. Perhaps the library only
needs a single, fixed allocation for all its operations. Or maybe the
application specifies its requirements and the library communicates
how much memory is needed to meet those requirements. I'll give
specific examples shortly.

One nice result of this property is that it eliminates one of the
common failure conditions: the out of memory error. If the library
doesn't allocate memory, then it can't run out of it!

Another convenient, minor outcome is the lack of casts from `void *` to
the appropriate type (e.g. on the return from `malloc()`). These casts
are implicit in C but must be made explicit in C++. Often, completely by
accident, my minimalist C libraries can be compiled as C++ without any
changes. This is only a minor benefit since these casts could be made
explicit in C, too, if C++ compatibility was desired. It's just ugly.

#### (3) No input or output.

In simple terms, the library mustn't use functions from `stdio.h` —
with the exception of the `sprintf()` family. Like with memory
allocation, it leaves input and output to the application, letting it
decide exactly how, where, and when information comes and goes.

Like with memory allocation, maybe the application prefers not to use
the C standard library's buffered IO. Perhaps the application is using
[cooperative][thr] or green threads, and it would be bad for the
library to block internally on IO.

Also like avoiding memory allocation, a library that doesn't perform IO
can't have IO errors. Combined, this means it's quite possible that *a
minimalist library may have no error cases at all*. Eliminating those
error handling paths makes the library a lot simpler. The one major
error condition left that's difficult to eliminate are those [pesky
integer overflow checks][int].

Communicating IO preferences to libraries can be a real problem with
C, since the standard library lacks generic input and output. Putting
`FILE *` pointers directly into an API mingles it with the C standard
library in potentially bad ways. Passing file names as strings is an
option, but this limits IO to files — versus, say, sockets. On POSIX
systems, at least it could talk about IO in terms of file descriptors,
but even that's not entirely flexible — e.g. output to a memory
buffer, or anything not sufficiently file-like.

Again, a common way to deal with this is for the application to
provide IO function pointers to the library. But a minimalist
library's API would be designed such that not even this is needed,
instead operating strictly on buffers. I'll also have a couple
examples of this shortly.

With IO and memory allocation out of the picture, another frequent,
accidental result is no dependency on the C standard library. The only
significant functionality left in the standard library are the
mathematical functions (`math.h`), [float parsing][flt], and a few of
the string functions (`string.h`), like `memset()` and `memmove()`.
These are valuable since they're [handled specially by the
compiler][bite].

#### (4) Define at most one structure, and perhaps even none.

More types means more complexity, perhaps even more so than having lots
of functions. Some minimalist libraries can be so straightforward that
they can operate solely on simple, homogeneous buffers. I'll show some
examples of this, too.

As I said initially, minimalism is about interface, not implementation.
The library is free to define as many structures internally as it needs
since the application won't be concerned with them.

One common way to avoid complicated types in an API is to make them
*opaque*. The structures aren't defined in the API, and instead the
application only touches pointers, making them like handles.

```c
struct foo;

struct foo *foo_create(...);
int         foo_method(struct foo *, ...);
void        foo_destroy(struct foo *);
```

However, this is difficult to pull off when the library doesn't allocate
its own memory.

### Bitmap library

The first example is a library for creating bitmap (BMP) images. As you
may already know, I [strongly prefer Netpbm][ppm], which is so simple
that it doesn't even need a library. But nothing is quite so universally
supported as BMP.

**[24-bit BMP (Bitmap) ANSI C header library][bmp]**

This library is a perfect example of minimalist properties 2, 3, and 4.
It also doesn't use any of the C standard library, though only by
accident.

It's not a general purpose BMP library. It only supports 24-bit true
color, ignoring most BMP features such as palettes. Color is
represented as a 24-bit integer, packed `0xRRGGBB`.

```c
unsigned long bmp_size(long width, long height);
void          bmp_init(void *, long width, long height);
void          bmp_set(void *, long x, long y, unsigned long color);
unsigned long bmp_get(const void *, long x, long y);
```

Strictly speaking, even the `bmp_get()` function could be tossed since
the library is not intended to load external bitmap images. The
application really shouldn't *need* to read back previously set pixels.

There is no allocation, no IO, and no data structures. The application
indicates the dimensions of image it wants to create, and the library
says how large of a buffer it needs. The remaining functions all operate
on this opaque buffer. To write the image out, the application only
needs to dump the buffer to a file.

Here's a complete, strict error checking example of its usage:

```c
#define RED   0xff0000UL
#define BLUE  0x0000ffUL

unsigned long size = bmp_size(width, height);
if (!size || size > SIZE_MAX) die("invalid dimensions");

void *bmp = calloc(size, 1);
if (!bmp) die("out of memory");
bmp_init(bmp, width, height);

/* Checkerboard pattern */
for (long y = 0; y < height; y++)
    for (long x = 0; x < width; x++)
        bmp_set(bmp, x, y, x % 2 == y % 2 ? RED : BLUE);

if (!fwrite(bmp, size, 1, out))
    die("output error");

free(bmp);
```

The only library function that can fail is `bmp_size()`. When the
given image dimensions would overflow one of the BMP header fields, it
returns zero to indicate as such.

In `bmp_set()`, how does it know the dimensions of the image so that
it can find the pixel? It reads that from the buffer just like a BMP
reader would — *and* in a [endian-agnostic manner][endian]. There are
no bounds checks — that's the caller's job — so it only needs to read
the image's *width* in order to find the pixel's location.

Since IO is under control of the application, it can always choose load
the original buffer contents *back* from a file, allowing a minimal sort
of BMP loading. However, this only works for *trusted* input as there
are no validation checks on the buffer.

### 32-bit integer hash set library

The second example is an integer hash set library. It uses closed
hashing. I initially wrote this for [r/dailyprogrammer][dp] solution and
then formalized it into a little reusable library.

**[C99 32-bit integer hash set header library][set32]**

Here's the entire API:

```c
int  set32_z(uint32_t max);
void set32_insert(uint32_t *table, int z, uint32_t v);
void set32_remove(uint32_t *table, int z, uint32_t v);
int  set32_contains(uint32_t *table, int z, uint32_t v);
```

Again, it's a good example of properties 2, 3, and 4. Like the BMP
library, the application indicates the maximum number of integers it
will store in the hash set, and the library returns the *power of two*
number of `uint32_t` it needs to allocate (and zero-initialize).

In this API I'm just barely skirting not defining a data structure.
The caller must pass both the table pointer and the power of two size,
and these two values would normally be bundled together into a
structure.

```c
int z = set32_z(max);
unsigned long long n = 1ULL << z;
if (n > SIZE_MAX) die("table too large");
uint32_t *table = calloc(sizeof(*table), n);
if (!table) die("out of memory");

set32_insert(table, z, value);

if (set32_contains(table, z, value))
    /* ... */;

set32_remove(table, z, value);

free(table);
```

Iteration is straightforward, which is why it's not in the API: visit
each element in the allocated buffer. Zeroes are empty slots.

If a different maximum number of elements is needed, the application
initializes a new, separate table, then iterates over the old table
inserting each integer in turn.

Perhaps the most interesting part of the API is that *it has no errors*.
No function can fail.

Also, like the BMP library, it accidentally doesn't use the standard
library, except for a `typedef` from `stdint.h`.

### Fantasy name generator

Nearly a decade ago I [cloned in Perl][perl] the [RinkWorks Fantasy Name
Generator][rw]. This version was slow and terrible, and I'm sometimes
tempted to just delete it.

A few years later I [rewrote it in JavaScript][js] using an entirely
different approach. In order to improve performance, it has a template
compilation step. The compiled template is a hierarchical composition of
simple generator objects. It's much faster, and easily enabled some
extensions to the syntax.

[Germán Méndez Bravo ported the JavaScript version to C++][cpp]. This
C++ implementation [was recently adopted][merge] into [IVAN][ivan], a
roguelike game.

This recent commotion made me realize something: *I hadn't yet
implemented it in C!* So I did.

**[Fantasy name generator ANSI C header library][fn]**

The entire API is just a single function with four possible return
values. It's a perfect example of minimalist property 1.

```c
#define NAMEGEN_SUCCESS    0
#define NAMEGEN_TRUNCATED  1  /* Output was truncated */
#define NAMEGEN_INVALID    2  /* Pattern is invalid */
#define NAMEGEN_TOO_DEEP   3  /* Exceeds maximum nesting depth */

int namegen(char *dest,
            size_t len,
            const char *pattern,
            unsigned long *seed);
```

There's no template compilation step, and it generates names straight
from the template.

There are three kinds of errors.

1. If the output buffer wasn't large enough, it warns about the name
   being truncated.

2. The template could be invalid — e.g. incorrectly paired brackets.

3. The template could have too much nesting. I decided to hard code the
   maximum nesting depth to a generous 32 levels. This limitation makes
   the generator a lot simpler without any practical impact. It also
   protects against unbounded memory usage — particularly stack
   overflows — by arbitrarily complex patterns. This means it's
   perfectly safe to generate names from untrusted, arbitrarily long
   input patterns.

Here's a usage example:

```c
char name[64];
unsigned long seed = 0xb9584b61UL;
namegen(name, sizeof(name), "!sV'i (the |)!id", &seed);
/* name = "Engia'pin the Doltolph" */
```

The generator supports UTF-8, almost by accident. (I'd have to go out of
my way *not* to support it.)

Despite the lack of a compilation step, which requires the parsing the
template for each generated name, it's *an order of magnitude faster
than the C++ version*, which caught me by surprise. The high
performance is due to name generation being a single pass over the
template using [reservoir sampling][rs].

Internally it maintains a stack of "reset" pointers, each pointing
into the output buffer where the current nesting level began its
output. Each time it hits an alternation (`|`), it generates a random
number and decides whether or not to use the new option. The first
time it's a 1/2 chance it chooses the new option. The second time, a
1/3 chance. The third time a 1/4 chance, and so on. When the new
option is selected, the reset pointer is used to "undo" any previous
output for the current nesting level.

The reservoir sampling means it needs to generate more random numbers
(once per option) than the JavaScript and C++ version (once per nesting
level). However, it uses [its own, fast internal PRNG][prng] rather than
`rand()`. Generating these random numbers is basically free.

Not using `rand()` means that, like the previous libraries, it doesn't
need anything from the standard library. It also has better quality
results since the typical standard library `rand()` is total rubbish,
both in terms of speed and quality (and typically has a [PLT
penalty][plt]). Finally it means the results are identical across all
platforms for the same template and seed, which is one reason it's part
of the API.

Another slight performance boost comes from the representation of
pattern substitutions, i.e. `i` will select a random "idiot" name from a
fixed selection of strings. The obvious representation is an array of
string pointers, as seen in the C++ version. However, there are a lot of
these little strings, which makes for a lot of pointers [cluttering up
the relocation table][reloc]. Instead, I packed it all into few small
pointerless tables, which on x86-64 are accessed efficiently via
RIP-relative addressing. It's efficient, though not friendly to
modification.

I'm very happy with how this library turned out.

### UTF-7 encoder and decoder

The last example is a [UTF-7][w7] encoder and decoder. UTF-7 is a method
for encoding arbitrary Unicode text within ASCII text, created as a
nasty hack to allow Unicode messages to be sent over ASCII-limited email
infrastructure. The gist of it is that the Unicode parts of a message
are encoded as UTF-16, then base64 encoded, then interpolated into the
ASCII stream between delimiters.

Einstein (allegedly) said "If you can't explain it to a six year old,
you don't understand it yourself." The analog for programming is to
replace the six year old with a computer, and explaining an idea to a
computer is done by writing a program. I wanted to understand UTF-7, so
I implemented it.

**[A UTF-7 stream encoder and decoder in ANSI C][utf7]**

Here's the entire API. It's modeled a little after the [zlib][zlib] API.

```c
/* utf7_encode() special code points */
#define UTF7_FLUSH       -1L

/* return codes */
#define UTF7_OK          -1
#define UTF7_FULL        -2
#define UTF7_INCOMPLETE  -3
#define UTF7_INVALID     -4

struct utf7 {
    char *buf;
    size_t len;
    /* then some "private" internal fields */
};

void utf7_init(struct utf7 *, const char *indirect);
int  utf7_encode(struct utf7 *, long codepoint);
long utf7_decode(struct utf7 *);
```

Finally a library that defines a structure! The other fields (not shown)
hold important state information, but the application is only concerned
with `buf` and `len`: an input or output buffer. The same structure is
used for encoding and decoding, though only for one task at a time.

Following the minimalist library principle, there is no memory
allocation. When encoding a UTF-7 stream, the application's job is to
point `buf` to an output buffer, indicating its length with `len`. Then
it feeds code points one at a time into the encoder. When the output is
full, it returns `UTF7_FULL`. The application must provide a new buffer
and try again.

This example usage is more complicated than I anticipated it would be.
Properly pumping code points through the encoder requires a loop (or
at least a second attempt).

```c
char buffer[1024];
struct utf7 ctx;

utf7_init(&ctx, 0);
ctx.buf = buffer;
ctx.len = sizeof(buffer));

/* Assumes "wide character" input is Unicode */
for (;;) {
    wint_t c = fgetwc(stdin);
    if (c == WEOF)
        break;

    while (utf7_encode(ctx, c) != UTF7_OK) {
        /* Flush output and reset buffer */
        fwrite(buffer, sizeof(buffer), 1, stdout);
        ctx.buf = buffer;
        ctx.len = sizeof(buffer));
    }
}

/* Flush all pending output */
while (utf7_encode(ctx, UTF7_FLUSH) != UTF7_OK) {
    fwrite(buffer, sizeof(buffer), 1, stdout);
    ctx.buf = buffer;
    ctx.len = sizeof(buffer));
}

/* Write remaining output */
fwrite(buffer, sizeof(buffer) - ctx.len, 1, stdout);

/* Check for errors */
if (fflush(stdout))
    die("output error");
if (ferror(stdin))
    die("input error");
```

Flushing (`UTF7_FLUSH`) is necessary since, due to base64 encoding,
adjacent Unicode characters usually share a base64 character. Just
because a code point was absorbed into the encoder doesn't mean it was
written into the output buffer. The encoding for that character may
depend on the next character to come. The special "flush" input forces
this out. It's valid to flush in the middle of a stream, though this
[may penalize encoding efficiency][flush] (e.g. the output may be
larger than necessary).

It's not possible for the encoder to fail, so there are no error
conditions to worry about from the library.

Decoding is a different matter. It works almost in reverse from the
encoder: `buf` points to the *input* and the decoder is pumped to
return one code point at a time. It returns one of:

1. A non-negative value: a valid code point (including ASCII).

2. `UTF7_OK`: Input was exhausted. Stopping here would be valid. This is
   what you should get when there's no more input.

2. `UTF7_INVALID`: The input was invalid. `buf` points at the invalid byte.

3. `UTF7_INCOMPLETE`: Input was exhausted, but more is expected. If
   there is no more input, then the input must have been truncated,
   which is an error.

So there are two possible errors for two kinds of invalid input.
Parsing errors are unavoidable when parsing input.

Again, this library accidentally doesn't require the standard library.
It doesn't even [depend on the compiler's locale][locale] being
compatible with ASCII since none of its internal tables use string or
character literals. It behaves *exactly the same* across all conforming
platforms.

### More examples

I had a few more examples in mind, but this article has gone on long
enough.

* [ANSI C implementation of ElsieFour (LC4)][lc4]
* [A minimal POSIX getopt() ANSI C header library][getopt]
* [Growable Memory Buffers for C99][buf]

Instead I'll save these for other articles!


[bite]: /blog/2018/05/01/
[bmp]: https://github.com/skeeto/bmp
[buf]: https://github.com/skeeto/growable-buf
[cpp]: https://github.com/skeeto/fantasyname/pull/2
[dp]: https://old.reddit.com/r/dailyprogrammer/
[endian]: https://commandcenter.blogspot.com/2012/04/byte-order-fallacy.html
[flt]: https://www.exploringbinary.com/incorrectly-rounded-conversions-in-visual-c-plus-plus/
[flush]: /blog/2016/09/09/
[fn]: https://github.com/skeeto/fantasyname/blob/master/c/namegen.h
[free]: /blog/2016/01/31/
[getopt]: https://github.com/skeeto/getopt
[int]: /blog/2017/07/19/
[ivan]: https://github.com/Attnam/ivan
[js]: /blog/2013/03/27/
[lc4]: https://github.com/skeeto/scratch/tree/master/elsiefour
[locale]: https://www.sigbus.info/how-i-wrote-a-self-hosting-c-compiler-in-40-days.html#day42
[merge]: https://github.com/Attnam/ivan/pull/363
[perl]: /blog/2009/01/04
[plt]: /blog/2018/05/27/
[ppm]: /blog/2017/11/03/
[prng]: /blog/2017/09/21/
[reloc]: /blog/2016/12/23/
[rs]: https://en.wikipedia.org/wiki/Reservoir_sampling
[rw]: http://www.rinkworks.com/namegen/
[set32]: https://github.com/skeeto/scratch/tree/master/set32
[stupid]: https://www.npmjs.com/package/is-odd
[thr]: /blog/2018/05/31/
[utf7]: https://github.com/skeeto/utf-7
[w7]: https://en.wikipedia.org/wiki/UTF-7
[zlib]: https://zlib.net/
