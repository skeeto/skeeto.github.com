---
title: My review of the C standard library in practice
layout: post
date: 2023-02-11T03:04:11Z
tags: [c]
uuid: 31a77d1d-219c-4677-995a-8e869f9ab610
excerpt_separator: <!--more-->
---

*This article was discussed [on Hacker News][hn] and critiqued [on
Wandering Thoughts][wt].*

In general, when working in C I avoid the standard library, libc, as much
as possible. If possible I won't even link it. For people not used to
working and thinking this way, the typical response is confusion. Isn't
that like re-inventing the wheel? For me, *libc is a wheel barely worth
using* — too many deficiencies in both interface and implementation.
Fortunately, it's easy to build a better, simpler wheel when you know the
terrain ahead of time. In this article I'll review the functions and
function-like macros of the C standard library and discuss practical
issues I've faced with them.

<!--more-->

Fortunately the flexibility of C-in-practice makes up for the standard
library. I already have all the tools at hand to do what I need — not
beholden to any runtime.

How does one write portable software while relying little on libc?
Implement the bulk of the program as platform-agnostic, libc-free code
then write platform-specific code per target — a platform layer — each in
its own source file. The platform code is small in comparison: mostly
unportable code, perhaps [raw system calls][raw], graphics functions, or
even assembly. It's where you get access to all the coolest toys. On some
platforms it will still link libc anyway because it's got useful
platform-specific features, or because it's mandatory.

The discussion below is specifically about standard C. Some platforms
provide special workarounds for their standard function shortcomings, but
that's irrelevant. If I need to use a non-standard function then I'm
already writing platform-specific code and I might as well take full
advantage of that fact, bypassing the original issue entirely by calling
directly into the platform.

The rest of this article goes through the standard library listing in [the
C18 draft][drafts] mostly in order.

### assert and abort

I [wrote about the `assert` macro last year][assert]. While C assertions
are better than the same in any other language I know — a trap *without
first unwinding the stack* — the typical implementation doesn't have the
courtesy to trap in the macro itself, creating friction. Or worse, it
doesn't trap at all and instead exits the process normally with a non-zero
status. It's not optimized for debuggers.

My non-trivial programs quickly pick up this definition instead, adjusted
later as needed:

```c
#define ASSERT(c) if (!(c)) __builtin_trap()
```

There's no diagnostic, but I usually don't want that anyway. The vast
majority of the time these are caught in a debugger, and I don't need or
want a diagnostic.

I have no objections to `static_assert`, but it's also not part of the
runtime.

### Math functions

By this I mean all the stuff in `math.h`, `complex.h`, etc. It's good that
these are, in practice, pseudo-intrinsics. They're also one of the more
challenging parts of libc to replace. It prioritizes precision more than I
usually need, but that's a reasonable default.

### Character classification and mapping

Includes `isalnum`, `isalpha`, `isascii`, `isblank`, `iscntrl`, `isdigit`,
`isgraph`, `islower`, `isprint`, `ispunct`, `isspace`, `isupper`,
`isxdigit`, `tolower`, and `toupper`. The interface is misleading, almost
maliciously so, and these functions are misused in every case I've seen in
the wild. If you see `#include <ctype.h>` in a source file then it's
probably defective. I've been guilty of it myself. When it's up to me,
these functions are banned without exception.

Their prototypes are all shaped like so:

```c
int isXXXXX(int);
```

However, the domain of the input is `unsigned char` plus `EOF`. Negative
arguments, aside from `EOF`, are *undefined behavior*, despite the obvious
use case being strings. So this is incorrect:

```c
char *s = ...;
if (isdigit(s[0])) {   // WRONG!
    ...
}
```

If `char` is signed, as it is on x86, then it's undefined for arbitrary
strings, `s`. Some implementations even crash on such inputs.

If the argument was `unsigned char`, then it would at least truncate into
range, *usually* leading to the desired result. (Though not so if [passing
Unicode code points][tale], which is an odd mistake to make.) Except that
it has to accommodate `EOF`. Why that? **These functions are defined for
use with `fgetc`, not strings!**

You could patch over it with truncation by masking:

```c
if (isdigit(s[0] & 255)) {
    ...
}
```

However, you're still left with *locales*. This is a bit of *global state*
that changes [how a number of libc functions behave][locale], including
character classification. While locales have some niche uses, most of the
time [the behavior is surprising and undesirable][bash]. It's also bad for
performance. I've developed a habit of using `LC_ALL=C` before some GNU
programs so that they behave themselves. If you're parsing a fixed format
that doesn't adapt to locale — virtually everything — you definitely do
not want locale-based character classification of input.

Since the interface and behavior both unsuited for most uses, you're
better off making your own range checks or lookup tables for your use
case. When you name it, probably avoid starting the function with `is`
since [it's reserved][res].

```c
_Bool xisdigit(char c)
{
    return c>='0' && c<='9';
}
```

I used `char`, but this still works fine for naive UTF-8 parsing.

### errno

Without libc you don't have to use this global, hopefully thread-local,
pseudo-variable. Good riddance. Return your errors, and use a struct if
necessary.

### locales

As discussed, locales have some niche uses — formatting dates comes to
mind — but what little use they have is trapped behind global state set by
`setlocale`, making it sometimes impossible to use correctly.

On Windows I've instead used [GetLocaleInfoW][] to get information like,
"What is the local name of the [current month][cal]?"

### setjmp and longjmp

Sometimes tricky to use correctly, particularly with regard to qualifying
local variables as `volatile`. It can compose with region-based allocation
to [automatically and instantly free][uc] all objects created between set
and jump. These macros are fine, but don't overdo it.

### variable arguments

Variadic functions are occasionally useful, and the `va_start`/`va_end`
macros make them possible. These are, unfortunately, notoriously complex
because calling conventions do not go out of their way to make them any
simpler. They [require compiler assistance][va], and in practice they're
implemented as part of the compiler rather than libc. They're okay, but I
can live without it.

### signals

While important on unix-like systems, signals as defined in the C standard
library are essentially useless. If you're dealing with signals, or even
something *like* signals, it will be in platform-specific code that goes
beyond the C standard library.

### atomics

I've used the `_Atomic` qualifier [in examples][q] since it helps with
conciseness, but I hardly use it in practice. In part because it has the
inconvenient effect of bleeding into APIs and ABIs. As with `volatile`, C
is using the type system to indirectly achieve a goal. Types are not
atomic, *loads* and *stores* are atomic. Predating standardization, C
implementations have been expressing these loads and stores using
intrinsics, functions, or macros rather than through types.

The `_Atomic` qualifier provides access to the most basic and most strict
atomic operations without libc. That is, it's implemented purely in the
compiler. However, everything outside that involves libc, and potentially
even requires linking a special atomics library.

Even more, one major implementation (MSVC) still doesn't support C11
atomics. Anywhere I care about using C atomics, I can already use the
[richer set of GCC built-ins][gcc], which Clang also supports. If I'm
writing code intended for Windows, I'll use the [interlocked macros][il],
which work [across all the compilers][w] for that platform.

### stdio

Standard input and output, stdio, is perhaps the primary driving factor
for my own routing around libc. Nearly every program does some kind of
input or output, but going through stdio makes things harder.

To read or write a file, one must first open it, e.g. `fopen`. However,
all the implementations for one platform in particular [does not allow
`fopen` to access most of the file system][crt], so using libc immediately
limits the program's capabilities on that platform.

The standard library distinguishes between "text" and "binary" streams. It
makes no difference on unix-like platforms, but it does on others, where
input and output are translated. Besides destroying your data, text
streams have terrible performance. Opening everything in binary mode is a
simple enough work around, but standard input, output, and error are
opened as text streams, and there is no standard function for changing
them to binary streams.

When using `fread`, some implementations use the entire buffer as a
[temporary work space][fread], even if it returns a length less than the
entire buffer. So the following won't work reliably:

```c
char buf[N] = {0};
fread(buf, N-1, 1, f);
puts(buf);
```

It may print junk after the expected output because `fread` overwrote the
zeroes beyond it.

Streams are buffered, and there's no reliable access to unbuffered input
and output, such as when an application is already buffering, perhaps as a
natural consequence of how it works. There's `setvbuf` and `_IONBF`
("unbuffered"), but in [at least one case][fbsd] this really just means
"one byte at a time." It's common for my libc-using programs to end up
with double buffering since I can't reliably turn off stdio buffering.

Typical implementations assume streams will be used by multiple threads,
and so every access goes through a mutex. This causes terrible performance
for small reads and writes — exactly the case buffering is supposed to
most *help*. Not only is this unusual, such programs are probably broken
anyway — oblivious to the still-present race conditions — and so **stdio
is optimized for the unusual, broken case at the cost of the most needed
typical case**.

There is no reliable way to interactively input and display Unicode text.
The C standard makes vague concessions for dealing with "wide characters"
but it's useless in practice. I've tried! The most common need for me is
printing a path to standard error such that it displays properly to the
user.

Seek offsets are limited to `long`. Some real implementations can't even
open files large than 2GiB.

Rather than deal with all this, I add a couple of unbuffered I/O functions
to the platform layer, then put a small buffered stream implementation in
the application which flushes to the platform layer. UTF-8 for text input
and output, and if the platform layer detects it's connected to a terminal
or console, it does the appropriate translation. It doesn't take much to
get something more reliable than stdio. The details are [the topic for a
future article][fo], especially since you might be wondering about
formatted output.

As for formatted input, [don't ever bother with `scanf`][scanf].

### Numeric conversion

Float conversion [is generally a difficult problem][strtod], especially if
you care about [round trips][rt]. It's one of the better and most useful
parts of libc. Though even with libc it's still difficult to get the
simplest or shortest round-trip representation. Also, this is an area
where changing locales can be disastrous!

The question is then: How much does this matter in your application's
context? There's a good chance you only need to display a rounded,
low-precision representation of a float to users — perhaps displaying a
player's position in a debug window, etc. Or you only need to parse
medium-precision non-integral inputs following a relatively simple format.
These are not so difficult.

Parsing (`atoi`, `strtol`, `strtod`, etc.) requires null-terminated
strings, which is generally inconvenient. These integers likely came from
something *not* null-terminated like a file, and so I need to first append
a null terminator. I can't just feed it a token from a memory-mapped file.
Even when using libc, I often write my own integer parser anyway since the
libc parsers lack an appropriate interface.

**Update**: NRK points out that [unsigned integer parsing treats negative
inputs as in range][strtoul]. This is both surprising and rarely useful.
Looking more closely at the specification, I see it is also affected by
locale. Given these revelations, I would ban without exception `atoi`,
`atol`, `strtoul`, and `strtoull`, and avoid `strtol` and `strtoll`.

Formatting integers is easy. Parsing integers within in narrow range (e.g.
up to a million) is easy. Parsing integers to the very limits of the
numeric type [is tricky][strtonum] because every operation must guard
against overflow regardless of signed or unsigned. Fortunately the first
two are common and the last is rarely necessary!

### Random numbers

We have `rand`, `srand`, and `RAND_MAX`. As [a PRNG enthusiast][prng], I
could never recommend using this under any circumstances. It's a PRNG with
mediocre output, poor performance, and global state. `RAND_MAX` being
unknown ahead of time makes it even more difficult to make effective use
of `rand`. You can do better on all dimensions with just [a few lines of
code][pcg].

To make matters worse, typical implementations *expect* it to be accessed
concurrently from multiple threads, so they wrap it in a mutex. Again, it
optimizes for the unusual, broken case — threads fighting each other over
non-deterministic racy results from a deterministic PRNG — at the cost of
the typical, sensible case. Programs relying on that mutex are already
broken.

### Memory allocation

Includes `malloc`, `calloc`, `realloc`, `free`, etc. Okay, but in practice
used too granularly and too much such that many C programs [are tangles of
lifetimes][arena]. Sometimes I wish there was a standard region allocator
so that independently-written libraries could speak a common, sensible,
[caller-controlled][mini] allocation interface.

A major standardization failure here has been not moving size computations
into the allocators themselves. `calloc` is a start: You say how big and
how many, and it works out the total allocation, checking for overflow.
There should be more of this, even if just to [discourage individual
allocations and encourage group allocations][cm].

There are some edge cases around zero sizes, like `malloc(0)`, and the
standard leaves the behavior [a bit too open ended][m0]. However, if your
program is so poorly structured such that it may possibly pass zero to
`malloc` then you have bigger problems anyway.

### Communication with the environment

`getenv` is straightforward, though I'd prefer to just access the
environment block directly, *a la* the non-standard third argument to
`main`.

`exit` is fine, but `atexit` is jank.

`system` is [essentially useless][system] in practice.

### Sorting and searching

`qsort` is ~~[fine][]~~poor because it [lacks a context argument][ctx].
[Quality varies][qsort]. Not difficult to implement from scratch if
necessary. I rarely need to sort.

Similar story  for `bsearch`. Though if I need a binary search over an
array, `bsearch` probably isn't sufficient because I usually want to find
lower and upper bounds of a range.

### Multi-byte encodings and wide characters

`mblen`, `mbtowc`, `mbtowc`, `wctomb`, `mbstowcs`, and `wcstombs` are
connected to the locale system and don't necessarily operate on any
particular encodings like UTF-8, which makes them unreliable. This is the
case for all the other wide character functionality, which is quite a few
functions. Fortunately I only ever need wide characters on one platform in
particular, not in portable code.

More recently are `mbrtoc16`, `c16rtomb`, `mbrtoc32`, and `c32rtomb` where
the "wide" side is specified (UTF-16, UTF-32) but not the multi-byte side.
Limited support in implementations and not particularly useful.

### Strings

Like `ctype.h`, `string.h` is another case where everything is terrible,
and some functions are virtually always misused.

`memcpy`, `memmove`, `memset`, and `memcmp` are fine except for one issue:
it is undefined behavior to pass a null pointer to these functions, *even
with a zero size*. That's ridiculous. A null pointer legitimately and
usefully points to a zero-sized object. As mentioned, even `malloc(0)` is
permitted to behave this way. These functions would be fine if not for
this one defect.

`strcpy`, `strncpy`, `strcat`, and `strncat` [have no legitimate
uses][strcpy] and their use indicates confusion. As such, any code calling
them is suspect and should receive extra scrutiny. In fact, **I have yet
to see a single correct use of `strncpy` in a real program**. (Usage hint:
the length argument should refer to the destination, not the source.) When
it's up to me, these functions are banned without exception. This applies
equally to non-standard versions of these functions like `strlcpy`.

`strlen` has legitimate uses, but is used too often. It should only appear
at system boundaries when receiving strings of unknown size (e.g. `argv`,
`getenv`), and should never be applied to a static string. (Hint: you can
use `sizeof` on those.)

When I see `strchr`, `strcmp` or `strncmp` I wonder why you don't know the
lengths of your strings. On the other hand, `strcspn`, `strpbrk`,
`strrchr`, `strspn`, and `strstr` do not have `mem` equivalents, though
the null termination requirement hurts their usefulness.

`strcoll` and `strxfrm` depend on locale and so are at best niche.
Otherwise unpredictable. Avoid.

`memchr` is fine except for the aforementioned null pointer restriction,
though it comes up less often here.

`strtok` has hidden global state. Besides that, how long is the returned
token? It knew the length before it returned. You mean I have to call
`strlen` to find out? Banned.

`strerror` has an obvious, simple, robust solution: return a pointer to a
static string in a lookup table corresponding to the error number. No
global state, thread-safe, re-entrant, and the returned string is good
until the program exits. Some implementations do this, but unfortunately
it's not true for [at least one real world implementation][strerror],
which instead writes to a shared, global buffer. Hopefully you were
avoiding `errno` anyway.

### Threads

Introduced in C11, but never gained significant traction. Anywhere you can
use C threads you can use pthreads, which are better anyway.

Besides, thread creation probably belongs in the platform layer anyway.

### Time functions

Fairly niche, and I can't remember using any of these except for `time`
and `clock` [for seeding][seed].

### Wrap-up

I hand-waved away a long list of vestigial wide character functions, but
the above is pretty much all there is to the C standard library. The only
things I miss when avoiding it altogether are the math functions, and
[occasionally `setjmp`/`longjmp`][setjmp]. Everything else I can do better
myself, with little difficulty, starting from the platform layer.

All of the C implementations I had in mind above are very old. They will
rarely, if ever, *change*, just accrue. There isn't a lot of innovation
happening in this space, which is fine since I like stable targets. If you
*would* like to see interesting innovation, check out [what Cosmopolitan
Libc is up to][cosmo]. It's what I imagine C could be if it continued
evolving along practical dimensions.


[GetLocaleInfoW]: https://learn.microsoft.com/en-us/windows/win32/api/winnls/nf-winnls-getlocaleinfow
[arena]: https://www.rfleury.com/p/untangling-lifetimes-the-arena-allocator
[assert]: /blog/2022/06/26/
[bash]: https://utcc.utoronto.ca/~cks/space/blog/linux/BashLocaleScriptDestruction?showcomments
[cal]: https://github.com/skeeto/scratch/blob/master/windows/cal.c
[cm]: https://www.youtube.com/watch?v=f4ioc8-lDc0&t=4407s
[cosmo]: https://justine.lol/sizetricks/
[crt]: /blog/2021/12/30/
[ctx]: /blog/2017/01/08/
[drafts]: https://stackoverflow.com/a/83763
[fbsd]: https://github.com/openssl/openssl/issues/3281
[fine]: https://lists.sr.ht/~skeeto/public-inbox/%3C1676216350-sup-9306%40thyestes.tartarus.org%3E
[fo]: /blog/2023/02/13/
[fread]: https://learn.microsoft.com/en-us/cpp/c-runtime-library/reference/fread#remarks
[gcc]: https://gcc.gnu.org/onlinedocs/gcc/_005f_005fatomic-Builtins.html
[hn]: https://news.ycombinator.com/item?id=34752400
[il]: https://docs.microsoft.com/en-us/windows/win32/sync/interlocked-variable-access
[locale]: https://github.com/mpv-player/mpv/commit/1e70e82baa9193f6f027338b0fab0f5078971fbe
[m0]: https://yarchive.net/comp/linux/malloc_0.html
[mini]: /blog/2018/06/10/
[pcg]: https://www.pcg-random.org/download.html
[prng]: /blog/2017/09/21/
[q]: /blog/2022/05/14/
[qsort]: /blog/2016/09/05/
[raw]: /blog/2015/05/15/
[res]: https://devblogs.microsoft.com/oldnewthing/20230109-00/?p=107685
[rt]: https://possiblywrong.wordpress.com/2015/06/21/floating-point-round-trips/
[scanf]: https://sekrit.de/webdocs/c/beginners-guide-away-from-scanf.html
[seed]: /blog/2019/04/30/
[setjmp]: /blog/2023/02/12/
[strcpy]: /blog/2021/07/30/
[strerror]: https://man.freebsd.org/cgi/man.cgi?query=strerror&sektion=0
[strtod]: https://www.exploringbinary.com/a-better-way-to-convert-integers-in-david-gays-strtod/
[strtonum]: https://github.com/skeeto/scratch/blob/master/parsers/strtonum.c
[strtoul]: https://lists.sr.ht/~skeeto/public-inbox/%3C20230218081805.z57kyrbc6xzqlnx6%40gen2.localdomain%3E
[system]: /blog/2022/02/18/
[tale]: https://drewdevault.com/2020/09/25/A-story-of-two-libcs.html
[uc]: /blog/2023/01/18/#implementation-highlights
[va]: https://blog.nelhage.com/2010/10/amd64-and-va_arg/
[w]: /blog/2022/10/05/#four-elements-windows
[wt]: https://utcc.utoronto.ca/~cks/space/blog/programming/CaseForAtomicTypes
