---
title: My personal C coding style as of late 2023
layout: post
date: 2023-10-08T23:30:57Z
tags: [c]
uuid: 60db7343-43f1-469f-9e9a-8af4d4c46b5a
excerpt_separator: <!--more-->
---

*This article was discussed [on Hacker News][hn] and [on reddit][reddit].*

This has been a ground-breaking year for my C skills, and paradigm shifts
in my technique has provoked me to reconsider my habits and coding style.
It's been my largest personal style change in years, so I've decided to
take a snapshot of its current state and my reasoning. These changes have
produced significant productive and organizational benefits, so while most
is certainly subjective, it likely includes a few objective improvements.
I'm not saying everyone should write C this way, and when I contribute
code to a project I follow their local style. This is about what works
well for me.

<!--more-->

### Primitive types

Starting with the fundamentals, I've been using short names for primitive
types. The resulting clarity was more than I had expected, and it's made
my code more enjoyable to review. These names appear frequently throughout
a program, so conciseness pays. Also, now that I've gone without, `_t`
suffixes are more visually distracting than I had realized.

```c
typedef uint8_t   u8;
typedef char16_t  c16;
typedef int32_t   b32;
typedef int32_t   i32;
typedef uint32_t  u32;
typedef uint64_t  u64;
typedef float     f32;
typedef double    f64;
typedef uintptr_t uptr;
typedef char      byte;
typedef ptrdiff_t size;
typedef size_t    usize;
```

Some people prefer an `s` prefix for signed types. I prefer `i`, plus as
you'll see, I have other designs for `s`. For sizes, `isize` would be more
consistent, and wouldn't hog the identifier, but [signed sizes are the
way][sign] and so I want them in a place of privilege. `usize` is niche,
mainly for interacting with external interfaces where it might matter.

`b32` is a "32-bit boolean" and communicates intent. I could use `_Bool`,
but I'd rather stick to a natural word size and stay away from its weird
semantics. To beginners it might seem like "wasting memory" by using a
32-bit boolean, but in practice that's never the case. It's either in a
register (return value, local variable) or would be padded anyway (struct
field). When it actually matters, I pack booleans into a `flags` variable,
and a 1-byte boolean rarely important.

While UTF-16 might seem niche, it's a necessary evil when dealing with
Win32, so `c16` ("16-bit character") has made a frequent appearance. I
could have based it on `uint16_t`, but putting the name `char16_t` in its
"type hierarchy" communicates to debuggers, particularly GDB, that for
display purposes these variables hold character data. Officially Win32
uses a type named `wchar_t`, but I like being explicit about UTF-16.

`u8` is for octets, usually UTF-8 data. It's distinct from `byte`, which
represents raw memory and is a special *aliasing* type. In theory these
can be distinct types with differing semantics, though I'm not aware of
any implementation that does so (yet?). For now it's about intent.

What about systems that don't support fixed width types? That's academic,
and far too much time has been wasted worrying about it. That includes
time wasted on typing out `int_fast32_t` and similar nonsense. Virtually
no existing software would actually work correctly on such systems — I'm
certain nobody's *testing* it after all — so it seems nobody else cares
either.

I don't intend to use these names in isolation, such as in code snippets
(outside of this article). If I did, examples would require the `typedefs`
to give readers the complete context. That's not worth extra explanation.
Even in the most recent articles I've used `ptrdiff_t` instead of `size`.

### Macros

Next, some "standard" macros:

```c
#define countof(a)    (size)(sizeof(a) / sizeof(*(a)))
#define lengthof(s)   (countof(s) - 1)
#define new(a, t, n)  (t *)alloc(a, sizeof(t), _Alignof(t), n)
```

While I still prefer `ALL_CAPS` for constants, I've adopted lowercase for
function-like macros because it's nicer to read. They don't have the same
namespace problems as other macro definitions: I can have a macro named
`new()` and also variables and fields named `new` because they don't look
like function calls.

For GCC and Clang, my favorite `assert` macro now looks like this:

```c
#define assert(c)  while (!(c)) __builtin_unreachable()
```

It has useful properties beyond [the usual benefits][assert]:

* It does not require separate definitions for debug and release builds.
  Instead it's controlled by the presence of Undefined Behavior Sanitizer
  (UBSan), which is already present/absent in these circumstances. That
  includes [fuzz testing][fuzz].

* `libubsan` provides a diagnostic printout with a file and line number.

* In release builds it turns into a practical optimization hint.

To enable assertions in release builds, put UBSan in trap mode with
`-fsanitize-trap` and then enable at least `-fsanitize=unreachable`. In
theory this can also be done with `-funreachable-traps`, but as of this
writing it's been broken for the past few GCC releases.

### Parameters and functions

No `const`. It serves no practical role in optimization, and **I cannot
recall an instance where it caught, or would have caught, a mistake**. I
held out for awhile as prototype documentation, but on reflection I found
that good parameter names were sufficient. Dropping `const` has made me
noticeably more productive by reducing cognitive load and eliminating
visual clutter. I now believe its inclusion in C was a costly mistake.

(One small exception: I still like it as a hint to place static tables in
read-only memory closer to the code. I'll cast away the `const` if needed.
This is only of minor importance.)

Literal `0` for null pointers. Short and sweet. This is not new, but a
style I've used for about 7 years now, and has appeared all over my
writing since. There are some theoretical edge cases where it may cause
defects, and lots of [ink has been spilled][null] on the subject, but
after a couple 100K lines of code I've yet to see it happen.

`restrict` when necessary, but better to organize code so that it's not,
e.g. don't write to "out" parameters in loops, or don't use out parameters
at all (more on that momentarily). I don't bother with `inline` because I
compile everything as one translation unit anyway.

`typedef` all structures. I used to shy away from it, but eliminating the
`struct` keyword makes code easier to read. If it's a recursive structure,
use a forward declaration immediately above so that such fields can use
the short name:

```c
typedef struct map map;
struct map {
    map *child[4];
    // ...
};
```

Declare all functions `static` except for entry points. Again, with
everything compiled as a single translation unit there's no reason to do
otherwise. It was probably a mistake for C not to default to `static`,
though I don't have a strong opinion on the matter. With the clutter
eliminated through short types, no `const`, no `struct`, etc. **functions
fit comfortably on the same line as their return type**. I used to break
them apart so that the function name began on its own line, but that's no
longer necessary.

In my writing I sometimes omit `static` to simplify, and because outside
the context of a complete program it's mostly irrelevant. However, I will
use it below to emphasize this style.

For awhile I capitalized type names as that effectively put them in a kind
of namespace apart from variables and functions, but I eventually stopped.
I may try this idea in different way in the future.

### Strings

One of my most productive changes this year has been the total rejection
of null terminated strings — another of those terrible mistakes — and the
embrace of [this basic string type][str]:

```c
#define s8(s) (s8){(u8 *)s, lengthof(s)}
typedef struct {
    u8  *data;
    size len;
} s8;
```

I've used a few names for it, but this is my favorite. The `s` is for
string, and the `8` is for UTF-8 or `u8`. The `s8` macro (sometimes just
spelled `S`) wraps a C string literal, making a `s8` string out of it. A
`s8` is handled like a [fat pointer][fat], passed and returned by copy.
`s8` makes for a great function prefix, unlike `str`, all of which are
reserved. Some examples:

```c
static s8   s8span(u8 *, u8 *);
static b32  s8equals(s8, s8);
static size s8compare(s8, s8);
static u64  s8hash(s8);
static s8   s8trim(s8);
static s8   s8clone(s8, arena *);
```

Then when combined with the macro:

```c
    if (s8equals(tagname, s8("body"))) {
        // ...
    }
```

You might be tempted to use a flexible array member to pack the size and
array together as one allocation. Tried it. Its inflexibility is totally
not worth whatever benefits it might have. Consider, for instance, how
you'd create such a string out of a literal, and how it would be used.

A few times I've thought, "This program is simple enough that I don't need
a string type for this data." That thought is nearly always wrong. Having
it available helps me think more clearly, and makes for simpler programs.
(C++ got it only a few years ago with `std::string_view` and `std::span`.)

It has a natural UTF-16 counterpart, `s16`:

```c
#define s16(s) (s16){u##s, lengthof(u##s)}
typedef struct {
    c16 *data;
    size len;
} s16;
```

I'm not entirely sold on gluing `u` to the literal in the macro, versus
writing it out on the string literal.

### More structures

Another change has been preferring structure returns instead of out
parameters. It's effectively a multiple value return, though without
destructuring. A great organizational change. For example, this function
returns two values, a parse result and a status:

```c
typedef struct {
    i32 value;
    b32 ok;
} i32parsed;

static i32parsed i32parse(s8);
```

Worried about the "extra copying?" Have no fear, because in practice
calling conventions turn this into a hidden, `restrict`-qualified out
parameter — if it's not inlined such that any return value overhead would
be irrelevant anyway. With this return style I'm less tempted to use
in-band signals like special null returns to indicate errors, which is
less clear.

It's also led to a style of defining a zero-initialized return value at
the top of the function, i.e. `ok` is false, and then use it for all
`return` statements. On error, it can bail out with an immediate return.
The success path sets `ok` to true before the return.

```c
static i32parsed i32parse(s8 s)
{
    i32parsed r = {0};
    for (size i = 0; i < s.len; i++) {
        u8 digit = s.data[i] - '0';
        // ...
        if (overflow) {
            return r;
        }
        r.value = r.value*10 + digit;
    }
    r.ok = 1;
    return r;
}
```

Aside from static data, I've also moved away from initializers except the
conventional zero initializer. (Notable exception: `s8` and `s16` macros.)
This includes designated initializers. Instead I've been initializing with
assignments. For example, this [buffered output][buf] "constructor":

```c
typedef struct {
    u8 *buf;
    i32 len;
    i32 cap;
    i32 fd;
    b32 err;
} u8buf;

static u8buf newu8buf(arena *perm, i32 cap, i32 fd)
{
    u8buf r = {0};
    r.buf = new(perm, u8, cap);
    r.cap = cap;
    r.fd  = fd;
    return r;
}
```

I like how this reads, but it also eliminates a cognitive burden: The
assignments are separated by sequence points, giving them an explicit
order. It doesn't matter here, but in other cases it does:

```c
    example e = {
        .name = randname(&rng),
        .age  = randage(&rng),
        .seat = randseat(&rng),
    };
```

There are 6 possible values for `e` from the same seed. I like no longer
thinking about these possibilities.

### Odds and ends

Prefer `__attribute` to `__attribute__`. The `__` suffix is excessive and
unnecessary.

```c
__attribute((malloc, warn_unused_result))
```

For Win32 systems programming, which typically only requires a modest
number of declarations and definitions, rather than include `windows.h`,
[write the prototypes out by hand][w32] using custom types. It reduces
build times, declutters namespaces, and interfaces more cleanly with the
program (no more `DWORD`/`BOOL`/`ULONG_PTR`, but `u32`/`b32`/`uptr`).

```c
#define W32(r) __declspec(dllimport) r __stdcall
W32(void)   ExitProcess(u32);
W32(i32)    GetStdHandle(u32);
W32(byte *) VirtualAlloc(byte *, usize, u32, u32);
W32(b32)    WriteConsoleA(uptr, u8 *, u32, u32 *, void *);
W32(b32)    WriteConsoleW(uptr, c16 *, u32, u32 *, void *);
```

For inline assembly, treat the outer parentheses like braces, put a space
before the opening parenthesis, just like `if`, and start each constraint
line with its colon.

```c
static u64 rdtscp(void)
{
    u32 hi, lo;
    asm volatile (
        "rdtscp"
        : "=d"(hi), "=a"(lo)
        :
        : "cx", "memory"
    );
    return (u64)hi<<32 | lo;
}
```

There's surely a lot more to my style than this, but unlike the above,
those details haven't changed this year. To see most of the mentioned
items in action in a small program, see [`wordhist.c`][wh], one of my
testing grounds for [hash-tries][ht], or for a slightly larger program,
[`asmint.c`][asm], a mini programming language implementation.


[asm]: https://github.com/skeeto/scratch/blob/master/misc/asmint.c
[assert]: /blog/2022/06/26/
[buf]: /blog/2023/02/13/
[fat]: /blog/2019/06/30/
[fuzz]: /blog/2019/01/25/
[hn]: https://news.ycombinator.com/item?id=37815674
[ht]: /blog/2023/09/30/
[null]: https://ljabl.com/nullptr.xhtml
[reddit]: https://old.reddit.com/r/C_Programming/comments/173e0vn/
[sign]: /blog/2023/09/27/
[str]: /blog/2023/01/18/#implementation-highlights
[w32]: /blog/2023/05/31/
[wh]: https://github.com/skeeto/scratch/blob/master/misc/wordhist.c
