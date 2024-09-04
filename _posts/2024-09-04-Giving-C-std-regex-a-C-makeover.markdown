---
title: Giving C++ std::regex a C makeover
layout: post
date: 2024-09-04T17:15:07Z
tags: [c, cpp, win32]
uuid: 83fb81ed-290e-4bc7-87bd-d0bbc6c01d25
---

Suppose you're working in C using one of the major toolchains — that is,
it's mainly a C++ implementation — and you need regular expressions. You
could integrate a library, but there's a regex implementation in the C++
standard library included with your compiler, just within reach. As a
resourceful engineer, using an asset already in hand seems prudent. But
it's a C++ interface, and you're using C instead of C++ for a reason,
perhaps *to avoid dealing with C++*. Have no worries. This article is
about wrapping [`std::regex`][re] in a tidy C interface which not only
hides all the C++ machinery, but *utterly tames it*. It's not so much
practical as a potpourri of interesting techniques.

If you'd like to skip ahead, here's the full source up front. Tested with
[w64devkit][], MSVC `cl`, and `clang-cl`: **[scratch/regex-wrap][src]**

### Interface design

The C interface I came up with, `regex.h`:

```c
#pragma once
#include <stddef.h>

#define S(s) (str){s, sizeof(s)-1}

typedef struct {
    char     *data;
    ptrdiff_t len;
} str;

typedef struct {
    char *beg;
    char *end;
} arena;

typedef struct regex regex;

typedef struct {
    str      *data;
    ptrdiff_t len;
} strlist;

regex  *regex_new(str, arena *);
strlist regex_match(regex *, str, arena *);
```

Longtime readers will find it familiar: [my favorite][str] non-owning,
counted strings form in place of null-terminated strings — similar to C++
`std::string_view` — and [arena allocation][arena]. Yes, such fundamental
types wouldn't "belong" to a regex library like this, but imagine they're
standardized by the project or whatever. Also, this is purely a C header,
not a C/C++ polyglot, and will not be used by the C++ portion.

In particular note the lack of "free" functions. **The regex engine
allocates everything in the arena**, including all temporary working
memory used while compiling, matching, etc. So in a sense, it could be
called [a *non-allocating library*][mini]. This requires a bit of C++
abuse: I will not call some C++ regex destructors. It shouldn't matter
because they only redundantly manage memory in the arena.  (If regex
objects are holding file handles or something else unnecessary then its
implementation so poor as to not be worth using, and we should just use a
better regex library.)

Now's a good time to mention a caveat: In order to pull this off the regex
library lives in its own Dynamic-Link Library with its own copy of the C++
standard library, i.e. statically linked. My demo is Windows-only, but
this concept theoretically extends to shared objects on Linux. Since it's
a C interface that doesn't expose standard library objects, the DLL can be
used by programs compiled with different toolchains. Though that wouldn't
apply to my inciting hypothetical.

Example usage:

```c
regex  *re = regex_new(S("(\\w+)"), perm);
str     s  = S("Hello, world! This is a test.");
strlist m  = regex_match(re, s, perm);
for (ptrdiff_t i = 0; i < m.len; i++) {
    printf("%2td = %.*s\n", i, (int)m.data[i].len, m.data[i].data);
}
```

This program prints:

     0 = Hello
     1 = world
     2 = This
     3 = is
     4 = a
     5 = test

If matching lots of source strings, scope the arena to the loop and then
the results, and any regex working memory, are automatically freed in O(1)
at the end of each iteration:

```c
for (ptrdiff_t i = 0; i < ninputs; i++) {
    arena   scratch = *perm;
    strlist matches = regex_match(re, inputs[i], &scratch);
    // ... consume matches ...
}
```

### C++ implementation

On the C++ side the first thing I do is replace `new` and `delete`, which
is how I force it to allocate from the arena. This replaces `new`/`delete`
for *globally*, but recall that the regex library has its own, private C++
implementation. Replacements apply only to itself even if there's other
C++ present in the process. If this is the only C++ in the process then it
doesn't require such careful isolation.

I can't tell `std::regex` about the arena — it calls `operator new` the
usual way, without extra arguments — so I have to smuggle it in through a
thread-local variable:

```c++
static thread_local arena *perm;
```

If I'm sure the library is only used by a single thread then I can omit
`thread_local`, but it's useful here to demonstrate and measure. Using it
in my operator replacements:

```c++
void *operator new(size_t size, std::align_val_t align)
{
    arena    *a     = perm;
    ptrdiff_t ssize = size;
    ptrdiff_t pad   = (uintptr_t)a->end & ((int)align - 1);
    if (ssize < 0 || ssize > a->end - a->beg - pad) {
        throw std::bad_alloc{};
    }
    return a->end -= size + pad;
}

void *operator new(size_t size)
{
    return operator new(
        size,
        std::align_val_t(__STDCPP_DEFAULT_NEW_ALIGNMENT__)
    );
}
```

Starting in C++17, replacing the global allocator requires definitions for
both plain `new`/`delete` and aligned `new`/`delete`. The [many other
variants][many], including arrays, call these four and so may be skipped.
Allocating over-aligned objects isn't a special case for arenas, so I
implemented plain `new` by calling aligned `new`. I'd prefer to [allocate
through a template][new] so that I can "see" the type, but that's not an
option in this case.

After converting to signed sizes [because they're simpler][sign], it's the
usual from-the-end allocation. I prefer `-fno-exceptions` but `std::regex`
is inherently *exceptional* — and I mean that in at least two bad ways —
so they're required. The good news is this library gracefully and reliably
handles out-of-memory errors. (The arena makes this trivial to test, so
try it for yourself!)

I added a little extra flair replacing `delete`:

```c++
void operator delete(void *) noexcept {}
void operator delete(void *, std::align_val_t) noexcept {}

void operator delete(void *p, size_t size) noexcept
{
    arena *a = perm;
    if (a->end == (char *)p) {
        a->end += size;
    }
}
```

The two mandatory replacements are no-ops because that's simply how arenas
work. We don't free individual objects, but many at once. It's *completely
optional*, but I also replaced sized `delete` for little other reason than
[sized deallocation is cool][free]. C++ destructs in reverse order, so
this is likely to work out. At least with GCC libstdc++, it freed about a
third of the workspace memory before returning to C. I'd rather it didn't
try to free anything at all, but since it's going to call `delete` anyway
I can get some use out of it.

Interesting side note: In a rough benchmark these replacements made MSVC
`std::regex` matching four times faster! I expected a *small* speedup, but
not that. In the typical case it appears to be wasting most of its time on
allocation. On the other hand, libstdc++ `std::regex` is overall quite a
bit slower than MSVC, and my replacements had no performance effect. It's
spending its time elsewhere, and the small gains are lost interacting with
the thread-local.

Finally the meat:

```c++
extern "C" std::regex *regex_new(str re, arena *a)
{
    perm = a;
    try {
        return new std::regex(re.data, re.data+re.len);
    } catch (...) {
        return {};
    }
}
```

It sets the thread-local to the arena, then constructs with "iterators" at
each end of the input. All exceptions are caught and turned into a null
return. Depending on need, we may want to indicate *why* it failed — out
of memory, invalid regex, etc. — by returning an error value of some sort.
An exercise for the reader.

The matcher is a little more complicated:

```c++
extern "C" strlist regex_match(std::regex *re, str s, arena *a)
{
    perm = a;
    try {
        std::cregex_iterator it(s.data, s.data+s.len, *re);
        std::cregex_iterator end;

        strlist r = {};
        r.len  = std::distance(it, end);
        r.data = new str[r.len]();
        for (ptrdiff_t i = 0; it != end; it++, i++) {
            r.data[i].data = s.data + it->position();
            r.data[i].len  = it->length();
        }
        return r;

    } catch (...) {
        return {};
    }
}
```

I create a `char *` "cregex" iterator, again giving it each end of the
input. I hope it's not just making a copy (MSVC `std::regex` does *grumble
grumble*). The result is allocated out of the arena. As before, exceptions
convert to a null return. Callers can distinguish errors because no-match
results have a non-null pointer. The iterator, being a local variable, is
destroyed before returning, uselessly calling `delete`. I could avoid this
by allocating it with `new`, but in practice it doesn't matter.

You might have noticed the lack of `declspec(dllexport)`. [DEF files are
great][def], and I've come to appreciate and prefer them. GCC and MSVC
accept them as another input on the command line, and the source need not
be aware exports. My `regex.def`:

    LIBRARY regex
    EXPORTS
    regex_new
    regex_match

In w64devkit, the command to build the DLL:

    $ g++ -shared -std=c++17 -o regex.dll regex.cpp regex.def

The MSVC command almost maps 1:1 to the GCC command:

	$ cl /LD /std:c++17 /EHsc regex.cpp regex.def

In either case only the C interface is exported (via [peports][]):

    $ peports -e regex.dll
    EXPORTS
            1       regex_match
            2       regex_new

### Reasons against

Though this library is conveniently on hand, and my minimalist C wrapper
interface is nicer than a typical C regex library interface, and even
hides some `std::regex` problems, trade-offs must be considered:

* No Unicode support, particularly UTF-8
* `std::regex` implementations are universally poor and slow
* libstdc++ `std::regex` is especially slow to compile
* Isolating in a DLL (if needed) is inconvenient
* DLL is 200K (MSVC) to 700K (GCC) or so

Depending on what I'm doing, some of these may have me looking elsewhere.


[arena]: /blog/2023/09/27/
[def]: /blog/2023/08/27/
[free]: /blog/2023/12/17/
[many]: https://en.cppreference.com/w/cpp/memory/new/operator_new
[mini]: /blog/2018/06/10/
[new]: /blog/2024/04/14/
[peports]: /blog/2024/06/30/
[re]: https://en.cppreference.com/w/cpp/regex
[sign]: /blog/2024/05/24/
[src]: https://github.com/skeeto/scratch/tree/master/regex-wrap
[str]: /blog/2023/10/08/
[w64devkit]: https://github.com/skeeto/w64devkit
