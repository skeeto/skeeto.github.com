---
title: My favorite C compiler flags during development
layout: post
date: 2023-04-29T22:55:25Z
tags: [c, cpp]
uuid: a90f3f5b-b4c3-4153-ac8e-6cdbf235f44b
excerpt_separator: <!--more-->
---

*This article was discussed [on Hacker News][hn] and [on reddit][reddit].*

The major compilers have an [enormous number of knobs][man]. Most are
highly specialized, but others are generally useful even if uncommon. For
warnings, the venerable `-﻿Wall -﻿Wextra` is a good start, but
circumstances improve by tweaking this warning set. This article covers
high-hitting development-time options in GCC, Clang, and MSVC that ought
to get more consideration.

<!--more-->

There's an irony that the more you use these options, the less useful they
become. Given a reasonable workflow, they are a harsh mistress in a fast,
tight feedback loop quickly breaking the habits that cause warnings and
errors. It's a kind of self-improvement, where eventually most findings
will be false positives. With heuristics internalized, you will be able
spot the same issues just reading code — a handy skill during code review.

### Static warnings

Traditionally, C and C++ compilers are by default conservative with
warnings. Unless configured otherwise, they only warn about the most
egregious issues where it's highly confident. That's too conservative. For
`gcc` and `clang`, the first order of business is turning on more warnings
with **`-﻿Wall`**. Despite the name, this doesn't actually enable all
warnings. (`clang` has `-﻿Weverything` which does literally this, but
trust me, you don't want it.) However, that still falls short, and you're
better served enabling *extra* warnings on with **`-﻿Wextra`**.

    $ cc -Wall -Wextra ...

That should be the baseline on any new project, and closer to what these
compilers should do by default. Not using these means leaving value on the
table. If you come across such a project, there's a good chance you can
find bugs statically just by using this baseline. Some warnings only occur
at higher optimization levels, so leave these on for your release builds,
too.

For MSVC, including `clang-cl`, a similar baseline is **`/W4`**. Though it
goes a bit far, warning about use of unary minus on unsigned types
(C4146), and sign conversions (C4245). If you're [using a CRT][crt], also
disable the bogus and irresponsible "security" warnings. Putting it
together, the warning baseline becomes:

    $ cl /W4 /wd4146 /wd4245 /D_CRT_SECURE_NO_WARNINGS ...

As for `gcc` and `clang`, I dislike unused parameter warnings, so I often
turn it off, at least while I'm working: **`-﻿Wno-unused-parameter`**.
Rarely is it a defect to not use a parameter. It's common for a function
to fit a fixed prototype but not need all its parameters (e.g. `WinMain`).
Were it up to me, this would not be part of `-﻿Wextra`.

I also dislike unused functions warnings: **`-﻿Wno-unused-function`**.
I can't say this is wrong for the baseline since, in most cases, ultimately
I do want to know if there are unused functions, e.g. to be deleted. But
while I'm working it's usually noise.

If I'm [working with OpenMP][openmp], I may also disable warnings about
unknown pragmas: **`-﻿Wno-unknown-pragmas`**. One cool feature of
OpenMP is that the typical case gracefully degrades to single-threaded
behavior when not enabled. That is, compiling without `-﻿fopenmp`.
I'll test both ways to ensure I get deterministic results, or just to ease
debugging, and I don't want warnings when it's disabled. It's fine for the
baseline to have this warning, but sometimes it's a poor match.

When working with single-precision floats, perhaps on games or graphics,
it's easy to accidentally introduce promotion to double precision, which
can hurt performance. It could be neglecting an `f` suffix on a constant
or using `sin` instead of `sinf`. Use **`-﻿Wdouble-promotion`** to
catch such mistakes. Honestly, this is important enough that it should go
into the baseline.

```c
#define PI 3.141592653589793
float degs = ...;
float rads = degs * PI / 180;  // warns about promotion
```

It can be awkward around variadic functions, particularly `printf`, which
cannot receive `float` arguments, and so implicitly converts. You'll need
a explicit cast to disable the warning. I imagine this is the main reason
the warning is not part of `-﻿Wextra`.

```c
float x = ...;
printf("%.17g\n", (double)x);
```

Finally, an advanced option: **`-﻿Wconversion -Wno-sign-conversion`**.
It warns about implicit conversions that may result in data loss. Sign
conversions do not have data loss, the implicit conversions are useful,
and in my experience they're not a source of defects, so I disable that
part using the second flag (like MSVC `/wd4245`). The important warning
here is truncation of size values, warning about unsound uses of sizes and
subscripts. For example:

```c
// NOTE: would be declared/defined via windows.h
typedef uint32_t DWORD;
BOOL WriteFile(HANDLE, const void *, DWORD, DWORD *, OVERLAPPED *);

void logmsg(char *msg, size_t len)
{
    HANDLE err = GetStdHandle(STD_ERROR_HANDLE);
    DWORD out;
    WriteFile(err, msg, len, &out, 0);  // len truncation warning
}
```

On 64-bit targets, it will warn about truncating the 64-bit `len` for the
32-bit parameter. To dismiss the warning, you must either address it by
using a loop to [call `WriteFile` multiple times][buf], or acknowledge the
truncation with an explicit cast and accept the consequences. In this case
I may know from context it's impossible for the program to even construct
such a large message, so I'd use an assertion and truncate.

```c
void logmsg(char *msg, size_t len)
{
    HANDLE err = GetStdHandle(STD_ERROR_HANDLE);
    DWORD out;
    assert(len <= 0xffffffff);
    WriteFile(err, msg, (DWORD)len, &out, 0);
}
```

You might consider changing the interface instead:

```c
void logmsg(char *msg, uint32_t len);
```

That probably passes the buck and doesn't solve the underlying problem.
The caller may be holding a `size_t` length, so the truncation happens
there instead. Or maybe you keep propagating this change backwards until
it, say, dissipates on a known constant. `-﻿Wconversion` leads to
these ripple effects that improves the overall program, which is why I
like it.

The catch is that the above warning only happens for 64-bit targets. So
you might miss it. The inverse is true in other cases. This is one area
where [cross-architecture testing][cross] can pay off.

Unfortunately since this warning is off the beaten path, it seems like it
doesn't quite get the attention it could use. It warns about simple cases
where truncation has been explicitly handled/avoided. For example:

```c
int x = ...;
char digit = '0' + x%10;  // false warning
```

The `'0'` is a known constant. The operation `x%10` has a known range (-9
to 9). Therefore the addition result has a known range, and all results
can be represented in a `char`. Yet it still warns. This often comes up
dealing with character data like this.

In my `logmsg` fix I had used an assertion to check that no truncation
actually occurred. But wouldn't it be nice if the compiler could generate
that for us somehow? That brings us to dynamic checks.

### Dynamic run-time checks

Sanitizers have been around for nearly a decade but are still criminally
underused. They insert run-time assertions into programs at the flip of a
switch typically at a modest performance cost — less than the cost of a
debug build. All three major compilers support at least one sanitizer on
all targets. In most cases, failing to use them is practically the same as
not even trying to find defects. Every beginner tutorial ought to be using
sanitizers *from page 1* where they teach how to compile a program with
`gcc`. (That this is universally *not* the case, and that these same
tutorials also do not begin with teaching a debugger, is a major, on-going
education failure.)

There are multiple different sanitizers with lots of overlap, but Address
Sanitizer (ASan) and Undefined Behavior Sanitizer (UBSan) are the most
general. They are compatible with each other and form a solid, general
baseline. To use address sanitizer, at both compile and link time do:

    $ cc ... -fsanitize=address ...

It's even spelled the same way in MSVC. It's needed at link time because
it includes a runtime component. When working properly it's aware of all
allocations and checks all memory accesses that might be out of bounds,
producing a run-time error if that occurs. It's not always appropriate,
but most projects that can use it probably should.

UBSan is enabled similarly:

    $ cc ... -fsanitize=undefined ...

It adds checks around operations that might be undefined, emitting a
run-time error if it occurs. It has an optional runtime component to
produce a helpful diagnostic. You can instead insert a trap instruction,
which is how I prefer to use it: **`-﻿fsanitize-trap=undefined`**.
(Until recently it was **`-﻿fsanitize-undefined-trap-on-error`**.)
This works on platforms where the UBSan runtime is unsupported. Some
instrumentation is only inserted at higher optimization levels.

For me, the most useful UBSan check is signed overflow — e.g. computing
the wrong result — and it's instrumentation I miss when not working in C.
In programs where this might be an issue, combine it [with a fuzzer][fuzz]
to search for inputs that cause overflows. This is yet another argument in
favor of [signed sizes][size], as UBSan can detect such overflows. (Yes,
UBSan optionally instruments unsigned overflow, too, but then you must
somehow distinguish [intentional][lcg] from [unintentional][int]
overflow.)

On Linux, ASan and UBSan strangely do not have [debugger-oriented
defaults][debug]. Fortunately that's easy to address with a couple of
environment variables, which cause them to break on error instead of
uselessly exiting:

```sh
export ASAN_OPTIONS=abort_on_error=1:halt_on_error=1
export UBSAN_OPTIONS=abort_on_error=1:halt_on_error=1
```

Also, when compiling you can combine sanitizers like so:

    $ cc ... -fsanitize=address,undefined ...

As of this writing, MSVC does not have UBSan, but it does have a similar
feature, [run-time error checks][rtc]. Three sub-flags (`c`, `s`, `u`)
enable different checks, and **`/RTCcsu`** turns them all on. The `c` flag
generates the assertion I had manually written with `-﻿Wconversion`,
and traps any truncation at run time. There's nothing quite like this in
UBSan! It's so extreme that it's compatible with neither standard runtime
libraries (fortunately [not a big deal][libc]) nor with ASan.

Caveat: Explicit casts aren't enough, you must actually truncate variables
using a mask in order to pass the check. For example, to accept truncation
in the `logmsg` function:

```c
    WriteFile(err, msg, len&0xffffffff, &out, 0);
```

Thread Sanitizer (TSan) is occasionally useful for finding — or, more
often, *proving* the presence of — data races. It has a runtime component
and so must be used at compile time and link time.

    $ cc ... -fsanitize=thread ...

Unfortunately it only works in a narrow context. The target must use
pthreads, not C11 threads, OpenMP, nor [direct cloning][clone]. It must
only synchronize through code that was compiled with TSan. That means no
synchronization [through system calls][edge], especially no futexes. Most
non-trivial programs do not meet the criteria.

### Debug information

Another common mistake in tutorials is using plain old `-﻿g` instead
of **`-﻿g3`** (read: "debug level 3"). That's like using `-﻿O`
instead of `-﻿O3`. It adds a lot more debug information to the
output, particularly enums and macros. The extra information is useful and
you're better off having it!

    $ cc ... -g3 ...

All the major build systems — CMake, Autotools, Meson, etc. — get this
wrong in their standard debug configurations. Producing a fully-featured
debug build from these systems is a constant battle for me. Often it's
easier to ignore the build system entirely and `cc -g3 **/*.c` (plus
sanitizers, etc.).

(Short term note: GCC 11, released in March 2021, switched to DWARF5 by
default. However, GDB could not access the extra `-﻿g3` debug
information in DWARF5 until GDB 13, released February 2023. If you have a
toolchain from that two year window — except [mine][w64] because I patched
it — then you may also need `-﻿gdwarf-4` to switch back to DWARF4.)

What about `-﻿Og`? In theory it enables optimizations that do not
interfere with debugging, and potentially some additional warnings. In
practice I still get far too many "optimized out" messages from GDB when I
use it, so I don't bother. Fortunately C is such a simple language that
debug builds are nearly as fast as release builds anyway.

On MSVC I like having debug information embedded in binaries, as GCC does,
which is done using **`/Z7`**.

    $ cl ... /Z7 ...

Though I certainly understand the value of separate debug information,
`/Zi`, in some cases. Sometimes I wish the GNU toolchain made this easier.

### Summary

My personal rigorous baseline for development using `gcc` and `clang`
looks like this (all platforms):

    $ cc -g3 -Wall -Wextra -Wconversion -Wdouble-promotion
         -Wno-unused-parameter -Wno-unused-function -Wno-sign-conversion
         -fsanitize=undefined -fsanitize-trap ...

While ASan is great for quickly reviewing and evaluating other people's
projects, I don't find it useful for my own programs. I avoid that class
of defects through smarter paradigms (region-based allocation, no null
terminated strings, etc.). I also prefer the behavior of trap instruction
UBSan versus a diagnostic, as it behaves better under debuggers.

For `cl` and `clang-cl`, my personal baseline looks like this:

    $ cl /Z7 /W4 /wd4146 /wd4245 /RTCcsu ...

I don't normally need `/D_CRT_SECURE_NO_WARNINGS` since I don't use a CRT
anyway.


[buf]: /blog/2023/02/13/
[clone]: /blog/2023/03/23/
[cross]: /blog/2021/08/21/
[crt]: /blog/2023/02/15/
[debug]: /blog/2022/06/26/
[edge]: /blog/2022/10/03/
[fuzz]: /blog/2019/01/25/
[hn]: https://news.ycombinator.com/item?id=35758898
[int]: /blog/2017/07/19/
[lcg]: /blog/2019/11/19/
[libc]: /blog/2023/02/11/
[man]: https://man7.org/linux/man-pages/man1/gcc.1.html
[openmp]: /blog/2017/03/01/
[reddit]: https://old.reddit.com/r/C_Programming/comments/133bjlp
[rtc]: https://learn.microsoft.com/en-us/cpp/build/reference/rtc-run-time-error-checks
[size]: https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2019/p1428r0.pdf
[w64]: https://github.com/skeeto/w64devkit
