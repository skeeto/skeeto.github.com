---
title: Tips for more effective fuzz testing with AFL++
layout: post
date: 2025-02-05T18:03:55Z
tags: [c, cpp, tutorial]
uuid: eff3b773-99ee-4c38-9f9c-f51294a1b9e0
---

Fuzz testing is incredibly effective for mechanically discovering software
defects, yet remains underused and neglected. Pick any program that must
gracefully accept complex input, written *in any language*, which has not
yet been been fuzzed, and fuzz testing usually reveals at least one bug.
At least one program currently installed on your own computer certainly
qualifies. Perhaps even most of them. [Everything is broken][broken] and
low-hanging fruit is everywhere. After fuzz testing ~1,000 projects [over
the past six years][first], I've accumulated tips for picking that fruit.
The checklist format has worked well in the past ([1][asm], [2][sdl2]), so
I'll use it again. This article discusses [AFL++][afl] on source-available
C and C++ targets, running on glibc-based Linux distributions, currently
the best fuzzing platform for C and C++.

My tips complement the official, upstream documentation, so consult them,
too:

* [Performance Tips][tips] on the AFL++ website
* [Technical "whitepaper" for afl-fuzz][paper]

Even if a program has been fuzz tested, applying the techniques in this
article may reveal defects missed by previous fuzz testing.

### (1) Configure sanitizers and assertions

More assertions means more effective fuzzing, and sanitizers are a kind of
automatically-inserted assertions. By default, fuzz with both Address
Sanitizer (ASan) and Undefined Behavior Sanitizer (UBSan):

    $ afl-gcc-fast -g3 -fsanitize=address,undefined ...

ASan's default configuration is not ideal, and should be adjusted via the
`ASAN_OPTIONS` environment variable. If customized at all, AFL++ requires
at least these options:

    export ASAN_OPTIONS="abort_on_error=1:halt_on_error=1:symbolize=0"

Except `symbolize=0`, [this *ought to be* the ASan default][assert]. When
debugging a discovered crash, you'll want UBSan set up the same way so
that it behaves under in a debugger. To improve fuzzing, make ASan even
more sensitive to defects by detecting use-after-return bugs. It slows
fuzzing slightly, but it's well worth the cost:

    ASAN_OPTIONS+=":detect_stack_use_after_return=1"

By default ASan fills the first 4KiB of fresh allocations with a pattern,
to help detect use-after-free bugs. That's not nearly enough for fuzzing.
Crank it up to completely fill virtually all allocations with a pattern:

    ASAN_OPTIONS+=":max_malloc_fill_size=$((1<<30))"

In the default configuration, if a program allocates more than 4KiB with
`malloc` then, say, uses `strlen` on the uninitialized memory, no bug will
be detected. There's almost certainly a zero somewhere after 4KiB. Until I
noticed it, the 4KiB limit hid a number of bugs from my fuzz testing. Per
(4), fulling filling allocations with a pattern better isolates tests when
using persistent mode.

When fuzzing C++ and linking GCC's libstdc++, consider `-D_GLIBCXX_DEBUG`.
ASan cannot "see" out-of-bounds accesses within a container's capacity,
and the extra assertions fill in the gaps. Mind that it changes the ABI,
though fuzz testing will instantly highlight such mismatches.

### (2) Prefer the persistent mode

While AFL++ can fuzz many programs in-place without writing a single line
of code (`afl-gcc`, `afl-clang`), prefer AFL++'s [persistent mode][fast]
(`afl-gcc-fast`, `afl-clang-fast`). It's typically an order of magnitude
faster and worth the effort. Though it also has pitfalls (see (4), (5)). I
keep a file on hand, `fuzztmpl.c` — the progenitor of all my fuzz testers:

```c
#include <unistd.h>

__AFL_FUZZ_INIT();

int main(void)
{
    __AFL_INIT();
    char *src = 0;
    unsigned char *buf = __AFL_FUZZ_TESTCASE_BUF;
    while (__AFL_LOOP(10000)) {
        int len = __AFL_FUZZ_TESTCASE_LEN;
        src = realloc(src, len);
        memcpy(src, buf, len);
        // ... send src to target ...
    }
}
```

I [`:r`][read] this into my Vim buffer, then modify as needed. It's a
stripped and improved version of the official template, which itself has a
serious flaw (see (5)). There are unstated constraints about the position
of `buf` and `len` in the code, so if in doubt, refer to the original
template.

### (3) Include source files, not header files

We're well into the 21st century. Nobody is compiling software on 16-bit
machines anymore. Don't get hung up on the one translation unit (TU) per
source file mindset. When fuzz testing, we need at most two TUs: One TU
for instrumented code and one TU for uninstrumented code. In most cases
the latter takes the form of a library (libc, libstdc++, etc.) and we
don't need to think about it.

Fuzz testing typically requires only a subset of the program. Including
just those sources straight in the template is both effective and simple.
In my template I put includes just *above* `unistd.h` so that the header
isn't visible to the sources unless they include it themselves.

```c
#include "src/utils.c"
#include "src/parser.c"
#include <unistd.h>
```

I know, if you've never seen this before it looks bonkers. This isn't what
they taught you in college. Trust me, [this simple technique][unity] will
save you a thousand lines of build configuration. Otherwise you'll need to
manage different object files between fuzz testing and otherwise.

Perhaps more importantly, you can now fuzz test *any arbitrary function*
in the program, including static functions! They're all right there in the
same TU. You're not limited to public-facing interfaces. Perhaps you can
skip (7) and test against a better internal interface. It also gives you
direct access to static variables so that you can clear/reset them between
tests, per (4).

Programs are often not designed for fuzz testing, or testing generally,
and it may be difficult to tease apart tightly-coupled components. Many of
the programs I've fuzz tested look like this. This technique lets you take
a hacksaw to the program and substitute troublesome symbols just for fuzz
testing without modifying a single original source line. For example, if
the source I'm testing contains a `main` function, I can remove it:

```c
#define main oldmain
#  include "src/utils.c"
#  include "src/parser.c"
#undef main
#include <unistd.h>
```

Sure, better to improve the program so that such hacks are unnecessary,
but most cases I'm fuzz testing as part of a drive-by review of some open
source project. It allows me to quickly discover defects in the original,
unmodified program, and produces simpler bug reports like, "Compile with
ASan, open this 50-byte file, and then the program will crash."

### (4) Isolate fuzz tests from each other

Tests should be unaffected by previous tests. This is challenging in
persistent mode, sometimes even impractical. That means resetting all
global state, even something like the internal `strtok` buffer if that
function is used. Add fuzz testing to your list of reasons to eschew
global variables.

It's mitigated by (1), but otherwise uninitialized heap memory may hold
contents from previous tests, breaking isolation. Besides interference
with fuzzing instrumentation, bugs found this way are wickedly difficult
to reproduce.

Don't pass uninitialized memory into a test, e.g. an output parameter
allocated on the stack. Zero-initialize or fill it with a pattern. If it
accepts an arena, fill it with a pattern before each test.

Typically you have little control over heap addresses, which likely varies
across tests and depends on the behavior previous tests. If the program
[depends on address values][uptr], this may affect the results and make
reproduction difficult, so watch for that.

### (5) Do not test directly on the fuzz test buffer

Passing `buf` and `len` straight into the target is the most common
mistake, especially when fuzzing better-designed C programs, and
particularly because the official template encourages it.

```c
    myprogram(buf, len);  // BAD!
```

While it's a great sign the program doesn't depend on null termination, it
creates a subtle trap. The underlying buffer allocated by AFL++ is larger
than `len`, and ASan will not detect read overflows on inputs! Instead
pass a copy sized to fit, which is the purpose of `src` in my template.
Adjust the type of `src` as needed.

If the program expects null-terminated input then you'll need to do this
anyway in order to append the null byte. If it accepts an "owning" type
like `std::string`, then it's also already done on your behalf. With
"non-owning" views like `std::string_view` you'll still want to your own
size-fit copy.

If you see a program's checked in fuzz test using `buf` directly, make
this change and see if anything new pops out. It's worked for me on a
number of occasions.

### (6) Don't bother freeing memory

In general, avoid doing work irrelevant to the fuzz test. The official
tips say to "use a simpler target" and "instrument just what you need,"
and keeping destructors out of the tests helps in both cases. Unless the
program is especially memory-hungry, you won't run out of memory before
AFL++ resets the target process.

If not for (1), it also helps with isolation (4), as different tests are
less likely contaminated with uninitialized memory from previous tests.

As an exception, if you want your destructor included in the fuzz test,
then use it in the test. Also, it's easy to exhaust non-memory resources,
particularly file descriptors, and you may need to [clean those up][range]
in order to fuzz test reliably.

Of course, if the target uses [arena allocation][arena] then none of this
matters! It also makes for perfect isolation, as even addresses won't vary
between tests.

### (7) Use a memory file descriptor to back named paths

Many interfaces are, shall we say, *not so well-designed* and only accept
input from a named file system path, insisting on opening and reading the
file themselves. Testing such interfaces presents challenges, especially
if you're interested in parallel fuzzing. Fortunately there's usually an
easy out: Create a memory file descriptor and use its `/proc` name.

```c
int fd = memfd_create("fuzz", 0);
assert(fd == 3);
while (...) {
    // ...
    ftruncate(fd, 0);
    pwrite(fd, buf, len, 0);
    myprogram("/proc/self/fd/3");
}
```

With standard input as 0, output as 1, and error as 2, I've assumed the
memory file descriptor will land on 3, which makes the test code a little
simpler. If it's not 3 then something's probably gone wrong anyway, and
aborting is the best option. If you don't want to assume, use `snprintf`
or whatever to construct the path name from `fd`.

Using `pwrite` (instead of `write`) leaves the file description offset at
the beginning of the file.

Thanks to the memory file descriptor, fuzz test data doesn't land in
permanent storage, so less wear and tear on your SSD from the occasional
flush. Because of `/proc`, the file is unique to the process despite the
common path name, so no problems parallel fuzzing. No cleanup needed,
either.

If the program wants a file descriptor — i.e. it wants a socket because
you're fuzzing some internal function — pass the file descriptor directly:

```c
    myprogram(fd);
```

If it accepts a `FILE *`, you *could* `fopen` the `/proc` path, but better
to use `fdmemopen` to create a `FILE *` on the object:

```c
    myprogram(fdmemopen(buf, len, "rb"));
```

Note how, per (6), we don't need to bother with `fclose` because it's not
associated with a file descriptor.

### (8) Configure the target for smaller buffers

A common sight in [diseased programs][disease] are "generous" fixed buffer
sizes:

```c
#define MY_MAX_BUFFER_LENGTH 65536

void example(...)
{
    char path[PATH_MAX];  // typically 4,096
    char buf[MY_MAX_BUFFER_LENGTH];
    // ...
}
```

These huge buffers tend to hide bugs. Turn those stones over! It takes a
lot of fuzzing time to max them out and excite the unhappy paths — or the
super-unhappy paths, overflows. Better if the fuzz test can reach worst
case conditions quickly and explore the execution paths out of it.

So when you see these, cut them way down, possibly using (3). Change 65536
to, say, 16 and see what happens. If fuzzing finds a crash on the short
buffer, typically extending the input to crash on the original buffer size
is straightforward, e.g. repeat one of the bytes even more than it already
repeats.

### Conclusion and samples

Hopefully something here will help you catch a defect that would have
otherwise gone unnoticed. Even better, perhaps awareness of these fuzzing
techniques will prevent the bug in the first place. Thanks to my template,
some solid tooling, and the know-how in this article, I can whip up a fuzz
test in a couple of minutes. But that ease means I discard it as just as
casually, and so I don't take time to capture and catalog most. If you'd
like to see some samples, [I do have an old, short list][samples]. Perhaps
after another kiloproject of fuzz testing I'll pick up more techniques.


[afl]: https://aflplus.plus/
[arena]: /blog/2023/09/27/
[asm]: /blog/2024/12/20/
[assert]: /blog/2022/06/26/
[broken]: https://danluu.com/everything-is-broken/
[disease]: http://catb.org/jargon/html/C/C-Programmers-Disease.html
[fast]: https://github.com/AFLplusplus/AFLplusplus/blob/stable/instrumentation/README.persistent_mode.md
[first]: /blog/2019/01/25/
[paper]: https://lcamtuf.coredump.cx/afl/technical_details.txt
[range]: https://man7.org/linux/man-pages/man2/close_range.2.html
[read]: https://vimhelp.org/insert.txt.html#%3Aread
[samples]: https://old.reddit.com/r/C_Programming/comments/15wouat/_/jx2ld4a/
[sdl2]: /blog/2023/01/08/
[tips]: https://afl-1.readthedocs.io/en/latest/tips.html
[unity]: https://en.wikipedia.org/wiki/Unity_build
[uptr]: /blog/2025/01/19/#hash-hardening-bonus
