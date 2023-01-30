---
title: 'u-config: a new, lean pkg-config clone'
layout: post
date: 2023-01-18T06:39:51Z
tags: []
uuid: c07ce83a-7871-4561-a77f-3b62b7a817bd
excerpt_separator: <!--more-->
---

*This article was discussed [on Hacker News][hn].*

In [my common SDL2 mistakes listing][sdl2], the first was about winging it
instead of using the `sdl2-config` script. It's just one of three official
options for portably configuring SDL2, but I had dismissed the others from
consideration. One is the [pkg-config][] facility common to unix-like
systems. However, the SDL maintainers recently announced SDL3, which will
not have a `sdl3-config`. The concept has been deprecated in favor of the
existing pkg-config option. I'd like to support this on w64devkit, except
that it lacks pkg-config — not the first time this has come up. So last
weekend I wrote a new pkg-config from scratch with first-class Windows
support: **[u-config][]** ("*micro*-config"). It will serve as pkg-config
in w64devkit starting in the next release.

<!--more-->

Ultimately pkg-config's entire job is to find named `.pc` text files in
one of several predetermined locations, read fields from them, then write
those fields to standard output. Additional search directories may be
supplied through the `$PKG_CONFIG_PATH` environment variable. At a high
level there's really not much to it.

As a concrete example, here's a hypothetical `example.pc` which might live
in `/usr/lib/pkgconfig`.

```
prefix = /usr
major = 1
minor = 2
patch = 3
version = ${major}.${minor}.${patch}

Name: Example Library
Description: An example of a .pc file
Version: ${version}
Requires: zlib >= 1.2, sdl2
Libs: -L${prefix}/lib -lexample
Libs.private: -lm
Cflags: -I${prefix}/include
Cflags.private: -DEXAMPLE_STATIC
```

If you invoke pkg-config with `--cflags` you get the `Cflags` field. With
`--libs`, you get the `Libs` field. With `--static`, you also get the
"private" fields. It will also recursively pull in packages mentioned in
`Requires`. The `prefix` variable is more than convention and is designed
to be overridden (and u-config does so by default). In theory pkg-config
is supposed to be careful about maintaining argument order and removing
redundant arguments, but in practice… well, pkg-config's actual behavior
often makes little sense. We'll get to that.

For SDL2, where you might use:

    $ cc app.c $(sdl2-config --cflags --libs)

You could instead use:

    $ eval cc app.c $(pkg-config sdl2 --cflags --libs)

Which is still a build command that works uniformly for all supported
platforms, even cross-compiling, given a correctly-configured pkg-config.
For w64devkit, the first command requires placing the directory containing
`sdl2-config` on your `$PATH`. The second instead requires placing the
directory containing `sdl2.pc` in your `$PKG_CONFIG_PATH`. To upgrade to
SDL3, replace the `sdl2` with `sdl3` in the second command.

### Why two when you can have three?

There are already two major, mostly-compatible pkg-config implementations:
the original from freedesktop.org (2001), and [pkgconf][] (2011). Both
ostensibly support Windows, but in practice this support is second class,
which is a reason why I hadn't included one in w64devkit. A lot of hassle
for what is a ultimately a relatively simple task.

As for the original pkg-config, I've been unable to produce a functioning
Windows build. It's obvious from the compiler warnings that there are many
problems, and my builds immediately crash on start. I'd try debugging it,
except that I've been cross-compiling this whole time. I cannot build it
on Windows because (1) GNU Autotools and (2) pkg-config ~~requires~~wants
pkg-config as a build dependency. That's right, *you have to bootstrap
pkg-config*! Remember, this is a tool whose entire job is to copy some
bits of text from a text file to its output. One could use pkg-config as a
case study of accidental complexity, and this is just the beginning.

*Update*: It was [pointed out][debug] that I wouldn't need the full,
two-stage bootstrap just for my debugging scenario.

The bootstrap issue is part of pkgconf's popularity as an alternative.
It's also a tidier code base, does a *far* better job of sorting and
arranging its outputs than the original pkg-config, and its overall
behavior makes more sense. However, despite its three independent build
systems, pkgconf is still annoying to build, not to mention its memory
corruption bugs. We'll get to that, too.

Considering pkg-config's relatively simple job, obtaining one shouldn't be
this difficult! I could muddle through until one or the other worked, or I
could just write my own. I'm glad I did, since I'm extremely happy with
the results.

### u-config implementation

As of this writing, u-config is about 2,000 lines of C. It doesn't support
every last pkg-config feature, nor will it ever. The goal is to support
support existing pkg-config based builds, not make more of them. So, for
example, features for debugging `.pc` files are omitted. Some features are
of dubious usefulness (`--errors-to-stdout`) even if they'd be simple to
implement; there are already way too many flags. Other features clearly
don't work correctly — either not as documented or the results don't make
sense — so I skipped those as well.

It comes in two flavors: "generic" C and Windows. The former works on any
system with a C99 compiler. In fact, it only uses these 9 standard library
functions:

* `exit`
* `fclose`
* `ferror`
* `fflush`
* `fopen`
* `fread`
* `fwrite`
* `getenv`
* `malloc`

That is, it needs to open `.pc` files, read from them, close those
handles, write to standard output and standard error, check for I/O
errors, and exactly once call `malloc` to allocate a block of memory for
an arena allocator. It's not even important the streams are buffered
because u-config does its own buffering. Not that it would be useful, but
porting to an unhosted 16-bit microcontroller, with `fopen` implemented as
a virtual file system, would be trivial. (You know… it could be dropped
into [busybox-w32][] as a new app with little effort…)

It's also a unity build — compiled as a single translation unit — so
building u-config is as easy as it gets:

    $ cc -o pkg-config generic_main.c

Reminder: the original pkg-config *cannot even be built without a
bootstrapping step.*

Since standard C functions are [implemented poorly on Windows][c], but
also so that it can do some smarter self-configuration at run-time based
on the `.exe` location, the Windows platform layer calls directly into
Win32 and no C runtime (CRT) is used. Input `.pc` files are memory mapped.
Internally u-config is all UTF-8, and the platform layer does the Unicode
translations at the Win32 boundaries for paths, [arguments][], environment
variables, and console outputs.

Building is *slightly* more complicated:

    $ cc -o pkg-config -nostartfiles win32_main.c

### Implementation highlights

Greenfield projects present a great opportunity for trying new things, and
this is no exception. Contrary to my usual style, I decided I would make
substantial use of `typedef` and capitalize all the type names.

```c
typedef int Bool;
typedef unsigned char Byte;

typedef struct {
    Byte *s;
    Size len;
} Str;

typedef struct {
    Str head;
    Str tail;
    Bool ok;
} Cut;
```

I like it! It makes the type names stand apart, avoids conflicts with
variable names, and cuts down the visual noise of `struct`. I've more
recently realized that `const` is doing virtually nothing for me — it has
never prevented me from making a mistake — so I left it out (aside from
static lookup tables). That's even more visual noise gone, and reduced
cognitive load.

In recent years I've been convinced that unsigned sizes were a serious
error, probably even one of the great early computing mistakes, and that
[sizes and subscripts should be signed][signed]. Not only that, pkg-config
has no business dealing with gigantic objects! We're talking about short
strings and tiny files. If it ends up with a large object, then there's a
defect somewhere — either in itself or the system — and it should abort.
Therefore sizes and subscripts are a natural `int`!

```c
typedef int Size;
typedef unsigned Usize;
#define Size_MAX (Size)((Usize)-1 >> 1)
#define SIZEOF(x) (Size)(sizeof(x))
```

The `Usize` is just for the occasional bit-twiddling, like in `Size_MAX`,
and not for regular use. However, u-config objects are no smaller by this
decision because the unused space is nearly always padded on 64-bit
machines. Further, the x86-64 code is about 5% larger with 32-bit sizes
compared to 64-bit sizes — opposite my expectation. Curious.

You might have noticed that `Str` type above. Aside from interfaces with
the host that make it mandatory, u-config makes no use of null-terminated
strings anywhere. Every string is a pointer and a size. There's even a
macro to do this for string literals:

```c
#define S(s) (Str){(Byte *)s, SIZEOF(s)-1}
```

Then I can use and pass them casually:

```c
    if (equals(realname, S("pkg-config"))) {
        // ...
    }

    *insert(arena, &global, S("pc_sysrootdir")) = S("/");

    return startswith(arg, S("-I"));
```

Like strings in other languages, I can also slice out the middle of
strings without copying, handy for parsing and constructing paths. It also
works well with memory-mapped `.pc` files since I can extract tokens from
them for use directly in data structures without copying.

That leads into the next item: How does one free or manipulate a data
structure where the different parts are arbitrarily allocated across
static storage, heap storage, and memory mapped files? The hash tables in
u-config are exactly this, the keys themselves allocated in every possible
fashion. Don't you have to keep track of how pointed-at part is allocated?
No! The individual objects do not have [individual lifetimes][arena] due
to the arena allocator. The gist of it:

```c
typedef struct {
    Str mem;
    Size off;
} Arena;

static void *alloc(Arena *a, Size size)
{
    ASSERT(size >= 0);
    Size avail = a->mem.len - a->off;
    if (avail < size) {
        oom();
    }
    Byte *p = a->mem.s + a->off;
    a->off += size;
    return p;
}
```

Since it's passed often, arena parameters are conventionally named `a`
throughout the program and are always the first argument when needed. If
it runs out of memory, it bails. On 32-bit and 64-bit hosts, the default
arena is 256MiB. If pkg-config needs more than that, then something's
seriously wrong and it should give up.

While u-config *could* quite reasonably never "free" (read: reuse) memory,
it *does* do so in practice. In some cases it computes a temporary result,
then resets the arena to an earlier state to discard its allocations. A
simplified, hypothetical:

```c
    for (int i = 0; ...) {
        Arena tmparena = *a;
        // Use only tmparena in the loop
        Env env = {0};
        Str value = fmtint(&tmparena, i);
        *insert(&tmparena, &env, S("i")) = value;
        // ...
        // allocations freed when tmparena goes out of scope
    }
```

I had mentioned that u-config does its own output buffering. It's an
object I call an `Out`, modeled loosely after a [Plan 9 `bio`][p9] or a Go
`bufio.Writer`. It has a destination "file descriptor", a memory buffer,
and an integer to track the fill level of the buffer.

```c
typedef struct {
    Str buf;
    Size fill;
    Arena *a;
    int fd;
} Out;
```

Output bytes are copied into the buffer. When it fills, the buffer is
automatically emptied into the file descriptor. The caller can manually
flush the buffer at any time, and it's up to the caller to do so before
exiting the program.

But wait, what's the `Arena` pointer doing in there? That's a little extra
feature of my own invention! I can open a stream on an arena, writes into
the stream go into a growing buffer, and "closing" the stream gives me a
string allocated in the arena with the written content. The arena is held
in order to manage all this. It's also locked out from other allocations
until the stream is closed. The entire implementation is only about a
dozen lines of code.

What use is this? It's nice when I might want to output either to standard
output or to a memory buffer for further use. It's even more useful when I
need to build a string but don't know its final length ahead of time.

The variable expansion function is both cases. Given a string like
`${version}` I want to recursively interpolate until there's nothing left
to interpolate. The output could go to standard output to print it out, or
into a string for further use. For example, here I have my global variable
environment `global`, a package `pkg`, its environment (`pkg->env`), and I
want to expand its `Version:` field, `pkg->version`.

```c
    Out mem = newmembuf(a);
    expand(&mem, global, pkg, pkg->version);
    Str version = finalize(&mem);
```

Or I just print it to standard output, and the value is free to expand
beyond what would fit in memory since it flushes as it goes:

```c
    Out out = newoutput(1);  // 1 == standard output
    expand(&out, global, pkg, pkg->version);
    flush(&out);
```

I'm particularly happy about this, and I'm sure I'll use such "arena
streams" again in the future.

### Subtleties

While pkgconf tries, and succeeds at, being a faithful (if smarter) clone,
in certain ways u-config more closely follows pkg-config's behavior. For
example, pkg-config behaves as though it concatenates all its positional
arguments with commas in between, then re-tokenizes them like a `Requires`
field. For example, these commands are all equivalent:

    $ pkg-config 'sdl2 > 2' --libs
    $ pkg-config 'sdl2 >' --libs 2
    $ pkg-config sdl2 --libs '> 2'
    $ pkg-config --libs 'sdl2 > 2'

pkgconf does not copy this behavior, but u-config does. Similarly, the
original `.pc` format has undocumented, arcane quoting syntax that sort of
works like shell quotes. I tried to match this closely in u-config, while
pkgconf tries to be more logical. For example, pkg-config allows this:

```
quote = "
Cflags: "-I${prefix}/include${quote}
```

Where the `${quote}` will actually close the quote. I retained this but
pkgconf did not.

Does anyone use quoting? On my own system I have one package using quotes,
but it's probably a mistake since they're used improperly. In theory,
everyone should be quoting almost everything. For example, this is a very
common `Cflags`:

```
Cflags: -I${prefix}/include
```

If a crazy person — or well-known multinational corporation — comes along
puts has a space in their system's installation "prefix", this `.pc` will
not work. The output would be:

    -I/Program Files/include

Actually, that's a lie. I suspect that's the *intended* output, and it's
the output of pkgconf and u-config, but pkg-config instead outputs this
head-scratcher:

    Files/include -I/Program

Seeing this sort of thing repeatedly is why I have little concern with
matching every last pkg-config nuance. Regardless, this parses as two
arguments, but if written with quotes:

```
Cflags: "-I${prefix}/include"
```

Then pkg-config will escape spaces in the expansion:

    -I/Program\ Files/include

This will actually work correctly in the `eval` context where `pkg-config`
is intended for use (read: [*not command substitution*][cs]). I've made
u-config automatically quote the prefix if it contains spaces, so it will
work correctly despite the lack of `.pc` file quotes when the library is
under a path containing a space.

Here's a fun input. pkg-config has its own [billion laughs][b]:

```
v9=lol
v8=${v9}${v9}${v9}${v9}${v9}${v9}${v9}${v9}${v9}${v9}
v7=${v8}${v8}${v8}${v8}${v8}${v8}${v8}${v8}${v8}${v8}
v6=${v7}${v7}${v7}${v7}${v7}${v7}${v7}${v7}${v7}${v7}
v5=${v6}${v6}${v6}${v6}${v6}${v6}${v6}${v6}${v6}${v6}
v4=${v5}${v5}${v5}${v5}${v5}${v5}${v5}${v5}${v5}${v5}
v3=${v4}${v4}${v4}${v4}${v4}${v4}${v4}${v4}${v4}${v4}
v2=${v3}${v3}${v3}${v3}${v3}${v3}${v3}${v3}${v3}${v3}
v1=${v2}${v2}${v2}${v2}${v2}${v2}${v2}${v2}${v2}${v2}
v0=${v1}${v1}${v1}${v1}${v1}${v1}${v1}${v1}${v1}${v1}
Name: One Billion Laughs
Version: ${v0}
Description: Don't install this!
```

That expands to 1,000,000,001 "lol" (an extra for good luck!) and in
theory `--modversion` will print it out:

    $ pkg-config --modversion lol.pc

Some different outcomes:

* pkg-config will expand it in memory and see it to the bitter end, using
  however many GiBs are necessary. Add a few more lines and your computer
  will thrash. By the way, bash-completion will ask pkg-config load `.pc`
  files named in the command when completing further arguments. Ask me how
  I know.

* u-config could fully output it with only a few kB of memory if directed
  to a "file descriptor" output, but alas, the `Version` field must be
  processed in memory for comparison with another version string, so it
  doesn't attempt to do so. It runs out of arena memory and gives up.
  That's a feature, especially if you're using bash-completion.

* pkgconf I had built with Address Sanitizer in case it found anything,
  and boy did it. This input overflows a stack variable and then ASan
  kills it. I'm unsure what's supposed to happen next, but I suspect
  silent truncation.

But that's a crazy edge case right? Well, it also overflows on *empty
`.pc` files*, or for all sorts of inputs. I probed both pkg-config and
pkgconf with weird inputs to learn how it's supposed to work, and it was
rather irritating having pkgconf crash for so many of them. Someone on the
project ought to do testing with ASan sometime. Important note: *This is
not a security vulnerability*!

Further, as you might notice when you build it, pkgconf first tries to
link the system `strlcpy`, if it exists. Failing that, it uses its own
version. That's one of the annoying details about building it. However,
[using `strlcpy` never, ever makes sense][strcpy]! Now that I think about
it, there's probably a connection with those buffer overflows.

In general, neither pkg-config nor pkgconf fare well when [fuzz tested
with sanitizers][fuzz].

### Conclusions

I had a lot of fun writing u-config, and I'm excited about this new
addition to w64devkit. Despite my pkg-config grumbling, it *is* neat that
it's established this *de facto* standard and encouraged a distributed
database of `.pc` files to exist, at least as documentation if not for a
mechanical process like this.

For u-config, there's still more testing to do, and I'm still open to
picking up more behaviors from pkg-config or pkgconf where they make
sense. Though given its primary use case — building software on Windows
without a package manager — it will probably never be stressed hard enough
to matter. Further, w64devkit does not include any `.pc` files of its own,
and since I do not intend to add libraries — that is, beyond the standard
language libraries and Windows SDK — that probably won't change.

If you'd like to try it early, build it with w64devkit, toss in on your
`PATH`, point `PKG_CONFIG_PATH` at a library with `.pc` files, and try it
out. It already works flawlessly with at least SDL2.


[arena]: https://www.rfleury.com/p/untangling-lifetimes-the-arena-allocator
[arguments]: /blog/2022/02/18/
[b]: https://en.wikipedia.org/wiki/Billion_laughs_attack
[busybox-w32]: https://frippery.org/busybox/
[c]: /blog/2021/12/30/
[cs]: https://github.com/skeeto/u-config/issues/1#issuecomment-1397700442
[debug]: https://lists.sr.ht/~skeeto/public-inbox/%3C1750680.o7JgDH7DvH%40laptop%3E
[fuzz]: /blog/2019/01/25/
[hn]: https://news.ycombinator.com/item?id=34426430
[p9]: https://9p.io/sys/doc/comp.html
[pkg-config]: https://www.freedesktop.org/wiki/Software/pkg-config/
[pkgconf]: http://pkgconf.org/
[sdl2]: /blog/2023/01/08/
[signed]: https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2019/p1428r0.pdf
[strcpy]: /blog/2021/07/30/
[u-config]: https://github.com/skeeto/u-config
[w64devkit]: https://github.com/skeeto/w64devkit
