---
title: What's been going on in w64devkit the past year
layout: post
date: 2026-09-20T01:49:36Z
tags: [c, cpp]
uuid: 843e3ff4-8bd3-4106-bca9-608a80e66545
---

The past year has been exciting for w64devkit, which like any software
distribution is never complete. [Peter0x44][] joined as co-maintainer, and
has pushed the project in good, new directions, with ideas I would never
have considered. Many of his improvements have gone back upstream, and so
you may benefit even if you don't use w64devkit. I'd like to touch on the
various odds and ends from the past year.

### Release security

In April I [announced that release packaging is now signed][sign]. Today
_all_ EXEs and DLLS in w64devkit are code-signed with my key. My signing
key established a good reputation thanks to thousands of unique signatures
observed on the order of ~100,000 hosts — a pleasant side effect from
including ~300 binaries in a release. Users should have fewer problems
these days with security software. MSYS2 has also [adopted my signing
tool][msys2], `aas-sign`, which is now included in w64devkit releases.

Builds are now automated by GitHub Actions, triggered when I (and _only_
I) push a new tag. That process code-signs and creates the release. Every
step of the release process is transparent, derived strictly from source
in the repository. Nowhere does it go behind a curtain and permit secret
tampering.

But that's not all. I enabled _release immutability_: On publish, release
artifacts are locked in and nobody, not even me, can modify them. You can
tell by the presence of a release attestation at the end of the artifacts
listing. Nobody involved in the project down the road can go rogue and
sneak something into an old release.

### Toolchain changes

The x64 release is now a "multilib" toolchain. That is it can compile
programs for 32-bit Windows, like a superset of the x86 release. The only
purpose of the x86 release is to run w64devkit on older hardware or older
operating systems. Like the x86 release, it targets Windows XP by default
and requires a CPU supporting SSE2 (i.e. at least Pentium 4).

To target x86, pass `-m32` when compiling and linking. Or better, use
tools prefixed with the `i686-w64-mingw32` architecture triple. The latter
is easier in general because different tools require different switches,
and the prefixed tools are [aliases][] that automatically do the right
thing.

    $ x86_64-w64-mingw32-gcc -o hello64.exe hello.c
    $ i686-w64-mingw32-gcc -o hello32.exe hello.c

Adding multilib was cheaper and easier than I anticipated, and it's thanks
to Peter0x44. It's already been convenient for me for several projects.

In the past you needed w64devkit's `bin/` on your `$PATH` in order to use
the compiler, because that's how GCC found the other tools it needs. This
is no longer the case, and you can invoke `path/to/gcc.exe` from scripts
and such without touching your path. This required a small GCC patch.

The standard COFF object format supports up to 65,535 sections. When the
format was designed this probably seemed like more than enough, but modern
C++ programs can easily exceed it, particularly debug builds of programs
using lots of lambda functions. As C++ projects grow they eventually hit a
threshold where builds fail unless you ask for the [bigobj][] COFF format,
which supports over 4 billion sections. This is annoying, and the tools
ought to deal with this automatically.

A couple years ago I patched Bintutils to produce bigobj by default so the
old limit would never affect a build. The catch is that Binutils lacks an
interface to request standard COFF, so you can't downgrade if needed. Why
would you need to? When interacting some simpler linkers like the official
Go toolchain (gc)! Some linkers can't consume bigobj. It also breaks some
builds that auto-detect the toolchain's object format, as some detection
scripts don't know about bigobj (ex. libbacktrace at the time).

Ideally Binutils would quietly upgrade to bigobj as needed. Only large
programs see bigobj. Naive linkers and detection scripts only deal with
standard COFF. Well, thanks to Peter0x44 that's now what Bintutils does
upstream! My bigobj hack is no longer necessary, and cgo works again.

This past year I've decided to put less emphasis on making w64devkit as
small as possible, and I'm willing to increase the installation and
distribution size for worthwhile features. This is embodied by switching
from `-Os` to `-O2` for static runtimes and tools that are computationally
expensive. Everything is now a bit bigger and a bit faster. Programs that
spend significant time in the standard library will be a little faster,
too. Programs already designed for performance will see no difference.

### New tools

CMake, Ninja, and [a new, graphical CMake debugger][dcmake] are all now
included. They require at least Windows 7. I've patched CMake to default
to Ninja, as CMake's default is to look for a Visual Studio installation.
You can still `-G MinGW Makefiles` for GNU Make instead of Ninja, but I
don't recommend it. Ninja is faster and more robust.

    $ cmake -B build       # configure for Ninja
    $ cmake --build build  # build with Ninja

`ccmake` is included (a Peter0x44 suggestion), a TUI front-end to examine
and modify CMake configurations. It's been useful, more than I expected.
You can pass `-j` for parallel builds, but I strongly recommend using the
environment instead, e.g. in your `.profile`:

    export CMAKE_BUILD_PARALLEL_LEVEL="$(nproc)"
    export CTEST_PARALLEL_LEVEL=$(nproc)

Yes, parallel testing, too! I also recommend multi-config if that makes
sense for you:

    $ cmake -B build -G 'Ninja Multi-Config'
    $ cmake --build build --config Debug
    $ cmake --build build --config Release

I was tempted to make this default, but it changes the build tree layout
(`Debug/` and `Release/` directories) and a shocking percentage of real
world `CMakeLists.txt` break under multi-config because they're written
incorrectly. The world needs a good CMake linter, which could catch most
cases statically.

Complementing is Ccache, with special patches from Peter0x44 to improve
Windows support. If you switch between branches often then using `ccache`
will speed up your (re-)builds. I also set up the conventional Ccache
`lib/ccache/` directory. It's a directory in w64devkit that when added
your `$PATH` transparently backs all your builds with Ccache. On a typical
Linux distribution this would be `/usr/lib/ccache/`. You can enabled it in
your `w64devkit.ini` using `path type`, too:

    path type = minimal+ccache

In my case Ccache hasn't been useful as I hoped when I added it. I've
adopted Git worktrees workflow instead, so each branch sits still with its
own build tree(s).

To aid COM programming, the kit now includes `widl` and `uuidgen`, the
open source alternative to Microsoft MIDL, for generating Interface
Definition Language (IDL) files. `uuidgen` is a [minimalist rewrite by
yours truly][uuidgen.c] of [the Microsoft tool][uuidgen].

[G. Berthiaume wrote][make2compdb] a new tool, `make2compdb`, that
[extracts][] JSON Compilation Database files, `compile_commands.json`,
from Make builds. It operates as a unix filter:

    $ make -Bwn | make2compdb >compile_commands.json

I wrote a `recycle` tool that sends files and folders to the recycle bin,
which for the latter is usually faster than deleting them with `rm -rf`.
The `trash` tool on other systems. I rarely delete files anymore, instead
using this command to send them off to sit in the recycle bin for a couple
weeks until Storage Sense automatically deletes them.

A [few months ago][quilt] I announced the addition of `quilt`, an old
school patch management tool. The original doesn't support Windows, so
this is a [full rewrite in C++][Quilt.cpp]. Now that every platform has
Quilt, w64devkit's patches are now managed with Quilt.

NSIS, an installer creator, is now included, so you can build your own
application installers. Just the command line tool, `makensis`. It's also
in a "multilib" configuration, and x64 w64devkit can produce 32-bit and
64-bit installers. (My motivation to add NSIS was my [alternative alt-tab
switcher][dat], which requires an installer to install properly.)

Zstandard `zstd`/`unzstd` is now included because source tarballs are
often in `.tar.zst` format. It's a great compression format, and ought to
be your default (rather than gzip) when you need compression. busybox-w32
`tar` knows to use `zstd` for `.tar.zst`.

### Runtimes

Unique to w64devkit, C11 threads are now part of the Mingw-w64 runtime. A
new implementation by yours truly, independent of winpthreads, smaller
than winpthreads, and requires no special compiler or linker flags. (If
you're careful, you can even use it in [CRT-free programs][crt].) The
catch is that it requires Windows 7 or later because I built it on [SRW
locks][srw]; XP would have required substantial complexity.

Writing an implementation taught me that _C11 threads are underspecified
poorly designed_ — in case it wasn't already obvious by the presence of
[recursive locks][rec]! This C standard addition did not get nearly the
attention is needed and should have been cut ([par for the course][par]).
My implementation excludes recursive locks. Trying to create one always
fails. It also has a weakened `thrd_current` because the semantics don't
map onto the Windows threading model.

`std::terminate` actually terminates (traps) instead of calling `exit`, so
it traps in GDB for inspection. This includes uncaught exceptions. It also
no longer prints a half-baked stack trace, shedding ~100k of dead weight
from most C++ programs. I did this in two steps a year apart, the second
just recently. I misplaced the trap in the first change, preventing it
from shedding as much weight as it could.

### Future directions

Years ago I disabled Link-Time Optimization (LTO) due to bugs in GCC and
Binutils. Just having it available in the toolchain triggered LTO bugs.
These problems reduced my faith that LTO could produce correct programs,
so I disabled it.

However, I'm toying with the idea of not only re-enabling LTO, but even
distributing "FatLTO" runtimes, particularly for C++ and Fortran. That is,
runtime object files will contain both native code (at `-O2` per above)
and LTO bytecode. If you don't request LTO, you get the pre-compiled
native code just like today, at no link-time cost. If you enable LTO, it
essentially rebuilds the runtime itself at link time. Very computationally
expensive, but you get full control, and you can even coerce it back to
the old `-Os` (or even `-Oz`) if you prefer that.

As far as I can tell, nobody actually distributes a toolchain with FatLTO
runtimes, so w64devkit might be the first. I know I'm the first to cross
compile FatLTO objects with GCC — remember that everything in w64devkit is
cross-compiled — because it's never worked correctly in a GCC release. My
first attempt produced a toolchain that didn't work.

I'm feeling more confident this time around because fixing LTO bugs is now
very easy: Strap a frontier AI into a good coding harness, show it what's
broken, give it all the sources (`source.tar`), and ~15 minutes later I
have a patch. We live in a sci-fi world. I started by fixing known-to-me
LTO bugs, enabled FatLTO, then worked through newly-discovered LTO bugs.
I've been dogfooding it, and nothing new has popped up in awhile, but it
will take some time for me to feel confident. I'm less confident in my
ability to discover Fortran runtime bugs. Upstream GCC [rejects these
fixes out of hand][ai], so through no choice of my own w64devkit will have
a unique edge in this space.

While some of the new features require at least Windows 7, I'm still
committed to supporting Windows XP as a baseline for the x86 release. I
keep an old XP laptop on hand, on which I test and enjoy w64devkit. You
just have to make do with some older tools, but that's what you were
already doing anyway.


[Peter0x44]: https://peter0x44.github.io/
[Quilt.cpp]: https://github.com/skeeto/quilt.cpp
[ai]: https://gcc.gnu.org/ai-policy.html
[aliases]: /blog/2021/02/08/
[bigobj]: https://peter0x44.github.io/posts/bigobj_format_explained/
[crt]: /blog/2023/02/15/
[dat]: https://github.com/skeeto/dumb-alt-tab
[dcmake]: /blog/2026/04/07/
[extracts]: https://github.com/skeeto/w64devkit/issues/251
[make2compdb]: https://github.com/skeeto/w64devkit/pull/392
[msys2]: https://www.msys2.org/dev/code-signing/
[par]: /blog/2023/02/11/
[quilt]: https://nullprogram.com/blog/2026/03/29/#quiltcpp
[rec]: https://blog.stephencleary.com/2013/04/recursive-re-entrant-locks.html
[sign]: /blog/2026/04/25/
[srw]: /blog/2024/10/03/
[uuidgen]: https://learn.microsoft.com/en-us/windows/win32/rpc/generating-interface-uuids
[uuidgen.c]: https://github.com/skeeto/w64devkit/blob/master/src/uuidgen.c
