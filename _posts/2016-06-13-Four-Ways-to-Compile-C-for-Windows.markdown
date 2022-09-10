---
title: Four Ways to Compile C for Windows
layout: post
date: 2016-06-13T04:13:25Z
tags: [c, cpp, win32]
uuid: 1e99288c-0500-36f5-9fe7-262e6c6287c4
---

*Update 2020: If you're on Windows, just use [**w64devkit**][w64devkit].
It's [my own toolchain distribution][announce], and it's the best option
available. [Everything you need][everything] is in one package.*

I primarily work on and develop for unix-like operating systems —
Linux in particular. However, when it comes to desktop applications,
most potential users are on Windows. Rather than develop on Windows,
which I'd rather avoid, I'll continue developing, testing, and
debugging on Linux while keeping portability in mind. Unfortunately
every option I've found for building Windows C programs has some
significant limitations. These limitations advise my approach to
portability and restrict the C language features used by the program
for all platforms.

As of this writing I've identified four different practical ways to
build C applications for Windows. This information will definitely
become further and further out of date as this article ages, so if
you're visiting from the future take a moment to look at the date.
Except for LLVM shaking things up recently, development tooling on
unix-like systems has had the same basic form for the past 15 years
(i.e. dominated by GCC). While Visual C++ has been around for more
than two decades, the tooling on Windows has seen more churn by
comparison.

Before I get into the specifics, let me point out a glaring problem
common to all four: Unicode arguments and filenames. Microsoft jumped
the gun and adopted UTF-16 early. UTF-16 is a kludge, a worst of all
worlds, being a variable length encoding (surrogate pairs), backwards
incompatible ([unlike UTF-8][utf8]), and having byte-order issues (BOM).
Most Win32 functions that accept strings generally come in two flavors,
ANSI and UTF-16. The standard, portable C library functions wrap the
ANSI-flavored functions. This means **portable C programs can't interact
with Unicode filenames**. (Update 2021: [Now they can][sane].) They must
call the non-portable, Windows-specific versions. This includes `main`
itself, which is only handed ANSI-truncated arguments.

Compare this to unix-like systems, which generally adopted UTF-8, but
rather as a convention than as a hard rule. The operating system
doesn't know or care about Unicode. Program arguments and filenames
are just zero-terminated bytestrings. Implicitly decoding these as
UTF-8 [would be a mistake anyway][args]. What happens when the
encoding isn't valid?

This doesn't *have* to be a problem on Windows. A Windows standard C
library could connect to Windows' Unicode-flavored functions and
encode to/from UTF-8 as needed, allowing portable programs to maintain
the bytestring illusion. It's only that none of the existing standard
C libraries do it this way.

### Mingw-w64

Of course my first natural choice is MinGW, specifically the
[Mingw-w64][mingw64] fork. It's GCC ported to Windows. You can
continue relying on GCC-specific features when you need them. It's got
all the core language features up through C11, plus the common
extensions. It's probably packaged by your Linux distribution of
choice, making it trivial to cross-compile programs and libraries from
Linux — and with Wine you can even execute them on x86. Like regular
GCC, it outputs GDB-friendly DWARF debugging information, so you can
debug applications with GDB.

If I'm using Mingw-w64 on Windows, ~~I prefer to do so from inside
Cygwin~~. Since it provides a complete POSIX environment, it maximizes
portability for the whole tool chain. This isn't strictly required.

However, it has one big flaw. Unlike unix-like systems, Windows doesn't
supply a system standard C library. That's the compiler's job. But
Mingw-w64 doesn't have one. Instead it links against `msvcrt.dll`,
~~which [isn't officially supported by Microsoft][msvcrt]. It just
happens to exist on modern Windows installations. Since it's not
supported,~~ it's way out of date and doesn't support much of C99. A lot
of these problems are patched over by the compiler, ~~but if you're
relying on Mingw-w64, you still have to stick to some C89 library
features, such as limiting yourself to the C89 printf specifiers~~.

~~Update: Mārtiņš Možeiko has pointed out `__USE_MINGW_ANSI_STDIO`, an
undocumented feature that fixes the printf family. I now use this by
default in all of my Mingw-w64 builds. It fixes most of the formatted
output issues, except that it's incompatible with the [`format` function
attribute][format].~~ (Update 2021: Mingw-w64 now does the right thing
out of the box.)

~~Another problem is that [position-independent code generation is
broken][pie], and so ASLR is not an option. This means binaries produced
by Mingw-w64 are less secure than they should be. There are also a
number of [subtle code generation bugs][gen] that might arise if you're
doing something unusual.~~ (Update 2021: Mingw-w64 makes PIE mandatory.)

### Visual C++

The behemoth usually considered in this situation is Visual Studio and
the Visual C++ build tools. I strongly prefer open source development
tools, and Visual Studio obviously the *least* open source option, but
at least it's cost-free these days. Now, I have absolutely no interest
in Visual Studio, but fortunately the Visual C++ compiler and
associated build tools can be used standalone, supporting both C and
C++.

Included is a "vcvars" batch file — vcvars64.bat for x64. Execute that
batch file in a cmd.exe console and the Visual C++ command line build
tools will be made available in that console and in any programs
executed from it (your editor). It includes the compiler (cl.exe),
linker (link.exe), assembler (ml64.exe), disassembler (dumpbin.exe),
and more. It also includes a [mostly POSIX-complete][make] make called
nmake.exe. All these tools are noisy and print a copyright banner on
every invocation, so get used to passing `-nologo` every time, which
suppresses some of it.

When I said behemoth, I meant it. In my experience it literally takes
*hours* (unattended) to install Visual Studio 2015. ~~The good news is you
don't actually need it all anymore. The build tools [are available
standalone][vcbt]. While it's still a larger and slower installation
process than it really should be, it's is much more reasonable to
install. It's good enough that I'd even say I'm comfortable relying on
it for Windows builds.~~ (Update: The build tools are unfortunately no
longer standalone.)

That being said, it's not without its flaws. Microsoft has never
announced any plans to support C99. They only care about C++, with C as
a second class citizen. Since C++11 incorporated most of C99 and
Microsoft supports C++11, Visual Studio 2015 supports most of C99. The
only things missing as far as I can tell are variable length arrays
(VLAs), complex numbers, and C99's array parameter declarators, since
none of these were adopted by C++. Some C99 features are considered
extensions (as they would be for C89), so you'll also get warnings about
them, which can be disabled.

The command line interface (option flags, intermediates, etc.) isn't
quite reconcilable with the unix-like ecosystem (i.e. GCC, Clang), so
**you'll need separate Makefiles**, or you'll need to use a build
system that generates Visual C++ Makefiles.

~~Debugging is a major problem.~~ (Update 2022: It's actually quite good
once [you know how to do it][db].) Visual C++ outputs separate .pdb
[program database][pdb] files, which aren't usable from GDB. Visual
Studio has a built-in debugger, though it's not included in the
standalone Visual C++ build tools. ~~I'm still searching for a decent
debugging solution for this scenario. I tried WinDbg, but I can't stand
it.~~ (Update 2022: [RemedyBG is amazing][rdb].)

In general the output code performance is on par with GCC and Clang,
so you're not really gaining or losing performance with Visual C++.

### Clang

Unsurprisingly, [Clang][clang] has been ported to Windows. It's like
Mingw-w64 in that you get the same features and interface across
platforms.

Unlike Mingw-w64, it doesn't link against msvcrt.dll. Instead **it
relies directly on the official Windows SDK**. You'll basically need
to install the Visual C++ build tools as if were going to build with
Visual C++. This means no practical cross-platform builds and you're
still relying on the proprietary Microsoft toolchain. In the past you
even had to use Microsoft's linker, but LLVM now provides its own.

It generates GDB-friendly DWARF debug information (in addition to
CodeView) so in theory **you can debug with GDB** again. I haven't
given this a thorough evaluation yet.

### Pelles C

Finally there's [Pelles C][pellesc]. It's cost-free but not open
source. It's a reasonable, small install that includes a full IDE with
an integrated debugger and command line tools. It has its own C
library and Win32 SDK with the most complete C11 support around. It
also supports OpenMP 3.1. All in all it's pretty nice and is something
I wouldn't be afraid to rely upon for Windows builds.

Like Visual C++, it has a couple of "povars" batch files to set up the
right environment, which includes a C compiler, linker, assembler,
etc. The compiler interface mostly mimics cl.exe, though there are far
fewer code generation options. The make program, pomake.exe, mimics
nmake.exe, but is even less POSIX-complete. The compiler's **output
code performance is also noticeably poorer than GCC, Clang, and Visual
C++**. It's definitely a less mature compiler.

It outputs CodeView debugging information, so **GDB is of no use**.
The best solution is to simply use the compiler built into the IDE,
which can be invoked directly from the command line. You don't
normally need to code from within the IDE just to use the debugger.

Like Visual C++, it's Windows only, so cross-compilation isn't really
in the picture.

If performance isn't of high importance, and you don't require
specific code generation options, then Pelles C is a nice choice for
Windows builds.

### Other Options

I'm sure there are a few other options out there, and I'd like to hear
about them so I can try them out. I focused on these since they're all
cost free and easy to download. If I have to register or pay, then
it's not going to beat these options.


[mingw64]: http://mingw-w64.org/doku.php
[utf8]: http://utf8everywhere.org/
[args]: https://utcc.utoronto.ca/~cks/space/blog/python/Python3UnicodeIssue
[msvcrt]: https://blogs.msdn.microsoft.com/oldnewthing/20140411-00/?p=1273
[format]: https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html#index-g_t_0040code_007bformat_007d-function-attribute-3318
[pie]: http://thelinuxjedi.blogspot.com/2014/07/tripping-up-using-mingw.html
[gen]: https://gcc.gnu.org/ml/gcc-bugs/2015-05/msg02025.html
[pellesc]: http://www.smorgasbordet.com/pellesc/
[vcbt]: http://landinghub.visualstudio.com/visual-cpp-build-tools
[clang]: http://clang.llvm.org/
[make]: /blog/2016/04/30/
[pdb]: https://en.wikipedia.org/wiki/Program_database
[w64devkit]: https://github.com/skeeto/w64devkit
[announce]: /blog/2020/05/15/
[everything]: /blog/2020/09/25/
[sane]: /blog/2021/12/30/
[rdb]: https://www.youtube.com/watch?v=r9eQth4Q5jg
[db]: /blog/2022/06/26/
