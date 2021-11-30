---
title: How to Write Portable C Without Complicating Your Build
layout: post
date: 2017-03-30T04:06:58Z
tags: [c, posix, win32]
uuid: e1651834-8033-3bfa-6eaf-00bc38a0584a
---

Suppose you're writing a non-GUI C application intended to run on a
number of operating systems: Linux, the various BSDs, macOS, [classical
unix][illumos], and perhaps even something as exotic as Windows. It might
sound like a rather complicated problem. These operating systems have
slightly different interfaces (or *very* different in one case), and they
run different variants of the standard unix tools — a problem for
portable builds.

With some up-front attention to detail, this is actually not terribly
difficult. **Unix-like systems are probably the least diverse and least
buggy they've ever been.** Writing portable code is really just a matter
of **coding to the standards** and ignoring extensions unless
*absolutely* necessary. Knowing what's standard and what's extension is
the tricky part, but I'll explain how to find this information.

You might be tempted to reach for an [overly complicated][ac] solution
such as GNU Autoconf. Sure, it creates a configure script with the
familiar, conventional interface. This has real value. But do you
*really* need to run a single-threaded gauntlet of hundreds of
feature/bug tests for things that sometimes worked incorrectly in some
weird unix variant back in the 1990s? On a machine with many cores
(parallel build, `-j`), this may very well be the slowest part of the
whole build process.

For example, the configure script for Emacs checks that the compiler
supplies `stdlib.h`, `string.h`, and `getenv` — things that were
standardized nearly 30 years ago. It also checks for a slew of POSIX
functions that have been standard since 2001.

There's a much easier solution: Document that the application requires,
say, C99 and POSIX.1-2001. It's the responsibility of the person
building the application to supply these implementations, so there's no
reason to waste time testing for it.

### How to code to the standards

Suppose there's some function you want to use, but you're not sure if
it's standard or an extension. Or maybe you don't know what standard it
comes from. Luckily the man pages document this stuff very well,
especially on Linux. Check the friendly "CONFORMING TO" section. For
example, look at [getenv(3)][getenv]. Here's what that section has to
say:

    CONFORMING TO
        getenv(): SVr4, POSIX.1-2001, 4.3BSD, C89, C99.

        secure_getenv() is a GNU extension.

This says this function comes from the original C standard. It's *always*
available on anything that claims to be a C implementation. The man page
also documents `secure_getenv()`, which is a GNU extension: to be avoided
in anything intended to be portable.

What about [sleep(3)][sleep]?

    CONFORMING TO
        POSIX.1-2001.

This function isn't part of standard C, but it's available on any system
claiming to implement POSIX.1-2001 (the POSIX standard from 2001). If the
program needs to run on an operating system not implementing this POSIX
standard (i.e. Windows), you'll need to call an alternative function,
probably inside a different `#if .. #endif` branch. More on this in a
moment.

If you're coding to POSIX, you [*must* define the `_POSIX_C_SOURCE`
feature test macro][source] to the standard you intend to use prior to
any system header includes:

> A POSIX-conforming application should ensure that the feature test
> macro `_POSIX_C_SOURCE` is defined before inclusion of any header.

For example, to properly access POSIX.1-2001 functions in your
application, define `_POSIX_C_SOURCE` to `200112L`. With this defined,
it's safe to assume access to all of C and everything from that standard
of POSIX. You can do this at the top of your sources, but I personally
like the tidiness of a global `config.h` that gets included before
everything.

### How to create a portable build

So you've written clean, portable C to the standards. How do you build
this application? The natural choice is `make`. It's available
everywhere and it's part of POSIX.

Again, the tricky part is teasing apart the standard from the extension.
I'm a long-time sinner in this regard, having far too often written
Makefiles that depend on GNU Make extensions. This is a real pain when
building programs on systems without the GNU utilities. I've been making
amends (and [finding][bug1] some [bugs][bug2] as a result).

No implementation makes the division clear in its documentation, and
especially don't bother looking at the GNU Make manual. Your best
resource is [the standard itself][make]. If you're already familiar with
`make`, coding to the standard is largely a matter of *unlearning* the
various extensions you know.

Outside of [some hacks][turing], this means you don't get conditionals
(`if`, `else`, etc.). With some practice, both with sticking to portable
code and writing portable Makefiles, you'll find that you *don't really
need them*. Following the macro conventions will cover most situations.
For example:

* `CC`: the C compiler program
* `CFLAGS`: flags to pass to the C compiler
* `LDFLAGS`: flags to pass to the linker (via the C compiler)
* `LDLIBS`: libraries to pass to the linker

You don't need to do anything weird with the assignments. The user
invoking `make` can override them easily. For example, here's part of a
Makefile:

    CC     = c99
    CFLAGS = -Wall -Wextra -Os

But the user wants to use `clang`, and their system needs to explicitly
link `-lsocket` (e.g. Solaris). The user can override the macro
definitions on the command line:

    $ make CC=clang LDLIBS=-lsocket

The same rules apply to the programs you invoke from the Makefile. Read
the standards documents and ignore your system's man pages as to avoid
accidentally using an extension. It's especially valuable to learn [the
Bourne shell language][shell] and avoid any accidental bashisms in your
Makefiles and scripts. The `dash` shell is good for testing your scripts.

Makefiles conforming to the standard will, unfortunately, be more verbose
than those taking advantage of a particular implementation. If you know
how to code Bourne shell — which is not terribly difficult to learn —
then you might even consider hand-writing a `configure` script to
generate the Makefile (a la metaprogramming). This gives you a more
flexible language with conditionals, and, being generated, redundancy in
the Makefile no longer matters.

As someone who frequently dabbles with BSD systems, my life has gotten a
lot easier since learning to write portable Makefiles and scripts.

### But what about Windows

It's the elephant in the room and I've avoided talking about it so far.
If you want to [build with Visual Studio's command line tools][vs] —
something I do on occasion — build portability goes out the window.
Visual Studio has `nmake.exe`, which nearly conforms to POSIX `make`.
However, without the standard unix utilities and with the completely
foreign compiler interface for `cl.exe`, there's absolutely no hope of
writing a Makefile portable to this situation.

The nice alternative is MinGW(-w64) with MSYS or Cygwin supplying the
unix utilities, though it has [the problem][ms] of linking against
`msvcrt.dll`. Another option is a separate Makefile dedicated to
`nmake.exe` and the Visual Studio toolchain. Good luck defining a
correctly working "clean" target with `del.exe`.

My preferred approach lately is an amalgamation build (as seen in
[Enchive][enchive]): Carefully concatenate all the application's sources
into one giant source file. First concatenate all the headers in the
right order, followed by all the C files. Use `sed` to remove and local
includes. You can do this all on a unix system with the nice utilities,
then point `cl.exe` at the amalgamation for the Visual Studio build.
It's not very useful for actual development (i.e. you don't want to edit
the amalgamation), but that's what MinGW-w64 resolves.

What about all those POSIX functions? You'll need to find Win32
replacements on MSDN. I prefer to do this is by abstracting those
operating system calls. For example, compare POSIX `sleep(3)` and [Win32
`Sleep()`][msdn].

~~~c
#if defined(_WIN32)
#include <windows.h>

void
my_sleep(int s)
{
    Sleep(s * 1000);  // TODO: handle overflow, maybe
}

#else /* __unix__ */
#include <unistd.h>

void
my_sleep(int s)
{
    sleep(s);  // TODO: fix signal interruption
}
#endif
~~~

Then the rest of the program calls `my_sleep()`. There's another example
in [the OpenMP article][openmp] with `pwrite(2)` and `WriteFile()`. This
demonstrates that supporting a bunch of different unix-like systems is
really easy, but introducing Windows portability adds a disproportionate
amount of complexity.

#### Caveat: paths and filenames

There's one major complication with filenames for applications portable
to Windows. In the unix world, filenames are null-terminated bytestrings.
Typically these are Unicode strings encoded as UTF-8, but it's not
necessarily so. The kernel just sees bytestrings. A bytestring doesn't
necessarily have a formal Unicode representation, which can be a problem
for [languages that want filenames to be Unicode strings][pep]
([also][emacs]).

On Windows, filenames are somewhere between UCS-2 and UTF-16, but end up
being neither. They're really null-terminated unsigned 16-bit integer
arrays. It's *almost* UTF-16 except that Windows allows unpaired
surrogates. This means Windows filenames *also* don't have a formal
Unicode representation, but in a completely different way than unix. Some
[heroic efforts have gone into working around this issue][wtf].

As a result, it's highly non-trivial to correctly support all possible
filenames on both systems in the same program, *especially* when they're
passed as command line arguments.

### Summary

The key points are:

1. Document the standards your application requires and strictly stick
   to them.
2. Ignore the vendor documentation if it doesn't clearly delineate
   extensions.

This was all a discussion of non-GUI applications, and I didn't really
touch on libraries. Many libraries are simple to access in the build
(just add it to `LDLIBS`), but some libraries — GUIs in particular — are
particularly complicated to manage portably and will require a more
complex solution (pkg-config, CMake, Autoconf, etc.).


[openmp]: /blog/2017/03/01/
[msdn]: https://msdn.microsoft.com/en-us/library/windows/desktop/ms686298(v=vs.85).aspx
[enchive]: https://github.com/skeeto/enchive
[ms]: https://blogs.msdn.microsoft.com/oldnewthing/20140411-00/?p=1273
[vs]: /blog/2016/06/13/
[shell]: http://pubs.opengroup.org/onlinepubs/009695399/utilities/xcu_chap02.html
[turing]: /blog/2016/04/30/
[make]: http://pubs.opengroup.org/onlinepubs/009695399/utilities/make.html
[source]: http://pubs.opengroup.org/onlinepubs/007904975/functions/xsh_chap02_02.html
[sleep]: https://manpages.debian.org/jessie/manpages-dev/sleep.3.en.html
[getenv]: https://manpages.debian.org/jessie/manpages-dev/getenv.3.en.html
[illumos]: https://en.wikipedia.org/wiki/Illumos
[bug1]: https://marc.info/?l=openbsd-bugs&m=148815538325392&w=2
[bug2]: https://marc.info/?l=openbsd-bugs&m=148734102504016&w=2
[pep]: https://www.python.org/dev/peps/pep-0383/
[wtf]: https://simonsapin.github.io/wtf-8/
[emacs]: http://www.gnu.org/software/emacs/manual/html_node/elisp/Text-Representations.html
[ac]: https://undeadly.org/cgi?action=article;sid=20170930133438
