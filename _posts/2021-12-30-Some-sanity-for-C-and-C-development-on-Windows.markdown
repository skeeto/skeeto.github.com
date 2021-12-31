---
title: Some sanity for C and C++ development on Windows
layout: post
date: 2021-12-30T23:25:53Z
tags: [c, cpp, win32]
uuid: 2e417030-915f-4897-99ff-2a0dafd0ac89
---

A hard reality of C and C++ software development on Windows is that there
has never been a good, native C or C++ standard library implementation for
the platform. A standard library should abstract over the underlying host
facilities in order to ease portable software development. On Windows, C
and C++ is so poorly hooked up to operating system interfaces that most
portable or mostly-portable software — programs which work perfectly
elsewhere — are subtly broken on Windows, particularly outside of the
English-speaking world. The reasons are almost certainly political,
originally motivated by vendor lock-in, than technical, which adds insult
to injury. This article is about what's wrong, how it's wrong, and some
easy techniques to deal with it in portable software.

There are [multiple C implementations][four], so how could they all be
bad, even the [early ones][blast]? Microsoft's C runtime has defined how
the standard library should work on the platform, and everyone else
followed along for the sake of compatibility. I'm excluding [Cygwin][] and
its major fork, [MSYS2][], despite not inheriting any of these flaws. They
change so much that they're effectively whole new platforms, not truly
"native" to Windows.

In practice, C++ standard libraries are implemented on top of a C standard
library, which is why C++ shares the same problems. CPython dodges these
issues: Though written in C, on Windows it bypasses the broken C standard
library and directly calls the proprietary interfaces. Other language
implementations, such "gc" Go, simply aren't built on C at all, and
instead do things correctly in the first place — the behaviors the C
runtimes should have had all along.

If you're just working on one large project, bypassing the C runtime isn't
such a big deal, and you're likely already doing so to access important
platform functionality. You don't really even need a C runtime. However,
if you write many small programs, [as I do][scratch], writing the same
special Windows support for each one ends up being most of the work, and
honestly makes properly supporting Windows not worth the trouble. I end up
just accepting the broken defaults most of the time.

Before diving into the details, if you're looking for a quick-and-easy
solution for the Mingw-w64 toolchain, [including w64devkit][w64], which
magically makes your C and C++ console programs behave well on Windows,
I've put together a "library" named **[libwinsane][]**. It solves all
problems discussed in this article, except for one. No source changes
required, simply link it into your program.

### What exactly is broken?

The Windows API comes in two flavors: narrow with an "A" ("ANSI") suffix,
and wide (Unicode, UTF-16) with a "W" suffix. The former is the legacy
API, where an active *code page* maps 256 bytes onto (up to) 256 specific
characters. On typical machines configured for European languages, this
means [code page 1252][cp1252]. [Roughly speaking][wtf8], Windows
internally uses UTF-16, and calls through the narrow interface use the
active code page to translate the narrow strings to wide strings. The
result is that calls through the narrow API have limited access to the
system.

The UTF-8 encoding was invented in 1992 and standardized by January 1993.
UTF-8 was adopted by the unix world over the following years due to [its
backwards-compatibility][utf8] with its existing interfaces. Programs
could read and write Unicode data, access Unicode paths, pass Unicode
arguments, and get and set Unicode environment variables without needing
to change anything. Today UTF-8 has become the dominant text encoding
format in the world, in large part due to the world wide web.

In July 1993, Microsoft introduced the wide Windows API with the release
of Windows NT 3.1, placing all their bets on UCS-2 (later UTF-16) rather
than UTF-8. This turned out to be a mistake, since [UTF-16 is inferior to
UTF-8 in practically every way][ew], though admittedly some problems
weren't so obvious at the time.

The major problem: **The C and C++ standard libraries only hook up to the
narrow Windows interfaces**. The standard library, and therefore typical
portable software on Windows, cannot handle anything but ASCII. The
effective result is that these programs:

* Cannot accept non-ASCII arguments
* Cannot get/set non-ASCII environment variables
* Cannot access non-ASCII paths
* Cannot read and write non-ASCII on a console

Doing any of these requires calling proprietary functions, treating
Windows as a special target. It's part of what makes correctly porting
software to Windows so painful.

The sensible solution would have been for the C runtime to speak UTF-8 and
connect to the wide API. Alternatively, the narrow API could have been
changed over to UTF-8, phasing out the old code page concept. In theory
this is what the UTF-8 "code page" is about, though it doesn't always
work. There would have been compatibility problems with abruptly making
such a change, but until very recently, *this wasn't even an option*. Why
couldn't there be a switch I could flip to get sane behavior that works
like every other platform?

### How to mostly fix Unicode support

In 2019, Microsoft introduced a feature to allow programs to [request
UTF-8 as their active code page on start][xml], along with supporting
UTF-8 on more narrow API functions. This is like the magic switch I
wanted, except that it involves embedding some ugly XML into your binary
in a particular way. At least it's now an option.

For Mingw-w64, that means writing a resource file like so:

    #include <winuser.h>
    CREATEPROCESS_MANIFEST_RESOURCE_ID RT_MANIFEST "utf8.xml"

Compiling it with `windres`:

    $ windres -o manifest.o manifest.rc

Then linking that into your program. Amazingly it mostly works! Programs
can access Unicode arguments, Unicode environment variables, and Unicode
paths, including with `fopen`, just as it's worked on other platforms for
decades. Since the active code page is set at load time, it happens before
`argv` is constructed (from `GetCommandLineA`), which is why that works
out.

Alternatively you could create a "side-by-side assembly" placing that XML
in a file with the same name as your EXE but with `.manifest` suffix
(after the `.exe` suffix), then placing that next to your EXE. Just be
mindful that there's a "side-by-side" cache (WinSxS), and so it might not
immediately pick up your changes.

What *doesn't* work is console input and output since the console is
external to the process, and so isn't covered by the process's active code
page. It must be configured separately using a proprietary call:

```c
SetConsoleOutputCP(CP_UTF8);
```

Annoying, but at least it's not *that* painful. This only covers output,
though, meaning programs can only print UTF-8. Unfortunately [UTF-8 input
still doesn't work][broken], and setting the input code page doesn't do
anything despite reporting success:

```c
SetConsoleCP(CP_UTF8);  // doesn't work
```

If you care about reading interactive Unicode input, you're [stuck
bypassing the C runtime][pw] since it's still broken.

### Text stream translation

Another long-standing issue is that C and C++ on Windows has distinct
"text" and "binary" streams, which it inherited from DOS. Mainly this
means automatic newline conversion between CRLF and LF. The C standard
explicitly allows for this, though unix-like platforms have never actually
distinguished between text and binary streams.

The standard also specifies that standard input, output, and error are all
open as text streams, and there's no portable method to change the stream
mode to binary — a serious deficiency with the standard. On unix-likes
this doesn't matter, but on Windows it means programs can't read or write
binary data on standard streams without calling a non-standard function.
It also means reading and writing standard streams is slow, [frequently a
bottleneck][csv] unless I route around it.

Personally, I like [writing binary data to standard output][ppm],
[including video][ai], and sometimes [binary filters][bin] that also read
binary input. I do it so often that in probably half my C programs I have
this snippet in `main` just so they work correctly on Windows:

```c
    #ifdef _WIN32
    int _setmode(int, int);
    _setmode(0, 0x8000);
    _setmode(1, 0x8000);
    #endif
```

That incantation sets standard input and output in the C runtime to binary
mode without the need to include a header, making it compact, simple, and
self-contained.

This built-in newline translation, along with the Windows standard text
editor, Notepad, [lagging decades behind][crlf], meant that many other
programs, including Git, grew their own, annoying, newline conversion
[misfeatures][git1] that cause [other problems][git2].

### libwinsane

I introduced libwinsane at the beginning of the article, which fixes all
this simply by being linked into a program. It includes the magic XML
manifest `.rsrc` section, configures the console for UTF-8 output, and
sets standard streams to binary before `main` (via a GCC constructor). I
called it a "library", but it's actually a single object file. It can't be
a static library since it must be linked into the program despite not
actually being referenced by the program.

So normally this program:

```c
#include <stdio.h>
#include <string.h>

int main(int argc, char **argv)
{
    char *arg = argv[argc-1];
    size_t len = strlen(arg);
    printf("%zu %s\n", len, arg);
}
```

Compiled and run:

    C:\>cc -o example example.c
    C:\>example π
    1 p

As usual, the Unicode argument is silently mangled into one byte. Linked
with libwinsane, it just works like everywhere else:

    C:\>gcc -o example example.c libwinsane.o
    C:\>example π
    2 π

If you're maintaining a substantial program, you probably want to copy and
integrate the necessary parts of libwinsane into your project and build,
rather than always link against this loose object file. This is more for
convenience and for succinctly capturing the concept. You may even want to
[enable ANSI escape processing][ansi] in your version.


[Cygwin]: https://www.cygwin.com/
[MSYS2]: https://www.msys2.org/
[ai]: /blog/2020/11/24/
[ansi]: https://github.com/skeeto/hastyhex/blob/master/hastyhex.c#L220
[bin]: /blog/2017/07/02/
[blast]: /blog/2018/04/13/
[broken]: https://github.com/microsoft/terminal/issues/4551#issuecomment-585487802
[cp1252]: https://en.wikipedia.org/wiki/Windows-1252
[crlf]: https://devblogs.microsoft.com/commandline/extended-eol-in-notepad/
[csv]: /blog/2021/12/04/
[ew]: http://utf8everywhere.org/
[four]: /blog/2016/06/13/
[git1]: https://github.com/skeeto/w64devkit/issues/10
[git2]: https://github.com/skeeto/binitools/commit/2efd690c3983856c9633b0be66d57483491d1e10
[libwinsane]: https://github.com/skeeto/scratch/tree/master/libwinsane
[ppm]: /blog/2020/06/29/
[pw]: /blog/2020/05/04/
[scratch]: https://github.com/skeeto/scratch
[utf8]: /blog/2017/10/06/#what-is-utf-8
[w64]: /blog/2020/05/15/
[wtf8]: http://simonsapin.github.io/wtf-8/
[xml]: https://docs.microsoft.com/en-us/windows/apps/design/globalizing/use-utf8-code-page
