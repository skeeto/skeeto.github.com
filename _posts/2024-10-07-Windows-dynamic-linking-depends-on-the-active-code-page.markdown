---
title: Windows dynamic linking depends on the active code page
layout: post
date: 2024-10-07T19:50:17Z
tags: [c, win32]
uuid: cc7861a5-aaa0-4a27-8867-9f48cf72e444
---

Windows paths have been [WTF-16][]-encoded for decades, but module names
in the [import tables][peports] of [Portable Executable][pe] are octets.
If a name contains values beyond ASCII — technically out of spec — then
the dynamic linker must somehow decode those octets into Unicode in order
to construct a lookup path. There are multiple ways this could be done,
and the most obvious is the process's active code page (ACP), which is
exactly what happens. As a consequence, the specific DLL loaded by the
linker may depend on the system code page. In this article I'll contrive
such a situation.

[LoadLibraryA][] is a similar situation, and potentially applies the code
page to a longer portion of the module path. [LoadLibraryW][] is
unaffected, at least for the directly-named module, because it's Unicode
all the way through.

For my contrived demonstration I came up with two names that to
English-reading eyes appears as two words with extraneous markings:

* `Ãµral.dll`: CP-1252=`"C3 B5 …"`
* `õral.dll`: CP-1252=`"F5 …"`; UTF-8=`"C3 B5 …"`

Both end with `ral.dll`. I've included the [CP-1252][] encoding for the
differing prefixes, and the UTF-8 encoding for the second. I'm using
CP-1252 because it's the most common system code page in the world,
especially the Western hemisphere. Due to case insensitivity, the actual
DLL may be named `ãµral.dll` — i.e. to match the second library case — but
the module name *must* be encoded as uppercase when [building the import
library][dll]. Alternatively the second could be `Õral.dll`, particularly
because I won't use it when constructing an import library.

The plan is to store the octets `C3 B5 …` in the import table. A process
using CP-1252 decodes it to `Ãµral.dll`. In the UTF-8 code page it decodes
to `õral.dll`. For testing we can use an [application manifest][sane] to
control the code page for a particular PE image — a lot easier than
changing the system code page. Otherwise, this trick could dynamically
change the behavior of a program in response to the system code page
without actually inspecting the active code page.

The libraries will have a single function `get`, which returns a string
indicating which library was loaded:

```c
#define X(s) #s
#define S(s) X(s)
__declspec(dllexport) char *get(void) { return S(V); }
```

Constructing the import library can be tricky because you must consider
how the toolchain, editors, and shells decode and encode text, which may
involve the build system's code page. It's shockingly difficult to script!
Binutils `dlltool` cannot process these names and cannot be used at all.
With bleeding edge [w64devkit][] I could reliably construct the DLLs and
import library like so, even in a script (Windows 10 and later only):

    $ gcc -shared -DV=UTF-8 -o Õral.dll  detect.c
    $ gcc -shared -DV=ANSI  -o Ãµral.dll detect.c -Wl,--out-implib=detect.lib

That produces two DLLs and one import library, `detect.lib`, with the
desired module name octets. A straightforward MSVC `cl` invocation also
works so long as it's not from a batch file. It will quite correctly warn
about the strange name situation, which I like. My test program, `main.c`:

```c
#include <stdio.h>

char *get(void);
int main(void) { puts(get()); }
```

I link `detect.lib` when I build it:

    $ cc -o main.exe main.c detect.lib

I designed [`peports`][peports] to print non-ASCII octets unambiguously
(`\xXX`), and it's the only tool I know that does so:

    $ peports main.exe | tail -n 2
    \xc3\xb5ral.dll
            1       get

The module name has the `C3 B5 …` prefix octets. When I run it under my
system code page, CP-1252:

    $ ./main
    ANSI

If I [add a UTF-8 manifest][sane], even just a "side-by-side" manifest, it
loads the other library despite an identical import table:

    $ cc -o main.exe main.c detect.lib libwinsane.o
    $ ./main
    UTF-8

Again, without the manifest, if I switched my system code page to UTF-8
then `UTF-8` would still be the result.

I can't think of much practical use for this trick outside of malware. In
a real program it would be simpler to inspect code page, and there's no
benefit to avoiding such a check if it's needed. Malware could use it to
trick inspection tools and scanners that decode module names differently
than the dynamic linker. Such tools often incorrectly assume UTF-8, which
is what motivated this article.


[CP-1252]: https://en.wikipedia.org/wiki/Windows-1252
[LoadLibraryA]:https://learn.microsoft.com/en-us/windows/win32/api/libloaderapi/nf-libloaderapi-loadlibrarya
[LoadLibraryW]: https://learn.microsoft.com/en-us/windows/win32/api/libloaderapi/nf-libloaderapi-loadlibraryw
[WTF-16]: https://simonsapin.github.io/wtf-8/#ill-formed-utf-16
[dll]: /blog/2021/05/31/
[pe]: https://learn.microsoft.com/en-us/windows/win32/debug/pe-format
[peports]: /blog/2024/06/30/
[sane]: /blog/2021/12/30/
[w64devkit]: https://github.com/skeeto/w64devkit
