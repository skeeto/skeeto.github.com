---
title: Calling the Native API While Freestanding
layout: post
date: 2016-02-28T23:47:22Z
tags: [c, win32, x86]
uuid: 3649a761-d3dc-391b-7f24-a28398100102
---

When developing [minimal, freestanding Windows programs][prev], it's
obviously beneficial to take full advantage of dynamic libraries that
are already linked rather than duplicate that functionality in the
application itself. Every Windows process automatically, and
involuntarily, has kernel32.dll and ntdll.dll loaded into its process
space before it starts. As discussed previously, kernel32.dll provides
the Windows API (Win32). The other, ntdll.dll, provides the *Native
API* for user space applications, and is the focus of this article.

The Native API is a low-level API, a foundation for the implementation
of the Windows API and various components that don't use the Windows
API (drivers, etc.). It includes a runtime library (RTL) suitable for
replacing important parts of the C standard library, unavailable to
freestanding programs. Very useful for a minimal program.

Unfortunately, *using* the Native API is a bit of a minefield. Not all
of the documented Native API functions are actually exported by
ntdll.dll, making them inaccessible both for linking and
GetProcAddress(). Some are exported, but not documented as such.
Others are documented as exported but are not documented *when* (which
release of Windows). If a particular function wasn't exported until
Windows 8, I don't want to use when supporting Windows 7.

This is further complicated by the Microsoft Windows SDK, where many
of these functions are just macros that alias C runtime functions.
Naturally, MinGW closely follows suit. For example, in both cases,
here is how the Native API function `RtlCopyMemory` is "declared."

~~~c
#define RtlCopyMemory(dest,src,n) memcpy((dest),(src),(n))
~~~

This is certainly not useful for freestanding programs, though it has
a significant benefit for *hosted* programs: The C compiler knows the
semantics of `memcpy()` and can properly optimize around it. Any C
compiler worth its salt will replace a small or aligned, fixed-sized
`memcpy()` or `memmove()` with the equivalent inlined code. For
example:

~~~c
    char buffer0[16];
    char buffer1[16];
    // ...
    memcpy(buffer0, buffer1, 16);
    // ...
~~~

On x86\_64 (GCC 4.9.3, -Os), this `memmove()` call is replaced with
two instructions. This isn't possible when calling an opaque function
in a non-standard dynamic library. The side effects could be anything.

~~~nasm
    movaps  xmm0, [rsp + 48]
    movaps  [rsp + 32], xmm0
~~~

These Native API macro aliases are what have allowed certain Wine
issues [to slip by unnoticed for years][wine]. Very few user space
applications actually call Native API functions, even when addressed
directly by name in the source. The development suite is pulling a
bait and switch.

Like [last time I danced at the edge of the compiler][dance], this has
caused headaches in my recent experimentation with freestanding
executables. The MinGW headers assume that the programs including them
will link against a C runtime. Dirty hack warning: To work around it,
I have to undo the definition in the MinGW headers and make my own.
For example, to use the real `RtlMoveMemory()`:

~~~c
#include <windows.h>

#undef RtlMoveMemory
__declspec(dllimport)
void RtlMoveMemory(void *, const void *, size_t);
~~~

Anywhere where I might have previously used `memmove()` I can instead
use `RtlMoveMemory()`. Or I could trivially supply my own wrapper:

~~~c
void *
memmove(void *d, const void *s, size_t n)
{
    RtlMoveMemory(d, s, n);
    return d;
}
~~~

As of this writing, the same approach is not reliable with
`RtlCopyMemory()`, the cousin to `memcpy()`. As far as I can tell, it
was only exported starting in Windows 7 SP1 and Wine 1.7.46 (June
2015). Use `RtlMoveMemory()` instead. The overlap-handling overhead is
negligible compared to the function call overhead anyway.

As a side note: one reason besides minimalism for not implementing
your own `memmove()` is that it can't be implemented efficiently in a
conforming C program. According to the language specification, your
implementation of `memmove()` would not be permitted to compare its
pointer arguments with `<`, `>`, `<=`, or `>=`. That would lead to
undefined behavior when pointing to unrelated objects (ISO/IEC
9899:2011 §6.5.8¶5). The simplest legal approach is to allocate a
temporary buffer, copy the source buffer into it, then copy it into
the destination buffer. However, buffer allocation may fail — i.e.
NULL return from `malloc()` — introducing a failure case to
`memmove()`, which isn't supposed to fail.

Update July 2016: Alex Elsayed pointed out a solution to the
`memmove()` problem in the comments. In short: iterate over the
buffers bytewise (`char *`) using equality (`==`) tests to check for
an overlap. In theory, a compiler could optimize away the loop and
make it efficient.

I keep mentioning Wine because I've been careful to ensure my
applications run correctly with it. So far it's worked *perfectly*
with both Windows API and Native API functions. Thanks to the hard
work behind the Wine project, despite being written sharply against
the Windows API, these tiny programs remain relatively portable (x86
and ARM). It's a good fit for graphical applications (games), but I
would *never* write a command line application like this. The command
line has always been a second class citizen on Windows.

Mostly for my own future reference, here are export lists for two
different versions of kernel32.dll and ntdll.dll:

* [7sp1.kernel32.txt.gz](/download/exports/7sp1.kernel32.txt.gz)
* [7sp1.ntdll.txt.gz](/download/exports/7sp1.ntdll.txt.gz)
* [xpsp3.kernel32.txt.gz](/download/exports/xpsp3.kernel32.txt.gz)
* [xpsp3.ntdll.txt.gz](/download/exports/xpsp3.ntdll.txt.gz)

As I collect more of these export lists, I'll be able to paint a full
picture of when particular functions first appeared as exports. These
lists were generated with `objdump -p <path_to_dll>`.

Now that I've got these Native API issues sorted out, I've
significantly expanded the capabilities of my tiny, freestanding
programs without adding anything to their size. Functions like
`RtlUnicodeToUTF8N()` and `RtlUTF8ToUnicodeN()` will surely be handy.


[prev]: /blog/2016/01/31/
[wine]: https://bugs.winehq.org/show_bug.cgi?id=38783
[dance]: /blog/2014/12/09/
