---
title: 'Hand-written Windows API prototypes: fast, flexible, and tedious'
layout: post
date: 2023-05-31T01:38:31Z
tags: [win32, c, cpp]
uuid: 35b44114-7ad2-422b-9eaf-dc37e7eaaf97
---

I love fast builds, and for years I've been bothered by the build penalty
for translation units including `windows.h`. This header has an enormous
number of definitions and declarations and so, for C programs, it tends to
dominate the build time of those translation units. Most programs,
especially systems software, only needs a tiny portion of it. For example,
when compiling [u-config][] with GCC, two thirds of the debug build was
spent processing `windows.h` just for [4 types, 16 definitions, and 16
prototypes][mini].

To give a sense of the numbers, here's `empty.c`, which does nothing but
include `windows.h`.

```c
#include <windows.h>
```

With the current Mingw-w64 headers, that's ~82kLOC (non-blank):

    $ gcc -E empty.c | grep -vc '^$'
    82041

With [w64devkit][] this takes my system ~450ms to compile with GCC:

    $ time gcc -c empty.c
    real    0m 0.45s
    user    0m 0.00s
    sys     0m 0.00s

Compiling an actually empty source file takes ~10ms, so it really is
spending practically all that time processing headers. MSVC is a faster
compiler, and this extends to processing an even larger `windows.h` that
crosses over 100kLOC (VS2022). It clocks in at 120ms on the same system:

    $ cl /nologo /E empty.c | grep -vc '^$'
    empty.c
    100944
    $ time cl /nologo /c empty.c
    empty.c
    real    0m 0.12s
    user    0m 0.09s
    sys     0m 0.01s

That's just low enough to be tolerable, but I'd like the situation with
GCC to be better. Defining `WIN32_LEAN_AND_MEAN` reduces the number of
included headers, which has a significant effect:

    $ gcc -E -DWIN32_LEAN_AND_MEAN empty.c | grep -vc '^$'
    55025
    $ time gcc -c -DWIN32_LEAN_AND_MEAN empty.c
    real    0m 0.30s
    user    0m 0.00s
    sys     0m 0.00s

    $ cl /nologo /E /DWIN32_LEAN_AND_MEAN empty.c | grep -vc '^$'
    empty.c
    41436
    $ time cl /nologo /c /DWIN32_LEAN_AND_MEAN empty.c
    empty.c
    real    0m 0.07s
    user    0m 0.01s
    sys     0m 0.01s

### Precompiled headers

The official solution is precompiled headers. Put all the system header
includes, [or similar][sdl], into a dedicated header, then compile that
header into a special format. For example, `headers.h`:

```c
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
```

Then `main.c` includes `windows.h` through this header:

```c
#include "headers.h"

int mainCRTStartup(void)
{
    return 0;
}
```

If I ask [GCC to compile `headers.h`][hdr]:

    $ gcc headers.h

It produces `headers.h.gch`. When a source includes `headers.h`, GCC first
searches for an appropriate `.gch`. Not only must the name match, but so
must all the definitions at the moment of inclusion: `headers.h` should
always be the first included header, otherwise it may not work. Now when I
compile `main.c`:

    $ time gcc -c main.c
    real    0m 0.04s
    user    0m 0.00s
    sys     0m 0.00s

Much better! MSVC has a conventional name for this header recognizable to
every Visual Studio user: `stdafx.h`. It works a bit differently, and I've
never used it myself, but I trust it has similar results.

Precompiled headers requires some extra steps that vary by toolchain. Can
we do better? That depends on your definition of "better!"

### Artisan, handcrafted prototypes

As mentioned, systems software tends to need only a few declarations:
open, read, write, stat, etc. What if I wrote these out manually? A bit
tedious, but it doesn't require special precompiled header handling. It
also creates some new possibilities. To illustrate, a [CRT-free][crt]
"hello world" program:

```c
#include <windows.h>

int mainCRTStartup(void)
{
    HANDLE stdout = GetStdHandle(STD_OUTPUT_HANDLE);
    char message[] = "Hello, world!\n";
    DWORD len;
    return !WriteFile(stdout, message, sizeof(message)-1, &len, 0);
}
```

This takes my system half a second to compile — quite long to produce just
26 assembly instructions:

    $ time cc -nostartfiles -o hello.exe hello.c
    real    0m 0.50s
    user    0m 0.00s
    sys     0m 0.00s
    $ ./hello.exe
    Hello, world!

The program requires prototypes only for GetStdHandle and WriteFile, a
definition for `STD_OUTPUT_HANDLE`, and some typedefs. Starting with the
easy stuff, the definition and [types look like this][types]:

```c
#define STD_OUTPUT_HANDLE ((DWORD)-11)

typedef int BOOL;
typedef void *HANDLE;
typedef unsigned long DWORD;
```

By the way, here's a cheat code for quickly finding preprocessor
definitions, faster than looking them up elsewhere:

    $ echo '#include <windows.h>' | gcc -E -dM - | grep 'STD_\w*_HANDLE'
    #define STD_INPUT_HANDLE ((DWORD)-10)
    #define STD_ERROR_HANDLE ((DWORD)-12)
    #define STD_OUTPUT_HANDLE ((DWORD)-11)

Did you catch the pattern? It's `-10 - fd`, where `fd` is the conventional
unix file descriptor number: a kind of mnemonic.

Prototypes are a little trickier, especially if you care about 32-bit. The
Windows API uses the "stdcall" calling convention, which is distinct from
the "cdecl" calling convention on x86, though the same on x64. Of course,
you must already be aware of this merely using the API, as your own
callbacks must usually be stdcall themselves. Further, API functions are
[DLL imports][dll] and should be declared as such. Putting it together,
here's GetStdHandle:

```c
__declspec(dllimport)
HANDLE __stdcall GetStdHandle(DWORD);
```

This works with both Mingw-w64 and MSVC. MSVC requires `__stdcall` between
the return type and function name, so don't get clever about it. If you
only care about GCC then you can declare both using attributes, which I
think is a bit nicer:

```c
HANDLE GetStdHandle(DWORD)
    __attribute__((dllimport,stdcall));
```

The prototype for [WriteFile][writefile]:

```c
__declspec(dllimport)
BOOL __stdcall WriteFile(HANDLE, const void *, DWORD, DWORD *, void *);
```

You may have noticed I'm taking some shortcuts. The "official" definition
uses an ugly pointer typedef, `LPCVOID`, instead of pointer syntax, but I
skipped that type definition. I also replaced the last argument, an
`OVERLAPPED` pointer, with a generic pointer. I only need to pass null. I
can keep sanding it down to something more ergonomic:

```c
__declspec(dllimport)
int __stdcall WriteFile(void *, void *, int, int *, void *);
```

That's how I typically write these prototypes. I dropped the `const`
because it doesn't help me. I used signed sizes because I like them better
and it's [what I'm usually holding][buf] at the call site. But doesn't
changing the signedness potentially break compatibility? It makes no
difference to any practical ABI: It's passed the same way. In general,
signedness is a matter for *operators*, and only some of them — mainly
comparisons (`<`, `>`, etc.) and division. It's a similar story for
pointers starting with the 32-bit era, so I can choose whatever pointer
types are convenient.

In general, I can do anything I want so long as I know my compiler will
produce an appropriate function call. These are not standard functions,
like `printf` or `memcpy`, which are implemented in part by the compiler
itself, but foreign functions. It's no different than teaching [an
FFI][ffi] how to make a call. This is also, in essence, how OpenGL and
Vulkan work, with applications [defining the API for themselves][gl].

Considering all this, my new hello world:

```c
__declspec(dllimport)
int __stdcall WriteFile(void *, void *, int, int *, void *);
__declspec(dllimport)
void *__stdcall GetStdHandle(int);

int mainCRTStartup(void)
{
    void *stdout = GetStdHandle(-10 - 1);
    char message[] = "Hello, world!\n";
    int len;
    return !WriteFile(stdout, message, sizeof(message)-1, &len, 0);
}
```

You know, there's a kind of beauty to a program that requires no external
definitions. It builds quickly and produces a binary bit-for-bit identical
to the original:

    $ time cc -nostartfiles -o hello.exe main.c
    real    0m 0.04s
    user    0m 0.00s
    sys     0m 0.00s

    $ time cl /nologo hello.c /link /subsystem:console kernel32.lib
    hello.c
    real    0m 0.03s
    user    0m 0.00s
    sys     0m 0.00s

I've also been using this to patch over API rough edges. For example,
[WSARecvFrom][] takes [WSAOVERLAPPED][], but [GetQueuedCompletionStatus][]
takes [OVERLAPPED][]. These types are explicitly compatible, and only
defined separately for annoying technical reasons. I must use the same
overlapped object with both APIs at once, meaning I would normally need
ugly pointer casts on my Winsock calls, or vice versa with I/O completion
ports. But because I'm writing all these definitions myself, I can define
a common overlapped structure for both!

Perhaps you're worried that this would be too fragile. Well, as a legacy
software aficionado, I enjoy [building and running my programs on old
platforms][blast]. So far these programs still work properly [going back
30 years][ww] to Windows NT 3.5 and Visual C++ 4.2. When I do hit a snag,
it's always been a bug (now long fixed) in the old operating system, not
in my programs or these prototypes. So, in effect, this technique has
worked well for the past 30 years!

Writing out these definitions is a bit of a chore, but after paying that
price I've been quite happy with the results. I will likely continue doing
it in the future, at least for non-graphical applications.


[GetQueuedCompletionStatus]: https://learn.microsoft.com/en-us/windows/win32/api/ioapiset/nf-ioapiset-getqueuedcompletionstatus
[OVERLAPPED]: https://learn.microsoft.com/en-us/windows/win32/api/minwinbase/ns-minwinbase-overlapped
[WSAOVERLAPPED]: https://learn.microsoft.com/en-us/windows/win32/api/winsock2/ns-winsock2-wsaoverlapped
[WSARecvFrom]: https://learn.microsoft.com/en-us/windows/win32/api/ioapiset/nf-ioapiset-getqueuedcompletionstatus
[WSARecvFrom]: https://learn.microsoft.com/en-us/windows/win32/api/winsock2/nf-winsock2-wsarecvfrom
[blast]: /blog/2018/04/13/
[buf]: /blog/2023/02/13/
[crt]: /blog/2023/02/15/
[dll]: /blog/2021/05/31/
[ffi]: /blog/2018/05/27/
[gl]: https://www.khronos.org/opengl/wiki/OpenGL_Loading_Library
[hdr]: https://gcc.gnu.org/onlinedocs/gcc/Precompiled-Headers.html
[sdl]: /blog/2023/01/08/
[types]: https://learn.microsoft.com/en-us/windows/win32/winprog/windows-data-types
[u-config]: /blog/2023/01/18/
[w64devkit]: https://github.com/skeeto/w64devkit
[writefile]: https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-writefile
[ww]: https://winworldpc.com/library/
[mini]: https://github.com/skeeto/u-config/blob/e6ebb9b/miniwin32.h
