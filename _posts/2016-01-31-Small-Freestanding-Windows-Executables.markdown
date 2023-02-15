---
title: Small, Freestanding Windows Executables
layout: post
date: 2016-01-31T22:53:03Z
tags: [c, x86, linux, win32, optimization]
uuid: 8eddc701-52d3-3b0c-a8a8-dd13da6ead2c
---

**Update**: This is old and [was **updated in 2023**](/blog/2023/02/15/)!

Recently I've been experimenting with freestanding C programs on
Windows. *Freestanding* refers to programs that don't link, either
statically or dynamically, against a standard library (i.e. libc).
This is typical for operating systems and [similar, bare metal
situations][com]. Normally a C compiler can make assumptions about the
semantics of functions provided by the C standard library. For
example, the compiler will likely replace a call to a small,
fixed-size `memmove()` with move instructions. Since a freestanding
program would supply its own, it may have different semantics.

My usual go to for C/C++ on Windows is [Mingw-w64][mingw], which has
greatly suited my needs the past couple of years. It's [packaged on
Debian][deb], and, when combined with Wine, allows me to fully develop
Windows applications on Linux. Being GCC, it's also great for
cross-platform development since it's essentially the same compiler as
the other platforms. The primary difference is the interface to the
operating system (POSIX vs. Win32).

However, it has one glaring flaw inherited from MinGW: it links
against msvcrt.dll, an ancient version of the Microsoft C runtime
library that currently ships with Windows. Besides being dated and
quirky, [it's not an official part of Windows][msvcrt] and never has
been, despite its inclusion with every release since Windows 95.
Mingw-w64 doesn't have a C library of its own, instead patching over
some of the flaws of msvcrt.dll and linking against it.

Since so much depends on msvcrt.dll despite its unofficial nature,
it's unlikely Microsoft will ever drop it from future releases of
Windows. However, if strict correctness is a concern, we must ask
Mingw-w64 not to link against it. An alternative would be
[PlibC][plibc], though the LGPL licensing is unfortunate. Another is
Cygwin, which is a very complete POSIX environment, but is heavy and
GPL-encumbered.

Sometimes I'd prefer to be more direct: [skip the C standard library
altogether][hh] and talk directly to the operating system. On Windows
that's the Win32 API. Ultimately I want a tiny, standalone .exe that only
links against system DLLs.

### Linux vs. Windows

The most important benefit of a standard library like libc is a
portable, uniform interface to the host system. So long as the
standard library suits its needs, the same program can run anywhere.
Without it, the programs needs an implementation of each
host-specific interface.

On Linux, operating system requests at the lowest level are made
directly via system calls. This requires a bit of assembly language
for each supported architecture (`int 0x80` on x86, `syscall` on
x86-64, `swi` on ARM, etc.). The POSIX functions of the various Linux
libc implementations are built on top of this mechanism.

For example, here's a function for a 1-argument system call on x86-64.

~~~c
long
syscall1(long n, long arg)
{
    long result;
    __asm__ volatile (
        "syscall"
        : "=a"(result)
        : "a"(n), "D"(arg)
    );
    return result;
}
~~~

Then `exit()` is implemented on top. Note: A *real* libc would do
cleanup before exiting, like calling registered `atexit()` functions.

~~~c
#include <syscall.h>  // defines SYS_exit

void
exit(int code)
{
    syscall1(SYS_exit, code);
}
~~~

The situation is simpler on Windows. Its low level system calls are
undocumented and unstable, changing across even minor updates. The
formal, stable interface is through the exported functions in
kernel32.dll. In fact, kernel32.dll is essentially a standard library
on its own (making the term "freestanding" in this case dubious). It
includes functions usually found only in user-space, like string
manipulation, formatted output, font handling, and heap management
(similar to `malloc()`). It's not POSIX, but it has analogs to much of
the same functionality.

### Program Entry

The standard entry for a C program is `main()`. However, this is not
the application's *true* entry. The entry is in the C library, which
does some initialization before calling your `main()`. When `main()`
returns, it performs cleanup and exits. Without a C library, programs
don't start at `main()`.

On Linux the default entry is the symbol `_start`. It's prototype
would look like so:

~~~c
void _start(void);
~~~

Returning from this function leads to a segmentation fault, so it's up
to your application to perform the exit system call rather than
return.

On Windows, the entry depends on the type of application. The two
relevant subsystems today are the *console* and *windows* subsystems.
The former is for console applications (duh). These programs may still
create windows and such, but must always have a controlling console.
The latter is primarily for programs that don't run in a console,
though they can still create an associated console if they like. In
Mingw-w64, give `-mconsole` (default) or `-mwindows` to the linker to
choose the subsystem.

The default [entry for each is slightly different][entry].

~~~c
int WINAPI mainCRTStartup(void);
int WINAPI WinMainCRTStartup(void);
~~~

Unlike Linux's `_start`, Windows programs can safely return from these
functions, similar to `main()`, hence the `int` return. The `WINAPI`
macro means the function may have a special calling convention,
depending on the platform.

On any system, you can choose a different entry symbol or address
using the `--entry` option to the GNU linker.

### Disabling libgcc

One problem I've run into is Mingw-w64 generating code that calls
`__chkstk_ms()` from libgcc. I believe this is a long-standing bug,
since `-ffreestanding` should prevent these sorts of helper functions
from being used. The workaround I've found is to disable [the stack
probe][probe] and pre-commit the whole stack.

    -mno-stack-arg-probe -Xlinker --stack=0x100000,0x100000

Alternatively you could link against libgcc (statically) with `-lgcc`,
but, again, I'm going for a tiny executable.

### A freestanding example

Here's an example of a Windows "Hello, World" that doesn't use a C
library.

~~~c
#include <windows.h>

int WINAPI
mainCRTStartup(void)
{
    char msg[] = "Hello, world!\n";
    HANDLE stdout = GetStdHandle(STD_OUTPUT_HANDLE);
    WriteFile(stdout, msg, sizeof(msg), (DWORD[]){0}, NULL);
    return 0;
}
~~~

To build it:

    x86_64-w64-mingw32-gcc -std=c99 -Wall -Wextra \
        -nostdlib -ffreestanding -mconsole -Os \
        -mno-stack-arg-probe -Xlinker --stack=0x100000,0x100000 \
        -o example.exe example.c \
        -lkernel32

Notice I manually linked against kernel32.dll. The stripped final
result is only 4kB, mostly PE padding. There are [techniques to trim
this down even further][small], but for a substantial program it
wouldn't make a significant difference.

From here you could create a GUI by linking against `user32.dll` and
`gdi32.dll` (both also part of Win32) and calling the appropriate
functions. I already [ported my OpenGL demo][opengl] to a freestanding
.exe, dropping GLFW and directly using Win32 and WGL. It's much less
portable, but the final .exe is only 4kB, down from the original 104kB
(static linking against GLFW).

I may go this route for [the upcoming 7DRL 2016][7drl] in March.


[com]: /blog/2014/12/09/
[mingw]: http://mingw-w64.org/
[deb]: https://packages.debian.org/search?keywords=mingw-w64
[msvcrt]: https://blogs.msdn.microsoft.com/oldnewthing/20140411-00/?p=1273
[plibc]: http://plibc.sourceforge.net/
[entry]: https://msdn.microsoft.com/en-us/library/f9t8842e.aspx
[small]: http://www.phreedom.org/research/tinype/
[opengl]: /blog/2015/06/06/
[7drl]: http://7drl.org/2016/01/13/7drl-2016-announced-for-5-13-march/
[hh]: https://hero.handmade.network/forums/code-discussion/t/94-guide_-_how_to_avoid_c_c++_runtime_on_windows
[probe]: https://metricpanda.com/rival-fortress-update-45-dealing-with-__chkstk-__chkstk_ms-when-cross-compiling-for-windows/
