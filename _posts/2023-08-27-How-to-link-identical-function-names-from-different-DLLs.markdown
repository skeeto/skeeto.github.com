---
title: How to link identical function names from different DLLs
layout: post
date: 2023-08-27T01:46:31Z
tags: [c, win32]
uuid: 265f121d-9418-4eb6-929f-a125264d0f2a
---

For the typical DLL function call you declare the function prototype (via
header file), you inform the link editor (`ld`, `link`) that the DLL
exports a symbol with that name (import library), it matches the declared
name with this export, and it becomes an import in your program's import
table. What happens when two different DLLs export the same symbol? The
link editor will pick the first found. But what if you want to use *both*
exports? If they have the same name, how could program or link editor
distinguish them? In this article I'll demonstrate a technique to resolve
this by creating a program which links with and directly uses two
different C runtimes (CRTs) simultaneously.

In [PE executable images][pe], an import isn't just a symbol, but a tuple
of DLL name and symbol. For human display, a tuple is typically formatted
with an exclamation point delimiter, as in `msvcrt.dll!malloc`, though
sometimes without the `.dll` suffix. You've likely seen this in stack
traces. Because it's a tuple and not just a symbol, it's possible to refer
to, and import, the same symbol from different DLLs. Contrast that with
ELF, which has a list of shared objects, and a separate list of symbols,
with the dynamic linker pairing them up at load time. That permits cool
tricks like `LD_PRELOAD`, but for the same reason loading is less
predictable.

Windows comes with several CRTs, and various libraries and applications
use one or another ([or none][free]) depending on how they were built. As
C standard library implementations they export mostly the same symbols,
`malloc`, `printf`, etc. With imports as tuples, it's not so unusual for
an application to load multiple CRTs at once. Typically coexistence is
transitive. That is, a module does not directly access both CRTs but
depends on modules that use different CRTs. One module calls, say,
`msvcrt.dll!malloc`, and another module calls `ucrtbase.dll!malloc`. With
DLL-qualified symbols, this is sound so long as modules don't cross the
streams, e.g. an allocation in one module must not be freed in the other.
Libraries in this ecosystem must avoid exposing their CRT through their
interfaces, such as expecting the library's caller to `free()` objects:
The caller might not have access to the right `free`!

Contrast again with the unix ecosystem generally, where a process can only
load one libc and everyone is expected to share. Libraries commonly expect
callers to `free()` their objects (e.g. [libreadline][rl], [xcb][]),
blending their interface with libc.

Suppose you're in such a situation where, due to unix-oriented libraries,
your application must use functions from two different CRTs at once. One
might have been compiled with Mingw-w64 and linked with MSVCRT, and the
other compiled with MSVC and linked with UCRT. We need to call `malloc`
and `free` in each, but they have the same name. What a pickle!

There's an obvious, and probably most common, solution: [run-time dynamic
linking][dl]. Use load-time linking on one CRT, and LoadLibrary on the
other CRT with GetProcAddress to obtain function pointers. However, it's
possible to do this entirely with load-time linking!

### A malloc by any other name would allocate as well

Think about it a moment and you might wonder: If the names are the same,
how can I pick which I'm calling? The tuple representation won't work
because `!` cannot appear in an identifier, which is, after all, why it
was chosen. The trick is that we're going to *rename* one of them! To
demonstrate, I'll use [my Windows development kit][all], [w64devkit][], a
Mingw-w64 distribution that links MSVCRT. I'm going to use UCRT as the
second CRT to access `ucrtbase.dll!malloc`.

I can choose whatever valid identifier I'd like, so I'm going to pick
`ucrt_malloc`. This will [require a declaration][dll]:

```c
__declspec(dllimport) void *ucrt_malloc(size_t);
```

If I stop here and try to use it, of course it won't work:

    ld: undefined reference to `__imp_ucrt_malloc'

The linker hasn't yet been informed of the change in management. For that
we'll need an import library. I'll define one using a [.def file][def],
which I'll name `ucrtbase.def`:

    LIBRARY ucrtbase.dll
    EXPORTS
    ucrt_malloc == malloc

The last line says that this library has the symbol `ucrt_malloc`, but
that it should be imported as `malloc`. This line is the lynchpin to the
whole scheme. Note: The double equals is important, as a single equals
sign means something different.  Next, use `dlltool` to build the import
library:

    $ dlltool -d ucrtbase.def -l ucrtbase.lib

The equivalent MSVC tool is [`lib`][lib], but as far as I know it cannot
quite do this sort of renaming. However, MSVC `link` will work just fine
with this `dlltool`-created import library. The name `ucrtbase.lib`, while
obvious, is irrelevant. It's that `LIBRARY` line that ties it to the DLL.
My test source file looks like this:

```c
#include <stdlib.h>

__declspec(dllimport) void *ucrt_malloc(size_t);

int main(void)
{
    void *msvcrt[] = {malloc(1), malloc(1), malloc(1)};
    void *ucrt[] = {ucrt_malloc(1), ucrt_malloc(1), ucrt_malloc(1)};
    return 0;
}
```

It compiles successfully:

    $ cc -g3 -o main.exe main.c ucrtbase.lib

I can see the two `malloc` imports with `objdump`:

    $ objdump -p main.exe
    ...
	DLL Name: msvcrt.dll
    ...
	844a	 1021  malloc
    ...
	DLL Name: ucrtbase.dll
	847e	    1  malloc

It loads and runs successfully, too:

    $ gdb main.exe
    Reading symbols from main.exe...
    (gdb) break 9
    Breakpoint 1 at 0x1400013cd: file main.c, line 9.
    (gdb) run
    Thread 1 hit Breakpoint 1, main () at main.c:9
    9           return 0;
    (gdb) p msvcrt
    $1 = {0xd06a30, 0xd06a70, 0xd06ab0}
    (gdb) p ucrt
    $2 = {0x6e9490, 0x6eb7c0, 0x6eb800}

The pointer addresses confirm that these are two, distinct allocators.
Perhaps you're wondering what happens if I cross the streams?

```c
int main(void)
{
    free(ucrt_malloc(1));
}
```

The MSVCRT allocator justifiably panics over the bad pointer:

```c
$ cc -g3 -o chaos.exe chaos.c ucrtbase.lib
$ gdb -ex run chaos.exe
Starting program: chaos.exe
warning: HEAP[chaos.exe]:
warning: Invalid address specified to RtlFreeHeap
Thread 1 received signal SIGTRAP, Trace/breakpoint trap.
0x00007ffc42c369af in ntdll!RtlRegisterSecureMemoryCacheCallback ()
(gdb)
```

While you're probably not supposed to meddle with `ucrtbase.dll` like
this, the general principle of export renames is reasonable. I don't
expect I'll ever need to do it, but I like that I have the option.


[all]: /blog/2020/09/25/
[def]: https://sourceware.org/binutils/docs/binutils/def-file-format.html
[dl]: https://learn.microsoft.com/en-us/windows/win32/dlls/run-time-dynamic-linking
[dll]: /blog/2021/05/31/
[free]: /blog/2023/02/15/
[lib]: https://learn.microsoft.com/en-us/cpp/build/reference/overview-of-lib
[pe]: https://learn.microsoft.com/en-us/windows/win32/debug/pe-format
[rl]: https://tiswww.case.edu/php/chet/readline/readline.html#Basic-Behavior
[w64devkit]: https://github.com/skeeto/w64devkit
[xcb]: https://man.archlinux.org/man/xcb-requests.3.en
