---
title: 'Frankenwine: Multiple personas in a Wine process'
layout: post
date: 2026-01-19T21:51:38Z
tags: [c, win32, linux, x86]
uuid: d2b53f8d-88a6-400b-a748-693a758741c5
---

I came across a recent article on [making Linux system calls from a Wine
process][exe]. Windows programs running under Wine are still normal Linux
processes and may interact with the Linux kernel like any other process.
None of this was surprising, and the demonstration works just as I expect.
Still, it got the wheels spinning and I realized an *almost* practical
application: build [my pkg-config implementation][pc] such that on Windows
`pkg-config.exe` behaves as a native pkg-config, but when run under Wine
this same binary takes the persona of a Linux program and becomes a cross
toolchain pkg-config, bypassing Win32 and talking directly with the Linux
kernel. [Cosmopolitcan Libc][libc] cleverly does this out-of-the-box, but
in this article we'll mash together a couple existing sources with a bit
of glue.

The results are in [the merge-demo branch][br] of u-config, and took
hardly any work:

    $ git show --stat
    ...
     main_linux_amd64.c |   8 ++---
     main_wine.c        | 101 +++++++++++++++++++++++++++++++++++++++++
     src/linux_noarch.c |  16 ++++-----
     src/u-config.c     |   1 +
     4 files changed, 114 insertions(+), 12 deletions(-)

A platform layer, `main_wine.c`, is a merge of two existing platform
layers, one of which required unavoidable tweaks. We'll get to those
details in a moment. First we'll need to detect if we're running under
Wine, and [the best solution I found][so] was to locate
`ntdll!wine_get_version`. If this function exists, we're in Wine. That
works out to a pretty one-liner because `ntdll.dll` is already loaded:

```c
bool running_on_wine()
{
    return GetProcAddress(GetModuleHandleA("ntdll"), "wine_get_version");
}
```

An x86-64 Linux syscall wrapper with [thorough inline assembly][asm]:

```c
ptrdiff_t syscall3(int n, ptrdiff_t a, ptrdiff_t b, ptrdiff_t c)
{
    ptrdiff_t r;
    asm volatile (
        "syscall"
        : "=a"(r)
        : "a"(n), "D"(a), "S"(b), "d"(c)
        : "rcx", "r11", "memory"
    );
    return r;
}

ptrdiff_t write(int fd, void *buf, ptrdiff_t len)
{
    return syscall3(SYS_write, fd, (ptrdiff_t)buf, len);
}
```

I'd normally use `long` for all these integers because Linux is [LP64][]
(`long` is pointer-sized), but Windows is LLP64 (only `long long` is 64
bits). It's so bizarre to interface with Linux from LLP64, and this will
have consequences later. With these pieces we can see the basic shape of a
split personality program:

```c
    if (running_on_wine()) {
        write(1, "hello, wine\n", 12);
    } else {
        HANDLE h = GetStdHandle(STD_OUTPUT_HANDLE);
        WriteFile(h, "hello, windows\n", 15, 0, 0);
    }
```

We can cram two programs into this binary and select which program at run
time depending on what we see. In typical programs locating and calling
into glibc would be a challenge, particularly with the incompatible ABIs
involved. We're avoiding it here by interfacing directly with the kernel.

### Application to u-config

Luckily u-config has completely-optional platform layers implemented with
Linux system calls. The POSIX platform layer works fine, and that's what
distributions should generally use, but these bonus platforms are unhosted
and do not require libc. That means we can shove it into a Windows build
with relatively little trouble.

Before we do that, let's think about what we're doing. [Debian has great
cross toolchain support][home], including Mingw-w64. There are even a few
Windows libraries in the Debian package repository, [such as zlib][z], and
we can build Windows programs against them. If you're cross-building and
using pkg-config, you ought to use the cross toolchain pkg-config, which
in GNU ecosystems gets an architecture prefix like the other cross tools.
Debian cross toolchains each include a cross pkg-config, and it sometimes
*almost* works correctly! Here's what I get on Debian 13:

    $ x86_64-w64-mingw32-pkg-config --cflags --libs zlib
    -I/usr/x86_64-w64-mingw32/include -L/usr/x86_64-w64-mingw32/lib -lz

Note the architecture in the `-I` and `-L` options. It really is querying
the [cross sysroot][sysroot]. Though these paths are in the cross sysroot,
and so should not be listed by pkg-config. It's unoptimal and indicates
this pkg-config is probably misconfigured. In other cases it's far from
correct:

    $ x86_64-w64-mingw32-pkg-config --variable pc_path pkg-config
    /usr/local/lib/x86_64-linux-gnu/pkgconfig:...

A tool prefixed `x86_64-w64-mingw32-` should not produce paths containing
`x86_64-linux-gnu` (the host architecture in this case). Our version won't
have these issues.

The u-config platform interface is five functions:

```c
filemap os_mapfile(os *, arena *, s8 path);  // read whole files
s8node *os_listing(os *, arena *, s8 path);  // list directories
void    os_write(os *, i32 fd, s8);          // standard out/err
void    os_fail(os *);                       // non-zero exit

void uconfig(config *);
```

Platforms implement the first four functions, and call `uconfig()` with
the platform's configuration, context pointer (`os *`), command line
arguments, environment, and some memory (all in the `config` object). My
strategy is to link two platforms into the binary, and the first challenge
is they both define `os_write`, etc. I did not plan nor intend for one
binary to contain more than one platform layer. Unity builds offer a fix
without changing a single line of code:

```c
#define os_fail     win32_fail
#define os_listing  win32_listing
#define os_mapfile  win32_mapfile
#define os_write    win32_write
#include "main_windows.c"
#undef os_write
#undef os_mapfile
#undef os_listing
#undef os_fail

#define os_fail     linux_fail
#define os_listing  linux_listing
#define os_mapfile  linux_mapfile
#define os_write    linux_write
#include "main_linux_amd64.c"
#undef os_write
#undef os_mapfile
#undef os_listing
#undef os_fail
```

This dirty, but effective trick [may look familiar][fuzz]. It also doesn't
interfere with the other builds. Next I define the real platform functions
as a dispatch based on our run-time situation:

```c
b32 wine_detected;

filemap os_mapfile(os *ctx, arena *a, s8 path)
{
    if (wine_detected) {
        return linux_mapfile(ctx, a, path);
    } else {
        return win32_mapfile(ctx, a, path);
    }
}
```

If I were serious about keeping this experiment, I'd lift `os` as I did
the functions (as `win32_os`, `linux_os`) and include `wine_detected` in
the context, eliminating this global variable. That cannot be done with
simple hacks and macros.

The next challenge is that I wrote the Linux platform layer assuming LP64,
and so it uses `long` instead of an equivalent platform-agnostic type like
`ptrdiff_t`. I never thought this would be an issue because this source
literally contains `asm` blocks and no conditional compilation, yet here
we are. Lesson learned. I wanted to try an extremely janky `#define` on
`long` to fix it, but this source file has a couple `long long` that won't
play along. These multi-token type names of C are antithetical to its
preprocessor! So I adjusted the source manually instead.

The Windows and Linux platform entry points are completely different, both
in name and form, and so co-exist naturally. The merged platform layer is
a new entry point that will pass control to the appropriate entry point:

```c
void entrypoint(ptrdiff_t *stack);  // Linux
void __stdcall mainCRTStartup();    // Windows
```

On Linux `stack` is [the initial value of the stack pointer][start], which
[points to `argc`, `argv`, `envp`, and `auxv`][auxv]. We'll need construct
an artificial "stack" for the Linux platform layer to harvest. On Windows
this is [the process entry point][crt], and it will find the rest on its
own as a normal Windows process. Ultimately this ended up simpler than I
expected:

```c
void __stdcall merge_entrypoint()
{
    wine_detected = running_on_wine();
    if (wine_detected) {
        u8 *fakestack[CMDLINE_ARGV_MAX+1];
        c16 *cmd = GetCommandLineW();
        fakestack[0] = (u8 *)(iz)cmdline_to_argv8(cmd, fakestack+1);
        // TODO: append envp to the fake stack
        entrypoint((iz *)fakestack);
    } else {
        mainCRTStartup();
    }
}
```

Where [`cmdline_to_argv8` is my Windows argument parser][wild], already
used by u-config, and I reserve one element at the front to store `argc`.
Since this is just a proof-of-concept I didn't bother fabricating and
pushing `envp` onto the fake stack. The Linux entry point doesn't need
`auxv` and can be omitted. Once in the Linux entry point it's essentially
a Linux process from then on, except the x64 calling convention still in
use internally.

Finally, I configure the Linux platform layer for Debian's cross sysroot:

```c
#define PKG_CONFIG_LIBDIR "/usr/x86_64-w64-mingw32/lib/pkgconfig"
#define PKG_CONFIG_SYSTEM_INCLUDE_PATH "/usr/x86_64-w64-mingw32/include"
#define PKG_CONFIG_SYSTEM_LIBRARY_PATH "/usr/x86_64-w64-mingw32/lib"
```

And that's it! We have our platform merge. Build ([w64devkit][]):

    $ cc -nostartfiles -e merge_entrypoint -o pkg-config.exe main_wine.c

On Debian use `x86_64-w64-mingw32-gcc` for `cc`. The `-e` linker option
selects the new, higher level entry point. After installing [Wine
binfmt][binfmt], here's how it looks on Debian:

    $ ./pkg-config.exe --cflags --libs zlib
    -lz

That's the correct output, but is it using the cross sysroot? Ask it to
include the `-I` argument despite it being in the cross sysroot:

    $ ./pkg-config.exe --cflags --libs --keep-system-cflags zlib
    -I/usr/x86_64-w64-mingw32/include -lz

Looking good! It passes the `pc_path` test, too:

    $ ./pkg-config.exe --variable pc_path pkg-config
    /usr/x86_64-w64-mingw32/lib/pkgconfig

Running *this same binary* on Windows after installing zlib in w64devkit:

    $ ./pkg-config.exe --cflags --libs --keep-system-cflags zlib
    -IC:/w64devkit/include -lz

Also:

    $ ./pkg-config.exe --variable pc_path pkg-config
    C:/w64devkit/lib/pkgconfig;C:/w64devkit/share/pkgconfig

My Frankenwine is a success!


[LP64]: https://en.wikipedia.org/wiki/64-bit_computing#64-bit_data_models
[asm]: /blog/2024/12/20/
[auxv]: https://articles.manugarg.com/aboutelfauxiliaryvectors
[binfmt]: https://packages.debian.org/trixie/wine-binfmt
[br]: https://github.com/skeeto/u-config/commit/e0008d7e
[crt]: /blog/2023/02/15/
[exe]: https://gpfault.net/posts/drunk-exe.html
[fuzz]: /blog/2025/02/05/
[home]: /blog/2021/08/21/
[libc]: https://justine.lol/cosmopolitan/
[pc]: /blog/2023/01/18/
[so]: https://web.archive.org/web/20250923061634/https://stackoverflow.com/questions/7372388/determine-whether-a-program-is-running-under-wine-at-runtime/42333249#42333249
[start]: /blog/2025/03/06/
[sysroot]: https://peter0x44.github.io/posts/cross-compilers/
[w64devkit]: https://github.com/skeeto/w64devkit
[wild]: /blog/2022/02/18/
[z]: https://packages.debian.org/trixie/x32/libz-mingw-w64
