---
title: The wild west of Windows command line parsing
layout: post
date: 2022-02-18T03:52:12Z
tags: [c, win32, x86]
uuid: 04c886e0-3434-4292-b7de-e8213461838c
---

I've been experimenting again lately with [writing software without a
runtime][fs] aside from the operating system itself, both on Linux and
Windows. Another way to look at it: I write and embed a bespoke, minimal
runtime within the application. One of the runtime's core jobs is
retrieving command line arguments from the operating system. On Windows
this is a deeper rabbit hole than I expected, and far more complex than I
realized. There is no standard, and every runtime does it a little
differently. Five different applications may see five different sets of
arguments — even different argument counts — from the same input, and this
is *before* any sort of option parsing. It's truly a modern day Tower of
Babel: "Confound their command line parsing, that they may not understand
one another's arguments."

Unix-like systems pass the `argv` array directly from parent to child. On
Linux it's literally copied onto the child's stack just above the stack
pointer on entry. The runtime just bumps the stack pointer address a few
bytes and calls it `argv`. Here's a minimalist x86-64 Linux runtime in
just 6 instructions (22 bytes):

```nasm
_start: mov   edi, [rsp]     ; argc
        lea   rsi, [rsp+8]   ; argv
        call  main
        mov   edi, eax
        mov   eax, 60        ; SYS_exit
        syscall
```

It's 5 instructions (20 bytes) on ARM64:

```nasm
_start: ldr  w0, [sp]        ; argc
        add  x1, sp, 8       ; argv
        bl   main
        mov  w8, 93          ; SYS_exit
        svc  0
```

On Windows, `argv` is passed in serialized form as a string. That's how
MS-DOS did it (via the [Program Segment Prefix][psp]), because [that's how
CP/M did it][cpm]. It made more sense when processes were mostly launched
directly by humans: The string was literally typed by a human operator,
and *somebody* has to parse it after all. Today, processes are nearly
always launched by other programs, but despite this, must still serialize
the argument array into a string as though a human had typed it out.

Windows itself provides an operating system routine for parsing command
line strings: [CommandLineToArgvW][cl2a]. Fetch the command line string
with [GetCommandLineW][gcl], pass it to this function, and you have your
`argc` and `argv`. Plus maybe LocalFree to clean up. It's only available
in "wide" form, so [if you want to work in UTF-8][ws] you'll also need
`WideCharToMultiByte`. It's around 20 lines of C rather than 6 lines of
assembly, but it's not too bad.

### My GetCommandLineW

GetCommandLineW returns a pointer into static storage, which is why it
doesn't need to be freed. More specifically, it comes from the [Process
Environment Block][peb]. This got me thinking: Could I locate this address
myself without the API call? First I needed to find the PEB. After some
research I found a PEB pointer in the [Thread Information Block][tib],
itself found via the `gs` register (x64, `fs` on x86), an [old 386 segment
register][seg]. Buried in the PEB is a [`UNICODE_STRING`][us], with the
command line string address. I worked out all the offsets for both x86 and
x64, and the whole thing is just three instructions:

```c
wchar_t *cmdline_fetch(void)
{
    void *cmd = 0;
    #if __amd64
    __asm ("mov %%gs:(0x60), %0\n"
           "mov 0x20(%0), %0\n"
           "mov 0x78(%0), %0\n"
           : "=r"(cmd));
    #elif __i386
    __asm ("mov %%fs:(0x30), %0\n"
           "mov 0x10(%0), %0\n"
           "mov 0x44(%0), %0\n"
           : "=r"(cmd));
    #endif
    return cmd;
}
```

From Windows XP through Windows 11, this returns exactly the same address
as GetCommandLineW. There's little reason to do it this way other than to
annoy Raymond Chen, but it's still neat and maybe has some super niche
use. Technically some of these offsets are undocumented and/or subject to
change, except Microsoft's own static link CRT also hardcodes all these
offsets. It's easy to find: disassemble any statically linked program,
look for the `gs` register, and you'll find it using these offsets, too.

If you look carefully at the `UNICODE_STRING` you'll see the length is
given by a `USHORT` in units of bytes, despite being a 16-bit `wchar_t`
string. This is [the source][src] of Windows' maximum command line length
of [32,767 characters][cp] (including terminator).

GetCommandLineW is from `kernel32.dll`, but CommandLineToArgvW is a bit
more off the beaten path in `shell32.dll`. If you wanted to avoid linking
to `shell32.dll` for [important reasons][shell32], you'd need to do the
command line parsing yourself. Many runtimes, including Microsoft's own
CRTs, don't call CommandLineToArgvW and instead do their own parsing. It's
messier than I expected, and when I started digging into it I wasn't
expecting it to involve a few days of research.

The GetCommandLineW has a rough explanation: split arguments on whitespace
(not defined), quoting is involved, and there's something about counting
backslashes, but only if they stop on a quote. It's not quite enough to
implement your own, and if you test against it, it's quickly apparent that
this documentation is at best incomplete. It links to a deprecated page
about [parsing C++ command line arguments][pcl] with a few more details.
Unfortunately the algorithm described on this page is not the algorithm
used by GetCommandLineW, nor is it used by any runtime I could find. It
even varies between Microsoft's own CRTs. There is no canonical command
line parsing result, not even a *de facto* standard.

I eventually came across David Deley's [How Command Line Parameters Are
Parsed][dd], which is the closest there is to an authoritative document on
the matter ([also][also]). Unfortunately it focuses on runtimes rather
than CommandLineToArgvW, and so some of those details aren't captured. In
particular, the first argument (i.e. `argv[0]`) follows entirely different
rules, which really confused me for while. The [Wine documentation][wine]
was helpful particularly for CommandLineToArgvW. As far as I can tell,
they've re-implemented it perfectly, matching it bug-for-bug as they do.

### My CommandLineToArgvW

Before finding any of this, I started building my own implementation,
which I now believe matches CommandLineToArgvW. These other documents
helped me figure out what I was missing. In my usual fashion, it's [a
little state machine][sm]: **[`cmdline.c`][mine]**. The interface:

```c
int cmdline_to_argv8(const wchar_t *cmdline, char **argv);
```

Unlike the others, mine encodes straight into [WTF-8][wtf8], a superset of
UTF-8 that can round-trip ill-formed UTF-16. The WTF-8 part is negative
lines of code: invisible since it involves *not* reacting to ill-formed
input. If you use the new-ish UTF-8 manifest Win32 feature then your
program cannot handle command line strings with ill-formed UTF-16, a
problem solved by WTF-8.

As documented, that `argv` must be a particular size — a pointer-aligned,
224kB (x64) or 160kB (x86) buffer — which covers the absolute worst case.
That's not too bad when the command line is limited to 32,766 UTF-16
characters. The worst case argument is a single long sequence of 3-byte
UTF-8. 4-byte UTF-8 requires 2 UTF-16 code points, so there would only be
half as many. The worst case `argc` is 16,383 (plus one more `argv` slot
for the null pointer terminator), which is one argument for each pair of
command line characters. The second half (roughly) of the `argv` is
actually used as a `char` buffer for the arguments, so it's all a single,
fixed allocation. There is no error case since it cannot fail.

```c
int mainCRTStartup(void)
{
    static char *argv[CMDLINE_ARGV_MAX];
    int argc = cmdline_to_argv8(cmdline_fetch(), argv);
    return main(argc, argv);
}
```

Also: Note the `FUZZ` option in my source. It has been pretty thoroughly
[fuzz tested][fuzz]. It didn't find anything, but it does make me more
confident in the result.

I also peeked at some language runtimes to see how others handle it. Just
as expected, Mingw-w64 has the behavior of an old (pre-2008) Microsoft
CRT. Also expected, CPython implicitly does whatever the underlying C
runtime does, so its exact command line behavior depends on which version
of Visual Studio was used to build the Python binary. OpenJDK
[pragmatically calls CommandLineToArgvW][jdk]. Go (gc) [does its own
parsing][go], with behavior mixed between CommandLineToArgvW and some of
Microsoft's CRTs, but not quite matching either.

### Building a command line string

I've always been boggled as to why there's no complementary inverse to
CommandLineToArgvW. When spawning processes with arbitrary arguments,
everyone is left to implement the inverse of this under-specified and
non-trivial command line format to serialize an `argv`. Hopefully the
receiver parses it compatibly! There's no falling back on a system routine
to help out. This has lead to a lot of repeated effort: it's not limited
to high level runtimes, but almost any extensible application (itself a
kind of runtime). Fortunately serializing is not quite as complex as
parsing since many of the edge cases simply don't come up if done in a
straightforward way.

Naturally, I also wrote my own implementation (same source):

```c
int cmdline_from_argv8(wchar_t *cmdline, int len, char **argv);
```

Like before, it accepts a WTF-8 `argv`, meaning it can correctly pass
through ill-formed UTF-16 arguments. It returns the actual command line
length. Since this one *can* fail when `argv` is too large, it returns
zero for an error.

```c
char *argv[] = {"python.exe", "-c", code, 0};
wchar_t cmd[CMDLINE_CMD_MAX];
if (!cmdline_from_argv8(cmd, CMDLINE_CMD_MAX, argv)) {
    return "argv too large";
}
if (!CreateProcessW(0, cmd, /*...*/)) {
    return "CreateProcessW failed";
}
```

How do others handle this?

* The [aged Emacs implementation][emacs] is written in C rather than Lisp,
  steeped in history with vestigial wrong turns. Emacs still only calls
  the "narrow" CreateProcessA despite having every affordance to do
  otherwise, and [uses the wrong encoding at that][esql]. A personal
  source of headaches.

* CPython uses Python rather than C via [`subprocess.list2cmdline`][py].
  While [undocumented][undoc], it's accessible on any platform and easy to
  test against various inputs. Try it out!

* Go (gc) is [just as delightfully boring I'd expect][go2].

* OpenJDK [optimistically optimizes][jdk2] for command line strings under
  80 bytes, and like Emacs, displays the weathering of long use.

I don't plan to write a language implementation anytime soon, where this
might be needed, but it's nice to know I've already solved this problem
for myself!

[also]: https://web.archive.org/web/20210615061518/http://www.windowsinspired.com/how-a-windows-programs-splits-its-command-line-into-individual-arguments/
[cl2a]: https://docs.microsoft.com/en-us/windows/win32/api/shellapi/nf-shellapi-commandlinetoargvw
[cp]: https://docs.microsoft.com/en-us/windows/win32/api/processthreadsapi/nf-processthreadsapi-createprocessw
[cpm]: http://www.gaby.de/cpm/manuals/archive/cpm22htm/ch5.htm
[dd]: https://daviddeley.com/autohotkey/parameters/parameters.htm
[emacs]: https://git.savannah.gnu.org/cgit/emacs.git/tree/src/w32proc.c?h=emacs-27.2#n2009
[esql]: https://github.com/skeeto/emacsql/issues/77#issuecomment-887125675
[fs]: /blog/2016/01/31/
[fuzz]: /blog/2019/01/25/
[gcl]: https://docs.microsoft.com/en-us/windows/win32/api/processenv/nf-processenv-getcommandlinew
[go2]: https://go.googlesource.com/go/+/refs/tags/go1.17.7/src/syscall/exec_windows.go#101
[go]: https://go.googlesource.com/go/+/refs/tags/go1.17.7/src/os/exec_windows.go#115
[jdk2]: https://github.com/openjdk/jdk/blob/jdk-17%2B35/src/java.base/windows/classes/java/lang/ProcessImpl.java#L229
[jdk]: https://github.com/openjdk/jdk/blob/jdk-17+35/src/jdk.jpackage/windows/native/common/WinSysInfo.cpp#L141
[mine]: https://github.com/skeeto/scratch/blob/master/parsers/cmdline.c#L27
[pcl]: https://docs.microsoft.com/en-us/previous-versions/17w5ykft(v=vs.85)
[peb]: https://docs.microsoft.com/en-us/windows/win32/api/winternl/ns-winternl-peb
[psp]: https://en.wikipedia.org/wiki/Program_Segment_Prefix
[py]: https://github.com/python/cpython/blob/3.10/Lib/subprocess.py#L529
[seg]: https://en.wikipedia.org/wiki/X86_memory_segmentation
[shell32]: https://randomascii.wordpress.com/2018/12/03/a-not-called-function-can-cause-a-5x-slowdown/
[sm]: /blog/2020/12/31/
[src]: https://devblogs.microsoft.com/oldnewthing/20031210-00/?p=41553
[tib]: https://en.wikipedia.org/wiki/Win32_Thread_Information_Block
[undoc]: https://bugs.python.org/issue10838
[us]: https://docs.microsoft.com/en-us/windows/win32/api/subauth/ns-subauth-unicode_string
[wine]: https://source.winehq.org/git/wine.git/blob/5a66eab72:/dlls/shcore/main.c#l264
[ws]: /blog/2021/12/30/
[wtf8]: https://simonsapin.github.io/wtf-8/
