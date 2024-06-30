---
title: 'Symbol inspection tools for w64devkit: vc++filt and peports'
layout: post
date: 2024-06-30T21:30:19Z
tags: []
uuid: c2154db5-7975-4497-9ce6-eb9077b72042
---

I introduced two new tools to [w64devkit][w64], [`vc++filt`][vc++filt] and
[`peports`][peports] (pronounced like *purports*), which aid manual symbol
inspection and complement one another. As of this writing, the latter is
not yet in a release, but it's feature-complete and trivial to build if
you wanted to try it out early. This article explains the motivation and
purpose for each.

### vc++filt

Binutils has `c++filt`, a tool to *demangle* C++ symbols. Its primary use
case is operating on whole files or streams, passing through anything that
doesn't look like a mangled C++ symbol, and interpolating human-friendly
names in place of mangled symbols. In, say, Vim I could run the current
buffer through it to translate mangled symbols ([`:%!c++filt`][vim]).
Otherwise it's often composed with other tools in a pipeline.

For example, suppose I want to inspect the assembly of this C++ source:

```c++
int bar(int, int);

int foo(int x)
{
    return bar(x, x);
}
```

Without needing to create intermediate files, I could compile and filter
out assembler directives in one pipeline:

    $ cc -S -O2 -o - x.cpp | grep -Fv .
    _Z3fooi:
            movl    %ecx, %edx
            jmp     _Z3barii

The two symbols, `foo` and `bar` have been mangled according to the
[Itanium C++ ABI][it], preferred by GNU toolchains. It might be nice to
see human-friendly names for these symbols. `c++filt` to the rescue:

    $ cc -S -O2 -o - x.cpp | grep -Fv . | c++filt
    foo(int):
            movl    %ecx, %edx
            jmp     bar(int, int)

That's no longer valid assembly of course — which is essentially the point
of mangling in the first place — but it's friendlier to human inspection.

However, MSVC uses [the Microsoft ABI][w] and is substantially different.
Instead of the pejorative "mangle," they choose more positive terminology,
[*decoration*][ms]. When inspecting MSVC-generated code, `c++filt` is no
use. The closest alternative is `undname` from the MSVC toolchain, but
it's far less flexible and powerful. Plus it requires installing Visual
Studio. A number of times I've wanted a tool like `c++filt` for Visual
C++. In other words, `vc++filt`!

The name decoration ABI is undocumented and complicated. Rather than write
my own implementation based on reverse engineered documentation, or pull
in a third-party library (i.e. LLVM), I call [`UnDecorateSymbolName`][und]
from `dbghelp.dll` — [included in every version of Windows][ver] since at
least Windows XP. The cost is that `vc++filt` is not portable and couldn't
be included in a cross toolchain.

`UnDecorateSymbolName` is reasonably performant, but after testing on real
data I found it was still worth caching calls. The program never queries
the same symbol twice in a run, instead referring to [its cache][map];
except in the unlikely case of running out of memory, in which case it
gracefully reverts to calling `dbghelp.dll`.

Otherwise `vc++filt` behaves like `c++filt`, minus features inapplicable
to the Microsoft ABI. It has a simple tokenizer for Microsoft ABI name
decorations. Tokens are passed to `UnDecorateSymbolName`. All other bytes
pass through, as are false positives. In practice, decorated symbols can
be *quite lengthy* — in my observations of real world libraries, as long
as 10kB — so the tokenizer buffer is generous in order to compensate.

To nobody's surprise, MSVC tools are not composable in pipelines, so
repeating the previous assembly demonstration requires an extra step.
First compile to assembly (`/Fa`):

    $ cl /c /Fa /O2 x.cpp
    x.cpp

Applying some directive and comment filtering to the assembly:

    $ sed 's/;.*//' x.asm | grep -Eiv '^_|^include|^end|^$|\$'
    PUBLIC  ?foo@@YAHH@Z
    EXTRN   ?bar@@YAHHH@Z:PROC
    ?foo@@YAHH@Z PROC
            mov     edx, ecx
            jmp     ?bar@@YAHHH@Z
    ?foo@@YAHH@Z ENDP

Applying `vc++filt`:

    $ sed 's/;.*//' x.asm | grep -Eiv '^_|^include|^end|^$|\$' | vc++filt
    PUBLIC  int __cdecl foo(int)
    EXTRN   int __cdecl bar(int,int):PROC
    int __cdecl foo(int) PROC
            mov     edx, ecx
            jmp     int __cdecl bar(int,int)
    int __cdecl foo(int) ENDP

Like before, I could also undecorate a buffer in Vim: `:%!vc++filt`. It's
not limited to assembly, of course. It could be applied to the output of
Binutils `nm` or `objdump`, or MSVC `dumpbin`. Speaking of which…

### peports

[Portable Executables][pe] (PE), which includes EXE and DLL images, have
an *export table* and an *import table*. The `peports` command displays
these tables: "portable executable 'ports." It's equivalent to the MSVC
`dumpbin` (i.e. `link /dump`) options `/exports` and `/imports`, but I
wanted an implementation I could include in w64dk. Binutils `objdump -p`
prints export and import tables, but it's unreliable on binaries not
linked by [BFD][bfd] (case in point: Go binaries), and the output includes
extra information before and after. Unlike `vc++filt`, `peports` is
portable and runs on Linux, etc.

An export table entry is a 3-tuple: address, ordinal, symbol. The symbol
may be null, though this is unusual. An import table entry is a different
3-tuple: module, hint, symbol. The module names another PE, typically a
DLL. The hint references an ordinal in the named module's export table.
The symbol may also be null, in which case the hint is not just a hint.
That's also unusual, though less so. Ordinals are a relic of the 16-bit
era, but in my testing I found quite a few modern, 64-bit programs which
import Win32 and Winsock functions by ordinal, i.e. null symbol, which is
supported for certain symbols.

Export and import tables have grown on me by their robustness. One can
[link the same symbol from different DLLs][dlls] — unlikely by intention,
but not so unlikely by accident. Symbols are bound to modules at *link
time*, not at *load time*. In ELF, modules and symbols are separate,
disconnected tables, delaying binding to load time — or, with lazy
loading, even later — and risking mix-ups. Though that fast-and-loose
paradigm is also allows semantic interposition via `LD_PRELOAD`, for
better [or worse][sem].

My problem with PE was a lack of tooling to examine export and, more so,
import tables! I filled that gap with `peports`. Now I can conveniently
answer a question such as, "Is my program importing anything unexpected?"
For export tables there is also the [previously discussed][dll] Mingw-w64
`gendef` — w64dk includes an improved version — which outputs concisely in
the [DEF format][def].

If you run `peports.exe` against `vc++filt` in the latest w64dk builds:

    $ peports "$(which vc++filt)"
    dbghelp.dll
            0       UnDecorateSymbolName
    KERNEL32.dll
            0       ExitProcess
            0       GetCommandLineW
            0       GetConsoleMode
            0       GetStdHandle
            0       ReadFile
            0       VirtualAlloc
            0       WideCharToMultiByte
            0       WriteConsoleW
            0       WriteFile
    SHELL32.dll
            0       CommandLineToArgvW

Like a typical EXE, it has no export table — or, from another point of
view, the export table has no entries — so the output lists no exports.
Import table entries are grouped by module in a way that matches their
representation in the PE image. The number is the ordinal hint and the
rest of the line is the symbol. Because the program [is CRT-free][crt],
there are no imports from `msvcrt.dll` or similar. For typical programs,
`peports` will print hundreds of imports.

All zeros for hints is new, and as of this writing, not yet the situation
in a w64dk release. In most programs, hints are populated with essentially
random numbers. Historically, GNU and MSVC toolchains write *guesses* that
are incorrect more than 99.9% of the time. Rather than pretend knowledge,
hints in binaries built by w64dk will be zero when not explicitly chosen.
In my observations, independent toolchains behave this way.

There are few DLLs in w64dk itself to test out. One is Vim, the bulk of
which is in a DLL shared between console (`vim`) and graphical (`gvim`)
EXEs. It has one export, and requesting just exports (`-e`):

    $ peports -e "$W64DEVKIT_HOME/share/vim/vim64.dll"
    EXPORTS
            1       VimMain

Just one export, the entry point to start the editor. The export format is
like the import format, but the module name is replaced with `EXPORTS`.
Because it's an export, that's not an ordinal *hint*, but an actual
ordinal, hence it's non-zero. Whether or not an ordinal is stable is
semantic, and more a social issue than a technical one.

I had not written a serious PE parser until `peports`, where "serious"
includes [thorough fuzz testing][fuzz] (when there *is* a bug, it's
[likely in the toolchain][bug] and not in `peports`). Some unexpected
findings:

* Most offsets are *Relative Virtual Addresses* (RVA), a run time address
  relative to the image base after the image loaded. Therefore a PE parser
  must emulate loading the PE image into memory. Consider the potential
  challenges of a 32-bit program parsing a 64-bit PE. Fortunately it's not
  as bad as it seems because…

* The RVA address space is 32 bits, so even 64-bit PEs must load within a
  32-bit region. For this reason, handling 32-bit and 64-bit PEs in one
  program is not terribly complicated — certainly simpler than ELF — as
  the emulated load is essentially the same. Some real world PE images I
  observed, including one from Microsoft, are recklessly approaching the
  4GiB RVA limit. By the end of the decade this will probably become a
  real constraint for the people behind them. Though in practice there's
  already [a 2GiB barrier][mm] for code.

* Windows only supports ASCII for dynamic linking. That includes both
  module and symbol names. In other words, only use ASCII file names for
  DLLs. Otherwise nobody could import its symbols, at least not reliably.
  It's possible to express non-ASCII 8-bit names. These are not currently
  rejected, but their interpretation is left to code page configuration.

To aid debugging, `peports` escapes octets that are not printable ASCII.
This may clear up mismatches that do not otherwise appear as such. For
example, exporting `π` as a UTF-8 symbol will display as `\xcf\x80`. For
this reason, backslashes are also hex-encoded as though unprintable. Angle
brackets indicate special notation, such as null symbols (`<NONAME>`),
empty symbols (`<>`), and forwarders, and so are also hex-encoded to
disambiguate output. I chose angle brackets for notation because they're
invalid in both file names and symbol names.

### Let's get together, yeah yeah yeah

Suppose there's a DLL exporting Visual C++ symbols, and you'd like to both
see those exports and undecorate them. Combine `peports` and `vc++filt`!
Surprising perhaps, but one such DLL is `msvcrt.dll`, which exports pieces
of a C++ implementation. For example, allocation functions:

    $ peports "$WINDIR/system32/msvcrt.dll" | vc++filt -p | grep 'operator '
            20      operator new
            21      operator new
            22      operator delete
            35      operator new[]
            36      operator new[]
            37      operator delete[]

Not to be used seriously, but with the [DLL symbol renaming trick][dlls],
we can translate Visual C++ decoration into GNU mangling and hook GNU C++
up to a Visual C++ allocator. Take this `msvcrt.def`:

    LIBRARY msvcrt
    EXPORTS
    _Znwy  == ??2@YAPEAX_K@Z
    _ZdlPv == ??3@YAXPEAX@Z

The left side is GNU, the right side is Microsoft, both referring to the
same prototype (64-bit only). Creating the import library:

    $ dlltool -l msvcrt.lib -d msvcrt.def

Finally a little test program:

```c++
extern "C" int mainCRTStartup()
{
    int *p = new int;
    *p = 1234;
    delete p;
    return 0;
}
```

Build and test:

    $ cc -nostdlib -g3 -fno-sized-deallocation example.cpp msvcrt.lib
    $ gdb a.exe

Stepping through in GDB, everything works as expected. The "no sized
deallocation" option is required because, [quite correctly][custom], GCC
defaults to [size-aware deallocation][sa], but the MSVCRT implementation
is not. Alternatively I could do this with a different translation in the
DEF, mismatching the prototypes on purpose while knowing that the extra
parameter (size) is harmless in this particular calling convention.

While this last example is kind of silly, it's a useful demonstration of
the capabilities these tools add to w64dk. I now use each regularly.


[bfd]: https://ftp.gnu.org/pub/old-gnu/Manuals/bfd-2.9.1/html_chapter/bfd_1.html
[bug]: https://github.com/skeeto/w64devkit/issues/135
[crt]: /blog/2023/02/15/
[custom]: /blog/2023/12/17/
[def]: https://sourceware.org/binutils/docs/binutils/def-file-format.html
[dll]: /blog/2021/05/31/
[dlls]: /blog/2023/08/27/
[fuzz]: /blog/2019/01/25/
[it]: https://itanium-cxx-abi.github.io/cxx-abi/abi.html#mangling
[map]: /blog/2023/09/30/
[mm]: https://eli.thegreenplace.net/2012/01/03/understanding-the-x64-code-models
[ms]: https://learn.microsoft.com/en-us/cpp/build/reference/decorated-names
[pe]: https://learn.microsoft.com/en-us/windows/win32/debug/pe-format
[peports]: https://github.com/skeeto/w64devkit/blob/master/src/peports.c
[sa]: https://isocpp.org/files/papers/n3778.html
[sem]: https://maskray.me/blog/2021-05-09-fno-semantic-interposition
[und]: https://learn.microsoft.com/en-us/windows/win32/api/dbghelp/nf-dbghelp-undecoratesymbolname
[vc++filt]: https://github.com/skeeto/w64devkit/blob/master/src/vc%2B%2Bfilt.c
[ver]: https://learn.microsoft.com/en-us/windows/win32/debug/dbghelp-versions
[vim]: https://vimhelp.org/change.txt.html#complex-change
[w64]: /blog/2020/09/25/
[w]: https://en.wikiversity.org/wiki/Visual_C%2B%2B_name_mangling
