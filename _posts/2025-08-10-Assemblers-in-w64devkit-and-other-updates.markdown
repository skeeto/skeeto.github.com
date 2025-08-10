---
title: 'Assemblers in w64devkit, and other updates'
layout: post
date: 2025-08-10T15:42:50Z
tags: [x86]
uuid: 3e24c35f-9fac-470a-8225-2e0a0bc8f7ac
---

Today I'm releasing [w64devkit][] 2.4.0, mostly for [GCC 15.2][gcc]. As
usual, it includes the continuous background improvements, and ideally
each release is the best so far. The [first release][init] included the
Netwide Assembler, [NASM][], but it's now been a year since I removed NASM
from the distribution (2.0.0). I'm asked on occasion why, or how to get it
back. Because I value thorough source control logs, my justifications for
this, and all changes, are captured in these logs, so `git log` is a kind
of miniature, project blog. I understand this is neither discoverable nor
obvious, especially because the GitHub UI (ugh) lacks anything like `git
log` in the terminal. So let's talk about it here, along with other recent
changes.

NASM is nice assembler, and I still in general prefer its x86 syntax to
the GNU Assembler, GAS. It's tidy, self-contained, dependency-free other
than a C toolchain, reliable, easy to build and cross compile, which is
why I included it in the first place. However, *it's just not a good fit
for w64dk*. It's redundant with Binutils `as`, which is already mandatory
for supporting GCC. As a rule, w64dk is a curation that avoids redundancy,
and a second assembler requires special justification. Originally it was
that the syntax was nicer, but last year I decided that wasn't enough to
outweigh against NASM's weaknesses.

First, it doesn't integrate well with the GNU toolchain, at least not to
the extent that `as` does. For example, many people don't realize they can
assemble GAS assembly through the general `gcc` driver.

    $ gcc -c myprogram.s

There's rarely a reason to invoke `as` directly. Need a specific assembler
flag? Use `-Wa` or `-Xassembler`. Going through the compiler driver for
both assembly and linking (i.e. instead of `ld`) has the advantage of
operating at a "higher level." The compiler driver knows about the whole
toolchain, and better understands [the sysroot][sysroot]. Use a capital
file extension, `.S`, and `gcc` will automatically run it through the C
preprocessor.

    $ gcc -c myprogram.S

This is quite nifty, especially for cross builds. NASM isn't so integrated
and so requires invoking the `nasm` command directly. It's not a big deal,
but it's friction that adds up.

But even more, the most useful form of assembly in the context of w64dk is
[inline assembly][asm], which of course is completely out of NASM's lane.
So most of the time you're going to be writing GAS anyway. Again, NASM is
just not integrated into the toolchain like `as`.

Second, NASM is a *dreadfully slow* assembler — in the vicinity of *two
orders of magnitude slower* than GAS! If your assembly program is a couple
hundred lines, no big deal. If you're writing a compiler targeting NASM,
it's impractical beyond toy programs. Friends don't let friends use NASM
as a back-end. If you're so allergic to GAS, note that [YASM][yasm] has
matching syntax and better performance.

Third, and the last nail in the coffin, NASM doesn't support DWARF debug
information in Windows targets (`win32`/`win64`). That means you cannot
debug NASM programs with GDB on Windows, at least not with source-level
debugging. Were you even aware GAS has source-level debugging with GDB?
Sure, you can show the assembly pane (`layout asm`) and step through the
*disassembly* with `ni`, but stepping through the original source (`layout
src`) is a whole different experience. Even better, bring up the register
pane (`layout regs`) and you have something akin to a Visual Studio watch
window. It's such a pleasant experience, and yet no tutorial I've seen has
ever mentioned it. It should be the first thing people are taught. *Get
your act together, assembly tutorials!*

In theory, YASM ought to solve this with its `-g dwarf2`, but alas this
feature appears to be broken. So that really just leaves GAS as the most
practical game in town for assembly on Windows with GNU-style toolchains.
In case it helps: [Learning a little PDP-11 assembly][pdp11] gave me a
deeper understanding — and appreciation — of why GAS x86 is the way it is.
Makes it sting a little less than before.

### Compiling NASM

At the same time w64dk lost NASM, it gained the ability to run Autotools
`configure` scripts. It only took a few environment variables as hints for
the script, and [a small hack in the shell][conf] to *undo* an unnecessary
Autotools hack. The typical native `cmd.exe` invocation with a command
uses `/c`:

    cmd /c echo hello world

It's [*roughly*][cmd] equivalent to `-c` in a unix shell. This is what
you'd use if you're in another shell and you need to invoke `cmd` for a
specific purpose. However, if you're in an MSYS2 shell with its virtual
file system, `/c` looks like a path to the C drive. So MSYS2 "helpfully"
translates the switch to a native path, something like so:

    cmd C:\ echo hello world

Not helpful at all. So Autotools, assuming Cygwin-like environments are
the only that exist, uses a special escape form when invoking `cmd`:

    cmd //c echo hello world

Which Cygwin-like environments translate into the desired `/c`. If you're
not in such an environment, then `cmd.exe` sees the `//c`, which doesn't
work. So the [busybox-w32][] shell now pattern matches for precisely:

    cmd //c echo [...]

For a similar translation. That's right, it matches `echo` in particular
because that's the only `cmd` feature Autotools uses. So it's completely
unnecessary, just poor code generation.

With that work in place, you can download a NASM source release, untar it,
run `./configure`, `make -j`, and copy the resulting `nasm.exe` into w64dk
or wherever else on your `$PATH` is convenient. The same is true for quite
a bit of software! You can build Binutils, including `as` itself, exactly
the same way. Being so easy for users to build their own tools means I'm
less concerned with including extraneous, more specialized tools, such as
NASM.

### Path Style

Borrowing [a concept from MSYS2][path], `w64devkit.ini` now has `path
style` option for controlling the initial `PATH`, using [the same names
and configuration][src]. I've already found it useful in testing w64dk
itself in a relatively pristine, hermetic environment:

```ini
[w64devkit]
home = .
path style = strict
```

This uses the `w64devkit/` directory itself as `$HOME`, and `$PATH` is
initially just the w64dk `bin/` directory. See the `w64devkit.ini` header
for full documentation.

Otherwise, most of the major features have been discussed already:
[peports and vc++filt][tools], [pkg-config][], and [xxd][].


[NASM]: https://www.nasm.us/
[asm]: /blog/2024/12/20/
[busybox-w32]: https://frippery.org/busybox/
[cmd]: /blog/2022/02/18/
[conf]: https://github.com/skeeto/w64devkit/commit/7785eb9c
[gcc]: https://gcc.gnu.org/pipermail/gcc/2025-August/246491.html
[init]: /blog/2020/05/15/
[path]: https://www.msys2.org/wiki/MSYS2-introduction/#path
[pdp11]: https://archive.org/details/h42_Assembly_Language_Programming_for_PDP-11_and_LSL-11_Computers_ISBN_0-697-08164-8/page/n1/mode/2up
[pkg-config]: /blog/2023/01/18/
[src]: https://github.com/msys2/MSYS2-packages/blob/ae252e94/filesystem/profile#L28-L45
[sysroot]: https://peter0x44.github.io/posts/cross-compilers/
[tools]: /blog/2024/06/30/
[w64devkit]: https://github.com/skeeto/w64devkit/
[xxd]: /blog/2025/02/17/
[yasm]: https://github.com/yasm/yasm
