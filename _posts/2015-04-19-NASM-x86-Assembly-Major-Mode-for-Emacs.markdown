---
title: NASM x86 Assembly Major Mode for Emacs
layout: post
date: 2015-04-19T02:38:23Z
tags: [emacs, x86]
uuid: 6966e5d3-9e81-3fc0-5d47-eeb29677f7da
---

Last weekend I created a new Emacs mode, [**nasm-mode**][nasm-mode],
for editing [Netwide Assembler][nasm] (NASM) x86 assembly programs.
Over the past week I tweaked it until it felt comfortable enough to
share on [MELPA][melpa]. It's got what you'd expect from a standard
Emacs programming language mode: syntax highlighting, automatic
indentation, and imenu support. It's not a full parser, but it knows
all of NASM's instructions and directives.

Until recently I didn't really have preferences about x86 assemblers
([GAS][gas], NASM, [YASM][yasm], [FASM][fasm], MASM, etc.) or syntax
(Intel, AT&T). I stuck to the GNU Assembler (GAS) since it's already
there with all the other GNU development tools I know and love, and
it's required for inline assembly in GCC. However, nasm-mode now marks
my commitment to NASM as my primary x86 assembler.

### Why NASM?

I need an assembler that can assemble 16-bit code (8086, 8088, 80186,
80286), because [real mode is fun][com]. Despite its `.code16gcc`
directive, GAS is not suitable for this purpose. It's *just* enough to
get the CPU into protected mode — as needed when writing an operating
system with GCC — and that's it. A different assembler is required
for serious 16-bit programming.

[GAS syntax has problems][bad]. I'm not talking about the argument
order (source first or destination first), since there's no right
answer to that one. The linked article covers a number of problems,
with these being the big ones for me:

* The use of `%` sigils on all registers is tedious. I'm sure it's
  handy when generating code, where it becomes a register namespace,
  but it's annoying to write.

* Integer constants are an easy source of bugs. Forget the `$` and
  suddenly you're doing absolute memory access, which is a poor
  default. NASM simplifies this by using brackets `[]` for all such
  "dereferences."

* GAS cannot produce pure binaries — raw machine code without any
  headers or container (ELF, COFF, PE). Pure binaries are useful for
  developing [shellcode][shellcode], bootloaders, 16-bit COM programs,
  and [just-in-time compilers][jit].

Being a portable assembler, GAS is the jack of all instruction sets,
master of none. If I'm going to write a lot of x86 assembly, I want a
tool specialized for the job.

#### YASM

I also looked at YASM, a rewrite of NASM. It supports 16-bit assembly
and mostly uses NASM syntax. In my research I found that NASM used to
lag behind in features due to slower development, which is what
spawned YASM. In recent years this seems to have flipped around, with
YASM lagging behind. If you're using YASM, nasm-mode should work
pretty well for you, since it's still very similar.

YASM optionally supports GAS syntax, but this reintroduces almost all
of GAS's problems. Even YASM's improvements (i.e. its `ORG` directive)
become broken when switching to GAS syntax.

#### FASM

FASM is the "flat assembler," an assembler written in assembly
language. This means it's only available on x86 platforms. While I
don't really plan on developing x86 assembly on a Raspberry Pi, I'd
rather not limit my options! I already regard 16-bit DOS programming
as a form of embedded programming, and this may very well extend to
the rest of x86 someday.

Also, it hasn't made its way into the various Linux distribution
package repositories, including Debian, so it's already at a
disadvantage for me.

#### MASM

This is Microsoft's assembler that comes with Visual Studio. Windows
only and not open source, this is in no way a serious consideration.
But since NASM's syntax was originally derived from MASM, it's worth
mentioning. NASM takes the good parts of MASM and [fixes the
mistakes][vs] (such as the `offset` operator). It's different enough
that nasm-mode would not work well with MASM.

#### NASM

It's not perfect, but it's got an [excellent manual][manual], it's a
solid program that does exactly what it says it will do, has a
powerful macro system, great 16-bit support, highly portable, easy to
build, and its semantics and syntax has been carefully considered. It
also comes with a simple, pure binary disassembler (`ndisasm`). In
retrospect it seems like an obvious choice!

My one complaint would be that it's that it's *too* flexible about
labels. The colon on labels is optional, which can lead to subtle
bugs. NASM will warn about this under some conditions (orphan-labels).
Combined with the preprocessor, the difference between a macro and a
label is ambiguous, short of re-implementing the entire preprocessor
in Emacs Lisp.

### Why nasm-mode?

Emacs comes with an `asm-mode` for editing assembly code for various
architectures. Unfortunately it's another jack-of-all-trades that's
not very good. More so, it doesn't follow Emacs' normal editing
conventions, having unusual automatic indentation and self-insertion
behaviors. It's what prompted me to make nasm-mode.

To be fair, I don't think it's possible to write a major mode that
covers many different instruction set architectures. Each architecture
has its own quirks and oddities that essentially makes gives it a
unique language. This is especially true with x86, which, from its 37
year tenure touched by so many different vendors, comes in a number of
incompatible flavors. Each assembler/architecture pair needs its own
major mode. I hope I just wrote NASM's.

One area where I'm still stuck is that I can't find an x86 style
guide. It's easy to find half a dozen style guides of varying
authority for any programming language that's more than 10 years old
... except x86. There's no obvious answer when it comes to automatic
indentation. How are comments formatted and indented? How are
instructions aligned? Should labels be on the same line as the
instruction? Should labels require a colon? (I've decided this is
"yes.") What about long label names? How are function
prototypes/signatures documented? (The mode could take advantage of
such a standard, a la ElDoc.) It seems everyone uses their own style.
This is another conundrum for a generic asm-mode.

There are a couple of [other nasm-modes][old] floating around with
different levels of completeness. Mine should supersede these, and
will be much easier to maintain into the future as NASM evolves.


[nasm-mode]: https://github.com/skeeto/nasm-mode
[nasm]: http://www.nasm.us/
[melpa]: http://melpa.org/
[gas]: https://www.gnu.org/software/binutils/
[yasm]: http://yasm.tortall.net/
[fasm]: http://flatassembler.net/
[com]: /blog/2014/12/09/
[bad]: http://x86asm.net/articles/what-i-dislike-about-gas/
[jit]: /blog/2015/03/19/
[old]: http://matthieuhauglustaine.blogspot.com/2011/08/nasm-mode-for-emacs.html
[shellcode]: http://www.vividmachines.com/shellcode/shellcode.html
[vs]: https://courses.engr.illinois.edu/ece390/archive/mp/f99/mp5/masm_nasm.html
[manual]: http://www.nasm.us/doc/
