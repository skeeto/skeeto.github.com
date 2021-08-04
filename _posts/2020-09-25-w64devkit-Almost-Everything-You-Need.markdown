---
title: 'w64devkit: (Almost) Everything You Need'
layout: post
date: 2020-09-25T00:04:11Z
tags: [c, cpp, win32, rant]
uuid: e594c82d-a2e1-4035-8527-1b998045ceeb
excerpt_separator: <!--more-->
---

*This article was discussed [on Hacker News][hn].*

[This past May][intro] I put together my own C and C++ development
distribution for Windows called [**w64devkit**][gh]. The *entire*
release weighs under 80MB and requires no installation. Unzip and run it
in-place anywhere. It's also entirely offline. It will never
automatically update, or even touch the network. In mere seconds any
Windows system can become a reliable development machine. (To further
increase reliability, [disconnect it from the internet][up].) Despite
its simple nature and small packaging, w64devkit is *almost* everything
you need to develop *any* professional desktop application, from a
command line utility to a AAA game.

<!--more-->

I don't mean this in some [useless Turing-complete sense][make], but in
a practical, *get-stuff-done* sense. It's much more a matter of
*know-how* than of tools or libraries. So then what is this "almost"
about?

* The distribution does not have WinAPI documentation. It's notoriously
  [difficult to obtain][chm] and, besides, unfriendly to redistribution.
  It's essential for interfacing with the operating system and difficult
  to work without. Even a dead tree reference book would suffice.

* Depending on what you're building, you may still need specialized
  tools. For instance, game development requires [tools for editing art
  assets][3d].

* There is no formal source control system. Git is excluded per the
  issues noted in the announcement, and my next option, [Quilt][quilt],
  has similar limitations. However, `diff` and `patch` *are* included,
  and are sufficient for a kind of old-school, patch-based source
  control. I've used it successfully when dogfooding w64devkit in a
  fresh Windows installation.

### Everything else

As I said in my announcement, w64devkit includes a powerful text editor
that fulfills all text editing needs, from code to documentation. The
editor includes a tutorial (`vimtutor`) and complete, built-in manual
(`:help`) in case you're not yet familiar with it.

What about navigation? Use the included [ctags][ctags] to generate a
tags database (`ctags -R`), then [jump instantly][tags] to any
definition at any time. No need for [that Language Server Protocol
rubbish][lsp]. This does not mean you must laboriously type identifiers
as you work. Use [built-in completion][compl]!

Build system? That's also covered, via a Windows-aware unix-like
environment that includes `make`. [Learning how to use it][tut] is a
breeze. Software is by its nature unavoidably complicated, so [don't
make it more complicated than necessary][simple].

What about debugging? Use the debugger, GDB. Performance problems? Use
the profiler, gprof. Inspect compiler output either by asking for it
(`-S`) or via the disassembler (`objdump -d`). No need to go online for
the [Godbolt Compiler Explorer][gb], as slick as it is. If the compiler
output is insufficient, use [SIMD intrinsics][simd]. In the worst case
there are two different assemblers available. Real time graphics? Use an
operating system API like OpenGL, DirectX, or Vulkan.

w64devkit *really is* nearly everything you need in a [single, no
nonsense, fully-*offline* package][cm]! It's difficult to emphasize this
point as much as I'd like. When interacting with the broader software
ecosystem, I often despair that [software development has lost its
way][jb]. This distribution is my way of carving out an escape from some
of the insanity. As a C and C++ toolchain, w64devkit by default produces
lean, sane, trivially-distributable, offline-friendly artifacts. All
runtime components in the distribution are [static link only][static],
so no need to distribute DLLs with your application either.

### Customize the distribution, own the toolchain

While most users would likely stick to my published releases, building
w64devkit is a two-step process with a single build dependency, Docker.
Anyone can easily customize it for their own needs. Don't care about
C++? Toss it to shave 20% off the distribution. Need to tune the runtime
for a specific microarchitecture? Tweak the compiler flags.

One of the intended strengths of open source is users can modify
software to suit their needs. With w64devkit, you *own the toolchain*
itself. It is [one of your dependencies][deps] after all. Unfortunately
the build initially requires an internet connection even when working
from source tarballs, but at least it's a one-time event.

If you choose to [take on dependencies][stb], and you build those
dependencies using w64devkit, all the better! You can tweak them to your
needs and choose precisely how they're built. You won't be relying on
the goodwill of internet randos nor the generosity of a free package
registry.

### Customization examples

Building existing software using w64devkit is probably easier than
expected, particularly since much of it has already been "ported" to
MinGW and Mingw-w64. Just don't bother with GNU Autoconf configure
scripts. They never work in w64devkit despite having everything they
technically need. So other than that, here's a demonstration of building
some popular software.

One of [my coworkers][dan] uses his own version of [PuTTY][putty]
patched to play more nicely with Emacs. If you wanted to do the same,
grab the source tarball, unpack it using the provided tools, then in the
unpacked source:

    $ make -C windows -f Makefile.mgw

You'll have a custom-built putty.exe, as well as the other tools. If you
have any patches, apply those first!

Would you like to embed an extension language in your application? Lua
is a solid choice, in part because it's such a well-behaved dependency.
After unpacking the source tarball:

    $ make PLAT=mingw

This produces a complete Lua compiler, runtime, and library. It's not
even necessary to use the Makefile, as it's nearly as simple as "`cc
*.c`" — painless to integrate or embed into any project.

Do you enjoy NetHack? Perhaps you'd like to [try a few of the custom
patches][nhp]. This one is a little more complicated, but I was able to
build NetHack 3.6.6 like so:

    $ sys/winnt/nhsetup.bat
    $ make -C src -f Makefile.gcc cc="cc -fcommon" link="cc"

NetHack has [a bug necessitating `-fcommon`][fcommon]. If you have any
patches, apply them with `patch` before the last step. I won't belabor it
here, but with just a little more effort I was also able to produce a
NetHack binary with curses support via [PDCurses][pd] — statically-linked
of course.

How about my archive encryption tool, [Enchive][enchive]? The one that
[even works with 16-bit DOS compilers][dos]. It requires nothing special
at all!

    $ make

w64devkit can also host parts of itself: Universal Ctags, Vim, and NASM.
This means you can modify and recompile these tools without going
through the Docker build. Sadly [busybox-w32][bb] cannot host itself,
though it's close. I'd *love* if w64devkit could fully host itself, and
so Docker — and therefore an internet connection and such — would only
be needed to bootstrap, but unfortunately that's not realistic given the
state of the GNU components.

### Offline and reliable

Software development has increasingly become [dependent on a constant
internet connection][e]. Robust, offline tooling and development is
undervalued.

Consider: Does your current project depend on an external service? Do
you pay for this service to ensure that it remains up? If you pull your
dependencies from a repository, how much do you trust those who maintain
the packages? [Do you even know their names?][rel] What would be your
project's fate if that service went down permanently? It will someday,
though hopefully only after your project is dead and forgotten. If you
have the ability to work permanently offline, then you already have
happy answers to all these questions.


[3d]: https://www.blender.org/
[bb]: https://frippery.org/busybox/
[chm]: http://laurencejackson.com/win32/
[cm]: https://www.youtube.com/watch?v=W3ml7cO96F0&t=1h25m50s
[compl]: https://georgebrock.github.io/talks/vim-completion/
[ctags]: https://github.com/universal-ctags/ctags
[dan]: /blog/2016/09/02/
[deps]: https://research.swtch.com/deps
[dos]: /blog/2018/04/13/
[e]: https://deftly.net/posts/2017-06-01-measuring-the-weight-of-an-electron.html
[enchive]: https://github.com/skeeto/enchive
[fcommon]: https://wiki.gentoo.org/wiki/Gcc_10_porting_notes/fno_common
[gb]: https://godbolt.org/
[gh]: https://github.com/skeeto/w64devkit
[go]: /blog/2020/01/21/
[hn]: https://news.ycombinator.com/item?id=24586556
[intro]: /blog/2020/05/15/
[jb]: https://www.youtube.com/watch?v=ZSRHeXYDLko
[lsp]: https://old.reddit.com/r/vim/comments/b3yzq4/a_lsp_client_maintainers_view_of_the_lsp_protocol/
[lua]: https://www.lua.org/
[make]: /blog/2016/04/30/
[nhp]: https://bilious.alt.org/
[pd]: https://pdcurses.org/
[putty]: https://www.chiark.greenend.org.uk/~sgtatham/putty/
[quilt]: https://wiki.debian.org/UsingQuilt
[rel]: https://drewdevault.com/2020/02/06/Dependencies-and-maintainers.html
[simd]: /blog/2015/07/10/
[simple]: https://nullprogram.com/blog/2017/03/30/
[static]: https://drewdevault.com/dynlib
[stb]: https://github.com/nothings/stb
[tags]: http://vimdoc.sourceforge.net/htmldoc/tagsrch.html#tagsrch.txt
[tut]: /blog/2017/08/20/
[up]: https://jacquesmattheij.com/why-johnny-wont-upgrade/
