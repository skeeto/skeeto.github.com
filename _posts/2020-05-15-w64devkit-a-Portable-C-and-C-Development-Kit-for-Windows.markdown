---
title: "w64devkit: a Portable C and C++ Development Kit for Windows"
layout: post
date: 2020-05-15T03:43:04Z
tags: [c, cpp, win32]
uuid: d600d846-3692-474f-adbf-45db63079581
excerpt_separator: <!--more-->
---

*This article was discussed [on Hacker News][hn].*

As a computer engineer, my job is to use computers to solve important
problems. Ideally my solutions will be efficient, and typically that
means making the best use of the resources at hand. Quite often these
resources are machines running Windows and, despite my misgivings about
the platform, there is much to be gained by properly and effectively
leveraging it.

Sometimes [targeting Windows while working from another platform][wine]
is sufficient, but other times I must work on the platform itself. There
[are various options available][four] for C development, and I've
finally formalized my own development kit: [**w64devkit**][repo].

<!--more-->

For most users, the value is in the **78MiB .zip** available in the
"Releases" on GitHub. This (relatively) small package includes a
state-of-the-art C and C++ compiler ([latest GCC][w64]), a [powerful
text editor][vim], [debugger][gdb], a [complete x86 assembler][nasm],
and [miniature unix environment][bb]. It's "portable" in that there's no
installation. Just unzip it and start using it in place. With w64devkit,
it literally takes a few seconds on any Windows to get up and running
with a fully-featured, fully-equipped, first-class [development
environment][ide].

The development kit is cross-compiled entirely from source using Docker,
though Docker is not needed to actually use it. The repository is just a
Dockerfile and some documentation. The only build dependency is Docker
itself. It's also easy to customize it for your own personal use, or to
audit and build your own if, for whatever reason, you didn't trust my
distribution. This is in stark contrast to Windows builds of most open
source software where the build process is typically undocumented,
under-documented, obtuse, or very complicated.

### From script to Docker

Publishing this is not necessarily a commitment to always keep w64devkit
up to date, but this Dockerfile *is* derived from (and replaces) a shell
script I've been using continuously [for over two years now][blast]. In
this period, every time GCC has made a release, I've built myself a new
development kit, so I'm already in the habit.

I've been using Docker on and off for about 18 months now. It's an
oddball in that it's something I learned on the job rather than my own
time. I formed an early impression that still basically holds: **The
main purpose of Docker is to contain and isolate misbehaved software to
improve its reliability**. Well-behaved, well-designed software benefits
little from containers.

My unusual application of Docker here is no exception. [Most software
builds are needlessly complicated and fragile][simple], especially
Autoconf-based builds. Ironically, the worst configure scripts I've
dealt with come from GNU projects. They waste time on superfluous checks
("Does your compiler define `size_t`?") then produce a build that
doesn't work anyway because you're doing something slightly unusual.
Worst of all, despite my best efforts, the build will be contaminated by
the state of the system doing the build.

My original build script was fragile by extension. It would work on one
system, but not another due to some subtle environment change — a
slightly different system header that reveals a build system bug
([example in GCC][bug]), or the system doesn't have a file at a certain
hard-coded absolute path that shouldn't be hard-coded. Converting my
script to a Dockerfile locks these problems in place and makes builds
much more reliable and repeatable. The misbehavior is contained and
isolated by Docker.

Unfortunately it's not *completely* contained. In each case I use make's
`-j` option to parallelize the build since otherwise it would take
hours. Some of the builds have subtle race conditions, and some bad luck
in timing can cause a build to fail. Docker is good about picking up
where it left off, so it's just a matter of trying again.

In one case a build failed because Bison and flex were not installed
even though they're not normally needed. Some dependency isn't expressed
correctly, and unlucky ordering leads to an unused `.y` file having the
wrong timestamp. Ugh. I've had this happen a lot more in Docker than
out, probably because file system operations are slow inside Docker and
it creates greater timing variance.

### Other tools

The README explains some of my decisions, but I'll summarize a few here:

* Git. Important and useful, so I'd love to have it. But it has a weird
  installation (many [.zip-unfriendly symlinks][sym]) tightly-coupled
  with msys2, and its build system does not support cross-compilation.
  I'd love to see a clean, straightforward rewrite of Git in a single,
  appropriate implementation language. Imagine installing the latest Git
  with `go get git-scm.com/git`. (*Update*: [libgit2 is working on
  it][git2]!)

* Bash. It's a much nicer interactive shell than BusyBox-w32 `ash`. But
  the build system doesn't support cross-compilation, and I'm not sure
  it supports Windows without some sort of compatibility layer anyway.

* Emacs. Another powerful editor. But the build system doesn't support
  cross-compilation. It's also *way* too big.

* Go. Tempting to toss it in, but [Go already does this all correctly
  and effectively][go]. It simply doesn't require a specialized
  distribution. It's trivial to manage a complete Go toolchain with
  nothing but Go itself on any system. People may say its language
  design comes from the 1970s, but the tooling is decades ahead of
  everyone else.

### Alternatives 

For a long, long time Cygwin filled this role for me. However, I never
liked its bulky nature, the complete opposite of portable. Cygwin
processes always felt second-class on Windows, particularly in that it
has its own view of the file system compared to other Windows processes.
They could never fully cooperate. I also don't like that there's no
toolchain for cross-compiling with Cygwin as a target — e.g. compile
Cygwin binaries from Linux. Finally [it's been essentially obsoleted by
WSL][wsl] which matches or surpasses it on every front.

There's msys and [msys2][msys2], which are a bit lighter. However, I'm
still in an isolated, second-class environment with weird path
translation issues. These tools *do* have important uses, and it's the
only way to compile most open source software natively on Windows. For
those builds that don't support cross-compilation, it's *the* only path
for producing Windows builds. It's just not what I'm looking for when
developing my own software.

*Update*: [llvm-mingw][llvm] is an eerily similar project using Docker
the same way, but instead builds LLVM.

### Using Docker for other builds

I also [converted my GnuPG build script][gnupg] to a Dockerfile. Of
course I don't plan to actually *use* GnuPG on Windows. I just need it
[for passphrase2pgp][p2], which I test against GnuPG. This tests the
Windows build.

In the future I may extend this idea to a few other tools I don't intend
to include with w64devkit. If you have something in mind, you could use
my Dockerfiles as a kind of starter template.


[bb]: https://frippery.org/busybox/
[blast]: /blog/2018/04/13/#a-better-alternative
[bug]: https://gcc.gnu.org/legacy-ml/gcc/2017-05/msg00219.html
[four]: /blog/2016/06/13/
[gdb]: https://www.gnu.org/software/gdb/
[git2]: https://github.com/libgit2/libgit2/pull/5507
[gnupg]: https://github.com/skeeto/gnupg-windows-build
[go]: /blog/2020/01/21/
[hn]: https://news.ycombinator.com/item?id=23292161
[ide]: https://sanctum.geek.nz/arabesque/unix-as-ide-introduction/
[llvm]: https://github.com/mstorsjo/llvm-mingw
[msys2]: https://www.msys2.org/
[nasm]:https://www.nasm.us/ 
[p2]: /blog/2019/07/10/
[repo]: https://github.com/skeeto/w64devkit
[simple]: /blog/2017/03/30/
[sym]: https://github.com/skeeto/w64devkit/issues/1
[vim]: https://www.vim.org/
[w64]: http://mingw-w64.org/
[wine]: /blog/2018/11/15/
[wsl]: /blog/2017/11/30/
