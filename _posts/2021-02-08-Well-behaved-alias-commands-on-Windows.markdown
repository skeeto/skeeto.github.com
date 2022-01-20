---
title: Well-behaved alias commands on Windows
layout: post
date: 2021-02-08T20:32:45Z
tags: [c, cpp, win32, trick]
uuid: d1c90d96-3696-4183-a52b-b10598a630c7
---

Since its inception I've faced a dilemma with [w64devkit][w64], my
[all-in-one][all] Mingw-w64 toolchain and [development environment
distribution for Windows][intro]. A major goal of the project is no
installation: unzip anywhere and it's ready to go as-is. However, full
functionality requires alias commands, particularly for BusyBox applets,
and the usual solutions are neither available nor viable. It seemed that
an installer was needed to assemble this last puzzle piece. This past
weekend I finally discovered a tidy and complete solution that solves this
problem for good.

That solution is a small C source file, [`alias.c`][c]. This article is
about why it's necessary and how it works.

### Hard and symbolic links

Some alias commands are for convenience, such as a `cc` alias for `gcc` so
that build systems need not assume any particular C compiler. Others are
essential, such as an `sh` alias for "`busybox sh`" so that it's available
as a shell for `make`. These aliases are usually created with links, hard
or symbolic. A GCC installation might include (roughly) a symbolic link
created like so:

```sh
ln -s gcc cc
```

BusyBox looks at its `argv[0]` on startup, and if it names an applet
(`ls`, `sh`, `awk`, etc.), it behaves like that applet. Typically BusyBox
aliases are installed as hard links to the original binary, and there's
even a `busybox --install` to set these up. Both kinds of aliases are
cheap and effective.

```sh
ln busybox sh
ln busybox ls
ln busybox awk
```

Unfortunately links are not supported by .zip files on Windows. They'd
need to be created by a dedicated installer. As a result, I've strongly
recommended that users run "`busybox --install`" at some point to
establish the BusyBox alias commands. While w64devkit works without them,
it works better with them. Still, that's an installation step!

An alternative option is to simply include a full copy of the BusyBox
binary for each applet — all 150 of them — simulating hard links. BusyBox
is small, around 4kB per applet on average, but it's not quite *that*
small. Since the .zip format doesn't use block compression — files are
compressed individually — this duplication will appear in the .zip itself.
My 573kB BusyBox build duplicated 150 times would double the distribution
size and increase the installation footprint by 25%. It's not worth the
cost.

Since .zip is so limited, perhaps I should use a different distribution
format that supports links. However, another w64devkit goal is making no
assumptions about what other tools are installed. Windows natively
supports .zip, even if that support isn't so great (poor performance, low
composability, missing features, etc.). With nothing more than the
w64devkit .zip on a fresh, offline Windows installation, you can begin
efficiently developing professional, native applications in under a
minute.

### Scripts as aliases

With links off the table, the next best option is a shell script. On
unix-like systems shell scripts are an effective tool for creating complex
alias commands. Unlike links, they can manipulate the argument list. For
instance, w64devkit includes a `c99` alias to invoke the C compiler
configured to use the C99 standard. To do this with a shell script:

```sh
#!/bin/sh
exec cc -std=c99 "$@"
```

This prepends `-std=c99` to the argument list and passes through the rest
untouched via the Bourne shell's special case `"$@"`. Because I used
`exec`, the shell process *becomes* the compiler in place. The shell
doesn't hang around in the background. It's just gone. This really quite
elegant and powerful.

The closest available on Windows is a .bat batch file. However, like some
other parts of DOS and Windows, the Batch language was designed as though
its designer once glimpsed at someone using a unix shell, perhaps looking
over their shoulder, then copied some of the ideas without understanding
them. As a result, it's not nearly as useful or powerful. Here's the Batch
equivalent:

```bat
@cc -std=c99 %*
```

The `@` is necessary because Batch prints its commands by default (Bourne
shell's `-x` option), and `@` disables it. Windows lacks the concept of
`exec(3)`, so Batch file interpreter `cmd.exe` continues running alongside
the compiler. A little wasteful but that hardly matters. What does matter
though is that `cmd.exe` doesn't behave itself! If you, say, Ctrl+C to
cancel compilation, you will get the infamous "Terminate batch job (Y/N)?"
prompt which interferes with other programs running in the same console.
The so-called "batch" script isn't a batch job at all: It's interactive.

I tried to use Batch files for BusyBox applets, but this issue came up
constantly and made this approach impractical. Nearly all BusyBox applets
are non-interactive, and lots of things break when they aren't. Worst of
all, you can easily end up with layers of `cmd.exe` clobbering each other
to ask if they should terminate. It was frustrating.

The prompt is hardcoded in `cmd.exe` and cannot be disabled. Since so much
depends on `cmd.exe` remaining exactly the way it is, Microsoft will never
alter this behavior either. After all, that's why they made PowerShell a
new, separate tool.

Speaking of PowerShell, could we use that instead? Unfortunately not:

1. It's installed by default on Windows, but is not necessarily enabled.
   One of my own use cases for w64devkit involves systems where PowerShell
   is disabled by policy. A common policy is it can be used interactively
   but not run scripts ("Running scripts is disabled on this system").

2. PowerShell is not a first class citizen on Windows, and will likely
   never be. Even under the friendliest policy it's not normally possible
   to put a PowerShell script on the `PATH` and run it by name. (I'm sure
   there are ways to make this work via system-wide configuration, but
   that's off the table.)

3. Everything in PowerShell is broken. For example, it does not support
   input redirection with files, and instead you must use the `cat`-like
   command, `Get-Content`, to pipe file contents. However, `Get-Content`
   translates its input and quietly damages your data. There is no way to
   disable this "feature" in the version of PowerShell that ships with
   Windows, meaning it cannot accomplish the simplest of tasks. This is
   just one of many ways that PowerShell is broken beyond usefulness.

Item (2) also affects w64devkit. It has a Bourne shell, but shell scripts
are still not first class citizens since Windows doesn't know what to do
with them. Fixing would require system-wide configuration, antithetical to
the philosophy of the project.

### Solution: compiled shell "scripts"

My working solution is inspired by an insanely clever hack used by my
favorite media player, [mpv][mpv]. The Windows build is strange at first
glance, containing two binaries, `mpv.exe` (large) and `mpv.com` (tiny).
Is that COM as in [an old-school 16-bit DOS binary][com]? No, that's just
a trick that works around a Windows limitation.

The Windows technology is broken up into subsystems. Console programs run
in the Console subsystem. Graphical programs run in the Windows subsystem.
[The original WSL][wsl] was a subsystem. Unfortunately this design means
that a program must statically pick a subsystem, hardcoded into the binary
image. The program cannot select a subsystem dynamically. For example,
this is why Java installations have both `java.exe` and `javaw.exe`, and
Emacs has `emacs.exe` and `runemacs.exe`. Different binaries for different
subsystems.

On Linux, a program that wants to do graphics just talks to the Xorg
server or Wayland compositor. It can dynamically choose to be a terminal
application or a graphical application. Or even both at once. This is
exactly the behavior of `mpv`, and it faces a dilemma on Windows: With
subsystems, how can it be both?

The trick is based on the environment variable `PATHEXT` which tells
Windows how to prioritize executables with the same base name but
different file extensions. If I type `mpv` and it finds both `mpv.exe` and
`mpv.com`, which binary will run? It will be the first listed in
`PATHEXT`, and by default that starts with:

    PATHEXT=.COM;.EXE;.BAT;...

So it will run `mpv.com`, which is actually a plain old [PE+][pe] `.exe`
in disguise. The Windows subsystem `mpv.exe` gets the shortcut and file
associations while Console subsystem `mpv.com` catches command line
invocations and serves as console liaison as it invokes the real
`mpv.exe`. Ingenious!

I realized I can pull a similar trick to create command aliases — not the
`.com` trick, but the miniature flagger program. If only I could compile
each of those Batch files to tiny, well-behaved `.exe` files so that it
wouldn't rely on the badly-behaved `cmd.exe`…

#### Tiny C programs

Years ago [I wrote about tiny, freestanding Windows executables][free].
That research paid off here since that's exactly what I want. The alias
command program need only manipulate its command line, invoke another
program, then wait for it to finish. This doesn't require the C library,
just a handful of `kernel32.dll` calls. My alias command programs can be
so small that would no longer matter that I have 150 of them, and I get
complete control over their behavior.

To compile, I use `-nostdlib` and `-ffreestanding` to disable all system
libraries, `-lkernel32` to pull that one back in, `-Os` (optimize for
size), and `-s` (strip) all to make the result as small as possible.

I don't want to write a little program for each alias command. Instead
I'll use a couple of C defines, `EXE` and `CMD`, to inject the target
command at compile time. So this Batch file:

```bat
@target arg1 arg2 %*
```

Is equivalent to this alias compilation:

```sh
gcc -DEXE="target.exe" -DCMD="target arg1 arg2" \
    -s -Os -nostdlib -ffreestanding -o alias.exe alias.c -lkernel32
```

The `EXE` string is the actual *module* name, so the `.exe` extension is
required. The `CMD` string replaces the first complete token of the
command line string (think `argv[0]`) and may contain arbitrary additional
arguments (e.g. `-std=c99`). Both are handled as wide strings (`L"..."`)
since the alias program uses the wide Win32 API in order to be fully
transparent. Though unfortunately at this time it makes no difference: All
currently aliased programs use the "ANSI" API since the underlying C and
C++ standard libraries only use the ANSI API. (As far as I know, nobody
has ever written fully-functional C and C++ standard libraries for
Windows, not even Microsoft.)

You might wonder why the heck I'm gluing strings together for the
arguments. These will need to be parsed (word split, etc.) by someone
else, so shouldn't I construct an argv array instead? That's not how it
works on Windows: Programs receive a flat command string and are expected
to parse it themselves following [the format specification][spec]. When
you write a C program, the C runtime does this for you to provide the
usual argv array.

This is upside down. The caller creating the process already has arguments
split into an argv array — or something like it — but Win32 requires the
caller to encode the argv array as a string following a special format so
that the recipient can immediately decode it. Why marshaling rather than
pass structured data in the first place? Why does Win32 only supply a
decoder ([`CommandLineToArgv`][argv]) and not an encoder (e.g. the missing
`ArgvToCommandLine`)? Hey, I don't make the rules; I just have to live
with them.

You can look at the original source for the details, but the summary is
that I supply my own `xstrlen()`, `xmemcpy()`, and partial Win32 command
line parser — just enough to identify the first token, even if that token
is quoted. It glues the strings together, calls `CreateProcessW`, waits
for it to exit (`WaitForSingleObject`), retrieves the exit code
(`GetExitCodeProcess`), and exits with the same status. (The stuff that
comes for free with `exec(3)`.)

This all compiles to a 4kB executable, mostly padding, which is small
enough for my purposes. These compress to an acceptable 1kB each in the
.zip file. Smaller would be nicer, but this would require at minimum a
custom linker script, and even smaller would require hand-crafted
assembly.

This lingering issue solved, w64devkit now works better than ever. The
`alias.c` source is included in the kit in case you need to make any of
your own well-behaved alias commands.


[all]: /blog/2020/09/25/
[argv]: https://docs.microsoft.com/en-us/windows/win32/api/shellapi/nf-shellapi-commandlinetoargvw
[c]: https://github.com/skeeto/w64devkit/blob/master/src/alias.c
[com]: /blog/2014/12/09/
[free]: /blog/2016/01/31/
[intro]: /blog/2020/05/15/
[mpv]: https://mpv.io/
[pe]: https://wiki.osdev.org/PE
[spec]: https://docs.microsoft.com/en-us/previous-versions/17w5ykft(v=vs.85)
[w64]: https://github.com/skeeto/w64devkit
[wsl]: /blog/2017/11/30/
