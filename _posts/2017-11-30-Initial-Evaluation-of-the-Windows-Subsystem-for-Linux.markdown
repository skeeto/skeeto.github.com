---
title: Initial Evaluation of the Windows Subsystem for Linux
layout: post
date: 2017-11-30T21:03:53Z
tags: [linux, win32]
uuid: 3edd1b7d-74c3-3dab-83b6-aa07ee54460f
---

Recently I had my first experiences with the [*Windows Subsystem for
Linux*][wsl] (WSL), evaluating its potential as an environment for
getting work done. This subsystem, introduced to Windows 10 in August
2016, allows Windows to natively run x86 and x86-64 Linux binaries.
It's essentially the counterpart to Wine, which allows Linux to
natively run Windows binaries.

WSL interfaces with Linux programs only at the kernel level, servicing
system calls the same way [the Linux kernel would][raw]. The
subsystem's main job is translating Linux system calls into NT
requests. There's a [series of articles about its internals][blog] if
you're interested in learning more.

I was honestly impressed by how well this all works, especially since
Microsoft has long had an affinity for producing flimsy imitations
(Windows console, PowerShell, Arial, etc.). WSL's design allows
Microsoft to dump an Ubuntu system wholesale inside Windows — and,
more recently, other Linux distributions — bypassing a bunch of
annoying issues, particularly in regards to glibc.

WSL processes can `exec(2)` Windows binaries, which then run in under
their appropriate subsystem, similar to [binfmt][binfmt] on Linux. In
theory this nice interop should allow for *some* automation
Linux-style even for Windows' services and programs. More on that
later.

There are some notable issues, though.

### Lack of device emulation

No soundcard devices are exposed to the subsystem, so Linux programs
can't play sound. There's [a hack to talk PulseAudio][pa] with a
Windows' process that can access, but that's about it. Generally
there's not much reason to be playing media or games under WSL, but
this can be an annoyance if you're, say, [writing software that
synthesizes audio][mm].

Really, there's almost no device emulation at all and `/proc` is
pretty empty. You won't see hard drives or removable media under
`/dev`, nor will you see USB devices like webcams and
[joysticks][joy]. A lot of the useful things you might do on a Linux
system aren't available under WSL.

### No Filesystem in Userspace (FUSE)

Microsoft hasn't implemented any of the system calls for FUSE, so don't
expect to use your favorite userspace filesystems. The biggest loss for
me is [sshfs][sshfs], which I use frequently.

If FUSE *was* supported, it would be interesting to see how the rest of
Windows interacts with these mounted filesystems, if at all.

### Fragile services

Services running under WSL are flaky. The big issue is that when the
initial WSL shell process exits, all WSL processes are killed and the
entire subsystem is torn down. This includes any services that are
running. That's certainly surprising to anyone with experience running
services on any kind of unix system. This is probably the worst part
of WSL.

While systemd is the standard for Linux these days and may even be
"installed" in the WSL virtual filesystem, it's not actually running
and you can't use `systemctl` to interact with services. Services can
only be controlled the old fashioned way, and, per above, that initial
WSL console window has to remain open while services are running.

That's a bit of a damper if you're intending to spend a lot of time
remotely SSHing into your Windows 10 system. So yes, it's trivial to run
an OpenSSH server under WSL, but it won't feel like a proper system
service.

### Limited graphics support

WSL doesn't come with an X server, so you have to supply one
separately ([Xming][xming], etc.) that runs outside WSL, as a normal
Windows process. WSL processes can connect to that server (`DISPLAY`)
allowing you to run most Linux graphical software.

However, this means there's no hardware acceleration. There will be no
[GLX extensions][GLX] available. If your goal is to run the Emacs or
Vim GUIs, that's not a big deal, but it might matter if you were
interested in running a browser under WSL. It also means it's not a
suitable environment for [developing software using OpenGL][opengl].

### Filesystem woes

The filesystem manages to be both one of the smallest issues as well
as one of the biggest.

#### Filename translation

On the small issue side is filename translation. Under most Linux
filesystems — and even more broadly for unix — [a filename is just a
bytestring][case]. They're not necessarily UTF-8 or any other
particular encoding, and that's partly why filenames are
case-sensitive — the meaning of case depends on the encoding.

However, Windows uses a [pseudo-UTF-16 scheme][four] for filenames,
incompatible with bytestrings. Since WSL lives *within* a Windows'
filesystem, there must be some bijection between bytestring filenames
and pseudo-UTF-16 filenames. It will also have to reject filenames
that can't be mapped. WSL does both.

I couldn't find any formal documentation about how filename
translation works, but most of it can be reverse engineered through
experimentation. In practice, Linux filenames are [UTF-8 encoded
strings][utf8], and WSL's translation takes advantage of this.
Filenames are decoded as UTF-8 and re-encoded as UTF-16 for Windows.
Any byte that doesn't decode as valid UTF-8 is silently converted to
REPLACEMENT CHARACTER (U+FFFD), and decoding continues from the next
byte.

I wonder if there are security consequences for different filenames
silently mapping to the same underlying file.

Exercise for the reader: How is an unmatched surrogate half from
Windows translated to WSL, where it doesn't have a UTF-8 equivalent? I
haven't tried this yet.

Even for valid UTF-8, there are many bytes that most Linux filesystems
allow in filenames that Windows does not. This ranges from simple things
like ASCII backslash and colon — special components of Windows' paths —
to unusual characters like newlines, escape, and other ASCII control
characters. There are two different ways these are handled:

1. The C drive is available under `/mnt/c`, and WSL processes can access
   regular Windows files under this "mountpoint." Attempting to access
   filenames with invalid characters under this mountpoint always
   results in ENOENT: "No such file or directory."

2. Outside of `/mnt/c` is WSL territory, and Windows processes aren't
   supposed to touch these files. This allows for more freedom when
   translating filenames. REPLACEMENT CHARACTER is still used for
   invalid UTF-8 sequences, but the forbidden characters, including
   backslashes, are all permitted. They're translated to `#XXXX` where X
   is hexadecimal for the normally invalid character. For example, `a:b`
   becomes `a#003Ab`.

While WSL doesn't let you get away with all the crazy, ill-advised
filenames that Linux allows, it's still quite reasonable. Since Windows
and Linux filenames aren't entirely compatible, there's going to be some
trade-off no matter how this translation is done.

#### Filesystem performance

On the other hand, filesystem performance is abysmal, and I doubt the
subsystem is to blame. This isn't a surprise to anyone who's used
moderately-sized Git repositories on Windows, where the large numbers
of loose files brings things to a crawl. This has been a Windows issue
for years, and that's even *before* you start plugging in the
typically "security" services — virus scanners, whitelists, etc. —
that are typically present on a Windows system and make this even
worse.

To test out WSL, I went around my normal business [compiling
tools][home] and making myself at home, just as I would on Linux.
Doing nearly anything in WSL was noticably slower than doing the same
on Linux on the exact same hardware. I didn't run any benchmarks, but
I'd expect to see around an order of magnitude difference on average
for filesystem operations. Building LLVM and Clang took a couple
hours rather than the typical 20 minutes.

I don't expect this issue to get fixed anytime soon, and it's probably
always going to be a notable limitation of WSL.

### So is WSL useful?

One of my hopes for WSL appears to be unfeasible. I thought it might
be a way to avoid [porting software from POSIX to Win32][port]. I
could just supply Windows users with the same Linux binary and they'd
be fine. ~~However, WSL requires switching Windows into a special
"developer mode," putting it well out of reach of the vast majority of
users, especially considering the typical corporate computing
environment that will lock this down. In practice, WSL is only useful
to developers. I'm sure this is no accident.~~ (Developer mode is [no
longer required][mode] as of October 2017.)

Mostly I see WSL as a Cygwin killer. [Unix is my IDE][ide] and, on
Windows, Cygwin has been my preferred go to for getting a solid unix
environment for software development. Unlike WSL, Cygwin processes can
make direct Win32 calls, which is occasionally useful. But, in exchange,
WSL will overall be better equipped. It has native Linux tools,
including a better suite of debugging tools — even better than you get
in Windows itself — Valgrind, strace, and properly-working GDB (always
been flaky in Cygwin). WSL is not nearly as good as actual Linux, but
it's better than Cygwin *if* you can get access to it.


[binfmt]: https://github.com/torvalds/linux/blob/master/Documentation/admin-guide/binfmt-misc.rst
[blog]: https://blogs.msdn.microsoft.com/wsl/
[four]: /blog/2016/06/13/
[joy]: /blog/2016/11/05/
[mm]: /blog/2017/11/03/
[opengl]: /blog/2015/06/06/
[pa]: https://trzeci.eu/configure-graphic-and-sound-on-wsl/
[sshfs]: https://github.com/libfuse/sshfs
[wine]: https://www.winehq.org/
[wsl]: https://blogs.msdn.microsoft.com/wsl/2016/04/22/windows-subsystem-for-linux-overview/
[xming]: https://sourceforge.net/projects/xming/
[raw]: /blog/2015/05/15/
[case]: http://yarchive.net/comp/linux/case_insensitive_filenames.html
[utf8]: /blog/2017/10/06/
[glx]: https://en.wikipedia.org/wiki/GLX
[home]: /blog/2017/06/19/
[ide]: https://sanctum.geek.nz/arabesque/series/unix-as-ide/
[port]: /blog/2017/03/30/
[mode]: https://blogs.msdn.microsoft.com/commandline/2017/10/11/whats-new-in-wsl-in-windows-10-fall-creators-update/
