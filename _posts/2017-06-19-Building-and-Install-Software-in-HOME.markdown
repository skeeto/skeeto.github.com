---
title: Building and Installing Software in $HOME
layout: post
date: 2017-06-19T02:34:39Z
tags: [linux, tutorial, debian, c, cpp]
uuid: ae490550-a3b8-3b8f-4338-c2aba7306c8f
---

For more than 5 years now I've kept a private "root" filesystem within
my home directory under `$HOME/.local/`. Within are the standard
`/usr` directories, such as `bin/`, `include/`, `lib/`, etc.,
containing my own software, libraries, and man pages. These are
first-class citizens, indistinguishable from the system-installed
programs and libraries. With one exception (setuid programs), none of
this requires root privileges.

Installing software in $HOME serves two important purposes, both of
which are indispensable to me on a regular basis.

* **No root access**: Sometimes I'm using a system administered by
  someone else, and I don't have root access.

This prevents me from installing packaged software myself through the
system's package manager. Building and installing the software myself in
my home directory, without involvement from the system administrator,
neatly works around this issue. As a software developer, it's already
perfectly normal for me to build and run custom software, and this is
just an extension of that behavior.

In the most desperate situation, all I need from the sysadmin is a
decent C compiler and at least a minimal POSIX environment. I can
[bootstrap anything I might need][needle], both libraries and
programs, including a better C compiler along the way. This is one
major strength of open source software.

I have noticed one alarming trend: Both GCC (since 4.8) and Clang are
written in C++, so it's becoming less and less reasonable to bootstrap
a C++ compiler from a C compiler, or even from a C++ compiler that's
more than a few years old. So you may also need your sysadmin to
supply a fairly recent C++ compiler if you want to bootstrap an
environment that includes C++. I've had to avoid some C++ software
(such as CMake) for this reason.

* **Custom software builds**: Even if I *am* root, I may still want to
  install software not available through the package manager, a version
  not available in the package manager, or a version with custom
  patches.

In theory this is what `/usr/local` is all about. It's typically the
location for software not managed by the system's package manager.
However, I think it's cleaner to put this in `$HOME/.local`, so long
as other system users don't need it.

For example, I have an installation of each version of Emacs between
24.3 (the oldest version worth supporting) through the latest stable
release, each suffixed with its version number, under `$HOME/.local`.
This is useful for quickly running a test suite under different
releases.

    $ git clone https://github.com/skeeto/elfeed
    $ cd elfeed/
    $ make EMACS=emacs24.3 clean test
    ...
    $ make EMACS=emacs25.2 clean test
    ...

Another example is NetHack, which I prefer to play with a couple of
custom patches ([Menucolors][nhmc], [wchar][nhwchar]). The install to
`$HOME/.local` [is also captured as a patch][nhhome].

    $ tar xzf nethack-343-src.tar.gz
    $ cd nethack-3.4.3/
    $ patch -p1 < ~/nh343-menucolor.diff
    $ patch -p1 < ~/nh343-wchar.diff
    $ patch -p1 < ~/nh343-home-install.diff
    $ sh sys/unix/setup.sh
    $ make -j$(nproc) install

Normally NetHack wants to be setuid (e.g. run as the "games" user) in
order to restrict access to high scores, saves, and bones — saved levels
where a player died, to be inserted randomly into other players' games.
This prevents cheating, but requires root to set up. Fortunately, when I
install NetHack in my home directory, this isn't a feature I actually
care about, so I can ignore it.

[Mutt][mutt] is in a similar situation, since it wants to install a
special setgid program (`mutt_dotlock`) that synchronizes mailbox
access. All MUAs need something like this.

Everything described below is relevant to basically any modern
unix-like system: Linux, BSD, etc. I personally install software in
$HOME across a variety of systems and, fortunately, it mostly works
the same way everywhere. This is probably in large part due to
everyone standardizing around the GCC and GNU binutils interfaces,
even if the system compiler is actually LLVM/Clang.

### Configuring for $HOME installs

Out of the box, installing things in `$HOME/.local` won't do anything
useful. You need to set up some environment variables in your shell
configuration (i.e. `.profile`, `.bashrc`, etc.) to tell various
programs, such as your shell, about it. The most obvious variable is
$PATH:

~~~sh
export PATH=$HOME/.local/bin:$PATH
~~~

Notice I put it in the front of the list. This is because I want my
home directory programs to override system programs with the same
name. For what other reason would I install a program with the same
name if not to override the system program?

In the simplest situation this is good enough, but in practice you'll
probably need to set a few more things. If you install libraries in
your home directory and expect to use them just as if they were
installed on the system, you'll need to tell the compiler where else
to look for those headers and libraries, both for C and C++.

~~~sh
export C_INCLUDE_PATH=$HOME/.local/include
export CPLUS_INCLUDE_PATH=$HOME/.local/include
export LIBRARY_PATH=$HOME/.local/lib
~~~

The first two are like the `-I` compiler option and the third is like
`-L` linker option, except you *usually* won't need to use them
explicitly. Unfortunately [`LIBRARY_PATH` doesn't override the system
library paths][lpath], so in some cases, you will need to explicitly set
`-L`. Otherwise you will still end up linking against the system library
rather than the custom packaged version. I really wish GCC and Clang
didn't behave this way.

Some software uses `pkg-config` to determine its compiler and linker
flags, and your home directory will contain some of the needed
information. So set that up too:

~~~sh
export PKG_CONFIG_PATH=$HOME/.local/lib/pkgconfig
~~~

#### Run-time linker

Finally, when you install libraries in your home directory, the run-time
dynamic linker will need to know where to find them. There are three
ways to deal with this:

1. The [crude, easy way][env]: `LD_LIBRARY_PATH`.
2. The elegant, difficult way: ELF runpath.
3. Screw it, just statically link the bugger. (Not always possible.)

For the crude way, point the run-time linker at your `lib/` and you're
done:

~~~sh
export LD_LIBRARY_PATH=$HOME/.local/lib
~~~

However, this is like using a shotgun to kill a fly. If you install a
library in your home directory that is also installed on the system,
and then run a system program, it may be linked against *your* library
rather than the library installed on the system as was originally
intended. This could have detrimental effects.

The precision method is to set the ELF "runpath" value. It's like a
per-binary `LD_LIBRARY_PATH`. The run-time linker uses this path first
in its search for libraries, and it will only have an effect on that
particular program/library. This also applies to `dlopen()`.

Some software will configure the runpath by default in their build
system, but often you need to configure this yourself. The simplest way
is to set the `LD_RUN_PATH` environment variable when building software.
Another option is to manually pass `-rpath` options to the linker via
`LDFLAGS`. It's used directly like this:

    $ gcc -Wl,-rpath=$HOME/.local/lib -o foo bar.o baz.o -lquux

Verify with `readelf`:

    $ readelf -d foo | grep runpath
    Library runpath: [/home/username/.local/lib]

ELF supports a special `$ORIGIN` "variable" set to the binary's
location. This allows the program and associated libraries to be
installed anywhere without changes, so long as they have the same
relative position to each other . (Note the quotes to prevent shell
interpolation.)

    $ gcc -Wl,-rpath='$ORIGIN/../lib' -o foo bar.o baz.o -lquux

There is one situation where `runpath` won't work: when you want a
system-installed program to find a home directory library with
`dlopen()` — e.g. as an extension to that program. You either need to
ensure it uses a relative or absolute path (i.e. the argument to
`dlopen()` contains a slash) or you must use `LD_LIBRARY_PATH`.

Personally, I always use the [Worse is Better][wib] `LD_LIBRARY_PATH`
shotgun. Occasionally it's caused some annoying issues, but the vast
majority of the time it gets the job done with little fuss. This is
just my personal development environment, after all, not a production
server.

#### Manual pages

Another potentially tricky issue is man pages. When a program or
library installs a man page in your home directory, it would certainly
be nice to access it with `man <topic>` just like it was installed on
the system. Fortunately, Debian and Debian-derived systems, using a
mechanism I haven't yet figured out, discover home directory man pages
automatically without any assistance. No configuration needed.

It's more complicated on other systems, such as the BSDs. You'll need to
set the `MANPATH` variable to include `$HOME/.local/share/man`. It's
unset by default and it overrides the system settings, which means you
need to manually include the system paths. The `manpath` program can
help with this ... if it's available.

~~~sh
export MANPATH=$HOME/.local/share/man:$(manpath)
~~~

I haven't figured out a portable way to deal with this issue, so I
mostly ignore it.

### How to install software in $HOME

While I've [poo-pooed autoconf][pp] in the past, the standard
`configure` script usually makes it trivial to build and install
software in $HOME. The key ingredient is the `--prefix` option:

    $ tar xzf name-version.tar.gz
    $ cd name-version/
    $ ./configure --prefix=$HOME/.local
    $ make -j$(nproc)
    $ make install

Most of the time it's that simple! If you're linking against your own
libraries and want to use `runpath`, it's a little more complicated:

    $ ./configure --prefix=$HOME/.local \
                  LDFLAGS="-Wl,-rpath=$HOME/.local/lib"

For [CMake][cmake], there's `CMAKE_INSTALL_PREFIX`:

    $ cmake -DCMAKE_INSTALL_PREFIX=$HOME/.local ..

The CMake builds I've seen use ELF runpath by default, and no further
configuration may be required to make that work. I'm sure that's not
always the case, though.

Some software is just a single, static, standalone binary with
[everything baked in][bake]. It doesn't need to be given a prefix, and
installation is as simple as copying the binary into place. For example,
[Enchive][enchive] works like this:

    $ git clone https://github.com/skeeto/enchive
    $ cd enchive/
    $ make
    $ cp enchive ~/.local/bin

Some software uses its own unique configuration interface. I can respect
that, but it does add some friction for users who now have something
additional and non-transferable to learn. I demonstrated a NetHack build
above, which has a configuration much more involved than it really
should be. Another example is LuaJIT, which uses `make` variables that
must be provided consistently on every invocation:

    $ tar xzf LuaJIT-2.0.5.tar.gz
    $ cd LuaJIT-2.0.5/
    $ make -j$(nproc) PREFIX=$HOME/.local
    $ make PREFIX=$HOME/.local install

(You *can* use the "install" target to both build and install, but I
wanted to illustrate the repetition of `PREFIX`.)

Some libraries aren't so smart about `pkg-config` and need some
handholding — for example, [ncurses][ncurses]. I mention it because
it's required for both Vim and Emacs, among many others, so I'm often
building it myself. It ignores `--prefix` and needs to be told a
second time where to install things:

    $ ./configure --prefix=$HOME/.local \
                  --enable-pc-files \
                  --with-pkg-config-libdir=$PKG_CONFIG_PATH

Another issue is that a whole lot of software has been hardcoded for
ncurses 5.x (i.e. `ncurses5-config`), and it requires hacks/patching
to make it behave properly with ncurses 6.x. I've avoided ncurses 6.x
for this reason.

### Learning through experience

I could go on and on like this, discussing the quirks for the various
libraries and programs that I use. Over the years I've gotten used to
many of these issues, committing the solutions to memory.
Unfortunately, even within the same version of a piece of software,
the quirks can change [between major operating system
releases][debian], so I'm continuously learning my way around new
issues. It's really given me an appreciation for all the hard work
that package maintainers put into customizing and maintaining software
builds to [fit properly into a larger ecosystem][maint].


[bake]: /blog/2016/11/15/
[cmake]: https://cmake.org/
[debian]: https://www.debian.org/News/2017/20170617.en.html
[enchive]: https://github.com/skeeto/enchive
[env]: https://web.archive.org/web/20090312014334/http://blogs.sun.com/rie/entry/tt_ld_library_path_tt
[lpath]: https://stackoverflow.com/a/29100649
[maint]: https://www.debian.org/doc/manuals/maint-guide/
[mutt]: /blog/2017/06/15/
[ncurses]: https://www.gnu.org/software/ncurses/
[needle]: /blog/2016/11/17/
[nhhome]: https://gist.github.com/skeeto/5cb9d5e774ce62655aff3507cb806981
[nhmc]: https://bilious.alt.org/?11
[nhwchar]: https://gist.github.com/skeeto/11fed852dbfe9889a5fce80e9f6576ac
[pp]: /blog/2017/03/30/
[wib]: https://www.jwz.org/doc/worse-is-better.html
