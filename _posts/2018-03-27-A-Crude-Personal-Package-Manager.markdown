---
title: A Crude Personal Package Manager
layout: post
date: 2018-03-27T02:10:35Z
tags: [c, posix, linux, bsd]
uuid: b100f50f-c8f8-3a08-e149-a04b2308226b
---

For the past couple of months I've been using a custom package manager
to manage a handful of software packages within various unix-like
environments. Packages are [installed in my home directory][home] under
`~/.local/bin`, and the package manager itself is just a 110 line Bourne
shell script. It's is not intended to replace the system's package
manager but, instead, compliment it in some cases where I need more
flexibility. I use it to run custom versions of specific pieces of
software — newer or older than the system-installed versions, or with my
own patches and modifications — without interfering with the rest of
system, and without a need for root access. It's worked out *really*
well so far and I expect to continue making heavy use of it in the
future.

It's so simple that I haven't even bothered putting the script in its
own repository. It sits unadorned within my dotfiles repository with the
name *qpkg* ("quick package"):

* <https://github.com/skeeto/dotfiles/blob/master/bin/qpkg>

Sitting alongside my dotfiles means it's always there when I need it,
just as if it was a built-in command.

I say it's crude because its "install" (`-I`) procedure is little more
than a wrapper around tar. It doesn't invoke libtool after installing a
library, and there's no post-install script — or `postinst` as Debian
calls it. It doesn't check for conflicts between packages, though
there's a command for doing so manually ahead of time. It doesn't manage
dependencies, nor even have them as a concept. That's all on the user to
screw up.

In other words, it doesn't attempt solve most of the hard problems
tackled by package managers… *except* for three important issues:

1. It provides a clean, guaranteed-to-work uninstall procedure. Some
   Makefiles *do* have a token "uninstall" target, but it's often
   unreliable.

2. Unlike blindly using a Makefile "install" target, I can check for
   conflicts *before* installing the software. I'll know if and how a
   package clobbers an already-installed package, and I can manage, or
   ignore, that conflict manually as needed.

3. It produces a compact, reusable package file that I can reinstall
   later, even on a different machine (with a couple of caveats). I
   don't need to keep around the original source and build directories
   should I want to install or uninstall later. I can also rapidly
   switch back and forth between different builds of the same software.

The first caveat is that the package will be configured for exactly my
own home directory, so I usually can't share it with other users, or
install it on machines where I have a different home directory. Though I
could still create packages for different installation prefixes.

The second caveat is that some builds tailor themselves by default to
the host (e.g. `-march=native`). If care isn't taken, those packages may
not be very portable. This is more common than I had expected and has
mildly annoyed me.

### Birth of a package manager

While the package manager is new, I've been building and installing
software in my home directory for years. I'd follow the normal process
of setting the install *prefix* to `$HOME/.local`, running the build,
and then letting the "install" target do its thing.

    $ tar xzf name-version.tar.gz
    $ cd name-version/
    $ ./configure --prefix=$HOME/.local
    $ make -j$(nproc)
    $ make install

This worked well enough for years. However, I've come to rely a lot on
this technique, and I'm using it for increasingly sophisticated
purposes, such as building custom cross-compiler toolchains.

A common difficulty has been handling the release of new versions of
software. I'd like to upgrade to the new version, but lack a way to
cleanly uninstall the previous version. Simply clobbering the old
version by installing it on top *usually* works. Occasionally it
wouldn't, and I'd have to blow away `~/.local` and start all over again.
With more and more software installed in my home directory, restarting
has become more and more of a chore that I'd like to avoid.

What I needed was a way to track exactly which files were installed so
that I could remove them later when I needed to uninstall. Fortunately
there's a widely-used convention for exactly this purpose: `DESTDIR`.

It's expected that when a Makefile provides an "install" target, it
prefixes the installation path with the `DESTDIR` macro, which is
assigned to the empty string by default. This allows the user to install
the software to a temporary location for the purposes of packaging.
Unlike the installation prefix (`--prefix`) configured before the build
began, the software is not expected to function properly when run in the
`DESTDIR` location.

    $ DESTDIR=_destdir
    $ mkdir $DESTDIR
    $ make DESTDIR=$DESTDIR install

A different tool will used to copy these files into place and actually
install it. This tool can track what files were installed, allowing them
to be removed later when uninstalling. My package manager uses the tar
program for both purposes. First it creates a package by packing up the
`DESTDIR` (at the root of the actual install prefix):

    $ tar czf package.tgz -C $DESTDIR$HOME/.local .

So a package is nothing more than a gzipped tarball. To install, it
unpacks the tarball in `~/.local`.

    $ cd $HOME/.local
    $ tar xzf ~/package.tgz

But how does it uninstall a package? It didn't keep track of what was
installed. Easy! The tarball itself contains the package list, and it's
printed with tar's `t` mode.

    cd $HOME/.local
    for file in $(tar tzf package.tgz | grep -v '/$'); do
        rm -f "$file"
    done

I'm using `grep` to skip directories, which are conveniently listed with
a trailing slash. Note that in the example above, there are a couple of
issues with file names containing whitespace. If the file contains a
space character, it will word split incorrectly in the `for` loop. A
Makefile couldn't handle such a file in the first place, but, in case
it's still necessary, my package manager sets `IFS` to just a newline.

If the file name contains a newline, then my package manager relies on
[a cosmic ray striking just the right bit at just the right
instant][squat] to make it all work out, because no version of tar can
unambiguously print such file names. Crossing your fingers during this
process may help.

### Commands

There are five commands, each assigned to a capital letter: `-B`, `-C`,
`-I`, `-V`,  and `-U`. It's an interface pattern inspired by [Ted
Unangst's signify][tedu] (see [`signify(1)`][man]). I also used this
pattern with [Blowpipe][bp] and, in retrospect, wish I had also used
with [Enchive][enchive].

#### Build (`-B`)

Unlike the other three commands, the "build" command isn't essential,
and is just for convenience. It assumes the build uses an Autoconfg-like
configure script and runs it automatically, followed by `make` with the
appropriate `-j` (jobs) option. It automatically sets the `--prefix`
argument when running the configure script.

If the build uses something other and an Autoconf-like configure script,
such as CMake, then you can't use the "build" command and must perform
the build yourself. For example, I must do this when building LLVM and
Clang.

Before using the "build" command, the package must first be unpacked and
patched if necessary. Then the package manager can take over to run the
build.

    $ tar xzf name-version.tar.gz
    $ cd name-version/
    $ patch -p1 < ../0001.patch
    $ patch -p1 < ../0002.patch
    $ patch -p1 < ../0003.patch
    $ cd ..
    $ mkdir build
    $ cd build/
    $ qpkg -B ../name-version/

In this example I'm doing an out-of-source build by invoking the
configure script from a different directory. Did you know Autoconf
scripts support this? I didn't know until recently! Unfortunately some
hand-written Autoconf-like scripts don't, though this will
be immediately obvious.

Once `qpkg` returns, the program will be fully built — or stuck on a
build error if you're unlucky. If you need to pass custom configure
options, just tack them on the `qpkg` command:

    $ qpkg -B ../name-version/ --without-libxml2 --with-ncurses

Since the second and third steps — creating the build directory and
moving into it — is so common, there's an optional switch for it: `-d`.
This option's argument is the build directory. `qpkg` creates that
directory and runs the build inside it. In practice I just use "x" for
the build directory since it's so quick to add "dx" to the command.

    $ tar xzf name-version.tar.gz
    $ qpkg -Bdx ../name-version/

With the software compiled, the next step is creating the package.

#### Create (`-C`)

The "create" command creates the `DESTDIR` (`_destdir` in the working
directory) and runs the "install" Makefile target to fill it with files.
Continuing with the example above and its `x/` build directory:

    $ qpkg -Cdx name

Where "name" is the name of the package, without any file name
extension. Like with "build", extra arguments after the package name are
passed to `make` in case there needs to be any additional tweaking.

When the "create" command finishes, there will be new package named
`name.tgz` in the working directory. At this point the source and build
directories are no longer needed, assuming everything went fine.

    $ rm -rf name-version/
    $ rm -rf x/

This package is ready to install, though you may want to verify it
first.

#### Verify (`-V`)

The "verify" command checks for collisions against installed packages.
It works like uninstallation, but rather than deleting files, it checks
if any of the files already exist. If they do, it means there's a
conflict with an existing package. These file names are printed.

    $ qpkg -V name.tgz

The most common conflict I've seen is in the info index (`info/dir`)
file, which is safe to ignore since I don't care about it.

If the package has already been installed, there will of course be tons
of conflicts. This is the easiest way to check if a package has been
installed.

#### Install (`-I`)

The "install" command is just the dumb `tar xzf` explained above. It
will clobber anything in its way without warning, which is why, if that
matters, "verify" should be used first.

    $ qpkg -I name.tgz

When `qpkg` returns, the package has been installed and is probably
ready to go. A lot of packages complain that you need to run libtool to
finalize an installation, but I've never had a problem skipping it. This
dumb unpacking generally works fine.

#### Uninstall (`-U`)

Obviously the last command is "uninstall". As explained above, this
needs the original package that was given to the "install" command.

    $ qpkg -U name.tgz

Just as "install" is dumb, so is "uninstall," blindly deleting anything
listed in the tarball. One thing I like about dumb tools is that there
are no surprises.

I typically suffix the package name with the version number to help keep
the packages organized. When upgrading to a new version of a piece of
software, I build the new package, which, thanks to the version suffix,
will have a distinct name. Then I uninstall the old package, and,
finally, install the new one in its place. So far I've been keeping the
old package around in case I still need it, though I could always
rebuild it in a pinch.

### Package by accumulation

Building a GCC cross-compiler toolchain is a tricky case that doesn't
fit so well with the build, create, and install process illustrated
above. It would be nice for the cross-compiler to be a single, big
package, but due to the way it's built, it would need to be five or so
packages, a couple of which will conflict (one being a subset of
another):

1. binutils
2. C headers
3. core GCC
4. C runtime
5. rest of GCC

Each step needs to be installed before the next step will work. (I don't
even want to think about cross-compiling a cross-compiler.)

To deal with this, I added a "keep" (`-k`) option that leaves the
`DESTDIR` around after creating the package. To keep things tidy, the
intermediate packages exist and are installed, but the final, big
cross-compiler package *accumulates* into the `DESTDIR`. The final
package at the end is actually the whole cross compiler in one package,
a superset of them all.

Complicated situations like these are where I can really understand the
value of Debian's [fakeroot][fakeroot] tool.

### My use case, and an alternative

The role filled by my package manager is actually pretty well suited for
[pkgsrc][pkgsrc], which is NetBSD's ports system made available to other
unix-like systems. However, I just need something really lightweight
that gives me absolute control — even more than I get with pkgsrc — in
the dozen or so cases where I *really* need it.

All I need is a standard C toolchain in a unix-like environment (even a
really old one), the source tarballs for the software I need, my 110
line shell script package manager, and one to two cans of elbow grease.
From there I can bootstrap everything I might need without root access,
even [in a disaster][vim]. If the software I need isn't written in C, it
can ultimately get bootstrapped from some crusty old C compiler, which
might even involve building some newer C compilers in between. After a
certain point it's C all the way down.


[bp]: /blog/2017/09/15/
[enchive]: /blog/2017/03/12/
[fakeroot]: https://wiki.debian.org/FakeRoot
[home]: /blog/2017/06/19/
[make]: /blog/2017/08/20/
[man]: https://man.openbsd.org/signify.1
[pkgsrc]: https://www.pkgsrc.org/
[squat]: http://dinaburg.org/bitsquatting.html
[src]: https://github.com/skeeto/dotfiles/blob/master/bin/qpkg
[tedu]: https://www.openbsd.org/papers/bsdcan-signify.html
[vim]: /blog/2017/04/01/
