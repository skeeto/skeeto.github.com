---
title: A guide to Windows application development using w64devkit
layout: post
date: 2021-03-11T01:40:31Z
tags: [c, cpp, win32]
uuid: b04dbe3d-2e79-4afd-ad20-6ce0b232242e
excerpt_separator: <!--more-->
---

There's a trend of building services where a monolithic application is
better suited, or using JavaScript and Python then being stumped by their
troublesome deployment story. This leads to solutions like [bundling an
entire web browser][e] with an application, or using containers to
circumscribe [a sprawling dependency tree made of mystery meat][deps].

My [small development distribution][intro] for Windows, [w64devkit][w64],
is my own little way of pushing back against this trend where it affects
me most. Following in the footsteps of projects like [Handmade Hero][hh]
and [Making a Video Game from Scratch][rr], this is my guide to
no-nonsense software development using my development kit. It's an
overview of the tooling and development workflow, and I've tried not to
assume too much knowledge of the reader. Being a guide rather than manual,
it is incomplete on its own, and I link to substantial external resources
to fill in the gaps. The guide is capped with a small game I wrote
entirely using my development kit, serving as a demonstration of what
sorts of things are not only possible, but quite reasonably attainable.

<!--more-->

<video src="https://nullprogram.s3.amazonaws.com/asteroids/asteroids.mp4"
       width="600" height="600" controls>
</video>

Game repository: <https://github.com/skeeto/asteroids-demo>  
Guide to source: [Understanding Asteroids][guide]

### Initial setup

Of course you cannot use the development kit if you don't have it yet. Go
to the [releases section][rel] and download the latest release. It will be
a .zip file named `w64devkit-x.y.z.zip` where `x.y.z` is the version.

You will need to unzip the development kit before using it. Windows has
built-in support for .zip files, so you can either right-click to access
"Extract All…" or navigate into it as a folder then drag-and-drop the
`w64devkit` directory somewhere outside the .zip file. It doesn't care
where it's unzipped (aka it's "portable"), so put it where ever is
convenient: your desktop, user profile directory, a thumb drive, etc. You
can move it later if you change your mind just so long as you're not
actively running it. If you decide you don't need it anymore then delete
it.

### Entering the development environment

There is a `w64devkit.exe` in the unzipped `w64devkit` directory. This is
the easiest way to enter the development environment, and will not require
system configuration changes. This program puts the kit's programs in the
`PATH` environment variable then runs a Bourne shell — the standard unix
shell. Aside from the text editor, this is the primary interface for
developing software. In time you may even extend this environment with
your own tools.

If you want an additional "terminal" window, run `w64devkit.exe` again. If
you use it a lot, you may want to create a shortcut and even pin it to
your task bar.

Whether on Windows or unix-like systems, when you type a command into the
system shell it uses the `PATH` environment variable to locate the actual
program to run for that command. In practice, the `PATH` variable is a
concatenation of multiple directories, and the shell searches these
directories in order. On unix-like systems, `PATH` elements are separated
by colons. However, Windows uses colons to delimit drive letters, so its
`PATH` elements are separated by semicolons.

```sh
# Prepending to PATH on unix
PATH="$HOME/bin:$PATH"

# Prepending to PATH on Windows (w64devkit)
PATH="$HOME/bin;$PATH"
```

For more advanced users: Rather than use `w64devkit.exe`, you could "Edit
environment variables for your account" and manually add w64devkit's `bin`
directory to your `PATH`, making the tools generally available everywhere
on your system. If you've gone this route, you can start a Bourne shell at
any time with `sh -l`. (The `-l` option requests a login shell.)

Also borrowed from the unix world is the concept of a *home directory*,
specified by the `HOME` environment variable. By default this will be your
user profile directory, typically `C:/Users/$USER`. Login shells always
start in the home directory. This directory is often indicated by tilde
(`~`), and many programs automatically expand a leading tilde to the home
directory.

### Shell basics

The shell is a command interpreter. It's named such because [it was
originally a *shell* around the operating system kernel][kern] — the user
interface to the kernel. Your system's graphical interface — Windows
Explorer, or `Explorer.exe` — is really just a kind of shell, too. That
shell is oriented around the mouse and graphics. This is fine for some
tasks, but a keyboard-oriented command shell is far better suited for
development tasks. It's more efficient, but more importantly its features
are composable: Complex operations and processes can be [constructed
from][pipelines] simple, easy-to-understand tools. Embrace it!

In the shell you can navigate between directories with `cd`, make
directories with `mkdir`, remove files with `rm`, regular expression text
searches with `grep`, etc. Run `busybox` to see a listing of the available
standard commands. Unfortunately there are no manual pages, but you can
access basic usage information for any command with `busybox CMD --help`.

Windows' standard command shell is `cmd.exe`. Unfortunately this shell is
terrible and exists mostly for legacy compatibility. The intended
replacement is PowerShell for users who regularly use a shell. However,
PowerShell is fundamentally broken, does virtually everything incorrectly,
and manages to be even worse than `cmd.exe`. Besides, sticking to POSIX
shell conventions significantly improves build portability, and unix tool
knowledge is transferable to basically every other operating system.

Unix's standard shell was the Bourne shell, `sh`. The shells in use today
are Bourne shell clones with a superset of its features. The most popular
interactive shells are Bash and Zsh. On Linux, dash (Debian Almquist
shell) has become popular for non-interactive use (scripting). The shell
included with w64devkit is the BusyBox fork of the Almquist shell (`ash`),
closely related to dash. The Almquist shell has almost no non-interactive
features beyond the standard Bourne shell, and so as far as scripts are
concerned can be regarded as a plain Bourne shell clone. That's why I
typically refer to it by the name `sh`.

However, BusyBox's Almquist shell has interactive features much like Bash,
and Bash users should be quite comfortable. It's not just tab-completion
but a slew of Emacs-like keybindings:

* <kbd>Ctrl-r</kbd>: search backwards in history
* <kbd>Ctrl-s</kbd>: search forwards in history
* <kbd>Ctrl-p</kbd>: previous command (Up)
* <kbd>Ctrl-n</kbd>: next command (Down)
* <kbd>Ctrl-a</kbd>: cursor to the beginning of line (Home)
* <kbd>Ctrl-e</kbd>: cursor to the end of line (End)
* <kbd>Alt-b</kbd>: cursor back one word
* <kbd>Alt-f</kbd>: cursor forward one word
* <kbd>Ctrl-l</kbd>: clear the screen
* <kbd>Alt-d</kbd>: delete word after the cursor
* <kbd>Ctrl-w</kbd>: delete the word before the cursor
* <kbd>Ctrl-k</kbd>: delete to the end of the line
* <kbd>Ctrl-u</kbd>: delete to the beginning of the line
* <kbd>Ctrl-f</kbd>: cursor forward one character (Right)
* <kbd>Ctrl-b</kbd>: cursor backward one character (Left)
* <kbd>Ctrl-d</kbd>: delete character under the cursor (Delete)
* <kbd>Ctrl-h</kbd>: delete character before the cursor (Backspace)

Take special note of Ctrl-r, which is the most important and powerful
shortcut of the bunch. Frequent use is a good habit. Don't mash the up
arrow to search through the command history.

Special note for Cygwin and MSYS2 users: the shell is aware of Windows
paths and does not present a virtual unix file system scheme. This has
important consequences for scripting, both good and bad. The shell even
supports backslash as a directory separator, though you should of course
prefer forward slashes.

#### Shell customization

Login shells (`-l`) evaluate the contents of `~/.profile` on startup. This
is your chance to customize the shell configuration, such as setting
environment variables or defining aliases and functions. For instance, if
you wanted the prompt to show the working directory in green you'd set
`PS1` in your `~/.profile`:

```sh
PS1="$(printf '\x1b[33;1m\\w\x1b[0m$ ')"
```

If you find yourself using the same command sequences or set of options
again and again, you might consider putting those commands into a script,
and then installing that script somewhere on your `PATH` so that you can
run it as a new command. First make a directory to hold your scripts, say
in `~/bin`:

```sh
mkdir ~/bin
```

In `~/.profile` prepend it to your `PATH`:

```sh
PATH="$HOME/bin;$PATH"
```

If you don't want to start a fresh shell to try it out, then load the new
configuration in your current shell:

```sh
source ~/.profile
```

Suppose you keep getting the `tar` switches mixed up and you'd like to
just have an `untar` command that does the right thing. Create a file
named `untar` or `untar.sh` in `~/bin` with these contents:

```sh
#!/bin/sh
set -e
tar -xaf "$@"
```

Now a command like `untar something.tar.gz` will extract the archive
contents.

To learn more about Bourne shell scripting, the POSIX [shell command
language specification][sh] is a good reference. All of the features
listed in that document are available to your shell scripts.

### Text editing

The development kit includes the powerful and popular text editor
[Vim][vim]. It takes effort to learn, but is well worth the investment.
It's packed with features, but since you only need a small number of them
on a regular basis it's not as daunting as it might appear. Using Vim
effectively, you will write and edit text so much more quickly than
before. That includes not just code, but prose: READMEs, documentation,
etc.

(The catch: Non-modal editing will forever feel frustratingly inefficient.
That's not because you will become unpracticed at it, or even have trouble
code switching between input styles, but because you'll now be aware how
bad it is. Ignorance is bliss.)

Vim includes its own tutorial for absolute beginners which you can access
with the `vimtutor` command. It will run in the console window and guide
you through the basics in about half an hour. Do not be afraid to return
to the tutorial at any time since this is the stuff you need to know by
heart.

When it comes time to actually use Vim to write code, you can continue
writing code via the terminal interface (`vim`), or you can run the
graphical interface (`gvim`). The latter is recommended since it has some
nice quality-of-life features, but it's not strictly necessary. When
starting the GUI, put an ampersand (`&`) on the command so that it runs in
the background. For instance this brings up the editor with two files open
but leaves the shell running in the foreground so you can continue using
it while you edit:

```sh
gvim main.c Makefile &
```

Vim's defaults are good but imperfect. Before getting started with
actually editing code you should establish at least the following minimal
configuration in `~/_vimrc`. (To understand these better, use `:help` to
jump the built-in documentation.)

```vim
set hidden encoding=utf-8 shellslash
filetype plugin indent on
syntax on
```

The graphical interface defaults to a white background. Many people prefer
"dark mode" when editing code, so inverting this is simply a matter of
choosing a dark color scheme. Vim comes with a handful of color schemes,
around half of which have dark backgrounds. Use `:colorscheme` to change
it, and put it in your `~/_vimrc` to persist it.

```vim
colorscheme slate
```

The default graphical interface includes a menu bar and tool bar. There
are better ways to accomplish all these operations, none of which require
touching the mouse, so consider removing all that junk:

```vim
set guioptions=ac
```

Finally, since the development kit is oriented around C and C++, here's my
own entire Vim configuration for C which makes it obey my own style:

```
set cinoptions+=t0,l1,:0 cinkeys-=0#
```

Once you're comfortable with the basics, the best next step is to read
[*Practical Vim: Edit Text at the Speed of Thought*][pv] by Drew Neil.
It's an opinionated guide to Vim that instills good habits. If you want
something cost-free to whet your appetite, check out [*Seven habits of
effective text editing*][bm].

### Writing an application

We've established a shell and text editor. Next is the development
workflow for writing an actual application. Ultimately you will invoke a
compiler from within Vim, which will parse compiler messages and take you
directly to the parts of your source code that need attention. Before we
get that far, let's start with the basics.

The classic example is the "hello world" program, which we'll suppose is
in a file called `hello.c`:

```c
#include <stdio.h>

int main(void)
{
    puts("Hello, world!");
}
```

While this development kit provides a version of the GNU compiler, `gcc`,
this guide mostly speaks of it in terms of the generic unix C compiler
name, `cc`. Unix-like systems install `cc` as an alias for the system's
default C compiler, and w64devkit is no exception.

```sh
cc -o hello.exe hello.c
```

This command creates `hello.exe` from `hello.c`. Since this is not (yet?)
on your `PATH`, you must invoke it via a path name (i.e. the command must
include a slash), since otherwise the shell will search for it via the
`PATH` variable. Typically this means putting `./` in front of the program
name, meaning "run the program in the current directory". As a convenience
you do not need to include the `.exe` extension:

```sh
./hello
```

Unlike the `untar` shell script from before, this `hello.exe` is entirely
independent of w64devkit. You can share it with anyone running Windows and
they'll be able to execute it. There's a little bit of runtime embedded in
the executable, but the bulk of the runtime is in the operating system
itself. I want to highlight this point because *most programming languages
don't work like this*, or at least doing so is unnatural with lots of
compromises. The users of your software do not need to install a runtime
or other supporting software. They just run the executable you give them!

That executable is probably pretty small, less than 50kB — basically a
miracle by today's standards. Sure, it's hardly doing anything right now,
but you can add a whole lot more functionality without that executable
getting much bigger. In fact, it's entirely unoptimized right now and
could be even smaller. Passing the `-Os` flag tells the compiler to
optimize for size and `-s` flag tells the linker to strip out unneeded
information.

```sh
cc -Os -s -o hello.exe hello.c
```

That cuts the program down to around a third of its previous size. If
necessary you can still do even better than this, but that's outside the
scope of this guide.

So far the program could still be valid enough to compile but contain
obvious mistakes. The compiler can warn about many of these mistakes, and
so it's always worth enabling these warnings. This requires two flags:
`-Wall` ("all" warnings) and `-Wextra` (extra warnings).

```sh
cc -Wall -Wextra -o hello.exe hello.c
```

When you're working on a program, you often don't want optimization
enabled since it makes it more difficult to debug. However, some warnings
aren't fired unless optimization is enabled. Fortunately there's an
optimization level to resolve this, `-Og` (optimize for debugging).
Combine this with `-g3` to embed debug information in the program. This
will be handy later.

```sh
cc -Wall -Wextra -Og -g3 -o hello.exe hello.c
```

These are the compiler flags you typically want to enable while developing
your software. When you distribute it, you'd use either `-Os -s` (optimize
for size) or `-O3 -s` (optimize for speed).

#### Makefiles

I mentioned running the compiler from Vim. This isn't done directly but
via special build script called a Makefile. You invoke the `make` program
from Vim, which invokes the compiler as above. The simplest Makefile would
look like this, in a file literally named `Makefile`:

```makefile
hello.exe: hello.c
    cc -Wall -Wextra -Og -g3 -o hello.exe hello.c
```

This tells `make` that the file named `hello.exe` is derived from another
file called `hello.c`, and the tab-indented line is the recipe for doing
so. Running the `make` command will run the compiler command if and only
if `hello.c` is newer than `hello.exe`.

To run `make` from Vim, use the `:make` command inside Vim. It will not
only run `make` but also capture its output in an internal buffer called
the *quickfix list*. If there is any warning or error, Vim will jump to
it. Use `:cn` (next) and `:cp` (prev) to move between issues and correct
them, or `:cc` to re-display the current issue. When you're done fixing
the issues, run `:make` again to start the cycle over.

Try that now by changing the printed message and recompiling from within
Vim. Intentionally create an error (bad syntax, too many arguments, etc.)
and see what happens.

Makefiles are a powerful and conventional way to build C and C++ software.
Since the development kit includes the standard set of unix utilities,
it's very easy to write portable Makefiles that work across a variety a
operating systems and environments. Your software isn't necessarily tied
to Windows just because you're using a Windows-based development
environment. If you want to learn how Makefiles work and how to use them
effectively, read [*A Tutorial on Portable Makefiles*][make]. From here on
I'll assume you've read that tutorial.

Ultimately I'd probably write my "hello world" Makefile like so:

```makefile
.POSIX:
CC      = cc
CFLAGS  = -Wall -Wextra -Og -g3
LDFLAGS =
LDLIBS  =
EXE     = .exe

hello$(EXE): hello.c
    $(CC) $(CFLAGS) $(LDFLAGS) -o $@ hello.c $(LDLIBS)
```

When building a release, optimize for size or speed:

```sh
make CFLAGS=-Os LDFLAGS=-s
```

This is very much a Windows-first style of Makefile, but still allows it
to be comfortably used on other systems. On Linux this `make` invocation
strips away the `.exe` extension:

```sh
make EXE=
```

For a Windows-second Makefile, remove the line with `EXE = .exe`. This
allows `EXE` to come from the environment. So, for instance, I already
define the `EXE` environment variable in my w64devkit `~/.profile`:

```sh
export EXE=.exe
```

On Linux running `make` does the right thing, as does running `make` on
Windows. No special configuration required.

If my software is truly limited to Windows, I'm likely still interested in
supporting cross-compilation. A common convention for GNU toolchains is a
`CROSS` Makefile macro. For example:

```makefile
.POSIX:
CROSS   =
CC      = $(CROSS)gcc
CFLAGS  = -Wall -Wextra -Og -g3
LDFLAGS =
LDLIBS  =

hello.exe: hello.c
    $(CC) $(CFLAGS) $(LDFLAGS) -o $@ hello.c $(LDLIBS)
```

On Windows I just run `make`, but on Linux I'd set `CROSS` appropriately.

```sh
make CROSS=x86_64-w64-mingw32-
```

#### Navigating

What happens if you're working on a larger program and you need to jump to
the definition of a function, macro, or variable? It would be tedious to
use `grep` all the time to find definitions. The development kit includes
a solid implementation of `ctags` for building a *tags database* lists the
locations for various kinds of definitions, and Vim knows how to read this
database. Most often you'll want to run it recursively like so:

```sh
ctags -R
```

You can of course do this from Vim, too: `:!ctags -R`

With the cursor over an identifier, press `CTRL-]` to jump to a definition
for that name. Use `:tn` and `:tp` to move between different definitions
(e.g. when the name is overloaded). Or if you have a tag in mind rather
than a name listed in the buffer, use the `:tag` command to jump by name.
Vim maintains a tag stack and jump list for going back and forth, like the
backward and forward buttons in a browser.

#### Debugging

I had mentioned that the `-g3` option embeds extra information in the
executable. This is for debuggers, and the development kit includes the
GNU Debugger, `gdb`, to help you debug your programs. To use it, invoke
GDB on your executable:

```sh
gdb hello.exe
```

From here you can set breakpoints and such, then run the program with
`start` or `run`, then `step` through it line by line. See [*Beej's Quick
Guide to GDB*][gdb] for a guide. During development, always run your
program through GDB, and never exit GDB. See also: [*Assertions should be
more debugger-oriented*][assert].

#### Learning C and C++

So far this guide hasn't actually assumed any C knowledge. One of the best
ways to learn C is by reading the highly-regarded [*The C Programming
Language*][kr] and doing the exercises. Alternatively, cost-free options
are [*Beej's Guide to C Programming*][beej] and [*Modern C*][mc] (more
advanced). You can use the development kit to go through any of these.

I've focused on C, but everything above also applies to C++. To learn C++
[*A Tour of C++*][tour] is a safe bet.

### Demonstration

To illustrate how much you can do with nothing beyond than this 76MB
development kit, here's a taste in the form of a weekend project: an
[Asteroids Clone for Windows][repo]. That's the game in the video at the
top of this guide.

The development kit doesn't include Git so you'd need to install it
separately in order to clone the repository, but you could at least skip
that and download a .zip snapshot of the source. It has no third-party
dependencies yet it includes hardware-accelerated graphics, real-time
sound mixing, and gamepad input. Building a larger and more complex game
is much less about tooling and more about time and skill. That's what I
mean about w64devkit being [(almost) everything you need][all].


[all]: /blog/2020/09/25/
[assert]: /blog/2022/06/26/
[beej]: http://beej.us/guide/bgc/
[bm]: https://www.moolenaar.net/habits.html
[deps]: https://research.swtch.com/deps
[e]: https://deftly.net/posts/2017-06-01-measuring-the-weight-of-an-electron.html
[gdb]: https://beej.us/guide/bggdb/
[guide]: https://idle.nprescott.com/2021/understanding-asteroids.html
[hh]: https://handmadehero.org/
[intro]: /blog/2020/05/15/
[kern]: https://www.youtube.com/watch?v=tc4ROCJYbm0&t=4m57s
[kr]: https://en.wikipedia.org/wiki/The_C_Programming_Language
[make]: /blog/2017/08/20/
[mc]: https://modernc.gforge.inria.fr/
[pipelines]: https://www.youtube.com/watch?v=bKzonnwoR2I
[pv]: https://pragprog.com/titles/dnvim2/practical-vim-second-edition/
[rel]: https://github.com/skeeto/w64devkit/releases
[repo]: https://github.com/skeeto/asteroids-demo
[rr]: https://www.youtube.com/playlist?list=PLlaINRtydtNWuRfd4Ra3KeD6L9FP_tDE7
[sh]: https://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html
[tour]: https://www.stroustrup.com/tour2.html
[vim]: https://www.vim.org/
[w64]: https://github.com/skeeto/w64devkit
