---
title: A Makefile for Emacs Packages
layout: post
date: 2020-01-22T02:54:41Z
tags: [emacs, elisp]
uuid: 2e138ef3-bc68-4115-bb84-af260db641c0
---

Each of my Emacs packages has a Makefile to byte-compile all source
files, run the tests, build a package file, and, in some cases, run the
package in an interactive, temporary, isolated Emacs instance. These
[portable Makefiles][make] have a similar structure and follow the same
conventions. It would require more thought and feedback before I'd try
to make it a *standard*, but these are conventions I'd like to see in
other package Makefiles.

Here's an incomplete list of examples:

* <https://github.com/skeeto/bitpack/blob/master/Makefile>
* <https://github.com/skeeto/cplx/blob/master/Makefile>
* <https://github.com/skeeto/devdocs-lookup/blob/master/Makefile>
* <https://github.com/skeeto/elfeed/blob/master/Makefile>
* <https://github.com/skeeto/emacs-aio/blob/master/Makefile>
* <https://github.com/skeeto/emacs-bencode/blob/master/Makefile>
* <https://github.com/skeeto/emacs-memoize/blob/master/Makefile>
* <https://github.com/skeeto/emacs-web-server/blob/master/Makefile>
* <https://github.com/skeeto/impatient-mode/blob/master/Makefile>
* <https://github.com/skeeto/lcg128/blob/master/Makefile>
* <https://github.com/skeeto/nasm-mode/blob/master/Makefile>
* <https://github.com/skeeto/skewer-mode/blob/master/Makefile>
* <https://github.com/skeeto/x86-lookup/blob/master/Makefile>

You should make a habit of compiling your Emacs Lisp files even if you
don't think you need the performance. The byte-compiler, while
[dumb][dumb], does [static analysis][lex] and may spot bugs and other
issues early.

First things first: Every portable Makefile starts with a special
target, `.POSIX`, to request standard behavior. This is followed by
macro definitions. When compiling a C program, the `CC` macro is the
name of the compiler. Analogously, when compiling Emacs packages the
`EMACS` macro is the name of the Emacs program.

```make
.POSIX:
EMACS = emacs
```

Users can now override the macro to specify alternate Emacs binaries. I
use this all the time to test my packages under different versions of
Emacs.

    $ make clean
    $ make EMACS=emacs-24.3 check
    $ make clean
    $ make EMACS=emacs-25.1 check

Note: It's common to use `?=` assignment here, but that is both
non-standard and unnecessary. If you want to override macro definitions
from the environment, use the `-e` option:

    $ export EMACS=emacs-24.3
    $ make -e

The first non-special target in the Makefile is the default target. For
Emacs packages, this target should byte-compile all the source files,
including tests. List the byte-compiled file names as the target
dependencies:

```make
compile: foo.elc foo-test.elc
```

Now for the tedious part: Define the dependencies between your different
source files. It would be nice to automate this part somehow, but
fortunately most packages just aren't that complicated. You do not need
to list trivial dependencies — i.e. mapping each .el file to its .elc
file — since make will figure that out on its own.

Since `foo-test.elc` relies on `foo.elc` — it's testing this file after
all — the relationship must be indicated to make. For single file
packages (one package file, one test file), this is all that's needed:

```make
foo-test.elc: foo.elc
```

I call my testing targets "check" and this target must depend on the
byte-compiled files containing tests. It will transiently depend on the
other package source files because of the previous section.

```make
check: foo-test.elc
    $(EMACS) -Q --batch -L . -l foo-test.elc -f ert-run-tests-batch
```

The `-Q` option runs Emacs with "minimum customizations." The `-L .`
option puts the current directory in the load path so that `(require
'foo`) will work. Finally it loads the file containing the tests and
instructs ERT to run all defined tests.

A good build can clean up after itself:

```make
clean:
    rm -f foo.elc foo-test.elc
```

Finally we need one more thing to tie it all together: an inference rule
to teach make how to compile .elc files from .el files.

```make
.SUFFIXES: .el .elc
.el.elc:
    $(EMACS) -Q --batch -L . -f batch-byte-compile $<
```

This is similar to the "check" target, but compiles a source file
instead of running tests.

For simple, single source file packages, this is all you need!

### Complex packages

My most complex package is Elfeed which has 10 source files and 4 test
files. It also includes a target to build a package file, which I would
upload to Marmalade when it was still functioning. I did a few extra
things to keep this tidy.

First, I define the package version in the Makefile:

```make
VERSION = 1.2.3
```

It would be nice to grab this information from a reliable place (Git
tag, source file, etc.), but I never found a reliable and satisfactory
way to do this. Simple wins.

To avoid repeating myself, I list the source files in a macro as well:

```make
EL   = foo-a.el foo-b.el foo-c.el
DOC  = README.md
TEST = foo-test.el
```

These will still need to have all their interdependencies individually
defined for make. For example, if C depends on both A and B, but neither
A nor B depend on each other, this is all you'd need:

```make
foo-c.elc: foo-a.elc foo-b.elc
```

Done correctly you can perform parallel builds with the non-standard but
common `-j` make option. This is pretty nice since Emacs can't do
parallel builds itself.

I use the file list macros in the "compile" and "check" targets:

```make
compile: $(EL:.el=.elc) $(TEST:.el=.elc)
test: $(TEST:.el=.elc)
```

The "package" target copies everything under a directory and tars it up.
The directory is removed first, if it exists, so that any potenntial
leftover garbage from doesn't get included.

```make
package: foo-$(VERSION).tar
foo-$(VERSION).tar: $(EL) $(DOC)
    rm -rf foo-$(VERSION)/
    mkdir foo-$(VERSION)/
    cp $(EL) $(DOC) foo-$(VERSION)/
    tar cf $@ foo-$(VERSION)/
    rm -rf foo-$(VERSION)/
```

In Elfeed, the target to test in an interactive, temporary Emacs
instance is called "virtual". In Skewer it's called "run". The name of
the target and the specific rules will depend on the package, should you
even want this target at all. It's handy to have the option test without
my own configuration contaminating Emacs, and vice versa. When people
report issues, I can also direct them to reproduce their issue in the
clean environment.

Here's what a simple "run" target might look like:

```make
run: $(EL:.el=.elc)
    $(EMACS) -Q -L . -l foo-c.elc -f foo-mode
```

Make is not really designed to run interactive programs like this, but
it works in practice.

### Dependencies

What about packages with dependencies? I've used [Cask][cask] in the
past but was never satisfied, especially when integrating it into a
Makefile. So, again, I've opted for the dumb-but-reliable option:
request that dependencies are cloned in adjacent directories matching
the dependency's package name. For example, the [EmacSQL][sql] Makefile
header:

```make
# Clone the dependencies of this package in sibling directories:
#     $ git clone https://github.com/cbbrowne/pg.el ../pg
```

I also define a new "linker flags" macro, `LDFLAGS`. Like with `EMACS`,
this lets users override it if needed:

```make
LDFLAGS = -L ../pg
```

Everywhere I use `-L .` I also include `$(LDFLAGS)`. For example, in the
inference rule:

```make
.SUFFIXES: .el .elc
.el.elc:
    $(EMACS) -Q --batch -L . $(LDFLAGS) -f batch-byte-compile $<
```

If the dependencies follow these conventions, then these can also be
compiled in a recursive way with little effort:

    $ make -C ../pg

I'm not completely satisfied with this solution, particularly since it's
an odd burden on anyone using the Makefile, but it's worked well enough
for my needs. This is when I wish Emacs had [distributed package
management][pkg].


[cask]: https://github.com/cask/cask
[dumb]: /blog/2019/02/24/
[lex]: /blog/2016/12/22/
[make]: /blog/2017/08/20/
[pkg]: /blog/2020/01/21/#package-management
[sql]: /blog/2014/02/06/
