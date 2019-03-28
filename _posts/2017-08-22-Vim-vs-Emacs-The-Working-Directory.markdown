---
title: "Vim vs. Emacs: the Working Directory"
layout: post
date: 2017-08-22T04:51:36Z
tags: [vim, emacs]
uuid: f27469f8-4731-35b5-1c55-4bbeb200fcad
---

Vim and Emacs have different internals models for the current working
directory, and these models influence the overall workflow for each
editor. They decide how files are opened, how shell commands are
executed, and how the build system is operated. These effects even reach
outside the editor to influence the overall structure of the project
being edited.

In the traditional unix model, which was [eventually adopted][w32]
everywhere else, each process has a particular working directory
tracked by the operating system. When a process makes a request to the
operating system using a relative path — a path that doesn't begin
with a slash — the operating system uses the process' working
directory to convert the path into an absolute path. When a process
forks, its child starts in the same directory. A process can change
its working directory at any time using [`chdir(2)`][chdir], though
most programs never need to do it. The most obvious way this system
call is exposed to regular users is through the shell's built-in `cd`
command.

Vim's spiritual heritage is obviously rooted in vi, one of the classic
unix text editors, and the [most elaborate text editor standardized by
POSIX][vi]. Like vi, Vim closely follows the unix model for working
directories. At any given time Vim has exactly one working directory.
Shell commands that are run within Vim will start in Vim's working
directory. Like a shell, the `cd` ex command changes and queries Vim's
working directory.

Emacs eschews this model and instead each buffer has its own working
directory tracked using a buffer-local variable, `default-directory`.
Emacs internally simulates working directories for its buffers like an
operating system, resolving absolute paths itself, giving credence to
the idea that Emacs is an operating system ("lacking only a decent
editor"). Perhaps this model comes from ye olde lisp machines?

In contrast, Emacs' `M-x cd` command manipulates the local variable
and has no effect on the Emacs process' working directory. In fact,
Emacs completely hides its operating system working directory from
Emacs Lisp. This can cause some trouble if that hidden working
directory happens to be sitting on filesystem you'd like to unmount.

Vim can be configured to simulate Emacs' model with its `autochdir`
option. When set, Vim will literally `chdir(2)` each time the user
changes buffers, switches windows, etc. To the user, this feels just
like Emacs' model, but this is just a convenience, and the core
working directory model is still the same.

### Single instance editors

For most of my Emacs career, I've stuck to running a single,
long-lived Emacs instance no matter how many different tasks I'm
touching simultaneously. I start the Emacs daemon shortly after
logging in, and it continues running until I log out — typically only
when the machine is shut down. It's common to have multiple Emacs
windows (frames) for different tasks, but they're all bound to the
same daemon process.

While [with care][up] it's possible to have a complex, rich Emacs
configuration that doesn't significantly impact Emacs' startup time, the
general consensus is that Emacs is slow to start. But since it has a
really solid daemon, this doesn't matter: hardcore Emacs users only ever
start Emacs occasionally. The rest of the time they're launching
`emacsclient` and connecting to the daemon. Outside of system
administration, it's the most natural way to use Emacs.

The case isn't so clear for Vim. Vim is so fast that many users fire
it up on demand and exit when they've finished the immediate task. At
the other end of the spectrum, others [advocate using a single
instance of Vim][one] like running a single Emacs daemon. In [my
initial dive into Vim][six], I tried the single-instance, Emacs way of
doing things. I set `autochdir` out of necessity and pretended each
buffer had its own working directory.

At least for me, this isn't the right way to use Vim, and it all comes
down to working directories. **I want Vim to be anchored at the
project root** with one Vim instance per project. Everything is
smoother when it happens in the context of the project's root
directory, from opening files, to running shell commands (`ctags` in
particular), to invoking the build system. With `autochdir`, these
actions are difficult to do correctly, particularly the last two.

### Invoking the build

I suspect the Emacs' model of per-buffer working directories has, in a
[Sapir-Whorf][sw] sort of way, been responsible for leading developers
towards [poorly-designed, recursive Makefiles][make]. Without a global
concept of working directory, it's inconvenient to invoke the build
system (`M-x compile`) in some particular grandparent directory that
is the root of the project. If each directory has its own Makefile, it
usually makes sense to invoke `make` in the same directory as the file
being edited.

Over the years I've been reinventing the same solution to this
problem, and it wasn't until I spent time with Vim and its alternate
working directory model that I truly understood the problem. Emacs
itself has long had a solution lurking deep in its bowels, unseen by
daylight: *dominating files*. The function I'm talking about is
`locate-dominating-file`:

> `(locate-dominating-file FILE NAME)`
>
> Look up the directory hierarchy from FILE for a directory containing
> NAME. Stop at the first parent directory containing a file NAME, and
> return the directory. Return nil if not found. Instead of a string,
> NAME can also be a predicate taking one argument (a directory) and
> returning a non-nil value if that directory is the one for which we’re
> looking.

The trouble of invoking the build system at the project root is that
Emacs doesn't really have a concept of a project root. It doesn't know
where it is or how to find it. The vi model inherited by Vim is to
leave the working directory at the project root. While Vim can
simulate Emacs' working directory model, Emacs cannot (currently)
simulate Vim's model.

Instead, by identifying a file name unique to the project's root (i.e.
a "dominating" file) such as `Makefile` or `build.xml`, then
`locate-dominating-file` can discover the project root. All that's
left is wrapping `M-x compile` so that `default-directory` is
temporarily adjusted to the project's root.

That looks *very* roughly like this (and needs more work):

~~~cl
(defun my-compile ()
  (interactive)
  (let ((default-directory (locate-dominating-file "." "Makefile")))
    (compile "make")))
~~~

It's a pattern I've used [again][ex1] and [again][ex2] and
[again][ex3], working against the same old friction. By running one
Vim instance per project at the project's root, I get the correct
behavior for free.


[w32]: https://blogs.msdn.microsoft.com/oldnewthing/20101011-00/?p=12563
[chdir]: http://pubs.opengroup.org/onlinepubs/9699919799/functions/chdir.html
[vi]: http://pubs.opengroup.org/onlinepubs/9699919799/utilities/vi.html
[up]: https://github.com/jwiegley/use-package
[one]: https://vimeo.com/4446112
[six]: /blog/2017/04/01/
[make]: /blog/2017/08/20/
[sw]: https://en.wikipedia.org/wiki/Linguistic_relativity
[ex1]: https://github.com/skeeto/.emacs.d/blob/e8af63ca3585598f5e509bc274e0bb3b875206d3/lisp/ctags.el#L40
[ex2]: https://github.com/skeeto/.emacs.d/blob/e8af63ca3585598f5e509bc274e0bb3b875206d3/etc/compile-bind.el#L38
[ex3]: https://github.com/skeeto/ant-project-mode/blob/335070891f1fabe8d3205418374a68bb13cec8c0/ant-project-mode.el#L211
