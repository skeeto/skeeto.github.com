---
title: Counting Processor Cores in Emacs
layout: post
date: 2015-10-14T03:17:16Z
tags: [emacs, elisp, c, cpp]
uuid: dbfba1a0-b3af-356d-4d01-96917d622906
---

One of the great advantages of dependency analysis is parallelization.
Modern processors reorder instructions whose results don't affect each
other. Compilers reorder expressions and statements to improve
throughput. Build systems know which outputs are inputs for other
targets and can choose any arbitrary build order within that
constraint. This article involves the last case.

The build system I use most often is GNU Make, either directly or
indirectly (Autoconf, CMake). It's far from perfect, but it does what
I need. I almost always invoke it from within Emacs rather than in a
terminal. In fact, I do it so often that I've wrapped Emacs' `compile`
command for rapid invocation.

I recently helped a co-worker set this set up for himself, so it had
me thinking about the problem again. The situation [in my
config][config] is much more complicated than it needs to be, so I'll
share a simplified version instead.

First bring in the usual goodies (we're going to be making closures):

~~~cl
;;; -*- lexical-binding: t; -*-
(require 'cl-lib)
~~~

We need a couple of configuration variables.

~~~cl
(defvar quick-compile-command "make -k ")
(defvar quick-compile-build-file "Makefile")
~~~

Then a couple of interactive functions to set these on the fly. It's
not strictly necessary, but I like giving each a key binding. I also
like having a history available via `read-string`, so I can switch
between a couple of different options with ease.

~~~cl
(defun quick-compile-set-command (command)
  (interactive
   (list (read-string "Command: " quick-compile-command)))
  (setf quick-compile-command command))

(defun quick-compile-set-build-file (build-file)
  (interactive
   (list (read-string "Build file: " quick-compile-build-file)))
  (setf quick-compile-build-file build-file))
~~~

Now finally to the good part. Below, `quick-compile` is a
non-interactive function that returns an interactive closure ready to
be bound to any key I desire. It takes an optional target. This means
I don't use the above `quick-compile-set-command` to choose a target,
only for setting other options. That will make more sense in a moment.

~~~cl
(cl-defun quick-compile (&optional (target ""))
  "Return an interaction function that runs `compile' for TARGET."
  (lambda ()
    (interactive)
    (save-buffer)  ; so I don't get asked
    (let ((default-directory
            (locate-dominating-file
             default-directory quick-compile-build-file)))
      (if default-directory
          (compile (concat quick-compile-command " " target))
        (error "Cannot find %s" quick-compile-build-file)))))
~~~

It traverses up (down?) the directory hierarchy towards root looking
for a Makefile — or whatever is set for `quick-compile-build-file`
— then invokes the build system there. I [don't believe in recursive
`make`][harmful].

So how do I put this to use? I clobber some key bindings I don't
otherwise care about. A better choice might be the F-keys, but my
muscle memory is already committed elsewhere.

~~~cl
(global-set-key (kbd "C-x c") (quick-compile)) ; default target
(global-set-key (kbd "C-x C") (quick-compile "clean"))
(global-set-key (kbd "C-x t") (quick-compile "test"))
(global-set-key (kbd "C-x r") (quick-compile "run"))
~~~

Each of those invokes a different target without second guessing me.
Let me tell you, having "clean" at the tip of my fingers is wonderful.

### Parallel Builds

An extension common to many different `make` programs is `-j`, which
asks `make` to build targets in parallel where possible. These days
where multi-core machines are the norm, you nearly always want to use
this option, ideally set to the number of logical processor cores on
your system. It's a huge time-saver.

My recent revelation was that my default build command could be
better: `make -k` is minimal. It should at least include `-j`, but
choosing an argument (number of processor cores) is a problem. Today I
use different machines with 2, 4, or 8 cores, so most of the time any
given number will be wrong. I could use a per-system configuration,
but I'd rather not. Unfortunately GNU Make will not automatically
detect the number of cores. That leaves the matter up to Emacs Lisp.

Emacs doesn't currently have a built-in function that returns the
number of processor cores. I'll need to reach into the operating
system to figure it out. My usual development environments are Linux,
Windows, and OpenBSD, so my solution should work on each. I've ranked
them by order of importance.

#### Number of cores on Linux

Linux has the `/proc` virtual filesystem in the fashion of Plan 9,
allowing different aspects of the system to be explored through the
standard filesystem API. The relevant file here is `/proc/cpuinfo`,
listing useful information about each of the system's processors. To
get the number of processors, count the number of processor entries in
this file. I've wrapped it in `if-file-exists` so that it returns
`nil` on other operating systems instead of throwing an error.

~~~cl
(when (file-exists-p "/proc/cpuinfo")
  (with-temp-buffer
    (insert-file-contents "/proc/cpuinfo")
    (how-many "^processor[[:space:]]+:")))
~~~

#### Number of cores on Windows

When I was first researching how to do this on Windows, I thought I
would need to invoke the `wmic` command line program and hope the
output could be parsed the same way on different versions of the
operating system and tool. However, it turns out the solution for
Windows is trivial. The environment variable `NUMBER_OF_PROCESSORS`
gives every process the answer for free. Being an environment
variable, it will need to be parsed.

~~~cl
(let ((number-of-processors (getenv "NUMBER_OF_PROCESSORS")))
  (when number-of-processors
    (string-to-number number-of-processors)))
~~~

#### Number of cores on BSD

This seems to work the same across all the BSDs, including OS X,
though I haven't yet tested it exhaustively. Invoke `sysctl`, which
returns an undecorated number to be parsed.

~~~cl
(with-temp-buffer
  (ignore-errors
    (when (zerop (call-process "sysctl" nil t nil "-n" "hw.ncpu"))
      (string-to-number (buffer-string)))))
~~~

Also not complicated, but it's the heaviest solution of the three.

### Putting it all together

Join all these together with `or`, call it `numcores`, and ta-da.

~~~cl
(setf quick-compile-command (format "make -kj%d" (numcores)))
~~~

Now `make` is invoked correctly on any system by default.


[harmful]: http://aegis.sourceforge.net/auug97.pdf
[config]: https://github.com/skeeto/.emacs.d
