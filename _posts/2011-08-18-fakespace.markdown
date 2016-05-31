---
title: Fake Emacs Namespaces
layout: post
tags: [elisp, emacs]
uuid: f89408fd-9b2f-3110-af83-fe96f7c1e7f7
---

Back in May I wrote a crude `defpackage` function for Elisp, modeled
after Common Lisp's version. I'm calling them fakespaces.

 * [https://github.com/skeeto/elisp-fakespace](https://github.com/skeeto/elisp-fakespace)

It works like so (see `example.el` for detailed information on this
code),

~~~cl
(require 'fakespace)

(defpackage example
  (:use cl ido)
  (:export example-main example-var eq-hello hello))

(defvar my-var 100
  "A hidden variable.")

(defvar example-var nil
  "A public variable.")

(defun my-func ()
  "A private function."
  my-var)

(defun example-main ()
  "An exported function. Notice we can access all the private
variables and functions from here."
  (interactive)
  (list (list (my-func) my-var) example-var
        (ido-completing-read "New value: " (list "foo" "bar"))))

(defun eq-hello (sym)
  (eq sym 'hello))

(end-package)
~~~

Notice `end-package` at the end, which is not needed in Common
Lisp. That's part of what makes it crude.

If you run those functions and try changing the assignment of
non-exported symbols, you'll see the namespace separation in
action. `my-var` and `my-func` are a completely different symbols than
the ones you're seeing after `end-package`.

It's really simple in how it works (it's 40 lines of code). The
`defpackage` macro takes a snapshot of the symbol table. Then new
symbols get interned through various function and variable
definitions. Finally `end-package` compares the current symbol table
to the snapshot and uninterns any new symbols. These symbols will be
unaccessible to other code, effectively giving them their own
namespace.

Snapshots are pushed onto a stack, so it's safe to create a new
package within another package, as long as `end-package` is used
properly. This is necessary when one namespaced package depends on
another, because the dependency will tend to be loaded in the middle
of defining the current package.

`in-package` is not provided, so there's no way to get the symbols
back to where they can be accessed. It's impossible to modify a
package using fake namespacing. Worst of all, implementing
`in-package` is currently (and will likely always be) impossible. When
symbols are uninterned they would need to be stored in a package
symbol table for future re-interning. `in-package`'s job would be to
unintern and store away the current package's symbols and then place
the new package's symbols into the main symbol table.

However, symbols cannot be re-interned. This is because it's
impossible for a symbol to exist in two different obarrays at the same
time, so the functionality is intentionally not provided. An obarray
is an Elisp vector containing symbols. It's treated like a hash table:
the symbol is hashed to choose a location in the vector. If the slot
is already taken, the symbol is invisibly chain behind the residing
symbol by an inaccessible linked list. If the symbol was in two
obarrays at once, it would need to be able to chain to two different
symbols at the same time.

Providing access to symbols through a colon-specificed namespace
(`my-package:my-symbol`) is also currently impossible — without
hacking in C anyway.

There's a neat trick to the `:export` list. The `defpackage` macro
definition actually ignores that list altogether, because it works
automatically. By the time `defpackage` is invoked, the listed symbols
have already been interned by the reader, so they get stored in the
snapshot.

I doubt I'll ever make use of this for my own packages. This was
mostly a fun exercise in toying with Elisp.
