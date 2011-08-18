---
title: Fake Emacs Namespaces
layout: post
---

Back in May I wrote a crude `defpackage` function for Elisp, modeled
after Common Lisp's version. I'm calling them fakespaces.

<pre>
git clone <a href="https://github.com/skeeto/elisp-fakespace">git://github.com/skeeto/elisp-fakespace.git</a>
</pre>

It works like so (see `example.el` for detailed information on this
code),

{% highlight cl %}
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
{% endhighlight %}

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

`in-package` is not provided, so there's no (simple) way to get the
symbols back to where they can be accessed. Right now it would be hard
to modify a package using this fake namespacing. Fortunately,
providing `in-package` wouldn't be too hard to do -- and, if added as
advice in the right places, it would replace `end-package`. When
symbols are uninterned they would need to be stored in a package
symbol table for future fetching. `in-package`'s job would be to
unintern and store away the current package's symbols and then place
the new package's symbols into the main symbol table.

Providing access to symbols through a colon-specificed namespace
(`my-package:my-symbol`) is not currently possible without hacking in
C.

There's a neat trick to the `:export` list. The `defpackage` macro
definition actually ignores that list altogether, because it works
automatically. By the time `defpackage` is invoked, the listed symbols
have already been interned by the reader, so they get stored in the
snapshot.

I doubt I'll ever make use of this for my own packages. This was
mostly a fun exercise in toying with Elisp.
