---
title: Emacs Lisp Defstruct Namespace Convention
layout: post
date: 2014-03-19T01:41:52Z
tags: [emacs, lisp, elisp]
uuid: 624f92a9-6696-33bb-f955-d6c83da56fc1
---

One of the drawbacks of Emacs Lisp is the lack of namespaces. Every
`defun`, `defvar`, `defcustom`, `defface`, `defalias`, `defstruct`,
and `defclass` establishes one or more names in the global scope. To
work around this, package authors are strongly encouraged to prefix
every global name with the name of its package. That way there should
never be a naming conflict between two different packages.

~~~cl
(defvar mypackage-foo-limit 10)

(defvar mypackage--bar-counter 0)

(defun mypackage-init ()
  ...)

(defun mypackage-compute-children (node)
  ...)

(provide 'mypackage)
~~~

While this has solved the problem for the time being, attaching the
package name to almost every identifier, including private function
and variable names, is quite cumbersome. Namespaces can *almost* be
hacked into the language by using multiple obarrays,
[but symbols have internal linked lists][obarray] that prohibit
inclusion in multiple obarrays.

By convention, private names are given a double-dash after the
namespace. If a "bar counter" is an implementation detail that may
disappear in the future, it will be called `mypackage--bar-counter` to
warn users and other package authors not to rely on it.

There's been a recent push to follow this namespace-prefix policy more
strictly, particularly with the depreciation of `cl` and introduction
of `cl-lib`. I suspect someday when namespaces are finally introduced,
packages with strictly clean namespaces with be at an advantage,
somehow automatically supported. [Nic Ferrier has proposed ideas][nic]
for how to move forward on this.

### How strict are we talking?

Over the last few years I've gotten much stricter in my own packages
when it comes to namespace prefixes. You can see the progression going
from [javadoc-lookup][jdl] (2010) where I was completely sloppy about
it, to [EmacSQL][emacsql] (2014) where every single global identifier
is meticulously prefixed.

For a time I considered names such as `make-*` and `with-*` to be
exceptions to the rule, since these names are idioms inherited from
Common Lisp. The namespace comes *after* the expected prefix. I've
changed my mind about this, which has caused me to change my usage of
`defstruct` (now `cl-defstruct`).

Just as in Common Lisp, by default `cl-defstruct` defines a
constructor starting with `make-*`. This is fine in Common Lisp, where
it's a package-private function by default, but in Emacs Lisp this
pollutes the global namespace.

~~~cl
(require 'cl-lib)

;; Defines make-circle, circle-x, circle-y, circle-radius, circle-p
(cl-defstruct circle
  x y radius)

(defvar unit-circle (make-circle :x 0.0 :y 0.0 :radius 1.0))

unit-circle
;; => [cl-struct-circle 0.0 0.0 1.0]

(circle-radius unit-circle)
;; => 1.0
~~~

This constructor isn't namespace clean, so package authors should
avoid defstruct's default. If the package is named `circle` then all
of the accessors are perfectly fine, though.

To fix this, I now use another, more recent Emacs Lisp idiom: name the
constructor `create`. That is, for the package `circle`, we desire
`circle-create`. To get this behavior from `cl-defstruct`, use the
`:constructor` option.

~~~cl
;; Clean!
(cl-defstruct (circle (:constructor circle-create))
  x y radius)

(circle-create :x 0 :y 0 :radius 1)
;; => [cl-struct-circle 0 0 1]

(provide 'circle)
~~~

This affords a new opportunity to craft a better constructor. Have
`cl-defstruct` define a private constructor, then manually write a
constructor with a nicer interface. It may also do additional work,
like enforce invariants or initialize dependent slots.

~~~cl
(cl-defstruct (circle (:constructor circle--create))
  x y radius)

(defun circle-create (x y radius)
  (let ((circle (circle--create :x x :y y :radius radius)))
    (if (< radius 0)
        (error "must have non-negative radius")
      circle)))

(circle-create 0 0 1)
;; => [cl-struct-circle 0 0 1]

(circle-create 0 0 -1)
;; error: "must have non-negative radius"
~~~

This is now how I always use `cl-defstruct` in Emacs Lisp. It's a tidy
convention that will probably become more common in the future.


[obarray]: /blog/2011/08/18/
[nic]: http://nic.ferrier.me.uk/blog/2013_06/adding-namespaces-to-elisp
[jdl]: https://github.com/skeeto/javadoc-lookup
[emacsql]: https://github.com/skeeto/emacsql
