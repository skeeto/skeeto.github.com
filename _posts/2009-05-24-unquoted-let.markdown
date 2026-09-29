---
title: Unquoted Let
layout: post
tags: []
uuid: fd8ed6df-1a72-3568-7dfe-0be6fde803a0
---

<!-- 24 May 2009 -->

*Update: The basic idea is unsound as is my implementation below.
Besides this, the `cl-letf` macro can already do this sort
of thing. At the time I obviously did not fully understand Lisp macros
nor language implementation in general. Using `eval` is
usually a stupid idea, and it's even more stupid to try to use it
inside of a macro where it will most likely be used, causing an error,
at compile time. I doubt this would work well with byte-compiled code.
I'll leave this post here for historical purposes.*

The lisp macro/form `let` quotes variable's symbols. This
is almost always more useful than not quoting it. Otherwise, simple
use would look like,

<pre>
(let (('var 100))
  <i>body</i>)
</pre>

instead of,

<pre>
(let ((var 100))
  <i>body</i>)
</pre>

In elisp, this is analogous to `setq`, which quotes its
first argument, and `set`, which doesn't. It could be used
like so in elisp,

```cl
(setq foo 'bar)
(setq bar 25)

(uq-let ((foo 10))
  (+ 10 bar))
```

The unquoted `let` form evalues to `20`, not 35,
because `foo` evaluates to the symbol `bar`,
which is bound to 10. The unquoted version is used to select variable
names dynamically.

This isn't very useful in a lexically scoped lisp because the
variables that can access it are decided, well, lexically. With
dynamic scoping we can use it to temporarily mask variables and select
what variables to mask dynamically. I found a use for it in one of my
projects.

I had a recursive function that searched a graph made of symbols. The
symbols globally stored the state of the node. The current node was
passed into the function, which would change the state of that node,
and recurse. Here's how I was doing it,

<pre>
(defun search (graph node)
  <i>...</i>
  (set node 'visited)
  <i>recurse</i>
  (set node 'unvisited))
</pre>

That looks a lot like it is emulating a `let` form. If we
use an unquoted `let`,

<pre>
(defun search (graph node)
  <i>...</i>
  (uq-let ((node 'visited))
    <i>recurse</i>))
</pre>

There, that's a lot more lispy. Here is an elisp macro for uq-let,

```cl
(defmacro uq-let (vars &rest body)
  "Unquoted let."
  (declare (indent defun))
  `(let ,(mapcar
          (lambda (a)
            (cons (eval (car a)) (cdr a))) vars)
     ,@body))
```

From this, making a `uq-let*` is trivial. The documentation
string could use some work too.
