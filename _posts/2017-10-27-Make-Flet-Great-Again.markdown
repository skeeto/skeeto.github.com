---
title: Make Flet Great Again
layout: post
date: 2017-10-27T21:02:58Z
tags: [emacs, elisp]
uuid: 46576058-9269-392b-96b2-0f434cbb87a2
---

Do you long for the days before Emacs 24.3 when `flet` was dynamically
scoped? Well, you probably shouldn't since there are [some very good
reasons][lex] lexical scope. But, still, a dynamically scoped `flet`
is situationally really useful, particularly in unit testing. The good
news is that it's trivial to get this original behavior back without
relying on deprecated functions nor third-party packages.

But first, what is `flet` and what does it mean for it to be
dynamically scoped? The name stands for "function let" (or something
to that effect). It's a macro to bind named functions within a local
scope, just as `let` binds variables within some local scope. It's
provided by the now-deprecated `cl` package.

```cl
(require 'cl)  ; deprecated!

(defun norm (x y)
  (flet ((square (v) (* v v)))
    (sqrt (+ (square x) (square y)))))
```

However, a gotcha here is that `square` is visible not just to the body
of `norm` but also to any function called directly or indirectly from
the `flet` body. That's dynamic scope.

```cl
(flet ((sqrt (v) (/ v 2)))  ; close enough
  (norm 2 2))
;; -> 4
```

Note: This works because `sqrt` hasn't (yet?) been assigned a bytecode
opcode. One weakness with `flet` is that, due to being dynamically
scoped, it is unable to define or override functions whose calls
evaporate under byte compilation. For example, addition:

```cl
(defun add-with-flet ()
  (flet ((+ (&rest _) :override))
    (+ 1 2 3)))

(add-with-flet)
;; -> :override

(funcall (byte-compile #'add-with-flet))
;; -> 6
```

Since `+` has its own opcode, the function call is eliminated under
byte-compilation and `flet` can't do its job. This is similar [these
same functions being *unadvisable*][limits].

### cl-lib and cl-flet

The `cl-lib` package introduced in Emacs 24.3, replacing `cl`, adds a
namespace prefix, `cl-`, to all of these Common Lisp style functions.
In most cases this was the only change. One exception is `cl-flet`,
which has different semantics: It's lexically scoped, just like in
Common Lisp. Its bindings aren't visible outside of the `cl-flet`
body.

```cl
(require 'cl-lib)

(cl-flet ((sqrt (v) (/ v 2)))
  (norm 2 2))
;; -> 2.8284271247461903
```

In most cases *this is what you actually want*. The old `flet` subtly
changes the environment for all functions called directly or
indirectly from its body.

Besides being cleaner and less error prone, `cl-flet` also doesn't
have special exceptions for functions with assigned opcodes. At
macro-expansion time it walks the body, taking its action before the
byte-compiler can interfere.

```cl
(defun add-with-cl-flet ()
  (cl-flet ((+ (&rest _) :override))
    (+ 1 2 3)))

(add-with-cl-flet)
;; -> :override

(funcall (byte-compile #'add-with-cl-flet))
;; -> :override
```

In order for it to work properly, it's essential that functions are
quoted with sharp-quotes (`#'`) so that the macro can tell the
difference between functions and symbols. Just make a general habit of
sharp-quoting functions.

In unit testing, temporarily overriding functions for all of Emacs is
useful, so `flet` still has some uses. But it's deprecated!

### Unit testing with flet

Since Emacs can do anything, suppose there is an Emacs package that
makes sandwiches. In this package there's an interactive function to
set the default sandwich cheese.

```cl
(defvar default-cheese 'cheddar)

(defun set-default-cheese (type)
  (interactive
   (let* ((options '("cheddar" "swiss" "american"))
          (input (completing-read "Cheese: " options nil t)))
     (when input
       (list (intern input)))))
  (setf default-cheese type))
```

Since it's interactive, it uses `completing-read` to prompt the user
for input. A unit test could call this function non-interactively, but
perhaps we'd also like to test the interactive path. The code inside
`interactive` occasionally gets messy and may warrant testing. It
would obviously be inconvenient to prompt the user for input during
testing, and it wouldn't work at all in batch mode (`-batch`).

With `flet` we can stub out `completing-read` just for the unit test:

```cl
;;; -*- lexical-binding: t; -*-

(ert-deftest test-set-default-cheese ()
  ;; protect original with dynamic binding
  (let (default-cheese)
    ;; simulate user entering "american"
    (flet ((completing-read (&rest _) "american"))
      (call-interactively #'set-default-cheese)
      (should (eq 'american default-cheese)))))
```

Since `default-cheese` was defined with `defvar`, it will be
dynamically scoped despite `let` normally using lexical scope in this
example. Both of the *side effects* of the tested function — setting a
global variable and prompting the user — are captured using a
combination of `let` and `flet`.

Since `cl-flet` is lexically scoped, it cannot serve this purpose. If
`flet` is deprecated and `cl-flet` can't do the job, what's the right
way to fix it? The answer lies in *generalized variables*.

### cl-letf

What's *really* happening inside `flet` is it's globally binding a
function name to a different function, evaluating the body, and
rebinding it back to the original definition when the body completes.
It macro-expands to something like this:

~~~cl
(let ((original (symbol-function 'completing-read)))
  (setf (symbol-function 'completing-read)
        (lambda (&rest _) "american"))
  (unwind-protect
      (call-interactively #'set-default-cheese)
    (setf (symbol-function 'completing-read) original)))
~~~

The `unwind-protect` ensures the original function is rebound even if
the body of the call were to fail. This is very much a `let`-like
pattern, and I'm using `symbol-function` as a generalized variable via
`setf`. Is there a generalized variable version of `let`?

Yes! It's called `cl-letf`! In this case the `f` suffix is analogous
to the `f` suffix in `setf`. That form above can be reduced to a more
general form:

```cl
(cl-letf (((symbol-function 'completing-read)
           (lambda (&rest _) "american")))
  (call-interactively #'set-default-cheese))
```

And *that's* the way to reproduce the dynamically scoped behavior of
`flet` since Emacs 24.3. There's nothing complicated about it.

```cl
(ert-deftest test-set-default-cheese ()
  (let (default-cheese)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "american")))
      (call-interactively #'set-default-cheese)
      (should (eq 'american default-cheese)))))

```

Keep in mind that this suffers the exact same problem with
bytecode-assigned functions as `flet`, and for exactly the same
reasons. If `completing-read` were to ever be assigned its own opcode
then `cl-letf` would no longer work for this particular example.


[lex]: /blog/2016/12/22/
[limits]: /blog/2013/01/22/
