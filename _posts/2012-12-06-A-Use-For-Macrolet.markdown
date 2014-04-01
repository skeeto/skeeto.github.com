---
title: A Use For Macrolet
layout: post
tags: [lisp, elisp]
uuid: 271d91dd-abad-31c8-4e26-f8dfdd5ffa92
---

I recently had a good use for Common Lisp's `macrolet` special
operator. Just as `let` establishes a new variable bindings and `flet`
establishes new function bindings, `macrolet` establishes a new macro
definitions.

For example, here's a locally-defined [anaphoric][anaphoric] `lambda`
macro called `fn`.

~~~cl
(macrolet ((fn (&body body) `(lambda (_) ,@body)))
  (map 'string (fn (if (standard-char-p _) _ #\*)) "naïve"))
;; => "na*ve"
~~~

My particular use case was about making my code cleaner for
[a brainfuck interpreter][bf]. The state of the machine was being
tracked by this struct. (Interesting side note: SBCL warns about using
`p` as a slot name because the accessor function will look like a
predicate.)

~~~cl
(defstruct bf
  (p 0)
  (mem (make-array 30000 :initial-element 0)))
~~~

The BF instructions `+` and `-` increment the byte at the data
pointer. The Common Lisp `incf` and `decf` macros can be used to do
this. Similarly, the `,` instruction sets the byte at the data
pointer, which can be done with `setf`. All three of these macros are
*place*-modifying.

~~~cl
(defun interp (program state)
  ;; ...
  (incf (aref (bf-mem state) (bf-p state)))
  ;; ...
  (decf (aref (bf-mem state) (bf-p state)))
  ;; ...
  (setf (aref (bf-mem state) (bf-p state)) (char-code (read-char))))
~~~

That's a whole lot of redundancy for a Lisp program. Under similar
circumstances elsewhere I might use `flet` to reduce it.

~~~cl
;; This won't work.
(defun interp (program state)
  (flet ((ref () (aref (bf-mem state) (bf-p state))))
    ;; ...
    (incf (ref))
    ;; ...
    (decf (ref))))
~~~

The problem is that `ref` isn't a [*generalized reference*][ref],
which `incf`, `decf`, and `setf` all require. Common Lisp's
*place*-modifying utilities are implemented as macros. It's known at
compile-time what kind of place they are modifying: a variable, array
index, object/struct slot, car, cdr, or many other things (Emacs `cl`
package allows all sorts of things to be `setf`ed, like
`(point)`). The macro expands into the proper form for setting that
kind of place.

The specific expansion is implementation-dependent, but, for example,
`setf` could expand into a `setq` when the first argument is a
symbol. New generalized references can be defined with `defsetf`.

In my case, a simple macro expansion can fill the role. Below, the
place-modifying macro will expand `ref`
([*after* looking elsewhere][expand]) to decide what to do, and `ref`
will expand to an `aref` form.

~~~cl
(defun interp (program state)
  (macrolet ((ref () '(aref (bf-mem state) (bf-p state))))
    ;; ...
    (incf (ref))
    ;; ...
    (decf (ref))
    ;; ...
    (setf (ref) (char-code (read-char)))))
~~~

Because the macro has no parameters I could have even more easily used
`symbol-macrolet`. I just didn't think of it at the time.


[bf]: http://redd.it/137f7h
[ref]: http://www.lispworks.com/documentation/HyperSpec/Body/05_aa.htm
[expand]: http://www.lispworks.com/documentation/lw60/CLHS/Body/05_abg.htm
[anaphoric]: http://en.wikipedia.org/wiki/Anaphoric_macro
