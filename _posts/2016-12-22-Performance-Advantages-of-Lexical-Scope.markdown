---
title: Some Performance Advantages of Lexical Scope
layout: post
date: 2016-12-22T02:33:36Z
tags: [emacs, elisp, optimization, compsci]
uuid: 21bc4afa-caa8-37ed-a912-a35f35d0e432
---

I recently had a discussion with [Xah Lee][xah] about lexical scope in
Emacs Lisp. The topic was why `lexical-binding` exists at a file-level
when there was already `lexical-let` (from `cl-lib`), prompted by my
previous article on [JIT byte-code compilation][prev]. The specific
context is Emacs Lisp, but these concepts apply to language design in
general.

Until Emacs 24.1 (June 2012), Elisp only had dynamically scoped
variables — a feature, mostly by accident, common to old lisp
dialects. While dynamic scope has some selective uses, it's widely
regarded as a mistake for local variables, and virtually no other
languages have adopted it.

Way back in 1993, Dave Gillespie's deviously clever `lexical-let`
macro [was committed][dg] to the `cl` package, providing a rudimentary
form of opt-in lexical scope. The macro walks its body replacing local
variable names with guaranteed-unique gensym names: the exact same
technique used in macros to create "hygienic" bindings that aren't
visible to the macro body. It essentially "fakes" lexical scope within
Elisp's dynamic scope by preventing variable name collisions.

For example, here's one of the consequences of dynamic scope.

~~~cl
(defun inner ()
  (setq v :inner))

(defun outer ()
  (let ((v :outer))
    (inner)
    v))

(outer)
;; => :inner
~~~

The "local" variable `v` in `outer` is visible to its callee, `inner`,
which can access and manipulate it. The meaning of the *free variable*
`v` in `inner` depends entirely on the run-time call stack. It might
be a global variable, or it might be a local variable for a caller,
direct or indirect.

Using `lexical-let` deconflicts these names, giving the effect of
lexical scope.

~~~cl
(defvar v)

(defun lexical-outer ()
  (lexical-let ((v :outer))
    (inner)
    v))

(lexical-outer)
;; => :outer
~~~

But there's more to lexical scope than this. Closures only make sense
in the context of lexical scope, and the most useful feature of
`lexical-let` is that lambda expressions evaluate to closures. The
macro implements this using a technique called [*closure
conversion*][ll]. Additional parameters are added to the original
lambda function, one for each lexical variable (and not just each
closed-over variable), and the whole thing is wrapped in *another*
lambda function that invokes the original lambda function with the
additional parameters filled with the closed-over variables — yes, the
variables (e.g. symbols) themselves, *not* just their values, (e.g.
pass-by-reference). The last point means different closures can
properly close over the same variables, and they can bind new values.

To roughly illustrate how this works, the first lambda expression
below, which closes over the lexical variables `x` and `y`, would be
converted into the latter by `lexical-let`. The `#:` is Elisp's syntax
for uninterned variables. So `#:x` is *a* symbol `x`, but not *the*
symbol `x` (see `print-gensym`).

~~~cl
;; Before conversion:
(lambda ()
  (+ x y))

;; After conversion:
(lambda (&rest args)
  (apply (lambda (x y)
           (+ (symbol-value x)
              (symbol-value y)))
         '#:x '#:y args))
~~~

I've said on multiple occasions that `lexical-binding: t` has
significant advantages, both in performance and static analysis, and
so it should be used for all future Elisp code. The only reason it's
not the default is because it breaks some old (badly written) code.
However, **`lexical-let` doesn't realize any of these advantages**! In
fact, it has worse performance than straightforward dynamic scope with
`let`.

1. New symbol objects are allocated and initialized (`make-symbol`) on
   each run-time evaluation, one per lexical variable.

2. Since it's just faking it, `lexical-let` still uses dynamic
   bindings, which are more expensive than lexical bindings. It varies
   depending on the C compiler that built Emacs, but dynamic variable
   accesses (opcode `varref`) take around 30% longer than lexical
   variable accesses (opcode `stack-ref`). Assignment is far worse,
   where dynamic variable assignment (`varset`) takes 650% longer than
   lexical variable assignment (`stack-set`). How I measured all this
   is a topic for another article.

3. The "lexical" variables are accessed using `symbol-value`, a full
   function call, so they're even slower than normal dynamic
   variables.

4. Because converted lambda expressions are constructed dynamically at
   run-time within the body of `lexical-let`, the resulting closure is
   only partially byte-compiled even if the code as a whole has been
   byte-compiled. In contrast, `lexical-binding: t` closures are fully
   compiled. How this works is worth [its own article][followup].

5. Converted lambda expressions include the additional internal
   function invocation, making them slower.

While `lexical-let` is clever, and occasionally useful prior to Emacs
24, it may come at a hefty performance cost if evaluated frequently.
There's no reason to use it anymore.

### Constraints on code generation

Another reason to be weary of dynamic scope is that it puts needless
constraints on the compiler, preventing a number of important
optimization opportunities. For example, consider the following
function, `bar`:

~~~cl
(defun bar ()
  (let ((x 1)
        (y 2))
    (foo)
    (+ x y)))
~~~

Byte-compile this function under dynamic scope (`lexical-binding:
nil`) and [disassemble it][bc] to see what it looks like.

~~~cl
(byte-compile #'bar)
(disassemble #'bar)
~~~

That pops up a buffer with the disassembly listing:

    0       constant  1
    1       constant  2
    2       varbind   y
    3       varbind   x
    4       constant  foo
    5       call      0
    6       discard
    7       varref    x
    8       varref    y
    9       plus
    10      unbind    2
    11      return

It's 12 instructions, 5 of which deal with dynamic bindings. The
byte-compiler doesn't always produce optimal byte-code, but this just
so happens to be *nearly* optimal byte-code. The `discard` (a very
fast instruction) isn't necessary, but otherwise no more compiler
smarts can improve on this. Since the variables `x` and `y` are
visible to `foo`, they must be bound before the call and [loaded after
the call][const]. While generally this function will return 3, the
compiler cannot assume so since it ultimately depends on the behavior
`foo`. Its hands are tied.

Compare this to the lexical scope version (`lexical-binding: t`):

    0       constant  1
    1       constant  2
    2       constant  foo
    3       call      0
    4       discard
    5       stack-ref 1
    6       stack-ref 1
    7       plus
    8       return

It's only 8 instructions, none of which are expensive dynamic variable
instructions. And this isn't even close to the optimal byte-code. In
fact, as of Emacs 25.1 the byte-compiler often doesn't produce the
optimal byte-code for lexical scope code and still needs some work.
**Despite not firing on all cylinders, lexical scope still manages to
beat dynamic scope in performance benchmarks.**

Here's the optimal byte-code, should the byte-compiler become smarter
someday:

    0       constant  foo
    1       call      0
    2       constant  3
    3       return

It's down to 4 instructions due to computing the math operation at
compile time. Emacs' byte-compiler only has rudimentary constant
folding, so it doesn't notice that `x` and `y` are constants and
misses this optimization. I speculate this is due to its roots
compiling under dynamic scope. Since `x` and `y` are no longer exposed
to `foo`, the compiler has the opportunity to optimize them out of
existence. I haven't measured it, but I would expect this to be
significantly faster than the dynamic scope version of this function.

### Optional dynamic scope

You might be thinking, "What if I really *do* want `x` and `y` to be
dynamically bound for `foo`?" This is often useful. Many of Emacs' own
functions are designed to have certain variables dynamically bound
around them. For example, the print family of functions use the global
variable `standard-output` to determine where to send output by
default.

~~~cl
(let ((standard-output (current-buffer)))
  (princ "value = ")
  (prin1 value))
~~~

Have no fear: **With `lexical-binding: t` you can have your cake and
eat it too.** Variables declared with `defvar`, `defconst`, or
`defvaralias` are marked as "special" with an internal bit flag
(`declared_special` in C). When the compiler detects one of these
variables (`special-variable-p`), it uses a classical dynamic binding.

Declaring both `x` and `y` as special restores the original semantics,
reverting `bar` back to its old byte-code definition (next time it's
compiled, that is). But it would be poor form to mark `x` or `y` as
special: You'd de-optimize all code (compiled *after* the declaration)
anywhere in Emacs that uses these names. As a package author, only do
this with the namespace-prefixed variables that belong to you.

The only way to unmark a special variable is with the undocumented
function `internal-make-var-non-special`. I expected `makunbound` to
do this, but as of Emacs 25.1 it does not. This could possibly be
considered a bug.

### Accidental closures

I've said there are absolutely no advantages to `lexical-binding: nil`.
It's only the default for the sake of backwards-compatibility. However,
there *is* one case where `lexical-binding: t` introduces a subtle issue
that would otherwise not exist. Take this code for example (and
nevermind `prin1-to-string` for a moment):

~~~cl
;; -*- lexical-binding: t; -*-

(defun function-as-string ()
  (with-temp-buffer
    (prin1 (lambda () :example) (current-buffer))
    (buffer-string)))
~~~

This creates and serializes a closure, which is [one of Elisp's unique
features][rd]. It doesn't close over any variables, so it should be
pretty simple. However, this function will only work correctly under
`lexical-binding: t` when byte-compiled.

~~~cl
(function-as-string)
;; => "(closure ((temp-buffer . #<buffer  *temp*>) t) nil :example)"
~~~

The interpreter doesn't analyze the closure, so just closes over
everything. This includes the hidden variable `temp-buffer` created by
the `with-temp-buffer` macro, resulting in an abstraction leak.
Buffers aren't readable, so this will signal an error if an attempt is
made to read this function back into an s-expression. The
byte-compiler fixes this by noticing `temp-buffer` isn't actually
closed over and so doesn't include it in the closure, making it work
correctly.

Under `lexical-binding: nil` it works correctly either way:

~~~cl
(function-as-string)
;; -> "(lambda nil :example)"
~~~

This may seem contrived — it's certainly unlikely — but [it has come
up in practice][bug]. Still, it's no reason to avoid `lexical-binding: t`.

### Use lexical scope in all new code

As I've said again and again, always use `lexical-binding: t`. Use
dynamic variables judiciously. And `lexical-let` is no replacement. It
has virtually none of the benefits, performs *worse*, and it only
applies to `let`, not any of the other places bindings are created:
function parameters, `dotimes`, `dolist`, and `condition-case`.


[xah]: http://ergoemacs.org/
[dg]: http://git.savannah.gnu.org/cgit/emacs.git/commit/?h=fcd73769&id=fcd737693e8e320acd70f91ec8e0728563244805
[prev]: /blog/2016/12/11/
[ll]: https://en.wikipedia.org/wiki/Lambda_lifting
[bc]: /blog/2014/01/04/
[rd]: /blog/2013/12/30/
[bug]: https://github.com/jwiegley/emacs-async/issues/17
[const]: /blog/2016/07/25/
[followup]: /blog/2017/12/14/
