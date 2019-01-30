---
title: What's in an Emacs Lambda
layout: post
date: 2017-12-14T18:18:57Z
tags: [emacs, elisp, compsci, lang]
uuid: efcc8cf7-11d3-3bd3-9fc9-a23e80f7bf33
---

There was recently some [interesting discussion][src] about correctly
using backquotes to express a mixture of data and code. Since lambda
expressions *seem* to evaluate to themselves, what's the difference?
For example, an association list of operations:

```cl
'((add . (lambda (a b) (+ a b)))
  (sub . (lambda (a b) (- a b)))
  (mul . (lambda (a b) (* a b)))
  (div . (lambda (a b) (/ a b))))
```

It looks like it would work, and indeed it does work in this case.
However, there are good reasons to actually evaluate those lambda
expressions. Eventually invoking the lambda expressions in the quoted
form above are equivalent to using `eval`. So, instead, prefer the
backquote form:

```cl
`((add . ,(lambda (a b) (+ a b)))
  (sub . ,(lambda (a b) (- a b)))
  (mul . ,(lambda (a b) (* a b)))
  (div . ,(lambda (a b) (/ a b))))
```

There are a lot of interesting things to say about this, but let's
first reduce it to two very simple cases:

```cl
(lambda (x) x)

'(lambda (x) x)
```

What's the difference between these two forms? The first is a lambda
expression, and it evaluates to a function object. The other is a quoted
list that *looks like* a lambda expression, and it evaluates to a list —
a piece of data.

A naive evaluation of these expressions in `*scratch*` (`C-x C-e`)
suggests they are are identical, and so it would seem that quoting a
lambda expression doesn't really matter:

```cl
(lambda (x) x)
;; => (lambda (x) x)

'(lambda (x) x)
;; => (lambda (x) x)
```

However, there are two common situations where this is not the case:
**byte compilation** and **lexical scope**.

### Lambda under byte compilation

It's a little trickier to evaluate these forms byte compiled in the
scratch buffer since that doesn't happen automatically. But if it did,
it would look like this:

```cl
;;; -*- lexical-binding: nil; -*-

(lambda (x) x)
;; => #[(x) "\010\207" [x] 1]

'(lambda (x) x)
;; => (lambda (x) x)
```

The `#[...]` is the syntax for a byte-code function object. As
discussed in detail in [my byte-code internals article][bc], it's a
special vector object that contains byte-code, and other metadata, for
evaluation by Emacs' virtual stack machine. Elisp is one of very few
languages with [readable function objects][read], and this feature is
core to its ahead-of-time byte compilation.

The quote, by definition, prevents evaluation, and so inhibits byte
compilation of the lambda expression. It's vital that the byte compiler
does not try to guess the programmer's intent and compile the expression
anyway, since that would interfere with lists that just so happen to
look like lambda expressions — i.e. any list containing the `lambda`
symbol.

There are three reasons you want your lambda expressions to get byte
compiled:

* Byte-compiled functions are significantly faster. That's the main
  purpose for byte compilation after all.

* The compiler performs static checks, producing warnings and errors
  ahead of time. This lets you spot certain classes of problems before
  they occur. The static analysis is even better under lexical scope due
  to its tighter semantics.

* Under lexical scope, byte-compiled closures may use less memory. More
  specifically, they won't accidentally keep objects alive longer than
  necessary. I've never seen a name for this implementation issue, but I
  call it *overcapturing*. More on this later.

While it's common for personal configurations to skip byte compilation,
Elisp should still generally be written as if it were going to be byte
compiled. General rule of thumb: **Ensure your lambda expressions are
actually evaluated.**

### Lambda in lexical scope

As I've stressed many times, [you should *always* use lexical
scope][lex]. There's no practical disadvantage or trade-off involved.
Just do it.

Once lexical scope is enabled, the two expressions diverge even without
byte compilation:

```cl
;;; -*- lexical-binding: t; -*-

(lambda (x) x)
;; => (closure (t) (x) x)

'(lambda (x) x)
;; => (lambda (x) x)
```

Under lexical scope, lambda expressions evaluate to *closures*.
Closures capture their lexical environment in their closure object —
nothing in this particular case. It's a type of function object,
making it a valid first argument to `funcall`.

Since the quote prevents the second expression from being evaluated,
semantically it evaluates to a list that just so happens to look like
a (non-closure) function object. **Invoking a *data* object as a
function is like using `eval`** — i.e. executing data as code.
Everyone already knows `eval` should not be used lightly.

It's a little more interesting to look at a closure that actually
captures a variable, so here's a definition for `constantly`, a
higher-order function that returns a closure that accepts any number of
arguments and returns a particular constant:

```cl
(defun constantly (x)
  (lambda (&rest _) x))
```

Without byte compiling it, here's an example of its return value:

```cl
(constantly :foo)
;; => (closure ((x . :foo) t) (&rest _) x)
```

The environment has been captured as an association list (with a
trailing `t`), and we can plainly see that the variable `x` is bound to
the symbol `:foo` in this closure. Consider that we could manipulate
this data structure (e.g. `setcdr` or `setf`) to change the binding of
`x` for this closure. *This is essentially how closures mutate their own
environment.* Moreover, closures from the same environment share
structure, so such mutations are also shared. More on this later.

Semantically, closures are distinct objects (via `eq`), even if the
variables they close over are bound to the same value. This is because
they each have a distinct environment attached to them, even if in
some invisible way.

```cl
(eq (constantly :foo) (constantly :foo))
;; => nil
```

Without byte compilation, this is true *even when there's no lexical
environment to capture*:

```cl
(defun dummy ()
  (lambda () t))

(eq (dummy) (dummy))
;; => nil
```

The byte compiler is smart, though. [As an optimization][fast], the
same closure object is reused when possible, avoiding unnecessary
work, including multiple object allocations. Though this is a bit of
an abstraction leak. A function can (ab)use this to introspect whether
it's been byte compiled:

```cl
(defun have-i-been-compiled-p ()
  (let ((funcs (vector nil nil)))
    (dotimes (i 2)
      (setf (aref funcs i) (lambda ())))
    (eq (aref funcs 0) (aref funcs 1))))

(have-i-been-compiled-p)
;; => nil

(byte-compile 'have-i-been-compiled-p)

(have-i-been-compiled-p)
;; => t
```

The trick here is to evaluate the exact same non-capturing lambda
expression twice, which requires a loop (or at least some sort of
branch). *Semantically* we should think of these closures as being
distinct objects, but, if we squint our eyes a bit, we can see the
effects of the behind-the-scenes optimization.

Don't actually do this in practice, of course. That's what
`byte-code-function-p` is for, which won't rely on a subtle
implementation detail.

### Overcapturing

I mentioned before that one of the potential gotchas of not byte
compiling your lambda expressions is overcapturing closure variables in
the interpreter.

To evaluate lisp code, Emacs has both an interpreter and a virtual
machine. The interpreter evaluates code in list form: cons cells,
numbers, symbols, etc. The byte compiler is like the interpreter, but
instead of directly executing those forms, it emits byte-code that, when
evaluated by the virtual machine, produces identical visible results to
the interpreter — *in theory*.

What this means is that **Emacs contains two different implementations
of Emacs Lisp**, one in the interpreter and one in the byte compiler.
The Emacs developers have been maintaining and expanding these
implementations side-by-side for decades. A pitfall to this approach
is that the *implementations can, and do, diverge in their behavior*.
We saw this above with that introspective function, and it [comes up
in practice with advice][advice].

Another way they diverge is in closure variable capture. For example:

```cl
;;; -*- lexical-binding: t; -*-

(defun overcapture (x y)
  (when y
    (lambda () x)))

(overcapture :x :some-big-value)
;; => (closure ((y . :some-big-value) (x . :x) t) nil x)
```

Notice that the closure captured `y` even though it's unnecessary.
This is because the interpreter doesn't, and shouldn't, take the time
to analyze the body of the lambda to determine which variables should
be captured. That would need to happen at run-time each time the
lambda is evaluated, which would make the interpreter much slower.
Overcapturing can get pretty messy if macros are introducing their own
hidden variables.

On the other hand, the byte compiler can do this analysis just once at
compile-time. And it's already doing the analysis as part of its job.
It can avoid this problem easily:

```cl
(overcapture :x :some-big-value)
;; => #[0 "\300\207" [:x] 1]
```

It's clear that `:some-big-value` isn't present in the closure.

But… how does this work?

### How byte compiled closures are constructed

Recall from the [internals article][bc] that the four core elements of a
byte-code function object are:

1. Parameter specification
2. Byte-code string (opcodes)
3. Constants vector
4. Maximum stack usage

While a closure *seems* like compiling a whole new function each time
the lambda expression is evaluated, there's actually not that much to
it! Namely, [the *behavior* of the function remains the same][c]. Only
the closed-over environment changes.

What this means is that closures produced by a common lambda
expression can all share the same byte-code string (second element).
Their bodies are identical, so they compile to the same byte-code.
Where they differ are in their constants vector (third element), which
gets filled out according to the closed over environment. It's clear
just from examining the outputs:

```cl
(constantly :a)
;; => #[128 "\300\207" [:a] 2]

(constantly :b)
;; => #[128 "\300\207" [:b] 2]

```

`constantly` has three of the four components of the closure in its own
constant pool. Its job is to construct the constants vector, and then
assemble the whole thing into a byte-code function object (`#[...]`).
Here it is with `M-x disassemble`:

    0       constant  make-byte-code
    1       constant  128
    2       constant  "\300\207"
    4       constant  vector
    5       stack-ref 4
    6       call      1
    7       constant  2
    8       call      4
    9       return

(Note: since byte compiler doesn't produce perfectly optimal code, I've
simplified it for this discussion.)

It pushes most of its constants on the stack. Then the `stack-ref 5` (5)
puts `x` on the stack. Then it calls `vector` to create the constants
vector (6). Finally, it constructs the function object (`#[...]`) by
calling `make-byte-code` (8).

Since this might be clearer, here's the same thing expressed back in
terms of Elisp:

```cl
(defun constantly (x)
  (make-byte-code 128 "\300\207" (vector x) 2))
```

To see the disassembly of the closure's byte-code:

```cl
(disassemble (constantly :x))
```

The result isn't very surprising:

    0       constant  :x
    1       return

Things get a little more interesting when mutation is involved. Consider
this adder closure generator, which mutates its environment every time
it's called:

```cl
(defun adder ()
  (let ((total 0))
    (lambda () (cl-incf total))))

(let ((count (adder)))
  (funcall count)
  (funcall count)
  (funcall count))
;; => 3

(adder)
;; => #[0 "\300\211\242T\240\207" [(0)] 2]
```

The adder essentially works like this:

```cl
(defun adder ()
  (make-byte-code 0 "\300\211\242T\240\207" (vector (list 0)) 2))
```

*In theory*, this closure could operate by mutating its constants vector
directly. But that wouldn't be much of a *constants* vector, now would
it!? Instead, mutated variables are *boxed* inside a cons cell. Closures
don't share constant vectors, so the main reason for boxing is to share
variables between closures from the same environment. That is, they have
the same cons in each of their constant vectors.

There's no equivalent Elisp for the closure in `adder`, so here's the
disassembly:

    0       constant  (0)
    1       dup
    2       car-safe
    3       add1
    4       setcar
    5       return

It puts two references to boxed integer on the stack (`constant`,
`dup`), unboxes the top one (`car-safe`), increments that unboxed
integer, stores it back in the box (`setcar`) via the bottom reference,
leaving the incremented value behind to be returned.

This all gets a little more interesting when closures interact:

```cl
(defun fancy-adder ()
  (let ((total 0))
    `(:add ,(lambda () (cl-incf total))
      :set ,(lambda (v) (setf total v))
      :get ,(lambda () total))))

(let ((counter (fancy-adder)))
  (funcall (plist-get counter :set) 100)
  (funcall (plist-get counter :add))
  (funcall (plist-get counter :add))
  (funcall (plist-get counter :get)))
;; => 102

(fancy-adder)
;; => (:add #[0 "\300\211\242T\240\207" [(0)] 2]
;;     :set #[257 "\300\001\240\207" [(0)] 3]
;;     :get #[0 "\300\242\207" [(0)] 1])
```

This is starting to resemble object oriented programming, with methods
acting upon fields stored in a common, closed-over environment.

All three closures share a common variable, `total`. Since I didn't
use `print-circle`, this isn't obvious from the last result, but each
of those `(0)` conses are the same object. When one closure mutates
the box, they all see the change. Here's essentially how `fancy-adder`
is transformed by the byte compiler:

```cl
(defun fancy-adder ()
  (let ((box (list 0)))
    (list :add (make-byte-code 0 "\300\211\242T\240\207" (vector box) 2)
          :set (make-byte-code 257 "\300\001\240\207" (vector box) 3)
          :get (make-byte-code 0 "\300\242\207" (vector box) 1))))
```

The backquote in the original `fancy-adder` brings this article full
circle. This final example wouldn't work correctly if those lambdas
weren't evaluated properly.


[src]: https://old.reddit.com/r/emacs/comments/7h23ed/dynamically_construct_a_lambda_function/
[lex]: /blog/2016/12/22/
[bc]: /blog/2014/01/04/
[read]: /blog/2013/12/30/
[fast]: /blog/2017/01/30/
[advice]: /blog/2013/01/22/
[c]: /blog/2017/01/08/
