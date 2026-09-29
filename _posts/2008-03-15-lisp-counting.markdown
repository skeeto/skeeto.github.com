---
title: Lisp Number Representations
layout: post
tags: [lisp, compsci]
uuid: 32c3088b-0e23-3299-0efe-03bbcdd5ce52
---

<!-- 15 March 2008 -->

This exercise partly comes from a couple different chapters in the
book [*The
Little Schemer*](http://www.ccs.neu.edu/home/matthias/BTLS/). The book is an introduction to the Scheme
programming language, a dialect of Lisp. The purpose to to teach basic
programming concepts in a way that anyone can follow along just as
well as someone with a degree in, say, computer science. It is still
very useful for us programmer types because there are some good
practice you get from reading and playing along.

First of all, Lisp is famous (infamous?) for lacking syntax. Any Lisp
program is simply
an [S-expression](http://en.wikipedia.org/wiki/S-expression),
put simply, a list of lists. There is no operator precedence because
operators are treated just like functions. This leads to prefix
notation for mathematical expressions,

```
(+ 4 5)
=> 9
```

where the `=>` indicates the result of evaluating the
expression. We can apply as many operands as we want,

```
(+ 2 3 4 5 10)
=> 24
```

We can put another list right in there as an operand,

```
(+ 3 (* 2 5) 4)
=> 17
```

You get the idea. In a function, the value of the last expression is
the return value. For example, here is the `square`
function in Scheme, which squares its input,

```
(define (square x)
  (* x x))
```

Then we can use it,

```
(+ (square 2) (square 5))
=> 29
```

There are three important list operators to understand as
well: `car`, `cdr`,
and `cons`. `car` returns the first element in a
list. In the example below, the `'`, a single quote, tells
the interpreter or compiler that the list is to be treated as data and
not to be executed. This is shorthand, or syntactic sugar, for
the `quote` operator: `(quote (stallman moglen))` is the same as `'(stallman moglen)`.

```
(car '(stallman moglen lessig))
=> stallman
```

`cdr` returns the "rest" of a list (everything *but*
the `car` of the list). When passing a list with only one
element `cdr` returns the empty list: `()`.

```
(cdr '(stallman moglen lessig))
=> (moglen lessig)
(cdr '(stallman))
=> ()
```

We can ask if a list is empty or not
with `null?`. `#t` and `#f` are true
and false.

```
(null? '(stallman moglen lessig))
=> #f
(null? '())
=> #t
```

And finally, for lists, we have `cons`. This function
allows us to build a list. It glues the first argument to the front of
the list in the second argument,

```
(cons 'stallman '(moglen lessig))
=> (stallman moglen lessig)
(cons 'stallman '())
=> (stallman)
```

And one last function you need to know: `eq?`. It
determines the two atoms are the same atom,

```
(eq? 'stallman 'moglen)
=> #f
(eq? 'stallman 'stallman)
=> #t
```

Now, for this exercise we will pretend that the basic arithmetic
functions have not been defined for us. Instead all we have
is `add1` and `sub1`, each of which adds or
subtracts 1 from its argument respectively.

```
(add1 5)
=> 6
(sub1 5)
=> 4
```

Oh, I almost forgot. We also have the `zero?` function
defined for us, which tells us if its argument is 0 or not. Notice
that functions that return true or false, called predicates, have
a `?` on the end.

```
(zero? 2)
=> #f
(zero? 0)
=> #t
```

To make things simple, these definitions will only consider positive
numbers. We can define the `+` function (for only two
arguments) in terms of the three basic functions shown above. It might
be interesting to try to write this yourself before you look any
further. (Hint: define it recursively!)

```
;; Adds together n and m
(define (+ n m)
  (if (zero? m) n
      (add1 (+ n (sub1 m)))))
```

If the second argument is 0 we are done and simply return the first
argument. If not, we add 1 to `n + (m - 1)`. The `-` function is defined similarly.

```
;; Subtracts m from n
(define (- n m)
  (if (zero? m) n
      (sub1 (- n (sub1 m)))))
```

Multiplication is the act of performing addition many times. We can go
on defining it in terms of addition,

```
(define (* n m)
  (if (zero? m) 0
      (+ n (* n (sub1 m)))))
```

(We'll leave division as an exercise for the reader as it gets a
little more complicated than I need to go in order to get my overall
point across.)

We will leave math behind for a moment take a look at
[The Roots of
Lisp](http://www.paulgraham.com/rootsofLisp.html). In that link is an excellent paper written by Paul Graham
about John McCarthy, the inventor (or perhaps discoverer?) of Lisp,
and how Lisp came to be. It turns out that in order to have a fully
functional Lisp engine we only need seven primitive operators:
operators defined outside of the language itself as building blocks
for the language. For Lisp these seven operators are (Scheme-ized for
our purposes): `eq?`, `atom?`, `car`,
`cdr`, `cons`, `quote`, and
`if`.

Notice how none of these are math operators. You may wonder how we can
possibly perform mathematical operations when we lack these
facilities. The answer: we have to define our own representation for
numbers! Let's try this, define a number as a list of empty lists. So,
the number 3 is,

```
'(() () ())
```

And here is 0, 2, and 4,

```
'()
'(() ())
'(() () () ())
```

See how that works? Before, when we wanted to define addition and
subtraction, we needed three other
functions: `zero?`, `add1`,
and `sub1`. With our number representation, how could we
define `add1` with our seven primitive operators? Our
numbers are defined as lists, so we can use our list operators. To add
1 to a number, we append another empty list. Hey, that sounds a lot
like `cons`!

```
(define (add1 n)
  (cons '() n))
```

Subtraction is removing an element from the list, which sounds a lot
like `cdr`,

```
(define (sub1 n)
  (cdr n))
```

And to define `zero?` we need to check for an empty
list. Notice this will also be the definition for `null?`.

```
(define (zero? n)
  (eq? '() n))
```

And now we are back where we started. In fact, you can use the exact
definitions above to define `+`, `-`,
and `*`. Our entire method number representation depends on
how we define `add1`, `sub1`,
and `zero?`. Let's try it out,

<pre>
;; 3 + 4
(+ '(() () ()) '(() () () ()))
=&gt; (() () () () () () ())

;; 5 - 2
(- '(() () () () ()) '(() ()))
=&gt; (() () ())

;; 2 * 2
(* '(() ()) '(() ()))
=&gt; (() () () ())

;; 3 + 4 * 2   bolded for clarity
(+ (* '(<b>() () () ()</b>) '(<b>() ()</b>)) '(<b>() () ()</b>))
=&gt; (() () () () () () () () () () ())
</pre>

Pretty cool, huh? We just added arithmetic (albeit extremely simple)
to our basic Lisp engine. With some modifications we should be able to
define and operate on negative integers and even define any rational
number (limited by how much memory your computer's hardware can
provide).

Now, thank goodness this isn't how real Lisp implementations actually
handle numbers. It would be incredibly slow and impractical, not to
mention annoying to read. Normally, numbers and math operators are
primitive so that they are fast.
