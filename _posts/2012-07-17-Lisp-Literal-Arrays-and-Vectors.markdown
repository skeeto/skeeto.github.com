---
title: Literal Arrays and Vectors in Lisp
layout: post
tags: [lisp, elisp]
uuid: 71e44f9e-2e92-30e9-8b29-8418229a7ce1
---

Despite being a Lisper, Unlike [Brian](http://www.50ply.com/) I
haven't gotten into Clojure yet. I've been following along at a safe
distance. Due to
[a recent post of his](http://www.50ply.com/blog/2012/07/06/asynchronous-sequential-code-shape/)
I learned about a significant difference between Clojure and other
Lisps when it comes to arrays/vectors.

In this recent post, Brian wrote a ClojureScript `let`-like macro to
hide JavaScript asynchronous function chains so that they can be used
just like regular synchronous functions. Follow Clojure's style, the
asynchronous functions are written inside a vector rather than a list
to indicate to the macro that they're special.

~~~clojure
(doasync
  [text [fetch "/foo/json"]
   url (str text ".html")
   result [fetch url]
   _ (.show view result)
   _ [timeout 1000]
   _ (.makeEditable view)])
~~~

That sounded completely reasonable to me, since array literals are
rarely used inside code Common Lisp. When they are used, it's as a
global constant.

A few days later when I was talking to Brian at the metaphorical water
cooler he mentioned that the macro was actually conflicting with what
he would normally write. Sometimes he really did want to use a vector
literal in a `let` binding. Why would he do that? In Common Lisp,
that's just asking for trouble — same for Elisp and Scheme.

~~~cl
(let ((v #(1 2 3)))
  (foo v))
~~~

The reason why this is a bad idea is that the *same exact* array will
always be passed to `foo`. The array is created once at *read time* by
the reader and re-used for the life of that code. If anyone makes a
modification to the array it will damage the array for everyone using
it.

~~~cl
(defun foo ()
  #(1 2 3))
(eq (foo) (foo))
=> T
~~~

The safer method is to create a fresh array every time by *not* using
a literal but instead calling `vector`.

~~~cl
(let ((v (vector 1 2 3)))
  (foo v))
~~~

Clojure data structures are immutable, including vectors, so using the
same exact vector in multiple places is safe. That makes use literal
vectors in code less awkward. But that still left a question hanging:
why was Brian using literal vectors so often that he needed one so
soon after writing this macro?

In Common Lisp, they're not very useful because the elements are not
evaluated by the parser. When this vector is evaluated the result is a
vector where the second element is a list containing three atoms.

~~~cl
#(1 (+ 2 3) 4)
=> #(1 (+ 2 3) 4)
~~~

Evaluated arrays return themselves unchanged. To do most useful
things, a fresh vector needs to be constructed piecemeal. If somehow
the uniqueness of a literal array wasn't an issue, they still couldn't
be used for much.

~~~cl
(defun foo (x)
  #(x x x))
(foo 10)
=> #(X X X)
~~~

To achieve the desired effect, the `vector` function needs to be used
again. Because it's a normal function call, the arguments are
evaluated.

~~~cl
(defun foo (x)
  (vector x x x))
(foo 10)
=> #(10 10 10)
~~~

However, to my surprise, Clojure doesn't work like this! Literal
vectors have their elements evaluated and, if necessary, are created
fresh on every use — exactly like a call to `vector`.

~~~clojure
(defn foo [x]
  [x x x])
(foo 10)
=> [10 10 10]
(identical? (foo 10) (foo 10))
=> false
~~~

If the exact form of the vector is needed unevaluated, it needs to be
quoted just like lists.

~~~clojure
(defn foo [x]
  '[x x x])
(foo 10)
=> [x x x]
(identical? (foo 10) (foo 10))
=> true
~~~

After further reflection, I now feel like this is the *right* way to
go about implementing vectors. When I was first learning Lisp the
non-evaluating nature of arrays really caught me by surprise. Vectors
should evaluate their elements by default; if the Common Lisp behavior
is needed it can always be quoted. It's impossible to "fix" any
established Lisp of course, so I'm merely wishing this was the
behavior defined decades ago.

To recap: normally in Lisp, vectors evaluate to themselves, like
numbers and strings. Instead, evaluation of a vector should return a
*new* vector containing the results of each of the element
evaluated. Since Clojure's data structures are immutable, the compiler
can take a shortcut when it can guarantee each of a vector's elements
always evaluate to themselves, and have the vector evaluate to itself
— purely as an optimization.
