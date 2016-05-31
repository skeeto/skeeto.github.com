---
title: Clojure-style Multimethods in Emacs Lisp
layout: post
date: 2013-12-18T23:06:15Z
tags: [emacs, elisp, clojure]
uuid: 029f9acb-a29f-3e58-14f3-457f245cdb5d
---

This past week I added [Clojure-style multimethods][clojure] to Emacs
Lisp through a package I call `predd` (predicate dispatch). **I
believe it is Elisp's very first complete *multiple dispatch* object
system!** That is, methods are dispatched based on the dynamic,
run-time type of [more than one of its arguments][multimethods].

 * [https://github.com/skeeto/predd](https://github.com/skeeto/predd)

(Unfortunately I was unaware of the
[other Clojure-style multimethod library][other] when I wrote mine.
However, my version is *much* more complete, has better performance,
and is public domain.)

As of version 23.2, Emacs includes a CLOS-like object system cleverly
named EIEIO. While CLOS (Common Lisp Object System) is multiple
dispatch, EIEIO is, like most object systems, only *single dispatch*.
The predd package is also very different than my other Elisp object
system, [@][at], which was prototype based and, therefore, also single
dispatch (and comically slow).

The [Clojure multimethods documentation][clojure] provides a good
introduction. The predd package works almost exactly the same way,
except that due to Elisp's lack of namespacing the function names are
prefixed with `predd-`. Also different is that the optional hierarchy
(`h`) argument is handled by the dynamic variable `predd-hierarchy`,
which holds the global hierarchy.

### Combination Example

To define a multimethod, pick a name and give it a *classifier
function*. The classifier function will look at the method's arguments
and return a *dispatch value*. This value is used to select a
particular method. What makes predd a multiple dispatch system is the
dispatch value can be derived from any number of methods arguments.
Because the dispatch value is computed at run-time this is called a
*late binding*.

Here I'm going to define a multimethod called `combine` that takes two
arguments. It combines its arguments appropriately depending on their
dynamic run-time types.

~~~cl
(predd-defmulti combine (lambda (a b) (vector (type-of a) (type-of b)))
  "Appropriately combine A and B.")
~~~

The classifier uses `type-of`, an Elisp built-in, to examine its
argument types. It returns them as tuple in the form of a vector. The
classifier of a method can be accessed with `predd-classifier`, which
I'll use to demonstrate what these dispatch values will look like.

~~~cl
(funcall (predd-classifier 'combine) 1 2)    ; => [integer integer]
(funcall (predd-classifier 'combine) 1 "2")  ; => [integer string]
~~~

I chose a vector for the dispatch value because I like the bracket
style when defining methods (you'll see below). The dispatch value can
be literally anything that `equal` knows how to compare, not just
vectors. Note that it's actually faster to create a list than a vector
up to a length of about 6, so this multimethod would be faster if the
classifier returned a list — or even better: a single cons.

Now define some methods for different dispatch values.

~~~cl
(predd-defmethod combine [integer integer] (a b)
  (+ a b))

(predd-defmethod combine [string string] (a b)
  (concat a b))

(predd-defmethod combine [cons cons] (a b)
  (append a b))
~~~

Now try it out.

~~~cl
(combine 1 2)            ; => 3
(combine "a" "b")        ; =>"ab"
(combine '(1 2) '(3 4))  ; => (1 2 3 4)

(combine 1 '(3 4))
; error: "No method found in combine for [integer cons]"
~~~

Notice in the last case it didn't know how to combine these two types,
so it threw an error. In this simple example where we're only calling
a single function, so rather than use the `predd-defmethod` macro
these methods can be added directly with the `predd-add-method`
function. This has the exact same result except that it has slightly
better performance (no wrapper functions).

~~~cl
(predd-add-method 'combine [integer integer] #'+)
(predd-add-method 'combine [string string]   #'concat)
(predd-add-method 'combine [cons cons]       #'append)
~~~

#### Use the Hierarchy

Hmmm, the `+` function is already polymorphic. It seamlessly operates
on both floats and integers. So far it seems there's no way to exploit
this with multimethods. Fortunately we can solve this by defining our
own ad hoc hierarchy using `predd-derive`. Both integers and floats
are a kind of number. It's important to note that `type-of` never
returns `number`. We're introducing that name here ourselves.

~~~cl
(type-of 1.0)  ; => float

(predd-derive 'integer 'number)
(predd-derive 'float 'number)

;; Types can derive from multiple parents, like multiple inheritance
(predd-derive 'integer 'exact)
(predd-derive 'float 'inexact)
~~~

This says that `integer` and `float` are each a kind of `number`. Now
we can use `number` in a dispatch value. When it sees something like
`[float integer]` it knows that it matches `[number number]`.

~~~cl
(predd-add-method 'combine [number number] #'+)

(combine 1.5 2)  ; => 3.5
~~~

We can check the hierarchy explicitly with `predd-isa-p` (like
Clojure's `isa?`). It compares two values just like `equal`, but it
also accounts for all `predd-derive` declarations. Because of this
extra concern, unlike `equal`, `predd-isa-p` is *not* commutative.

~~~cl
(predd-isa-p 'number 'number)  ; => 0
(predd-isa-p 'float 'number)   ; => 1
(predd-isa-p 'number 'float)   ; => nil

(predd-isa-p [float float] [number number])  ; => 2
~~~

(Remember that `0` is truthy in Elisp.) The integer returned is a
distance metric used by method dispatch to determine which values are
"closer" so that the most appropriate method is selected.

You might be worried that introducing `number` will make the
multimethod slower. Examining the hierarchy will definitely have a
cost after all. Fortunately predd has a dispatch cache, so
introducing this indirection will have *no* additional performance
penalty after the first call with a particular dispatch value.

### Struct Example

Something that really sets these multimethods apart from other object
systems is a lack of concern about encapsulation — or really about
object data in general. That's the classifier's concern. So here's an
example of how to combine predd with `defstruct` from cl/cl-lib.

Imagine we're making some kind of game where each of the creatures is
represented by an `actor` struct. Each actor has a name, hit points,
and active status effects.

~~~cl
(defstruct actor
  (name "Unknown")
  (hp 100)
  (statuses ()))
~~~

The `defstruct` macro has a useful inheritance feature that we can
exploit for our game to create subtypes. The parent accessors will
work on these subtypes, immediately providing some (efficient)
polymorphism even before multimethods are involved.

~~~cl
(defstruct (player (:include actor))
  control-scheme)

(defstruct (stinkmonster (:include actor))
  (type 'sewage))

(actor-hp (make-stinkmonster))  ; => 100
~~~

As a side note: this isn't necessarily the best way to go about
modeling a game. We probably shouldn't be relying on inheritance too
much, but bear with me for this example.

Say we want an `attack` method for handling attacks between different
types of monsters. Elisp structs have a very useful property by
default: they're simply vectors whose first element is a symbol
denoting its type. We can use this in a multimethod classifier.

~~~cl
(make-player)
;; => [cl-struct-player "Unknown" 100 nil nil]

(predd-defmulti attack
    (lambda (attacker victim)
      (vector (aref attacker 0) (aref victim 0)))
  "Perform an attack from ATTACKER on VICTIM.")
~~~

Let's define a base case. This will be overridden by more specific
methods (determined by that distance metric).

~~~cl
(predd-defmethod attack [cl-struct-actor cl-struct-actor] (a v)
  (decf (actor-hp v) 10))
~~~

We could have instead used `:default` for the dispatch value, which is
a special catch-all value. The `actor-hp` function will signal an
error for any victim non-actors anyway. However, not using `:default`
will force both argument types to be checked. It will also demonstrate
specialization for the example.

However, before we can make use of this we need to teach predd about
the relationship between these structs. It doesn't check `defstruct`
hierarchies. This step is what makes combining `defstruct` and predd
a little unwieldy. A wrapper macro is probably due for this.

~~~cl
(predd-derive 'cl-struct-player 'cl-struct-actor)
(predd-derive 'cl-struct-stinkmonster 'cl-struct-actor)

(let ((player (make-player))
      (monster (make-stinkmonster)))
  (attack player monster)
  (actor-hp monster))
;; => 90
~~~

When the stinkmonster attacks players it doesn't do damage. Instead it
applies a status effect.

~~~cl
(predd-defmethod attack [cl-struct-stinkmonster cl-struct-player] (a v)
  (pushnew (stinkmonster-type a) (actor-statuses v)))

(let ((player (make-player))
      (monster (make-stinkmonster)))
  (attack monster player)
  (actor-statuses player))
;; => (sewage)
~~~

If the monster applied a status effect in addition to the default
attack behavior then CLOS-style method combination would be far more
appropriate here (if only it was available in Elisp). The method would
instead be defined as an "after" method and it would automatically run
in addition to the default behavior.

If I was actually building a system combing structs and predd, I would
be using this helper function for building classifiers. It returns a
dispatch value for selected arguments.

~~~cl
;;; -*- lexical-binding: t; -*-

(defun struct-classifier (&rest pattern)
  (lambda (&rest args)
    (loop for select-p in pattern and arg in args
          when select-p collect (elt arg 0))))

;; Takes 3 arguments, dispatches on the first 2 argument types.
(predd-defmulti speak (struct-classifier t t nil))

;; Messages sent to the player are displayed.
(predd-defmethod speak '(cl-struct-actor cl-struct-player) (from to message)
  (message "%s says %s." (actor-name from) message))
~~~

### The Future

As of this writing there isn't yet a `prefer-method` for
disambiguating equally preferred dispatch values. I will add it in the
future. I think `prefer-method` gets unwieldy quickly as the type
hierarchy grows, so it should be avoided anyway.

I haven't put predd in MELPA or otherwise published it yet. That's
what this post is for. But I think it's ready for prime time, so feel
free to try it out.


[clojure]: http://clojure.org/multimethods
[multimethods]: http://en.wikipedia.org/wiki/Multimethods
[at]: /blog/2013/04/07/
[other]: https://github.com/kurisuwhyte/emacs-multi
