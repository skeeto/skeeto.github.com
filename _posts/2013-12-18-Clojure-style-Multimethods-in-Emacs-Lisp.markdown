---
title: Clojure-style Multimethods in Emacs Lisp
layout: post
date: 2013-12-18T23:06:15Z
tags: [emacs, lisp, clojure]
uuid: 029f9acb-a29f-3e58-14f3-457f245cdb5d
---

This past week I added [Clojure-style multimethods][clojure] to Emacs
Lisp through a package I call `multi`. **I believe it is Elisp's very
first *multiple dispatch* object system!** That is, methods are
dispatched based on the dynamic, run-time type of
[more than one of its arguments][multimethods].

 * [https://github.com/skeeto/multi](https://github.com/skeeto/multi)

As of version 23.2, Emacs includes a CLOS-like object system cleverly
named EIEIO. While CLOS (Common Lisp Object System) is multiple
dispatch, EIEIO is, like most object systems, only *single dispatch*.
The multi package is also very different than my other Elisp object
system, [@][at], which was prototype based and, therefore, also single
dispatch (and comically slow).

The [Clojure multimethods documentation][clojure] provides a good
introduction. The multi package works almost exactly the same way,
except that due to Elisp's lack of namespacing the function names are
prefixed with `multi-`. Also different is that the optional hierarchy
(`h`) argument is handled by the dynamic variable `multi-hierarchy`,
which holds the global hierarchy.

### Combination Example

To define a multimethod, pick a name and give it a *classifier
function*. The classifier function will look at the method's arguments
and return a *dispatch value*. This value is used to select a
particular method. What makes multi a multiple dispatch system is the
dispatch value can be derived from any number of methods arguments.
Because the dispatch value is computed at run-time this is called a
*late binding*.

Here I'm going to define a multimethod called `combine` that takes two
arguments. It combines its arguments appropriately depending on their
dynamic run-time types.

{% highlight cl %}
(multi-defmulti combine (lambda (a b) (vector (type-of a) (type-of b)))
  "Appropriately combine A and B.")
{% endhighlight %}

The classifier uses `type-of`, an Elisp built-in, to examine its
argument types. It returns them as tuple in the form of a vector. The
classifier of a method can be accessed with `multi-classifier`, which
I'll use to demonstrate what these dispatch values will look like.

{% highlight cl %}
(funcall (multi-classifier 'combine) 1 2)    ; => [integer integer]
(funcall (multi-classifier 'combine) 1 "2")  ; => [integer string]
{% endhighlight %}

I chose a vector for the dispatch value because I like the bracket
style when defining methods (you'll see below). The dispatch value can
be literally anything that `equal` knows how to compare, not just
vectors. Note that it's actually faster to create a list than a vector
up to a length of about 6, so this multimethod would be faster if the
classifier returned a list -- or even better: a single cons.

Now define some methods for different dispatch values.

{% highlight cl %}
(multi-defmethod combine [integer integer] (a b)
  (+ a b))

(multi-defmethod combine [string string] (a b)
  (concat a b))

(multi-defmethod combine [cons cons] (a b)
  (append a b))
{% endhighlight %}

Now try it out.

{% highlight cl %}
(combine 1 2)            ; => 3
(combine "a" "b")        ; =>"ab"
(combine '(1 2) '(3 4))  ; => (1 2 3 4)

(combine 1 '(3 4))
; error: "No method found in combine for [integer cons]"
{% endhighlight %}

Notice in the last case it didn't know how to combine these two types,
so it threw an error. In this simple example where we're only calling
a single function, so rather than use the `multi-defmethod` macro
these methods can be added directly with the `multi-add-method`
function. This has the exact same result except that it has slightly
better performance (no wrapper functions).

{% highlight cl %}
(multi-add-method 'combine [integer integer] #'+)
(multi-add-method 'combine [string string]   #'concat)
(multi-add-method 'combine [cons cons]       #'append)
{% endhighlight %}

#### Use the Hierarchy

Hmmm, the `+` function is already polymorphic. It seamlessly operates
on both floats and integers. So far it seems there's no way to exploit
this with multimethods. Fortunately we can solve this by defining our
own ad hoc hierarchy using `multi-derive`. Both integers and floats
are a kind of number. It's important to note that `type-of` never
returns `number`. We're introducing that name here ourselves.

{% highlight cl %}
(type-of 1.0)  ; => float

(multi-derive 'integer 'number)
(multi-derive 'float 'number)

;; Types can derive from multiple parents, like multiple inheritance
(multi-derive 'integer 'exact)
(multi-derive 'float 'inexact)
{% endhighlight %}

This says that `integer` and `float` are each a kind of `number`. Now
we can use `number` in a dispatch value. When it sees something like
`[float integer]` it knows that it matches `[number number]`.

{% highlight cl %}
(multi-add-method 'combine [number number] #'+)

(combine 1.5 2)  ; => 3.5
{% endhighlight %}

We can check the hierarchy explicitly with `multi-isa-p` (like
Clojure's `isa?`). It compares two values just like `equal`, but it
also accounts for all `multi-derive` declarations. Because of this
extra concern, unlike `equal`, `multi-isa-p` is *not* commutative.

{% highlight cl %}
(multi-isa-p 'number 'number)  ; => 0
(multi-isa-p 'float 'number)   ; => 1
(multi-isa-p 'number 'float)   ; => nil

(multi-isa-p [float float] [number number])  ; => 2
{% endhighlight %}

(Remember that `0` is truthy in Elisp.) The integer returned is a
distance metric used by method dispatch to determine which values are
"closer" so that the most appropriate method is selected.

You might be worried that introducing `number` will make the
multimethod slower. Examining the hierarchy will definitely have a
cost after all. Fortunately multi has a dispatch cache, so
introducing this indirection will have *no* additional performance
penalty after the first call with a particular dispatch value.

### Struct Example

Something that really sets these multimethods apart from other object
systems is a lack of concern about encapsulation -- or really about
object data in general. That's the classifier's concern. So here's an
example of how to combine multi with `defstruct` from cl/cl-lib.

Imagine we're making some kind of game where each of the creatures is
represented by an `actor` struct. Each actor has a name, hit points,
and active status effects.

{% highlight cl %}
(defstruct actor
  (name "Unknown")
  (hp 100)
  (statuses ()))
{% endhighlight %}

The `defstruct` macro has a useful inheritance feature that we can
exploit for our game to create subtypes. The parent accessors will
work on these subtypes, immediately providing some (efficient)
polymorphism even before multimethods are involved.

{% highlight cl %}
(defstruct (player (:include actor))
  control-scheme)

(defstruct (stinkmonster (:include actor))
  (type 'sewage))

(actor-hp (make-stinkmonster))  ; => 100
{% endhighlight %}

As a side note: this isn't necessarily the best way to go about
modeling a game. We probably shouldn't be relying on inheritance too
much, but bear with me for this example.

Say we want an `attack` method for handling attacks between different
types of monsters. Elisp structs have a very useful property by
default: they're simply vectors whose first element is a symbol
denoting its type. We can use this in a multimethod classifier.

{% highlight cl %}
(make-player)
;; => [cl-struct-player "Unknown" 100 nil nil]

(multi-defmulti attack
    (lambda (attacker victim)
      (vector (aref attacker 0) (aref victim 0)))
  "Perform an attack from ATTACKER on VICTIM.")
{% endhighlight %}

Let's define a base case. This will be overridden by more specific
methods (determined by that distance metric).

{% highlight cl %}
(multi-defmethod attack [cl-struct-actor cl-struct-actor] (a v)
  (decf (actor-hp v) 10))
{% endhighlight %}

We could have instead used `:default` for the dispatch value, which is
a special catch-all value. The `actor-hp` function will signal an
error for any victim non-actors anyway. However, not using `:default`
will force both argument types to be checked. It will also demonstrate
specialization for the example.

However, before we can make use of this we need to teach multi about
the relationship between these structs. It doesn't check `defstruct`
hierarchies. This step is what makes combining `defstruct` and multi
a little unwieldy. A wrapper macro is probably due for this.

{% highlight cl %}
(multi-derive 'cl-struct-player 'cl-struct-actor)
(multi-derive 'cl-struct-stinkmonster 'cl-struct-actor)

(let ((player (make-player))
      (monster (make-stinkmonster)))
  (attack player monster)
  (actor-hp monster))
;; => 90
{% endhighlight %}

When the stinkmonster attacks players it doesn't do damage. Instead it
applies a status effect.

{% highlight cl %}
(multi-defmethod attack [cl-struct-stinkmonster cl-struct-player] (a v)
  (pushnew (stinkmonster-type a) (actor-statuses v)))

(let ((player (make-player))
      (monster (make-stinkmonster)))
  (attack monster player)
  (actor-statuses player))
;; => (sewage)
{% endhighlight %}

If the monster applied a status effect in addition to the default
attack behavior then CLOS-style method combination would be far more
appropriate here (if only it was available in Elisp). The method would
instead be defined as an "after" method and it would automatically run
in addition to the default behavior.

If I was actually building a system combing structs and multi, I would
be using this helper function for building classifiers. It returns a
dispatch value for selected arguments.

{% highlight cl %}
;;; -*- lexical-binding: t; -*-

(defun struct-classifier (&rest pattern)
  (lambda (&rest args)
    (loop for select-p in pattern and arg in args
          when select-p collect (elt arg 0))))

;; Takes 3 arguments, dispatches on the first 2 argument types.
(multi-defmulti speak (struct-classifier t t nil))

;; Messages sent to the player are displayed.
(multi-defmethod speak '(cl-struct-actor cl-struct-player) (from to message)
  (message "%s says %s." (actor-name from) message))
{% endhighlight %}

### The Future

As of this writing there isn't yet a `prefer-method` for
disambiguating equally preferred dispatch values. I will add it in the
future. I think `prefer-method` gets unwieldy quickly as the type
hierarchy grows, so it should be avoided anyway.

I haven't put multi in MELPA or otherwise published it yet. That's
what this post is for. But I think it's ready for prime time, so feel
free to try it out.


[clojure]: http://clojure.org/multimethods
[multimethods]: http://en.wikipedia.org/wiki/Multimethods
[at]: /blog/2013/04/07/
