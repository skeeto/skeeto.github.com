---
title: Prototype-based Elisp Objects with @
layout: post
tags: [elisp, emacs]
uuid: d1361157-9022-3e77-270c-5410d903c7d4
---

**Reflection from the future**: *This library is super slow and
inefficient. It should probably not be used for anything serious.*

Last weekend I had the itch to play around with a multiple-inheritance
prototype-based object system in lisp. It would
[look a lot like JavaScript's object system][yegge] but wanted to try
experimenting some different ideas. My favorite lisp to hack in is
Emacs Lisp, so that's what I built it on. What I ended up with is
actually pretty neat. Despite the lack of reader macros in Elisp, I
still managed to introduce new syntax by manipulating symbols at
compile time.

 * [https://github.com/skeeto/at-el][at]

See the README for a quick demonstration. What follows is the long
explanation.

It's called [@][at], due to the syntax that it adds to Elisp as a
domain-specific language. It's a mini-language, really. The name is also
a challenge to the code that supports Elisp, because so much of it —
including emacs-lisp-mode and Paredit — doesn't properly handle @ in
identifiers. ~~Even [Maruku][maruku], the Markdown to HTML translator I
use for this blog, has bugs that won't allow it to handle the @
characters in my code, so I had to forgo most syntax highlighting for
this post.~~ (Update: I now use Kramdown so this is no longer an issue.)

Fortunately `require` *does* manage just fine.

~~~cl
(require '@)
~~~

Objects in @ are vectors with the symbol @ as the first element. The
rest of the elements are implementation specific, but, at the moment,
the second element is a plist (property list) of all of that object's
properties.

The root object of @ is @, and all other objects are instances of this
object, either directly or indirectly. Because it's prototype based,
creating a new object is a matter of extending one or more
(multiple-inheritance) existing objects. This is done with the
function `@extend`.

~~~cl
;; Create a brand new object
(defvar foo (@extend @))
~~~

If no objects are given to `@extend`, @ will be used as the parent
object, so it's not necessary as an argument above. This is actually
very important, as objects that don't inherit from @ will not work at
all! I'll get into that detail in a bit. Additionally, `@extend`
accepts keyword arguments, which become properties on the created
object.

The function @ is used to access properties on an object. Remember,
Elisp is a *lisp-2* meaning that variables and functions exist in
their own namespaces. This means there can be both a variable @ (the
root object) and function @ (property accessor).

~~~cl
(setf rectangle (@extend :width 3 :height 4))
(@ rectangle :width)  ; => 3
(@ rectangle :height)  ; => 4
~~~

The @ function is also *setf-able*, so setting properties should be
obvious to any lisper.

~~~cl
(setf (@ rectangle :width) 13)
(@ rectangle :width)  ; => 13
~~~

Like JavaScript, methods are just functions stored in properties on an
object. In @, the first argument for a method is the object itself,
which is called @@ by convention.

~~~cl
(setf (@ rectangle :area)
  (lambda (@) (* (@ @@ :width) (@ @@ :height))))

(funcall (@ rectangle :area) rectangle)  ; => 52
~~~

### New Syntax

Here's the first really neat part. I find all that `(@ @@ ...)`
business to be visually unpleasing. Fortunately this can be fixed by
adding syntax. The macro `def@` transforms variables that look like @:
into these @ accessors. The following declaration is equivalent to the
lambda assignment above. It's meant to be very convenient.

~~~cl
(def@ rectangle :area ()
  (* @:width @:height))
~~~

This macro walks the body of the function at compile-time (macro
expansion time) and transforms these symbols into the full @ calls
above. Like most lisp macros, this has *no* run-time performance cost.

Because using `funcall` all the time and remembering to pass the
object as the first argument is tedious, the @! function is provided
for calling methods.

~~~cl
(@! rectangle :area)  ; => 52
~~~

The @: variables become function calls when in function position.

~~~cl
(def@ rectangle :double-area ()
  (* 2 (@:area))
~~~

In a *lisp-1* this would happen for free, but in Elisp this situation
expands to the @! form.

### Inheritance

This `rectangle` is starting to look like a nice re-usable object.
There's a @ convention for this: prefix "class" object names with @.

~~~cl
(setf @rectangle rectangle)
~~~

Now to create new rectangle objects.

~~~cl
(setf foo (@extend @rectangle :width 3 :height 7.1))
(@! foo :area)  ; => 21.3
~~~

Notice that the `foo` object doesn't actually have an `:area` property
on itself. It was found on its parent, `@rectangle` by inheritance.
`:width` and `:height` were not looked up on the parent because
they're already bound on `foo`.

Here's another re-usable prototype. Notice that @: variables are
also setf-able — using `push` in this case.

~~~cl
(defvar @colored (@extend :color ()))

(def@ @colored :mix (color)
  (push color @:color))
~~~

The object system has multiple-inheritance, so colored rectangles can
be created from these two objects. The parent objects of an object are
listed in the `:proto` property as a list (similar to JavaScript's
`__proto__`), which can be modified at any time to change an object's
prototype chain.

~~~cl
(defvar foo (@extend @colored @rectangle :width 10 :height 4))

(@! foo :area)  ; => 40
(@! foo :mix :red)
(@! foo :mix :blue)
(@ foo :color)  ; => (:blue :red)
~~~

Even though the initial property was read from the parent, the
assignment (`push`), like all assignments, actually occurred on `foo`.

### Setters and Getters

Remember how I said that objects that don't eventually inherit from @
will be broken? This is because properties are actually set and
accessed through `:set` and `:get` methods. That is, @ calls these
methods as needed. The @ object provides the default actions for
these. An interesting part of the @ code: initially setting `:set` on
@ is a circularity problem, so there's a special bootstrap step to
accomplish it.

By providing your own you can fundamentally change how your object
works. For example, here's an `@immutable` mix-in which prevents all
property assignments. It's provided as part of @.

~~~cl
(defvar @immutable (@extend))

(def@ @immutable :set (property _value)
  (error "Object is immutable, cannot set %s" property))
~~~

This `:set` method will be found before the @ `:set` method, so it
gets overridden.

Remember how I said all object have a `:proto` that can be used to
modify the objects inheritance? This can be used to *freeze* an
object's properties in place. Here's a `:freeze` method for all
objects.

~~~cl
(def@ @ :freeze ()
  "Make this object immutable."
  (push @immutable @:proto))
~~~

Pretty cool, eh?

The `:get` method can be used to provide virtual properties.

~~~cl
(defvar @squares (@extend))

(def@ @squares :get (property)
  (if (numberp property)
      (expt property 2)
    (@^:get property)))  ; explained in a moment

(mapcar (lambda (n) (@ @squares n)) '(0 1 2 3 4))
; => (0 1 4 9 16)
~~~

I use this technique in the `@vector` class under `lib/` to expose the
elements of the internal vector as if they were properties.
[Brian][brian] used this trick to make a @buffer prototype that wraps
Emacs' buffers, with methods provided virtually by `:get`. For
example, the `:string` property would return a lambda that calls
`buffer-string`.

With multiple-inheritance and these setters and getters, there are a
lot of interesting mix-in possibilities. I'm only just discovering
some of them now.

### Supermethods

Sometimes it's really useful to call supermethods. There's syntax for
this: @^:. This calls the next method of that name in the prototype
chain. For example, here's a `@watchable` mix-in (also provided by @)
that allows other code to be notified of changes to an object. It
needs to override `:set` but still call the original `:set`.

~~~cl
(defvar @watchable (@extend :watchers nil))

(def@ @watchable :watch (callback)
  (push callback @:watchers))

(def@ @watchable :unwatch (callback)
  (setf @:watchers (remove callback @:watchers)))

(def@ @watchable :set (property new)
  (dolist (callback @:watchers)
    (funcall callback @@ property new))
  (@^:set property new))
~~~

This behavior is also used for constructors. By convention, the
`:init` method is the constructor. It should generally call the next
constructor with `(@^:init)`. @ has a no-op, no-argument `:init`
method to bottom-out this process.

~~~cl
(def@ @rectangle :init (width height)
  (@^:init)
  (setf @:width width @:height height))

(@! (@! @rectangle :new 13.2 2.1) :area) ; => 27.72
~~~

As shown, the `:new` method provided by the @ object combines both
`@extend` and `:init` to provide simple single-object inheritance.

### The Cost of @

In the lib/ directory there are a bunch of example objects
implemented: including @vector, @queue, @stack, and @heap. I found
these to be very enjoyable to write, and they've been the testing
grounds for @. @heap uses an internal @vector instance and exercises
@'s features the most.

The performance cost of @ very apparent with @heap. Even byte-compiled
it's slower than the naive implementation (compose `push` and `sort`)
for even as high as 1,000 elements. While I think @ leads to elegant
code, there's still plenty to do for performance. It's comically slow.

This really caught Brian's interest, because it was an opportunity to
put on his programming language designer's hat — which I believe to
be his favorite hat. He's been trying different caching strategies to
reduce all the walking of the prototype chain. This effort can be
found in the other repository branches and in his fork. The system is
so dynamic that cache invalidation is a really complex problem.

Every time a property is set, @ has to find the `:set` property for
that object, which generally means walking all the way up to @.
Because `:proto` can be modified at any time, every property look-up
requires computing the precedence order (lazily). This all makes
property assignment quite expensive! I can understand why real object
systems aren't this flexible. It comes at a high price.


[at]: https://github.com/skeeto/at-el
[brian]: http://50ply.com/
[maruku]: https://github.com/bhollis/maruku
[yegge]: http://steve-yegge.blogspot.com/2008/10/universal-design-pattern.html
