---
title: Parameter Lists in Common Lisp and Clojure
layout: post
tags: [lisp, clojure]
uuid: 01182f77-6dfa-3448-2c3a-7bb428d5b430
---

Parameter lists in Common Lisp, called [lambda lists][lambda-list],
are written in their own mini-language, making it convenient to write
functions with a flexible call interface. A lambda list can specify
optional parameters, optionally with default arguments, named
(keyword) parameters, and whether or not the function is
variadic. It's something I miss a lot when using other languages,
especially JavaScript.

### Common Lisp Parameters

Here some some examples. This function, `foo`, has three required
parameters, `a`, `b`, and `c`.

~~~cl
(defun foo (a b c)
  ...)
~~~


To make `b` and `c` optional, place them after the symbol `&optional`,

~~~cl
(defun foo (a &optional b c)
  ...)
~~~


If second and third arguments are not provided, `b` and `c` will be
bound to `nil`. To provide a default argument, put that parameter
inside a list. Below, when a third argument is not provided, `c` will
be bound to `"bar"`.

~~~cl
(defun foo (a &optional b (c "bar"))
  ...)
~~~


To write a function that accepts any number of arguments, use `&rest`
followed by the parameter to hold the list of the remaining
arguments. Below, `args` will be a list of all arguments after the
third. Note how this can be combined with `&optional`.

~~~cl
(defun foo (a &optional b c &rest args)
  ...)
~~~


Often, the *position* of a parameter may be hard to remember or read,
especially if there are many parameters. It may be more convenient to
name them with `&key`. Below, the function has three named parameters,
specified at the call site using *keywords* — special symbols from
the keyword package that always evaluate to themselves.

~~~cl
(defun foo (&key a b c)
  ...)

(foo :b "world" :a "hello")
~~~

Like optional parameters, when a parameter is not provided it is bound
to `nil`. In the same way, it can be given a default argument.

~~~cl
(defun foo (&key (a "hello") (b "world") c)
  ...)
~~~

`&key` can be combined with `&optional` and `&rest`. However, the
`&rest` argument will be filled with all of key-value pairs, so it's
generally not useful to use them together.

Lambda lists are not exclusive to `defun` and can be used in any place
that needs to receive values in parameters, such as `flet` (*function*
let), `defmethod`, and so on.

### Clojure Parameters

Clojure forgoes these complex lambda lists in preference for
overloading by arity. When a function is being defined, multiple
functions of different arities can be defined at once. This makes for
optional parameters. Note how this leaves no room for a default
argument of `nil` for unspecified optional arguments.

Here, `b` is an optional parameter for `foo`, defaulting to `"bar"`
when not provided by the caller. The first definition has an arity of
one and it calls the second definition with the optional argument
filled in.

~~~clojure
(defn foo
  ([a] (foo a "bar"))
  ([a b] ...))
~~~

Variadic functions are specified with `&`, similar to `&rest` in
Common Lisp. Below, `xs` is a sequence of all of the arguments
provided after the first.

~~~clojure
(defn foo [x & xs]
  ...)
~~~

As far as *parameters* are concerned, this is all Clojure
has. However, Clojure's parameter specification is actually *more*
flexible than Common Lisp's lambda lists in two important ways. One is
that **parameter position can vary with the number of provided
arguments**. The Clojure core functions use this a lot (ex.
[`reduce`][reduce]).

The following in Common Lisp would require manually parsing the
parameters on some level. The `last` parameter can be either second or
third depending on whether a middle name was provided.

~~~clojure
(defn make-name
  ([first last]
     (make-name first "Q" last))
  ([first middle last]
     {:first first, :middle middle, :last last}))

(make-name "John" "Public")
;; => {:first "John", :middle "Q", :last "Public"}
~~~

That covers optional parameters with default arguments and variadic
functions. What about keyword parameters? Well, to cover that we need
to talk about *destructuring*, which is another way that Clojure
parameters are more powerful than lambda lists.

### Destructuring

A powerful Lisp idiom is destructuring bindings. Variables can be
bound to values in a structure by position in the structure. In Common
Lisp there are three macros for making destructuring bindings,
`destructuring-bind`, `loop` and `with-slots` (CLOS).

Below, in the body of the form, `a`, `b`, and `c` are bound to 1, 2,
and 3 respectively. The form `(a (b c))` is mapped into the quoted
structure of the same shape to the right.

~~~cl
(destructuring-bind (a (b c)) '(1 (2 3))
  (+ a (* b c)))
;; => 7
~~~

Because of Common Lisp's concept of *cons cells*, the *cdr* of a cell
can be bound to a variable if that variable appears in the `cdr`
position. This is similar to the `&rest` parameter (and is how Scheme
does variadic functions). I like using this to match the head and tail
of a list,

~~~cl
(destructuring-bind (x . xs) '(1 2 3 4 5)
  (list x xs))
;; => (1 (2 3 4 5))
~~~

Perhaps the neatest use of destructuring is in the `loop` macro. This
loop walks over a list two at a time, binding a variable to each side
of the pair,

~~~cl
(loop for (keyword value) on '(:a 1 :b 2 :c 3) by #'cddr
   collect keyword into keywords
   collect value into values
   finally (return (values keywords values)))
;; => (:A :B :C), (1 2 3)
~~~

Unfortunately destructuring in Common Lisp is limited to these few
cases, or where ever else you write your own destructuring macros.

Clojure takes destructuring to its logical conclusion: **destructuring
can be used any place bindings are established**! This includes
parameter lists. It works on any core data structure, not just lists.

Below, I'm doing destructuring inside of a standard `let` form.

~~~clojure
(defn greet-dr [fullname]
  (let [[first last] (clojure.string/split fullname #" +")]
    (str "Hello, Dr. " last ". "
         "It's good to see you again, " first ".")))

(greet-dr "John Doe")
;; "Hello, Dr. Doe. It's good to see you again, John."
~~~

Similarly, I could destructure an argument into my parameters. (Note
the double square brackets.)

~~~clojure
(defn greet-dr-2 [[first last]]
  ...)

(greet-dr-2 ["John" "Doe"])
~~~

Because hashmaps are a core language feature in Clojure, they can also
be destructured. The syntax is a bit like flipping the hashmap inside
out. The variable is specified, then the key it's mapped to.

~~~clojure
(let [{a :a, b :b} {:a 1 :b 2}]
  (list a b))
;; => (1 2)
~~~

When variables and keys have the same name, there's a shorthand with
`:keys`.

~~~clojure
(let [{:keys [a b]} {:a 1 :b 2}]
  ...)
~~~

Variables default to `nil` when the corresponding key is not in the
map. They can be given default values with `:or`.

~~~clojure
(let [{a :a, b :b :or {a 0 b 0}} {}]
  (list a b))
;; => (0 0)
~~~

Now, here's where it gets really neat. In Common Lisp, the `&key` part
of a lambda list is a special case. In Clojure it comes for free as
part of destructuring. Just destructure the *rest* argument!

~~~clojure
(defn height-opinion [name & {height :height}]
  (if-not height
    (str "I have no opinion on " name ".")
    (if (< height 6)
      (str name " is short.")
      (str name " is tall."))))

(height-opinion "Chris" :height 6.25)
;; => "Chris is tall."
~~~

We can still access the entire rest argument at the same time, using
`:as`, so it covers everything Common Lisp covers.

~~~clojure
(defn foo [& {a :a, b :b :as args}]
  args)

(foo :b 10)
;; => {:b 10}
~~~

(A side note while we're making comparisons: keywords in Clojure are
*not* symbols, but rather a whole type of their own.)

### Conclusion

Clojure parameter lists are simpler than Common Lisp's lambda lists
and, thanks to destructuring anywhere, they end up being *more
powerful* at the same time. It's a full super set of lambda lists, so
there's no practical trade-off.


[lambda-list]: http://www.lispworks.com/documentation/HyperSpec/Body/03_d.htm
[reduce]: http://clojuredocs.org/clojure_core/1.2.0/clojure.core/reduce
