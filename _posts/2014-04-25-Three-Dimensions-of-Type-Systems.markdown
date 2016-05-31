---
title: Three Dimensions of Type Systems
layout: post
date: 2014-04-25T22:03:01Z
tags: [lang]
uuid: b953054a-3681-3f91-a3a0-f075261f2e69
---

I occasionally come across articles, and even some books, that get
terminology mixed up when discussing type systems. The author might
say "strong" when what they're talking about is "lexical." In this
article I'll define three orthogonal properties of type systems. A new
programming language design could select from each category
independently.

### Static vs Dynamic

Static versus dynamic typing is probably the most obvious type system
property when glancing at an example of a language's source code. This
refers to whether or not variables have types.

In a statically typed language, variables have a compile-time
determined type. At run-time, a variable will only ever hold a value
of this type. Except where type information can be *inferred*,
variable and function declarations are generally accompanied by its
type (manifest). Type violations are generally discovered early, at
compile-time, at the cost of additional up-front planning.

In a dynamically typed language, only values have types. A variable
may be bound to any type of value. Though a smart compiler may reason
about a program enough to know that certain variables are only ever
bound to a limited set of types. Type violations are generally
discovered late, at run-time, but this allows for more ad hoc design.

Statically typed languages: C, C++, Java, C#, Haskell

Dynamically typed languages: Python, Ruby, JavaScript, Lisp, Clojure

To give a quick comparison, here's the same function definition in C
and JavaScript.

~~~c
double distance(struct point a, struct point b) {
    return sqrt(pow(a.x - b.x, 2) + pow(a.y - b-y, 2));
}
~~~

~~~javascript
function distance(a, b) {
    return Math.sqrt(Math.pow(a.x - b.x, 2) + Math.pow(a.y - b.y, 2));
}
~~~

Dynamic type systems are more apt for *duck typing*, though there are
exceptions such as [with C++ templates][duck]. In the JavaScript
example above, anything that has numeric `x` and `y` properties can be
passed to `distance`. The actual type doesn't matter. In the C
version, only that very specific type, `struct point`, may be passed
to `distance`.

The C example could be made more generic, circumventing its type
system through a trick called [*type punning*][pun]. This is where a
value is accessed by the program as though it was a different type of
value. This requires up-front planning and may potentially
[violate *strict aliasing*][undef].

### Lexical vs. Dynamic Scope

Lexical versus dynamic scope refers not to any values or objects
themselves, but rather to how variables are accessed. Virtually all
popular programming languages in use today use lexical scope. This is
because dynamic scope has serious performance and correctness
problems. In fact, it was likely invented entirely by accident.

However, it's still useful to use dynamic scope on a careful, opt-in
process. Perl, Common Lisp, Clojure, and Emacs Lisp all permit
selective dynamic scope. It's a clean method for temporarily masking a
global variable, such as the default stream for reading/writing
input/output.

Under lexical scope, the scope of a variable is determined statically
at compile-time. The compiler knows about all accesses to a particular
variable. This is sometimes called static scope but I'm using the word
lexical here to help differentiate from static typing (above).

In dynamic scope, all variables are essentially global variables. A
new binding masks any existing global binding for any functions called
from within that binding's extent. If a function accesses a *free
variable*, it's not known until run-time from where that value may
come. When the last binding is removed, such as when a local
variable's scope exited, that global variable is then said to be
*unbound*. Dynamic scope is incompatible with closures.

Lexically scoped languages: C, C++, Java, JavaScript, Python, Ruby, (many more)

Dynamically scoped languages: Emacs Lisp, bash

As of Emacs 24, lexical scope can be enabled by default for a
file/buffer by setting `lexical-binding` to true. I imagine this will
some day become the default, making Emacs Lisp a lexically scoped
language. This is also a perfect example of lexical scope having
better performance:
[turning on lexical scope makes Elisp programs run faster][lexperf].

Here's an example of dynamic scope in Emacs Lisp.

~~~cl
(defun reveal ()
  x) ; free variable

(defun foo ()
  (let ((x :foo))
    (reveal)))

(defun bar ()
  (let ((x :bar))
    (reveal)))

(foo)
;; => :foo

(bar)
;; => :bar
~~~

The value of `x` as seen by `reveal` depends on which function called
it, since the binding leaks through. Running the exact same code in
Common Lisp, where it's lexically scoped, would result in a run-time
error. It always tries to access `x` as a global variable. The scope
of `x` is strictly limited to `foo` or `bar`.

### Strong vs. Weak

Strong versus weak is probably the least understood property. Strong
typing is often mixed up with static typing despite being an
orthogonal concept. A language can be strongly, dynamically typed
(Python) — or weakly, statically typed (C). Strong/weak is also
sometimes confused with *type safety*.

This aspect refers to the tendency of values to be implicitly coerced
into another type depending on how they are used. Unlike the previous
two type system properties, this one isn't bimodal. There's a degree
to just how much implicit coercion occurs.

Strongly typed languages: Python, Common Lisp, Java, Ruby

Weakly typed languages: JavaScript, PHP, Perl, C

For example, take the following expression.

 * `"8" - 5`

In strongly typed languages this will generally be an error: strings
have no definition with the subtraction operator. In weakly typed
languages, the "8" is likely to be parsed into a number as part of
being handled by the operator, with the expression evaluating to 3.

If a language has a triple equality operator (`===`), that's a dead
giveaway that it's weakly typed.

In the case of C, its pointers are what make it weakly typed. It's
easy to make a pointer to a value, then dereference it as a different
type (usually leading to *undefined behavior*).

This is another trade-off between safety and convenience. Modern
languages tend towards strong typing.

### Further Reading

Here are three more type system properties that weren't discussed.

 * Type-safe vs. Type-unsafe : the ability of the programmer to
   circumvent the language's type system. Like strong versus weak,
   this one is a spectrum, where some languages are always safe, some
   offer some unsafe features, and some are very unsafe.

 * Typed vs. untyped : determines if types exist at all. Sometimes
   confused with static versus dynamic typing. Virtually all
   programming languages are typed. An example of untyped languages
   would be shell scripting languages where everything is a character
   string.

 * [Nominal][nominal] vs. [Structural][struct] : types are compatible
   depending on whether they are declared as such versus because they
   have similar structure (i.e. duck typing). C++ straddles this one
   with its template system.

Now a language can be summed up in six tokens (using the order I
presented them here). Unfortunately, applying the terms to languages
is somewhat subjective, especially when languages fall somewhere in
between, so not everyone will come to the same conclusions as I do
here.

 * **Common Lisp**: dynamic, lexical, strong, type-safe, typed, nominal
 * **Go**: static, lexical, strong, type-safe, typed, structural
 * **C**: static, lexical, weak, type-unsafe, typed, nominal
 * **JavaScript**: dynamic, lexical, weak, type-safe, typed, nominal
 * **Elisp**: dynamic, dynamic, strong, type-safe, typed, nominal
 * **bash**: n/a, dynamic, n/a, untyped, type-safe, n/a

*Update May 2014*: Exactly one day day after I posted this article,
Robert Smallshire made a video covering the same topic in the same
way: [The Unreasonable Effectiveness of Dynamic Typing for Practical
Programs](http://www.infoq.com/presentations/dynamic-static-typing).


[duck]: /blog/2014/04/01/
[pun]: http://en.wikipedia.org/wiki/Type_punning
[undef]: http://blog.llvm.org/2011/05/what-every-c-programmer-should-know.html
[lexperf]: /blog/2014/01/04/
[nominal]: http://en.wikipedia.org/wiki/Nominative_type_system
[struct]: http://en.wikipedia.org/wiki/Structural_type_system
