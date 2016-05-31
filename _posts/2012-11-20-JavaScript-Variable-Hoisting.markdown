---
title: Raising the Dead with JavaScript
layout: post
tags: [javascript, lang]
uuid: 8ba4b264-c7ce-33f5-d69f-51a6240a24b3
---

After my [last post](/blog/2012/11/19/), [Gavin][gavin] sent me this:
[Scope Cheatsheet][cheatsheet]. Besides its misleading wording, an
interesting fact stood out and gave me another JavaScript challenge
question. I'll also show you how it allows JavaScript to raise the
dead!

### Background

Like its close cousin, Scheme, JavaScript is a [Lisp-1][lisp-1]:
functions and variables share the same namespace. In Scheme, the
`define` form defines new variables.

~~~scheme
(define foo "Hello, world!")
~~~

Combine with the `lambda` form and it can be used to name functions,

~~~scheme
(define square (lambda (x) (* x x)))

(square -4)  ;; => 16
~~~

The variable `square` is assigned to an anonymous function, and
afterward it can be called as a function. Since this is so common,
there's a syntactic shorthand (*sugar*) for this,

~~~scheme
(define (square x) (* x x))
~~~

Notice that the first argument to `define` is now a list rather than a
symbol. This is a signal to `define` that a function is being defined,
and that this should be expanded into the `lambda` example
above. (Also note that the declaration mimics a function call, which
is pretty neat.)

JavaScript also has syntactic sugar for the same purpose. The `var`
statement establishes a binding in the current scope. This can be used
to define both variables and functions, since they share a
namespace. For convenience, in addition to defining an anonymous
function, the `function` statement can be used to declare a variable
*and* assign it a function. These definitions below are equivalent
... most of the time.

~~~javascript
var square = function(x) {
    return x * x;
}

function square(x) {
    return x * x;
}
~~~

The second definition is actually more magical than a syntactic
shorthand, which leads into my quiz.

### Quiz

~~~javascript
function bar() {
    var foo = 0;
    function foo() {}
    return typeof foo;
}

bar(); // What does this return? Why?

function baz() {
    var foo;
    function foo() {}
    return typeof foo;
}

baz(); // How about now?

function quux() {
    var foo = 0;
    var foo = function () {}
    return typeof foo;
}

quux(); // How about now?
~~~

We have three functions, `bar()`, `baz()`, and `quux()`, each slightly
different. Try to figure out the return value of each without running
them in a JavaScript interpreter. Reading the cheatsheet should give
you a good idea of the answer.

### Answer

Figured it out? The first function, `bar()`, is the surprising one. If
the special `function` form was merely syntactic sugar then all this
means is that `foo` is redundantly declared (and re-assigned before
accessing it, which the compiler could optimize). The final assignment
is a function, so it should return `'function'`.

However, *this is not the case!* This function returns `'number'`. The
first assignment listed in the code actually happens *after* the
second assignment, the function definition. This is because functions
defined using the special syntax are *hoisted* to the top of the
function. The function assignments are evaluated before any other part
of the function body. This is the extra magic behind the special
`function` syntax.

The effect is more apparent when looking at the return value of
`quux()`, which is `'function'`. The special `function` syntax isn't
used so the assignments are performed in the order that they're
listed. This isn't surprising, except for the fact that variables can
be declared multiple times in a scope without any sort of warning.

The second function, `baz()`, returns `'function'`. The function
definition is still hoisted but the variable declaration performs no
assignment. The function assignment is not overridden. Because of the
lack of assignment, nothing actually happens at all for the variable
declaration.

Now, this seems to be a cloudy concept for even skilled programmers: a
variable declaration like `var foo = 0` accomplishes *two* separate
things. The merge of these two tasks into a single statement is merely
one of convenience.

 1. **Declaration**: declares a variable, modifying the *semantics*
    of the function's body. It changes what *place* in memory an
    *identifier* in the current scope will refer to. This is a
    compile-time activity. Nothing *happens* at run time — there is
    no *when*. When function definitions are hoisted, it's the
    *assignment* (part 2) that gets hoisted. In C, variables are
    initially assigned to stack garbage (globals are zeroed). In
    JavaScript, variables are initially assigned to `undefined`.

 2. **Assignment**: *binds* a variable to a new value. This is
    evaluated at run time. It matters *when* this happens in relation
    to other evaluations.

Consider this,

~~~javascript
var foo = foo;
~~~

The expression on the right-hand side is evaluated in the same scope
as the variable declaration. `foo` is initially assigned to
`undefined`, then it is re-assigned to `undefined`. This permits
recursive functions to be defined with `var` — otherwise the
identifier used to make the recursive call wouldn't refer to the
function itself.

~~~javascript
var factorial = function(n) {
    if (n === 0)
        return 1;
    else
        return factorial(n - 1) * n;
};
~~~

In contrast, Lisp's `let` does not evaluate the right-hand side within
the scope of the `let`, so recursive definitions are not possible with
a regular `let`. This is the purpose of `letrec` (Scheme) and `labels`
(Common Lisp).

~~~cl
;; Compile error, x is unbound
(let ((x x))
  x)
~~~

### Why function hoisting?

JavaScript's original goal was to be easy for novices to program. I
think that they wanted users to be able to define functions anywhere
in a function (at the top level) without thinking about it. Novices
generally don't think of functions as values, so this is probably more
intuitive for them. To accomplish this, the assignment needs to happen
before the real body of the function. Unfortunately, this leads to
surprising behavior, and, ultimately, it was probably a bad design
choice.

Below, in any other language the function definition would be dead
code, unreachable by any valid control flow, and the compiler would be
free to toss it.

~~~javascript
function foo() {
    return baz();
    function baz() { return 'Hello'; }
}

foo(); // => 'Hello'
~~~

But in JavaScript you can raise the dead!


[gavin]: http://devrand.org/
[cheatsheet]: https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Scope_Cheatsheet
[lisp-1]: http://en.wikipedia.org/wiki/Common_Lisp#The_function_namespace
