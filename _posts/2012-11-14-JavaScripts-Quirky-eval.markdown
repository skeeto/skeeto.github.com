---
title: JavaScript's Quirky eval
layout: post
tags: [javascript, lang]
uuid: 8676a0fa-c6ea-3231-717c-259c363be653
---

The infamous `eval` function is a strange beast in *any* language, but
I think JavaScript's is perhaps the strangest incarnation. Its very
presence in a function foils any possibility of optimization, because
it is capable of wreaking so much havoc.

The purpose of `eval` is to take an arbitrary data structure
containing a program (usually a string) and evaluate it. Most of the
time the use `eval` indicates a bad program — its use completely
unnecessary, very slow, and probably dangerous. There are exceptions,
like [Skewer](/blog/2012/10/31/), where a REPL is being provided to a
developer. *Something* needs to perform the "E" part of REPL.

If the language's platform already has a parser and
compiler/interpreter around, like an interpreted language, it's most
of the way to having an `eval`. `eval` just exposes the existing
functionality directly to programs. In a brute-force, trivial
approach, the string to be evaluated could be written to a file and
loaded like a regular program.

### Semantics

However, executing arbitrary code in an established context is
non-trivial. When a program is compiled, the compiler maps out the
program's various lexical bindings at compile time. For example, when
compiling C, a function's variables become offsets from the stack
pointer. As an optimization, unused variables can be discarded, saving
precious stack space. If the code calling `eval` has been compiled
like this and the evaluation is being done in the same lexical
environment as the call, then `eval` needs to be able to access this
mapping in order to map identifiers to bindings.

This complication can be avoided if the `eval` is explicitly done in
the global context. For example, take [Common Lisp's `eval`][lisp].

> Evaluates form in the current *dynamic environment* and the *null
> lexical environment*.

This means lexical bindings are not considered, only dynamic (global)
bindings. In the expression below, `foo` is bound lexically so `eval`
has no access to it. The compilation and optimization of this code is
unaffected by the `eval`. It's about as complicated as loading a new
source file with `load`.

~~~cl
(let ((foo 'bar))
  (eval 'foo))  ; error, foo is unbound
~~~

[Python][python] and [Ruby][ruby] are similar, where `eval` is done in
the global environment. In both cases, an evaluation environment can
be passed explicitly as an additional argument.

In [Perl][perl] things start to get a bit strange (string
version). `eval` *is* done in the current lexical
environment. <del>However, no assignments, either to change bindings
or modify data structures, are visible outside of the
<code>eval</code>.</del> (Fixed a string interpolation mistake.)

~~~perl
sub foo {
    my $bar = 10;
    eval '$bar = 5';
    return eval '$bar';
}
~~~

This function returns `5`. The `eval` modified the lexically scoped
`$bar`.

Note how short Lisp's `eval` documentation is compared to
Perl's. Lisp's `eval` semantics are dead simple — very important for
such a dangerous function. Perl's description is two orders of
magnitude larger than Lisp's and it still doesn't fully document the
feature.

### JavaScript

JavaScript goes much further than all of this. Not only is `eval` done
in the current lexical environment but **it can introduce entirely new
bindings!**

~~~javascript
function foo() {
    eval('var bar = 10');
    return bar;
}
~~~

This function returns 10. `eval` created a new lexical variable in
`foo` *at run time*. Because the environment can be manipulated so
drastically at run time, any hopes of effectively compiling `foo` are
thrown out the window. To have an outside function modify the local
environment is a severe side-effect. It essentially requires that
JavaScript be interpreted rather than compiled. Along with the `with`
statement, it's strong evidence that JavaScript was at some point
designed by novices.

`eval` also makes closures a lot heavier. Normally the compiler can
determine at compile time which variables are being accessed by a
function and minimize the environment captured by a closure. For
example,

~~~javascript
function foo(x) {
    var y = {x: x};
    return function() {
        return x * x;
    };
}
~~~

The function `foo` returns a closure capturing the bindings `x` and
`y`. The compiler can prove that `y` is never accessed by the closure
and omit it, [freeing the object][closure] bound to `y` for garbage
collection. However, if `eval` is present, *anything* could be
accessed at any time and the compiler can prove nothing. For example,

~~~javascript
function foo(x) {
    return function() {
        return eval('x * x');
    };
}
~~~

The variable `x` is never accessed lexically, but the `eval` can tease
it out at run time. The expression `foo(3)()` will evaluate to 9,
showing that anything exposed to the closure is not free to be garbage
collected as long as the closure is accessible.

If that's where the story ended, JavaScript optimization would look
pretty bleak. *Any* function call could be a call to `eval` and so any
time we call another function it may stomp all over the local
environment, preventing the compiler from proving anything useful. For
example,

~~~javascript
var secretEval = eval;
function foo(string) {
    // ...
    secretEval(string);
    // ...
}
~~~

There's good news and bad news. The good news is that this is *not*
the case in the above example. `string` will be evaluated in the
global environment, not the local environment. The bad news is that
this is because of a obscure, complicated concept of
[indirect and direct `eval`s][indirect].

In general, when `eval` is called by a name other than "`eval`" it is
an *indirect* call and is performed in the global environment (see the
linked article for a more exact description). This means the compiler
can tell at compile time whether or not `eval` will be evaluating in
the lexical environment. If not, it's free to make optimizations that
`eval` would otherwise prohibit. Whew!

### Strict mode

To address `eval`'s problems a bit further, along with some other
problems, ECMAScript 5 introduced [strict mode][strict]. Strict mode
modifies JavaScript's semantics so that it's a more robust and
compiler-friendly language.

In strict mode, `eval` still uses the local environment when called
directly but it gets its own nested environment. New bindings are
created in this nested environment, which is discarded when evaluation
is complete. JavaScript's `eval` is still quirky, but less so than
before.


[lisp]: http://www.lispworks.com/documentation/HyperSpec/Body/f_eval.htm
[python]: http://docs.python.org/2/library/functions.html#eval
[ruby]: http://ruby.about.com/od/advancedruby/a/Bindings.htm
[perl]: http://perldoc.perl.org/functions/eval.html
[indirect]: http://perfectionkills.com/global-eval-what-are-the-options/
[closure]: http://coding.smashingmagazine.com/2012/11/05/writing-fast-memory-efficient-javascript/
[strict]: https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Functions_and_function_scope/Strict_mode
