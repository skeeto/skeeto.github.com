---
title: No, PHP Doesn't Have Closures
layout: post
date: 2019-09-25T21:10:43Z
tags: [lang, rant]
uuid: 286e6fd2-0532-4848-8d4a-10101d1ffa53
---

The PHP programming language is bizarre and, if nothing else, worthy of
anthropological study. The only consistent property of PHP [is how badly
it's designed][fractal], yet it somehow remains widely popular. There's
a social dynamic at play here that science has yet to unlock.

I don't say this because I hate PHP. There's no reason for that: I don't
write programs in PHP, never had to use it, and don't expect to ever
need it. Despite this, I just can't look away from PHP in the same way I
can't look away from a car accident.

I recently came across a link to the PHP manual, and morbid curiosity
that caused me to look through it. It's fun to pick an arbitrary section
of the manual and see how many crazy design choices I can spot, or at
least see what sort of strange terminology the manual has invented to
describe a common concept. This time around, one such section was on
[anonymous functions][anon], including closures. It was even worse than
I expected.

In some circumstances, closures can be a litmus test. Closure semantics
are not complex, but they're subtle and [a little tricky][let] until you
get hang of them. If you're interviewing a candidate, toss in a question
or two about closures. Either they're familiar and get it right away, or
they're unfamiliar and get nothing right. The latter is when it's most
informative. PHP itself falls clearly into the latter. Not only that,
the example of a "closure" in the manual demonstrates a "closure"
closing over a global variable!

I'd been told for years that PHP has closures, and I took that claim at
face value. In fact, PHP has had "closures" since 5.3.0, released in
June 2009, so I'm over a decade late in investigating it. However, as
far as I can tell, nobody's ever pointed out that PHP "closures" are, in
fact, not actually closures.

### Anonymous functions and closures

Before getting into why they're not closures, let's go over how it
works, starting with a plain old anonymous function. PHP *does* have
anonymous functions — the easy part.

```php
function foo() {
    return function() {
        return 1;
    };
}
```

The function `foo` returns a function that returns 1. In PHP 7 you can
call the returned function immediately like so:

```php
$r = foo()();  // $r = 1
```

In PHP 5 this is a syntax error because, well, it's PHP and its parser
is [about as clunky as Matlab's][matlab].

In a well-designed language, you'd expect that this could also be a
closure. That is, it *closes over* local variables, and the function may
continue to access those variables later. For example:

```php
function bar($n) {
    return function() {
        return $n;
    };
}

bar(1)();  // error: Undefined variable: n
```

This fails because you must explicitly tell PHP what variables you
intend to access inside the anonymous function with `use`:

```php
function bar($n) {
    return function() use ($n) {
        return $n;
    };
}

bar(1)();  // 1
```

If this actually closed over `$n`, this would be a legitimate closure.
Having to tell the language exactly which variables are being closed
over would be pretty dumb, but it still meets the definition of a
closure.

But here's the catch: It's not actually closing over any variables. The
names listed in `use` are actually extra, hidden parameters bound to the
current value of those variables. In other words, **this is nothing more
than partial function evaluation**.

```php
function bar($n) {
    $f = function() use ($n) {
        return $n;
    };
    $n++;  // never used!
    return $f;
}

$r = bar(1)();  // $r = 1
```

Here's the equivalent in JavaScript using [the `bind()` method][bind]:

```js
function bar(n) {
    let f = function(m) {
        return m;
    };
    return f.bind(null, n);
}
```

This is actually more powerful than PHP's "closures" since any arbitrary
expression can be used for the bound argument. In PHP it's limited to a
couple of specific forms. If JavaScript didn't have proper closures, and
instead we all had to rely on `bind()`, nobody would claim that
JavaScript had closures. It shouldn't be different for PHP.

### References

PHP *does* have references, and binding a reference to an anonymous
function is kinda, sorta like a closure. But that's still just partial
function evaluation, but where that argument is a reference.

Here's how to tell these reference captures aren't actually closures:
They work equally well for global variables as local variables. So it's
still not *closing over* a lexical environment, just binding a reference
to a parameter.

```php
$counter = 0;

function bar($n) {
    global $counter;
    $f = function() use (&$n, &$counter) {
        $counter++;
        return $n;
    };
    $n++;  // now has an effect
    return $f;
}

$r = bar(1)();  // $r = 2, $counter = 1
```

In the example above, there's no difference between `$n`, a local
variable, and `$counter`, a global variable. It wouldn't make sense for
a closure to close over a global variable.

### Emacs Lisp partial function application

Emacs Lisp famously didn't get lexical scope, and therefore closures,
[until fairly recently][lex]. It was — and still is by default — a
dynamic scope oddball. However, it's long had an `apply-partially`
function for partial function application. It returns a closure-like
object, and did so when the language didn't have proper closures. So it
can be used to create a "closure" just like PHP:

```lisp
(defun bar (n)
  (apply-partially (lambda (m) m) n))
```

This works regardless of lexical or dynamic scope, which is because this
construct isn't really a closure, just like PHP's isn't a closure. In
PHP, its partial function evaluation is built directly into the language
with special `use` syntax.

### Monkey see, monkey do

Why does the shell command language use sigils? Because it's built atop
interactive command line usage, where bare words are taken literally and
variables are the exception. Why does Perl use sigils? Because it was
originally designed as an alternative to shell scripts, so it mimicked
that syntax. Why does PHP use sigils? Because Perl did.

The situation with closures follows that pattern, and it comes up all
over PHP. Its designers see a feature in another language, but don't
really understand its purpose or semantics. So when they attempt to add
that feature to PHP, they get it disastrously wrong.


[anon]: https://www.php.net/manual/en/functions.anonymous.php
[bind]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_objects/Function/bind#Syntax
[fractal]: https://eev.ee/blog/2012/04/09/php-a-fractal-of-bad-design/
[let]: /blog/2014/06/06/
[lex]: /blog/2016/12/22/
[matlab]: /blog/2008/08/29/
