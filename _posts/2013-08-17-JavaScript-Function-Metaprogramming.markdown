---
title: JavaScript Function Metaprogramming
layout: post
tags: [javascript]
uuid: c98de2cf-206f-3d7c-cf0c-6b05a06e62e8
---

The [JavaScript Function constructor][function] is useful
metaprogramming feature of JavaScript. It works like `eval`, treating
the contents of a string as code, but [without the quirkiness][eval].
The constructor's API looks like this,

    new Function([arg1[, arg2[, ... argN]],] functionBody)

For example, creating a 2-argument `add` function at run-time,

~~~javascript
var add = new Function('x', 'y', 'return x + y');
add(3, 5);  // => 8
~~~

In all of the JavaScript engines I'm aware of, functions created this
way are fully optimized and JITed just like any other, except that
this is done later. The function isn't established at compile-time but
at some point during run-time. The constructor could be implemented in
pure JavaScript using `eval`,

~~~javascript
/* Notice: not 100% correct, but close. */
function Function() {
    var args = Array.prototype.slice.call(arguments, 0);
    var body = args.pop();
    return eval('(function(' + args.join(', ') + ') { ' + body + ' })');
}
~~~

### Constructor Misuse

Misusing the Function constructor has the risk that you may invoke
compilation repeatedly. For example, both of these functions return an
array of adder functions.

~~~javascript
function literal() {
    var out = [];
    for (var i = 0; i < 10; i++) {
        out.push(function(x, y) { return x + y; });
    }
    return out;
}

function constructor() {
    var out = [];
    for (var i = 0; i < 10; i++) {
        out.push(new Function('x', 'y', 'return x + y'));
    }
    return out;
}
~~~

The `literal` function creates 10 unique closure objects.

~~~javascript
var x = literal();
x[0] === x[1];  // => false
~~~

While these appear to be unique objects, they're all backed by the
same code in memory. Since the function has no free variables, it
doesn't actually capture anything. These closures are really just
empty handles to the same function. This function will be recognized
and compiled ahead of time before `literal` is ever executed.

On the other hand, short of any serious optimization magic, the
`constructor` function version of this creates 10 unique backing
functions, invoking the compiler for each one individually at
*run-time*. If they're used enough to warrant it, each will also be
optimized separately. This is a misuse of the Function constructor, as
bad as misusing `eval`.

### What's it for?

As stated before, the Function constructor is useful for
metaprogramming. Use it to generate source code programmatically. For
example, this function generates 64 new functions by assembling source
code from strings.

~~~javascript
function opfuncs() {
    var ops = ['+', '-', '*', '/'];
    var names = ['a', 's', 'm', 'd'];
    var funcs = {};
    for (var i = 0; i < ops.length; i++) {
        for (var j = 0; j < ops.length; j++) {
            for (var k = 0; k < ops.length; k++) {
                var name = names[i] + names[j] + names[k],
                    body = ['w', ops[i], 'x', ops[j], 'y', ops[k], 'z'];
                funcs[name] = new Function('w', 'x', 'y', 'z',
                                           'return ' + body.join(''));
            }
        }
    }
    return funcs;
}
~~~

Writing out all these functions explicitly would take 66 lines of code
instead of just 16, and it would be error prone and more difficult to
maintain. Metaprogramming is a win here.

~~~javascript
/* Ugh ... */
var opfuncs = {
    aaa: function(w, x, y, z) { return w + x + y + z; },
    aas: function(w, x, y, z) { return w + x + y - z; },
    /* ... */
    ddd: function(w, x, y, z) { return w / x / y / z; }
};
~~~

The `opfuncs` function should be called exactly once. These functions
shouldn't be generated multiple times because the benefits of the
metaprogramming approach would be lost. To ensure that, I'm replacing
the function with its result in this example,

~~~javascript
opfuncs = opfuncs();
opfuncs.aaa(2,3,4,5); // 2+3+4+5 => 14
opfuncs.ama(2,3,4,5); // 2+3*4+5 => 19
~~~

The final metaprogrammed `opfuncs` object should completely
indistinguishable from the longer, explicit version.

### The Design Flaw

The primary flaw with the Function constructor is that it's variadic.
The whole purpose of this constructor is to dynamically generate
functions at run-time, but **there's (generally) no straightforward way
to call a constructor with a variable number of arguments**. The
[`apply` method can't directly compose with `new`][apply].

Say we want to write a version of `opfuncs` where, rather than
generate 64 4-argument functions, it generates 4^(n-1) n-argument
functions, taking an argument `n`. Now `new Function` needs to be
applied to a variable number of arguments (n + 1).

If rather than take argument names as individual arguments, Function
took them as an array, this would be straightforward. It would be like
having `apply` built-in. This is how I would have designed Function to
work.

    new Function(argNames, functionBody)

Used like this,

~~~javascript
var add = new Function(['x', 'y'], 'return x + y');
~~~

Fortunately there's a simple workaround. The built-in constructors do
something useful for the most part when used without `new`. My
personal favorite is the Boolean constructor. Without `new` it returns
a primitive boolean based on the truthiness of its argument. It can be
used to remove falsy values from an array.

~~~javascript
[1, '', 'foo', 0, null].filter(Boolean);
// => [1, "foo"]
~~~

In the case of Function, `new` isn't actually needed at all! This way,
`apply` can be used with the constructor. This works as expected,

~~~javascript
var add = Function.apply(null, ['x', 'y', 'return x + y']);
~~~

Here it is being used to generate functions of n arguments.

~~~javascript
/** Return a function of n-args that sums its arguments. */
function addN(n) {
    var args = [];
    for (var i = 0; i < n; i++) {
        args.push('a' + i);
    }
    args.push('return ' + args.join(' + '));
    return Function.apply(null, args);
}

addN(5)(3, 4, 5, 6, 7);  // => 25
~~~

A single variadic function that uses the `arguments` special variable
to sum its arguments could be used in place of these individual
functions, but generating a function with a specific arity and using
it many times will have much better performance than the variadic
version.

~~~javascript
function add() {
    var sum = 0;
    for (var i = 0; i < arguments.length; i++) {
        sum += arguments[i];
    }
    return sum;
}

function test(f, n) {
    var start = Date.now();
    for (var i = 0; i < n; i++) {
        f(3, 4, 5, 6, 7);
    }
    return (Date.now() - start) / 1000;
}

test(add,     10000000);  // => 0.698 seconds
test(addN(5), 10000000);  // => 0.152 seconds
~~~

In MonkeyScript, the metaprogramming approach is almost 5 times
faster.


[function]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Function
[eval]: /blog/2012/11/14/
[apply]: /blog/2013/03/24/
