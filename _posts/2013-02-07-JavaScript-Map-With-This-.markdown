---
title: JavaScript "Map With This"
layout: post
tags: [javascript]
uuid: ab4dce26-0d77-34c5-34dc-e27a07bf4359
---

JavaScript has a handy `map()` function for mapping a function across
the elements of an array, producing a new array. It's part of
JavaScript's functional side.

~~~javascript
[1, 2, 3, 4, 5].map(function(x) { return x * x; });
// => [1, 4, 9, 16, 25]
~~~

Each array element is passed as the first argument to the
function. (Unfortunately, it [also passes two more arguments][map]:
the element's index and the array itself, but I'm not using those
here.) However, I sometimes find myself wishing there was a map-like
function that used the element as the *context* of the function. Then
I could apply a *method* to each of the elements in the array rather
than be limited to functions.

It's JavaScript so this could be added by adding a new map-like method
on the Array prototype, but these sorts of extensions are bad
practice.  Fortunately, there's a clean way to do this by building on
the `map()` method. Here's a function that produces an adaptor
function, which translates the arguments of a provided function into a
modified call to the provided function.

~~~javascript
function withThis(f) {
    var args = Array.prototype.slice.call(arguments, 1);
    return function(object) {
        return f.apply(object, args);
    };
}
~~~

Here, `withThis()` takes a variadic function and some arguments,
returning a new function that accepts one additional argument on the
left and partially applies the provided function to the provided
arguments. The first argument on the new function is used as the
context of a call to the provided function. Here's an example,

~~~javascript
[1, 2, 3].map(withThis(Number.prototype.toFixed, 2));
// => ["1.00", "2.00", "3.00"]
~~~

The expression `withThis(Number.prototype.toFixed, 2)` returns a
non-method version of `toFixed()`, partially applied to 2, which
operates on its first argument rather than `this`. It's well-suited to
be passed to `map()` or `filter()`.

One downside to this is that it's not polymorphic; it doesn't dispatch
on the type of the element. This can be fixed,

~~~javascript
function withThis(f) {
    var args = Array.prototype.slice.call(arguments, 1);
    return function(object) {
        return object[f].apply(object, args);
    };
}
~~~

~~~javascript
[1, new Date(), [1, 2, 3]].map(withThis('toString'));
// => ["1", "Thu Feb 07 2013 17:08:46 GMT-0500 (EST)", "1,2,3"]
~~~

It's also easier to call. Here's the same `toFixed()` example.

~~~javascript
[1, 2, 3].map(withThis('toFixed', 2));
// => ["1.00", "2.00", "3.00"]
~~~

I haven't tested it yet, but I'd bet the second version of
`withThis()` is a lot slower. It has to look up the actual method
using the string at run time when, in the first version, the
identifier is established at compile time. If this is the case, here's
a final version that does the right thing depending on what type of
argument is provided.

~~~javascript
function withThis(f) {
    var args = Array.prototype.slice.call(arguments, 1);
    if (typeof f === 'string') {
        return function(object) {
            return object[f].apply(object, args);
        };
    } else {
        return function(object) {
            return f.apply(object, args);
        };
    }
}
~~~


[map]: https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Global_Objects/Array/map
