---
title: JavaScript Debugging Challenge
layout: post
tags: [javascript, lang]
uuid: 51e6cdc8-6562-3ac7-e298-ddb0c989f81a
---

As I've been exploring JavaScript I've come up with a few interesting
questions that expose some of JavaScript's quirks. This is the first
one, which I came up with over the weekend. Study it and try to come
up with an answer before looking a the explanation below. Go ahead and
use a JavaScript interpreter or debugger to poke at it if you need to.

~~~javascript
var count = 4;

function foo() {
    var table = [count];

    /* Build the table. */
    while (count-- > 0) {
        table.push([]);
    }

    /* Fill it with numbers. */
    for (var count = 1; count < table.length; count++) {
        table[count].push(count);
    }
    return table;
}

foo(); // What does this return? And why?
~~~

When I originally came up with the problem, I just enabled
[impatient-mode][imp] in my editor buffer to share it friends. It's a
really convenient alternative to pastebins!

### Answer

If you've gotten this far either you figured it out or you gave up
(hopefully not right away!). Without careful attention, the expected
output would be `[4, [1], [2], [3], [4]]`. Create an array containing
`count`, push on `count` arrays, and finally iterate over the whole
thing. Seems simple.

However, the actual return value is `[undefined]`, which at first may
seem to defy logic. There's a bit of a double-trick to this question
due to the way I wrote it.

The first trick is that this might appear to be a quirk in the Array
`push()` method. If you pass an array to `push()` does it actually
concatenate the array, flattening out the result? If it did, pushing
an empty array would result in nothing. This is not the case,
fortunately.

~~~javascript
var foo = [1, 2, 3];
foo.push([]);  // foo = [1, 2, 3, []]
~~~

The real quirk here is JavaScript's strange scoping rules. JavaScript
only has *function* scope [^let], not *block* scope like most other
languages. A loop, including `for`, doesn't get its own scope so the
looping variables are actually hoisted into the function scope. For
the first two uses of `count`, it isn't actually a *free variable*
like it appears to be. It refers to the `for` loop variable `count`
even though it's declared later in the function.

A variable doesn't spring into existence at the place it's declared —
otherwise that would be a sort-of hidden nested scope. The binding's
extent is determined at *compile-time* (i.e. *lexical* scope). If the
variable is declared anywhere in the function with `var`, it is bound
for the entire body of the function. In contrast, C requires that
variables be declared before they are used. This isn't strictly
necessary from the compiler's point of view, but it keeps humans from
making mistakes like above. A C variable "exists" (barring
optimizations, it's been allocated space on the stack) for the entire
block it's declared in, but since it can't be referenced before the
declaration that detail has no visible effect.

In the code above, because `count` was not assigned any value at the
beginning of the function, it is initially bound to `undefined`, which
is coerced into `0` when used as a number. The result is that the
array is initially filled with `undefined`, then zero arrays are
pushed onto it. In the final loop, the array doesn't have any elements
to loop over so nothing happens and `[undefined]` is returned.


[^let]: JavaScript 1.7 actually has [block scope when using `let`][let], but `let` is not widely supported
yet.

[imp]: http://50ply.com/blog/2012/08/13/introducing-impatient-mode/
[let]: https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Statements/let
