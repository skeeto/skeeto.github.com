---
title: JavaScript Truthiness Quiz
layout: post
tags: [javascript, lang]
uuid: d621c4c0-9f17-3c95-d907-8e57d029dfe5
---

I've got another quirky JavaScript quiz for you. This one has two
different answers.

~~~javascript
function foo(object) {
    object.bar = false;
    return object.bar && true;
}

foo(___); // Fill in an argument such that foo() returns true.
~~~

Obviously a normal object won't do the job. Something more special is
needed.

~~~javascript
foo({bar: true});  // => false
~~~

The fact that `foo()` *can* return `true` could introduce a bug during
refactoring: the code initially appears to be a tautology that could
be reduced to a simpler `return false`. Since this quiz has solutions
that's obviously not true.

Had I reversed the booleans — assign `bar` to `true` and make this
function return a falsy value — then almost any immutable object,
such as a string, would do.

~~~javascript
function foo2(object) {
    object.bar = true;  // inverse
    return object.bar && true;
}

foo2("baz");  // => undefined
~~~

The `bar` assignment would fail and attempting to access it would
return `undefined`, which is falsy.

### Answer

The two approaches are *getters* and *property descriptors*.

#### Getters

JavaScript [directly supports getter and setter properties][getset].
Without special language support, such accessors could be accomplished
with plain methods (like Java).

~~~javascript
var lovesBlue = {
    _color: "blue",  // private

    getColor: function() {
        return this._color;
    },

    /* Only blue colors are allowed! */
    setColor: function(color) {
        if (/blue/.test(color)) {
            this._color = color;
        }
        return this._color;
    }
};

lovesBlue.getColor();              // => "blue"
lovesBlue.setColor("red");         // => "blue" (set fails)
lovesBlue.setColor("light blue");  // => "light blue"
~~~

JavaScript allows properties themselves to transparently run methods,
such as to enforce invariants, even though it's not an obvious call
site. This is how many of the browser environment objects
work. There's a special syntax with `get` and `set` keywords. (Keep
this mind, JSON parser writers!)

~~~javascript
var lovesBlue = {
    _color: "blue",

    get color() {
        return this._color;
    },

    set color(color) {
        if (/blue/.test(color)) {
            this._color = color;
        }
    }
};

lovesBlue.color = "red";  // => "red", but assignment fails
lovesBlue.color;          // => "blue"
~~~

This can be used to solve the quiz,

~~~javascript
foo({get bar() { return true; }});
~~~

Because `bar` is a getter with no associated setter, there's
effectively no assigning values to `bar` and it always evaluates to
`true`.

#### Property descriptors

Object properties [themselves have properties][define], called
*descriptors*, governing their behavior. The accessors above are
examples of the descriptors `get` and `set`. For our situation there's
a `writable` descriptor which determines whether or not a particular
property can be assigned. If you really wanted to lock this in,
there's even a metadescriptor (a *metameta*property?), `configurable`,
that determines whether or not a property's descriptors, including
itself, can be modified.

There's no literal syntax for them, but these descriptors can be set
with `Object.defineProperty()`. Conveniently, this function returns
the object being modified.

~~~javascript
foo(Object.defineProperty({}, 'bar', {value: true, writable: false}));
~~~

This creates a new object, sets `bar` to `true`, and locks that in by
making the property read-only.

The fact that attempting to assign a read-only property *silently*
fails instead of throwing an exception is probably another mistake in
the language's design. While this behavior newbie-friendly, it allows
bugs to slip by undetected, only to be found much later when they're
more expensive to address. It makes JavaScript programs more brittle.

#### Existing objects: a third approach?

If you're lazy and in a browser environment, you don't even need to
construct new objects to solve the problem. There are some already
lying around! My favorite is HTML5's `localStorage`. It stringifies
all property assignments. This means that `false` becomes `"false"`,
which is truthy.

~~~javascript
foo(localStorage);  // => true
~~~

This is arguably a third approach because the stringification behavior
can't be accomplished with either normal accessors or descriptors
alone.


[getset]: https://developer.mozilla.org/en-US/docs/JavaScript/Guide/Working_with_Objects#Defining_getters_and_setters
[define]: https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Global_Objects/Object/defineProperty
