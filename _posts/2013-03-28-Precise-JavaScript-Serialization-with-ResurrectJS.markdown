---
title: Precise JavaScript Serialization with ResurrectJS
layout: post
tags: [javascript]
uuid: 6f9b879f-aeee-3a35-21ef-0f8b23444d0a
---

One of the problems I needed to solve while
[writing my 7DRL](/blog/2013/03/17/) was serializing the game's entire
state for a future restore. It needed to automatically save the game
so that the player could close their browser/tab and continue the game
later from where they left off. I attempted to use
[HydrateJS][hydrate], but finding it inadequate for the task I
[rolled my own solution from scratch](/blog/2013/03/11/).

After the week ended, I ripped my solution out of the game, filled in
the rest of the missing features, and created a precise serialization
library called [ResurrectJS][resurrect]. It can do everything
HydrateJS is meant to do and more: Dates, RegExps, and DOM objects.

 * [https://github.com/skeeto/resurrect-js][resurrect]

It works with all the major browsers, including You Know Who.

To demonstrate, here's another Greeter prototype.

~~~javascript
function Greeter(name) {
    this.name = name;
}
Greeter.prototype.greet = function() {
    return "Hello, my name is " + this.name;
};

var kelsey = new Greeter('Kelsey');
kelsey.greet();
// => "Hello, my name is Kelsey"
~~~

ResurrectJS can serialize `kelsey` for storage, including behavior.
I'm creating new Resurrect objects each time just to show that this
definitely works across different instances. Nothing up my sleeves!
There's no reason to avoid reusing Resurrect objects because the only
state they maintain between method calls is their configuration.

~~~javascript
var string = null;
string = new Resurrect().stringify(kelsey);
// => '[{"#":"Greeter","name":"Kelsey"}]'

kelsey;
// => {"name":"Kelsey","#id":null}
~~~

Notice that the serialization format is a bit unusual: it's wrapped in
an array. It's still a valid JSON encoding. Also notice that `kelsey`
gained a new property, `#id`, assigned to `null`, but this property
was not encoded. I'll explain all this below.

Here's object resurrection.

~~~javascript
var zombie = null;
zombie = new Resurrect().resurrect(string);
// => {"#":"Greeter","name":"Kelsey"}

zombie.greet();
// => "Hello, my name is Kelsey"

zombie === kelsey;  // A whole new object
// => false
~~~

The resurrected object has a `#` property.
[As explained before](/blog/2013/03/11/) this is used to link the
object back into the prototype chain so that its behavior is restored.

What's special now, which I didn't need in my game, is that
*identity*, including circularity, is properly maintained! For
example,

~~~javascript
var necromancer = new Resurrect();
necromancer.stringify([kelsey, kelsey]);
// => '[[{"#":1},{"#":1}],{"#":"Greeter","name":"Kelsey"}]'

var array = necromancer.resurrect(string);
array[0] === array[1];
// => true
~~~

The encoding should begin to reveal itself now. There's only one
Greeter object serialized and two `{'#': 1}` objects — references
into the top-level array.

### Identity and Equality Review

Just to make sure everyone's on the same page I'm going to go over the
difference between *identity* and *equality*. Identity is referential:
testing for it is effectively comparing memory pointers. Equality is
structural: testing for it walks the structures recursively.

In JavaScript the `===` operator tests equality for primitive values
(numbers, strings) and identity for objects.

~~~javascript
2 === 2;
// true, these values are equal

({foo: 2} === {foo: 2});
// false, these are equal but different object instances
~~~

JavaScript has no operator for testing object equality and writing a
function to do the job is surprisingly complicated.
[Underscore.js][underscore] has such a function and it's about 100
lines of code.

JSON maintains object equality but not object identity. Due to the
former it can be used to fake an equality test.

~~~javascript
Object.prototype.equals = function(that) {
    return JSON.stringify(this) === JSON.stringify(that);
};

({foo: 2}).equals({foo: 2});
// => true
~~~

However, keys are encoded in insertion order, so this is really
fragile. Bencode would be better suited (sorted keys), except that it
supports few of JavaScript's types.

~~~javascript
({a: 1, b: 2}).equals({b: 2, a: 1});
// => false (incorrect), due to ordering
~~~

ResurrectJS extends JSON to maintain identity as well as equality
across serialization. It does so through the use of references, as
explained below.

In functional languages, such as Haskell or most of Clojure, there is
little to no practical distinction between these two concepts. When
objects are immutable it makes no difference to a program if they are
identical. Everything is a *value*.

### Serialization Algorithm

The clever algorithm used by ResurrectJS was thought up by
[Brian][brian] while I discussing the problem with him. Unlike
HydrateJS, the serialized form doesn't follow the original structure's
form. While walking the data structure, copies of objects are placed
into an array as they're visited. When an object is first seen, it is
tagged with an `#id` property corresponding to the copy's position in
the array. If we come across an object with a non-null `#id` we know
we've seen it before and skip over it.

Most importantly, **these copies don't actually have any direct
references to other objects**. Instead these properties are replaced
with references to objects in other positions in the array. Primitive,
non-object values aren't referenced like this. They're left in place
and encoded as part of the object copy.

I'm using the word "object" broadly here to include Arrays. Objects
and Arrays are *composite* and everything else are *atoms*/*values*.
Composites are things that are made up of atoms, so they need to be
taken apart before serialization.

When walking the data structure is complete, the program walks the
copy array and sets the `#id` properties of the *original* objects to
`null`. This prevents them from being mistaken as already-visited by
future ResurrectJS walks (and this was one of HydrateJS's flaws). If
the `cleanup` config option is set to `true`, these `#id` properties
are completely removed with `delete`, which has performance
implications for those objects.

Finally, the copy array is serialized by JSON.stringify(). That's it!
Because objects are identified by their position in the copy array
they don't need identifiers attached to them when encoded.

In the array example above `{'#': 1}` is a reference to the second
object in the copy array. The first object in the copy array is the
original array being serialized. For example, here's a circular
reference,

~~~javascript
var circle = [];
circle.push(circle);
necromancer.stringify(circle);
// => '[[{"#":0}]]'
~~~

The first object in the copy array is an array that contains a
reference to itself.

JSON doesn't support `undefined` but I get it for free with this
scheme: any time I come across `undefined` I replace it with a
reference to the object at index -1. This will always be `undefined`!

~~~javascript
string = necromancer.stringify([undefined]);
// => '[[{"#":-1}]]'
~~~

### Deserialization

To deserialize, the string is parsed as regular JSON resulting in the
final copy array, which is walked. Prototypes are properly linked and
references are replaced with the appropriate object from the array. The
first object in the array is the root of the data structure (the very
first object seen during serialization), so it is returned as the
result. Simple!

### Special Values

ResurrectJS handles Dates automatically, treating them as *atomic
values*.

~~~javascript
var object = {date: new Date(Math.pow(10, 12))};
string = necromancer.stringify(object);
// => '[{"date":{"#.":"Date","#v":["2001-09-09T01:46:40.000Z"]}}]'

necromancer.resurrect(string).date.toString();
// => "Sat Sep 08 2001 21:46:40 GMT-0400 (EDT)"
~~~

When the program comes across one of these special values, a
"constructor" object is placed in the copy. On deserialization, not
only are references restored, but special values are also
reconstructed. The `#.` field indicates the constructor and the `#v`
field provides the constructor's arguments as an array.
[Applying a constructor](/blog/2013/03/24/) was a non-trivial issue.

A consequence of treating Dates as values is that ResurrectJS doesn't
maintain their identity. Having the same Date in two places on a data
structure will result in two different date objects after
deserialization.

~~~javascript
var date = new Date();
string = necromancer.stringify([date, date]);
array = necromancer.resurrect(string);
array[0] === array[1];
// => false
~~~

If the user was intending on mutating the Date and having it update
Dates (the same one) elsewhere in the structure, this will break it.
In my opinion, people mutating Dates deserve whatever is coming to
them.

Here's a RegExp being serialized,

~~~javascript
string = necromancer.stringify(/abc/i);
// => '{"#.":"RegExp","#v":["abc","i"]}'

necromancer.resurrect(string).test('abC');
// => true
~~~

If you were watching carefully you might notice there's no wrapper
array. If an atomic value is stringified directly then the copy array
process is not performed.

Here's one of the most interesting values to serialize: a DOM element.

~~~javascript
var h1 = document.createElement('h1');
h1.innerHTML = 'Hello';
necromancer.stringify(h1);
// => '{"#.":"Resurrect.Node","#v":["<h1>Hello</h1>"]}'

document.body.appendChild(necromancer.resurrect(string));
// (the heading appears on the page)
~~~

It uses [XMLSerializer][xml] to serialize the DOM element into XML.
The counterpart to XMLSerializer is [DOMParser][dom], but,
unfortunately, DOMParser is near useless. Instead I create a wrapper
`div`, shove the string in as `innerHTML`, and pull the DOM element
out. It works beautifully.

### Configuration

The Resurrect constructor accepts a configuration object. Check the
documentation for all of the details. It lets you control the prefix
used for the intrusive property names, in case you need `#` for
yourself. You can control prototype relinking and post-serialization
cleanup, as mentioned before.

~~~javascript
necromancer = new Resurrect({
    prefix: '__',
    cleanup: true,
    revive: false
});
~~~

I'm really quite proud of how this library turned out. *As far as I
know* it's the only library that can actually do all this. It's
tempting to pull it into [Skewer](/blog/2012/10/31/) so that it can
transmit data structures between Emacs Lisp and JavaScript as
perfectly as possible, but I'm afraid that the properties I'm adding
during serialization are too intrusive.


[hydrate]: http://nanodeath.github.com/HydrateJS/
[resurrect]: https://github.com/skeeto/resurrect-js
[brian]: http://50ply.com/
[xml]: https://developer.mozilla.org/en-US/docs/XMLSerializer
[dom]: https://developer.mozilla.org/en-US/docs/DOM/DOMParser
[underscore]: https://github.com/documentcloud/underscore
