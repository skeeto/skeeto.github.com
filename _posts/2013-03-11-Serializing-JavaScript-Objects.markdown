---
title: Serializing JavaScript Objects
layout: post
tags: [javascript]
uuid: 2d56d74b-eb80-3627-4908-d20be7dfa04e
---

This year I'm participating in the annual
[Seven Day Roguelike Challenge][7drl], where participants are
attempting to create their own fully-playable [roguelike][rl] within
seven days. My entry is called [Disc RL][disc] ([play][play]), a
client-side browser roguelike.

Today's issue was saving the game state in [Local Storage][ls].
Otherwise the entire game would be lost if the tab was closed or the
page left for any reason. This would limit the possible depth of my
game, as any time investment could easily be lost. The issue is that
Local Storage only stores strings, so the game state must be
serialized and deserialized by my application.

You might say, "Use JSON!" The problem is that I'm using JavaScript's
object system, including polymorphism. The actual monsters are
prototypes of the various types of monsters, which are themselves
prototypes of the base monster prototype. Serializing a monster with
`JSON.stringify()` loses all this "class" information, so the
deserialized object will have no behavior.

~~~javascript
function Foo() {}

Foo.prototype.greet = function() {
    return "hello";
};

var foo1 = new Foo();
foo1.greet();
// => "hello"

var foo2 = JSON.parse(JSON.stringify(foo1));
foo2.greet();
// => TypeError: Object has no method 'greet'
~~~

Specifically what's not being captured here is the `__proto__`
property of the original object. When a property is not found on the
current object, `__proto__` is followed to check the next object in
the prototype chain, all the way up to Object. The `greet()` method is
found on the next item in the chain.

So the next suggestion might be, "Include `__proto__` in the JSON
string." The main obstacle here is **functions values cannot be
serialized**. More specifically, *closures* cannot be
serialized. Closures capture their environment but there is no way to
access this environment in order to serialize it. If the prototype has
methods, which it likely does, it can't be serialized.

Fortunately for my purposes I don't actually need to serialize any
objects with methods directly attached. I only need to ensure the
`__proto__` property points at the right prototype before I start
using the object.

~~~javascript
// Setting __proto__ directly:

foo2.__proto__ = Foo.prototype;
foo2.greet();
// => "hello"

// Or if your implementation doesn't support __proto__ (IE):

var foo3 = Object.create(Foo.prototype);
for (var p in foo2) {
    if (foo2.hasOwnProperty(p)) {
        foo3[p] = foo2[p];
    }
}
foo3.greet();
// => "hello"
~~~

Of course this one was really easy because there was only one
prototype in my example. If we deserialize an arbitrary object how do
we know which prototype to connect it to? We could attach this
information to the object before serializing it. We can't store the
prototype itself because we'd run into the same problem as
before. Instead, we want to attach the *name* of prototype. When the
object is restored we can use the name to look up the appropriate
prototype. Prototypes themselves don't have names but constructors
generally do.

~~~javascript
foo.constructor.name;
// => "Foo"
~~~

I'm going to stuff this name in the `"#"` property of the object, a
name that is unlikely to be used. A longer name has a better chance of
avoiding a collision, but since I'm putting this in localStorage, and
every stored object gets this field, I want to keep it short.

~~~javascript
function serialize(object) {
    object['#'] = object.constructor.name;
    return JSON.stringify(object);
}

function deserialize(string) {
    var object = JSON.parse(string);
    object.__proto__ = window[object['#']].prototype;
    return object;
}

var string = serialize(new Foo());
// string === "{\"#\":\"Foo\"}"

deserialize(string).greet();
// => "hello"
~~~

To look up the prototype I check for a global variable of that name on
the global object, which in this case is `window`. This places one
important restriction on how I use my serializer: all constructors
must have names and must be assigned to the corresponding global
variable. Being a prototype language this isn't necessarily the case!

~~~javascript
(function() {
    var Bar = function Quux() {};
    var bar = new Bar();
    return bar.constructor.name;
}());
// => "Quux"
~~~

Here, the Bar/Quux prototype isn't global nor does the attached name
(Quux) match the name I used with `new` (Bar). If `bar` was serialized
there would be no way to get a hold of its prototype.

So the two constraints so far are:

 * I need to consistently name my prototypes and store them in global
   variables.

 * I must never store a function in the property of any object I want
   to serialize.

Before I started on this today I was actually violating both of these
rules. It took a small amount of refactoring to meet these
conditions. Those people trying to be clever by using closures to hide
private fields on their objects will ultimately get stuck on the
second constraint.

To be useful, I need to recursively enter arrays and objects,
attaching prototype information to any other objects I come
across. This introduces one last constraint: no object can be
reachable more than once from my root object. If an object appears
more than once, it will be serialized twice and duplicated in the data
structure when deserialized. Worse, if I make a circular reference my
serialization function will never return.

To deal with this the serializer would need to keep track of what
objects it has seen and, when an object is seen again, a reference is
emitted instead. The deserializer, after reading in the entire
structure, would need to replace these references with the right
object. Fortunately for my roguelike no single object appears more
than once in my core data structure, so I don't need to worry about
this.

After discussing all this with [Gavin][gavin] he did a search an found
[HydrateJS][hydrate], a JavaScript library to do exactly all this,
including the object reference stuff I didn't need. Unfortunately this
library turned out to be way too buggy for me to use. Objects returned
by the parser are unable to be properly re-serialized again, so I
couldn't save, load, and save again.

I ended up writing my own functions to do what I needed:
[save.js][save]. It's not as capable as HydrateJS intends to be, but
it does everything I need, and my entire game state can safely go back
and forth between load and save arbitrarily many times. Most of the
complexity comes from just figuring out exactly what type a particular
thing is. This is frustratingly non-trivial in JavaScript.

It was really neat to see all this working so smoothly once I got it
in place. When I started my roguelike I wasn't sure if I could pull
off load/restore properly. You can see this system in action right now
at the play link at the top of the post. Play a little bit, close the
tab, then visit the page again. It *should* restore your game (note:
IE unsupported).

I might rip my serialization stuff out into its own library when I'm
done. I bet I'll find it useful again.


[disc]: https://github.com/skeeto/disc-rl
[7drl]: http://roguebasin.roguelikedevelopment.org/index.php?title=7DRL
[rl]: http://en.wikipedia.org/wiki/Roguelike
[play]: /disc-rl/
[ls]: http://diveintohtml5.info/storage.html
[hydrate]: http://nanodeath.github.com/HydrateJS/
[gavin]: http://www.devrand.org/
[save]: https://github.com/skeeto/disc-rl/blob/master/src/save.js
