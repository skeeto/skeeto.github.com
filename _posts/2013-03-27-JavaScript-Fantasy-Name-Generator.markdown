---
title: JavaScript Fantasy Name Generator
layout: post
tags: [javascript, game, interactive]
uuid: 8c5e22c4-f826-3b81-3f55-4ea60882620e
---

Also in preparation for [my 7-day roguelike](/blog/2013/03/17/) I
rewrote the [RingWorks fantasy name generator][namegen]
[in JavaScript][js]. It's my third implementation of this generator
and this one is also the most mature *by far*.

Try it out by [playing with the demo][demo] ([GitHub][js]).

The first implementation was written in Perl. It worked by
interpreting the template string each time a name was to be generated.
This was incredibly slow, partly because of the needless re-parsing,
but mostly because the parser library I used had really poor
performance. It's literally *millions* of times slower than this new
JavaScript implementation.

The [second implementation][lisp] I did in Emacs Lisp. I didn't
actually write a parser. Instead, an s-expression is walked and
interpreted for each name generation. Much faster, but I missed having
the template syntax.

The JavaScript implementation has a template *compiler*. There are
five primitive name generator prototypes — including strings
themselves, because anything with a toString() method can be a name
generator — which the compiler composes into a composite generator
following the template. The neatest part is that it's an optimizing
compiler, using the smallest composition of generators possible. If a
template can only emit one possible pattern, the compiler will try to
return a string of exactly the one possible output.

~~~javascript
typeof NameGen.compile('(foo) (bar)');
// => "string"
~~~

Here's the example usage I have in the documentation. On my junk
laptop it can generate a million names for this template in just under
a second.

~~~javascript
var generator = NameGen.compile("sV'i");
generator.toString();  // => "entheu'loaf"
generator.toString();  // => "honi'munch"
~~~

However, in this case there aren't actually that many possible
outputs. How do I know? You can ask the generator about what it can
generate. Generators know quite a bit about themselves!

~~~javascript
generator.combinations();
// => 118910

var foobar = NameGen.compile('(foo|bar)');
foobar.combinations();
// => 2
foobar.enumerate(); // List all possible outputs.
// => ["foo", "bar"]
~~~

After some experience using it in Disc RL I found that it would be
*really* useful to be mark parts of the output to the capitalized.
Without this, capitalization is awkwardly separate metadata. So I
extended the original syntax to do this. Prefix anything with an
exclamation point and it gets capitalized in the output.

For example, here's a template I find amusing. There are 5,113,130
possible output names.

    !BsV (the) !i

Here are some of the interesting output names.

    Quisey the Dork
    Cunda the Snark
    Strisia the Numb
    Pustie the Dolt
    Blhatau the Clown

Mostly as an exercise, I also added tilde syntax, which reverses the
component that follows it. So `~(foobar)` will always emit `raboof`. I
don't think this is particularly useful but having it opens the door
for other similar syntax extensions.

If you're making a procedurally generated game in JavaScript, consider
using this library for name generation!


[js]: https://github.com/skeeto/fantasyname
[namegen]: /blog/2009/01/04/
[lisp]: /blog/2009/07/03/
[demo]: /fantasyname/
