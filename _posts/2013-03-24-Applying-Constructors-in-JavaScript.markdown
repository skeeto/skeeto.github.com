---
title: Applying Constructors in JavaScript
layout: post
tags: [javascript]
uuid: c190935b-b654-3cb4-ade4-0ce9c19a46aa
---

I've [split off my JavaScript serialization library][resurrect], where
[deserialized objects maintain their prototype chain](/blog/2013/03/11/).
One of the problems I ran into was applying a provided constructor
function to an arbitrary number of arguments. Due to abstraction leaks
in the language's design, this turns out to be disappointingly more
complicated than I thought. Fortunately there's a portable workaround
hack that works.

The goal of this article is to define a `create` function to replace
the `new` operator. This function will take a constructor function and
an arbitrary number of arguments for the constructor. Below, these
pairs should have identical effects.

{% highlight javascript %}
new Greeter('Kelsey');
create(Greeter, 'Kelsey');

new RegExp('abc', 'i');
create(RegExp, 'abc', 'i');

new Date(0);
create(Date, 0);
{% endhighlight %}

### Function Application Review

Functions are full-fledged objects with their own methods, the three
most important of which being `call`, `apply`, and `bind`. The first
two invoke the function and the last one creates a new function.

`call` is used when a particular context (`this`) needs to be
explicitly set. The argument provided as the first argument will be
the context and the remaining arguments will be the normal function
arguments.

{% highlight javascript %}
function foo(a, b, c) {
    return [this, a, b, c];
}

foo.call(0, 1, "bar", 3);
// => [0, 1, "bar", 3]

foo.call(null, 1, "bar", 3);
// => [[object Window], 1, "bar", 3]
// => [null, 1, "bar", 3] (strict mode)
{% endhighlight %}

Normally, `null` and `undefined` cannot be passed as `this`: they will
automatically be replaced with the global object. In
[strict mode][strict], these values are passed directly as
`this`. Also, primitive types will be boxed -- wrapped in an object.

`apply` is exactly like `call`, but the arguments are provided as an
array. This is necessary for truly dynamic function calls since the
arguments aren't fixed.

{% highlight javascript %}
foo.apply(0, [1, "bar", 3]);
// => [0, 1, "bar", 3]
{% endhighlight %}

Finally, `bind` is also like `call` except that it performs *partial
application*. It returns a function with the context and initial
arguments locked in place.

{% highlight javascript %}
var bar = foo.bind(0, 1, "bar");
bar(3);
// => [0, 1, "bar", 3]
{% endhighlight %}

Notice how a call to `bind` looks like a call to `call`. The arguments
are provided individually. What if I wanted a `bind` that was shaped
like `apply`? That is, what if the arguments I want to lock in place
are listed in an array? Here's is the really cool part: `bind` itself
is a function, so it can be applied to the array of arguments.

{% highlight javascript %}
var baz = foo.bind.apply(foo, [0, 1, "bar"]);
baz(3);
// => [0, 1, "bar", 3]
{% endhighlight %}

This can be a little confusing because there are two contexts being
bound. The first context is the context for `bind`, which is the
function (`foo`) being partially applied. The second context is `this`
(0) being locked into place.

### Manual Object Construction

Generally to create a new object in JavaScript, the `new` operator is
applied to a constructor function. How it works is generally very
simple:

 1. Create a new object with the constructor's prototype (`__proto__`)
    as its prototype.
 2. Apply the constructor function to this object.

The first step can be accomplished with the relatively recent
Object.create() function. The second is just a normal function call.

{% highlight javascript %}
function Greeter(name) {
    this.name = name;
}

Greeter.prototype.greet = function() {
    return "Hello, " + this.name;
};

// Standard construction with the new operator
var greeter = new Greeter('Kelsey');

greeter.greet();  // => "Hello, Kelsey"

// Manual construction with Object.create()
var manual = Object.create(Greeter.prototype);
Greeter.call(manual, 'Kelsey');

manual.greet();  // => "Hello, Kelsey"
{% endhighlight %}

Above, `call` had to be used in order to set the context of the
call. Similarly, if there's an array of arguments, `apply` could be
used instead.

{% highlight javascript %}
Greeter.apply(manual, ['Kelsey']);
manual.greet();  // => "Hello, Kelsey"
{% endhighlight %}

### Getting it Right

The above doesn't *entirely* capture everything about the `new`
operator. Constructors are allowed to return an object (i.e. not a
primitive value) other than `this`, and that will be the newly
constructed object -- even if it's an entirely different type!

{% highlight javascript %}
function Foo() {
    return new Greeter('Chris');
}

new Foo().greet();  // => "Hello, Chris"
{% endhighlight %}

Here's the proper way to write `create`, assuming the language doesn't
throw a curve-ball at us in some corner case.

{% highlight javascript %}
function create(constructor) {
    var args = Array.prototype.slice.call(arguments, 1);
    var object = Object.create(constructor.prototype);
    var result = constructor.apply(object, args);
    if (typeof result === 'object') {
        return result;
    } else {
        return object;
    }
}

create(Greeter, 'Chris').greet();  // => "Hello, Chris"
create(Foo).greet();  // => "Hello, Chris"
{% endhighlight %}

### The Abstraction Leak

The above works with any JavaScript objects defined by the developer,
but, unfortunately, built in types have special privilege that
complicates their construction. It *does* work with RegExp,

{% highlight javascript %}
create(RegExp, 'abc', 'i').test('abC');  // => true
{% endhighlight %}

However, it does *not* work with Date or the other built in types,

{% highlight javascript %}
create(Date, 0).toISOString(); // => TypeError
{% endhighlight %}

There are two reasons for this: the built in types don't *actually*
use the prototype chain. **Object.create() cannot create built in
types!** Below I will use the `toString` method from the Object
prototype to see what the runtime *really* thinks these types are.

{% highlight javascript %}
function toString(object) {
    return Object.prototype.toString.call(object);
}

var fakeDate = Object.create(Date.prototype);
toString(fakeDate);  // => "[object Object]"
toString(new Date());  // => "[object Date]"
{% endhighlight %}

The object returned by Object.create() isn't actually a Date object as
far as the runtime is concerned. The same applies to Number, RegExp,
String, etc. If you try to call any methods on these objects, it will
throw a type error.

Worse, with the exception of RegExp, the built in type constructors
don't return objects either. The wrapper objects Boolean, Number, and
String return the primitive version of their arguments. Date returns a
primitive string.

{% highlight javascript %}
typeof Date(0);  // "string"
{% endhighlight %}

So **the *only* way to create a Date or these other built in types
(except RegExp) is through the `new` operator**. To loop back around
to the original problem: we have an array of arguments and a
constructor.  We want to `apply` the constructor to the array to
create a new object. The conflict is that **`new` and `apply` can't be
used at the same time**, so it would seem there's no way to write
generic `create` function that works with built in types without
explicitly making them a special case.

### The Workaround

Fortunately, [kybernetikos at Stack Overflow][hack] found an ingenious
solution to this. We *can* have our cake and eat it, too. We can mix
`new` and `apply` by hacking `bind`. It turns out to be a lot simpler
than the "proper" `create` definition above.

{% highlight javascript %}
function create(constructor) {
    var factory = constructor.bind.apply(constructor, arguments);
    return new factory();
};
{% endhighlight %}

It works with all the built in types, covering all the examples at the
top of the article.

{% highlight javascript %}
toString(create(Date, 0));  // => "[object Date]"
toString(create(RegExp, 'abc'));  // => "[object RegExp]"
create(Greeter, 'Kelsey').greet();  // => "Hello, Kelsey"
{% endhighlight %}

The bizarre thing here is that `new` still gets to break the
rules. Normally, `bind` locks in `this` permanently, so that it can't
be overridden even by `call`. Here's `foo` again demonstrating this.

{% highlight javascript %}
function foo(a, b, c) {
    return [this, a, b, c];
}

foo.bind(100).call(0, 1, 2, 3);  // => [100, 1, 2, 3]
{% endhighlight %}

The `factory` constructor in `create` already has `this` bound, but
`new` gets to override it anyway. Moreso -- and this is really
important for my purposes -- the constructor name survives this
process, through both the unofficial `name` property and `toString`
method! Normally functions returned by `bind` have no name.

{% highlight javascript %}
Greeter.bind(null).name;  // ""
create(Greeter, '').constructor.name;  // => "Greeter"
{% endhighlight %}

Thanks to this hack, this final version of `create` is essentially
what I'm using in [my library][resurrect] to reconstruct arbitrary
"value" objects. I'm lucky I came across it or I really would have
been stuck.


[resurrect]: https://github.com/skeeto/resurrect-js
[strict]: https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Functions_and_function_scope/Strict_mode
[hack]: http://stackoverflow.com/a/14378462
