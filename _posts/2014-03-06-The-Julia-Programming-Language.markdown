---
title: The Julia Programming Language
layout: post
date: 2014-03-06T23:55:44Z
tags: [rant, lang]
uuid: 35f29378-6e92-3d11-43df-9c40139f142e
---

*Update 2020: This is an old, outdated review. With the benefit of more
experience, I no longer agree with my criticsms in this article.*

Julia is a new programming language primarily intended for scientific
computing. It's attempting to take on roles that are currently
occupied by Matlab, its clones, and R. "Matlab done right" could very
well be its tag-line, but it's more than that. It has a beautiful type
system, it's homoiconic, and its generic function support would make a
Lisp developer jealous. It still has a long ways to go, but, except
for some unfortunate issues, it's off to a great start.

Speaking strictly in terms of the language, doing better than Matlab
isn't really a significant feat. Among major programming languages,
[Matlab's awfulness and bad design][matlab] is
[second only to PHP][php]. Octave fixes a lot of the Matlab language,
but it can only go so far.

For both Matlab and R, the real strength is the enormous library of
toolboxes and functionality available to help solve seemingly any
scientific computing task. Plus the mindshare and the community. Julia
has none of this yet. The language is mostly complete, but it will
take years to build up its own package library to similar standards.

If you're curious about learning more, the [Julia manual][manual]
covers the entire language as it currently exists. Unfortunately
anything outside the language proper and its standard library is
under-documented at this time.

### A Beautiful Type System

One of the first things you'll be told is that Julia is *dynamically
typed*. That is, statically typed (C++, Java, Haskell) versus
dynamically typed (Lisp, Python, JavaScript). However, Julia has the
rather unique property that it straddles between these, and it could
be argued to belong to one or the other.

The defining characteristic of static typing is that *bindings* (i.e.
variables) have types. In dynamic typing, only values and objects have
types. In Julia, all bindings have a type, making it like a statically
typed language. If no type is explicitly declared, that type is `Any`,
an abstract supertype of all types. This comes into play with generic
functions.

Both abstract and concrete types can be parameterized by other types,
and certain values. The `::` syntax it used to declare a type.

    type Point {T}
      x::T
      y::T
    end

This creates a `Point` constructor function. When calling the
constructor, the parameter type can be implicit, derived from the type
of its arguments, or explicit. Because both `x` and `y` have the same
type, so must the constructor's arguments.

    # Implicit type:
    Point(1, -1)
    # => Point{Int64}(1,-1)

    # Explicit type:
    Point{Float64}(1.1, -1.0)
    # => Point{Float64}(1.1,-1.0)

    Point(1, 1.0)
    # ERROR: no method Point{T}(Int64,Float64)

The type can be constrained using `<:`. If `Point` is declared like
the following it is restricted to real numbers. This is just like
Java's `Point<T extends Number>`.

    type Point {T <: Real}
      x::T
      y::T
    end

Unlike most languages, arrays aren't built directly into the language.
They're implemented almost entirely in Julia itself using this type
system. The special part is that they get literal syntax.

    [1, 2, 3]
    # => Array{Int64,1}

    [1.0 2.0; 3.0 4.0]
    # => Array{Float64,2}

Each Array is parameterized by the type of value it holds and by an
integer, indicating its rank.

#### The Billion Dollar Mistake

Julia has avoided what some call [The Billion Dollar Mistake][null]:
null references. In languages such as Java, `null` is allowed in place
of any object of any type. This allowance has lead to many run-time
bugs that, if `null` didn't exist, would have been caught at compile
time.

Julia has no `null` and so there's no way to make this mistake, though
some kinds of APIs are harder to express without it.

### Generic Functions

All of Julia's functions are *generic*, including that `Point`
constructor above. Different methods can be defined for the same
function name, but for different types. In Common Lisp and Clojure,
generic functions are an opt-in feature, so most functions are not
generic.

Note that this is significantly different than function *overloading*,
where the specific function to call is determined at compile time. In
multimethods, the method chosen is determined by the run-time type of
its arguments. One of Julia's notable achievements is that its
multimethods have very high performance. There's usually more of a
trade-off.

Julia's operators are functions with special syntax. For example, the
`+` function,

    +(3, 4)
    # => 7

A big advantage is that operators can be passed around as first-class
values.

    map(-, [1, 2, 3])
    # [-1, -2, -3]

Because all functions are generic, operators can have methods defined
for specific types, effectively becoming operator overloading (but
better!).

    function +(p1::Point, p2::Point)
      return Point(p1.x + p1.y, p2.x + p2.y)
    end

    Point(1,1) + Point(1, 2)
    # => Point{Int64}(2,3)

(Note that to write this method correctly, either `Point` or the
method should probably [promote][promote] its arguments.)

### Foreign Function Interface

Julia has a *really* slick foreign function interface (FFI). Libraries
don't need to be explicitly loaded and call interfaces don't have to
be declared ahead of time. That's all taken care of automatically.

I'm not going to dive into the details, but basically all you have to
do is indicate the library, the function, the return type, and then
pass the arguments.

    ccall((:clock, "libc"), Int32, ())
    # => 2292761

Generally this would be wrapped up nicely in a regular function and
the caller would have no idea an FFI is being used. Unfortunately
structs aren't yet supported.

### Julia's Problems

Not everything is elegant, though. There are some strange design
decisions. The two big ones for me are strings and modules.

#### Confused Strings

Julia has a `Char` type that represents a Unicode code point. It's a
32-bit value. So far so good. However, a `String` is *not* a sequence
of these. A Julia string is a byte-array of UTF-8 encoded characters.

Indexing into a string operates on *bytes* rather than characters.
Attempting to index into the middle of a character results in an
error. Yuck!

    "naïvety"[4]
    # ERROR: invalid UTF-8 character index

I don't understand why this behavior was chosen. This would make sense
if Julia was an old language and this was designed before Unicode was
established (e.g. C). But, no, this is a brand new language. There's
no excuse not to get this right the first time. I suspect it has to do
with Julia's FFI.

#### Clunky, Closed Modules

Julia's module system looks like it was taken right out of Scheme's
R6RS. This isn't a good thing.

The `module` definition that wraps the entire module up in a single
syntactic unit. Here's an example from the documentation. According to
the style guide, the body of the module is not indented out.

    module MyModule
    using Lib
    export MyType, foo

    type MyType
      x
    end

    bar(x) = 2x
    foo(a::MyType) = bar(a.x) + 1

    import Base.show
    show(io, a::MyType) = print(io, "MyType $(a.x)")
    end

That final `end` seals the module for good. There's no opening the
module back up to define or redefine new functions or types. If you
want to change something you have to reload the entire module, which
will obsolete any type instances.

Compare this to Clojure, where the module isn't wrapped up in a
syntactical construct.

    (ns my.module
      (require : [clojure.set :refer [rename-keys]]))

Common Lisp's `defpackage` also works like this. At any time you can
jump into a namespace and make new definitions.

    (in-ns 'my.module)

This is absolutely essential to [interactive development][skewer]. The
lack of this makes Julia *far* less dynamic than it should be.
Combined with the lack of a printer, **Julia is not currently suitable
as an interactive interpreter subprocess** (Slime, Cider, Skewer,
etc.).

This is a real shame, because I'd like to start playing around with
Julia, but right now it feels like a chore. It's needlessly restricted
to a C++/Java style workflow.

I'll probably revisit Julia once it's had a few more years to mature.
Then we'll see if things have improved enough for real use.


[manual]: http://julia.readthedocs.org/en/latest/manual/
[matlab]: /blog/2008/08/29/
[php]: http://old.reddit.com/r/lolphp
[null]: http://yinwang0.wordpress.com/2013/06/03/null/
[promote]: http://julia.readthedocs.org/en/latest/manual/conversion-and-promotion/
[skewer]: /blog/2012/10/31/
