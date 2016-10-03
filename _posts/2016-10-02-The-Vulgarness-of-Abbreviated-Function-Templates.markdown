---
title: The Vulgarness of Abbreviated Function Templates
layout: post
date: 2016-10-02T23:59:59Z
tags: [c, cpp, rant, lang]
uuid: 048f746a-de7f-3357-8409-cfd531363726
---

The `auto` keyword has been a part of C and C++ since the very
beginning, originally as a one of the four *storage class specifiers*:
`auto`, `register`, `static`, and `extern`. An `auto` variable has
"automatic storage duration," meaning it is automatically allocated at
the beginning of its scope and deallocated at the end. It's the
default storage class for any variable without external linkage or
without `static` storage, so the vast majority of variables in a
typical C program are automatic.

In C and C++ *prior to C++11*, the following definitions are
equivalent because the `auto` is implied.

~~~c
int
square(int x)
{
    int x2 = x * x;
    return x2;
}

int
square(int x)
{
    auto int x2 = x * x;
    return x2;
}
~~~

As a holdover from *really* old school C, unspecified types in C are
implicitly `int`, and even today you can get away with weird stuff
like this:

~~~c
/* C only */
square(x)
{
    auto x2 = x * x;
    return x2;
}
~~~

By "get away with" I mean in terms of the compiler accepting this as
valid input. Your co-workers, on the other hand, may become violent.

Like `register`, as a storage class `auto` is an historical artifact
without direct practical use in modern code. However, as a *concept*
it's indispensable for the specification. In practice, automatic
storage means the variables lives on "the" stack (or [one of the
stacks][safestack]), but the specifications make no mention of a
stack. In fact, the word "stack" doesn't appear even once. Instead
it's all described in terms of "automatic storage," rightfully leaving
the details to the implementations. A stack is the most sensible
approach the vast majority of the time, particularly because it's both
thread-safe and re-entrant.

### C++11 Type Inference

One of the major changes in C++11 was repurposing the `auto` keyword,
moving it from a storage class specifier to a a *type specifier*. In
C++11, the compiler **infers the type of an `auto` variable from its
initializer**. In C++14, it's also permitted for a function's return
type, inferred from the `return` statement.

This new specifier is very useful in idiomatic C++ with its
ridiculously complex types. Transient variables, such as variables
bound to iterators in a loop, don't need a redundant type
specification. It keeps code *DRY* ("Don't Repeat Yourself"). Also,
templates easier to write, since it makes the compiler do more of the
work. The necessary type information is already semantically present,
and the compiler is a lot better at dealing with it.

With this change, the following is valid in both C and C++11, and, by
*sheer coincidence*, has the same meaning, but for entirely different
reasons.

~~~c
int
square(int x)
{
    auto x2 = x * x;
    return x2;
}
~~~

In C the type is implied as `int`, and in C++11 the type is inferred
from the type of `x * x`, which, in this case, is `int`. The prior
example with `auto int x2`, valid in C++98 and C++03, is no longer
valid in C++11 since `auto` and `int` are redundant type specifiers.

Occasionally I wish I had something like `auto` in C. If I'm writing a
`for` loop from 0 to `n`, I'd like the loop variable to be the same
type as `n`, even if I decide to change the type of `n` in the future.
For example,

~~~c
struct foo *foo = foo_create();
for (int i = 0; i < foo->n; i++)
    /* ... */;
~~~

The loop variable `i` should be the same type as `foo->n`. If I decide
to change the type of `foo->n` in the struct definition, I'd have to
find and update every loop. The idiomatic C solution is to `typedef`
the integer, using the new type both in the struct and in loops, but I
don't think that's much better.

### Abbreviated Function Templates

Why is all this important? Well, I was recently reviewing some C++ and
came across this odd specimen. I'd never seen anything like it before.
Notice the use of `auto` for the parameter types.

~~~c++
void
set_odd(auto first, auto last, const auto &x)
{
    bool toggle = false;
    for (; first != last; first++, toggle = !toggle)
        if (toggle)
            *first = x;
}
~~~

Given the other uses of `auto` as a type specifier, this kind of makes
sense, right? The compiler infers the type from the input argument.
But, as you should often do, put yourself in the compiler's shoes for
a moment. Given this function definition in isolation, can you
generate any code? Nope. The compiler needs to see the call site
before it can infer the type. Even more, different call sites may use
different types. That **sounds an awful lot like a template**, eh?

~~~c++
template<typename T, typename V>
void
set_odd(T first, T last, const V &x)
{
    bool toggle = false;
    for (; first != last; first++, toggle = !toggle)
        if (toggle)
            *first = x;
}
~~~

This is **a proposed feature called *abbreviated function
templates***, part of [*C++ Extensions for Concepts*][concepts]. It's
intended to be shorthand for the template version of the function. GCC
4.9 implements it as an extension, which is why the author was unaware
of its unofficial status. In March 2016 it was established that
[abbreviated function templates **would *not* be part of
C++17**][reject], but may still appear in a future revision.

Personally, I find this use of `auto` to be vulgar. It overloads the
keyword with a third definition. This isn't unheard of — `static` also
serves a number of unrelated purposes — but while similar to the
second form of `auto` (type inference), this proposed third form is
very different in its semantics (far more complex) and overhead
(potentially very costly). I'm glad it's been rejected so far.
Templates better reflect the nature of this sort of code.


[safestack]: http://clang.llvm.org/docs/SafeStack.html
[concepts]: http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2015/n4361.pdf
[reject]: http://honermann.net/blog/2016/03/06/why-concepts-didnt-make-cxx17/
