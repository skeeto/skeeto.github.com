---
title: The Adversarial Implementation
layout: post
date: 2017-05-03T17:51:53Z
tags: [c, python, lang]
uuid: e6f370f9-1d35-3295-3bd5-74ae20c52a0e
---

When [coding against a standard][std], whether it's a programming
language specification or an open API with multiple vendors, a common
concern is the conformity of a particular construct to the standard.
This cannot be determined simply by experimentation, since a piece of
code may work correctly due only to the specifics of a particular
implementation. It works *today* with *this* implementation, but it
may not work *tomorrow* or with a *different* implementation.
Sometimes an implementation will warn about the use of non-standard
behavior, but this isn't always the case.

When I'm reasoning about whether or not something is allowed, I like to
imagine an *adversarial implementation*. If the standard allows some
freedom, this implementation takes an imaginative or unique approach. It
chooses [non-obvious interpretations][hash] with possibly unexpected,
but valid, results. This is nearly the opposite of [djb's hypothetical
boringcc][bcc], though some of the ideas are similar.

Many argue that [this is already the case][opt] with modern C and C++
optimizing compilers. Compiler writers are already [creative with the
standard][ub] in order to squeeze out more performance, even if it's
at odds with the programmer's actual intentions. The most prominent
example in C and C++ is *strict aliasing*, where the optimizer is
deliberately blinded to certain kinds of aliasing because the standard
allows it to be, eliminating some (possibly important) loads. This
happens despite the compiler's ability to trivially prove that two
particular objects really do alias.

I want to be clear that I'm not talking about the [nasal daemon][nd]
kind of creativity. That's not a helpful thought experiment. What I
mean is this: **Can I imagine a conforming implementation that breaks
any assumptions made by the code?**

In practice, compilers typically have to bridge multiple
specifications: the language standard, the [platform ABI][abi], and
operating system interface (process startup, syscalls, etc.). This
really ties its hands on how creative it can be with any one of the
specifications. Depending on the situation, the imaginary adversarial
implementation isn't necessarily running on any particular platform.
If our program is expected to have a long life, useful for many years
to come, we should avoid making too many assumptions about future
computers and imagine an adversarial compiler with few limitations.

### C example

Take this bit of C:

~~~c
printf("%d", sizeof(foo));
~~~

The `printf` function is variadic, and it relies entirely on the format
string in order to correctly handle all its arguments. The `%d`
specifier means that its matching argument is of type `int`. The result
of the `sizeof` operator is an integer of type `size_t`, which has a
different sign and may even be a different size.

Typically this code will work just fine. An `int` and `size_t` are
generally passed the same way, the actual value probably fits in an
`int`, and two's complement means the signedness isn't an issue due to
the value being positive. From the `printf` point of view, it
typically can't detect that the type is wrong, so everything works by
chance. In fact, it's hard to imagine a real situation where this
wouldn't work fine.

However, this still undefined behavior — a scenario where a creative
adversarial implementation can break things. In this case there are a
few options for an adversarial implementation:

1. Arguments of type `int` and `size_t` are passed differently, so
   `printf` will load the argument it from the wrong place.
2. The implementation doesn't use two's complement and even small
   positive values have different bit representations.
3. The type of `foo` is given crazy padding for arbitrary reasons that
   makes it so large it doesn't fit in an `int`.

What's interesting about #1 is that *this has actually happened*. For
example, here's a C source file.

~~~c
float foo(float x, int y);

float
bar(int y)
{
    return foo(0.0f, y);
}
~~~

And in another source file:

~~~c
float
foo(int x, int y)
{
    (void)x;  // ignore x
    return y * 2.0f;
}
~~~

The type of argument `x` differs between the prototype and the
definition, which is undefined behavior. However, since this argument
is ignored, this code will still work correctly on many different
real-world computers, particularly where `float` and `int` arguments
are passed the same way (i.e. on the stack).

However, in 2003 the x86-64 CPU arrived with its new System V ABI.
Floating point and integer arguments were now passed differently, and
the types of preceding arguments mattered when deciding which register
to use. Some constructs that worked fine, by chance, prior to 2003 would
soon stop working due to what may have seemed like an adversarial
implementation years before.

### Python example

Let's look at some Python. This snippet opens a file a million times
without closing any handles.

~~~py
for i in range(1, 1000000):
    f = open("/dev/null", "r")
~~~

Assuming you have a `/dev/null`, this code will work fine without
throwing any exceptions on CPython, the most widely used Python
implementation. CPython uses a deterministic reference counting scheme,
and the handle is automatically closed as soon as its variable falls out
of scope. It's like having an invisible `f.close()` at the end of the
block.

However, this code is incorrect. The deterministic handle closing an
implementation behavior, [not part of the specification][py]. The
operating system limits the number of files a process can have open at
once, and there's a risk that this resource will run out even though
none of those handles are reachable. Imagine an adversarial Python
implementation trying to break this code. It could sufficiently delay
garbage collection, or even [have infinite memory][inf], omitting
garbage collection altogether.

Like before, such an implementation eventually did come about: PyPy, a
Python implementation written in Python with a JIT compiler. It uses (by
default) something closer to mark-and-sweep, not reference counting, and
those handles [are left open][pypy] until the next collection.

    >>>> for i in range(1, 1000000):
    ....     f = open("/dev/null", "r")
    .... 
    Traceback (most recent call last):
      File "<stdin>", line 2, in <module>
    IOError: [Errno 24] Too many open files: '/dev/null'

### A tool for understanding specifications

This fits right in with a broader method of self-improvement:
Occasionally put yourself in the implementor's shoes. Think about what
it would take to correctly implement the code that you write, either
as a language or the APIs that you call. On reflection, you may find
that some of those things that *seem* cheap may not be. Your
assumptions may be reasonable, but not guaranteed. (Though it may be
that "reasonable" is perfectly sufficient for your situation.)

An adversarial implementation is one that challenges an assumption
you've taken for granted by turning it on its head.


[opt]: http://yarchive.net/comp/linux/gcc.html
[bcc]: https://groups.google.com/forum/m/#!msg/boring-crypto/48qa1kWignU/o8GGp2K1DAAJ
[ub]: http://blog.llvm.org/2011/05/what-every-c-programmer-should-know.html
[hash]: /blog/2016/05/30/
[abi]: /blog/2016/11/17/
[nd]: http://www.catb.org/jargon/html/N/nasal-demons.html
[py]: https://docs.python.org/3/reference/datamodel.html
[inf]: https://blogs.msdn.microsoft.com/oldnewthing/20100809-00/?p=13203
[pypy]: https://utcc.utoronto.ca/~cks/space/blog/programming/NondeterministicGCII
[std]: /blog/2017/03/30/
