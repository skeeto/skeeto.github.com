---
title: A Few Tricky C Questions
layout: post
tags: [c]
uuid: 29f59a9d-86b9-387d-60bf-16417ea9888c
---

At work I recently came across an abandoned copy of the first edition
of *The C Programming Language* by Ken Thompson and Dennis Ritchie —
often lovingly abbreviated as *K&R*. It's a
[significant piece of computer science history](http://en.wikipedia.org/wiki/The_C_Programming_Language)
and I highly recommend it to anyone who writes software. As far as
computing manuals go, it's a thin book (228 pages) so I got through
the whole thing in about a week.

I've been programming in C for seven years now but it seems there's
always something new for me to learn about it. The book cleared up
some incomplete concepts I had about C, particularly the relationship
between pointers and arrays as well as operator precedence — the
reason why function pointers look so weird. By the end I re-gained an
appreciation for the simplicity and power of C. All of the examples in
the book are written without heap allocation (no `malloc()`), just
static memory, and it manages to get by with rather few limitations.

As I was reading I realized a handful of "tricky" questions that I
wouldn't have been able to answer with confidence before reading the
book. If you're a C developer, pause and reflect just after each chunk
of example code and try to answer the question as correctly as you
can. Pretend you're a compiler and think about what you need to do in
each situation.

### Register variables

What is the output of this program?

~~~c
#include <stdio.h>

int main()
{
    register int foo;
    printf("%p\n", &foo);
    return 0;
}
~~~

The `register` keyword hints to the compiler that the automatic
variable should be stored in a register rather than memory, making
access to the variable faster. This is only a hint so the compiler is
free to ignore it.

In the example we take a pointer to the variable. However, we declared
this variable to be stored in a register. Addresses only point to
locations in memory so registers can't be addressed by a
pointer. While the compiler can ignore the optimization hint and
provide an address, this is ultimately an inconsistent request. The
compiler will produce and error and the code will not compile.

### Pointers to struct fields

Is this program valid?

~~~c
struct {
    int foo, bar;
} baz;

int *example()
{
    return &baz.foo;
}
~~~

~~Here we're creating a struct called `baz` and take a pointer to one of
its fields. According to K&R C, this is invalid.~~ (**Update**: I
misunderstood. This is allowed.) Overall, structs are really limited in
K&R C: they can't be function arguments, nor can they be returned from
functions, ~~nor can pointers be taken to their fields~~. Only
*pointers* to structs are first-class. They acknowledged that this was
limiting and said they planned on fixing it in the future.

Fortunately, this *was* fixed with ANSI C and structs are first-class
objects. This means the above program is **valid** in ANSI C.

How about this one?

~~~c
struct {
    int foo : 4;
} baz;

int *example()
{
    return &baz.foo;
}
~~~


The `foo` field is a 4-bit wide bit-field — smaller than a single
byte. Pointers can only address whole bytes, so this is
**invalid**. Even if `foo` was 8 or 32 bits wide (full/aligned bytes
on modern architectures) this would still be invalid.

### Pointer arithmetic

We want to average two pointers to get a pointer in-between them. Is
this reasonable code?

~~~c
char *foo()
{
    char *start = "hello";
    char *end = start + 5;
    return (start + end) / 2;
}
~~~

A thoughtful programmer should notice that adding together pointers is
likely to be disastrous. Pointers tend to be very large, addressing
high areas of memory. Adding two pointers together is very likely to
lead to an overflow. When I posed this question to
[Brian](http://www.50ply.com/), he realized this and came up with this
solution to avoid the overflow.

~~~c
    return start / 2 + end / 2;
~~~

However, this is still **invalid**. As a complete precaution for
overflowing pointer arithmetic, pointer addition is forbidden and
neither of these will compile. Pointer subtraction is perfectly valid,
so it can be done like so.

~~~c
    return (end - start) / 2 + start;
~~~

Subtracting two pointers produces an integer. Adding *integers* to
pointers is not only valid but also essential, so this is only a
restriction about adding *pointers* together.

### Arrays

Is this valid?

~~~c
void foo()
{
    char hello[] = "hello";
    char *foo = hello;
}
~~~

`hello` is an array of `char`s and `foo` is a pointer to a `char`. In
general, arrays are interchangeable with pointers of the same type so
this is **valid**. Now how about this one?

~~~c
void foo()
{
    char hello[6];
    char *foo = "hello";
    hello = foo;
}
~~~

Here we've inverted the relationship are are trying to assign the
array as a pointer. This is **invalid**. Arrays are like pointer
constants in that they can't be used as *lvalues* — they can't be
reassigned to point to somewhere else. The closest you can get is to
*copy* the contents of `foo` into `hello`.

I think that about sums my questions. I (foolishly) didn't write them
down as I came up with them and this is everything I can remember.
