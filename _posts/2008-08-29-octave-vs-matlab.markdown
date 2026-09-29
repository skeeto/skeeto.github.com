---
title: A GNU Octave Feature
layout: post
tags: [octave, rant, lang]
uuid: a8aec192-263a-38d3-5d0d-86824c93fd4f
---

<!-- 29 August 2008 -->

At work they recently moved me to a new project. It is a Matlab-based
data analysis thing. I haven't really touched Matlab in over a year
(the last time I used Matlab at work), and, instead, use GNU Octave at
home when the language is appropriate. I got so used to Octave that I
found a pretty critical feature missing from Matlab's implementation:
treat an expression as if it were of the type of its output.

Let's say we want to index into the result of a function. Take, for
example, the magic square function, `magic()`. This spits
out a
[magic square](http://en.wikipedia.org/wiki/Magic_square)
of the given size. In Octave we can generate a 4x4 magic square and
chop out the middle 2x2 portion in one line.

```
octave> magic(4)(2:3,2:3)
ans =

   11   10
    7    6
```

Or more possibly clearly,

```
octave> [magic(4)](2:3,2:3)
ans =

   11   10
    7    6
```

Try this in Matlab and you will get a big, fat error. You have to
assign the magic square to a temporary variable to do the same
thing. I kept trying to do this sort of thing in Matlab and was
thinking to myself, "I *know* I can do this somehow!". Nope, I
was just used to having Octave.

Where this really shows is when you want to reshape a matrix into a
nice, simple vector. If you have a matrix `M` and want to
count the number of NaN's it has, you can't just apply
the `sum()` function over `isnan()` because it
only does sums of columns. You can get around this with a special
index, `(:)`.

So, to sum all elements in `M` directly,

```
octave> sum(M(:))
```

In Octave, to count NaN's with `isnan()`,

```
octave> sum(isnan(M)(:))
```

Again, Matlab won't let you index the result of `isnan()`
directly. Stupid. I guess the Matlab way to do this is to
apply `sum()` twice.

Every language I can think of handles this properly. C, C++, Perl,
Ruby, etc. It is strange that Matlab itself doesn't have it. Score one
more for Octave.
