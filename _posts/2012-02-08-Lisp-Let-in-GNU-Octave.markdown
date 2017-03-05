---
title: Lisp Let in GNU Octave
layout: post
tags: [octave, trick, lisp, media, math, video]
uuid: 05e5318e-0cf4-3d80-4bf5-da695dbe9e47
---

In [BrianScheme][bs], the standard Lisp binding form `let` isn't a
special form. That is, it's not a hard-coded language feature, or
*special form*. It's built on top of `lambda`. In any lexically-scoped
Lisp, the expression,

~~~cl
(let ((x 10)
      (y 20))
  (* 10 20))
~~~

Can also be written as,

~~~cl
((lambda (x y)
   (* x y))
 10 20)
~~~

BrianScheme's `let` is just a macro that transforms into a lambda
expression. This is also what made it so important to implement lambda
lifting, to optimize these otherwise-expensive forms.

It's possible to achieve a similar effect in GNU Octave (but not
Matlab, due to [its flawed parser design][flaw]). The language permits
simple lambda expressions, much like Python.

~~~matlab
> f = @(x) x + 10;
> f(4)
ans = 14
~~~

It can be used to create a scope in a language that's mostly devoid of
scope. For example, I can avoid assigning a value to a temporary
variable just because I need to use it in two places. This one-liner
generates a random 3D unit vector.

~~~matlab
(@(v) v / norm(v))(randn(1, 3))
~~~

The anonymous function is called inside the same expression where it's
created. In practice, doing this is stupid. It's confusing and there's
really nothing to gain by being clever, doing it in one line instead
of two. Most importantly, there's no macro system that can turn this
into a new language feature. *However*, I enjoyed using this technique
to create a one-liner that generates `n` random unit vectors.

~~~matlab
n = 1000;
p = (@(v) v ./ repmat(sqrt(sum(abs(v) .^ 2, 2)), 1, 3))(randn(n, 3));
~~~

Why was I doing this? I was using the Monte Carlo method to
double-check my solution to [this math problem][geo]:

> What is the average straight line distance between two points on a
> sphere of radius 1?

I was also demonstrating to [Gavin][gavin] that simply choosing two
*angles* is insufficient, because the points the angles select are not
evenly distributed over the surface of the sphere. I generated this
video, where the poles are clearly visible due to the uneven selection
by two angles.

<video src="https://s3.amazonaws.com/nullprogram/sphere/sphere-dark.webm"
       controls="controls" height="340" width="340">
</video>

This took hours to render with gnuplot! Here are stylized versions:
[Dark][dark] and [Light][light].


[bs]: /blog/2011/01/30/
[lift]: http://en.wikipedia.org/wiki/Lambda_lifting
[flaw]: /blog/2008/08/29/
[geo]: http://godplaysdice.blogspot.com/2011/12/geometric-probability-problem.html
[gavin]: http://devrand.org/
[dark]: https://s3.amazonaws.com/nullprogram/sphere/dark.html
[light]: https://s3.amazonaws.com/nullprogram/sphere/light.html
