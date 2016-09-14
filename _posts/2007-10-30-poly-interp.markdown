---
title: Polynomial Interpolation
layout: post
tags: [math]
uuid: bc611f11-9707-365c-94d2-8c253e240a92
---

This is something I learned the other day that I thought was
interesting.

So you have some data from experimentation or from a function that is
difficult to solve.

![](/img/poly/eq1.png)

Suppose you want to estimate a polynomial curve to that fits the data.
Then you could interpolate values between the data points. Let p(x) be
the polynomial. The equation for the polynomial we will fit to the
data will look like this,

![](/img/poly/eq2.png)

The *a* are the coefficients in our polynomial. We know *x* and we
want to satisfy the condition,

![](/img/poly/eq3.png)

which, when we want to solve it will take the form,

![](/img/poly/eq4.png)

Where the *a*'s are our unknowns for which we are solving. Notice
something? This is the linear system,

![](/img/poly/eq5.png)

We just have to solve for the *a* vector to get our
coefficients. I quickly wrote this GNU Octave code to try this out,

~~~matlab
function a = npoly (x, y)
  X = repmat (x', 1, length(x));

  for i = 1:length(x)
    X(:,i) = X(:,i) .^ (i - 1);
  end

  a = X \ y';
end
~~~

This is just an extremely simple and slow version of Octave's
`polyfit` function, except for the order of the coefficients solution
vector. I also wrote this function that will take the coefficient
vector and a value and do the polynomial interpolation at that point
(Octave's `polyval`),

~~~matlab
function v = psolve (x, a)
  v = zeros (size (x));

  for i = 1:length(a)
    v = v + a(i) * x.^(i-1);
  end
end
~~~

Here is an example of my polynomial interpolation function recognizing
a parabola,

    octave:88> x = 0:.5:3;
    octave:89> y = x.^2;
    octave:90> a = npoly(x, y)
    a =

       0.00000
      -0.00000
       1.00000
      -0.00000
       0.00000
      -0.00000
       0.00000

See how only the quadratic component is left because the zeros cancel
out everything else? Here is an example with some added Gaussian
noise, imitating data that might be pulled from a scientific
experiment,

    octave:142> x = 0:.5:3;
    octave:143> y = x.^2 + randn(size(x))*0.5;
    octave:144> a = npoly(x, y);
    octave:145> plot(x, y, "b*");
    octave:146> hold on
    octave:147> x_test = 0:.05:3;
    octave:148> y_test = psolve(x_test, a);
    octave:149> plot (x_test, y_test, "r")


[![](/img/poly/plot-thumb.png)](/img/poly/plot.png)

You can see that the order of our polynomial is too high for the data
we are using. The main problem, however, it that the linear system is
ill-conditioned. The condition number of the generated X matrix above
is 151900, meaning small changes in x result in large changes in the
solution. If we step out a bit you can see the polynomial quickly
diverge from the given data,

[![](/img/poly/illplot-thumb.png)](/img/poly/illplot.png)

So, I definitely wouldn't use this for extrapolation.
