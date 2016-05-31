---
title: Fast Monte Carlo Method with JavaScript
layout: post
tags: [emacs, lisp, c, javascript]
uuid: 0208230e-3f57-334e-5d57-7a18f3794288
---

> How many times should a random number from `[0, 1]` be drawn to have
> it sum over 1?

If you want to figure it out for yourself, stop reading now and come
back when you're done.

[The answer][answer] is *e*. When I came across this question I took
the lazy programmer route and, rather than work out the math, I
estimated the answer using the Monte Carlo method. I used the language
I always use for these scratchpad computations: Emacs Lisp. All I need
to do is switch to the `*scratch*` buffer and start hacking. No
external program needed.

The downside is that Elisp is incredibly slow. Fortunately, Elisp is
so similar to Common Lisp that porting to it is almost trivial. My
preferred Common Lisp implementation, SBCL, is very, very fast so it's
a huge speed upgrade with little cost, should I need it. As far as I
know, SBCL is the fastest Common Lisp implementation.

Even though Elisp was fast enough to determine that the answer is
probably *e*, I wanted to play around with it. This little test
program doubles as a way to estimate the value of *e*,
[similar to estimating *pi*][pi]. The more trial runs I give it the
more accurate my answer will get — to a point.

Here's the Common Lisp version. (I love the loop macro, obviously.)

~~~cl
(defun trial ()
  (loop for count upfrom 1
     sum (random 1.0) into total
     until (> total 1)
     finally (return count)))

(defun monte-carlo (n)
  (loop repeat n
     sum (trial) into total
     finally (return (/ total 1.0 n))))
~~~

Using SBCL 1.0.57.0.debian on an Intel Core i7-2600 CPU, once
everything's warmed up this takes about 9.4 seconds with 100 million
trials.

    (time (monte-carlo 100000000))
    Evaluation took:
      9.423 seconds of real time
      9.388587 seconds of total run time (9.380586 user, 0.008001 system)
      99.64% CPU
      31,965,834,356 processor cycles
      99,008 bytes consed
    2.7185063

Since this makes for an interesting benchmark I gave it a whirl in
JavaScript,

~~~javascript
function trial() {
    var count = 0, sum = 0;
    while (sum <= 1) {
        sum += Math.random();
        count++;
    }
    return count;
}

function monteCarlo(n) {
    var total = 0;
    for (var i = 0; i < n; i++) {
        total += trial();
    }
    return total / n;
}
~~~

I ran this on Chromium 24.0.1312.68 Debian 7.0 (180326) which uses V8,
currently the fastest JavaScript engine. With 100 million trials,
**this only took about 2.7 seconds**!

~~~javascript
monteCarlo(100000000); // ~2.7 seconds, according to Skewer
// => 2.71850356
~~~

Whoa! It beat SBCL! I was shocked. Let's try using C as a
baseline. Surely C will be the fastest.

~~~c
#include <stdio.h>
#include <stdlib.h>

int trial() {
    int count = 0;
    double sum = 0;
    while (sum <= 1.0) {
        sum += rand() / (double) RAND_MAX;
        count++;
    }
    return count;
}

double monteCarlo(int n) {
    int i, total = 0;
    for (i = 0; i < n; i++) {
        total += trial();
    }
    return total / (double) n;
}

int main() {
    printf("%f\n", monteCarlo(100000000));
    return 0;
}
~~~

I used the highest optimization setting on the compiler.

    $ gcc -ansi -W -Wall -Wextra -O3 temp.c
    $ time ./a.out
    2.718359

    real	0m3.782s
    user	0m3.760s
    sys	0m0.000s

Incredible! **JavaScript was faster than C!** That was completely
unexpected.

### The Circumstances

Both the Common Lisp and C code could probably be carefully tweaked to
improve performance. In Common Lisp's case I could attach type
information and turn down safety. For C I could use more compiler
flags to squeeze out a bit more performance. Then *maybe* they could
beat JavaScript.

In contrast, as far as I can tell the JavaScript code is already as
optimized as it can get. There just aren't many knobs to tweak. Note
that minifying the code will make no difference, especially since I'm
not measuring the parsing time. Except for the functions themselves,
the variables are all local, so they are never "looked up" at
run-time. Their name length doesn't matter. Remember, in JavaScript
*global* variables are expensive, because they're (generally) hash
table lookups on the global object at run-time. For any decent
compiler, local variables are basically precomputed memory offsets —
very fast.

The function names themselves are global variables, but the V8
compiler appears to eliminate this cost (inlining?). Wrapping the
entire thing in another function, turning the two original functions
into local variables, makes no difference in performance.

While Common Lisp and C *may* be able to beat JavaScript if time is
invested in optimizing them — something to be done rarely — in a
casual implementation of this algorithm, JavaScript beats them both. I
find this really exciting.


[answer]: http://bayesianthink.blogspot.com/2013/02/the-expected-draws-to-sum-over-one.html
[pi]: http://math.fullerton.edu/mathews/n2003/montecarlopimod.html
