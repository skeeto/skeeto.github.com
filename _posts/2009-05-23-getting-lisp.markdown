---
title: Getting Lisp
layout: post
tags: []
uuid: a9619303-89d6-3e8e-660b-4db0839f590b
---

<!-- 23 May 2009 -->

I've been using lisp on and off for the past few years. I read some
lisp books, went through [*The Little
Schemer*](http://www.ccs.neu.edu/home/matthias/BTLS/) and some of [SICP](http://mitpress.mit.edu/sicp/full-text/book/book.html). But
I could never really *think* in lisp. When I needed to write some
code, I would prefer another language first. I was writing imperative
code for 10 years before I saw lisp, and I was used to it.

Recently, I have found myself wanting to use lists (including alists,
plists, etc.) as data structures for everything, even when I'm not
even writing lisp. I think I finally
*got* lisp and now I want to use lisp for everything.

For example, take this little problem from the other day on [f(t)](http://function-of-time.blogspot.com/). [ Katie Nowak gives us a math problem](http://function-of-time.blogspot.com/2009/05/wait-what.html),

> There is a 75% chance of rain on any given day in the next
> week. What is the probability that it will rain on at least 5 of
> the 7 days?

<img src="/img/rain-cloud.png" alt="Rain cloud in parenthesis" title="The way lisp hackers see the world." class="right"/>
The purpose of the question was to point out a neat coincidence in the
problem (explained at the link). I used a program to solve the
problem and there are two reasons for this.

First, I wanted to use the program to explore the problem and find the
"special" property. With a program, I could quickly try different
parameters, which would take longer, and be more error prone, by hand.

Second, which is similar to the first, I hate evaluating a large
expression by hand. It's slow and error prone. Writing a program to do
the same work is faster and mistakes are easier to catch. Also, I can
quickly try different parameters to make sure my program's output is
reasonable. In this case, for any reasonable input, the output, a
probability, shouldn't be greater than 1.

Well, let's see, this is a [ Bernoulli
experiment](http://en.wikipedia.org/wiki/Bernoulli_process): each day is independent, so it is like flipping a coin
seven times and counting the heads. That means we need the [ *choose
function*](http://en.wikipedia.org/wiki/Binomial_coefficient).

My first thought was Octave, as this is a simple program and Octave
already provides `nchoosek()` for me.

```octave
# Rain at least n days
function sum = rain (n)
  sum = 0;
  for i = n:7
    sum += nchoosek(7,i) * 0.75 ^ i * 0.25 ^ (7 - i);
  end
end
```

Simple, but I actually made a couple little mistakes working it out,
and it took me a little longer than it should have. If you asked
someone to write this program in any imperative language, it would
probably look a lot like this.

I then made a lisp version (elisp), but I first needed to define the
binomial coefficient function since there wasn't one provided.

```cl
(defun nck (n k)
  "The binomial coefficient."
  (if (or (= k 0) (= k n)) 1
    (+ (nck (- n 1) (- k 1)) (nck (- n 1) k))))
```

This is the recursive version, based on [ Pascal's
rule](http://en.wikipedia.org/wiki/Pascal%27s_rule), so it doesn't need factorials.

In lisp, recursion is preferred to iteration, so that's the way I
approached the program.

```cl
(defun p-rain (n)
  "Probability of rain on exactly n days."
  (* (nck 7 n) (expt 0.75 n) (expt 0.25 (- 7 n))))

(defun rain-at-least (n)
  "Probability it will rain on at least n days."
  (if (> n 7) 0
    (+ (p-rain n) (rain-at-least (+ 1 n)))))
```

I like the recursive version, and this code, much better. It presents
the solution in a more straight forward way. And I got it right the
first time, too. Yes, it *was* written after the Octave version,
but I still think it counts for something.

The lisp version is also less complex. The Octave version has to use
some temporary variables, `i` and `sum`, which
is extra conceptual overhead. Sure, it could also be written
recursively, but this is not really the way Octave is meant to be
written.

Eh, not a great example, really. Lisp has so many powerful features,
like macros (the powerful lisp kind), symbols, low-level access into
the interpreter, and the REPL, that allow the programmer to do some
really cool things. Many of these features are unique to lisps.

I'm really liking lisp.
