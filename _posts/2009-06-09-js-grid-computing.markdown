---
title: JavaScript Distributed Computing
layout: post
tags: [javascript]
uuid: e859a5e0-5ce4-3eca-d8eb-9379578c00b0
---

<!-- 9 June 2009 -->

<img src="/img/diagram/js-grid.png" alt="" title="Browser computing grid." class="right"/>
I'm not the first to come across this idea: the browser could be used
as part of a distributed computing system. A web server hands out
JavaScript and the browser runs the script and reports the results
back to the server. The browser is a portable, widely available
platform so just about anyone can easily contribute, possibly without
even knowing.

Browsers aren't really expecting this sort of thing. They will
complain if a script is running for too long. If you tell Firefox to
continue running the script anyway, it will lock up until the script
is done (or when it complains again). This can be worked around by [writing a simple
scheduler with `setTimeout()`](http://www.julienlecomte.net/blog/2007/10/28/).

This could also potentially be used as an alternative to
advertising. Instead of selling advertising space, a website operator
could sell visitor's CPU time by including a little snippet of
code. This may be more successful, because most visitors would be
unaware of it, making it less intrusive. It will be less likely to be
blocked. Of course, there ethical issues about this. [In fact, there is already a
company doing this with secret Java applets](http://www.pluraprocessing.com/).

There are two serious constraints on using JavaScript in a browser as
a distributed computing platform:

**Low bandwidth**. There isn't a lot of opportunity to transfer data
between the server and the node, and nodes can't talk to
each other. The data needed by a node must be small. The results data
must also be small.

**Short computational units**. The JavaScript in the browser has no
way to store its running state between browser sessions, so it must
rely on the server for this. This means that the units of work must be
able to be completed within a short period of time. A few minutes at
the most on a normal computer.

A lot of problems won't fit inside these limitations. One that I
thought might was a [Mersenne prime](http://en.wikipedia.org/wiki/Mersenne_primes)
search. A Mersenne prime is a prime of the form,

2<sup>n</sup> - 1

So even though the largest known Mersenne prime has nearly 13 million
digits (about 5 MB just to store the entire number), it can be
described by it's exponent, `43,112,609`, which is small
enough to fit in a 4-byte integer. The result of a calculation, a
probabilistic primality test, is a "yes" or "no". One bit. It fits the
first constraint very well.

However, the smallest amount of work a node can do is an entire
primality test. If we break it down any further, the prime will have
been expanded and we will not fit the first constraint. There will be
too much data. To see how possible it might be, I implemented it,
which you can try out here,

[/download/mersenne/](/download/mersenne/) (sloppy code warning!)

I modified an [ existing JavaScript bigint library](http://www.leemon.com/crypto/BigInt.html) which allowed me to get it up
running quickly. After you receive the page, your browser will run the
[ Miller-Rabin primality test](http://en.wikipedia.org/wiki/Miller%E2%80%93Rabin_primality_test) on `2^9941 - 1`. You can
edit the source HTML to try a different exponent. Running it on
several of my computers on different browsers it took anywhere from an
hour to 8 hours. And that's only with an exponent of
`9941`. It's an unsuitable problem.

It would be neat to see a browser computing grid in action, but I
can't think of a problem to solve with it.
