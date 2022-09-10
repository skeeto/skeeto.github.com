---
title: You might not need machine learning
layout: post
date: 2020-11-24T04:04:36Z
tags: [ai, c, media, compsci, video]
uuid: 91aa121d-c796-4c11-99d4-41c707637672
excerpt_separator: <!--more-->
---

*This article was discussed [on Hacker News][hn].*

Machine learning is a trendy topic, so naturally it's often used for
inappropriate purposes where a simpler, more efficient, and more reliable
solution suffices. The other day I saw an illustrative and fun example of
this: [Neural Network Cars and Genetic Algorithms][orig]. The video
demonstrates 2D cars driven by a neural network with weights determined by
a generic algorithm. However, the entire scheme can be replaced by a
first-degree polynomial without any loss in capability. The machine
learning part is overkill.

[![](/img/screenshot/racetrack.jpg)][racetrack]

<!--more-->

Above demonstrates my implementation using a polynomial to drive the cars.
My wife drew the background. There's no path-finding; these cars are just
feeling their way along the track, "following the rails" so to speak.

My intention is not to pick on this project in particular. The likely
motivation in the first place was a desire to apply a neural network to
*something*. Many of my own projects are little more than a vehicle to try
something new, so I can sympathize. Though a professional setting is
different, where machine learning should be viewed with a more skeptical
eye than it's usually given. For instance, don't use active learning to
select sample distribution when a [quasirandom sequence][qr] will do.

In the video, the car has a limited turn radius, and minimum and maximum
speeds. (I've retained these contraints in my own simulation.) There are
five sensors — forward, forward-diagonals, and sides — each sensing the
distance to the nearest wall. These are fed into a 3-layer neural network,
and the outputs determine throttle and steering. Sounds pretty cool!

![](/img/diagram/racecar.svg)

A key feature of neural networks is that the outputs are a nonlinear
function of the inputs. However, steering a 2D car is simple enough that
**a linear function is more than sufficient**, and neural networks are
unnecessary. Here are my equations:

    steering = C0*input1 - C0*input3
    throttle = C1*input2

I only need three of the original inputs — forward for throttle, and
diagonals for steering — and the driver has just two parameters, `C0` and
`C1`, the polynomial coefficients. Optimal values depend on the track
layout and car configuration, but for my simulation, most values above 0
and below 1 are good enough in most cases. It's less a matter of crashing
and more about navigating the course quickly.

The lengths of the red lines below are the driver's three inputs:

<video src="/vid/racecar.mp4" width="530" height="330"
       loop muted autoplay controls>
</video>

These polynomials are obviously much faster than a neural network, but
they're also easy to understand and debug. I can confidently reason about
the entire range of possible inputs rather than worry about a trained
neural network [responding strangely][troj] to untested inputs.

Instead of doing anything fancy, my program generates the coefficients at
random to explore the space. If I wanted to generate a good driver for a
course, I'd run a few thousand of these and pick the coefficients that
complete the course in the shortest time. For instance, these coefficients
make for a fast, capable driver for the course featured at the top of the
article:

    C0 = 0.896336973, C1 = 0.0354805067

Many constants can complete the track, but some will be faster than
others. If I was developing a racing game using this as the AI, I'd not
just pick constants that successfully complete the track, but the ones
that do it quickly. Here's what the spread can look like:

<video src="/vid/racecars.mp4" width="530" height="330"
       loop muted autoplay controls>
</video>

If you want to play around with this yourself, here's my C source code
that implements this driving AI and [generates the videos and images
above][mm]:

**[aidrivers.c][gist]**

Racetracks are just images drawn in your favorite image editing program
using the colors documented in the source header.


[gist]: https://github.com/skeeto/scratch/blob/master/aidrivers/aidrivers.c
[hn]: https://news.ycombinator.com/item?id=25196574
[mm]: /blog/2017/11/03/
[orig]: https://www.youtube.com/watch?v=-sg-GgoFCP0
[qr]: http://extremelearning.com.au/unreasonable-effectiveness-of-quasirandom-sequences/
[racetrack]: https://nullprogram.com/video/?v=racetrack
[troj]: https://arxiv.org/abs/1903.06638
