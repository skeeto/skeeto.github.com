---
title: Revisiting an N-body Simulator
layout: post
tags: [c, video]
uuid: 4e450cd9-5541-3ff4-0c15-351a77926825
---

Ten years ago I was a high school senior taking my second year of
physics. Having recently reviewed vectors and gravity, as well as
being an avid Visual Basic programmer at the time, I decided to create
my own n-body simulation. I recently came across this old project and
fortunately (since I can no longer compile it) I had left a compiled
version with the source code. Here it is in Wine,

![](/img/screenshot/galsim.png)

Really, it's really not worth downloading but I'm putting a link here
for my own archival purposes.

 * [galsim.zip](https://nullprogram.s3.amazonaws.com/galaxy/galsim.zip) (712 kB)

I didn't quite understand what I was doing so I screwed up the
math. All the vector computations were done independently. Integration
was done by Euler method — a sin I continue to commit regularly to
this day but now I'm at least aware of the limitations. Despite this,
it was still accurate enough to look interesting.

Probably the most advanced thing to come out of it, and something I
*did* do correctly, was the display. I worked out my own graphics
engine to project three-dimensional star coordinates onto the
two-dimensional drawing surface, re-inventing perspective projection.

As I said, I recently came across it again while digging around my
digital archives. Now that I'm a professional developer I wondered how
much faster I could do the same thing with just a few hours of
coding. I did it in C and my implementation was about an order of
magnitude faster. Not as much as I hoped, but it's something!

 * [https://gist.github.com/3204862](https://gist.github.com/3204862)

It's still Euler method integration, the bodies are still point
masses, and there are no collisions so there's numerical instability
when they get close. However, I did get the vector math right! My goal
was to make something that looked interesting rather than an accurate
simulation, so all of this is alright.

I only wrote the simulation, not a display. To display the output I
just had GNU Octave plot it for me, which I turned into videos. This
first video is a static view of the origin of the coordinate
system. If you watch (or skip) all the way to the end you'll see that
the galaxy drifts out of view. This is due to a bias in the random
number generator — the galaxy's mass was lopsided.

<video src="https://nullprogram.s3.amazonaws.com/galaxy/attempt-1.webm"
       controls="controls" width="480" height="360">
  Video requires WebM support with HTML5.
</video>

After seeing this drift I added dynamic pan and zoom, so that the
camera follows the action. It's a bit excessive at the beginning (the
camera is *too* dynamic) and the end (the camera is too far out).

<video src="https://nullprogram.s3.amazonaws.com/galaxy/attempt-2.webm"
       controls="controls" width="480" height="360">
  Video requires WebM support with HTML5.
</video>

I bit more tweaking of the galaxy start state (normal distribution,
adding initial velocities) and the camera and I got this interesting
result. The galaxy initially bunches into two globs, which then merge.

<video src="https://nullprogram.s3.amazonaws.com/galaxy/v10-z9.webm"
       controls="controls" width="480" height="360">
  Video requires WebM support with HTML5.
</video>

I wouldn't have bothered with a post about this but I think these
videos turned out to be interesting.
