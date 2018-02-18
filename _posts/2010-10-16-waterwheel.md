---
title: Lorenz Chaotic Water Wheel Applet
layout: post
tags: [java, interactive]
uuid: dee2a554-b81f-3d55-85dd-e0bf9a220e51
---

**Update Feburary 2018**: This project [has gotten a huge facelift][ww].

Today's project is a Java applet that simulates and displays a
[Lorenz][lorenz] water wheel. Freely-swinging buckets with small holes
in the bottom are arranged around the outside of a loose wheel. Water
flows into the bucket at the top of the wheel, filling it up. As it gets
heavier it pulls down on the wheel, spinning it.

![](/img/wheel/waterwheel.png)

Source: <https://github.com/skeeto/ChaosWheel>

You can click with your mouse to adjust the simulation. If you run it
standalone (`java -jar`) it will allow you to give it the number of
buckets you want to use as a command-line argument. It's based on some
Octave/Matlab code written by a friend of mine, Michael Abraham. Those
environments are so slow, though, that they couldn't do it in real time
like this.

This simulation is [chaotic][chaos]: even though the behavior of the
system is deterministic it is *highly* sensitive to initial conditions.
The animation you see above is unique: no one saw this particular
variation before, nor will anyone again. If you refresh the page you'll
be given a new wheel with unique initial conditions (well, one out of
the 2\^128 possible starting conditions, since this is running inside of
a digital computer).

![](/img/wheel/butterfly.png)

On the state space plot you should be able to see the state orbiting two
[attractors][atr]. It's the classic butterfly image that gives the
phenomenon its name. The state space plot will look smoother with more
buckets, being perfectly smooth at an infinite amount of buckets. Your
mouse cannot possibly survive that much clicking, so don't try it.


[atr]: http://en.wikipedia.org/wiki/Lorenz_attractor
[chaos]: http://en.wikipedia.org/wiki/Chaos_theor)
[lorenz]: http://en.wikipedia.org/wiki/Edward_Norton_Lorenz
[ww]: https://github.com/skeeto/waterwheel
