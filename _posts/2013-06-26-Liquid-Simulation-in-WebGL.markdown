---
title: Liquid Simulation in WebGL
layout: post
tags: [javascript, interactive, webgl, opengl]
uuid: a0e42262-19a8-3208-4a5b-b70485f5ae8a
---

Over a year ago I implemented
[a liquid simulation using a Box2D and Java 2D][orig]. It's a neat
trick that involves simulating a bunch of balls in a container,
blurring the rendering of this simulation, and finally thresholding
the blurred rendering. Due to [my recent affection for WebGL][webgl],
this week I ported the whole thing to JavaScript and WebGL.

 * [nullprogram.com/fun-liquid/webgl/](/fun-liquid/webgl/)

Unlike the previous Java version, blurring and thresholding is
performed entirely on the GPU. It *should* therefore be less CPU
intensive and a lot more GPU intensive. Assuming a decent GPU, it will
run at a (fixed) 60 FPS, as opposed to the mere 30 FPS I could squeeze
out of the old version. Other than this, the JavaScript version should
look pretty much identical to the Java version.

### Box2D performance

I ran into a few complications while porting. The first was the
performance of Box2D. I started out by using [box2dweb][box2dweb],
which is a port of Box2DFlash, which is itself a port of Box2D. Even
on V8, the performance was poor enough that I couldn't simulate enough
balls to achieve the liquid effect. The original JBox2D version
handles 400 balls just fine while this one was struggling to do about
40.

[Brian][brian] suggested I try out
[the Box2D emscripten port][box2djs]. Rather than manually port Box2D
to JavaScript, emscripten compiles the original C++ to JavaScript via
LLVM, being so direct as to even maintain its own heap. The benefit is
much better performance, but the cost is a difficult API. Interacting
with emscripten-compiled code can be rather cumbersome, and this
emscripten port isn't yet fully worked out. For example, creating a
PolygonShape object involves allocating an array on the emscripten
heap and manipulating a pointer-like thing. And when you screw up, the
error messages are completely unhelpful.

Moving to this other version of Box2D allowed me to increase the
number of balls to about 150, which is just enough to pull off the
effect. I'm still a bit surprised how slow this is. The computation
complexity for this is something like an O(n^2), so 150 is a long ways
behind 400. I may revisit this in the future to try to get better
performance by crafting my own very specialized physics engine from
scratch.

### WebGL complexity

Before I even got into writing the WebGL component of this, I
implemented a 2D canvas display, without any blurring, just for
getting Box2D tuned. If you visit the demonstration page without a
WebGL-capable browser you'll see this plain canvas display.

Getting WebGL to do the same thing was very simple. I used `GL_POINTS`
to draw the balls just like I had done with [the sphere demo][sphere].
To do blurring I would need to render this first stage onto an
intermediate framebuffer, then using this framebuffer as an input
texture I would blur and threshold this into the default framebuffer.

This actually took me awhile to work out, much longer than I had
anticipated. To prepare this intermediate framebuffer you must first
create and configure a texture. Then create and configure a
*render*buffer to fill in as the depth buffer. Then finally create the
framebuffer and attach both of these to it. Skip any step and all you
get are some vague WebGL warnings. (With regular OpenGL it's worse,
since you get no automatic warnings at all.)

WebGL textures must have dimensions that are powers of two. However,
my final output does not. Carefully rendering onto a texture with a
different aspect ratio and properly sampling the results back off
introduces an intermediate coordinate system which mucks things up a
bit. It took me some time to wrap my head around it to work everything
out.

Finally, once I was nearly done, my fancy new shader was consistently
causing OpenGL to crash, taking my browser down with it. I had to
switch to a different computer to continue developing.

#### The GPU as a bottleneck

For the second time since I've picked up WebGL I have overestimated
graphics cards' performance capabilities. It turns out my CPU is
faster at convolving a 25x25 kernel — the size of the convolution
kernel in the Java version — than any GPU that I have access to. If I
reduce the size of the kernel the GPU gets its edge back. The only way
to come close to 25x25 on the GPU is to cut some corners. I finally
settled on an 19x19 kernel, which seems to work just about as well
without being horribly slow. I may revisit this in the future so that
lower-end GPUs can run this at 60 FPS as well.

### Conclusion

I'm really happy with the results, and writing this has been a good
exercise in OpenGL. I completely met one of my original goals: to look
practically identical to the original Java version. I *mostly* met my
second, performance goal. On my nice desktop computer it runs more
than twice as fast, but, unfortunately, it's very slow my tablet. If I
revisit this project in the future, the purpose will be to optimize
for these lower-end, mobile devices.

This project has also been a useful testbed for a low-level WebGL
wrapper library I'm working on called [Igloo][igloo], which I'll cover
in a future post.


[orig]: /blog/2012/02/03/
[webgl]: /blog/2013/06/10/
[box2djs]: https://github.com/kripken/box2d.js/
[box2dweb]: http://code.google.com/p/box2dweb/
[brian]: http://www.50ply.com/
[sphere]: /sphere-js/
[igloo]: https://github.com/skeeto/igloojs
