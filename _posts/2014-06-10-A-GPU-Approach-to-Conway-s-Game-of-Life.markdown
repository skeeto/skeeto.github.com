---
title: A GPU Approach to Conway's Game of Life
layout: post
date: 2014-06-10T06:29:48Z
tags: [webgl, javascript, interactive, gpgpu, opengl]
uuid: 205b43ce-faa8-33c8-db27-173ddad64229
---

<p class="abstract">
Update: In the next article, I <a href="/blog/2014/06/22/">extend this
program to solving mazes</a>. The demo has also been <a
href="http://rykap.com/graphics/skew/2016/01/23/game-of-life.html">
ported to the Skew programming language</a>.
</p>

[Conway's Game of Life][life] is [another well-matched
workload][voronoi] for GPUs. Here's the actual WebGL demo if you want
to check it out before continuing.

* [https://skeeto.github.io/webgl-game-of-life/](http://skeeto.github.io/webgl-game-of-life/)
  ([source](http://github.com/skeeto/webgl-game-of-life/))

To quickly summarize the rules:

* The universe is a two-dimensional grid of 8-connected square cells.
* A cell is either dead or alive.
* A dead cell with exactly three living neighbors comes to life.
* A live cell with less than two neighbors dies from underpopulation.
* A live cell with more than three neighbors dies from overpopulation.

![](/img/gol/gol.gif)

These simple cellular automata rules lead to surprisingly complex,
organic patterns. Cells are updated in parallel, so it's generally
implemented using two separate buffers. This makes it a perfect
candidate for an OpenGL fragment shader.

### Preparing the Textures

The entire simulation state will be stored in a single, 2D texture in
GPU memory. Each pixel of the texture represents one Life cell. The
texture will have the internal format GL_RGBA. That is, each pixel
will have a red, green, blue, and alpha channel. This texture is not
drawn directly to the screen, so how exactly these channels are used
is mostly unimportant. It's merely a simulation data structure. This
is because I'm using [the OpenGL programmable pipeline for general
computation][webgl]. I'm calling this the "front" texture.

Four multi-bit (actual width is up to the GPU) channels seems
excessive considering that all I *really* need is a single bit of
state for each cell. However, due to [framebuffer completion
rules][fbo], in order to draw onto this texture it *must* be GL_RGBA.
I could pack more than one cell into one texture pixel, but this would
reduce parallelism: the shader will run once per pixel, not once per
cell.

Because cells are updated in parallel, this texture can't be modified
in-place. It would overwrite important state. In order to do any real
work I need a second texture to store the update. This is the "back"
texture. After the update, this back texture will hold the current
simulation state, so the names of the front and back texture are
swapped. The front texture always holds the current state, with the
back texture acting as a workspace.

~~~javascript
GOL.prototype.swap = function() {
    var tmp = this.textures.front;
    this.textures.front = this.textures.back;
    this.textures.back = tmp;
    return this;
};
~~~

Here's how a texture is created and prepared. It's wrapped in a
function/method because I'll need two identical textures, making two
separate calls to this function. All of these settings are required
for framebuffer completion (explained later).

~~~javascript
GOL.prototype.texture = function() {
    var gl = this.gl;
    var tex = gl.createTexture();
    gl.bindTexture(gl.TEXTURE_2D, tex);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.REPEAT);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.REPEAT);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA,
                  this.statesize.x, this.statesize.y,
                  0, gl.RGBA, gl.UNSIGNED_BYTE, null);
    return tex;
};
~~~

A texture wrap of `GL_REPEAT` means the simulation will be
automatically [torus-shaped][torus]. The interpolation is
`GL_NEAREST`, because I don't want to interpolate between cell states
at all. The final OpenGL call initializes the texture size
(`this.statesize`). This size is different than the size of the
display because, again, this is *actually* a simulation data structure
for my purposes.

The `null` at the end would normally be texture data. I don't need to
supply any data at this point, so this is left blank. Normally this
would leave the texture content in an undefined state, but for
security purposes, WebGL will automatically ensure that it's zeroed.
Otherwise there's a chance that sensitive data might leak from another
WebGL instance on another page or, worse, from another process using
OpenGL. I'll make a similar call again later with `glTexSubImage2D()`
to fill the texture with initial random state.

In OpenGL ES, and therefore WebGL, wrapped (`GL_REPEAT`) texture
dimensions must be powers of two, i.e. 512x512, 256x1024, etc. Since I
want to exploit the built-in texture wrapping, I've decided to
constrain my simulation state size to powers of two. If I manually did
the wrapping in the fragment shader, I could make the simulation state
any size I wanted.

### Framebuffers

A framebuffer is the target of the current `glClear()`,
`glDrawArrays()`, or `glDrawElements()`. The user's display is the
*default* framebuffer. New framebuffers can be created and used as
drawing targets in place of the default framebuffer. This is how
things are drawn off-screen without effecting the display.

A framebuffer by itself is nothing but an empty frame. It needs a
canvas. Other resources are attached in order to make use of it. For
the simulation I want to draw onto the back buffer, so I attach this
to a framebuffer. If this framebuffer is bound at the time of the draw
call, the output goes onto the texture. This is really powerful
because **this texture can be used as an input for another draw
command**, which is exactly what I'll be doing later.

Here's what making a single step of the simulation looks like.

~~~javascript
GOL.prototype.step = function() {
    var gl = this.gl;
    gl.bindFramebuffer(gl.FRAMEBUFFER, this.framebuffers.step);
    gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0,
                            gl.TEXTURE_2D, this.textures.back, 0);
    gl.viewport(0, 0, this.statesize.x, this.statesize.y);
    gl.bindTexture(gl.TEXTURE_2D, this.textures.front);
    this.programs.gol.use()
        .attrib('quad', this.buffers.quad, 2)
        .uniform('state', 0, true)
        .uniform('scale', this.statesize)
        .draw(gl.TRIANGLE_STRIP, 4);
    this.swap();
    return this;
};
~~~

First, bind the custom framebuffer as the current framebuffer with
`glBindFramebuffer()`. This framebuffer was previously created with
`glCreateFramebuffer()` and required no initial configuration. The
configuration is entirely done here, where the back texture is
attached to the current framebuffer. This replaces any texture that
might currently be attached to this spot — like the front texture
from the previous iteration. Finally, the size of the drawing area is
locked to the size of the simulation state with `glViewport()`.

[Using Igloo again][igloo] to keep the call concise, a fullscreen quad
is rendered so that the fragment shader runs *exactly* once for each
cell. That `state` uniform is the front texture, bound as
`GL_TEXTURE0`.

With the drawing complete, the buffers are swapped. Since every pixel
was drawn, there's no need to ever use `glClear()`.

### The Game of Life Fragment Shader

The simulation rules are coded entirely in the fragment shader. After
initialization, JavaScript's only job is to make the appropriate
`glDrawArrays()` call over and over. To run different cellular automata,
all I would need to do is modify the fragment shader and generate an
appropriate initial state for it.

~~~glsl
uniform sampler2D state;
uniform vec2 scale;

int get(int x, int y) {
    return int(texture2D(state, (gl_FragCoord.xy + vec2(x, y)) / scale).r);
}

void main() {
    int sum = get(-1, -1) +
              get(-1,  0) +
              get(-1,  1) +
              get( 0, -1) +
              get( 0,  1) +
              get( 1, -1) +
              get( 1,  0) +
              get( 1,  1);
    if (sum == 3) {
        gl_FragColor = vec4(1.0, 1.0, 1.0, 1.0);
    } else if (sum == 2) {
        float current = float(get(0, 0));
        gl_FragColor = vec4(current, current, current, 1.0);
    } else {
        gl_FragColor = vec4(0.0, 0.0, 0.0, 1.0);
    }
}
~~~

The `get(int, int)` function returns the value of the cell at (x, y),
0 or 1. For the sake of simplicity, the output of the fragment shader
is solid white and black, but just sampling one channel (red) is good
enough to know the state of the cell. I've learned that loops and
arrays are are troublesome in GLSL, so I've manually unrolled the
neighbor check. Cellular automata that have more complex state could
make use of the other channels and perhaps even exploit alpha channel
blending in some special way.

Otherwise, this is just a straightforward encoding of the rules.

### Displaying the State

What good is the simulation if the user doesn't see anything? So far
all of the draw calls have been done on a custom framebuffer. Next
I'll render the simulation state to the default framebuffer.

~~~javascript
GOL.prototype.draw = function() {
    var gl = this.gl;
    gl.bindFramebuffer(gl.FRAMEBUFFER, null);
    gl.viewport(0, 0, this.viewsize.x, this.viewsize.y);
    gl.bindTexture(gl.TEXTURE_2D, this.textures.front);
    this.programs.copy.use()
        .attrib('quad', this.buffers.quad, 2)
        .uniform('state', 0, true)
        .uniform('scale', this.viewsize)
        .draw(gl.TRIANGLE_STRIP, 4);
    return this;
};
~~~

First, bind the default framebuffer as the current buffer. There's no
actual handle for the default framebuffer, so using `null` sets it to
the default. Next, set the viewport to the size of the display. Then
use the "copy" program to copy the state to the default framebuffer
where the user will see it. One pixel per cell is *far* too small, so
it will be scaled as a consequence of `this.viewsize` being four times
larger.

Here's what the "copy" fragment shader looks like. It's so simple
because I'm storing the simulation state in black and white. If the
state was in a different format than the display format, this shader
would need to perform the translation.

~~~glsl
uniform sampler2D state;
uniform vec2 scale;

void main() {
    gl_FragColor = texture2D(state, gl_FragCoord.xy / scale);
}

~~~

Since I'm scaling up by four — i.e. 16 pixels per cell — this
fragment shader is run 16 times per simulation cell. Since I used
`GL_NEAREST` on the texture there's no funny business going on here.
If I had used `GL_LINEAR`, it would look blurry.

You might notice I'm passing in a `scale` uniform and using
`gl_FragCoord`. The `gl_FragCoord` variable is in window-relative
coordinates, but when I sample a texture I need unit coordinates:
values between 0 and 1. To get this, I divide `gl_FragCoord` by the
size of the viewport. Alternatively I could pass the coordinates as a
varying from the vertex shader, automatically interpolated between the
quad vertices.

An important thing to notice is that **the simulation state never
leaves the GPU**. It's updated there and it's drawn there. The CPU is
operating the simulation like the strings on a marionette — *from a
thousand feet up in the air*.

### User Interaction

What good is a Game of Life simulation if you can't poke at it? If all
of the state is on the GPU, how can I modify it? This is where
`glTexSubImage2D()` comes in. As its name implies, it's used to set
the values of some portion of a texture. I want to write a `poke()`
method that uses this OpenGL function to set a single cell.

~~~javascript
GOL.prototype.poke = function(x, y, value) {
    var gl = this.gl,
        v = value * 255;
    gl.bindTexture(gl.TEXTURE_2D, this.textures.front);
    gl.texSubImage2D(gl.TEXTURE_2D, 0, x, y, 1, 1,
                     gl.RGBA, gl.UNSIGNED_BYTE,
                     new Uint8Array([v, v, v, 255]));
    return this;
};
~~~

Bind the front texture, set the region at (x, y) of size 1x1 (a single
pixel) to a very specific RGBA value. There's nothing else to it. If
you click on the simulation in my demo, it will call this poke method.
This method could also be used to initialize the entire simulation
with random values, though it wouldn't be very efficient doing it one
pixel at a time.

### Getting the State

What if you wanted to read the simulation state into CPU memory,
perhaps to store for reloading later? So far I can set the state and
step the simulation, but there's been no way to get at the data.
Unfortunately I can't directly access texture data. There's nothing
like the inverse of `glTexSubImage2D()`. Here are a few options:

* Call `toDataURL()` on the canvas. This would grab the rendering of
  the simulation, which would need to be translated back into
  simulation state. Sounds messy.

* Take a screenshot. Basically the same idea, but even messier.

* Use `glReadPixels()` on a framebuffer. The texture can be attached to
  a framebuffer, then read through the framebuffer. This is the right
  solution.

I'm reusing the "step" framebuffer for this since it's already
intended for these textures to be its attachments.

~~~javascript
GOL.prototype.get = function() {
    var gl = this.gl, w = this.statesize.x, h = this.statesize.y;
    gl.bindFramebuffer(gl.FRAMEBUFFER, this.framebuffers.step);
    gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0,
                            gl.TEXTURE_2D, this.textures.front, 0);
    var rgba = new Uint8Array(w * h * 4);
    gl.readPixels(0, 0, w, h, gl.RGBA, gl.UNSIGNED_BYTE, rgba);
    return rgba;
};
~~~

Voilà! This `rgba` array can be passed directly back to
`glTexSubImage2D()` as a perfect snapshot of the simulation state.

### Conclusion

This project turned out to be far simpler than I anticipated, so much
so that I was able to get the simulation running within an evening's
effort. I learned a whole lot more about WebGL in the process, enough
for me to revisit [my WebGL liquid simulation][liquid]. It uses a
similar texture-drawing technique, which I really fumbled through that
first time. I dramatically cleaned it up, making it fast enough to run
smoothly on my mobile devices.

Also, this Game of Life implementation is *blazing* fast. If rendering
is skipped, **it can run a 2048x2048 Game of Life at over 18,000
iterations per second!** However, this isn't terribly useful because
it hits its steady state well before that first second has passed.


[life]: http://en.wikipedia.org/wiki/Conway%27s_Game_of_Life
[voronoi]: /blog/2014/06/01/
[torus]: http://en.wikipedia.org/wiki/Wraparound_(video_games)
[fbo]: http://www.opengl.org/wiki/Framebuffer_Object#Framebuffer_Completeness
[webgl]: /blog/2013/06/10/
[igloo]: https://github.com/skeeto/igloojs
[liquid]: /blog/2013/06/26/
