---
title: Minimal OpenGL 3.3 Core Profile Demo
layout: post
date: 2015-06-06T19:35:34Z
tags: [opengl, c]
uuid: ada32e48-ae67-3da5-9772-7e61fee602c3
---

When I was first attempting to learn OpenGL years ago, what I really
wanted was a complete, minimal example program. OpenGL has enormous
flexibility and I wanted to [fully understand the fundamentals][fund]
in isolation before moving on to more advanced features. I had been
advised to specifically learn *core profile*, which drops nearly all
the legacy parts of the API.

However, since much of the OpenGL-related content to be found online,
even today, [is outdated][outdated] — and, worse, it's not marked as
such — good, modern core profile examples have been hard to come by.
The relevant examples I could find at the time were more complicated
than necessary, due to the common problem that full 3D graphics are
too closely conflated with OpenGL. The examples would include matrix
libraries, texture loading, etc. This is a big reason I ended up
[settling on WebGL][webgl]: a clean slate in a completely different
community. (The good news is that this situation has already improved
dramatically over the last few years!)

Until recently, [all of my OpenGL experience had been WebGL][tag].
Wanting to break out of that, earlier this year I set up a minimal
OpenGL 3.3 core profile demo in C, using [GLFW][glfw] and
[gl3w][gl3w]. You can find it here:

* [https://github.com/skeeto/opengl-demo](https://github.com/skeeto/opengl-demo)

No 3D graphics, no matrix library, no textures. It's just a spinning
red square.

![](/img/screenshot/opengl-demo.png)

It supports both Linux and Windows. The Windows' build is static, so
it compiles to a single, easily distributable, standalone binary. With
some minor tweaking it would probably support the BSDs as well. For
simplicity's sake, the shaders are baked right into the source as
strings, but if you're extending the demo for your own use, you may
want to move them out into their own source files.

### Why OpenGL 3.3?

I chose OpenGL 3.3 in particular for three reasons:

* Core and compatibility profiles were introduced in OpenGL 3.2
  (2009). Obviously anything that focuses on core profile is going to
  be 3.2 and up.
* OpenGL 3.3 (2010) [introduced version 3.30][gl330] of the shading
  language. This was a big deal and there's little reason not to take
  advantage of it. I specifically wanted to use the new `layout`
  keyword.
* Mesa 10.0 (2013) [supports up to OpenGL 3.3][mesa]. Mesa is the
  prominent 3D graphics library for open source operating systems.
  It's what applications use for both hardware-accelerated and
  software OpenGL rendering. This means the demo will work on any
  modern Linux installation. (Note: when running on older hardware
  without OpenGL 3.3 support, you may have to force software rendering
  with the environment variable `LIBGL_ALWAYS_SOFTWARE=1`. The
  software renderer will take advantage of your CPU's SIMD features.)

As far as "desktop" OpenGL goes, 3.3 is currently *the* prime target.

### Why GLFW?

Until [EGL][egl] someday fills this role, the process for obtaining an
OpenGL context is specific to each operating system, where it's
generally a pain in the butt. GLUT, the OpenGL Utility Toolkit, was a
library to make this process uniform across the different platforms.
It also normalized user input (keyboard and mouse) and provided some
basic (and outdated) utility functions.

The original GLUT isn't quite open source (licensing issues) and it's
no longer maintained. The open source replacement for GLUT is
[FreeGLUT][freeglut]. It's what you'd typically find on a Linux
system in place of the original GLUT.

I just need a portable library that creates a window, handles keyboard
and mouse events in that window, and gives me an OpenGL 3.3 core
profile context. FreeGLUT does this well, but we can do better. One
problem is that it includes a whole bunch of legacy cruft from GLUT:
immediate mode rendering utilities, menus, spaceball support, lots of
global state, and only one OpenGL context per process.

One of the biggest problems is that **FreeGLUT doesn't have a swap
interval function**. This is used to lock the application's redraw
rate to the system's screen refresh rate, preventing screen tearing
and excessive resource consumption. I originally used FreeGLUT for the
demo, and, as a workaround, had added my own macro work around this by
finding the system's swap interval function, but it was a total hack.

The demo was initially written with FreeGLUT, but I switched over to
[GLFW][glfw] since it's smaller, simpler, cleaner, and more modern.
GLFW also has portable joystick handling. With the plethora of modern
context+window creation libraries out there, it seems there's not much
reason to use FreeGLUT anymore.

[SDL 2.0][sdl] would also be an excellent choice. It goes beyond GLFW
with threading, audio, networking, image loading, and timers:
basically all the stuff you'd need when writing a game.

I'm sure there are some other good alternatives, especially when
you're not sticking to plain C, but these are the libraries I'm
familiar with at the time of this article.

### Why gl3w?

If you didn't think the interface between OpenGL and the operating
system was messy enough, I have good news for you. Neither the
operating system nor the video card drivers are going to provide any
of the correct headers, nor will you have anything meaningful to link
against! For these, you're on your own.

The OpenGL Extension Wrangler Library (GLEW) was invented solve this
problem. It dynamically loads the system's OpenGL libraries and finds
all the relevant functions at run time. That way your application
avoids linking to anything too specific. At compile time, it provides
the headers defining all of the OpenGL functions.

Over the years, GLEW has become outdated, to this day having no
support for core profile. So instead I used a replacement called
[gl3w][gl3w]. It's just like GLEW, but, as the name suggests, oriented
around core profile ... exactly what I needed. Unlike GLEW, it is
generated directly from Kronos' documentation by a script. In
practice, you drop the generated code directly into your project
(embedded) rather than rely on the system to provide it as a library.

A great (and probably better) alternative to gl3w is
[glLoadgen][glloadgen]. It's the same idea — an automatically
generated OpenGL loader — but allows for full customization of the
output, such as the inclusion of select OpenGL extensions.

### Conclusion

While I hope it serves an educational resources for others, I
primarily have it for my own record-keeping, pedagogical, and
reference purposes, born out of a weekend's worth of research. It's a
starting point for future projects, and it's somewhere easy to start
when I want to experiment with an idea.

Plus, someday I want to write a sweet, standalone game with fancy
OpenGL graphics.


[webgl]: /blog/2013/06/10/
[tag]: /tags/webgl/
[freeglut]: http://freeglut.sourceforge.net/
[gl3w]: https://github.com/skaslev/gl3w
[fund]: http://www.skorks.com/2010/04/on-the-value-of-fundamentals-in-software-development/
[outdated]: http://www.shamusyoung.com/twentysidedtale/?p=23079
[gl330]: https://www.opengl.org/wiki/History_of_OpenGL#OpenGL_3.3_.282010.29
[mesa]: http://en.wikipedia.org/wiki/Mesa_%28computer_graphics%29#Implementations_of_rendering_APIs
[egl]: https://www.khronos.org/egl/
[sdl]: https://www.libsdl.org/
[glfw]: http://www.glfw.org/
[glloadgen]: https://bitbucket.org/alfonse/glloadgen/wiki/Home
