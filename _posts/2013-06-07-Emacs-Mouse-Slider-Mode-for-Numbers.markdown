---
title: Emacs Mouse Slider Mode for Numbers
layout: post
tags: [emacs, media]
uuid: 26f803e9-776a-309d-9d7d-76448c2d1231
---

One of my regular commenters, and as of recently co-worker, Ahmed
Fasih, sent me a video, [Live coding in Lua][lua]. The author of the
video added support to his IDE for scaling numbers in source code by
dragging over them with the mouse. This feature was directly inspired
by [Bret Victor][bv], a user interface visionary, probably best
introduced through his presentation [Inventing on Principle][invent].

I think Bret's interface ideas are interesting and his demos very
impressive. However, I feel they're *too* specialized to generally be
very useful. [Skewer][skewer] suffers from the same problem: in order
to truly be useful, programs need to be written in a form that expose
themselves well enough for Skewer to manipulate at run-time. Some
styles of programming are simply better suited to live development
than others. This problem is amplified in Bret's case by the extreme
specialty of the tools. They're fun to play with, and probably great
for education, but I can't imagine any time I would find them useful
while being productive.

Anyway, Ahmed wanted to know if it would be possible to implement this
feature in Emacs. I said yes, knowing that Emacs ships with
[artist-mode][artist], where the mouse can be used to draw with
characters in an editing buffer. That's proof that Emacs has the
necessary mouse events to do the job. After spending a couple of hours
on the problem I was able to create a working prototype:
**mouse-slider-mode**.

 * [https://github.com/skeeto/mouse-slider-mode][msm]

<video src="https://nullprogram.s3.amazonaws.com/skewer/mouse-slider-mode.webm" controls="controls" width="350" height="350">
  Demo video requires HTML5 with WebM support.
</video>

It's a bit rough around the edges, but it works. When this minor mode
is enabled, right-clicking and dragging left or right on any number
will decrease or increase that number's value. More so, if the current
major mode has an entry in the `mouse-slider-mode-eval-funcs` alist,
as the value is scaled the expression around it is automatically
evaluated in the live environment. The documentation shows how to
enable this in js2-mode buffers using skewer-mode. This is actually a
step up from the other, non-Emacs implementations of this mouse slider
feature. If I understood correctly, the other implementations
re-evaluate the entire buffer on each update. My version only needs to
evaluate the surrounding expression, so the manipulated code doesn't
need to be so isolated.

There is one limitation that cannot be fixed using Elisp. If the mouse
exits the Emacs window, Elisp stops receiving valid mouse events.
Number scaling is limited by the width of the Emacs window. Fixing
this would require patching Emacs itself.

This is purely a proof-of-concept. It's not installed in my Emacs
configuration and I probably won't ever use it myself, except to show
it off as a flashy demo with an HTML5 canvas. If anyone out there
finds it useful, or thinks it could be better, go ahead and adopt it.


[lua]: http://youtu.be/FpxIfCHKGpQ
[bv]: http://worrydream.com/#
[invent]: http://youtu.be/PUv66718DII
[artist]: http://www.emacswiki.org/emacs/ArtistMode
[msm]: https://github.com/skeeto/mouse-slider-mode
[skewer]: https://github.com/skeeto/skewer-mode
