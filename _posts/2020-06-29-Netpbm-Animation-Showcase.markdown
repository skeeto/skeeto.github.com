---
title: Netpbm Animation Showcase
layout: post
date: 2020-06-29T21:03:02Z
tags: [c, media]
uuid: 282d487d-5840-4c30-9aa8-3d0d0f07bef2
---

Ever since I worked out [how to render video from scratch][mm] some
years ago, it's been an indispensable tool in my software development
toolbelt. It's the first place I reach when I need to display some
graphics, even if it means having to do the rendering myself. I've used
it often in throwaway projects in a disposable sort of way. More
recently, though, I've kept better track of these animations since some
of them *are* pretty cool, and I'd like to look a them again. This post
is a showcase of some of these projects.

Each project is in a ready-to-run state of compile, then run with the
output piped into a media player or video encoding. The header includes
the exactly commands you need. Since that's probably inconvenient for
most readers, I've included a pre-recorded sample of each. Though in a
few cases, especially those displaying random data, video encoding
really takes something away from the final result, and it may be worth
running yourself.

The projects are not in any particular order.

### RANDU

[![][randu-i]][randu-v]  
**Source**:  [randu.c][randu-s]

This is a little demonstration of the poor quality of the [RANDU
pseudorandom number generator][randu]. Note how the source embeds a
monospace font so that it can render the text in the corner. For the 3D
effect, it includes an orthographic projection function. This function
will appear again later since I tend to cannibalize my own projects.

### Color sorting

[![][colorsort-i]][colorsort-v]  
**Source**:  [colorsort.c][colorsort-s]

The original idea came from [an old reddit post][colorsort].

### Kruskal maze generator

[![][animaze-i]][animaze-v]  
**Source**:  [animaze.c][animaze-s]

This effect was invented by my current [mentee student][animaze] while
working on maze / dungeon generation late last year. This particular
animation is my own implementation. It outputs Netpbm by default, but,
for both fun and practice, also includes an entire implementation [in
OpenGL][opengl]. It's enabled at compile time with `-DENABLE_GL` so long
as you have GLFW and GLEW (even on Windows!).

### Sliding rooks puzzle

[![][rooks-i]][rooks-v]  
**Source**:  [rooks.c][rooks-s]

I wanted to watch an animated solution to [the sliding rooks
puzzle][rooks]. This program solves the puzzle using a bitboard, then
animates the solution. The rook images are embedded in the program,
compressed using a custom run-length encoding (RLE) scheme with a tiny
palette.

### Glauber’s dynamics

[![][magnet-i]][magnet-v]  
**Source**:  [magnet.c][magnet-s]

My own animation of [Glauber’s dynamics][magnet] using a totally
unoriginal color palette.

### Fire

[![][fire-i]][fire-v]  
**Source**:  [fire.c][fire-s]

This is the [classic Doom fire animation][fire]. I later [implemented it
in WebGL][fire-webgl] with a modified algorithm.

### Mersenne Twister

[![][mt-i]][mt-v]  
**Source**:  [mtvisualize.c][mt-s]

A visualization of the Mersenne Twister pseudorandom number generator.
Not terribly interesting, so I almost didn't include it.

### Pixel sorting

[![][pixelsort-i]][pixelsort-v]  
**Source**:  [pixelsort.c][pixelsort-s]

Another animation [inspired by a reddit post][pixelsort]. Starting from
the top-left corner, swap the current pixel to the one most like its
neighbors.

### Random walk (2D)

[![][walkers-i]][walkers-v]  
**Source**:  [walkers.c][walkers-s]

Another reproduction of [a reddit post][walkers]. This is recent enough
that I'm using a [disposable LCG][lcg].

### Manhattan distance Voronoi diagram

[![][voronoi-i]][voronoi-v]  
**Source**:  [voronoi.c][voronoi-s]

Another [reddit post][voronoi], though I think my version looks a lot
nicer. I like to play this one over and over on repeat with different
seeds.

### Random walk (3D)

[![][walk3d-i]][walk3d-v]  
**Source**:  [walk3d.c][walk3d-s]

Another ~~stolen idea~~ personal take [on a reddit post][walk3d]. This
features the orthographic projection function from the RANDU animation.
Video encoding makes a real mess of this one, and I couldn't work out
encoding options to make it look nice, so this one looks a lot better
"in person."

### Lorenz system

[![][lorenz-i]][lorenz-v]  
**Source**:  [lorenz.c][lorenz-s]

A 3D animation I adapted from the 3D random walk above, meaning it uses
the same orthographic projection. I have [a WebGL version of this
one][lorenz-webgl], but I like that I could do this in such a small
amount of code and without an existing rendering engine. Like before,
this is really damaged by video encoding and is best seen live.

Bonus: I made [an obfuscated version][lorenz-obf] just to show how
small this can get!


[animaze-i]: /img/showcase/animaze.jpg
[animaze-s]: https://github.com/skeeto/scratch/blob/master/animaze/animaze.c
[animaze-v]: https://nullprogram.com/video/?v=kruskal
[animaze]: /blog/2016/09/02/
[colorsort-i]: /img/showcase/colorsort.jpg
[colorsort-s]: https://github.com/skeeto/scratch/blob/master/animation/colorsort.c
[colorsort-v]: https://nullprogram.com/video/?v=colors-odd-even
[colorsort]: https://old.reddit.com/r/woahdude/comments/73oz1x/from_chaos_to_order/
[fire-i]: /img/showcase/fire.jpg
[fire-s]: https://github.com/skeeto/scratch/blob/master/animation/fire.c
[fire-v]: https://nullprogram.com/video/?v=fire
[fire-webgl]: /blog/2020/04/30/
[fire]: https://fabiensanglard.net/doom_fire_psx/
[lcg]: /blog/2019/11/19/
[lorenz-i]: /img/showcase/lorenz.jpg
[lorenz-obf]: https://gist.github.com/skeeto/45d825c01b00c10452634933d03e766d
[lorenz-s]: https://github.com/skeeto/scratch/blob/master/animation/lorenz.c
[lorenz-v]: https://nullprogram.com/video/?v=lorenz
[lorenz-webgl]: /blog/2018/02/15/
[magnet-i]: /img/showcase/magnet.jpg
[magnet-s]: https://github.com/skeeto/scratch/blob/master/animation/magnet.c
[magnet-v]: https://nullprogram.com/video/?v=magnet
[magnet]: http://bit-player.org/2019/glaubers-dynamics
[mm]: /blog/2017/11/03/
[mt-i]: /img/showcase/mt.jpg
[mt-s]: https://github.com/skeeto/scratch/blob/master/animation/mtvisualize.c
[mt-v]: https://nullprogram.com/video/?v=mt19937-shuffle
[opengl]: /blog/2015/06/06/
[pixelsort-i]: /img/showcase/pixelsort.jpg
[pixelsort-s]: https://github.com/skeeto/scratch/blob/master/animation/pixelsort.c
[pixelsort-v]: https://nullprogram.com/video/?v=pixelsort
[pixelsort]: https://old.reddit.com/r/generative/comments/9o1plu/generative_pixel_sorting_variant/
[randu-i]: /img/showcase/randu.jpg
[randu-s]: https://github.com/skeeto/scratch/blob/master/animation/randu.c
[randu-v]: https://nullprogram.com/video/?v=randu
[randu]: https://en.wikipedia.org/wiki/RANDU
[rooks-i]: /img/showcase/rooks.jpg
[rooks-s]: https://github.com/skeeto/scratch/blob/master/animation/rooks.c
[rooks-v]: https://nullprogram.com/video/?v=rooks
[rooks]: https://possiblywrong.wordpress.com/2020/05/20/sliding-rooks-and-queens/
[voronoi-i]: /img/showcase/voronoi.jpg
[voronoi-s]: https://github.com/skeeto/scratch/blob/master/animation/voronoi.c
[voronoi-v]: https://nullprogram.com/video/?v=voronoi
[voronoi]: https://old.reddit.com/r/proceduralgeneration/comments/fuy6tk/voronoi_with_manhattan_distance_in_c/
[walk3d-i]: /img/showcase/walk3d.jpg
[walk3d-s]: https://github.com/skeeto/scratch/blob/master/animation/walk3d.c
[walk3d-v]: https://nullprogram.com/video/?v=walk3d
[walk3d]: https://old.reddit.com/r/proceduralgeneration/comments/geka1q/random_walking_in_3d/
[walkers-i]: /img/showcase/walkers.jpg
[walkers-s]: https://github.com/skeeto/scratch/blob/master/animation/walkers.c
[walkers-v]: https://nullprogram.com/video/?v=walk2d
[walkers]: https://old.reddit.com/r/proceduralgeneration/comments/g49qwk/random_walkers_abstract_art/
