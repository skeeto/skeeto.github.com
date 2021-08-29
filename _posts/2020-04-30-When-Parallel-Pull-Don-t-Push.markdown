---
title: "When Parallel: Pull, Don't Push"
layout: post
date: 2020-04-30T22:35:51Z
tags: [optimization, interactive, javascript, opengl, media, webgl, c]
uuid: ac12ef1d-299f-4edb-9eb1-5ed4dac1219c
excerpt_separator: <!--more-->
---

*This article was discussed [on Hacker News][hn].*

I've noticed a small pattern across a few of my projects where I had
vectorized and parallelized some code. The original algorithm had a
"push" approach, the optimized version instead took a "pull" approach.
In this article I'll describe what I mean, though it's mostly just so I
can show off some pretty videos, pictures, and demos.

<!--more-->

### Sandpiles

A good place to start is the [Abelian sandpile model][wp], which, like
many before me, completely [captured][snipe] my attention for awhile.
It's a cellular automaton where each cell is a pile of grains of sand —
a sandpile. At each step, any sandpile with more than four grains of
sand spill one grain into its four 4-connected neighbors, regardless of
the number of grains in those neighboring cell. Cells at the edge spill
their grains into oblivion, and those grains no longer exist.

With excess sand falling over the edge, the model eventually hits a
stable state where all piles have three or fewer grains. However, until
it reaches stability, all sorts of interesting patterns ripple though
the cellular automaton. In certain cases, the final pattern itself is
beautiful and interesting.

Numberphile has a great video describing how to [form a group over
recurrent configurations][np] ([also][v2]). In short, for any given grid
size, there's a stable *identity* configuration that, when "added" to
any other element in the group will stabilize back to that element. The
identity configuration is a fractal itself, and has been a focus of
study on its own.

Computing the identity configuration is really just about running the
simulation to completion a couple times from certain starting
configurations. Here's an animation of the process for computing the
64x64 identity configuration:

[![](/img/identity-64-thumb.png)][v64]

As a fractal, the larger the grid, the more self-similar patterns there
are to observe. There are lots of samples online, and the biggest I
could find was [this 3000x3000 on Wikimedia Commons][3k]. But I wanted
to see one *that's even bigger, damnit*! So, skipping to the end, I
eventually computed this 10000x10000 identity configuration:

[![](/img/identity-10000-thumb.png)](/img/identity-10000.png)

This took 10 days to compute using my optimized implementation:

<https://github.com/skeeto/scratch/blob/master/animation/sandpiles.c>

I picked an algorithm described [in a code golf challenge][algo]:

    f(ones(n)*6 - f(ones(n)*6))

Where `f()` is the function that runs the simulation to a stable state.

I used [OpenMP to parallelize across cores, and SIMD to parallelize
within a thread][simd]. Each thread operates on 32 sandpiles at a time.
To compute the identity sandpile, each sandpile only needs 3 bits of
state, so this could potentially be increased to 85 sandpiles at a time
on the same hardware. The output format is my old mainstay, Netpbm,
[including the video output][ppm].

#### Sandpile push and pull

So, what do I mean about pushing and pulling? The naive approach to
simulating sandpiles looks like this:

```
for each i in sandpiles {
    if input[i] < 4 {
        output[i] = input[i]
    } else {
        output[i] = input[i] - 4
        for each j in neighbors {
            output[j] = output[j] + 1
        }
    }
}
```

As the algorithm examines each cell, it *pushes* results into
neighboring cells. If we're using concurrency, that means multiple
threads of execution may be mutating the same cell, which requires
synchronization — locks, [atomics][lstack], etc. That much
synchronization is the death knell of performance. The threads will
spend all their time contending for the same resources, even if it's
just false sharing.

The solution is to *pull* grains from neighbors:

```
for each i in sandpiles {
    if input[i] < 4 {
        output[i] = input[i]
    } else {
        output[i] = input[i] - 4
    }
    for each j in neighbors {
        if input[j] >= 4 {
            output[i] = output[i] + 1
        }
    }
}
```

Each thread only modifies one cell — the cell it's in charge of updating
— so no synchronization is necessary. It's shader-friendly and should
sound familiar if you've seen [my WebGL implementation of Conway's Game
of Life][gol]. It's essentially the same algorithm. If you chase down
the various Abelian sandpile references online, you'll eventually come
across a 2017 paper by Cameron Fish about [running sandpile simulations
on GPUs][fish]. He cites my WebGL Game of Life article, bringing
everything full circle. We had spoken by email at the time, and he
[shared his **interactive simulation** with me][sim].

Vectorizing this algorithm is straightforward: Load multiple piles at
once, one per SIMD channel, and use masks to implement the branches. In
my code I've also unrolled the loop. To avoid bounds checking in the
SIMD code, I pad the state data structure with zeros so that the edge
cells have static neighbors and are no longer special.

### WebGL Fire

Back in the old days, one of the [cool graphics tricks was fire
animations][fs]. It was so easy to implement on limited hardware. In
fact, the most obvious way to compute it was directly in the
framebuffer, such as in [the VGA buffer][dos], with no outside state.

There's a heat source at the bottom of the screen, and the algorithm
runs from bottom up, propagating that heat upwards randomly. Here's the
algorithm using traditional screen coordinates (top-left corner origin):

```
func rand(min, max) // random integer in [min, max]

for each x, y from bottom {
    buf[y-1][x+rand(-1, 1)] = buf[y][x] - rand(0, 1)
}
```

As a *push* algorithm it works fine with a single-thread, but
it doesn't translate well to modern video hardware. So convert it to a
*pull* algorithm!

```
for each x, y {
    sx = x + rand(-1, 1)
    sy = y + rand(1, 2)
    output[y][x] = input[sy][sx] - rand(0, 1)
}
```

Cells pull the fire upward from the bottom. Though this time there's a
catch: *This algorithm will have subtly different results.*

* In the original, there's a single state buffer and so a flame could
  propagate upwards multiple times in a single pass. I've compensated
  here by allowing a flames to propagate further at once.

* In the original, a flame only propagates to one other cell. In this
  version, two cells might pull from the same flame, cloning it.

In the end it's hard to tell the difference, so this works out.

[![](/img/fire-thumb.png)][fire]

[source code and instructions][src]

There's still potentially contention in that `rand()` function, but this
can be resolved [with a hash function][glhash] that takes `x` and `y` as
inputs.


[3k]: https://commons.wikimedia.org/wiki/File:Sandpile_group_identity_on_3000x3000_grid.png
[algo]: https://codegolf.stackexchange.com/a/106990
[dos]: /blog/2014/12/09/
[fire]: https://nullprogram.com/webgl-fire/
[fish]: http://people.reed.edu/~davidp/homepage/students/fish.pdf
[fs]: http://fabiensanglard.net/doom_fire_psx/
[glhash]: https://www.shadertoy.com/view/WttXWX
[gol]: /blog/2014/06/10/
[hn]: https://news.ycombinator.com/item?id=23089729
[lstack]: /blog/2014/09/02/
[np]: https://www.youtube.com/watch?v=1MtEUErz7Gg
[ppm]: /blog/2017/11/03/
[sim]: https://people.reed.edu/~davidp/web_sandpiles/
[simd]: /blog/2015/07/10/
[snipe]: https://xkcd.com/356/
[src]: https://github.com/skeeto/webgl-fire/
[v2]: https://www.youtube.com/watch?v=hBdJB-BzudU
[v64]: https://nullprogram.com/video/?v=sandpiles-64
[wp]: https://en.wikipedia.org/wiki/Abelian_sandpile_model
