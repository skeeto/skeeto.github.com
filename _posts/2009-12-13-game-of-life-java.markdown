---
title: Game of Life in Java
layout: post
tags: [java, interactive]
uuid: dfc591b0-65ed-39d6-80b2-57a8991c2a0d
---

<!-- 13 December 2009 -->

Sources:

<pre>
git clone <a href="https://github.com/skeeto/GameOfLife">git://github.com/skeeto/GameOfLife.git</a>
</pre>

<img src="/img/gol/game-of-life.png" width="250" alt="" title="Screenshot of this thing" class="right"/>

Since I recently got back into Java recently, I threw together this
little Game of Life implementation in Java. It looks a lot like my [maze generator/solver](/blog/2007/10/08/) on the inside,
reflecting the way I think about these things. Gavin wrote a [ competing version of the game](http://devrand.org/show_item.html?item=98&page=Project) in [Processing](http://processing.org/) which we were partially
discussing the other night, so I made my own.

The individual cells are actually objects themselves, so you could
inherit the abstract Cell class and drop in your own rules. I bet you
could even write a Cell that does the [ iterated prisoner's dilemma cellular automata](/blog/2007/11/06/). The Cell objects
are wired together into a sort of mesh network. Instead of growing it
wraps around on the sides.

It takes up to four arguments right now, with three types of cells,
`basic`, implementing the basic Conway's Game of Life,
`growth`, which is a cell that matures over time, and
`random` which mixes both types together (seen in the
screenshot). The arguments work as follows,

```
java -jar GoL.jar [<cell type> [<width> <height> [<cell pixel width>]]]
```

I may look into extending this to do some things beyond simple
cellular automata.
