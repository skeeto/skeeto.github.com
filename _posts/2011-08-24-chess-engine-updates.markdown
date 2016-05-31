---
title: October Chess Engine Updates
layout: post
tags: [java, game]
uuid: 438369c7-5406-3133-f427-237674b1e53e
---

When I wrote a [chess engine](/blog/2010/10/17/) a year ago, I left it
in a slightly incomplete state. The original intention was to try out
an old [genetic algorithm idea](/blog/2007/10/24/) of mine, and
creating a chess engine was just a side effect of that. Well, that
didn't work out so well since the vast majority of my engine's games
against itself would end in ties, leaving me with very little data to
work with.

I was left with a working chess engine with some rough edges. Mainly,
there was an "undo move" option that didn't work right, the graphics
could use a little work, and the only way to set the engine's strength
was by editing a text file and injecting it into the classpath.

Six months later it got
[some attention on a chess enthusiast forum](/blog/2011/04/09/). I
gave some advice on how to tweak the engine a bit and it made me
realize how hard that is for many people to do. "Hey, there are some
people actually interested in using my unnamed chess engine. Cool! I
should probably fix some of those rough edges." So I did.

First I gave it a name, since no one on that forum seemed to know what
to call it. Most of the work was done in October of 2010, so I went
with that: **October**. The October Chess Engine. It's also now in the
public domain, no copyrights attached.

Next, since I've learned so much about Java 2D graphics in the last
year, I reworked the graphics. The graphics code is shorter and
simpler, the GUI looks nicer, and it now permits user resizing of the
window. It's also a little larger by default.

I fixed the engine so that it can search an odd number of plies. All
it needed was a a sign change in one place. The hard part was figuring
out where exactly it needed to be done. With that fixed, I modified
the GUI so that the user can select the difficulty setting.

![](/img/chess/difficulty.png)

I've also advanced my knowledge of Java threading, cleaning up thread
management in the engine. There's no difference from a user
perspective, except that it no longer triggers thread-related JVM bugs
— which I've seen occur when rapidly instantiating and tearing down
many threads.

As a side-effect of tidying threading, I made an experimental branch
([`distributed`](https://github.com/skeeto/october-chess-engine/tree/distributed))
that allows the AI to make use of other computers in the network. The
entire board state is serialized, along with a single unevaluated
move, and sent off to other machines. They analyze the move and report
back with the move's score.

While investigating a move can be done independently, it misses out on
a serious optimization. Scores from previous moves can be used to
constrain the current search, allowing the AI to skip over entire
branches of the search tree
([alpha-beta pruning](http://en.wikipedia.org/wiki/Alpha-beta_pruning)).
When all 30-some possible moves are evaluated in parallel, that
optimization is completely lost. As a result, going from one machine
to two equal machines tends to have no real speedups, because
cumulatively they have to work harder than a single machine. It's not
until several machines are added that the optimization loss is
overcome.

Also with the threading change, I added an AI time estimate, so the
user knows how long the AI will take. This is important with the new
difficulty settings, since high difficulties can take awhile to
compute. Sometimes it's wildly off, particularly when the AI only
takes a few seconds, but, to my own surprise, it can be quite accurate
when the AI is taking several minutes to complete its turn. Because of
the previously-mentioned optimization, the AI speeds up as it
progresses, so newer timings are weighted more than older timings.

And finally, one of the most visible changes to the user, I created an
[NSIS installer](http://nsis.sourceforge.net/) for the Windows users
(available in the downloads section of
[October's website](https://github.com/skeeto/october-chess-engine)). They
can click their way through a familiar install wizard, making the
chess engine a neatly-packaged product. I got the idea from
[DCSS](http://crawl.develz.org/wordpress/), which has a very well
organized build system and makes good use of NSIS.

In the future I'd like to tidy up the experimental distributed AI
stuff, possibly rebuild it on top of Java RMI. I'd also like to add
support for the Universal Chess Interface (UCI). That would allow me
to determine an accurate rating for October, and it would give me an
excellent excuse to not do any more GUI work because another GUI could
easily be used in its place.
