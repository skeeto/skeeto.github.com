---
title: Rumor Simulation
layout: post
tags: [java, math, media, video, reddit]
uuid: 9fee2022-d273-34d6-0970-546b5e875460
---

A couple months ago someone posted
[an interesting programming homework problem][reddit] on reddit,
asking for help. Help had already been provided before I got there,
but I thought the problem was an interesting one.

> Write a program that simulates the spreading of a rumor among a group
> of people. At any given time, each person in the group is in one of
> three categories:
>
> * IGNORANT - the person has not yet heard the rumor
> * SPREADER - the person has heard the rumor and is eager to spread it
> * STIFLER - the person has heard the rumor but considers it old news
>   and will not spread it
>
> At the very beginning, there is one spreader; everyone else is
> ignorant. Then people begin to encounter each other.
>
> So the encounters go like this:
>
> * If a SPREADER and an IGNORANT meet, IGNORANT becomes a SPREADER.
> * If a SPREADER and a STIFLER meet, the SPREADER becomes a STIFLER.
> * If a SPREADER and a SPREADER meet, they both become STIFLERS.
> * In all other encounters nothing changes.
>
> Your program should simulate this by repeatedly selecting two people
> randomly and having them "meet."
>
> There are three questions we want to answer:
>
> * Will everyone eventually hear the rumor, or will it die out before
>   everyone hears it?
> * If it does die out, what percentage of the population hears it?
> * How long does it take? i.e. How many encounters occur before the
>   rumor dies out?


I wrote a very thorough version to [produce videos][videos] of the
simulation in action.

* [https://github.com/skeeto/rumor-sim](https://github.com/skeeto/rumor-sim)

It accepts some command line arguments, so you don't need to edit any
code just to try out some simple things.

And here are a couple of videos. Each individual is a cell in a 2D
grid. IGNORANT is black, SPREADER is red, and STIFLER is white. Note
that this is *not* a cellular automata, because cell neighborship does
not come into play.

<video src="https://s3.amazonaws.com/nullprogram/rumor/rumor-small.webm"
       controls="controls" width="400" height="250">
</video>

<video src="https://s3.amazonaws.com/nullprogram/rumor/rumor.webm"
       controls="controls" width="400" height="400">
</video>

Here's are the statistics for ten different rumors.

    Rumor(n=10000, meetups=132380, knowing=0.789)
    Rumor(n=10000, meetups=123944, knowing=0.7911)
    Rumor(n=10000, meetups=117459, knowing=0.7985)
    Rumor(n=10000, meetups=127063, knowing=0.79)
    Rumor(n=10000, meetups=124116, knowing=0.8025)
    Rumor(n=10000, meetups=115903, knowing=0.7952)
    Rumor(n=10000, meetups=137222, knowing=0.7927)
    Rumor(n=10000, meetups=134354, knowing=0.797)
    Rumor(n=10000, meetups=113887, knowing=0.8025)
    Rumor(n=10000, meetups=139534, knowing=0.7938)

Except for very small populations, the simulation always terminates
very close to 80% rumor coverage. I don't understand (yet) why this
is, but I find it very interesting.


[reddit]: http://old.reddit.com/r/javahelp/comments/ngvp4/
[videos]: /blog/2011/11/28/
