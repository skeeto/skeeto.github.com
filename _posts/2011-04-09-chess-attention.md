---
title: October's Chess Engine Gets Some Attention
layout: post
tags: [java, game]
uuid: bf181782-accb-396e-2855-b7e49b23f2db
---

Looking at my server logs the other I noticed that the unnamed [Chess
engine I wrote this past October][prev] recently got [some attention on
the Immortal Chess forum][forum]. As far as I know this is the first
time anyone who's good at Chess has played it. They were analyzing its
behavior a bit too.

As I expected, in its default setting (the *only* setting without
editing text configuration files) it's a pretty weak engine. I did this
to keep is fast, so its turns would take only seconds even on older
hardware. In this state it searches four plies (good engines get as high
as 12). They were able to defeat it easily.

![](/img/chess/chess-defeat.png)

I posted some information on how they can tweak the engine without
having to edit source code or download any new files. Specifically, it's
easy to increase the search depth to six plies, or more. This
information may be useful in general for anyone wanting to tweak my
Chess engine.

> You can increase the AI's strength with some trickery, without having
> to go to the source code. Open the .jar file with your favorite
> archive manager (looks like 7-Zip is popular around here), and locate
> the file `com/nullprogram/chess/ai/default.properties`. Open it up in
> a text editor and you can see some configuration for the AI (and
> remember to save your changes back into the .jar file).
>
> The most important setting is "depth", which is a default of 4 plies
> right now. You can set this to any **even** number. If you're on
> modern hardware, 6 plies is still reasonably fast. 8 if you're
> patient. It's multithreaded, so more cores means faster AI. Each
> increase in ply is probably a strength increase of about 200-250, so 4
> to 6 is about a 450 point increase.
>
> You can also adjust piece values and situational weights ("safety" is
> king safety), which could possibly yield a more powerful AI. Changing
> these will not cause any more CPU use, but the payoff will probably be
> little.

The next step to making it stronger would be running a Java profiler,
finding the bottlenecks, and getting the engine sped up enough to search
more plies in the same amount of time. This would certainly involve
tightening up memory use, because the tree search currently creates lots
and lots of garbage. Another option is using opening books, but that's
not something I'm interested in doing — I like that it currently doesn't
really rely on any domain knowledge.

Even though it's a below-average engine, it looks like it still made it
into some personal Chess engine collections. Perhaps I should have put
some credits in there so that, in the future, they could have found
their way back here. I should have also stuck the source code in the
.jar so they'd have it around too.


[prev]: /blog/2010/10/17/
[forum]: http://immortalchess.net/forum/showthread.php?t=8845
