---
title: Life Beyond Google Reader
layout: post
tags: [web]
uuid: 7e14731d-8cb7-32d3-5ec2-e22d79aefdac
---

*Update September 2013*: I'm now using [Elfeed](/blog/2013/09/04/).
The Old Reader was a victim of its own success, unable to keep up with
its surge in popularity, and I ended up writing my own reader to serve
my needs.

Google Reader will close its doors in about two more weeks. A few
people had wanted to know what my plans were for accessing web feeds
(Atom/RSS) once Reader is dead. Well, I finally figured it out and the
process was much easier than I anticipated. The winner for me is
[The Old Reader](http://theoldreader.com/).

This seems like such a strange move from Google. Judging from the
public response to this news, Reader obviously still has widespread
popularity. Google completely dominates this market and they're
throwing a huge opportunity out the window. The official statement is
that the closure is due to Reader's decline in popularity. However,
[Reader remains *far* more popular than Google+][more]. My personal
theory is that [they want Reader users to switch to Google+][plus],
even though [social media is no replacement for web feeds][hitler].

Oh well. While Reader's closure will probably be a step backwards for
web feeds in the short term, I think in the long term this will
ultimately be a good thing. Feature-wise Reader has stagnated over the
years. Removing this massively Google-subsidized client from the
market should [open up some interesting competition][good-news].

I waited awhile to look around for alternatives. Almost to the last
minute, you might say. Google's announcement in March was very sudden
and unexpected, and the alternatives quickly found themselves
overwhelmed. I wanted to give them time to respond to this massive
shift in the market before evaluating them.

### Requirements

From my experience with Google Reader, knowing my personal needs, I
developed a set of requirements that any replacement would need to
meet.

#### A cloud-based web application

Surprisingly to some, readers have a significant amount of state. Not
only do they need to store all of the feed URLs, they need to keep
track of which articles in each feed are read and unread. If a local
client, of which there are many to choose from, is used, this state is
stored on the local machine, tying the use of a reader down to a
single computer.

I see two ways to work around this. One would be to configure the
client to keep this state in locally-mounted cloud storage. I don't
currently have a solution in place for this sort of thing, nor would
such a solution be very friendly to access from the workplace.

The second is to use a local client that exposes a web interface.
Basically, hosting a reader service myself. Should any cloud-based
services be unreasonable, this would probably be the route I'd take.
However, I'd really prefer to not have to manage another host. I'd
have to worry about backing up the reader state and keeping the
service running. When I eventually move onto newer computers, I'd have
to migrate all of this as well.

Unfortunately, the Google Takeout export format (OPML) doesn't
including any of this state, just the subscription list. This state
will need to be resolved manually on the initial import no matter what
client I choose. In [contrast to others][luke], I personally have 0
unread articles most of the time, so this isn't difficult for me.

There's the privacy concern of using a cloud service. Someone I don't
know will have full access to a significant portion of my online
reading. This isn't really an issue for me. If you look at my
navigation side-bar here you'll see a listing of most of the feeds I
follow, making this information public anyway.

#### Support for a large number of feeds

I have around 150 subscriptions at the moment. I keep my subscription
list trimmed down to feeds active within the last year, so this cannot
be reduced any further. The new client must support *at least* twice
this number of feeds, since my trimmed subscription list grows with
time.

Google Reader offered an unlimited number of subscriptions at no cost.
I'd love for the alternative to also have no cost, but this isn't a
hard requirement, just a preference. I'd be willing to pay a few
dollars per month to support an unlimited number of feeds.

However, I would like for there to be some kind of full-featured trial
period, or the ability to pay for just one month so that I can import
all of my feeds and give the service a full test drive without
committing to it.

#### Support for reading articles in browser tabs

I don't actually read anything inside the reader itself. Articles that
are more complicated than plain text can't really adjust to any
arbitrary reader frame around them (including my own blog), so I don't
expect them to. The reader is only there to inform me that a new
article has been published.

When new articles arrive I pop them out into new tabs for viewing. If
there are many articles to be read, I position the mouse over the
title of the first article, middle click it, then hit `n` to advance
to the next. Alternating between middle-clicking and `n` I can quickly
knock each article out into a tab. Then I just go through the tabs,
closing them as I consume articles. Occasionally tabs remain open for
a couple of days until I finish them.

This means the alternative must not use fake JavaScript "links" that
can't be middle-clicked into its own tab. It needs to play nice with
the browser.

### Soft Requirements

These are things that would be nice, but have little impact on my
decision.

#### Support for mobile devices

While I [recently starting using a mobile device](/blog/2013/04/27/) I
don't currently access an web feed reader from it. It really comes
down to the one-article-per-tab thing, where I don't want to read
articles inside the reader itself. However, maybe someday I'll start
access a reader this way, so support would be nice.

#### Open Source

I'm using it entirely as a cloud service with no intention on running
it on my own machine, so this isn't very important. However, it would
be nice to see what's going on, and maybe even submit a patch to fix
problems I find.

This has been one of my biggest annoyance with these "app stores"
popping up over the last few years. There's no metadata for indicating
where to find an app's source code (if available), even if it's just a
link to a GitHub repository. When I find bugs in apps I have no way to
fix them myself — something I have taken for granted with Debian and
Emacs. These app stores are not made for technical people or power
users.

#### No social/sharing services in the way

Google Reader has this and I never used it. I don't really mind if
it's there, but it needs to stay out of the way.

#### Import/export

Very convenient, but I can live without it. Because I keep my
subscription list well-curated, going through them all one-at-a-time
to move to another client isn't a big deal. On the other hand, I'd
really prefer not to go through this process just to evaluate a
potential reader client.

### The Evaluations

I did a number of searches to learn the names of the alternative
readers so that I could evaluate them. These four were the most
popular, being named over and over in the results.

#### [Feedly](http://www.feedly.com/)

This one seems to have the most popularity of all. It's cloud-based,
but it's not a web application, rather it's a browser extension. This
doesn't fit the first requirement.

#### [Feedbin](https://feedbin.me/)

Unlike the others, this one's a straight $2 per month with no free
version. Fortunately you don't actually get billed unless you stick
around for three days. Unfortunately this one wasn't for me. The
interface is completely incompatible with reading articles in their
own tabs, [among other issues][review].

However, they do have [a really slick API][feedbin-api].

#### [NewsBlur](http://www.newsblur.com/)

This is the one that caught my eyes months ago. It's even open source,
in case I ever wanted to run my own private instance. However, I'm not
satisfied with the interface. It really wants everything to be read
within the client itself rather than popped out into new tabs.

Going beyond 64 feeds also costs $24 per year. That's a reasonable
price, but these circumstances make it hard to give it a full test
drive for a few days.

This one's a close second place. It also [has an API][newsblur-api].

#### [The Old Reader](http://theoldreader.com/)

Here's the winner. As advertised, the interface is almost exactly the
same as Reader, which makes it entirely compatible with reading
articles in their own tabs.

What's really insane is that it's entirely free to use for an
unlimited number of feeds! They're really copying Reader as far as
possible here. They do [accept donations][donate] to cover their
significant server costs. I intend to donate the typical web feed
reader subscription fee of $2/month, in yearly installments, when
Reader finally shuts down next month.

The downside is that it's *much* slower than Reader at getting updates
from feeds. [Up to a full day slower][slow]. I don't know how they did
it but Reader managed to catch articles just minutes after they were
published. I believe this is partly due to
[PubSubHubbub][pubsubhubbub], but they managed this speed even with my
own blog, which definitely doesn't use PubSubHubbub (I'm not pinging a
hub when I publish).

Slow updating is the only downside I've had so far, and it seems to be
an issue with all readers except Google Reader. If the option was
provided, I'd pay a premium to have feeds update faster.

### Optimism

Google Reader represented a significant part of my daily schedule. It
was my breakfast morning newspaper for about 6 years. Thanks to web
comics, it even had a metaphorical comic section. I'm just getting
settled into this new alternative and I'm crossing my fingers that it
will do as good of a job. I think it will.


[more]: http://www.buzzfeed.com/jwherrman/google-reader-still-sends-far-more-traffic-than-google
[hitler]: http://youtu.be/A25VgNZDQ08
[feedbin-api]: https://github.com/feedbin/feedbin-api
[newsblur-api]: http://www.newsblur.com/api
[review]: http://www.makeuseof.com/tag/feedbin-a-google-reader-replacement-that-may-be-worth-2-per-month/
[donate]: http://theoldreader.com/pages/donate
[pubsubhubbub]: https://code.google.com/p/pubsubhubbub/
[slow]: http://theoldreader.uservoice.com/knowledgebase/articles/146275-how-often-are-feeds-updated-i-see-some-delays-
[plus]: http://thenextweb.com/google/2013/03/14/former-google-reader-product-manager-confirms-our-suspicions-its-demise-is-all-about-google/
[good-news]: http://www.marco.org/2013/03/13/google-reader-sunset
[luke]: http://www.terminally-incoherent.com/blog/2013/03/18/goodbye-google-reader/
