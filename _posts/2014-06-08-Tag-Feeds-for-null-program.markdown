---
title: Tag Feeds for null program
layout: post
date: 2014-06-08T05:53:46Z
tags: [meta, rant]
uuid: f47e5404-cc4a-3cc0-01ce-a844c04721b8
---

I just added a [formal tags page][tags] along with individual feeds
for each tag. I've had tags for a couple of years now, but they were
really only useful for traveling sideways to similar articles. So now,
if you're only interested in a subset of my content, you can subscribe
to one or more tags rather than the main Atom feed.

What prompted this? In [my *Emacs Chat*][chat], Sacha asked me if this
blog was part of [Planet Emacsen][emacsen] (currently, it's not). If
my tags are accurate, only about 25% of my articles are about Emacs,
so most of my blog isn't relevant there. Tag feeds will go a long way
to help support these "planet" aggregators, should they want to
include my articles. For example, Planet Emacsen would use [my Emacs
feed][emacs].

### Static Site Generation

I couldn't practically support these extra feeds until recently.
Remember, this blog [is statically generated][host]. More feeds means
more content to generate, because articles are duplicated in whole for
each feed. In past years, Jekyll would probably take on the order of
an hour to do all this for a single build. Fortunately, Jekyll has
improved dramatically, especially in the past year or so, and these
feeds have little impact on the total build time. It's currently
around 10 seconds or so. Not bad at all!

A consequence of being statically generated is that you can't ask for
a combination of tags as a single feed. It would be a combinatorial
nightmare (billions of feeds). Plus, the request would have to
normalize the tag order (e.g. alphabetical) or else the combinatorial
explosion to be far worse (i.e. exceeding the number of atoms in the
universe). So I hope you can forgive me when subscribing to each tag
individually.

### Duplicate Articles

What if an article matches multiple tags? It will appear in each feed
where it's tagged, possibly showing up multiple times in your web feed
reader. Fortunately, this is where Atom saves the day! I'm leveraging
Atom's prudent design to make this work cleanly. Articles' UUIDs are
consistent across all of these feeds, so if your web feed reader is
smart enough, it will recognize these as being the same article. For
example, this article is `{{ page.uuid }}` regardless of which feed
you see it in.

Unfortunately, [Elfeed][elfeed] isn't smart enough for this. Sorry! In
order to better support all the broken RSS feeds out there, [I had to
compromise on entry keying][db]. I couldn't trust RSS feeds to provide
me a reasonably unique key, so, transitively, Elfeed doesn't fully
trust Atom's UUIDs either. These RSS feeds are broken largely because
[RSS itself is a broken mess][rss]. When making new feeds in the
future, please use Atom!

Atom *requires* that every feed and article have a proper UUID. It
doesn't matter where you get the feed from. You could subscribe to the
same exact feed at three different URLs (mirrors perhaps) and your
reader could reliably use the UUIDs to avoid duplication. Or, if
you're subscribed to an aggregator like Planet Emacsen, and it
includes content from a feed to which you're also directly subscribed,
your reader client should be able to merge these articles. In
comparison, RSS not only doesn't require UUIDs, it actively
discourages them with its broken `guid` tag, so merging content from
multiple sources is impossible with RSS.

Anyway, if most of my content doesn't suit you, you can now subscribe
to the subset that does. Aren't Atom feeds cool?


[emacs]: /tags/emacs/feed/
[tags]: /tags/
[chat]: /blog/2014/06/04/
[emacsen]: http://planet.emacsen.org/
[host]: /blog/2011/08/05/
[rss]: /blog/2013/09/23/
[db]: /blog/2013/09/09/
[elfeed]: /blog/2013/09/04/
