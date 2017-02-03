---
title: Atom vs. RSS
layout: post
date: 2013-09-23T06:23:51Z
tags: [web, elfeed]
uuid: a36dba78-5234-3269-bb3c-dc1e939f12b1
---

From [working on Elfeed](/blog/2013/09/04/), I've recently become
fairly intimate with the Atom and RSS specifications. I needed to
write a parser for each that would properly handle valid feeds but
would also reasonably handle all sorts of broken feeds that it would
come across. At this point I'm quite confident in saying that **Atom
is *by far* the better specification** and I really wish RSS didn't
exist. This isn't surprising: Atom was created specifically in
response to RSS's flawed and ambiguous specification.

One consequence of this realization is that I've added an Atom feed to
this blog and made it the the primary feed. Because so many people are
still using the RSS feed, it will continue to be supported even though
there are no longer links to it (Ha, try to find it now!). You may
have noticed that I also started including the full post body in my
feed entries. Now that my feed usage habits have changed, I felt that
truncating content was actually rather rude. There's still the issue
that it contains relative URLs, but I'm not aware of any way to fix
this with Jekyll. I also got a lot more precise with dates. Until
recently, all posts occurred at midnight PST on the post date.

For reference, here are the specifications. Just these two documents
cover about 99% of the web feeds out there.

 * [Atom][atom]
 * [RSS 2.0][rss2]

Not that it matters too much, but it's unfortunate that RSS has sort
of "won" this format war. Of the feeds that I follow, about 75% are
RSS and 25% are Atom. That's still a significant number of web feeds
and Atom is well-supported by all the clients that I'm aware of, so
it's in no danger of falling out of use. The broken (but still valid)
RSS feeds I'm come across probably wouldn't be broken if they were
originally created as Atom feeds. Atom is a stricter standard and,
therefore, would have guided these authors to create their feeds
correctly from the start. **RSS encourages authors to do the *wrong*
thing.**

### The Flaws of RSS

For reference, here's a typical, friendly RSS 2.0 feed.

~~~xml
<?xml version="1.0" encoding="UTF-8"?>
<rss version="2.0">
  <channel>
    <title>Example RSS Feed</title>
    <item>
      <title>Example Item</title>
      <description>A summary.</description>
      <link>http://www.example.com/foo</link>
      <guid>http://www.example.com/foo</guid>
      <pubDate>Mon, 23 Sep 2013 03:00:05 GMT</pubDate>
    </item>
  </channel>
</rss>
~~~

#### guid, the misnomer

Two of the biggest RSS flaws — flaws that forced me to make a major
design compromise when writing Elfeed — have to do with the `guid`
tag. That's GUID, as in Global Unique Identifier. Not only did it not
appear until RSS 2.0, but **the guid tag is not required**. In
practice an RSS client will be rereading the same feed items over and
over, so it's critical that it's able to identify what items it's seen
before.

Without a guid tag it's up to the client to guess what items have been
seen already, and there's no guidance in the specification for doing
so. Without a guid tag, some clients use contents of the `link` tag as
an identifier (Elfeed, The Old Reader). In practice it's very unlikely
for two unique items to have the same link. Other clients track the
entire contents of the item, so when any part changes, such as the
description, it's treated as a brand new item (Liferea). Some
guid-less feeds regularly change their `description` (advertising,
etc.), so they're not handled well by the latter clients. It's a mess.

In contrast, Atom's `id` element is required. If someone doesn't have
one you can send them angry e-mails for having an invalid feed.

The bigger flaw of the guid tag is that, **by default, guid tag
content is not actually a GUID**! This was a huge oversight by the
specification's authors. By default, the content of the guid tag
*must* be a permanent URL. Only if the `isPermalink` attribute is set
to false can it actually be a GUID (but even that's unlikely). If two
different feeds contain items that link to content with the same
permalink then that "GUID" is obviously no longer unique. Two unique
items have the same "unique" ID. Doh! Even if the guid tag was
required, I still couldn't rely on it in Elfeed.

In contrast, Atom's `id` element must contain an Internationalized
Resource Identifier ([IRI][iri]). This is guaranteed to be unique.

Unlike Atom, **RSS feeds themselves also don't have identifiers**. Due
to RSS guids never actually being GUIDs, in order to uniquely identify
feed entries in Elfeed I have to use a tuple of the feed URL and
whatever identifier I can gather from the entry itself. It's a lot
messier than it should be.

In a purely Atom world, the GUID alone would be enough to identify an
entry and the feed URL wouldn't matter for identification: I wouldn't
care where the feed came from, just what it's called. If the same feed
was hosted at two different URLs, a user could list both, the second
appearance acting as a backup mirror, and Elfeed would merge them
effortlessly.

#### pubDate, the incorrectly specified

RSS **didn't have any sort of date tag until version 2.0!** A standard
specifically oriented around syndication sure took a long time to have
date information. Before 2.0 the workaround was to pull in a date tag
from another XML namespace, such as Dublin Core.

In contrast, Atom has always had `published` and `updated` tags for
communicating date information.

Finally, in RSS 2.0, dates arrived in the form of the `pubDate` tag.
For some reason the name "date" wasn't good enough so they went with
this ugly camel-case name. Despite all the extra time, they *still*
screwed this part up. The specification says that **dates must conform
to the outdated [RFC 822][rfc822], then provides examples that
*aren't* RFC 822 dates**! Doh! This is because RFC 822 only allows for
2-digit years, so no one should be using it anymore. The RSS authors
unwittingly created yet another date specification — a mash-up
between these two RFCs. In practice everyone just pretends RSS uses
[RFC 2822][rfc2822], which superseded RFC 822.

In contrast, Atom consistently uses [RFC 3339][rfc3339] dates, along
with a couple of additional restrictions. These dates are *much*
simpler to parse than RFC 2822, which is complex because it attempts
to be backwards compatible with RFC 822.

#### RSS 1.0, the problem child

RSS changed *a lot* between versions. There was the 0.9x series,
several of which were withdrawn. Later on there was version 1.0 (2000)
and 2.0 (2002). The big problem here is that **[RSS 1.0][rss1] has
very little in common with 0.9x and 2.0**. It's practically a whole
different format. In order to officially support RSS, a client has to
be able to parse all of these different formats. In fact, in Elfeed I
have an entirely separate parser for RSS 1.0.

What's so weird about RSS 1.0? If you thought the name "pubDate" was
ugly you might want to skip this part. In practice it's namespace
hell. For example, look at [this Gmane RSS 1.0 feed][hell]. Unlike the
other RSS versions, the top level element is `rdf:RDF`. That's not a
typo.

~~~xml
<?xml version="1.0" encoding="UTF-8"?>
<rdf:RDF xmlns="http://purl.org/rss/1.0/"
         xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns">
  <channel>
    <title>RSS 1.0 Example</title>
    <items>
      <rdf:Seq>
        <rdf:li rdf:resource="http://example.com/foo"/>
      </rdf:Seq>
    </items>
  </channel>
  <item>
    <title>Example Item</title>
    <description>A summary.</description>
    <link>http://www.example.com/foo</link>
  </item>
</rdf:RDF>
~~~

Remember, if you want dates you'll need to import another namespace.

Notice the completely redundant `items` tag. It's not like you're
going to download a partial feed and use the `items` tag to avoid
grabbing full content. It's just noise.

Even more important: notice that the **items are *outside* the
`channel` tag**! Why would they completely restructure everything in
1.0? It's madness. Fortunately everything here was dumped in RSS 2.0
and, except for a very small number of feeds, it's almost just a bad
memory.

#### channel, the vestigial tag

Notice in the example RSS feed it goes `rss` -> `channel` -> `item*`.
Having a `channel` tag suggests a single feed can have a number of
different channels. Nope! Only one channel is allowed, meaning **the
channel tag serves absolutely no purpose**. It's just more noise. Why
was this ever added?

The good news is that RSS has a `category` tag which serves this
purpose much better anyway. Tagging is preferable to hierarchies —
e.g. an item could only belong to one channel but it could belong to
multiple categories.

### Atom

Atom is a much cleaner specification, with much clearer intent, and
without all the mistakes and ambiguities. It's also more general,
designed for the syndication of many types and shapes of content. This
is what made it popular for use with podcasts. Everything I listed
above I discovered myself while writing Elfeed. There are surely many
other problems with RSS I haven't noticed yet.

If I only had to support Atom, things would have been significantly
simpler. At the moment I have no complaints about Atom. It's given me
no trouble.

Someday if you're going to create a new feed for some content, please
do the web a favor and choose Atom! You're much more likely to get
things right the first time and you'll make someone else's job a lot
easier. As the author of a web feed client you can take my word for
it.


[atom]: http://www.ietf.org/rfc/rfc4287.txt
[rss2]: http://www.rssboard.org/rss-specification
[iri]: http://www.ietf.org/rfc/rfc3987.txt
[rss1]: http://web.resource.org/rss/1.0/spec
[rfc822]: http://www.ietf.org/rfc/rfc0822.txt
[rfc2822]: http://www.ietf.org/rfc/rfc2822.txt
[rfc3339]: http://www.ietf.org/rfc/rfc3339.txt
[hell]: http://rss.gmane.org/messages/excerpts/gmane.linux.kernel
