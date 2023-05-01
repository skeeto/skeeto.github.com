---
title: Web Tips For Webcomic Authors
layout: post
date: 2015-09-26T23:57:49Z
tags: [web]
uuid: b3a1c7ac-a2e1-3559-255c-ffae7eafc397
---

My wife and I are huge webcomic fans. The web is the medium that the
comic strip industry needed badly for decades, and, with Patreon and
such today, we're now living in a golden age of comics. As of this
writing, I currently follow ... let's see ... 39 different web comics.

~~~cl
(cl-count-if (lambda (x) (memq 'comic x)) elfeed-feeds)
;; => 39
~~~

My first exposure to comics was in my childhood when I got my hands on
Bill Watterson's *Something Under the Bed Is Drooling* (Calvin and
Hobbes). This gave me very high expectations of the Sunday comics
section of the newspaper when I'd read it at my grandmother's house.
Those hopes were shattered as I discovered just how awful nationally
syndicated comic strips are: mostly watered down, lowest common
denominator stuff like Garfield, Family Circus, Cathy, B.C., etc.

During Calvin and Hobbes's original run, Bill Watterson wrote about
his struggles with the newspapers and the Universal Press Syndicate,
one of the organizations responsible for this mess. Newspapers and the
Syndicate pushed for smaller frames and shorter comics. Authors were
required to plan around newspapers removing frames for layout
purposes. Many newspapers would drop comics that need meet stringent
content limitations — a line that even Calvin and Hobbes crossed on
occasion. Authors had little control over how their work was
published.

Those days are over. Today's authors can cheaply host their comics on
the web — *web*comics — with full control over content, layout,
and schedule. If they even try to monetize at all, it's generally
through advertising, merchandising, or reader donations. Some do it
all in their free time, while for others it's part or even full time
employment. The number of regular readers of a single webcomic can be
just a handful of people, or up to millions of people. The role of the
middleman is somewhere between diminished to non-existent. This is
great, because newspapers would *never* publish the vast majority of
the comics I read every day.

I've been fortunate to meet a couple of my favorite webcomic authors.
Here's a picture of my wife posing with Anthony Clark of [Nedroid
Picture Diary][nedroid] at the Small Press Expo.

![](/img/nedroid.jpg)

I've also met Philippa Rice of [My Cardboard Life][cardboard]. (Sorry,
no picture for this one, since taking pictures with people isn't
really my thing.)

Over the years I've seen webcomic authors blunder with the web as a
technology. In my experience it's been disproportionate, with mistakes
made more often by them than the bloggers I follow. I suspect that
this is because blogs I follow tend to be computing related and so
their authors have high proficiency in computing. The same is not
necessarily true of the webcomics I follow.

### Tips for web authors

Since I want to see this medium continue to thrive, and to do so in a
way friendly to my own preferences, I'd like to share some tips to
avoid common mistakes. Some of these apply more broadly than
webcomics.

If you're using a host designed for webcomics or similar, such as
Tumblr, a lot of this stuff will be correct by default without any
additional work on your part. However, you should still be aware of
common problems because you may unwittingly go out of your way to
break things.

#### URLs are forever

Every time you publish on the web, your content is accessible through
some specific URL: that sequence of characters that starts with
"http". **Each individual comic should be accessible through a unique,
*unchanging* URL.** That last adjective is critically important. That
URL should point to the same comic for as long as possible — ideally
until the heat death of the universe. This will be affected by
problems such as your host going down, but the impact should only be
temporary and short. A URL is a promise.

People will be using this URL to share your comics with others.
They'll make posts on other websites linking to your comic. They'll
e-mail that URLs to friends and family. Once you've published, you no
longer control how that URL is used.

On several occasions I've seen authors break all their URLs after
revamping their site. For example, the previously the URL contained
the date but the new URL is only the domain and the title. That breaks
thousands of links all over the Internet. Visitors using those old
links will be welcomed with an ugly "404 Not Found" — or worse, as
I've seen more than once, a "200 Found" blank page. These are missed
opportunities for new readers.

If you *really* must change your URLs, the next best thing is to use
an HTTP "301 Moved Permanently" and redirect to the new URL. This will
leave all those old links intact and encourage new links to use the
new address. If you don't know how this works, ask your local computer
geek about it.

You should also avoid having multiple URLs for the same content
without a redirect. Search engines will punish you for it and it's
confusing for users. Pick one URL as the canonical URL for a comic,
and if you've published any other URLs (short URLs, etc.), use the
previously mentioned "301 Moved Permanently" to redirect to the
canonical URL.

Your main page probably lists all your comics starting from the most
recent. This is a good design and doesn't violate anything I
previously said. That's not the URL for any particular comic, but to
the main page, which also serves as the list of recent comics. I
strongly recommend that the comics on the main page are also
hyperlinks to their specific URL. Users naturally expect to find the
comic's URL by clicking on the comic's image.

#### Have an Atom or RSS feed

Comics without feeds is much less of a problem than it used to
be, but it still comes up on occasion. If you need to pick
between Atom and RSS, [I recommend Atom][vs], but, honestly, it's only
important that you have a valid feed with a date. You don't even need
to put the comic in the feed itself (possibly costing you ad revenue),
just a link to the comic's URL is fine. It's main purpose is to say,
"hey, there's a new comic up!"

You may not use Atom/RSS yourself, but your readers will appreciate
it. Many of us don't use centralized services like Facebook, Twitter,
or Google+, and want to follow your work without signing up for a
third-party service. Atom/RSS is the widely-accepted decentralized
method for syndication on the web.

Web feeds are really easy; it's just an XML file on your website that
lists the most recent content. A [validator][valid] can help you
ensure you've done it correctly.

#### Pick a good, catchy title

One of the biggest barriers to sharing a comic is a lack of title. For
example, if a reader is going to post your comic on reddit, they need
to enter the comic's URL and its title. If the comic doesn't have a
title, then this person will need to make one up. There's two problems
with this:

* Coming up with a title is work. Work discourages sharing. The reason
  you publish your comic is probably because you want lots of people
  to see it. If this is true, you want sharing to be as easy as
  possible.

* You really don't want readers choosing titles for you, especially
  while they're impatiently trying to share your work. If the comic is
  shared in multiple places, it will end up with a different
  reader-made title at each.

At minimum your title should appear in the `<title>` element of the
page so that it shows up in the browser tab and browser's window
title. The title of the individual comic should come before the title
of the whole website, since that shows up better in search engines.
The title should also appear somewhere near the top of page for easy
clipboard copying, though it may be worth leaving out depending on the
style of your comic.

A page without a `<title>` element looks amateur, so don't do that!

#### Think of the future and include dates

This is one of those things that's important anywhere on the web and
is often violated by blog articles as well. Far too much content is
published without a date. Dates put your comic in context, especially
if it's about something topical. It also helps users navigate your
content though time.

Putting the date in the URL is sufficient — even preferred — if
you didn't want to display it on the page proper. Your Atom/RSS should
*always* have the comic's date. I personally benefit from a date-time
precision down to the publication hour. Some comics/articles are
always published as "midnight" even when posted in the afternoon,
which has the jarring effect of inserting it in time before a bunch of
things I've already read.

#### How do I contact you?

When I notice one of the previous problems, particularly when they
arise in comics I'm already following, I'd like to inform you of the
problem. Or perhaps I want to compliment you on a well-made comic and
you don't have a comments section. I can only do this if you include
some sort of contact information. An e-mail address, even in an
anti-spam image form, is preferable but not strictly required.

#### Take advantage of the medium and go big

Comics published in newspapers are really tiny because newspaper
editors want to cram a bunch of them onto a couple of pages. You're
not operating under these limitations, so fight the urge to copy that
familiar format. Your canvas is practically infinite, so make big,
colorful webcomics. The only limit is your readers' screen resolution.

### A final thanks

Thanks for all the work you do, webcomic authors. You regularly create
all this awesome stuff for free. If you're a webcomic author and you
need help with any of the information above, don't hesitate to contact
me. After all, I don't hesitate to bug you when something's not right!


[nedroid]: http://nedroid.com/
[cardboard]: http://mycardboardlife.com/
[vs]: /blog/2013/09/23/
[valid]: https://validator.w3.org/feed/
