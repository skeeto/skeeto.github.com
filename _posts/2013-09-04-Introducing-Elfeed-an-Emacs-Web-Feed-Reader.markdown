---
title: Introducing Elfeed, an Emacs Web Feed Reader
layout: post
date: 2013-09-04T05:33:10Z
tags: [emacs, web, elfeed]
uuid: fdfd55d2-65dd-39cc-6695-655c3ea7e8e0
---

Unsatisfied with my the results of
[recent search for a new web feed reader][reader], I created my own
from scratch, called [Elfeed][elfeed]. It's built on top of Emacs and
is available for download through [MELPA][melpa]. I intend it to be
highly extensible, a power user's web feed reader. It supports both
Atom and RSS.

 * [https://github.com/skeeto/elfeed][elfeed]

The design of Elfeed was inspired by [notmuch][notmuch], which is
[my e-mail client of choice][mail]. I've enjoyed the notmuch search
interface and the extensibility of the whole system — a side-effect
of being written in Emacs Lisp — so much that I wanted a similar
interface for my web feed reader.

### The search buffer

Unlike many other feed readers, Elfeed is oriented around *entries* —
the Atom term for articles — rather than *feeds*. It cares less about
where entries came from and more about listing relevant entries for
reading. This listing is the `*elfeed-search*` buffer. It looks like
this,

[![](/img/elfeed/search-thumb.png)](/img/elfeed/search.png)

This buffer is not necessarily about listing unread or recent entries,
it's a filtered view of all entries in the local Elfeed database.
Hence the "search" buffer. Entries are marked with various *tags*,
which play a role in view filtering — the notmuch model. By default,
all new entries are tagged `unread` (customize with
`elfeed-initial-tags`). I'll cover the filtering syntax shortly.

From the search buffer there are a number of ways to interact with
entries. You can select an single entry with the point, or multiple
entries at once with a region, and interact with them.

 * `b`: visit the selected entries in a browser
 * `y`: copy the selected entry URL to the clipboard
 * `r`: mark selected entries as read
 * `u`: mark selected entries as unread
 * `+`: add a specific tag to selected entries
 * `-`: remove a specific tag from selected entries
 * `RET`: view selected entry in a buffer

(This list can be viewed within Emacs with the standard `C-h m`.)

The last action uses the Simple HTTP Renderer (shr), now part of
Emacs, to render entry content into a buffer for viewing. It will even
fetch and display images in the buffer, assuming your Emacs has been
built for it. (Note: the GNU-provided Windows build of Emacs doesn't
ship with the necessary libraries.) It looks a lot like reading an
e-mail within Emacs,

[![](/img/elfeed/show-thumb.png)](/img/elfeed/show.png)

The standard read-only keys are in action. Space and backspace are for
page up/down. The `n` and `p` keys switch between the next and
previous entries from the search buffer. The idea is that you should
be able to hop into the first entry and work your way along reading
them within Emacs when possible.

### Configuration

Elfeed maintains a database in `~/.elfeed/` (configurable). It will
start out empty because you need to tell it what feeds you'd like to
follow. List your feeds `elfeed-feeds` variable. You would do this in
your `.emacs` or other initialization files.

~~~cl
(setq elfeed-feeds
      '("http://www.50ply.com/atom.xml"
        "http://possiblywrong.wordpress.com/feed/"
        ;; ...
        "http://www.devrand.org/feeds/posts/default"))
~~~

Once set, hitting `G` (capitalized) in the search buffer or running
`elfeed-update` will tell Elfeed to fetch each of these feeds and load
in their entries. Entries will populate the search buffer as they are
discovered (assuming they pass the current filter), where they can be
immediately acted upon. Pressing `g` (lower case) refreshes the search
buffer view without fetching any feeds.

Everything fetched will be added to the database for next time you run
Emacs. It's not required at all in order to use Elfeed, but I'll
discuss some of
[the details of the database format in another post](/blog/2013/09/09/).

### The search filter

Pressing `s` in the search buffer will allow you to edit the search
filter in action.

There are three kinds of ways to filter on entries, in order of
efficiency: by age, by tag, and by regular expression. For an entry to
be shown, it must pass each of the space-delimited components of the
filter.

Ages are described by plain language relative time, starting with `@`.
This component is ultimately parsed by Emacs' `time-duration`
function. Here are some examples.

 * `@1-year-old`
 * `@5-days-ago`
 * `@2-weeks`

Tag filters start with `+` and `-`. When `+`, entries *must* be tagged
with that tag. When `-`, entries *must not* be tagged with that tag.
Some examples,

 * `+unread`: show only unread posts.
 * `-junk +unread`: don't show unread "junk" entries.

Anything else is treated like a regular expression. However, the
regular expression is applied *only* to titles and URLs for both
entries and feeds. It's not currently possible to filter on entry
content, and I've found that I never want to do this anyway.

Putting it all together, here are some examples.

 * `linu[xs] @1-year-old`: only show entries about Linux or Linus from
   the last year.

 * `-unread +youtube`: only show previously-read entries tagged
   with `youtube`.

Note: the database is date-oriented, so age filtering is by far the
fastest. Including an age limit will greatly increase the performance
of the search buffer, so I recommend adding it to the default filter
(`elfeed-search-search-filter`).

### Tagging

Generally you don't want to spend time tagging entries. Fortunately
this step can easily be automated using `elfeed-make-tagger`. To tag
all YouTube entries with `youtube` and `video`,

~~~cl
(add-hook 'elfeed-new-entry-hook
          (elfeed-make-tagger :feed-url "youtube\\.com"
                              :add '(video youtube)))
~~~

Any functions added to `elfeed-new-entry-hook` are called with the new
entry as its argument. The `elfeed-make-tagger` function returns a
function that applies tags to entries matching specific criteria.

This tagger tags old entries as read. It's handy for initializing an
Elfeed database on a new computer, since I've likely already read most
of the entries being discovered.

~~~cl
(add-hook 'elfeed-new-entry-hook
          (elfeed-make-tagger :before "2 weeks ago"
                              :remove 'unread))
~~~

### Creating custom subfeeds

Tagging is also really handy for fixing some kinds of broken feeds or
otherwise filtering out unwanted content. I like to use a `junk` tag
to indicate uninteresting entries.

~~~cl
(add-hook 'elfeed-new-entry-hook
          (elfeed-make-tagger :feed-url "example\\.com"
                              :entry-title '(not "something interesting")
                              :add 'junk
                              :remove 'unread))
~~~

There are a few feeds I'd *like* to follow but do not because the
entries lack dates. This makes them difficult to follow without a
shared, persistent database. I've contacted the authors of these feeds
to try to get them fixed but have not gotten any responses. I haven't
quite figured out how to do it yet, but I will eventually create a
function for `elfeed-new-entry-hook` that adds reasonable dates to
these feeds.

### Custom actions

In [my own .emacs.d configuration][conf] I've added a new entry action
to Elfeed: video downloads with youtube-dl. When I hit `d` on a
YouTube entry either in the entry "show" buffer or the search buffer,
Elfeed will download that video into my local drive. I consume quite a
few YouTube videos on a regular basis (I'm a "cord-never"), so this
has already saved me a lot of time.

Adding custom actions like this to Elfeed is exactly the extensibility
I'm interested in supporting. I want this to be easy. After just a
week of usage I've already customized Elfeed a lot for myself — very
specific customizations which are not included with Elfeed.

### Web interface

Elfeed also includes a web interface! If you've loaded/installed
`elfeed-web`, start it with `elfeed-web-start` and visit this URL in
your browser (check your `httpd-port`).

 * http://localhost:8080/elfeed/

[![](/img/elfeed/web-thumb.png)](/img/elfeed/web.png)

Elfeed exposes a RESTful JSON API, consumable by any application. The
web interface builds on this using AngularJS, behaving as a
single-page application. It includes a filter search box that filters
out entries as you type. I think it's pretty slick, though still a bit
rough.

It still needs some work to truly be useful. I'm intending for this to
become the "mobile" interface to Elfeed, for remote access on a phone
or tablet. Patches welcome.

### Try it out

After Google Reader closed I tried The Old Reader for awhile. When
that collapsed under its own popularity I decided to go with a local
client reader. Canto was crushed under the weight of all my feeds, so
I ended up using Liferea for awhile. Frustrated at Liferea's lack of
extensibility and text-file configuration, I ended up writing Elfeed.

Elfeed now serving 100% of my personal web feed reader needs. I think
it's already far better than any reader I've used before. Another case
of "I should have done this years ago," though I think I lacked the
expertise to pull it off well until fairly recently.

At the moment I believe Elfeed is already the most extensible and
powerful web feed reader in the world.


[reader]: /blog/2013/06/13/
[elfeed]: https://github.com/skeeto/elfeed
[melpa]: http://melpa.milkbox.net/
[mail]: /blog/2013/09/03/
[notmuch]: http://notmuchmail.org/
[conf]: https://github.com/skeeto/.emacs.d
[rdf]: http://en.wikipedia.org/wiki/RDF_feed
