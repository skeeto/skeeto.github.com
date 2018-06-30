---
title: 9 Elfeed Features You Might Not Know
layout: post
date: 2015-12-03T22:33:17Z
tags: [emacs, elfeed]
uuid: 26807fd8-4b69-3caa-552a-90308cc0b24f
---

It's been two years since [I last wrote about Elfeed][prev], my
[Atom/RSS feed reader for Emacs][elfeed]. I've used it every single
day since, and I continue to maintain it with help from the community.
So far 18 people besides me have contributed commits. Over the last
couple of years it's accumulated some new features, some more obvious
than others.

Every time I mark a new release, I update the ChangeLog at the top of
elfeed.el which lists what's new. Since it's easy to overlook many of
the newer useful features, I thought I'd list the more important ones
here.

#### Custom Entry Colors

You can now customize entry faces through `elfeed-search-face-alist`.
This variable maps tags to faces. An entry inherits the face of any
tag it carries. Previously "unread" was a special tag that got a bold
face, but this is now implemented as nothing more than an initial
entry in the alist.

[![](/img/elfeed/colors-thumb.png)](/img/elfeed/colors.png)

I've been using it to mark different kinds of content (videos,
podcasts, comics) with different colors.

#### Autotagging

You can specify the starting tags for entries from particular feeds
directly in the feed listing. This has been a feature for awhile now,
but it's not something you'd want to miss. It started out as a feature
in my personal configuration that eventually migrated into Elfeed
proper.

For example, your `elfeed-feeds` may initially look like this,
especially if you imported from OPML.

~~~cl
("https://nullprogram.com/feed/"
 "http://nedroid.com/feed/"
 "https://www.youtube.com/feeds/videos.xml?user=quill18")
~~~

If you wanted certain tags applied to entries from each, you would
need to putz around with `elfeed-make-tagger`. For the most common
case — apply certain tags to all entries from a URL — it's much
simpler to specify the information as part of the listing itself,

~~~cl
(("https://nullprogram.com/feed/" blog emacs)
 ("http://nedroid.com/feed/" webcomic)
 ("https://www.youtube.com/feeds/videos.xml?user=quill18" youtube))
~~~

Today I only use custom tagger functions in my own configuration to
filter within a couple of particularly noisy feeds.

#### Arbitrary Metadata

Metadata is more for Elfeed extensions (i.e. [elfeed-org][elfeed-org])
than regular users. You can attach arbitrary, [readable][read]
metadata to any Elfeed object (entry, feed). This metadata is
automatically stored in the database. It's a plist.

Metadata is accessed entirely through one setf-able function:
`elfeed-meta`. For example, you might want to track *when* you've read
something, not just that you've read it. You could use this to
selectively update certain feeds or just to evaluate your own habits.

~~~cl
(defun my-elfeed-mark-read (entry)
  (elfeed-untag entry 'unread)
  (let ((date (format-time-string "%FT%T%z")))
    (setf (elfeed-meta entry :read-date) date)))
~~~

Two things motivated this feature. First, without a plist, if I added
more properties in the future, I would need to change the database
format to support them. I modified the database format to add
metadata, requiring an upgrade function to quietly upgrade older
databases as they were loaded. I'd really like to avoid this in the
future.

Second, I wanted to make it easy for extension authors to store their
own data. I still imagine an extension someday to update feeds
intelligently based on their history. For example, the database
doesn't track when the feed was last fetched, just the date of the
most recent entry (if any). A smart-update extension could use
metadata to tag feeds with this information.

Elfeed itself already uses two metadata keys: `:failures` on feeds and
`:title` on both. `:failures` counts the total number of times
fetching that feed resulted in an error. You could use this get a
listing of troublesome feeds like so,

~~~cl
(cl-loop for url in (elfeed-feed-list)
         for feed = (elfeed-db-get-feed url)
         for failures = (elfeed-meta feed :failures)
         when failures
         collect (cons url failures))
~~~

The `:title` property allows for a custom title for both feeds and
entries in the search buffer listing, assuming you're using the
default function (see below). It overrides the title provided by the
feed itself. This is different than `elfeed-entry-title` and
`elfeed-feed-title`, which is kept in sync with feed content. Metadata
is not kept in sync with the feed itself.

#### Filter Inversion

You can invert filter components by prefixing them with `!`. For
example, say you're looking at all my posts from the past 6 months:

    @6-months nullprogram.com

But say you're tired of me and decide you want to see every entry from
the past 6 months *excluding* my posts.

    @6-months !nullprogram.com

#### Filter Limiter

Normally you limit the number of results by date, but you can now
limit the result by count using `#n`. For example, to see my most
recent 12 posts regardless of date,

    nullprogram.com #12

This is used internally in the live filter to limit the number of
results to the height of the screen. If you noticed that live
filtering has been much more responsive in the last few months, this is
probably why.

#### Bookmark Support

Elfeed properly integrates with Emacs' bookmarks ([thanks to
groks][groks]). You can bookmark the current filter with `M-x
bookmark-set` (`C-x r m`). By default, Emacs will persist bookmarks
between sessions. To revisit a filter in the future, `M-x
bookmark-jump` (`C-x r b`).

Since this requires no configuration, this may serve as an easy
replacement for manually building "view" toggles — filters bound to
certain keys — which I know many users have done, including me.

#### New Header

If you've updated very recently, you probably noticed Elfeed got a
brand new header. Previously it faked a header by writing to the first
line of the buffer. This is because somehow I had no idea Emacs had
official support for buffer headers (despite notmuch using them all
this time).

The new header includes additional information, such as the current
filter, the number of unread entries, the total number of entries, and
the number of unique feeds currently in view. You'll see this as
`<unread>/<total>:<feeds>` in the middle of the header.

As of this writing, the new header has not been made part of a formal
release. So if you're only tracking stable releases, you won't see
this for awhile longer.

You can supply your own header via `elfeed-search-header-function`
([thanks to Gergely Nagy][header]).

#### Scoped Updates

As you already know, in the search buffer listing you can press `G` to
update your feeds. But did you know you it takes a prefix argument?
Run as `C-u G`, it only updates feeds with entries currently listed in
the buffer.

As of this writing, this is another feature not yet in a formal
release. I'd been wanting something like this for awhile but couldn't
think of a reasonable interface. Directly prompting the user for feeds
is neither elegant nor composable. However, groks [suggested the
prefix argument][interface], which composes perfectly with Elfeed's
existing idioms.

#### Listing Customizations

In addition to custom faces, there are a number of ways to customize
the listing.

* Choose the sort order with `elfeed-sort-order`.
* Set a custom date format with `elfeed-search-date-format`.
* Adjust field widths with `elfeed-search-*-width`.
* Or override everything with `elfeed-search-print-entry-function`.

Gergely Nagy has been throwing lots of commits at me over the last
couple of weeks to open up lots of Elfeed's behavior to customization,
so there are more to come.

### Thank You, Emacs Community

Apologies about any features I missed or anyone I forgot to mention
who's made contributions. The above comes from my ChangeLogs, the
commit log, the GitHub issue listing, and my own memory, so I'm likely
to have forgotten some things. A couple of these features I had
forgotten about myself!


[prev]: /blog/2013/11/26/
[elfeed]: https://github.com/skeeto/elfeed
[read]: /blog/2013/12/30/
[elfeed-org]: https://github.com/remyhonig/elfeed-org
[interface]: https://github.com/skeeto/elfeed/issues/109
[groks]: https://github.com/skeeto/elfeed/issues/110
[header]: https://github.com/skeeto/elfeed/issues/111
