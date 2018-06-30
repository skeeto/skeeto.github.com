---
title: Elfeed Tips and Tricks
layout: post
date: 2013-11-26T00:38:20Z
tags: [elfeed, elisp, emacs]
uuid: 45fbc221-dbea-302c-22c0-ec0527421ed8
---

This past weekend I had some questions from next-user-here (NUH) on my
[original Elfeed post][elfeed] about changing some of Elfeed's
behavior. NUH is an Elisp novice so accomplishing some of the
requested modifications wasn't obvious. A novice is mostly limited to
setting variables, not defining advice or using hooks. I've also been
using Elfeed daily for about three months now as my sole web feed
reader and along the way I've developed some best practices. In
addition to responding to some of NIH's questions here, I'd like to
share some tips and tricks.

### Custom Entry Launchers

Currently you can press "b" to launch one or more entries in your
browser. You can use "y" to copy an single entry to the clipboard.
What if you want to make another action.

In my configuration I have a fancy binding that sends the entry URLs
in the selected region to [youtube-dl][youtube-dl] for downloading the
videos. It's too large to share as a snippet so here's a small example
of something similar using a program called `xcowsay`.

~~~cl
(defun xcowsay (message)
  (call-process "xcowsay" nil nil nil message))

(defun elfeed-xcowsay ()
  (interactive)
  (let ((entry (elfeed-search-selected :single)))
    (xcowsay (elfeed-entry-title entry))))

(define-key elfeed-search-mode-map "x" #'elfeed-xcowsay)
~~~

Now when I hit "x" over an entry in Elfeed I'm greeted by a cow
announcing the title.

![](/img/screenshot/xcowsay-small.png)

### Entry Listing Customization

The *search* buffer you see when starting Elfeed, where entries are
listed, can be customized a few different ways. First, this buffer
*does* grow dynamically. After re-sizing the window/frame horizontally
you just have to refresh the view by pressing `g` (an Emacs
convention). How it fills out depends on the settings of these
variables,

 * `elfeed-search-title-max-width`
 * `elfeed-search-title-min-width`
 * `elfeed-search-trailing-width`

They control how wide the different columns should be as the window
size changes. An important caveat to this is that the cache stored in
`elfeed-search-cache` *must* be cleared before the changes will be
reflected in the display. This cache exists because building the
display, assembling all the special faces, is actually quite
CPU-intensive. It was an optimization I established early on.

~~~cl
(clrhash elfeed-search-cache)
~~~

If you set these variables in your start-up configuration you don't
need to worry about clearing the cache because it will already be
empty. It's only a concern when playing with the settings.

#### Date Display

Another question was about adding time to the entry listing. Elfeed
only displays the entry's date. Dates are formatted by the function
`elfeed-search-format-date`. This can be redefined to display dates
differently.

~~~cl
(defun elfeed-search-format-date (date)
  (format-time-string "%Y-%m-%d %H:%M" (seconds-to-time date)))
~~~

It's given epoch seconds as a float and it returns a string to display
as a date.

#### Faces and Colors

All of the faces used in the display are declared for customization,
so these can be changed to whatever you like.

 * `elfeed-search-date-face`
 * `elfeed-search-title-face`
 * `elfeed-search-feed-face`
 * `elfeed-search-tag-face`

Say you suffered a head injury and decided you want your Elfeed dates
to be bold, purple, and underlined,

~~~cl
(custom-set-faces
 '(elfeed-search-date-face
   ((t :foreground "#f0f"
       :weight extra-bold
       :underline t))))
~~~

### Database Manipulation

Feeds and entries in the database can be manipulated to become
whatever you want them to be. Because Elfeed is regularly modifying
the database, the trick is to perform the manipulation at *just* the
right time.

#### Feed Title Changes

Say you want to change a feed title because you don't like the title
supplied by the feed. For example, the title to my blog's feed is
"null program" but instead you think it should be "Seriously Handsome
Programmer" (head injury, remember?). The function
`elfeed-db-get-feed` can be used to fetch a feed's data structure from
the database, given it's exact URL as listed in your `elfeed-feeds`.

~~~cl
(let ((feed (elfeed-db-get-feed "https://nullprogram.com/feed/")))
  (setf (elfeed-feed-title feed) "Seriously Handsome Programmer"))
~~~

Hold it, that didn't work. First, that display cache is getting in the
way again. Feed titles change very infrequently so they're cached
aggressively. More importantly, next time you update your feeds Elfeed
will re-synchronize the feed title with the official title. It's going
to fight against your intervention.

The solution is to do it with a little bit of advice just before the
title is displayed. Advise the function `elfeed-search-update` with
some "before" advice.

~~~cl
(defadvice elfeed-search-update (before nullprogram activate)
  (let ((feed (elfeed-db-get-feed "https://nullprogram.com/feed/")))
    (setf (elfeed-feed-title feed) "Seriously Handsome Programmer")))
~~~

#### Entry Tweaking

Automatic entry modification should happen immediately upon discovery
so that it looks like the entry arrived that way. This is done through
the `elfeed-new-entry-hook`. Generally this would be used for applying
custom tags. These examples are from the documentation:

~~~cl
;; Mark all YouTube entries
(add-hook 'elfeed-new-entry-hook
          (elfeed-make-tagger :feed-url "youtube\\.com"
                              :add '(video youtube)))

;; Entries older than 2 weeks are marked as read
(add-hook 'elfeed-new-entry-hook
          (elfeed-make-tagger :before "2 weeks ago"
                              :remove 'unread))

;; Building subset feeds
(add-hook 'elfeed-new-entry-hook
          (elfeed-make-tagger :feed-url "example\\.com"
                              :entry-title '(not "something interesting")
                              :add 'junk
                              :remove 'unread))
~~~

Due to a feature I recently ported from my personal configuration,
this tagger helper function is less necessary. You can put lists in
your `elfeed-feeds` list to supply automatic tags.

~~~cl
(setq elfeed-feeds
      '(("https://nullprogram.com/feed/" blog emacs)
        "http://www.50ply.com/atom.xml"  ; no autotagging
        ("http://nedroid.com/feed/" webcomic)))
~~~

#### Content Tweaking

Going beyond tagging you could change the content of the feed. Say you
want to [make feeds 100 times better][xkcd].

~~~cl
(defun hundred-times-better (entry)
  (let* ((original (elfeed-deref (elfeed-entry-content entry)))
         (replace (replace-regexp-in-string "keyboard" "leopard" original)))
    (setf (elfeed-entry-content entry) (elfeed-ref replace))))

(add-hook 'elfeed-new-entry-hook #'hundred-times-better)
~~~

The same trick could be used to remove advertising, change the date,
change the title, etc. The `elfeed-deref` and `elfeed-ref` parts are
needed to fetch and store content in the content database. Only a
reference is stored on the structure. You can actually use these
functions at any time outside of Elfeed, but they'll eventually get
garbage collected if Elfeed doesn't know about them.

~~~cl
(setf ref (elfeed-ref "Hello, World"))
;; => [cl-struct-elfeed-ref "907d14fb3af2b0d4f18c2d46abe8aedce17367bd"]

(elfeed-deref ref)
;; => "Hello, World"
~~~

### Deletion

A question that's been asked few times is if entries can be *deleted*.
To start off, the answer to that question is "no." There is no
function provided to remove entries from the database. If you want to
remove entries you're probably taking the wrong approach.

The main problem with removal is that Elfeed needs to keep track of
what it's seen before. If an entry is removed and then rediscovered,
it will reappear as unread. There are better ways to "remove" entries,
such as tagging them specially.

On a moderately-powerful computer Elfeed can easily handle *at least*
several tens of thousands of database entries. If "too many entries"
ever becomes a performance problem I'd rather solve it by making the
database faster than by removing information from the database. It's
already very date-oriented so that older entries are infrequently
touched.

If storage is a concern, you shouldn't get too worked up about that.
As of this post I have about 6,000 entries in my database and the
index file is only 3.5 MB. The content database after garbage
collection, which is the `data/` directory under `~/.elfeed/`, with
these 6k entries is 17MB. When I run `M-x elfeed-db-compact`,
currently an experimental feature, it drops down to 1.8MB. That's less
than 1 kB per entry. It's also less than my personal Liferea database
of roughly the same amount of content (~15MB) before I wrote Elfeed.

If even this storage is still too much you can always blow away your
`data/` content database directory. This is safe to do even while
Emacs is running. You'll still see all of the entries listed in the
search buffer but won't be able to read them within Emacs until after
the next database update (when it re-fetches the most recent entry
content).

You can also clear out the content database from within Elisp by
visiting every entry and clearing its content field.

~~~cl
(with-elfeed-db-visit (entry _)
  (setf (elfeed-entry-content entry) nil))

(elfeed-db-gc)  ;; garbage collect everything
~~~

The same sort of expression can be used to run over all known entries
to perform other changes. If there was a delete function you might use
it here to remove entries older than a certain date, then hope they're
not rediscovered.

If you *never* want to store entry content (you never read entries
within Emacs), you can use a hook to always drop it on the floor as it
arrives,

~~~cl
(add-hook 'elfeed-new-entry-hook
          (lambda (entry) (setf (elfeed-entry-content entry) nil)))
~~~

### Questions?

If you have any questions or suggestions about how to make Elfeed do
what you want it to do, feel free to ask. Some things may actually
require that I make changes to Elfeed to support it, though I hope
I've anticipated your particular need well enough to avoid that.


[elfeed]: /blog/2013/09/04/
[xkcd]: http://xkcd.com/1031/
[youtube-dl]: http://rg3.github.io/youtube-dl/
