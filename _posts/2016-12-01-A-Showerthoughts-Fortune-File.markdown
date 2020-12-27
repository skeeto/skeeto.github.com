---
title: A Showerthoughts Fortune File
layout: post
date: 2016-12-01T23:58:15Z
tags: [reddit, linux, emacs]
uuid: 0a266c4d-a224-3399-a851-848f71b47dc3
---

I have created a [`fortune` file][fortune] for the all-time top 10,000
[/r/Showerthoughts][sr] posts, as of October 2016. As a word of
warning: Many of these entries are adult humor and may not be
appropriate for your work computer. These fortunes would be
categorized as "offensive" (`fortune -o`).

Download: [showerthoughts][dl]{: .download} (1.3 MB)

The copyright status of this file is subject to each of its thousands
of authors. Since it's not possible to contact many of these authors —
some may not even still live — it's obviously never going to be under
an open source license (Creative Commons, etc.). Even more, some
quotes are probably from comedians and such, rather than by the
redditor who made the post. I distribute it only for fun.

### Installation

To install this into your `fortune` database, first process it with
`strfile` to create a random-access index, showerthoughts.dat, then
copy them to the directory with the rest.

    $ strfile showerthoughts
    "showerthoughts.dat" created
    There were 10000 strings
    Longest string: 343 bytes
    Shortest string: 39 bytes

    $ cp showerthoughts* /usr/share/games/fortunes/

Alternatively, `fortune` can be told to use this file directly:

    $ fortune showerthoughts
    Not once in my life have I stepped into somebody's house and
    thought, "I sure hope I get an apology for 'the mess'."
            ―AndItsDeepToo, Aug 2016

If you didn't already know, `fortune` is an old unix utility that
displays a random quotation from a quotation database — a digital
*fortune cookie*. I use it as an interactive login shell greeting on
my [ODROID-C2][c2] server:

~~~bash
if shopt -q login_shell; then
    fortune ~/.fortunes
fi
~~~

### How was it made?

Fortunately I didn't have to do something crazy like scrape reddit for
weeks on end. Instead, I downloaded [the pushshift.io submission
archives][db], which is currently around 70 GB compressed. Each file
contains one month's worth of JSON data, one object per submission,
one submission per line, all compressed with bzip2.

Unlike so many other datasets, especially when it's made up of
arbitrary inputs from millions of people, the format of the
/r/Showerthoughts posts is surprisingly very clean and requires
virtually no touching up. It's some really fantastic data.

A nice feature of bzip2 is concatenating compressed files also
concatenates the uncompressed files. Additionally, it's easy to
parallelize bzip2 compression and decompression, which gives it [an
edge over xz][xz]. I strongly recommend using [lbzip2][lbzip2] to
decompress this data, should you want to process it yourself.

~~~bash
cat RS_*.bz2 | lbunzip2 > everything.json
~~~

[jq][jq] is my favorite command line tool for processing JSON (and
[rendering fractals][mb]). To filter all the /r/Showerthoughts posts,
it's a simple `select` expression. Just mind the capitalization of the
subreddit's name. The `-c` tells `jq` to keep it one per line.

~~~bash
cat RS_*.bz2 | \
    lbunzip2 | \
    jq -c 'select(.subreddit == "Showerthoughts")' \
    > showerthoughts.json
~~~

However, you'll quickly find that jq is the bottleneck, parsing all
that JSON. Your cores won't be exploited by lbzip2 as they should. So
I throw `grep` in front to dramatically decrease the workload for
`jq`.

~~~bash
cat *.bz2 | \
    lbunzip2 | \
    grep -a Showerthoughts | \
    jq -c 'select(.subreddit == "Showerthoughts")'
    > showerthoughts.json
~~~

This will let some extra things through, but it's a superset. The `-a`
option is necessary because the data contains some null bytes. Without
it, `grep` switches into binary mode and breaks everything. This is
incredibly frustrating when you've already waited half an hour for
results.

To further reduce the workload further down the pipeline, I take
advantage of the fact that only four fields will be needed: `title`,
`score`, `author`, and `created_utc`. The rest can — and should, for
efficiency's sake — be thrown away where it's cheap to do so.

~~~bash
cat *.bz2 | \
    lbunzip2 | \
    grep -a Showerthoughts | \
    jq -c 'select(.subreddit == "Showerthoughts") |
               {title, score, author, created_utc}' \
    > showerthoughts.json
~~~

This gathers all 1,199,499 submissions into a 185 MB JSON file (as of
this writing). Most of these submissions are terrible, so the next
step is narrowing it to the small set of good submissions and putting
them into the `fortune` database format.

**It turns out reddit already has a method for finding the best
submissions: a voting system.** Just pick the highest scoring posts.
Through experimentation I arrived at 10,000 as the magic cut-off
number. After this the quality really starts to drop off. Over time
this should probably be scaled up with the total number of
submissions.

I did both steps at the same time using a bit of Emacs Lisp, which is
particularly well-suited to the task:

* <https://github.com/skeeto/showerthoughts>

This Elisp program reads one JSON object at a time and sticks each
into a AVL tree sorted by score (descending), then timestamp
(ascending), then title (ascending). The AVL tree is limited to 10,000
items, with the lowest items being dropped. This was a lot faster than
the more obvious approach: collecting everything into a big list,
sorting it, and keeping the top 10,000 items.

#### Formatting

The most complicated part is actually paragraph wrapping the
submissions. Most are too long for a single line, and letting the
terminal hard wrap them is visually unpleasing. The submissions are
encoded in UTF-8, some with characters beyond simple ASCII. Proper
wrapping requires not just Unicode awareness, but also some degree of
Unicode *rendering*. The algorithm needs to recognize grapheme
clusters and know the size of the rendered text. This is not so
trivial! Most paragraph wrapping tools and libraries get this wrong,
some counting width by bytes, others counting width by codepoints.

Emacs' `M-x fill-paragraph` knows how to do all these things — only
for a monospace font, which is all I needed — and I decided to
leverage it when generating the `fortune` file. Here's an example that
paragraph-wraps a string:

~~~cl
(defun string-fill-paragraph (s)
  (with-temp-buffer
    (insert s)
    (fill-paragraph)
    (buffer-string)))
~~~

For the file format, items are delimited by a `%` on a line by itself.
I put the wrapped content, followed by a [quotation dash][qd], the
author, and the date. A surprising number of these submissions have
date-sensitive content ("on this day X years ago"), so I found it was
important to include a date.

    April Fool's Day is the one day of the year when people critically
    evaluate news articles before accepting them as true.
            ―kellenbrent, Apr 2015
    %
    Of all the bodily functions that could be contagious, thank god
    it's the yawn.
            ―MKLV, Aug 2015
    %

There's the potential that a submission itself could end with a lone
`%` and, with a bit of bad luck, it happens to wrap that onto its own
line. Fortunately this hasn't happened yet. But, now that I've
advertised it, someone could make such a submission, popular enough
for the top 10,000, with the intent to personally trip me up in a
future update. I accept this, though it's unlikely, and it would be
fairly easy to work around if it happened.

The `strfile` program looks for the `%` delimiters and fills out a
table of file offsets. The header of the `.dat` file indicates the
number strings along with some other metadata. What follows is a table
of 32-bit file offsets.

~~~c
struct {
    uint32_t str_version;  /* version number */
    uint32_t str_numstr;   /* # of strings in the file */
    uint32_t str_longlen;  /* length of longest string */
    uint32_t str_shortlen; /* shortest string length */
    uint32_t str_flags;    /* bit field for flags */
    char str_delim;        /* delimiting character */
}
~~~

Note that the table doesn't necessarily need to list the strings in
the same order as they appear in the original file. In fact, recent
versions of `strfile` can sort the strings by sorting the table, all
without touching the original file. Though none of this important to
`fortune`.

Now that you know how it all works, you can build your own `fortune`
file from your own inputs!


[fortune]: https://en.wikipedia.org/wiki/Fortune_(Unix)
[dl]: https://skeeto.s3.amazonaws.com/share/showerthoughts
[sr]: https://old.reddit.com/r/Showerthoughts/
[db]: http://files.pushshift.io/reddit/
[c2]: http://www.hardkernel.com/main/products/prdt_info.php
[lbzip2]: http://lbzip2.org/
[xz]: /blog/2009/03/16/
[mb]: /blog/2016/09/15/
[jq]: https://stedolan.github.io/jq/
[qd]: http://www.fileformat.info/info/unicode/char/2015/index.htm
