---
title: Shamus Young's Twenty-Sided Tale E-book
layout: post
date: 2015-09-03T19:20:09Z
tags: [media, rant]
uuid: 0d11edb9-17ba-336b-25b4-3cc479ba9f03
---

Last month I assembled and edited [Shamus Young's Twenty-Sided
Tale][orig], originally a series of 84 blog articles, into an e-book.
The book is 75,000 words — about the average length of a novel —
recording the complete story of one of Shamus' *Dungeons and Dragons*
campaigns. Since he's [shared the e-book on his blog][tenth], I'm now
free to pull back the curtain on this little project.

* Download: [twenty-sided-tale.epub][epub]
* Repository: <https://github.com/skeeto/twenty-sided-tale>

To build the book yourself, you will only need `make` and `pandoc`.

![](/img/twenty-sided-tale-cover.jpg)

### Why did I want this?

Ever since [I got a tablet][tablet] a couple years ago, I've
completely switched over to e-books. Prior to the tablet, if there was
an e-book I wanted to read, I'd have to read from a computer monitor
while sitting at a desk. Anyone who's tried it can tell you it's not a
comfortable way to read for long periods, so I only reserved the
effort for e-book-only books that were *really* worth it. However,
once comfortable with the tablet, I gave away nearly all my paper
books from my bookshelves at home. The remaining use of paper books is
because either an e-book version isn't reasonably available or the
book is very graphical, not suited to read/view on a screen (full
image astronomy books, *Calvin and Hobbes* collections).

As far as formats go, I prefer PDF and ePub, depending on the contents
of the book. Technical books fare better as PDFs due to elaborate
typesetting used for diagrams and code samples. For prose-oriented
content, particularly fiction, ePub is the better format due to its
flexibility and looseness. *Twenty-Sided Tale* falls in this latter
category. The reader gets to decide the font, size, color, contrast,
and word wrapping. I kept the ePub's CSS to a bare minimum as to not
get in the reader's way. Unfortunately I've found that most ePub
readers are awful at rendering content, so while technically you could
do the same fancy typesetting with ePub, it rarely works out well.

### The Process

To start, I spent about 8 hours with Emacs manually converting each
article into Markdown and concatenating them into a single document.
The ePub is generated from the Markdown using the [Pandoc][pandoc]
"universal document converter." The markup includes some HTML, because
Markdown alone, even Pandoc's flavor, isn't expressive enough for the
typesetting needs of this particular book. This means it can only
reasonably be transformed into HTML-based formats.

Pandoc [isn't good enough][emacs] for some kinds of publishing, but it
was sufficient here. The one feature I really wished it had was
support for tagging arbitrary document elements with CSS classes
(images, paragraphs, blockquotes, etc.), effectively extending
Markdown's syntax. Currently only headings support extra attributes.
Such a feature would have allowed me to bypass all use of HTML, and
the classes could maybe have been re-used in other output formats,
like LaTeX.

Once I got the book in a comfortable format, I spent another 1.5 weeks
combing through the book fixing up punctuation, spelling, grammar,
and, in some cases, wording. It was my first time editing a book —
fiction in particular — and in many cases I wasn't sure of the
correct way to punctuate and capitalize some particular expression. Is
"Foreman" capitalized when talking about a particular foreman? What
about "Queen?" How are quoted questions punctuated when the sentence
continues beyond the quotes? As an official source on the matter, I
consulted the *Chicago Manual of Style*. The [first edition is free
online][style]. It's from 1906, but style really hasn't changed *too*
much over the past century!

The original articles were written over a period of three years.
Understandably, Shamus forgot how some of the story's proper names
were spelled over this time period. There wasn't a wiki to check. Some
proper names had two, three, or even four different spellings.
Sometimes I picked the most common usage, sometimes the first usage,
and sometimes I had to read the article's comments written by the
game's players to see how they spelled their own proper names.

I also sunk time into a stylesheet for a straight HTML version of the
book, with the images embedded within the HTML document itself. This
will be one of the two outputs if you build the book in the
repository.

### A Process to Improve

Now I've got a tidy, standalone e-book version of one of my favorite
online stories. When I want to re-read it again in the future, it will
be as comfortable as reading any other novel.

This has been a wonderful research project into a new domain (for me):
[writing and editing][stross], style, and today's tooling for writing
and editing. As a software developer, the latter overlaps my expertise
and is particularly fascinating. A note to entrepreneurs: There's
*massive* room for improvement in this area. Compared software
development, the processes in place today for professional writing and
editing is, by my estimates, about 20 years behind. It's a place where
Microsoft Word is still the industry standard. Few authors and editors
are using source control or leveraging the powerful tools available
for creating and manipulating their writing.

Unfortunately it's not so much a technical problem as it is a
social/educational one. The tools mostly exist in one form or another,
but they're not being put to use. Even if an author or editor learns
or builds a more powerful set of tools, they must still interoperate
with people who do not. Looking at it optimistically, this is a
potential door into the industry for myself: a computer whiz editor
who doesn't require Word-formatted manuscripts; who can make the
computer reliably and quickly perform the tedious work. Or maybe that
idea only works in fiction.


[orig]: http://www.shamusyoung.com/twentysidedtale/?cat=1
[tenth]: http://www.shamusyoung.com/twentysidedtale/?p=23755
[epub]: https://nullprogram.s3.amazonaws.com/tst/twenty-sided-tale.epub
[style]: http://www.chicagomanualofstyle.org/facsimile/CMSfacsimile_all.pdf
[pandoc]: http://pandoc.org/
[tablet]: /blog/2013/04/27/
[stross]: http://www.antipope.org/charlie/blog-static/2010/04/common-misconceptions-about-pu-1.html
[emacs]: https://www.masteringemacs.org/article/how-to-write-a-book-in-emacs
