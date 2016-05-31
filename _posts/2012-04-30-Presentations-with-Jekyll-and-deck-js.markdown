---
title: Presentations with Jekyll and deck.js
layout: post
tags: [emacs, git]
uuid: 23949cf7-f8c2-332e-6889-5d4c9d128cf7
---

At work, this has been The Year of Presentations for me so far. I've
prepared and performed three hour-long presentations so far this year,
and I will continue to do more. The presentations I've done before
haven't been too serious; I'd just slap a few slides together in
whatever was handy and talk in front of them. However, with these more
serious presentations, I was making much more use of the associated
software. I haven't been happy with any of them. They violate my
[preference for precision](/blog/2012/04/29/), after all.

The first one I went with KPresenter, part of KOffice. It had been
years since I last used KOffice, so I thought I'd give it a shot. One
the good side, I liked the templates. However, it crashed on me a lot,
which was very frustrating. The GUI is lacking in a lot of places. For
example, I wanted to re-arrange my slides, and dragging and dropping
them feels like the natural choice. The mouse cursor even suggests it
by switching to a hand icon. Nope, dragging and dropping does
nothing. Overall, it felt like using a crummy version of Inkscape. The
presentation was a mess when viewed by other presentation software, so
I had to export it to a PDF to use it.

For the second one, I used LibreOffice's Impress. It's better than
KPresenter, but it still feels clunky. It took some wrestling to get
it to do what I wanted. As to be expected, I still had the same
feeling of uneasiness I have about any WYSIWYG tool.

For the third one I used PowerPoint, as provided by my employer. The
main reason for this was that I was <s>stealing</s> borrowing some
important slides from a couple of other people's presentations, so I
had little choice. It was also an opportunity to compare it to the
others. Overall I'd say it's on the same level as Impress, with some
slightly nicer GUI behavior.

Fortunately, I recently discovered what may become my preferred
presentation tool! It's [deck.js](http://imakewebthings.com/deck.js/).

With deck.js, I'll be writing my presentations in HTML 5, something
with which I'm already comfortable and experienced. Most importantly,
I'll be able to create my presentations with Emacs and version them
with Git. That allows for easy collaboration on presentations
without all the stupid e-mailing documents back and forth — though
the other person would need to be comfortable with using deck.js,
too. That leaves ... well, just [Brian](http://50ply.com/) I
guess. So, *in theory*, this could make collaboration easier.

The downside to deck.js is that it requires a lot of boilerplate,
especially if you want to use the extensions, a couple of which are
absolutely *essential* in my opinion. Creating a new presentation
requires going through this setup phase, and then working around all
the boilerplate the rest of the time. I've successfully used Git to
[work around this problem with Java](/blog/2010/10/04/), so I've done
the same here, with a little bit of help from
[Jekyll](https://github.com/mojombo/jekyll).

What I've done is used Jekyll as a default layout for deck.js. It
hides away all of the deck.js boilerplate so that I can focus on my
presentation. It also makes it trivial to start a new
presentation. All I have to do is clone this repository and I'm ready
to go.

<pre>
git clone --recursive <a href="https://github.com/skeeto/jekyll-deck">https://github.com/skeeto/jekyll-deck.git</a> <i>my-pres</i>
</pre>

The result looks like this: [A Jekyll / deck.js Presentation](/jekyll-deck/).

Jekyll *almost* opens up the opportunity to really take deck.js to the
next level: presentations written in Markdown! That would be
wonderful. Unfortunately, the HTML output is a little bit too
demanding for Jekyll (i.e. Maruku) to manage. It's not quite
extensible enough to pull it off. So it's just HTML5 for now, which is
unfortunately bulky when it comes to lists — a common element of
presentations. Oh well. I do still get syntax highlighting with
Pygments!

I haven't used it for anything serious yet, so it's still untried. In
my experimentation I found it enjoyable to work with, so I really look
forward to making use of it in the future. Feel free to use it
yourself, of course, and tell me how it goes.
