---
title: Ten Years of Blogging
layout: post
date: 2017-09-01T03:47:36Z
tags: [rant, meta]
uuid: 17953fb0-0161-343b-5bc6-7a569cedb128
---

As of today, I've been blogging for 10 years. In this time I've written
302,000 words across [343 articles][index] — a rate of one article every
week and a half. These articles form a record of my professional
progress, touching on both "hard" technical skills and "soft"
communication skills. My older articles are a personal reminder of how
far I've come. They are proof that I'm not as stagnant as I sometimes
feel, and it helps me to sympathize with others who are currently in
those earlier stages of their own career.

That index where you can find these 343 articles is sorted
newest-first, because it correlates with best-first. It's a trend I
hope to continue.

### History

Before blogging, I had a simple Penn State student page showcasing a
few small — and, in retrospect, trivial — side projects ([1][png],
[2][bini], [3][mb]), none of which went anywhere. Around the beginning
of my final semester of college, I was inspired by Mark Dominus' [The
Universe of Discourse][uod] and Andy Owen's [Friendly Robot
Overlord][fro] (gone since 2011) to start my own [blosxom][blosxom]
blog. It would be an outlet to actively discuss my projects. Some time
later GitHub was founded, and I [switched to a static blog][host]
hosted by [GitHub Pages][gh], which is where it lives to this day.

It's been more challenging to manage all this content than I ever
anticipated. It's like maintaining a large piece of software, except
it's naturally more fragile. Any time I make a non-trivial change to
the CSS, I have to inspect the archives to check if I broke older
articles. If I did, sometimes it's a matter of further adjusting the
CSS. Other times I'll mass edit a couple hundred articles in order to
normalize some particular aspect, such as heading consistency or image
usage. (Running a macro over an Emacs' [Dired][dired] buffer is great
for this.)

I decided in those early days to Capitalize Every Word of the
Title, and I've stuck with this convention purely out of
consistency even though it's looked weird to me for years. I don't
want to edit the old titles, and any hard changeover date would be
even weirder (in the index listing).

With more foresight and experience, I could have laid down better
conventions for myself from the beginning. Besides the universal
impossibility of already having experience before it's needed, there's
also the issue that the internet's conventions have changed, too. This
blog is currently built with HTML5, but this wasn't always the case —
especially considering that predates HTML5. When [I switched to
HTML5][html5], I also adjusted some of my own conventions to match,
since, at the time, I was still writing articles in raw HTML.

The mobile revolution also arrived since starting this blog. Today,
about one third of visitors read the blog from a mobile device. I've
also adjusted the CSS to work well on these devices. To that third of
you: I hope you're enjoying the experience!

Just in case you haven't tried it, the blog also works really well with
terminal-based browsers, such as Lynx and ELinks. Go ahead and give it a
shot. The header that normally appears at the top of the page is
actually at the bottom of the HTML document structure. It's out of the
way for browsers that ignore CSS.

If that's not enough, last year I also spent effort making the printed
style of my articles look nice. Take a look at the printed version of
this article (i.e. print preview, print to PDF), and make sure to turn
off the little headers added by the browser. A media selector provides
a separate print stylesheet. Chrome / Chromium has consistently had
the best results, followed by Firefox. Someday I'd like for browsers
to be useful as typesetting engines for static documents — as an
alternative to LaTeX and Groff — but they've still got a ways to go
with paged media. (Trust me, I've tried.)

With more foresight, I could have done something better with my
permanent URLs. Notice how they're just dates and don't include the
title. URLs work better when they include human-meaningful context.
Ideally I should be able to look at any one of my URLs and know what
it's about. Again, this decision goes all the way back to those early
days when I first configured blosxom, not knowing any better.

[URLs are forever][url], and I don't want to break all my old links.
Consistency is better than any sort of correction. I'm also practically
limited to one article per day, though this has never been a real
problem.

### Motivation

For me, an important motivation for writing is to say something unique
about a topic. For example, I'm not interested in writing a tutorial
unless either no such tutorial already exists, or there's some vital
aspect all the existing tutorials miss. Each article should add new
information to the internet, either raw information or through
assembling existing information in a new way or with a unique
perspective.

I also almost constantly feel like I'm behind the curve, like I don't
know enough or I don't have enough skill. As many of you know, the
internet is really effective at causing these feelings. Every day lots
of talented people are sharing interesting and complicated things
across all sorts of topics. For topics that overlap my own desired
expertise, those projects have me thinking, "Man, I have *no idea* how
to even start doing that. There's so much I don't know, and so much
more I need to learn." Writing articles as I learn is a great way to
keep on top of new subjects.

This is tied to another problem: I have a tendency to assume the
things I've known for awhile are common knowledge. This shows up in a
few ways. First, if everyone knows what I know, plus they know a bunch
of things that I don't know, then I've got a lot of catching up to do.
That's another source of feeling behind the curve.

Second, when writing an article on a topic where I've got years of
experience, I leave out way too many important details, assuming the
reader already knows them. When an article I regard as valuable gets a
weak response, it's probably related to this issue.

Third, after [three years of teaching][mentor], it seems like it's
becoming more difficult to put myself in the student's shoes. I'm
increasingly further away from my own days learning those early topics,
and it's harder to remember the limitations of my knowledge at that
time. Having this blog really helps, and I've re-read some of my older
articles to recall my mindset at the time.

Another way the blog helps is that it's like having my own textbook.
When teaching a topic to someone — and not necessarily a formal mentee
— or even when just having a discussion, I will reference my articles
when appropriate. Since they're designed to say something unique, my
article may be the only place to find certain information in a
conveniently packaged form.

Finally, the last important motivating factor is that I want to
[spread my preferred memes][meme]. *Obviously* the way I do things is
the Right Way, and the people who do things differently (the Wrong
Way) are stupid and wrong. By writing about the sorts of topics and
technologies I enjoy — C, low-level design, Emacs, Vim, programming on
unix-like systems, my personal programming style — I'm encouraging
others to follow my lead. Surely I'm responsible for at least one
Emacs convert out there!

### "Formal" professional writing

Despite having three or four novels worth of (mostly) technical writing
here, my formal workplace writing leaves much to be desired. I'm no
standout in this area. In the same period of time I've written just a
handful of formal memos, each about the same length as a long blog post.

Why so few? These memos are *painful* to write. In order to be
officially recognized, the formal memo process must be imposed upon
me. What this means is that, compared to a blog post, these memos take
at least an order of magnitude longer to write.

The process involves more people, dragged out over a long period of
time. The writing is a lot less personal, which, in my opinion, makes it
drier and less enjoyable. After the initial draft is complete, I have to
switch to vastly inferior tools: emailing the same Microsoft Office
documents back and forth between various people, without any proper
source control. The official, mandated memo template was created by
someone who didn't know how to effectively operate a word processor, who
had a poor sense of taste (ALL CAPS HEADINGS), and who obviously had no
training in typesetting or style.

At the end of this long process, it's filed into a system with no
practical search capability, and where it will be quietly purged after
five years, never to be seen again. Outside of the reviewers who were
directly involved in the memo process, somewhere between zero and two
people will have actually read it. Literally.

Arguably the memo might more polished than a blog post. I'm skeptical of
this, but suppose that's true. I'd still *much* rather have written ten
less-polished blog posts than one more-polished memo. That's also ten
shots to produce, by chance, a more valuable article than the single,
precious memo.

Let's broaden the scope to academic papers. Thanks to some great
co-workers — all three of whom are smarter and handsomer than me — a
year ago I finally got a published academic paper under my belt (and
more to come): [*ROP Gadget Prevalence and Survival under
Compiler-based Binary Diversification Schemes*][paper] (and I said
*memos* were dry!). A ton of work went into this paper, and it's far
more substantial than any memo or single blog post. The process was a
lot more pleasant (LaTeX instead of Word), and the results are
definitely much more polished than a typical blog post. It reads well
and has interesting information to present.

This all sounds great until you consider the impact. According to
ACM's statistics, the paper has been accessed 130 times as of this
writing. (Yes, providing an unofficial link to the paper like I just
did above doesn't help, but I ran out of those crappy "free" links.
Sue me.) Sure, the PDF might have been passed around in untrackable
ways, but I also bet a lot of those accesses were just downloads that
were never read. So let's split the difference and **estimate around
130 people read it**.

What kind of impact does a blog post have? Talking about these numbers
feels a bit taboo, like discussing salaries, but it's important for the
point I'm about to make.

* July 2017: [Rolling Shutter Simulation in C][shutter]
    * 10,400 unique visitors
    * #22 most popular
* August 2017: [A Tutorial on Portable Makefiles][make]
    * 14,300 unique visitors
    * #16 most popular
* June 2017: [Switching to the Mutt Email Client][mutt]
    * 23,100 unique visitors
    * #5 most popular
* May 2015: [Raw Linux Threads via System Calls][thread]
    * 40,500 unique visitors
    * #1 most popular, not even counting the Japanese publication

Note that all but the last has been published for less time than our
paper. The average time on these pages is between 5 and 6 minutes, so
these are actual readers, not just visitors that take one glance and
leave. Thanks to the information age, **a technical blog article on
established blog can reach an audience 100 times larger than a journal
for a fraction of the effort and cost**. There are other benefits, too:

1. I get immediate feedback in the form of comments and email (*open
   peer review*).

2. The content is available for free (*open access*). It's trivial to
   link and share blog articles.

3. Even more, this entire blog is in the public domain. If you don't
   believe me, check out the public domain dedication in the footer of
   this page. It's been there for years, and you can [verify that
   yourself][repo]. Every single change to this blog in the past 6 years
   has been publicly documented (*transparency*).

4. When I write about a topic, I make it a goal to provide the code and
   data to try it for yourself (*open data and materials*). This code
   and data is also either in the public domain, or as close to it as
   possible.

5. Link aggregators and social media are great at finding the best stuff
   and making it more visible (*censorship resistance*). When I have a
   big hit, it's often Reddit or Hacker News driving people to my
   article. Sometimes it's other blogs.

In 2017, a blog is by far the most effective way to publish and share
written information, and [have more scientific quality than
journals][science]. More people should be publishing through blog
articles than traditional forms of publications, which are far less
effective.

Since this has proven to be such an effective medium, I'm going to
make a promise right here, right now. And as I explained above with
transparency, there are no take-backs on this one. If you're reading
this, it's on the public record forever. **I promise to deliver
another 10 years of free, interesting, useful content.** This stuff is
going to be even better, too. On this blog's 20th anniversary,
September 1st, 2027, we'll see how I did.


[index]: /index/
[uod]: https://blog.plover.com/
[fro]: http://web.archive.org/web/20110608003905/http://ultra-premium.com/b/
[blosxom]: http://blosxom.sourceforge.net/
[png]: https://github.com/skeeto/pngarch
[bini]: https://github.com/skeeto/binitools
[gh]: https://pages.github.com/
[host]: /blog/2011/08/05/
[mb]: https://github.com/skeeto/mandelbrot
[html5]: /blog/2009/06/30/
[dired]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Dired.html
[url]: https://www.w3.org/Provider/Style/URI
[mentor]: /blog/2016/09/02/
[meme]: http://slatestarcodex.com/2016/07/25/how-the-west-was-won/
[paper]: https://skeeto.s3.amazonaws.com/share/p15-coffman.pdf
[science]: http://daniellakens.blogspot.com/2017/04/five-reasons-blog-posts-are-of-higher.html
[shutter]: /blog/2017/07/02/
[make]: /blog/2017/08/20/
[mutt]: /blog/2017/06/15/
[thread]: /blog/2015/05/15/
[repo]: https://github.com/skeeto/skeeto.github.com
