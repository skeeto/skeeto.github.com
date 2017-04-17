---
title: My Journey with Touch Typing and Vim
layout: post
date: 2017-04-01T04:02:08Z
tags: [vim, emacs, meatspace]
uuid: 985ef250-4b1f-3ec0-76a4-79406f3e993e
---

*Given the title, the publication date of this article is probably
really confusing. This was deliberate.*

Three weeks ago I made a conscious decision to improve my typing
habits. You see, I had [a dirty habit][tt]. Despite spending literally
decades typing on a daily basis, I've been a weak typist. It wasn't
exactly finger pecking, nor did it require looking down at the
keyboard as I typed, but rather a six-finger dance I developed
organically over the years. My technique was optimized towards Emacs'
frequent use of CTRL and ALT combinations, avoiding most of the hand
scrunching. It was fast enough to keep up with my thinking most of the
time, but was ultimately limiting due to its poor accuracy. I was
hitting the wrong keys far too often.

My prime motivation was to learn Vim — or, more specifically, to learn
modal editing. Lots of people swear by it, including people whose
opinions I hold in high regard. The modal editing community is without
a doubt larger than the Emacs community, especially since, thanks to
Viper and [Evil][evil], a subset of the Emacs community is also part
of the modal editing community. There's obviously *something*
significantly valuable about it, and I wanted to understand what that
was.

But I was a lousy typist who couldn't hit the right keys often enough to
make effective use of modal editing. I would need to learn touch typing
first.

### Touch typing

How would I learn? Well, the first search result for "online touch
typing course" was [Typing Club][tc], so that's what I went with. By
the way, here's my official review: "Good enough not to bother
checking out the competition." For a website it's pretty much the
ultimate compliment, but it's not exactly the sort of thing you'd want
to hear from your long-term partner.

My hard rule was that I would immediately abandon my old habits cold
turkey. Poor typing is a bad habit just like smoking, minus the cancer
and weakened sense of smell. It was vital that I unlearn all that old
muscle memory. That included not just my six-finger dance, but also my
[NetHack][nh] muscle memory. NetHack uses "hjkl" for navigation just
like Vim. The problem was that I'd spent a couple hundred hours in
NetHack over the past decade with my index finger on "h", not the
proper home row location. It was disorienting to navigate around Vim
initally, like [riding a bicycle with inverted controls][bike].

Based on reading other people's accounts, I determined I'd need
several days of introductory practice where I'd be utterly
unproductive. I took a three-day weekend, starting my touch typing
lessons on a Thursday evening. Boy, they weren't kidding about it
being slow going. It was a rough weekend. When checking in on my
practice, my wife literally said she pitied me. Ouch.

By Monday I was at a level resembling a very slow touch typist. For
the rest of the first week I followed all the lessons up through the
number keys, never progressing past an exercise until I had exceeded
the target speed with at least 90% accuracy. This was now enough to
get me back on my feet for programming at a glacial, frustrating pace.
Programming involves a lot more numbers and symbols than other kinds
of typing, making that top row so important. For a programmer, it
would probably be better for these lessons to be earlier in the
series.

### Modal editing

For that first week I mostly used Emacs while I was finding my feet
(or finding my fingers?). That's when I experienced first hand what
all these non-Emacs people — people who I, until recently, considered
to be unenlightened simpletons — had been complaining about all these
years: **Pressing CTRL and ALT key combinations from the home row is a
real pain in in the ass!** These complaints were suddenly making
sense. I was already seeing the value of modal editing before I even
started really learning Vim. It made me look forward to it even more.

During the second week of touch typing I went though [Derek Wyatt's
Vim videos][video] and learned my way around the :help system enough
to bootstrap my Vim education. I then read through the user manual,
practicing along the way. I'll definitely have to pass through it a
few more times to pick up all sorts of things that didn't stick. This
is one way that Emacs and Vim are a lot alike.

Update: [*Practical Vim: Edit Text at the Speed of Thought*][pv] was
recommended in the comments, and it's certainly a better place to
start than the Vim user manual. Unlike the manual, it's opinionated
and focuses on good habits, which is exactly what a newbie needs.

One of my rules when learning Vim was to resist the urge to remap
keys. I've done it a lot with Emacs: "Hmm, that's not very convenient.
I'll change it." It means [my Emacs configuration][conf] is fairly
non-standard, and using Emacs without my configuration is like using
an unfamiliar editor. This is both good and bad. The good is that I've
truly changed Emacs to be *my* editor, suited just for me. The bad is
that I'm extremely dependent on my configuration. What if there was a
text editing emergency?

With Vim as a sort of secondary editor, I want to be able to fire it
up unconfigured and continue to be nearly as productive. A pile of
remappings would prohibit this. In my mind this is like a form of
emergency preparedness. Other people stock up food and supplies. I'm
preparing myself to sit at a strange machine without any of my
configuration so that I can start the rewrite of the [software lost in
the disaster][needle], so long as that machine has [vi, cc, and
make][build]. If I can't code in C, then what's the point in surviving
anyway?

The other reason is that I'm just learning. A different mapping might
*seem* more appropriate, but what do I know at this point? It's better
to follow the beaten path at first, lest I form a bunch of bad habits
again. Trust in the knowledge of the ancients.

### Future directions

**I am absolutely sticking with modal editing for the long term.** I'm
*really* enjoying it so far. At three weeks of touch typing and two
weeks of modal editing, I'm around 80% caught back up with my old
productivity speed, but this time I've got a lot more potential for
improvement.

For now, Vim will continue taking over more and more of my text
editing work. My last three articles were written in Vim. It's really
important to keep building proficiency. I still [rely on Emacs for
email][email] and for [syndication feeds][elfeed], and that's not
changing any time soon. I also [really like Magit][magit] as a Git
interface. Plus I don't want to [abandon years of accumulated
knowledge][tag] and leave the users of my various Emacs packages out
to dry. Ultimately I believe will end up using Evil, to get what seems
to be the best of both worlds: modal editing and Emacs' rich
extensibility.


[tt]: http://steve-yegge.blogspot.com/2008/09/programmings-dirtiest-little-secret.html
[evil]: https://github.com/emacs-evil/evil
[tc]: https://www.typingclub.com/
[bike]: https://www.youtube.com/watch?v=MFzDaBzBlL0
[nh]: http://www.nethack.org/
[video]: http://derekwyatt.org/vim/tutorials/
[conf]: https://github.com/skeeto/.emacs.d
[build]: /blog/2017/03/30/
[needle]: /blog/2016/11/17/
[email]: /blog/2013/09/03/
[elfeed]: https://github.com/skeeto/elfeed
[tag]: /tags/emacs/
[magit]: https://github.com/magit/magit
[pv]: https://pragprog.com/book/dnvim2/practical-vim-second-edition
