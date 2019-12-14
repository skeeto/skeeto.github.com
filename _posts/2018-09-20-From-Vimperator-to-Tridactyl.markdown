---
title: From Vimperator to Tridactyl
layout: post
date: 2018-09-20T15:01:46Z
tags: [web, rant, debian, vim]
uuid: 85e7dab1-88f8-34d2-c4d9-7a35d5978b20
---

Earlier this month I experienced a life-changing event — or so I
thought it would be. It was fully anticipated, and I had been dreading
the day for almost a year, wondering what I was going to do. Could I
overcome these dire straits? Would I ever truly accept the loss, or
will I become a cranky old man who won't stop talking about how great
it all used to be?

So what was this [big event][ff57]? On September 5th, Mozilla
officially and fully ended support for XUL extensions ([XML User
Interface Language][xul]), a.k.a. "legacy" extensions. The last
Firefox release to support these extensions was Firefox 52 ESR, the
browser I had been using for some time. A couple days later, Firefox
60 ESR entered Debian Stretch to replace it.

The XUL extension API was never well designed. It was clunky, quirky,
and the development process for extensions was painful, [requiring
frequent restarts][yegge]. It was bad enough that I was never interested
in writing my own extensions. Poorly-written extensions unfairly gave
Firefox a bad name, causing [memory leaks][leak] and other issues, and
Firefox couldn't tame the misbehavior.

Yet this extension API was *incredibly powerful*, allowing for rather
extreme UI transformations that really did turn Firefox into a whole
new browser. For the past 15 years I wasn't using Firefox so much as a
highly customized browser *based on* Firefox. It's how Firefox has
really stood apart from everyone else, including Chrome.

The wide open XUL extension API was getting in the way of Firefox
moving forward. Continuing to support it required sacrifices that
Mozilla was less and less willing to make. To replace it, they
introduced the WebExtensions API, modeled very closely after Chrome's
extension API. These extensions are sandboxed, much less trusted, and
the ecosystem more closely resembles the "app store" model (Ugh!).
This is great for taming poorly-behaved extensions, but they are *far*
less powerful and capable.

The powerful, transformative extension I'd [been using the past
decade][vimp] was Vimperator — and occasionally with temporary stints in
its fork, Pentadactyl. It overhauled most of Firefox's interface,
turning it into a Vim-like modal interface. In normal mode I had single
keys bound to all sorts of useful functionality.

The problem is that Vimperator is an XUL extension, and it's not
possible to fully implement using the WebExtensions API. It needs
capabilities that WebExtensions will likely never provide. Losing XUL
extensions would mean being thrown back 10 years in terms my UI
experience. The possibility of having to use the web without it
sounded unpleasant.

Fortunately there was a savior on the horizon already waiting for me:
[**Tridactyl**][tridactyl]! It is essentially a from-scratch rewrite
of Vimperator using the WebExtensions API. To my complete surprise,
these folks have managed to recreate around 85% of what I had within
the WebExtensions limitations. It will never be 100%, but it's close
enough to keep me happy.

### What matters to me

There are some key things Vimperator gave me that I was afraid of
losing.

* Browser configuration from a text file.

I keep all [my personal configuration dotfiles under source
control][dotfiles]. It's a shame that Firefox, despite being so
flexible, has never supported this approach to configuration.
Fortunately Vimperator filled this gap with its `.vimperatorrc` file,
which could not only be used to configure the extension but also access
nearly everything on the `about:config` page. It's the killer feature
Firefox never had.

Since WebExtensions are sandboxed, they cannot (normally) access files.
Fortunately there's a work around: [**native messaging**][nm]. It's a
tiny, unsung backdoor that closes the loop on some vital features.
Tridactyl makes it super easy to set up (`:installnative`), and doing so
enables the `.tridactylrc` file to be loaded on startup. Due to
WebExtensions limitations it's not nearly as powerful as the old
`.vimperatorrc` but it covers most of my needs.

* Edit any text input using a real text editor.

In Vimperator, when a text input is focused I could press CTRL+i to
pop up my `$EDITOR` (Vim, Emacs, etc.) to manipulate the input much
more comfortably. This is *so*, so nice when writing long form content
on the web. The alternative is to copy-paste back and forth, which is
tedious and error prone.

Since WebExtensions are sandboxed, they cannot (normally) start
processes. Again, native messaging comes to the rescue and allows
Tridactyl to reproduce this feature perfectly.

* Mouseless browsing.

In Vimperator I could press `f` or `F` to enter a special mode that
allowed me to simulate a click to a page element, usually a hyperlink.
This could be used to navigate without touching the mouse. It's really
nice for "productive" browsing, where my fingers are already on home
row due to typing (programming or writing), and I need to switch to a
browser to look something up. I rarely touch the mouse when I'm in
productive mode.

This actually mostly works fine under WebExtensions, too. However, due
to sandboxing, WebExtensions aren't active on any of Firefox's "meta"
pages (configuration, errors, etc.), or Mozilla's domains. This means
no mouseless navigation on these pages.

The good news is that **Tridactyl has better mouseless browsing than
Vimperator**. Its "tag" overlay is alphabetic rather than numeric, so
it's easier to type. When it's available, the experience is better.

* Custom key bindings for *everything*.

In normal mode, which is the usual state Vimperator/Tridactyl is in,
I've got useful functionality bound to single keys. There's little
straining for the CTRL key. I use `d` to close a tab, `u` to undo it.
In my own configuration I use `w` and `e` to change tabs, and `x` and
`c` to move through the history. I can navigate to any "quickmark" in
three keystrokes. It's all very fast and fluid.

Since WebExtensions are sandboxed, extensions have limited ability to
capture these keystrokes. If the wrong browser UI element is focused,
they don't work. If the current page is one of those
extension-restricted pages, these keys don't work.

The worse problem of all, by *far*, is that **WebExtensions are not
active until the current page has loaded**. This is the most glaring
flaw in WebExtensions, and I'm surprised it still hasn't been addressed.
It negatively affects every single extension I use. What this means for
Tridactyl is that for a second or so after navigating a link, I can't
interact with the extension, and the inputs are completely lost. *This
is incredibly frustrating.* I have to wait on slow, remote servers to
respond before regaining control of my own browser, and I often forget
about this issue, which results in a bunch of eaten keystrokes. (Update:
Months have passed and I've never gotten used to this issue. It
irritates me a hundred times every day. This is by far Firefox's worst
design flaw.)

### Other extensions

I'm continuing to use [**uBlock Origin**][ublock]. Nothing changes. As
I've said before, an ad-blocker is by far the most important security
tool on your computer. If you practice good computer hygiene,
malicious third-party ads/scripts are the biggest threat vector for
your system. A website telling you to turn off your ad-blocker should
be regarded as suspiciously as being told to turn off your virus
scanner (for all you Windows users who are still using one).

The opposite of mouseless browsing is keyboardless browsing. When I'm
*not* being productive, I'm often not touching the keyboard, and
navigating with just the mouse is most comfortable. However, clicking
little buttons is not. So instead of clicking the backward and forward
buttons, I prefer to swipe the mouse, e.g. make a gesture.

I previously used FireGestures, an XUL extension. ~~I'm now using
[**Gesturefy**][gesturefy]~~. (Update: Gesturefy doesn't support ESR
either.) I also considered [Foxy Gestures][fg], but it doesn't currently
support ESR releases. Unfortunately all mouse gesture WebExtensions
suffer from the page load problem: any gesture given before the page
loads is lost. It's less of any annoyance than with Tridactyl, but it
still trips me up. They also don't work on extension-restricted pages.

Firefox 60 ESR is the first time I'm using a browser supported by
[**uMatrix**][umatrix] — another blessing from the author of uBlock
Origin (Raymond Hill) — so I've been trying it out. Effective use
requires some in-depth knowledge of how the web works, such as the
same-origin policy, etc. It's not something I'd recommend for most
people.

[**GreaseMonkey**][gm] was converted to the WebExtensions API awhile
back. As a result it's a bit less capable than it used to be, and I had
to adjust a couple of [my own scripts][gf] before they'd work again. I
use it as a "light extension" system.

### XUL alternatives

Many people have suggested using one of the several Firefox forks that's
maintaining XUL compatibility. I haven't taken this seriously for a
couple of reasons:

* Maintaining a feature-complete web browser like Firefox is a *very*
  serious undertaking, and I trust few organizations to do it correctly.
  Firefox and Chromium forks have [a poor security track record][exec].

Even the Debian community gave up on that idea long ago, and they've
made a special exception that allows recent versions of Firefox and
Chrome into the stable release. Web browsers are huge and complex
because web standards are huge and complex (a situation that concerns
me in the long term). The [vulnerabilities that pop up regularly are
frightening][cve].

In *Back to the Future Part II*, Biff Tannen was thinking too small.
Instead of a sports almanac, he should have brought a copy of the CVE
database.

This is why I also can't just keep using an old version of Firefox. If I
was unhappy with, say, the direction of Emacs 26, I could keep using
Emacs 25 essentially forever, frozen in time. However, Firefox is
*internet software*. [Internet software decays and must be
maintained][is].

* The community has already abandoned XUL extensions.

Most importantly, the Vimperator extension is no longer maintained.
There's no reason to stick around this ghost town.

### Special Tridactyl customizations

The syntax for `.tridactylrc` is a bit different than `.vimperatorrc`,
so I couldn't just reuse my old configuration file. Key bindings are
simple enough to translate, and quickmarks are configured almost the
same way. However, it took me some time to figure out the rest.

With Vimperator I'd been using Firefox's obscure "bookmark keywords"
feature, where a bookmark is associated with a single word. In
Vimperator I'd use this as a prefix when opening a new tab to change the
context of the location I was requesting.

For example, to visit the Firefox subreddit I'd press `o` to start
opening a new tab, then `r firefox`. I had `r` registered via
`.vimperatorrc` as the bookmark keyword for the URL template
`https://old.reddit.com/r/%s`.

WebExtensions doesn't expose bookmark keywords, and keywords are likely
to be removed in a future Firefox release. So instead someone showed me
this trick:

    set searchurls.r   https://old.reddit.com/r/%s
    set searchurls.w   https://en.wikipedia.org/w/index.php?search=%s
    set searchurls.wd  https://en.wiktionary.org/wiki/?search=%s

These lines in `.tridactylrc` recreates the old functionality. Works
like a charm!

Another initial annoyance is that WebExtensions only exposes the X
clipboard (`XA_CLIPBOARD`), not the X selection (`XA_PRIMARY`).
However, I nearly always use the X selection for copy-paste, so it was
like I didn't have any clipboard access. (Honestly, I'd prefer
`XA_CLIPBOARD` didn't exist at all.) Again, native messaging routes
around the problem nicely, and it's trivial to configure:

    set yankto both
    set putfrom selection

There's an experimental feature, `guiset` to remove most of Firefox's
UI elements, so that it even looks nearly like the old Vimperator. As
of this writing, this feature works poorly, so I'm not using it. It's
really not important to me anyway.

### Today's status

So I'm back to about 85% of the functionality I had before the
calamity, which is far better than I had imagined. Other than the
frequent minor annoyances, I'm pretty satisfied.

In exchange I get better mouseless browsing and much better performance.
I'm not kidding, the difference Firefox Quantum makes is night and day.
~~In my own case, Firefox 60 ESR is using *one third* of the memory of
Firefox 52 ESR~~ (Update: after more experience with it, I realize its
just as much of a memory hog as before), and I'm not experiencing the
gradual memory leak. ~~This really makes a difference on my laptop with
4GB of RAM.~~

So was it worth giving up that 15% capability for these improvements?
Perhaps it was. Now that I've finally made the leap, I'm feeling a lot
better about the whole situation.


[cve]: https://www.cvedetails.com/product/3264/Mozilla-Firefox.html?vendor_id=452
[dotfiles]: /blog/2012/06/23/
[exec]: https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=887875
[ff57]: https://utcc.utoronto.ca/~cks/space/blog/web/Firefox57ComingExplosion
[fg]: https://addons.mozilla.org/en-US/firefox/addon/foxy-gestures/
[gesturefy]: https://github.com/Robbendebiene/Gesturefy
[gf]: https://greasyfork.org/en/users/2022-skeeto
[gm]: https://github.com/greasemonkey/greasemonkey
[is]: https://utcc.utoronto.ca/~cks/space/blog/tech/InternetSoftwareDecay
[leak]: https://utcc.utoronto.ca/~cks/space/blog/web/FirefoxResignedToLeaks
[nm]: https://developer.mozilla.org/en-US/docs/Mozilla/Add-ons/WebExtensions/Native_messaging
[tridactyl]: https://github.com/tridactyl/tridactyl
[ublock]: https://github.com/gorhill/uBlock
[umatrix]: https://github.com/gorhill/uMatrix
[vimp]: /blog/2009/04/03/
[xul]: https://en.wikipedia.org/wiki/XUL
[yegge]: http://steve-yegge.blogspot.com/2007/01/pinocchio-problem.html
