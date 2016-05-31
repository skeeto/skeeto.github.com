---
title: Clojure and Emacs for Lispers
layout: post
tags: [lisp, emacs, clojure]
uuid: 5a316444-8f60-30c0-2a94-b87eb239eb13
---

According to my e-mail archives I've been interested in
[Clojure][clojure] for about three and a half years now. During that
period I would occasionally spend an evening trying to pick it up,
only to give up after getting stuck on some installation or
configuration issue. With a little bit of pushing from [Brian][brian],
and the fact that this installation and configuration *is now
trivial*, I finally broke that losing streak last week.

### I'm Damn Picky

Personally, there's a high barrier in place to learn new programming
languages. It's entirely my own fault. I'm *really* picky about my
development environment. If I'm going to write code in a language I
need Emacs to support a comfortable workflow around it. Otherwise
progress feels agonizingly sluggish. If at all possible this means
live interaction with the runtime (Lisp, JavaScript). If not, then I
need to be able to invoke builds and run tests from within Emacs (C,
Java). Basically, **I want to leave the Emacs window as infrequently
possible**.

I also need a major mode with decent indentation support. This tends
to be the hardest part to create. Automatic indentation in Emacs is
considered a black magic. Fortunately, it's unusual to come across a
language that doesn't already have a major mode written for it. It's
only happened once for me and that's because it was a custom language
for a computer languages course. To remedy this, I ended up
[writing my own major mode][parsel], including in-Emacs evaluation.

Unsatisfied with JDEE, I did the same for Java,
[growing my own extensions][java] to support my development for the
couple of years when Java was my primary programming language. The
dread of having to switch back and forth between Emacs and my browser
kept me away from web development for years. That changed this past
October when I [wrote skewer-mode][skewer] to support interactive
JavaScript development. JavaScript is now one of my favorite
programming languages.

I've wasted enough time in my life configuring and installing
software. I hate sinking time into doing so without capturing that
work in source control, so that I never need to spend time on that
particular thing again. I don't mean the installation itself but the
configuration — the difference from the defaults. (And the better the
defaults, the smaller my configuration needs to be.) With
[my dotfiles repository][dotfiles] and Debian, I can go from a
computer with no operating system to a fully productive development
environment inside of about one hour. Almost all of that time is just
waiting on Debian to install all its packages. Any new language
development workflow needs to be compatible with this.

### Clojure Installation

Until last year sometime the standard way to interact with Clojure
from Emacs was through [swank-clojure][swank-clojure] with
SLIME. Well, installing [SLIME itself can be a pain][slime].
[Quicklisp][quicklisp] now makes this part trivial but it's specific
to Common Lisp. This is also a conflict with Common Lisp, so I'd
basically need to choose one language or the other.

SLIME doesn't have any official stable releases. On top of this, the
SWANK protocol is undocumented and subject to change at any time. As a
result, SWANK backends are generally tied to a very specific version
of SLIME and it's not unusual for something to break when upgrading
one or the other. I know because I wrote
[my own SWANK backend][brianscheme] for BrianScheme. Thanks to
Quicklisp, today this isn't an issue for Common Lisp users, but it's
not as much help for Clojure.

The good news is that **swank-clojure is now depreciated**. The
replacement is a similar, but entirely independent, library called
**nREPL**. (I'd link to it but there doesn't seem to be a website.)
Additionally, there's an excellent Emacs interface to it:
[nrepl.el][nrepl.el]. It's available on MELPA, so installation is
trivial.

There's also a clojure-mode package on MELPA, so install that, too.

That covers the Emacs side of things, so what about Clojure itself?
The Clojure community is a fast-moving target and the Debian packages
can't quite keep up. At the time of this writing they're too old to
use nREPL. The good news is that there's an alternative that's just as
good, if not better: [Leiningen][leiningen].

Leiningen is the standard Clojure build tool and dependency
manager. Here, "dependencies" includes Clojure itself. If you have
Leiningen you have Clojure. Installing Leiningen is as simple as
placing a single shell script in your `$PATH`. Since I always have
`~/bin` in my `$PATH`, all I need to do is wget/curl the script there
and `chmod +x` it. The first time it runs it pulls down all of its own
dependencies automatically. Right now the biggest downside seems to be
that it's really slow to start. I think the JVM warmup time is to
blame.

Let's review. To install a working Emacs live-interaction Clojure
development environment,

 * **Install the nrepl.el package in Emacs.** For me this happens
   automatically by the configuration in my
   [.emacs.d repository][dotemacs]. I only had to do this step once.

 * **Install the clojure-mode package.** Same deal.

 * **Install a JDK.** OpenJDK is probably in your system's package
   manager, so this is trivial.

 * **Put the `lein` shell script in the `$PATH`.** This takes about
   five seconds. If even this was too much for my precious
   sensibilities I could put this script in my dotfiles repository.

With this all in place, do `M-x nrepl-jack-in` in Emacs and any
clojure-mode buffer will be ready to evaluate code as expected. It's
wonderful.

### Further Extending Emacs

I made some tweaks to further increase my comfort. Perhaps nREPL's
biggest annoyance is not focusing the error buffer, like all the other
interactive modes. Once I'm done glancing at it I'll dismiss it with
`q`. This advice fixes that.

~~~cl
(defadvice nrepl-default-err-handler (after nrepl-focus-errors activate)
  "Focus the error buffer after errors, like Emacs normally does."
  (select-window (get-buffer-window "*nrepl-error*")))
~~~

I also like having expressions flash when I evaluate them. Both SLIME
and Skewer do this. This uses `slime-flash-region` to do so when
available.

~~~cl
(defadvice nrepl-eval-last-expression (after nrepl-flash-last activate)
  (if (fboundp 'slime-flash-region)
      (slime-flash-region (save-excursion (backward-sexp) (point)) (point))))

(defadvice nrepl-eval-expression-at-point (after nrepl-flash-at activate)
  (if (fboundp 'slime-flash-region)
      (apply #'slime-flash-region (nrepl-region-for-expression-at-point))))
~~~

For Lisp modes I use parenface to de-emphasize parenthesis. Reading
Lisp is more about indentation than parenthesis. Clojure uses square
brackets (`[]`) and curly braces (`{}`) heavily, so these now also get
special highlighting. See [my .emacs.d][dotemacs] for that. Here's
what it looks like,

![](/img/screenshot/clojure-brackets.png)

### Learning Clojure

The next step is actually learning Clojure. I already know Common Lisp
very well. It has a lot in common with Clojure so I didn't want to
start from a pure introductory text. More importantly, I needed to
know upfront [which of my pre-conceptions were wrong][diff]. This was
an issue I had, and still have, with JavaScript. Nearly all the
introductory texts for JavaScript are aimed at beginner
programmers. It's a lot of text for very little new information.

More good news! There's a very thorough Clojure introductory guide
that starts at a reasonable level of knowledge.

 * [Clojure - Functional Programming for the JVM][intro]

A few hours going through that while experimenting in a `*clojure*`
scratch buffer and I was already feeling pretty comfortable. With a
few months of studying [the API][api], learning the idioms, and
practicing, I expect to be a fluent speaker.

I think it's ultimately a good thing I didn't get into Clojure a
couple of years ago. That gave me time to build up — as a sort of
rite of passage — needed knowledge and experience with Java, which
deliberately, through the interop, plays a significant role in
Clojure.


[clojure]: http://clojure.org/
[brian]: http://50ply.com/
[java]: /blog/2011/11/19/
[skewer]: /blog/2012/10/31/
[parsel]: /blog/2012/09/20/
[slime]: /blog/2010/01/15/
[swank-clojure]: https://github.com/technomancy/swank-clojure
[brianscheme]: /blog/2011/01/30/
[quicklisp]: http://www.quicklisp.org/
[nrepl.el]: https://github.com/kingtim/nrepl.el
[dotfiles]: /blog/2012/06/23/
[leiningen]: http://leiningen.org/
[dotemacs]: /blog/2011/10/19/
[intro]: http://java.ociweb.com/mark/clojure/article.html
[diff]: http://clojure.org/lisps
[api]: http://clojure.github.com/clojure/
