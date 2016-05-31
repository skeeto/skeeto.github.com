---
title: Emacs Configuration Repository
layout: post
tags: [emacs]
uuid: d3b9a99d-3526-3c74-7b43-643b752df6ac
---

I finally got my entire Emacs configuration into source control. My
previous solution was to copy around my `.emacs.d/` to each computer I
use. This works well enough with two computers, but beyond that it's
difficult to propagate any changes I make. Counting all my VMs, I have
around a dozen systems where I use Emacs. This is the exact problem
that source control exists to fix.

If you move your `.emacs` and `.emacs.d/` out of the way, clone my
repository right into your home directory, clone the submodules, and
then run Emacs 23 or greater, you'll see my exact Emacs setup, theme
and all.

<pre>
cd
git clone <a href="https://github.com/skeeto/.emacs.d">git://github.com/skeeto/.emacs.d.git</a>
cd .emacs.d
git submodule init
git submodule update
</pre>

Notice there's an `init.el` in there. Emacs tries to load `~/.emacs`
first, but if that doesn't exist it loads `~/.emacs.d/init.el`. That's
why you need to move your own `.emacs` out of the way to see my
stuff. I do still make use of a `.emacs` file. That's my
system-specific configuration, where, for example, I tell Emacs [where
to find Javadoc files](/blog/2010/10/14/). At the top of this file I
make sure to load my other init file.

~~~cl
;; Load standard configuration
(load-file "~/.emacs.d/init.el")
~~~

One reason I didn't use source control right away was the submodule
problem — my configuration is largely made up of *other* repositories.
Git has good support for putting foreign Git repositories within your
own repository, but a couple of repositories I was using were
Subversion and CVS. I managed to cut down to just Git repositories
<s>and one Subversion repository, for which I now maintain a Git
mirror, making these *all* Git repositories</s>. (*Update November
2011*: YASnippet has moved to Git.)

I also trimmed down a bit, cutting out some things I noticed I wasn't
using (breadcrumbs, pabbrev) or things that didn't need to be in
there, such as Slime. I now use [Quicklisp](http://www.quicklisp.org/)
to manage my Slime installation, which I connect with my configuration
in my system-specific `.emacs`. Using source control will help better
track what I'm using and not using, keeping the whole thing more
tidy. Removing an experimental addition should be a simple revert
commit.

Some of the important pieces of my configuration are a spattering of
new modes, [Magit](http://philjackson.github.com/magit/) (`M-x g`),
[yasnippet](https://github.com/capitaomorte/yasnippet) (including
several of my own snippets),
[dired+](http://www.emacswiki.org/emacs/DiredPlus),
[ParEdit](http://www.emacswiki.org/emacs/ParEdit),
[smex](https://github.com/nonsequitur/smex), my
[Java editing extensions](/blog/2010/10/15/), and
[a web server](/blog/2009/05/17/) (`M-x httpd-start`).
