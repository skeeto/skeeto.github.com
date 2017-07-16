---
title: Switching to the Emacs Lisp Package Archive
layout: post
tags: [emacs, elisp]
uuid: 3e3186c8-dccd-3167-1f42-79f34d08a3dd
---

*Update June 2017*: I no longer use Emacs' `package.el` and instead
manage packages and their dependencies (manually) through my own
decentralized package system called `gpkg` ("git package").

For those who are unaware, Emacs 24 was finally released this past
June. I had been following the official repository for about a year
before the release using what was becoming version 24, very quickly
becoming dependent on several of [the new features][news]. Now that
it's been officially released I'm back to using a stable version of
Emacs, about which I'm quite relieved.

One of the new features that I *hadn't* been using until recently was
the package manager, `package`, and the
[Emacs Lisp Package Archive](http://elpa.gnu.org/) (ELPA). You can now
ask Emacs to download and install new modes and extensions from the
Internet. By default, it only uses the official archive. It only hosts
packages with copyright assigned to the FSF — quite
restrictive. There are alternatives, the most popular of which is
[Marmalade](http://marmalade-repo.org/). Fortunately it's easy to ask
`package` to use additional repositories, so this is a non-issue.

Because it was still unstable and buggy at the time, I avoided using
it when [setting up my configuration repository](/blog/2011/10/19/).
Instead I opted to gather packages by way of Git submodules. I'd give
`package` a shot once Emacs 24 was released. Once it was released in
June it was just a matter of time until I invested into this new
system.

The trigger was an e-mail from one of my readers, Rolando. He asked me
if I could move my [recently updated](/blog/2012/08/02/) memoization
function into its own repository and touch it up so that it could be
turned into a package with [MELPA](http://melpa.milkbox.net/), another
alternative package repository. This forced me to finally investigate.

It turns out MELPA is *really* interesting. Each package is described
by a "recipe" file, which is essentially just a tiny s-expression
listing the repository URL. In the case of my memoization package,

~~~cl
(memoize :repo "skeeto/emacs-memoize"
         :fetcher github)
~~~

From a package maintainer's point-of-view, this is fantastic. I don't
have to take any extra steps to publish updates to my package. I just
keep doing what I do and it happens automatically. However, I need to
be more careful about not pushing broken commits — which is why I
started unit testing (to be
[covered in a future post](/blog/2012/08/15/)). And I need to be extra
careful with my SSH keys, since they're now used to publish code that
other people automatically trust and execute.

Excited about MELPA and wanting to actually use my own package, I
started throwing out my submodules, replacing them with their package
equivalents. If you follow my configuration repository you probably
noticed all the recent disruption, because updating requires manual
intervention. Git leaves submodules around (for good reason!) so they
need to be manually removed.

I also heavily updated and renamed [my web server](/blog/2009/05/17/)
(now called `simple-httpd`) to provide it as a package (also to be
covered in a future post). Thanks to MELPA, I follow the package
rather than my own repository since it follows so closely (< 1 hour).

Another barrier was that I was using an old version of [Magit][magit]
due to a bad interaction of modern versions with Wombat, my preferred
color theme. After [some face tweaking][tweak], I not only fixed it
but I made it better than it was before. Sinking a an hour or two into
these sorts of annoyances usually works out really well. I need to
remind myself of this in the future when I run into annoyance issues.

Surprisingly, `package` doesn't seem to be written with managed
configuration in mind. The provided functionally is designed to be
used interactively rather than programmatically. `package-install` is
only meant to be invoked once, so care needs to be taken in listing
packages in a configuration and doing everything in the right
order. Here's how I have it set up at the moment, after after listing
the packages to use in `my-packages`,

~~~cl
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
~~~

Upgrading/updating is currently a manual process. Run
`package-refresh-contents`, list the packages with `list-packages`,
type `U` to mark updates, then `x` to e`x`ecute the upgrade. Sometime
I may work that into my configuration to be done automatically
once-per-week or something.

I really look forward to making more use of the package manager,
especially as packages can more easily become interdependent, reducing
duplication of effort.


[news]: http://www.gnu.org/software/emacs/NEWS.24.1
[magit]: https://github.com/magit/magit
[tweak]: https://github.com/skeeto/.emacs.d/commit/aec488937ff9a344278359ded7732446f2380748
