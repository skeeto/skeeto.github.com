---
title: Load Libraries in Skewer with Bower
layout: post
tags: [javascript, emacs]
uuid: 4dc317df-853c-3d75-68da-ebaa2a43f628
---

I recently added support to [Skewer][skewer] for loading libraries on
the fly using [Bower's package infrastructure][bower]. Just make sure
you're up to date, then while skewering a page run `M-x
skewer-bower-load`. It will prompt for a package and version, download
the library, then inject it into the currently skewered page.

Because the Bower infrastructure is so simple, **Bower is not actually
needed in order to use this.** Only Git is required, configured by
`skewer-bower-git-executable`, which it tries to configure itself from
[Magit][magit] if it's been loaded.

### Motivation

Skewer comes with a [userscript][gm] that adds a small toggle button
to the top-right corner every page I visit. Here's a screenshot of the
toggle on this page.

![](/img/screenshot/skewer-toggle.png)

When that little red triangle is clicked, the page is connected to
Emacs and the triangle turns green. Click it again and it disconnects,
turning red. It remembers its state between page refreshes so that I'm
not constantly having to toggle.

It's mainly for development purposes, but it's occasionally useful to
Skewer an arbitrary page on the Internet so that I can poke at it from
Emacs. One habit that I noticed comes up a lot is that I want to use
jQuery as I fiddle with the page, but jQuery isn't actually loaded for
this page. What I'll do is visit a jQuery script in Emacs and load
this buffer (`C-c C-k`). As expected, this is tedious and easily
automated.

Rather than add specific support for jQuery, I thought it would be
more useful to hook into one of the existing JavaScript package
managers. Not only would I get jQuery but I'd be able to load anything
else provided by the package manager. This means if I learn about a
cool new library, chances are I could just switch to my `*javascript*`
scratch buffer, load the library with this new Skewer feature, and
play with it. Very convenient.

### How it Works

There are [a number of package managers][managers] out there. I chose
Bower because of its emphasis on client-side JavaScript and, more so,
because its infrastructure is *so* simple that I wouldn't actually
need to use Bower itself to access it. In adding this feature to
Skewer, I wrote half a Bower client from scratch very easily.

The only part of the Bower infrastructure hosted by Bower itself is a
tiny registry that maps package names to Git repositories. This host
also accepts new mappings, unauthenticated, for registering new
packages. The entire database is served up as plain old JSON.

 * [https://bower.herokuapp.com/packages](https://bower.herokuapp.com/packages)

To find out what versions are available, clone this repository with
Git and inspect the repository tags. Tags that follow the
[Semantic Versioning][semver] scheme are versions of the package
available for use with Bower. Once a version is specified, look at
`bower.json` in the tree-ish referenced by that tag to get the rest of
the package metadata, such as dependencies, endpoint listing, and
description.

This is all very clever. The Bower registry doesn't have to host any
code, so it remains simple and small. It could probably be rewritten
from scratch in 15-30 minutes. Almost all the repositories are on
GitHub, which most package developers are already comfortable with.
Package maintainers don't need to use any tools or interact with any
new host systems. Except for adding some metadata they just keep doing
what they're doing. I think this last point is a big part of
[MELPA's][melpa] success.

### Bower's Fatal Weaknesses

Unfortunately Bower has two issues, one of which is widespread, that
seriously impacts its usefulness.

#### Dependency Specification

Even though Bower specifies [Semantic Versioning][semver] for package
versions, which *very* precisely describes version syntax and
semantics, the **dependencies field in `bower.json` is
underspecified**. There's no agreed upon method for specifying
relative dependency versions.

Say your package depends on jQuery and it relies on the newer jQuery
1.6 behavior of `attr()`. You would mark down that you depend on
jQuery 1.6.0. Say a user of your package is also using another package
that depends on jQuery, it's using the `on()` method, which requires
jQuery 1.7 or newer. It specifies jQuery 1.7.0. This is a dependency
conflict.

Of course your package works perfectly fine with 1.7.0. It works fine
at 1.6.0 and later. In other package management systems, you would
probably have marked that you depend on ">=1.6.0" rather than just
1.6.0. Unfortunately, Bower doesn't specify this as a valid dependency
version. Some package maintainers have gone ahead and specified
relative versions anyway, but inconsistently. Some use the ">=" prefix
like I did above, some prefix with "~" ("*about* this version"), which
is pretty useless.

And this leads into the other flaw.

#### Most Bower Packages are Broken

While some parts of Bower are underspecified, most packages don't
follow the simple specifications that already exist! That is to say,
**most Bower packages are broken**. This is incredibly unfortunate
because it means at least half of the packages can't be loaded by this
new Skewer feature.

How are they broken? As of this writing, there are 2,195 packages list
in Bower's registry.

 * 113 (5%) of them have unreachable or unresponsive repositories.
   About half of these are due to invalid repository URLs.

 * 1,830 (83%) have no bower.json metadata file. This means the client
   has to guess at the metadata.

 * 1,034 (47%) have unguessable endpoints. My client looks for other
   package management metadata outside of Bower's, as well as tries to
   guess base on the package name. Failing to guess causes the package
   to fail to load. These packages aren't a subset of the last set
   with missing bower.json files. Sometimes the bower.json files
   contain incorrect information, which causes my client to drop into
   guessing mode.

 * 1400 (64%) don't use Semantic Versioning: either no versioning at
   all or some other arbitrary versioning system.

 * In total, **2041 (93%) of all Bower packages have invalid or
   missing metadata** — bad registry entry, missing bower.json file,
   or lack of semantic version tags.

The good news is that most of the important libraries, like jQuery and
Underscore, work properly. I've also registered two of my JavaScript
libraries, [ResurrectJS][resurrect] and [rng-js][rng], so these can be
loaded on the fly in Skewer.


[bower]: https://github.com/bower/bower
[semver]: http://semver.org/
[skewer]: /blog/2012/10/31/
[magit]: https://github.com/magit/magit
[gm]: http://en.wikipedia.org/wiki/Greasemonkey
[managers]: http://wp.me/p2UXnc-f
[melpa]: http://melpa.milkbox.net/
[resurrect]: /blog/2013/03/28/
[rng]: /blog/2013/03/25/
