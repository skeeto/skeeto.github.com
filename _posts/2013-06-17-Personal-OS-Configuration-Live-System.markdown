---
title: Personal OS Configuration Live System
layout: post
tags: [debian, emacs]
uuid: ce0f1153-4efa-3703-2e46-4c78b52adba6
---

I don't know what to title or name this thing, so bear with me.

Two years ago I started [versioning my Emacs configuration][emacs].
One year ago I started [versioning the rest of my dotfiles][dotfiles]
the same way. This has composed beautifully with Debian, which truly
is [*the* universal operating system][debian]. To create my
comfortable development environment from scratch on a new (or used)
computer, all I need to do is
[install a bare-bones Debian system][netinst], then direct it to
automatically install a short list of my preferred software (apt-get),
and finally clone these two repositories into place. Given a decent
Internet connection, the whole process takes under an hour to go from
blank hard drive to highly-productive computer system.

In fact, this whole process is so straightforward that it can be
automated using an amazing tool called [live-build][live-build]!
Taking the next step in versioning and automation, I wrote a
live-build build that creates a live system with my personal
configuration baked in.

 * [https://github.com/skeeto/live-dev-env][live]

**A link to the latest ISO build can be found in the above link**. To
try it out, burn it to a CD, write it onto a flash drive (it's a
hybrid ISO), or just fire it up in your favorite virtual machine. You
will be booted directly into *very nearly* my exact configuration,
down to the same random wallpaper selection and PGP keys
([don't worry, they're safe!][private]). It's extremely minimal and
will look like this.

![](/img/screenshot/live-skeeto.jpg)

I don't know for sure yet if this will be useful to anyone except me.
On occasions where I need to make quick use of some arbitrary
computer, or maybe just for system rescue, having this will be
incredibly handy. [Knoppix][knoppix] is nice, but working without my
own configuration can be discouraging; it's so slow in comparison.

It's also a chance for others to glimpse at my workflow without any
commitment (i.e. potential dotfile clobbering). I like to study other
developers' workflows, stealing their ideas for my own, so I want to
make mine easy to study. I think I'm doing some innovative things with
a [hacked-together pseudo-tiling window manager][tiling], my Firefox
configuration, and my Emacs configuration. At least two of my
co-workers' Emacs configurations are forked from mine (you know who
you are!). From a selfish perspective, the more people using workflows
like mine, the better these workflows will be supported by the
community at large!

I had said that it's "very nearly" my exact configuration. At the time
of this writing, what's missing is [Quicklisp][quicklisp] and
[Leiningen][lein], since these aren't available as fully-functioning
Debian packages at the moment. I'll work them in eventually. The build
is non-incremental and takes about an hour right now, so adding these
little extras by trial-and-error will take some time.

[Pentadactyl][pentadactyl] really shines here, because it allows me to
completely configure Firefox/Iceweasel from a version-friendly text
file. Except for some user scripts (still figuring out how to install
those at build time), the browser in that image is identical to what
I'm using right now. Even though [V8 is the king of performance][v8],
Firefox still wins hands-down for power users due to its superior
configurability.

However, this Firefox configuration still has an annoyance. Several of
the browser add-ons that I pre-install always pop up their first-run
welcome messages. These messages flip a setting in Firefox's registry
so they don't run again, a setting that is lost when the system shuts
down. I can toggle these settings from the Pentadactyl configuration
file, but not early enough. By the time Pentadactyl gets to applying
these settings it's too late. The messages, including Pentadactyl's,
are already queued to be shown. I don't think it's possible to
completely fix this, even if Pentadactyl is fixed.

Anything not already installed is readily installable through apt-get.
Right now I'm considering the feasibility of some sort of lazy-install
system based on command-not-found. Debian has a package called
command-not-found which intercepts the shell's error handler when an
issued command is not found. Instead of just giving the normal
warning, it prints out what package needs to be installed in order to
provide the command. What I think would be really neat is if the
needed package is automatically installed, then the requested command
is then re-run, all without returning control to the shell in the
interim. It would be a lot like Emacs autoloads. As long as I have an
Internet connection, most of Debian's packages would be *virtually*
installed on my live system as far as I'm concerned. The initial run
of any program just takes a little longer.

I'll continue to tweak this image over time, not only as I figure out
how to make things work in Debian live-build, but also as my
preferences and workflows evolve. Adjusting my configuration to work
on a live-system has been enlightening, revealing all sorts of little
manual things that I hadn't yet automated. Perhaps someday this build
will replace traditional operating system installations for me, at
least for productive work. I could do all of my work from a portable
read-only live system with a bit of short-lived (i.e. local cache)
user-data persistence stored on a separate writable medium.


[emacs]: /blog/2011/10/19/
[dotfiles]: /blog/2012/06/23/
[live]: https://github.com/skeeto/live-dev-env
[debian]: http://www.debian.org/
[private]: /blog/2012/06/24/
[netinst]: http://www.debian.org/CD/netinst/
[live-build]: http://live.debian.net/
[pentadactyl]: http://5digits.org/pentadactyl/
[lein]: https://github.com/technomancy/leiningen
[quicklisp]: http://www.quicklisp.org/
[knoppix]: http://www.knopper.net/knoppix/index-en.html
[v8]: /blog/2013/02/25/
[tiling]: https://github.com/skeeto/dotfiles#openbox
