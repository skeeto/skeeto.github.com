---
title: Git is Better
layout: post
tags: [git]
uuid: 4b8bdf7b-b583-316a-820c-5bec90c29e6e
---

<!-- 12 February 2009 -->

I finally finished dumping the rest of my lingering Subversion
repositories. I have converted them all to [Git](http://git-scm.com/) repositories. If you manage a
Subversion (or CVS, or Perforce, etc) repository, you should consider
doing the same. Git became my version control system (VCS) of choice
in June and I haven't looked back since.

Why? Because Git is better.

Yes, it really is. *Much* better.

Git is faster, smaller, more secure, and more powerful. This is a
virtue of decentralized version control systems. [ Subversion is Blub](http://www.paulgraham.com/avg.html).

It all starts with to [ Source
Code Control System](http://en.wikipedia.org/wiki/Source_Code_Control_System) (SCCS) and [ Revision
Control System](http://en.wikipedia.org/wiki/Revision_Control_System) (RCS). These systems could only track single files
and created headaches for projects with multiple files being worked on
by multiple people.

Then came [ Concurrent Versions System](http://en.wikipedia.org/wiki/Concurrent_Versions_System) (CVS), which improved things slightly,
but still sucked. It still *really* only tracks individual
files.

Now, CVS *did* anonymous reads, allowing anyone to access the
repository and see code history. [ OpenBSD](http://www.openbsd.org/) was the first code base to take advantage of this. These
days, coming across projects that don't give public read access to
their repository seems backwards.

Not using any of these systems would probably be better than using
them. Their flaws are obvious as soon as you start using them.

Finally in the year
2000, [ Subversion](http://en.wikipedia.org/wiki/Subversion_(software)) arrives. It's a huge step up from CVS, fixing many of
its problems. It has a much better interface and uses atomic commits —
finally tracking more than one file at a time. We still need to talk
to some server every time we want to do something. Branching and
merging also sucks so much no one wants to use it. But branching is
overrated, right? Wrong. I use branches all the time, now that they
are easy.

The reason branching sucks in Subversion can be explained with a
famous quote by Albert Einstein,

> Make everything as simple as possible, but not simpler.

Instead of implementing tagging, branching, and merging, the
Subversion guys just implemented "cheap copy". It's a pretty clever
idea, but in practice it doesn't work out well. It's *too
simple*.

CVS solves the wrong problem, and Subversion solves the right problem
wrongly.

Since Subversion, a number of decentralized VCSs have arisen. We have
[ GNU arch](http://www.gnu.org/software/gnu-arch/) (2001),
[ monotone](http://monotone.ca/) (2002),
[ darcs](http://darcs.net/) (2003),
[ Bazaar](http://bazaar-vcs.org/) (2005),
Git (2005),
[ Mercurial](http://www.selenic.com/mercurial/) (2005), and
[ fossil](http://www.fossil-scm.org/) (2007).
I played around with all of these when looking for a distributed VCS
(except fossil) and none struck me the same way that Git did. I would
recommend most of them over Subversion.

Distributed VCS has gotten a lot of attention in the past couple
years, which had much to do with the Linux kernel switching to one
(Git). In fact, Git and Mercurial were written precisely for this
event. Since then, some major projects have been switching to Git, or
at least to some sort of distributed VCS: Perl, Ruby on Rails,
Android, WINE, Fedora, X.org, and VLC to name a few.

You can also see the chatter on the Internet about Git. It's is really
popular with fresh, innovative projects, like the [ Arc programming language](http://arclanguage.org/). It's
pretty easy to accidentally run into various Git tutorials on the
web. It has a real presence.

**No Authority.** But why distributed VCS? Why are they better?

First of all, when you "checkout" a distributed VCS, it's really a
"clone" operation, which is what most of them call it. You get
everything. After that, the only reason you need to talk upstream,
which really isn't "up" anymore, is if either end has updates to the
code they wish to share. The only way one clone might more important
than another is human politics. Technically they are equals.

**Small.** But won't this be *huge*? A Subversion repository
can easily be several gigabytes. That would be a lot to transfer on
the initial clone.

Actually, distributed VCSs are extremely efficient. A Git clone will
usually be smaller than a Subversion checkout. For example, I once
cloned [Freeciv's](http://freeciv.wikia.com/) Subversion
repository using Git (converting it *to* Git). It was about 15000
revisions. The bare version of the Git repository, containing all
~15000 commits, was half the size of the Subversion repository, which
contained only a singly commit! The non-bare version was still smaller
by a few megabytes. I can't even imagine how much space the server was
using.

I would have some numbers on this example, but, alas, that clone was
lost on a failed hard drive and it took me a week to make. Note, Git
clones of Git repositories aren't that slow: Subversion isn't
optimized for cloning, and the Freeciv Subversion server is extremely
overloaded.

**Update**: I managed to get another clone, and it only took me a
couple hours. The Freeciv Subversion checkout at revision 15574 is
281MB. Remember, this contains just one single revision. The Git clone
after a repack and garbage collection, which contains all 15574
revisions, is 225MB. It's 56MB smaller! If I told it to leave out the
Subversion metadata it would be even smaller than that. On the server
side, the Subversion repository likely takes up gigabytes. And
finally, to add insult to injury, the Git "bare" clone is 144MB.

Someone *does* have an example over here: [Git's Major Features
Over Subversion](http://git.or.cz/gitwiki/GitSvnComparsion).

> The Mozilla project's CVS repository is about 3 GB; it's about 12
> GB in Subversion's fsfs format. In Git it's around 300 MB.

Git's packing format is fairly simple, yet so effective.

**Fast.** Well, duh. With everything being local, operations that
work on multiple revisions will be fast. Beyond this, decentralized VCS
is generally faster on all operations, except the initial clone.

**Reduced politics.** With a central repository, someone or some
group has to decide who has write access and who doesn't. Developers
without write access are basically stuck without version control,
unless they hack in their own. In the decentralized model, everyone
has write access to their own personal repository, and others can
choose, on their own, to pull revisions from it.

**Secure.** A centralized VCS has a central, single point of
failure. If that single point is compromised, the server needs to be
restored from backups. Or worse, the compromise goes unnoticed and the
repository history is modified without anyone ever being able to tell.

In a distributed model, each revision (and in Git's case, the files
themselves) is referenced by a hash (SHA-1 in Git's case) of it's
contents, a [ content-addressable storage system](http://en.wikipedia.org/wiki/Content-addressable_storage). Thanks to this, a file, no
matter where it is in the tree or in history, is stored only
*once*. The main purpose is to avoid collisions between revision
identifiers on parallel lines of development. It also happens to make
the repository tamper-proof.

If you know the revision ID of your HEAD no one will be able to
change any of its history. This is because each revision contains the
ID of its immediate ancestor, all the way back to the initial
commit. If a previous commit changed, it would change the ID of every
following commit. An attacker would have to find a desired collision
for each one: simply impossible.

The hash addressing also provides integrity, as corruption in the
repository is easily detected.

Another security gain, related to the reduced politics note, is the [ web of
trust](http://en.wikipedia.org/wiki/Web_of_trust). This is the same way PGP handles key authentication. In a
large project, a single developer may only trust a handful of people
to be competent programmers, and therefore only pull from these
developer's repositories. Those developers they pull from also have
their own set of people *they* trust. In this way, revisions can
safely be pulled from distant strange repositories through the web of
trust.

The only reason to interact with a Subversion repository is for legacy
reasons. Luckily, you don't have to use Subversion to use Subversion.

That wasn't a typo. Git has a Subversion/Git interfaced called
git-svn. I used it to convert my Subversion repositories to Git, but
it can be used as a fully functional Subversion client. It can clone
the Subversion repository and continue to pull changes from it as it
updates.

On your end, you can make commits to your local repository, use cheap
branches, and so on,all of which stay local. Changes can be pushed
back upstream to Subversion with the `dcommit` command,
which would be done after rebasing any changes on top of the current
Subversion HEAD. This provides most of the advantages of Git without
worrying about having the central repository change.

One of the major complaints about Git is that it once lacked a
plethora of GUIs, like CVS and Subversion have. Git *does* have
GUIs. I looked at a couple of them out of curiosity, so I am not sure
how good they are by comparison. I also have barely use any other VCS
GUIs. The ones I have used I find incredibly annoying.

I don't understand why people insist on using them anyway. It's like
using training wheels on a bike and claiming that it's better that
way. No, those training wheels just get in the way.

To be brutally honest, if you don't want to use Git because you are
afraid of the command line, what are you doing coding in the first
place?

One topic left is issue tracking. In a centralized VCS, you have a
centralized tracker. Subversion has [Trac](http://trac.edgewall.org/), for example. Well, what
about distributed VCSs? They should have distributed issue tracking
right?

I will go into this in my [next
post](/blog/2009/02/14#ditz).
