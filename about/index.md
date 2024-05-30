---
title: About Me
layout: about
---

My name is Chris Wellons, though I sometimes use the handle *skeeto*. I
am a professional software engineer with a passion for software
development. In my day job I regularly develop internal data processing
and data reduction tools, optimize software performance, and [mentor
students][mentor]. Occasionally I also do web development and data
visualization.

However, this blog — [ongoing since 2007][ten] — is largely about what I
do *outside* of my day job. Here I explain topics that I've recently
learned, share software development techniques I've discovered or even
invented, [showcase cool demos][showcase], and discuss my contributions to
open source.

In general, each of my articles should be unique on the internet at the
time they're published. I will not write a tutorial on some topic if
there already exist good, reliable tutorials. If I notice a gap on a
topic when learning it myself, then later, once I get the hang of it,
I'll fill that gap by publishing an article or two.

This blog is 100% transparent. Its complete source [is in a Git
repository][src]. From there you can see every single correction and
update I make.

### Interviews

* [Sacha Chua's Emacs Chat][chat] (2014)
* [Uses This][usesthis] (2019)

### Open Source Projects

Here are some of the more well known open source projects I've started.

* [w64devkit][]: a portable C and C++ development kit for Windows. It
  includes [every tool you need][w64-all] to comfortably build anything
  from high performance data crunchers [to graphical games][w64-game].

* [u-config][]: a small, highly-portable pkg-config implementation with
  first-class Windows support. Its primary use case is w64devkit.

* [Elfeed][elfeed]: an extensible syndication feed (RSS, Atom) reader
  for Emacs. I've written [lots of articles][elfeed-meta] about it.

* [Endlessh][endlessh]: an SSH tarpit. It [keeps hostile bots harmlessly
  tied up doing nothing][tarpit] while your server goes about its normal
  business uninterrupted.

* [Enchive][enchive]: encrypted personal archives. I've written about
  [the purpose behind this tool][enchive-meta].

* [binitools][binitools]: an old modding tool for the *Freelancer* space
  flight sim. I wrote it in college before starting this blog. There are
  still old copies floating around in various modding tool collections.
  The version linked here is a much more recent rewrite. The original
  source is still there, but on its own branch.

* [synSpace Map Editor][ssmapedit]: a map editor for [an old-school 2D
  space shooter game][synspace]. I initially developed this when I was a
  high school student, which is why it's written in Visual Basic 5.0. In
  college I made major updates to the interface, and that's what is in
  the repository. This was the first tool I ever wrote that was actually
  used productively by other people.

I have [hundreds more open source projects][repos], but these are the
most popular ones.

### Licensing

I'm a strong believer in the public domain. A healthy society needs a
rich and growing public domain.

Software licenses annoy me and [cause significant, unnecessary
friction][cc0]. Whenever possible, I put public domain dedications on
all my open source projects, generally via [the Unlicense][unlicense].
This very blog has a public domain dedication notice at the bottom of
each page. Even my [personal dotfiles][dotfiles] are in the public
domain.

If you use my work I do appreciate getting credit, but I do not legally
mandate it.

### Popular Articles

Some of my most popular articles:

* [Arena allocator tips and tricks](/blog/2023/09/27/)
* [Raw Linux Threads via System Calls](/blog/2015/05/15/)
* [Interactive Programming in C](/blog/2014/12/23/)
* [A GPU Approach to Path Finding](/blog/2014/06/22/)
* [A GPU Approach to Particle Physics](/blog/2014/06/29/)
* [C11 Lock-free Stack](/blog/2014/09/02/)
* [Switching to the Mutt Email Client](/blog/2017/06/15/)
* [Minimalist C Libraries](/blog/2018/06/10/)
* [Small, Freestanding Windows Executables](/blog/2016/01/31/)
* [A Basic Just-In-Time Compiler](/blog/2015/03/19/)

I also have a published paper: [ROP Gadget Prevalence and Survival under
Compiler-based Binary Diversification Schemes][rop]

### Donations

Occasionally someone is particularly happy with my open source work or
articles, and they'll ask if they can somehow donate money in support. I
make a very comfortable living outside of my blog, so such donations are
neither needed nor motivating. I'd much prefer your donation go to more
beneficial and effective causes than be wasted on a stingy miser like
me. So, instead, [**donate to GiveWell**][givewell]. It's a sort of
meta-charity that continuously analyzes where donations will have the
greatest impact in the world and directs your money in that direction.

### Software Preferences

In general I stick to smaller, simpler tools — especially those I can
modify and compile for myself. Sometimes this isn't possible (e.g. web
browsers), so I just use what's popular despite it being neither.

My preferred operating system is Debian. It has a great philosophy and
rock solid package ecosystem. Since I don't like software changing
underneath me, I strictly run the *stable* distribution. Of course I can
still be productive on other Linux distributions or even any of the
various BSDs. My dotfiles are quite portable and mostly work across all
of them.

Contrast to earlier in my career, Windows has grown on me as a development
platform. It's better than Linux on some dimensions, particularly as a
target, and for graphical applications. Though Windows has steadily grown
worse since 2012, and at an accelerating pace. w64devkit snapshots my
preferred development environment for any platform, even if lacking a few
important capabilities, such as [fuzz testing][fuzz]. My long-term goal is
to correct that situation.

For text editing, I [exclusively use Vim][vim]. That includes composing
blog articles and mail. I've never learned how to extend it, and probably
never will.

I use [Openbox][openbox] as my window manager, without adornments (panels,
taskbar, etc.).

My favorite programming language is C, and I've acquired [my own personal
style][style]. It's [fast, simple][c], and compiles at lightning speed.
Unlike most languages, it's quite reasonable for an individual to have a
comprehensive understanding of the entire language. C++ written in a C
style is also acceptable. While my day job requires mastery of a variety
of languages and technologies, for my own purposes I find little reason to
write software in anything other than C.

[I use Mutt][mutt] for reading email. It's not *perfect*, but it's close
enough. OpenPGP and email encryption are a technological dead end, so [I
don't bother with it][enchive]. I've never needed end-to-end encrypted
communication with strangers, so I haven't investigated the alternatives.

For consuming multimedia, I use [mpv][mpv]. I love how I can drive it
completely and precisely from the keyboard. If anything, its minimal
interface is *still* too cluttered for my tastes. Combined with
[yt-dlp][yt-dlp], I watch more (ad-free) YouTube than is probably
healthy.

### Contact

For most email related to my blog and projects, I have a [**public
inbox**][inbox] serving as a public forum. Consider using this first.
For private or personal messages, use my personal email address listed
at the top of this page.

I [accept patches][send-email] in both my private and public inboxes. This
means you can contribute changes without, say, a GitHub account.

### Cryptographic Identity

This is the key I use to sign important pieces of information, such as
Git tags and software releases. It interoperates perfectly with [my
verification script][simplegpg].

```
-----BEGIN PGP PUBLIC KEY BLOCK-----

xjMEAAAAABYJKwYBBAHaRw8BAQdAFBg8KN4P+OO3PW166sz9PVYoss4nXgdaxVSH
ieFHPdTNLUNocmlzdG9waGVyIFdlbGxvbnMgPHdlbGxvbnNAbnVsbHByb2dyYW0u
Y29tPsJhBBMWCAATBQIAAAAACRCv0VA6jI/0KgIbAwAAnC4A/jEr7DsBKzQ9ZkQf
P6debpDKDf/oTa0gUg7xYhetIBcEAP9p4R4IS2Om1ewd7muZ2Vz2JDZdQQS42N5q
Bd8c6WSnAQ==
=x6Nt
-----END PGP PUBLIC KEY BLOCK-----
```

Isn't that a little short for a PGP key?

1. It's an elliptic curve key, which has better security in a smaller
   package. You'll need newer software in order to use it.
2. It's a sign-only primary key with no subkeys. If you want to send me
   an encrypted message, I'll send you a short-term encryption subkey
   first.
3. I generated this key using [my own software][p2p], so it lacks the
   unnecessary cruft normally attached to public keys. Further, the
   secret key is never stored in any medium and is instead generated on
   demand when computing signatures.

Here's an example cleartext signature to try it out.

```
-----BEGIN PGP SIGNED MESSAGE-----
Hash: SHA256

My website: https://nullprogram.com/
My GitHub account: https://github.com/skeeto
My favorite color: Orange

-----BEGIN PGP SIGNATURE-----

wnUEARYIACcFAl1cV4oJEK/RUDqMj/QqFiEEXuuMjVBpxOm5SqhSr9FQOoyP9CoA
APSAAP4+Z0enXNnvEhOgT5kEoXH65RQQoR0Optjzdh+8JgaUKQEAqURrvXq+eaTt
Go9D+1vs/OY1xx126X850nJD7aJ8CQs=
=BwbG
-----END PGP SIGNATURE-----
```

It's wonderful having such concise keys and signatures!


[binitools]: https://github.com/skeeto/binitools
[c]: https://skeeto.s3.amazonaws.com/share/onward17-essays2.pdf
[cc0]: https://web.archive.org/web/20150225160057/https://dancohen.org/2013/11/26/cc0-by/
[chat]: https://sachachua.com/blog/2014/05/emacs-chat-christopher-wellons/
[dotfiles]: https://github.com/skeeto/dotfiles
[elfeed-meta]: /tags/elfeed/
[elfeed]: https://github.com/skeeto/elfeed
[emacsql]: https://github.com/skeeto/emacsql
[enchive-meta]: /blog/2017/03/12/
[enchive]: https://github.com/skeeto/enchive
[endlessh]: https://github.com/skeeto/endlessh
[fuzz]: /blog/2019/01/25/
[givewell]: https://secure.givewell.org/
[inbox]: https://lists.sr.ht/~skeeto/public-inbox
[mentor]: /blog/2016/09/02/
[mpv]: https://mpv.io/
[mutt]: /blog/2017/06/15/
[openbox]: http://openbox.org/wiki/Main_Page
[p2p]: https://github.com/skeeto/passphrase2pgp
[repos]: https://github.com/skeeto?tab=repositories
[rop]: https://skeeto.s3.amazonaws.com/share/p15-coffman.pdf
[send-email]: https://git-send-email.io/
[showcase]: /toys/
[simplegpg]: https://github.com/skeeto/simplegpg
[src]: https://github.com/skeeto/skeeto.github.com
[ssmapedit]: https://github.com/skeeto/ssMapEdit
[style]: /blog/2023/10/08/
[synspace]: http://www.synthetic-reality.com/synSpace.htm
[tarpit]: /blog/2019/03/22/
[ten]: /blog/2017/09/01/
[u-config]: https://github.com/skeeto/u-config
[unlicense]: http://unlicense.org/
[usesthis]: https://usesthis.com/interviews/chris.wellons/
[vim]: /blog/2017/04/01/
[w64-all]: /blog/2020/09/25/
[w64-game]: /blog/2021/03/11/
[w64devkit]: https://github.com/skeeto/w64devkit
[yt-dlp]: https://github.com/yt-dlp/yt-dlp
