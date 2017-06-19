---
title: Switching to the Mutt Email Client
layout: post
date: 2017-06-15T21:55:01Z
tags: [vim, debian]
uuid: ef3edda3-ddaa-37cd-6bfc-fc9d13cd3414
---

*Note: The way I manage my email wouldn't really work for most people,
so don't read this as a recommendation. This is just a discussion of
how I prefer to use email.*

It was almost four years ago I switched from webmail to a customized
email configuration [based on Notmuch and Emacs][gmail]. Notmuch served
as both as a native back-end that provided indexing and tagging, as well
as a front-end, written in Emacs Lisp. It dramatically improved my email
experience, and I wished I had done it earlier. I've really enjoyed
having so much direct control over my email.

However, I'm always fiddling with things — fiddling feels a lot more
productive than it actually is — and last month I re-invented my email
situation, this time switching to a combination of [Mutt][mutt],
[Vim][vim], [mu][mu], and [tmux][tmux]. The entirety of my email
interface now resides inside a terminal, and I'm enjoying it even more.
I feel I've "leveled up" again in my email habits.

[![](/img/screenshot/mutt-thumb.png)](/img/screenshot/mutt.png)

On the server-side I also switched from Exim to Postfix and procmail,
making the server configuration a whole lot simpler. Including
SpamAssassin, it's just three lines added to the default Debian
configuration. It leaves a lot less room for error, and I could rebuild
it from scratch with little trouble if there was an emergency. My
previous configuration required quite a bit of system configuration,
such as relying on [incron][incron] to sort incoming mail, particularly
spam, but procmail now does this job more cleanly.

### Towards Robustness

Over the years I've gotten less patient when it comes to [dealing with
breaking changes][rh] in software, and I've gotten more conservative
about system stability. Continuously updating my configurations and
habits to the latest software changes was an interesting challenge
earlier in my career, but today there are much better uses of my time.
Debian Stable, my preferred operating system, runs at pretty much the
perfect pace for me.

Following these changing preferences, one of the biggest motivations for
my recent email change was to make my email setup more robust and
stable. Until now, email was tied tightly to Emacs, with a configuration
drawing directly from [MELPA][melpa], pulling in the bleeding edge
version of every package I use. Breaking changes arrive at unexpected
times, and occasionally the current version of a package temporarily
doesn't work. Usually it's because the developer pushed a bad commit
right before the latest MELPA build, and so the package is broken for a
few hours or days. I've been [guilty of this myself][guilty]. MELPA
Stable is intended to address these issues, but it seems to break more
often than normal MELPA. For example, at the time of this writing,
[Evil][evil] is not installable via MELPA Stable due to an unmet
dependency.

Tying something as vital as email to this Rube Goldberg machine made me
nervous. Access to my email depended on a number of independent systems
of various levels of stability to mostly work correctly. My switch to
Mutt cut this down to just a couple of very stable systems.

### format=flowed

I've long believed HTML email is an abomination that should never have
been invented. Text is the ideal format for email, and there are a
number of specifications to make it work well across different systems.
One of those standards is [RFC 3676][rfcff], colloquially named
[format=flowed][ff], or just f=f.

Messages encoded with f=f allow mail clients to safely reflow the
paragraphs to nicely fit the user's display, whether that display be
thinner or wider than the sender's original message. It's also
completely compatible with mail clients that don't understand
format=flowed, which will display the message as the sender originally
wrapped it.

The gist of f=f is that messages can have both "soft" and "hard" line
breaks. If a line ends with a space, then it's a soft line break. The
mail client can safely reflow lines separated by a soft line break.
Without the trailing space, it's a hard line break, which prohibits
flowing with the next line. The last line of a paragraph ends with a
hard line break. It's also used for text that shouldn't reflow, such as
code samples.

I'll illustrate using an underscore in place of a space, so that you can
see it:

    This is a message in the format=flowed style, allowing_
    mail clients to flow this message nicely in displays of_
    different widths.

    > This is an example of a quote block in a message,_
    > which is supported by the format=flowed specification.
    >> It also supports nested quote blocks, which means_
    >> this paragraph won't flow into the previous.

The RFC covers edge cases that require special "space-stuffing" rules,
but, when editing a text email in an editor, you only need to think
about soft and hard line breaks. In my case, Mutt takes care of the rest
of the details.

Unfortunately [Emacs's lacks decent support for f=f][emacsff], though
I'm sure a minor mode could be written to make it work well. On the
other hand, Vim has been [playing an increasing role in my day-to-day
editing][tt], and it has excellent built-in support for f=f. Since I'm
now using Vim to compose all of my email, I get it for free.

First, I tell Mutt that I want to use f=f in my `.muttrc`:

    set text_flowed

Then in Vim, I add the `w` flag to `formatoptions`, which tells it to
wrap paragraphs using soft line breaks.

    set fo+=w

If I want to inspect my f=f formatting, I temporarily enable the `list`
option, which displays a `$` for all newlines.

    set list

Although few people would notice a difference, I feel a little bad for
*not* using f=f all these years! A few people may have endured some
ugly, non-flowing emails from me. My only condolance is that at least it
wasn't HTML.

It's not all roses, though. When I reply to a message, Mutt doesn't
insert the quoted text as f=f into my reply, so I have to massage it
into f=f myself. Also, just as GitHub doesn't support Markdown in email
responses, neither does it support f=f. When I reply to issues by email,
GitHub won't nicely reflow my carefully crafted f=f message, needlessly
making email responses an inferior option.

### Features unneeded

One reason I didn't choose this particular email arrangement 4 years ago
was that PGP support was one of my prime requirements. Mutt has solid
PGP support, but, with a Maildir setup (i.e. not IMAP), I'd have to use
the key on the server, which was out of the question. Since [I no longer
care about PGP][enchive], my email requirements are more relaxed.

Over the years wasn't making much use of Notmuch's tagging system. I
only used two tags: "unread" and "inbox" (e.g. read, but still needs
attention). Otherwise I'd use Notmuch's powerful search to find what I
wanted. I still needed to keep track of the tags I was using, so the
Notmuch index, nearly as large as the email messages themselves, became
part of my mail backup.

The [Maildir format][maildir] itself supports some flags: passed (P),
replied (R), seen (S), trashed (T), draft (D), and flagged (F). These
are stored in the message's filename. In my new configuration, the
"seen" tag (inversely) takes the place of Notmuch's "unread" tag. The
"flagged" tag takes place of the "inbox" tag. Normally in Mutt you'd use
mailboxes — i.e. Maildir subdirectories — for something like this, but I
prefer all my mail to sit in one big bucket. Search, don't sort.

Since the two flags are part of the filename, I no longer need to
include a tag database (i.e. the entire Notmuch index) in the backup,
and my mail backups are much smaller. I could continue to use Notmuch
for searching, but I've settled on mu instead. When I perform a search,
mu writes the results to a temporary Maildir using symbolic links, which
I visit with Mutt. The mu index is transient and doesn't need to be
backed up.

Mu also manages my contacts alias list. It can produce a Mutt-style
alias file based on the contents of my Maildir:

    mu cfind --format=mutt-alias > aliases

It's been really nice to have all my email sitting around as nothing
more than a big pile of files like this. I've begun [writing little
scripts to harvest data][py] from it, too.

### Configuration files

As with all my personal configuration files, you can [see my .muttrc
online][muttrc]. The first few weeks I was tweaking this file hourly,
but I've now got it basically the way I want.


[emacsff]: https://www.emacswiki.org/emacs/GnusFormatFlowed
[enchive]: /blog/2017/03/12/
[evil]: https://github.com/emacs-evil/evil
[ff]: https://joeclark.org/ffaq.html
[gmail]: /blog/2013/09/03/
[guilty]: https://github.com/skeeto/elfeed/issues/202
[incron]: http://inotify.aiken.cz/?section=incron&page=about
[maildir]: https://cr.yp.to/proto/maildir.html
[melpa]: https://melpa.org/
[mu]: https://www.djcbsoftware.nl/code/mu/
[mutt]: http://www.mutt.org/
[muttrc]: https://github.com/skeeto/dotfiles/blob/master/_muttrc
[py]: https://docs.python.org/3.4/library/email.html
[rfcff]: https://tools.ietf.org/html/rfc3676
[rh]: https://www.youtube.com/watch?v=oyLBGkS5ICk
[tmux]: https://tmux.github.io/
[tt]: /blog/2017/04/01/
[vim]: http://www.vim.org/
