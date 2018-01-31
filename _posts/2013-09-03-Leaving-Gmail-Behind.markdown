---
title: Leaving Gmail Behind
layout: post
date: 2013-09-03T03:45:58Z
tags: [emacs]
uuid: 8d4f4f52-2295-32c7-8452-d22d4f45a437
---

*Update May 2017*: Shortly after [switching to modal editing][vim], I
stopped using Emacs and Notmuch as my mail client. I [now use Mutt and
Vim][mutt].

For the last 8 years I have been using Gmail as my e-mail provider,
during which 3 years I was a student. It was very convenient,
inexpensive (cost-free!), and especially suitable for a student using
various lab computers and stuck behind a fairly restrictive firewall
(an anti-file-sharing measure). I didn't have to worry about filtering
spam or maintaining or paying for a server. Easy, easy, easy. This has
finally come to an end.

That convenience kept me as a user after college up until now. As of
two weeks ago I changed my e-mail address. You can find it listed
below my portrait on this page (if you're reading this on my website).
Not only am I using my own domain name, but I'm running my own e-mail
server. After attempting, and failing, at creating a decent setup with
[mu4e][mu4e], I ended up following this guide:

 * [A Hacker's Replacement for GMail][hacker]

It's built around the superb [notmuch mail indexer][notmuch], which
includes a powerful, fast Emacs e-mail client. Just from its technical
superiority **I'm wishing I switched to notmuch years ago**. I'm now
understanding for the first time why all those old fogey hackers like
to use e-mail for everything: mailing lists, software patches, bug
reporting, etc. E-mail a user interface agnostic system, giving
everyone their own choice. The tricky part is setting up a decent
interface to it.

### Prompting the change

As you know, [Google Reader shut down this past July][reader]. This
left me using only two Google services: Talk and Mail. Not only is
Talk easily replaceable — it has no significant data on Google's
servers — [it will be shut down soon as well][talk]. I would need to
move to a different XMPP server anyway. If I could move off of Gmail,
I would finally be able to discontinue my Google account *for good*!

Why would I want to do this? It's become increasingly apparent,
especially this year, that there is very little privacy to be had when
logged into a Google account. Various intelligence and law enforcement
organizations have easy — likely automated — access to user data,
especially e-mail. I'd really like to take that privacy back.

There are also the technical reasons. It's a bit embarrassing to
admit, but the final straw that pushed me to finally leaving Gmail was
[the new compose interface][compose] — a technical issue rather than
a privacy issue — which was pushed out just a few days before I left.
I found it to be very unpleasant and, worse, completely incompatible
with Pentadactyl, as there is no longer a plain-text option (the one
listed is a fake). A huge technical step backwards, towards the layman
and away from the power user. I could do so much better than this.

Also embarrassing was being unable to have any meaningful use of PGP
with e-mail all these years. That's something I've always wanted to
fix.

[Brian][brian] was a guinea pig for me, because, between the two of
us, he was actually the first one to move to his own e-mail platform.
Seeing his success was a big encouragement for me. Not only could it
be done fairly easily, but the results would be a huge improvement.
This was all within my grasp!

### A daunting task

Switching *everything* about my e-mail — provider, client, spam
filter, server, domain — and running it myself would be a daunting
task. There's 8 years of archived e-mail to manage, though I kept it
trimmed to a relatively light 1 GB of storage (which I cut down to 200
MB before exporting). I've made backups of all this e-mail on
occasion, but I never had to worry about searching or actually using
it. The backups were done "just in case."

Gmail has excellent spam filtering, an advantage of having so many
samples available, and I'm a complete newbie at dealing with it
myself. Despite having my e-mail address published on this blog for
the last 6 years, I surprisingly only receive about one spam message
per day, so this wasn't actually a huge risk for me.

Then there's the issue of not looking like spam myself. My e-mail
server needs to sit in a friendly IP neighborhood. I need to have a
proper PTR record (reverse DNS). I need to generally look legitimate.
No showing up to deliver mail to other mail servers in just a t-shirt.
This is actually something I'm still struggling with right now. In
fact, if I've sent you personal e-mail in the past and you're using
Gmail, you should check your spam folder right now, because something
I've sent recently may likely have been caught in it. I don't know why
yet.

I would also need to learn the ropes of a new e-mail client. I used
Eudora from 1995 to 2005, then Gmail up until now. Now, I'm the last
person to be reluctant about learning how to operate a new piece of
software. I'm constantly on the lookout for better software. The
problem is that I use e-mail for a lot of very important things. I
can't afford mistakes. I need to hit the ground running on this one.

### notmuch vs mu4e

Since I decided early on to go with an Emacs-based e-mail client,
learning a new e-mail client got a lot easier. I know Emacs Lisp
*pretty* darn well. In the worst case of getting stuck, I could very
easily study the client's source code and work out for myself whatever
is going on. I could even monkey patch it in my configuration if it
was causing problems for me (and I've already done exactly that).

I wanted to use the [Maildir format][maildir], something I could hack
on if needed. The two obvious choices for this were mu4e and notmuch,
both started in 2009. I initially reached for mu4e. Compared to
notmuch, it follows Emacs idioms more closely. For example, the e-mail
listing is oriented around a mark and execute paradigm, like a dired
buffer. After an initial glance, it felt more integrated.

Unfortunately, mu4e is still not mature enough for real productive
use, making it far too risky for e-mail. I found out the hard way that
the database format has varied regularly between versions. Worse, mu4e
is not suitable for remote access. Not only does it assume the Maildir
directory is on the local host, it uses absolute paths to access it,
so it won't work over [sshfs][sshfs] as I had hoped. Bummer.

In contrast, the notmuch client
[is specifically designed to be operated remotely][remote]. Emacs
doesn't realize that it runs the notmuch client over SSH. Emacs
doesn't need to touch the Maildir directly. It's a beautiful setup,
one very friendly to [versioning dotfiles][dotfiles]. I've already
done some pretty heavy configuration to get it exactly the way I want
it. On top of this, notmuch is incredibly fast and stable. It's been a
very enjoyable client. So much so that it inspired me to build a web
feed reader with a similar interface (to be described in my next
post).

### The mail server

My early plan was to run an e-mail server on a Raspberry Pi. It's
low-powered, making it very inexpensive and quiet to operate. It's
also very portable, so I wouldn't need to lug a server around if I
needed to move it — no more difficult than moving a cell phone
charger. I could run it from my nightstand next to my bed if I wanted
to. On the other hand, I would be a little nervous running my mail
server on a residential connection. The downtime would be a product of
my ISP, my power company, and my router. It's not bad, but it's risky
when I'm worried about receiving important e-mail. If I was in the
middle of job hunting I probably wouldn't attempt it at all.
Fortunately e-mail servers will retry over the course of several days,
so I think this would generally be manageable.

This plan was struck down by Comcast's network policies. The good news
is that my IP address has not changed in years. The bad news is that
they block port 25 both incoming and outgoing as an anti-spam measure.
This makes it impossible to run an e-mail server, because e-mail
*must* be received on port 25 (MX records were misdesigned feature).
For outgoing, I would need to send e-mail through Comcast's smarthost
server, which brings up the privacy issue again. I assume the same
organizations have this tapped as well. Even if port 25 wasn't
blocked, I wouldn't be able to set a PTR record and my IP neighborhood
would be suspicious.

I ended up going with [Digital Ocean][do], as the linked guide
suggested. The smallest, cheapest offering is more than suitable for
my needs both as an e-mail server and an XMPP server. It will probably
be handy for [other short-lived, experimental][experimental] servers
too.

[I used getmail][getmail] to get all of my old mail onto the mail
server in the Maildir format. It was completely straightforward and
probably the easiest part of it all.

### PGP

I can finally use GnuPG with my e-mail. An important factor in this
setup is that encryption and decryption is done *locally*, not on my
e-mail server. I don't need to trust the server with my private keys.
However, verification of signatures is done on the server, which is
slightly less than ideal, but manageable.

I thought I might need to generate a fresh PGP key for this new e-mail
address, but instead I learned something new about PGP. A key can have
multiple identities attached to it, so all I needed to do was edit the
key and add the new e-mail address. The PGP designers had already
thought of this problem two decades ago! The updated key is linked
next to my portrait, as well as distributed on the public keyservers.

I look forward to making more use of cryptography with my e-mail.

### The old address

It will take me awhile, a year or more, to move everything off of my
old e-mail address. I have a lot of accounts associated with it, many
of which won't allow me to change my e-mail address. So, in the
meantime, anything sent there will continue to be forwarded to me.
That's now the only purpose of my Google account.

I'm a little worried about using my new e-mail address as my Digital
Ocean account because it presents a circularity problem. I could
easily get myself locked out of everything. I'll have to figure out
how I'm going to handle that situation. (Use my work e-mail address?
So long as I don't get locked out of my e-mail and get fired all at
the same time.)

If you're a hacker, I encourage you to run your own e-mail server if
you're not doing so already. It's been extremely liberating for me.


[reader]: /blog/2013/06/13/
[mu4e]: http://www.djcbsoftware.nl/code/mu/mu4e.html
[hacker]: http://dbpmail.net/essays/2013-06-29-hackers-replacement-for-gmail.html
[notmuch]: http://notmuchmail.org/
[talk]: http://windowspbx.blogspot.com/2013/05/hangouts-wont-hangout-with-other.html
[brian]: http://www.50ply.com/
[compose]: http://gizmodo.com/gmails-new-compose-window-will-soon-be-your-only-choic-1123199551
[maildir]: http://cr.yp.to/proto/maildir.html
[mutt]: /blog/2017/06/15/
[sshfs]: http://fuse.sourceforge.net/sshfs.html
[remote]: http://notmuchmail.org/remoteusage/
[dotfiles]: /blog/2012/06/23/
[getmail]: http://www.mattcutts.com/blog/backup-gmail-in-linux-with-getmail/
[do]: https://www.digitalocean.com/?refcode=a613ef5c79c1
[experimental]: /blog/2013/01/26/
[vim]: /blog/2017/04/01/
