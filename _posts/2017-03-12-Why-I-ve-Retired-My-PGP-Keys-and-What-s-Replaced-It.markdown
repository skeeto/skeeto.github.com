---
title: Why I've Retired My PGP Keys and What's Replaced It
layout: post
date: 2017-03-12T21:54:38Z
tags: [crypto, openpgp]
uuid: d8fc83d0-4f6e-31dc-95b9-3ec4427725e1
---

*Update August 2019: I've got a PGP key again but only for signing. [I
use another of my own tools, **passphrase2pgp**][p2p], to manage it.*

**tl;dr**: [Enchive][enchive] (rhymes with "archive") has replaced my
use of GnuPG.

Two weeks ago I tried to encrypt a tax document for archival and
noticed my PGP keys had just expired. GnuPG had (correctly) forbidden
the action, requiring that I first edit the key and extend the
expiration date. Rather than do so, I decided to take this opportunity
to retire my PGP keys for good. Over time I've come to view PGP as
largely a failure — it [never reached the critical mass][fv], the
tooling has always [been problematic][mg], and it's now [a dead
end][mm]. The only thing it's been successful at is signing Linux
packages, and even there it could be replaced with something simpler
and better.

I still have a use for PGP: encrypting sensitive files to myself for
long term storage. I've also been using it to consistently to sign Git
tags for software releases. However, very recently [this lost its
value][sha], though I doubt anyone was verifying these signatures
anyway. It's never been useful for secure email, especially when [most
people use it incorrectly][inline]. I only need to find a replacement
for archival encryption.

I could use an encrypted filesystem, but which do I use? I use LUKS to
protect my laptop's entire hard drive in the event of a theft, but for
archival I want something a little more universal. Basically I want the
following properties:

* Sensitive content must not normally be in a decrypted state. PGP
  solves this by encrypting files individually. The archive filesystem
  can always be mounted. An encrypted volume would need to be mounted
  just prior to accessing it, during which everything would be
  exposed.

* I should be able to encrypt files from any machine, even
  less-trusted ones. With PGP I can load my public key on any machine
  and encrypt files to myself. It's like a good kind of ransomware.

* It should be easy to back these files up elsewhere, even on
  less-trusted machines/systems. This isn't reasonably possible with an
  encrypted filesystem which would need to be backed up as a huge
  monolithic block of data. With PGP I can toss encrypted files
  anywhere.

* I don't want to worry about per-file passphrases. Everything should
  be encrypted with/to the same key. PGP solves this by encrypting
  files to a recipient. This requirement prevents most stand-alone
  crypto tools from qualifying.

I couldn't find anything that fit the bill, so I did **exactly what
you're not supposed to do and rolled my own: [Enchive][enchive]**. It
was loosely inspired by [OpenBSD's signify][signify]. It has the tiny
subset of PGP features that I need — using modern algorithms — plus
one more feature I've always wanted: the ability to **generate a
keypair from a passphrase**. This means I can reliably access my
archive keypair anywhere without doing something strange like
[uploading my private keys onto the internet][pub].

### On Enchive

Here's where I'd put the usual disclaimer about not using it for
anything serious, blah blah blah. But really, I don't care if anyone
else uses Enchive. It exists just to scratch my own personal itch. If
you have any doubts, don't use it. I'm putting it out there in case
anyone else is in the same boat. It would also be nice if any glaring
flaws I may have missed were pointed out.

Not expecting it to be available as a nice package, I wanted to make it
trivial to build Enchive anywhere I'd need it. Except for including
stdint.h in exactly one place to get the correct integers for crypto,
it's written in straight C89. All the crypto libraries are embedded, and
there are no external dependencies. There's even an "amalgamation" build,
so `make` isn't required: just point your system's `cc` at it and you're
done.

#### Algorithms

For encryption, Enchive uses [Curve25519][curve], [ChaCha20][chacha],
and [HMAC-SHA256][hmac].

Rather than the prime-number-oriented RSA as used in classical PGP
(yes, GPG 2 *can* do better), Curve25519 is used for the asymmetric
cryptography role, using the relatively new elliptic curve
cryptography. It's stronger cryptography and the keys are *much*
smaller. It's a Diffie-Hellman function — an algorithm used to
exchange cryptographic keys over a public channel — so files are
encrypted by generating an ephemeral keypair and using this ephemeral
keypair to perform a key exchange with the master keys. The ephemeral
public key is included with the encrypted file and the ephemeral
private key is discarded.

I used the ["donna" implementation][donna] in Enchive. Despite being
the hardest to understand (mathematically), this is the easiest to
use. It's literally just one function of two arguments to do
everything.

Curve25519 only establishes the shared key, so next is the stream
cipher ChaCha20. It's keyed by the shared key to actually encrypt the
data. This algorithm has the same author as Curve25519 ([djb][djb]),
so it's natural to use these together. It's really straightforward, so
there's not much to say about it.

For the Message Authentication Code (MAC), I chose HMAC-SHA256. It
prevents anyone from modifying the message. Note: This doesn't prevent
anyone who knows the master public key from replacing the file
wholesale. That would be solved with a digital signature, but this
conflicts with my goal of encrypting files without the need of my secret
key. The MAC goes at the end of the file, allowing arbitrarily large
files to be encrypted single-pass as a stream.

There's a little more to it (IV, etc.) and is described in detail in the
README.

#### Usage

The first thing you'd do is generate a keypair. By default this is done
from `/dev/urandom`, in which case you should immediately back them up.
But if you're like me, you'll be using Enchive's `--derive` (`-d`)
feature to create it from a passphrase. In that case, the keys are
backed up in your brain!

    $ enchive keygen --derive
    secret key passphrase:
    secret key passphrase (repeat):
    passphrase (empty for none):
    passphrase (repeat):

The first prompt is for the secret key passphrase. This is converted
into a Curve25519 keypair using an scrypt-like key derivation algorithm.
The process requires 512MB of memory (to foil hardware-based attacks)
and takes around 20 seconds.

The second passphrase (or the only one when `--derive` isn't used), is
the *protection key* passphrase. The secret key is encrypted with this
passphrase to protect it at rest. You'll need to enter it any time you
decrypt a file. The key derivation step is less aggressive for this key,
but you could also crank it up if you like.

At the end of this process you'll have two new files under
`$XDG_CONFIG_DIR/enchive`: `enchive.pub` (32 bytes) and `enchive.sec`
(64 bytes). The first you can distribute anywhere you'd like to encrypt
files; it's not particularly sensitive. The second is needed to decrypt
files.

To encrypt a file for archival:

    $ enchive archive sensitive.zip

No prompt for passphrase. This will create `sensitive.zip.enchive`.

To decrypt later:

    $ enchive extract sensitive.zip.enchive
    passphrase:

If you've got many files to decrypt, entering your passphrase over and
over would get tiresome, so Enchive includes a key agent that keeps
the protection key in memory for a period of time (15 minutes by
default). Enable it with the `--agent` flag (it may be enabled by
default someday).

    $ enchive --agent extract sensitive.zip.enchive

Unlike ssh-agent and gpg-agent, there's no need to start the agent
ahead of time. It's started on demand as needed and terminates after
the timeout. It's completely painless.

Both `archive` and `extract` operate stdin to stdout when no file is
given.

### Feature complete

As far as I'm concerned, Enchive is feature complete. It does
everything I need, I don't want it to do anything more, and at least
two of us have already started putting it to use. The interface and
file formats won't change unless someone finds a rather significant
flaw. There *is* some wiggle room to replace the algorithms in the
future should Enchive have that sort of longevity.


[donna]: https://github.com/agl/curve25519-donna
[inline]: https://josefsson.org/inline-openpgp-considered-harmful.html
[chacha]: https://cr.yp.to/chacha.html
[curve]: https://cr.yp.to/ecdh.html
[sha]: https://shattered.io/
[signify]: http://www.tedunangst.com/flak/post/signify
[mg]: https://blog.cryptographyengineering.com/2014/08/13/whats-matter-with-pgp/
[fv]: https://blog.filippo.io/giving-up-on-long-term-pgp/
[mm]: https://moxie.org/blog/gpg-and-me/
[pub]: /blog/2012/06/24/
[enchive]: https://github.com/skeeto/enchive
[djb]: https://cr.yp.to/djb.html
[hmac]: https://tools.ietf.org/html/rfc2104
[p2p]: /blog/2019/07/10/
