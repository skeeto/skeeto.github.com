---
title: Keyringless GnuPG
layout: post
date: 2019-08-09T23:52:39Z
tags: [crypto, openpgp]
uuid: c51e0800-9bf5-4b77-93d4-35480c40a0ba
excerpt_separator: <!--more-->
---

*This article was discussed [on Hacker News][hn].*

My favorite music player is [Audacious][aud]. It follows the Winamp
Classic tradition of not trying to manage my music library. Instead it
waits patiently for me to throw files and directories at it. These
selections will be informally grouped into transient, disposable
playlists of whatever I fancy that day.

<!--more-->

This matters to me because my music collection is the result of around
25 years of hoarding music files from various sources including CD rips,
Napster P2P sharing, and, most recently, [YouTube downloads][yt]. It's
not well-organized, but it's organized well enough. Each album has its
own directory, and related albums are sometimes grouped together under
a directory for a particular artist.

Over the years I've tried various music players, and some have either
wanted to manage this library or hide the underlying file-organized
nature of my collection. Both situations are annoying because I really
don't want or need that abstraction. I'm going just fine thinking of
my music library in terms of files, thank you very much. Same goes for
ebooks.

**GnuPG is like a media player that wants to manage your whole music
library.** Rather than MP3s, it's crypto keys on a keyring. Nearly every
operation requires keys that have been imported into the keyring. Until
GnuPG 2.2.8 (June 2018), which added the `--show-keys` command, you
couldn't even be sure what you were importing until after it was already
imported. Hopefully it wasn't [garbage][poison].

GnuPG *does* has a pretty good excuse. It's oriented around the Web of
Trust model, and it can't follow this model effectively without having
all the keys at once. However, even if you don't buy into the Web of
Trust, the GnuPG interface still requires you to play by its rules.
Sometimes I've got a message, a signature, and a public key and I just
want to verify that they're all consistent with each other, *damnit*.

```
$ gpg --import foo.asc
gpg: key 1A719EF63AEB2CFE: public key "foo" imported
gpg: Total number processed: 1
gpg:               imported: 1
$ gpg --verify --trust-model always message.txt.sig message.txt
gpg: Signature made Fri 09 Aug 2019 05:44:43 PM EDT
gpg:                using EDDSA key ...1A719EF63AEB2CFE
gpg: Good signature from "foo" [unknown]
gpg: WARNING: Using untrusted key!
$ gpg --batch --yes --delete-key 1A719EF63AEB2CFE
```

Three commands and seven lines of output when one of each would do.
Plus there's a false warning: Wouldn't an "always" trust model mean
that this key is indeed trusted?

### Signify

Compare this to [OpenBSD's signify][bsdcan] ([also][tedu]). There's no
keyring, and it's up to the user — or the program shelling out to
signify — to supply the appropriate key. It's like the music player that
just plays whatever I give it. Here's a simplified [usage
overview][man]:

```
signify -G [-c comment] -p pubkey -s seckey
signify -S [-x sigfile] -s seckey -m message
signify -V [-x sigfile] -p pubkey -m message
```

When generating a new keypair (`-G`), the user must choose the
destination files for the public and secret keys. When signing a message
(a file), the user must supply the secret key and the message. When
verifying a file, the user must supply the public key and the message.
This is a popular enough model that [other, compatible implementations
with the same interface][minisign] have been developed.

Signify is deliberately incompatible with OpenPGP and uses its own
simpler, and less featureful, format. Wouldn't it be nice to have a
similar interface to verify OpenPGP signatures?

### SimpleGPG

Well, I thought so. So I put together a shell script that wraps GnuPG
and provides such an interface:

**[SimpleGPG][simplegpg]**

The interface is nearly identical to signify, and the GnuPG keyring is
hidden away as if it didn't exist. The main difference is that the keys
and signatures produced and consumed by this tool are fully compatible
with OpenPGP. You could use this script without requiring anyone else to
adopt something new or different.

To avoid touching your real keyring, the script creates a temporary
keyring directory each time it's run. The GnuPG option `--homedir`
instructs it to use this temporary keyring and ignore the usual one.
The temporary keyring is destroyed when the script exits. This is kind
of clunky, but there's no way around it.

Verification looks roughly like this in the script:

```
$ tmp=$(mktemp -d simplegpg-XXXXXX)
$ gpg --homedir $tmp
$ gpg --homedir $tmp --import foo.asc
$ gpg --homedir $tmp --verify message.txt.sig message.txt
$ rm -rf $tmp
```

Generating a key is trivial, and there's only a prompt for the
protection passphrase. Like signify, it will generate an Ed25519 key
and all outputs are ASCII-armored.

```
$ simplegpg -G -p keyname.asc -s keyname.pgp
passphrase:
passphrase (confirm):
```

Since signify doesn't have a concept of a user ID for a key, just an
"untrusted comment", the user ID is not emphasized here. The default
user ID will be "simplegpg key", so, if you plan to share the key with
regular GnuPG users who will need to import it into a keyring, you
probably want to use `-c` to give it a more informative name.

Unfortunately due GnuPG's very limited, keyring-oriented interface,
key generation is about three times slower than it should be. That's
because the protection key is run though the String-to-Key (S2K)
algorithm *three times*:

1. Immediately after the key is generated, the passphrase is converted
   to a key, the key is encrypted, and it's put onto the temporary
   keyring.

2. When exporting, the key passphrase is again run through the S2K to
   get the protection key to decrypt it.

3. The export format uses a slightly different S2K algorithm, so this
   export S2K is now used to create yet another protection key.

Technically the second *could* be avoided since gpg-agent, which is
always required, could be holding the secret key material. As far as I
can tell, gpg-agent simply does not learn freshly-generated keys. I do
not know why this is the case.

This is related to another issue. If you're accustomed to GnuPG, you may
notice that the passphrase prompt didn't come from pinentry, a program
specialized for passphrase prompts. GnuPG normally uses it for this.
Instead, the script handles the passphrase prompt and passes the
passphrase to GnuPG (via a file descriptor). This would not be necessary
if gpg-agent did its job. Without this part of the script, users are
prompted three times, via pinentry, for their passphrase when generating
a key.

When signing messages, the passphrase prompt comes from pinentry since
it's initiated by GnuPG.

```
$ simplegpg -S -s keyname.pgp -m message.txt
passphrase:
```

This will produce `message.txt.sig` with an OpenPGP detached signature.

The passphrase prompt is for `--import`, not `--detach-sign`. As with
key generation, the S2K is run more than necessary: twice instead of
once. First to generate the decryption key, then a second time to
generate a different encryption key for the keyring since the export
format and keyring use different algorithms. Ugh.

But at least gpg-agent does its job this time, so only one passphrase
prompt is necessary. In general, a downside of these temporary
keyrings is that gpg-agent treats each as different keys, and you will
need to enter your passphrase once for each message signed. Just like
signify.

Verification, of course, requires no prompting and no S2K.

```
$ simplegpg -V -p keyname.asc -m message.txt
```

That's all there is to keyringless OpenPGP signatures. Since I'm not
interested in the Web of Trust or keyservers, I wish GnuPG was more
friendly to this model of operation.

### passphrase2pgp

I mentioned that SimpleGPG is fully compatible with other OpenPGP
systems. This includes [my own passphrase2pgp][p2p], where your secret
key is stored only in your brain. No need for a secret key file. In the
time since I first wrote about it, passphrase2pgp has gained the ability
to produce signatures itself!

I've got my environment set up — `$REALNAME`, `$EMAIL`, and `$KEYID` per
the README — so I don't need to supply a user ID argument, nor will I be
prompted to confirm my passphrase since it's checked against a known
fingerprint. Generating the public key, for sharing, looks like this:

```
$ passphrase2pgp -K --armor --public >keyname.asc

Or just:

$ passphrase2pgp -ap >keyname.asc
```

Like with signify and SimplePGP, to sign a message I'm prompted for my
passphrase. It takes longer since the "S2K" here is much stronger by
necessity. The passphrase is used to generate the secret key, then from
that the signature on the message:

```
$ passphrase2pgp -S message.txt
```

For the SimpleGPG user on the other side it all looks the same as before:

```
$ simplegpg -V -p keyname.asc -m message.txt
```

I'm probably going to start signing my open source software releases,
and this is how I intend to do it.


[aud]: https://audacious-media-player.org/
[bsdcan]: https://www.openbsd.org/papers/bsdcan-signify.html
[hn]: https://news.ycombinator.com/item?id=20792472
[man]: https://man.openbsd.org/signify
[minisign]: https://jedisct1.github.io/minisign/
[p2p]: /blog/2019/07/10/
[poison]: https://github.com/skeeto/pgp-poisoner
[simplegpg]: https://github.com/skeeto/simplegpg
[tedu]: https://flak.tedunangst.com/post/signify
[yt]: https://ytdl-org.github.io/youtube-dl/
