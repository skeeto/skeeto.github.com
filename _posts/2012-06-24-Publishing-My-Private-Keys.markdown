---
title: Publishing My Private Keys
layout: post
tags: [crypto, openpgp, tutorial]
uuid: cb40de11-5f3c-306f-b792-6214d65605a1
---

*Update March 2017: I [no longer use PGP][update]. Also, there's a
bug in GnuPG [that silently discards these security settings][bug],
and it's unlikely to ever get fixed. You'll need to find/build an old
version of GnuPG if you want to properly protect your secret keys.*

*Update August 2019: I've got a PGP key again, but [I'm using my own
tool, **passphrase2pgp**][p2p], to manage it. This tool allows for a
particular workflow that GnuPG has never and will never provide. It
doesn't rely on S2K as described below.*

One of the items [in my dotfiles repository](/blog/2012/06/23/) is my
PGP keys, both private and public. I believe this is a unique approach
that hasn't been done before — a public experiment. It may *seem*
dangerous, but I've given it careful thought and I'm only using the
tools already available from GnuPG. It ensures my keys are well
backed-up (via the
[Torvalds method](http://markmail.org/message/bupvay4lmlxkbphr)) and
available wherever I should need them.

In your GnuPG directory there are two core files: `secring.gpg` and
`pubring.gpg`. The first contains your secret keys and the second
contains public keys. `secring.gpg` is not itself encrypted. You can
(should) have different passphrases for each key, after all. These
files (or any PGP file) can be inspected with `--list-packets`. Notice
it won't prompt for a passphrase in order to get this data,

    $ gpg --list-packets ~/.gnupg/secring.gpg
    :secret key packet:
        version 4, algo 1, created 1298734547, expires 0
        skey[0]: [2048 bits]
        skey[1]: [17 bits]
        iter+salt S2K, algo: 9, SHA1 protection, hash: 10, salt: ...
        protect count: 10485760 (212)
        protect IV:  a6 61 4a 95 44 1e 7e 90 88 c3 01 70 8d 56 2e 11
        encrypted stuff follows
    :user ID packet: "Christopher Wellons <...>"
    :signature packet: algo 1, keyid 613382C548B2B841
    ... and so on ...

Each key is encrypted *individually* within this file with a
passphrase. If you try to use the key, GPG will attempt to decrypt it
by asking for the passphrase. If someone were to somehow gain access
to your `secring.gpg`, they'd still need to get your passphrase, so
[pick a strong one](/blog/2009/02/07/). The official documentation
advises you to keep your `secring.gpg` well-guarded and only rely on
the passphrase as a cautionary measure. I'm ignoring that part.

If you're using GPG's defaults, your secret key is encrypted with
CAST5, a symmetric block cipher. The encryption key is your passphrase
salted (mixed with a non-secret random number) and hashed with SHA-1
65,536 times. Using the hash function over and over is called
[key stretching](http://en.wikipedia.org/wiki/Key_stretching). It
greatly increases the amount of required work for a brute-force
attack, making your passphrase more effective. All of these settings
can be adjusted to better protect the secret key at the cost of less
portability. Since I've chosen to publish my `secring.gpg` in my
dotfiles repository I cranked up the settings as far as I can.

I changed the cipher to AES256, which is more modern, more trusted,
and more widely used than CAST5. For the passphrase digest, I selected
SHA-512. There are better passphrase digest algorithms out there but
this is the longest, slowest one that GPG offers. The PGP spec
supports between 1024 and 65,011,712 digest iterations, so I picked
one of the largest. 65 million iterations takes my laptop over a
second to process — absolutely brutal for someone attempting a
brute-force attack. Here's the command to change to this configuration
on an existing key,

    gpg --s2k-cipher-algo AES256 --s2k-digest-algo SHA512 --s2k-mode 3 \
        --s2k-count 65000000 --edit-key <key id>

When the edit key prompt comes up, enter `passwd` to change your
passphrase. You can enter the same passphrase again and it will re-use
it with the new configuration.

I'm feeling quite secure with my secret key, despite publishing my
`secring.gpg`. Before now, I was much more at risk of losing it to
disk failure than having it exposed. I challenge anyone who doubts my
security to crack my secret key. I'd rather learn that I'm wrong
sooner than later!

With this established in my dotfiles repository, I can more easily
include private dotfiles. Rather than use a symmetric cipher with an
individual passphrase on each file, I encrypt the private dotfiles
*to* myself. All my private dotfiles are managed with one key: my PGP
key. This also plays better with Emacs. While it supports transparent
encryption, it doesn't even attempt to manage your passphrase (with
good reason). If the file is encrypted with a symmetric cipher, Emacs
will prompt for a passphrase on each save. If I encrypt them with my
public key, I only need the passphrase when I first open the file.

How it works right now is any dotfile that ends with `.priv.pgp` will
be decrypted into place — not symlinked, unfortunately, since this is
impossible. The install script has a `-p` switch to disable private
dotfiles, such as when I'm using an untrusted computer. `gpg-agent`
ensures that I only need to enter my passphrase once during the
install process no matter how many private dotfiles there are.


[update]: /blog/2017/03/12/
[bug]: https://dev.gnupg.org/T1800
[p2p]: /blog/2019/07/10/
