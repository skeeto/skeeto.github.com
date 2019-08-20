---
title: Predictable, Passphrase-Derived PGP Keys
layout: post
date: 2019-07-10T04:18:29Z
tags: [crypto, openpgp, go]
uuid: cae3111c-2887-404a-bc0e-80b8c45a2d06
---

*tl;dr*: **[passphrase2pgp][p2p]**.

One of my long-term concerns has been losing my core cryptographic keys,
or just not having access to them when I need them. I keep my important
data backed up, and if that data is private then I store it encrypted.
My keys are private, but how am I supposed to encrypt them? The chicken
or the egg?

The OpenPGP solution is to (optionally) encrypt secret keys using a key
derived from a passphrase. GnuPG prompts the user for this passphrase
when generating keys and when using secret keys. This protects the keys
at rest, and, with some caution, they can be included as part of regular
backups. The [OpenPGP specification, RFC 4880][rfc4880] has many options
for deriving a key from this passphrase, called *String-to-Key*, or S2K,
algorithms. None of the options are great.

In 2012, [I selected the strongest S2K configuration][publish] and,
along with a very strong passphrase, put my GnuPG keyring on the
internet as part of [my public dotfiles repository][dotfiles]. It was a
kind of super-backup that would guarantee their availability anywhere
I'd need them.

My timing was bad because, with the release of GnuPG 2.1 in 2014, GnuPG
fundamentally changed its secret keyring format. [S2K options are now
(quietly!) ignored][bug] when deriving the protection keys. Instead it
auto-calibrates to much weaker settings. With this new version of GnuPG,
I could no longer update the keyring in my dotfiles repository without
significantly downgrading its protection.

By 2017 I was pretty irritated with the whole situation. I let my
OpenPGP keys expire, and then [I wrote my own tool][enchive] to replace
the only feature of GnuPG I was actively using: encrypting my backups
with asymmetric encryption. One of its core features is that the
asymmetric keypair can be derived from a passphrase using a memory-hard
key derivation function (KDF). Attackers must commit a significant
quantity of memory (expensive) when attempting to crack the passphrase,
making the passphrase that much more effective.

Since the asymmetric keys themselves, not just the keys protecting them,
are derived from a passphrase, I never need to back them up! They're
also always available whenever I need them. **My keys are essentially
stored entirely in my brain** as if I was a character in a William
Gibson story.

### Tackling OpenPGP key generation

At the time I had expressed my interest in having this feature for
OpenPGP keys. It's something I've wanted for a long time. I first [took
a crack at it in 2013][old] (now the the `old-version` branch) for
generating RSA keys. [RSA isn't that complicated][rsa] but [it's very
easy to screw up][frsa]. Since I was rolling it from scratch, I didn't
really trust myself not to subtly get it wrong. Plus I never figured out
how to self-sign the key. GnuPG doesn't accept secret keys that aren't
self-signed, so it was never useful.

I took another crack at it in 2018 with a much more brute force
approach. When a program needs to generate keys, it will either read
from `/dev/u?random` or, on more modern systems, call `getentropy(3)`.
These are all ultimately system calls, and [I know how to intercept
those with Ptrace][int]. If I want to control key generation for *any*
program, not just GnuPG, I could intercept these inputs and replace them
with the output of a CSPRNG keyed by a passphrase.

**[Keyed][keyed]: Linux Entropy Interception**

In practice this doesn't work at all. Real programs like GnuPG and
OpenSSH's `ssh-keygen` don't rely solely on these entropy inputs. They
[also grab entropy from other places][ent], like `getpid(2)`,
`gettimeofday(2)`, and even extract their own scheduler and execution
time noise. Without modifying these programs I couldn't realistically
control their key generation.

Besides, even if it *did* work, it would still be fragile and unreliable
since these programs could always change how they use the inputs. So,
ultimately, it was more of an experiment than something practical.

### passphrase2pgp

For regular readers, it's probably obvious that I [recently learned
Go][go]. While searching for good projects idea for cutting my teeth, I
noticed that [Go's "extended" standard library][ext] has a lot of useful
cryptographic support, so the idea of generating the keys myself may be
worth revisiting.

Something else also happened since my previous attempt: The OpenPGP
ecosystem now has widespread support for elliptic curve cryptography. So
instead of RSA, I could generate a Curve25519 keypair, which, by design,
is basically impossible to screw up. **Not only would I be generating
keys on my own terms, I'd being doing it *in style*, baby.**

There are two different ways to use Curve25519:

1. Digital signatures: Ed25519 (EdDSA)
2. Diffie–Hellman (encryption): X25519 (ECDH)

In GnuPG terms, the first would be a "sign only" key and the second is
an "encrypt only" key. But can't you usually do both after you generate
a new OpenPGP key? If you've used GnuPG, you've probably seen the terms
"primary key" and "subkey", but you probably haven't had think about
them since it's all usually automated.

The *primary key* is the one associated directly with your identity.
It's always a signature key. The OpenPGP specification says this is a
signature key only by convention, but, practically speaking, it really
must be since signatures is what holds everything together. Like
packaging tape.

If you want to use encryption, independently generate an encryption key,
then sign that key with the primary key, binding that key as a *subkey*
to the primary key. This all happens automatically with GnuPG.

Fun fact: Two different primary keys can have the same subkey. Anyone
could even bind any of your subkeys to their primary key! They only need
to sign the public key! Though, of course, they couldn't actually use
your key since they'd lack the secret key. It would just be really
confusing, and could, perhaps in certain situations, even cause some
OpenPGP clients to malfunction. (Note to self: This demands
investigation!)

It's also possible to have signature subkeys. What good is that?
Paranoid folks will keep their primary key only on a secure, air-gapped,
then use only subkeys on regular systems. The subkeys can be revoked and
replaced independently of the primary key if something were to go wrong.

In Go, generating an X25519 key pair is this simple (yes, it actually
takes array pointers, [which is rather weird][fix]):

```go
package main

import (
	"crypto/rand"
	"fmt"

	"golang.org/x/crypto/curve25519"
)

func main() {
	var seckey, pubkey [32]byte
	rand.Read(seckey[:]) // FIXME: check for error
	seckey[0] &= 248
	seckey[31] &= 127
	seckey[31] |= 64
	curve25519.ScalarBaseMult(&pubkey, &seckey)
	fmt.Printf("pub %x\n", pubkey[:])
	fmt.Printf("sec %x\n", seckey[:])
}
```

The three bitwise operations are optional since it will do these
internally, but it ensures that the secret key is in its canonical form.
The actual Diffie–Hellman exchange requires just one more function call:
`curve25519.ScalarMult()`.

For Ed25519, the API is higher-level:

```go
package main

import (
	"crypto/rand"
	"fmt"

	"golang.org/x/crypto/ed25519"
)

func main() {
	seed := make([]byte, ed25519.SeedSize)
	rand.Read(seed) // FIXME: check for error
	key := ed25519.NewKeyFromSeed(seed)
	fmt.Printf("pub %x\n", key[32:])
	fmt.Printf("sec %x\n", key[:32])
}
```

Signing a message with this key is just one function call:
`ed25519.Sign()`.

Unfortunately that's the easy part. The other 400 lines of the real
program are concerned only with encoding these values in the complex
OpenPGP format. That's the hard part. GnuPG's `--list-packets` option
was really useful for debugging this part.

### OpenPGP specification

(Feel free to skip this section if the OpenPGP wire format isn't
interesting to you.)

Following the specification was a real challenge, especially since many
of the details for Curve25519 only appear in still incomplete (and still
erroneous) updates to the specification. I certainly don't envy the
people who have to parse arbitrary OpenPGP packets. It's finicky and has
arbitrary parts that don't seem to serve any purpose, such as redundant
prefix and suffix bytes on signature inputs. Fortunately I only had to
worry about the subset that represents an unencrypted secret key export.

OpenPGP data is broken up into *packets*. Each packet begins with a tag
identifying its type, followed by a length, which itself is a variable
length. All the packets produced by passphrase2pgp are short, so I could
pretend lengths were all a single byte long.

For a secret key export with one subkey, we need the following packets
in this order:

1. Secret-Key: Public-Key packet with secret key appended
2. User ID: just a length-prefixed, UTF-8 string
3. Signature: binds Public-Key packet (1) and User ID packet (2)
4. Secret-Subkey: Public-Subkey packet with secret subkey appended
5. Signature: binds Public-Key packet (1) and Public-Subkey packet (4)

A Public-Key packet contains the creation date, key type, and public key
data. A Secret-Key packet is the same, but with the secret key literally
appended on the end and a different tag. The Key ID is (essentially) a
SHA-1 hash of the Public-Key packet, meaning **the creation date is part
of the Key ID**. That's important for later.

I had wondered if the [SHAttered][sha] attack could be used to create
two different keys with the same full Key ID. However, there's no slack
space anywhere in the input, so I doubt it.

User IDs are usually a RFC 2822 name and email address, but that's only
convention. It can literally be an empty string, though that wouldn't be
useful. OpenPGP clients that require anything more than an empty string,
such as GnuPG during key generation, are adding artificial restrictions.

The first Signature packet indicates the signature date, the signature
issuer's Key ID, and then optional metadata about how the primary key is
to be used and the capabilities the key owner's client. The signature
itself is formed by appending the Public-Key packet portion of the
Secret-Key packet, the User ID packet, and the previously described
contents of the signature packet. The concatenation is hashed, the hash
is signed, and the signature is appended to the packet. Since the
options are included in the signature, they can't be changed by another
person.

In theory the signature is redundant. A client could accept the
Secret-Key packet and User ID packet and consider the key imported. It
would then create its own self-signature since it has everything it
needs. However, my primary target for passphrase2pgp is GnuPG, and it
will not accept secret keys that are not self-signed.

The Secret-Subkey packet is exactly the same as the Secret-Key packet
except that it uses a different tag to indicate it's a subkey.

The second Signature packet is constructed the same as the previous
signature packet. However, it signs the concatenation of the Public-Key
and Public-Subkey packets, binding the subkey to that primary key. This
key may similarly have its own options.

To create a public key export from this input, a client need only chop
off the secret keys and fix up the packet tags and lengths. The
signatures remain untouched since they didn't include the secret keys.
That's essentially what other people will receive about your key.

If someone else were to create a Signature packet binding your
Public-Subkey packet with their Public-Key packet, they could set their
own options on their version of the key. So my question is: Do clients
properly track this separate set of options and separate owner for the
same key? If not, they have a problem!

The format may not sound so complex from this description, but there are
a ton of little details that all need to be correct. To make matters
worse, the feedback is usually just a binary "valid" or "invalid". The
world could use an OpenPGP format debugger.

### Usage

There is one required argument: either `--uid` (`-u`) or `--load`
(`-l`). The former specifies a User ID since a key with an empty User ID
is pretty useless. It's my own artificial restriction on the User ID.
The latter loads a previously-generated key which will come with a User
ID.

To generate a key for use in GnuPG, just pipe the output straight into
GnuPG:

    $ passphrase2pgp --uid "Foo <foo@example.com>" | gpg --import

You will be prompted for a passphrase. That passphrase is run through
[Argon2id][argon2], a memory-hard KDF, with the User ID as the salt.
Deriving the key requires 8 passes over 1GB of state, which takes my
current computers around 8 seconds. With the `--paranoid` (`-x`) option
enabled, that becomes 16 passes over 2GB (perhaps not paranoid enough?).
The output is 64 bytes: 32 bytes to seed the primary key and 32 bytes to
seed the subkey.

Despite the aggressive KDF settings, you will still need to choose a
strong passphrase. Anyone who has your public key can mount an offline
attack. A 10-word Diceware or [Pokerware][poker] passphrase is more than
sufficient (~128 bits) while also being quite reasonable to memorize.

Since the User ID is the salt, an attacker couldn't build a single
rainbow table to attack passphrases for different people. (Though your
passphrase really should be strong enough that this won't matter!) The
cost is that you'll need to use exactly the same User ID again to
reproduce the key. *In theory* you could change the User ID afterward to
whatever you like without affecting the Key ID, though it will require a
new self-signature.

The keys are not encrypted (no S2K), and there are few options you can
choose when generating the keys. If you want to change any of this, use
GnuPG's `--edit-key` tool after importing. For example, to set a
protection passphrase:

    $ gpg --edit-key Foo
    gpg> passwd

There's a lot that can be configured from this interface.

If you just need the public key to publish or share, the `--public`
(`-p`) option will suppress the private parts and output only a public
key. It works well in combination with ASCII armor, `--armor` (`-a`).
For example, to put your public key on the clipboard:

    $ passphrase2pgp -u '...' -ap | xclip

The tool can create detached signatures (`--sign`, `-S`) entirely on its
own, too, so you don't need to import the keys into GnuPG just to make
signatures:

    $ passphrase2pgp --sign --uid '...' program.exe

This would create a file named `program.exe.sig` with the detached
signature, ready to be verified by another OpenPGP implementation. In
fact, you can hook it directly up to Git for signing your tags and
commits without GnuPG:

    $ git config --global gpg.program passphrase2pgp

This only works for signing, and it cannot verify (`verify-tag` or
`verify-commit`).

It's pretty tedious to enter the `--uid` option all the time, so, if
omitted, passphrase2pgp will infer the User ID from the environment
variables REALNAME and EMAIL. Combined with the KEYID environment
variable (see the README for details), you can easily get away with
*never* storing your keys: only generate them on demand when needed.

That's how I intend to use passphrase2pgp. When I want to sign a file,
I'll only need one option, one passphrase prompt, and a few seconds of
patience:

    $ passphrase2pgp -S path/to/file

#### January 1, 1970

The first time you run the tool you might notice one offensive aspect of
its output: Your key will be dated January 1, 1970 — i.e. unix epoch
zero. This predates PGP itself by more than two decades, so it might
alarm people who receive your key.

Why do this? As I noted before, the creation date is part of the Key ID.
Use a different date, and, as far as OpenPGP is concerned, you have a
different key. Since users probably don't want to remember a specific
datetime, *at seconds resolution*, in addition to their passphrase,
passphrase2pgp uses the same hard-coded date by default. A date of
January 1, 1970 is like NULL in a database: no data.

If you don't like this, you can override it with the `--time` (`-t`) or
`--now` (`-n`) options, but it's up to you to remain consistent.

#### Vanity Keys

If you're interested in vanity keys — e.g. where the Key ID spells out
words or looks unusual — it wouldn't take much work to hack up the
passphrase2pgp source into generating your preferred vanity keys. It
would easily beat anything else I could find online.

### Reconsidering limited OpenPGP

Initially my intention was *never* to output an encryption subkey, and
passphrase2pgp would only be useful for signatures. By default it still
only produces a sign key, but you can still get an encryption subkey
with the `--subkey` (`-s`) option. I figured it might be useful to
generate an encryption key, even if it's not output by default. Users
can always ask for it later if they have a need for it.

Why only a signing key? Nobody should be using OpenPGP for encryption
anymore. Use better tools instead and retire the [20th century
cryptography][mg]. If you don't have an encryption subkey, nobody can
send you OpenPGP-encrypted messages.

In contrast, OpenPGP signatures are still kind of useful and lack a
practical alternative. The Web of Trust failed to reach critical mass,
but that doesn't seem to matter much in practice. Important OpenPGP keys
can be bootstrapped off TLS by strategically publishing them on HTTPS
servers. Keybase.io has done interesting things in this area.

Further, [GitHub officially supports OpenPGP signatures][github], and I
believe GitLab does too. This is another way to establish trust for a
key. IMHO, there's generally too much emphasis on binding a person's
legal identity to their OpenPGP key (e.g. the idea behind key-signing
parties). I suppose that's useful for holding a person legally
accountable if they do something wrong. I'd prefer trust a key with has
an established history of valuable community contributions, even if done
so [only under a pseudonym][why].

So sometime in the future I may again advertise an OpenPGP public key.
If I do, those keys would certainly be generated with passphrase2pgp. I
may not even store the secret keys on a keyring, and instead generate
them on the fly only when I occasionally need them.


[argon2]: https://github.com/P-H-C/phc-winner-argon2
[bug]: https://dev.gnupg.org/T1800
[dotfiles]: /blog/2012/06/23/
[enchive]: /blog/2017/03/12/
[ent]: /blog/2019/04/30/
[ext]: https://golang.org/x/
[fix]: https://github.com/golang/go/issues/32670
[frsa]: https://blog.trailofbits.com/2019/07/08/fuck-rsa/
[github]: https://github.blog/2016-04-05-gpg-signature-verification/
[go]: /tags/go/
[int]: /blog/2018/06/23/
[keyed]: https://github.com/skeeto/keyed
[mg]: https://blog.cryptographyengineering.com/2014/08/13/whats-matter-with-pgp/
[old]: https://github.com/skeeto/passphrase2pgp/tree/old-version
[p2p]: https://github.com/skeeto/passphrase2pgp
[poker]: /blog/2017/07/27/
[publish]: /blog/2012/06/24/
[rfc4880]: https://tools.ietf.org/html/rfc4880
[rsa]: /blog/2015/10/30/
[sha]: https://shattered.io/
[why]: https://en.wikipedia.org/wiki/Why_the_lucky_stiff
