---
title: The Long Key ID Collider
layout: post
date: 2019-07-22T21:27:02Z
tags: [crypto, openpgp, go]
uuid: e688fdea-0699-42dd-89ac-564d0b2b65bc
---

Over the last couple weeks I've spent a lot more time working with
OpenPGP keys. It's a consequence of polishing my [passphrase-derived
PGP key generator][p2p]. I've tightened up the internals, and it's
enabled me to explore the corners of the format, try interesting
things, and observe how various OpenPGP implementations respond to
weird inputs.

For one particularly cool trick, take a look at these two (private)
keys I generated yesterday. Here's the first:

```
-----BEGIN PGP PRIVATE KEY BLOCK-----

xVgEXTU3gxYJKwYBBAHaRw8BAQdAjJgvdh3N2pegXPEuMe25nJ3gI7k8gEgQvCor
AExppm4AAQC0TNsuIRHkxaGjLNN6hQowRMxLXAMrkZfMcp1DTG8GBg1TzQ9udWxs
cHJvZ3JhbS5jb23CXgQTFggAEAUCXTU3gwkQmSpe7h0QSfoAAGq0APwOtCFVCxpv
d/gzKUg0SkdygmriV1UmrQ+KYx9dhzC6xwEAqwDGsSgSbCqPdkwqi/tOn+MwZ5N9
jYxy48PZGZ2V3ws=
=bBGR
-----END PGP PRIVATE KEY BLOCK-----
```

And the second:

```
-----BEGIN PGP PRIVATE KEY BLOCK-----

xVgEXTU3gxYJKwYBBAHaRw8BAQdAzjSPKjpOuJoLP6G0z7pptx4sBNiqmgEI0xiH
Z4Xb16kAAP0Qyon06UB2/gOeV/KjAjCi91MeoUd7lsA5yn82RR5bOxAkzQ9udWxs
cHJvZ3JhbS5jb23CXgQTFggAEAUCXTU3gwkQmSpe7h0QSfoAAEv4AQDLRqx10v3M
bwVnJ8BDASAOzrPw+Rz1tKbjG9r45iE7NQEAhm9QVtFd8SN337kIWcq8wXA6j1tY
+UeEsjg+SHzkqA4=
=QnLn
-----END PGP PRIVATE KEY BLOCK-----
```

Concatenate these and then import them into GnuPG to have a look at
them. To avoid littering in your actual keyring, especially with
private keys, use the `--homedir` option to set up a temporary
keyring. I'm going to omit that option in the examples.

    $ gpg --import < keys.asc
    gpg: key 992A5EEE1D1049FA: public key "nullprogram.com" imported
    gpg: key 992A5EEE1D1049FA: secret key imported
    gpg: key 992A5EEE1D1049FA: public key "nullprogram.com" imported
    gpg: key 992A5EEE1D1049FA: secret key imported
    gpg: Total number processed: 2
    gpg:               imported: 2
    gpg:       secret keys read: 2
    gpg:   secret keys imported: 2


The user ID is "nullprogram.com" since I made these and that's me
taking credit. "992A5EEE1D1049FA" is called the *long key ID*: a
64-bit value that identifies the key. It's the lowest 64 bits of the
full key ID, a 160-bit SHA-1 hash. In the old days everyone used a
*short key ID* to identify keys, which was the lowest 32 bits of the
key. For these keys, that would be "1D1049FA". However, this was
deemed *way* too short, and everyone has since switched to long key
IDs, or even the full 160-bit key ID.

The key ID is nothing more than a SHA-1 hash of the key creation date —
unsigned 32-bit unix epoch seconds — and the public key material. So
secret keys have the same key ID as their associated public key. This
makes sense since they're a key *pair* and they go together.

Look closely and you'll notice that both keypairs have the same long
key ID. If you hadn't already guessed from the title of this article,
these are two different keys with the same long key ID. In other
words, **I've created a long key ID collision**. The GnuPG
`--list-keys` command prints the full key ID since it's so important:

    $ gpg --list-keys
    ---------------------
    pub   ed25519 2019-07-22 [SCA]
          A422F8B0E1BF89802521ECB2992A5EEE1D1049FA
    uid           [ unknown] nullprogram.com
    
    pub   ed25519 2019-07-22 [SCA]
          F43BC80C4FC2603904E7BE02992A5EEE1D1049FA
    uid           [ unknown] nullprogram.com

I was only targeting the lower 64 bits, but I actually managed to
collide the lowest 68 bits by chance. So a long key ID still isn't
enough to truly identify any particular key.

This isn't news, of course. Nor am I the first person to create a long
key ID collision. In 2013, [David Leon Gil published a long key ID
collision for two 4096-bit RSA public keys][gil]. However, that is the
only other example I was able to find. He did not include the private
keys and did not elaborate on how he did it. I know he *did* generate
viable keys, not just garbage for the public key portions, since they're
both self-signed.

Creating these keys was trickier than I had anticipated, and there's an
old, clever trick that makes it work. Building atop the work I did for
passphrase2pgp, I created a standalone tool that will create a long key
ID collision and print the two keypairs to standard output:

* **<https://github.com/skeeto/pgpcollider>**

Example usage:

    $ go get -u github.com/skeeto/pgpcollider
    $ pgpcollider --verbose > keys.asc

This can take up to a day to complete when run like this. The tool can
optionally coordinate many machines — see the `--server` / `-S` and
`--client` / `-C` options — to work together, greatly reducing the total
time. It took around 4 hours to create the keys above on a single
machine, generating a around 1 billion extra keys in the process. As
discussed below, I actually got lucky that it only took 1 billion. If
you modify the program to do short key ID collisions, it only takes a
few seconds.

The rest of this article is about how it works.

### Birthday Attacks

An important detail is that **this technique doesn't target any specific
key ID**. Cloning someone's long key ID is still very expensive. No,
this is a [*birthday attack*][ba]. To find a collision in a space of
2^64, on average I only need to generate 2^32 samples — the square root
of that space. That's perfectly feasible on a regular desktop computer.
To collide long key IDs, I need only generate about 4 billion IDs and
efficiently do membership tests on that set as I go.

That last step is easier said than done. Naively, that might look like
this (pseudo-code):

```
seen := map of long key IDs to keys
loop forever {
    key := generateKey()
    longID := key.ID[12:20]
    if longID in seen {
        output seen[longID]
        output key
        break
    } else {
        seen[longID] = key
    }
}
```

Consider the size of that map. Each long ID is 8 bytes, and we expect
to store around 2^32 of them. That's *at minimum* 32 GB of storage
just to track all the long IDs. The map itself is going to have some
overhead, too. Since these are literally random lookups, this all
mostly needs to be in RAM or else lookups are going to be *very* slow
and impractical.

And I haven't even counted the keys yet. As a saving grace, these are
Ed25519 keys, so that's 32 bytes for the public key and 32 bytes for the
private key, which I'll need if I want to make a self-signature. (The
signature itself will be larger than the secret key.) That's around
256GB more storage, though at least this can be stored on the hard
drive. However, to address these from the map I'd need at least 38 bits,
plus some more in case it goes over. Just call it another 8 bytes.

So that's, at a bare minimum, 64GB of RAM plus 256GB of other storage.
Since nothing is ideal, we'll need more than this. This is all still
feasible, but will require expensive hardware. We can do a lot better.

#### Keys from seeds

The first thing you might notice is that we can jettison that 256GB of
storage by being a little more clever about how we generate keys. Since
we don't actually care about the security of these keys, we can generate
each key from a seed much smaller than the key itself. Instead of using
8 bytes to reference a key in storage, just use those 8 bytes to store
the seed used to make the key.

```
counter := rand64()
seen := map of long key IDs to 64-bit seeds
loop forever {
    seed := counter
    counter++
    key := generateKey(seed)
    longID := key.ID[12:20]
    if longID in seen {
        output generateKey(seen[longID])
        output key
        break
    } else {
        seen[longID] = seed
    }
}
```

I'm incrementing a counter to generate the seeds because I don't want to
experience the birthday paradox to apply to my seeds. Each really must
be unique. I'm using SplitMix64 for the PRNG [since I learned it's the
fastest][rng] for Go, so a simple increment to generate seeds [is
perfectly fine][hp].

Ultimately, this still uses utterly excessive amounts of memory.
Wouldn't it be crazy if we could somehow get this 64GB map down to just
a few MBs of RAM? Well, we can!

#### Rainbow tables

For decades, password crackers have faced a similar problem. They want
to precompute the hashes for billions of popular passwords so that they
can efficiently reverse those password hashes later. However, storing
all those hashes would be unnecessarily expensive, or even infeasible.

So they don't. Instead they use [*rainbow tables*][rt]. Password hashes
are chained together into a hash chain, where a password hash leads to a
new password, then to a hash, and so on. Then only store the beginning
and the end of each chain.

To lookup a hash in the rainbow table, run the hash chain algorithm
starting from the target hash and, for each hash, check if it matches
the end of one of the chains. If so, recompute that chain and note the
step just before the target hash value. That's the corresponding
password.

For example, suppose the password "foo" hashes to `9bfe98eb`, and we
have a *reduction function* that maps a hash to some password. In this
case, it maps `9bfe98eb` to "bar". A trivial reduction function could
just be an index into a list of passwords. A hash chain starting from
"foo" might look like this:

    foo -> 9bfe98eb -> bar -> 27af0841 -> baz -> d9d4bbcb

In reality a chain would be a lot longer. Another chain starting from
"apple" might look like this:

    apple -> 7bbc06bc -> candle -> 82a46a63 -> dog -> 98c85d0a

We only store the tuples (foo, `d9d4bbcb`) and (apple, `98c85d0a`) in
our database. If the chains had been one million hashes long, we'd
still only store those two tuples. That's literally a 1:1000000
compression ratio!

Later on we're faced with reversing the hash `27af0841`, which isn't
listed directly in the database. So we run the chain forward from that
hash until either I hit the maximum chain length (i.e. password not in
the table), or we recognize a hash:

    27af0841 -> baz -> d9d4bbcb

That `d9d4bbcb` hash is listed as being in the "foo" hash chain. So I
regenerate that hash chain to discover that "bar" leads to `27af0841`.
Password cracked!

#### Collider rainbow table

My collider works very similarly. A hash chain works like this: Start
with a 64-bit seed as before, generate a key, get the long key ID,
**then use the long key ID as the seed for the next key**.

![](/img/diagram/collider-chain.svg)

There's one big difference. In the rainbow table the purpose is to run
the hash function backwards by looking at the previous step in the
chain. For the collider, I want to know if any of the hash chains
collide. So long as each chain starts from a unique seed, it would mean
we've found **two different seeds that lead to the same long key ID**.

Alternatively, it could be two different seeds that lead to the same
key, which wouldn't be useful, but that's trivial to avoid.

A simple and efficient way to check if two chains contain the same
sequence is to stop them at the same place in that sequence. Rather than
run the hash chains for some fixed number of steps, they stop when they
reach a *distinguishing point*. In my collider a distinguishing point is
where the long key ID ends with at least N 0 bits, where N determines
the average chain length. I chose 17 bits.

```
func computeChain(seed) {
    loop forever {
        key := generateKey(seed)
        longID := key.ID[12:20]
        if distinguished(longID) {
            return longID
        }
        seed = longID
    }
}
```

If two different hash chains end on the same distinguishing point,
they're guaranteed to have collided somewhere in the middle.

![](/img/diagram/collision.svg)

To determine where two chains collided, regenerate each chain and find
the first long key ID that they have in common. The step just before are
the colliding keys.

```
counter := rand64()
seen := map of long key IDs to 64-bit seeds
loop forever {
    seed := counter
    counter++
    longID := computeChain(seed)
    if longID in seen {
        output findCollision(seed, seen[longID])
        break
    } else {
        seen[longID] = seed
    }
}
```

Hash chains computation is embarrassingly parallel, so the load can be
spread efficiently across CPU cores. With these rainbow(-like) tables,
my tool can generate and track billions of keys in mere megabytes of
memory. The additional computational cost is the time it takes to
generate a couple more chains than otherwise necessary.


[ba]: https://en.wikipedia.org/wiki/Birthday_attack
[gil]: https://mailarchive.ietf.org/arch/msg/openpgp/Al8DzxTH2KT7vtFAgZ1q17Nub_g
[hp]: /blog/2018/07/31/
[p2p]: /blog/2019/07/10/
[rng]: https://github.com/skeeto/rng-go
[rt]: https://en.wikipedia.org/wiki/Rainbow_table
