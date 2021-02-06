---
title: Single-primitive authenticated encryption for fun
layout: post
date: 2021-01-30T03:39:10Z
tags: [crypto, c]
uuid: 92013b12-7f4b-4175-8d19-93520798a919
---

Just as a fun exercise, I designed and implemented from scratch a
standalone, authenticated encryption tool, including key derivation with
stretching, using a single cryptographic primitive. Or, more specifically,
*half of a primitive*. That primitive is the encryption function of the
[XXTEA block cipher][xxtea]. The goal was to pare both design and
implementation down to the bone without being broken in practice — *I
hope* — and maybe learn something along the way. This article is the tour
of my design. Everything here will be nearly the opposite of the [right
answers][right].

The [tool itself is named **xxtea**][repo] (lowercase), and it's supported
on all unix-like and Windows systems. It's trivial to compile, [even on
the latter][w64]. The code should be easy to follow from top to bottom,
with commentary about specific decisions along the way, though I'll quote
the most important stuff inline here.

The command line options [follow the usual conventions][getopt]. The two
modes of operation are encrypt (`-E`) and decrypt (`-D`). It defaults to
using standard input and standard output so it works great in pipelines.
Supplying `-o` sends output elsewhere (automatically deleted if something
goes wrong), and the optional positional argument indicates an alternate
input source.

    usage: xxtea <-E|-D> [-h] [-o FILE] [-p PASSWORD] [FILE]

    examples:
        $ xxtea -E -o file.txt.xxtea file.txt
        $ xxtea -D -o file.txt file.txt.xxtea

If no password is provided (`-p`), it prompts for a [UTF-8-encoded
password][utf8]. Of course it's not normally a good idea to supply a
password via command line argument, but it's been useful for testing.

### XXTEA block cipher

TEA stands for *Tiny Encryption Algorithm* and XXTEA is the second attempt
at fixing weaknesses in the cipher — with partial success. The remaining
issues should not be an issue for this particular application. XXTEA
supports a variable block size, but I've hardcoded my implementation to a
128-bit block size, along with some unrolling. I've also discarded the
unneeded decryption function. There are no data-dependent lookups or
branches so it's immune to speculation attacks.

XXTEA operates on 32-bit words and has a 128-bit key, meaning both block
and key are four words apiece. My implementation is about a dozen lines
long. Its prototype:

```c
// Encrypt a 128-bit block using 128-bit key
void xxtea128_encrypt(const uint32_t key[4], uint32_t block[4]);
```

All cryptographic operations are built from this function. Another way to
think about it is that it accepts two 128-bit inputs and returns a 128-bit
result:

    uint128 r = f(uint128 key, uint128 block);

Tuck that away in the back of your head since this will be important
later.

### Encryption

If I tossed the decryption function, how are messages decrypted? I'm sure
many have already guessed: XXTEA will be used in *counter mode*, or CTR
mode. Rather than encrypt the plaintext directly, encrypt a 128-bit block
counter and treat it like a stream cipher. The message is XORed with the
encrypted counter values for both encryption and decryption.

* Only half the cipher is needed.
* No padding scheme is necessary. With other block modes, if message
  lengths may not be exactly a multiple of the block size then you need
  some scheme for padding the last block.

A 128-bit increment with 32-bit limbs is easy:

```c
void
increment(uint32_t ctr[4])
{
    /* 128-bit increment, first word changes fastest */
    if (!++ctr[0]) if (!++ctr[1]) if (!++ctr[2]) ++ctr[3];
}
```

In xxtea, words are always marshalled in little endian byte order (least
significant byte first). With the first word as the least significant
limb, the entire 128-bit counter is itself little endian.

The counter doesn't start at zero, but at some randomly-selected 128-bit
nonce called the *initialization vector* (IV), wrapping around to zero if
necessary (incredibly unlikely). The IV will be included with the message
in the clear. This nonce allows one key (password) to be used with
multiple messages, as they'll all be encrypted using different,
randomly-chosen regions of an enormous keystream. It also provides
*semantic security*: encrypt the same file more than once and the
ciphertext will always be completely different.

```c
for (/* ... */) {
    uint32_t cover[4] = {ctr[0], ctr[1], ctr[2], ctr[3]};
    xxtea128_encrypt(key, cover);
    block[i+0] ^= cover[0];
    block[i+1] ^= cover[1];
    block[i+2] ^= cover[2];
    block[i+3] ^= cover[3];
    increment(ctr);
}
```

### Hash function

That's encryption, but there's still a matter of *authentication* and *key
derivation function* (KDF). To deal with both I'll need to devise a hash
function. Since I'm only using the one primitive, somehow I need to build
a hash function from a block cipher. Fortunately there's a tool for doing
just that: the [Merkle–Damgård construction][md].

Recall that `xxtea128_encrypt` accepts two 128-bit inputs and returns a
128-bit result. In other words, it *compresses* 256 bits into 128 bits: a
compression function. The two 128-bit inputs are cryptographically
combined into one 128-bit result. I can repeat this operation to fold an
arbitrary number of 128-bit inputs into a 128-bit hash result.

```c
uint32_t *input = /* ... */;
uint32_t hash[4] = {0, 0, 0, 0};
xxtea128_encrypt(input +  0, hash);
xxtea128_encrypt(input +  4, hash);
xxtea128_encrypt(input +  8, hash);
xxtea128_encrypt(input + 12, hash);
// ...
```

Note how the input is the key, not the block. The hash state is repeatedly
encrypted using the hash inputs as the key, mixing hash state and input.
When the input is exhausted, that block is the result. Sort of.

I used zero for the initial hash state in my example, but it will be more
challenging to attack if the starting input is something random. [Like
Blowfish][blowfish], in xxtea I chose the first 128 bits of the decimals
of pi:

```c
void
xxtea128_hash_init(uint32_t ctx[4])
{
    /* first 32 hexadecimal digits of pi */
    ctx[0] = 0x243f6a88; ctx[1] = 0x85a308d3;
    ctx[2] = 0x13198a2e; ctx[3] = 0x03707344;
}

/* Mix one block into the hash state. */
void
xxtea128_hash_update(uint32_t ctx[4], const uint32_t block[4])
{
    xxtea128_encrypt(block, ctx);
}
```

There are still a couple of problems. First, what if the input isn't a
multiple of the block size? This time I *do* need a padding scheme to fill
out that last block. In this case I pad it with bytes where each byte is
the number of padding bytes. For instance, `helloworld` becomes, roughly
speaking, `helloworld666666`.

That creates a different problem: This will have the same hash result as
an input that actually ends with these bytes. So the second rule is that
there is always a padding block, even if that block is 100% padding.

Another problem is that the Merkle–Damgård construction is prone to
*length-extension attacks*. Anyone can take my hash result and continue
appending additional data without knowing what came before. If I'm using
this hash to authenticate the ciphertext, someone could, for example, use
this attack to append arbitrary data to the end of messages.

Some important hash functions, such as the most common forms of SHA-2, are
vulnerable to length-extension attacks. Keeping this issue in mind, I
could address it later using HMAC, but I have an idea for nipping this in
the bud now. Before mixing the padding block into the hash state, I swap
the two middle words:

```c
/* Append final raw-byte block to hash state. */
void
xxtea128_hash_final(uint32_t ctx[4], const void *buf, int len)
{
    assert(len < 16);
    unsigned char tmp[16];
    memset(tmp, 16-len, 16);
    memcpy(tmp, buf, len);
    uint32_t k[4] = {
        loadu32(tmp +  0), loadu32(tmp +  4),
        loadu32(tmp +  8), loadu32(tmp + 12),
    };
    /* swap middle words to break length extension attacks */
    uint32_t swap = ctx[1];
    ctx[1] = ctx[2];
    ctx[2] = swap;
    xxtea128_encrypt(k, ctx);
}
```

This operation "ties off" the last block so that the hash can't be
extended with more input. *Or so I hope.* This is my own invention, and so
it may not actually work right. Again, this is for fun and learning!

**Update**: Aristotle Pagaltzis pointed out that when these two words are
identical the hash result will be unchanged, leaving it vulnerable to
length extension attack. This occurs about once every 2<sup>32</sup>
messages, which is far too small a security margin.

#### Caveats

Despite all that care, there are still two more potential weaknesses.

First, XXTEA was never designed to be used with the Merkle–Damgård
construction. I assume attackers can modify files I will decrypt, and so
the hash input is usually and mostly under control of attackers, meaning
they control the cipher key. Ciphers are normally designed assuming the
key is not under hostile control. This might be vulnerable to related-key
attacks.

As will be discussed below, I use this custom hash function in two ways.
In one the input is not controlled by attackers, so this is a non-issue.
In the second, the hash state is completely unknown to the attacker before
they control the input, which I believe mitigates any issues.

Second, a 128-bit hash state is a bit small these days. For very large
inputs, the chance of [collision via the birthday paradox][birthday] is a
practical issue.

In xxtea, digests are only computed over a few megabytes of input at a time
at most, even when encrypting giant files, so a 128-bit state should be
fine.

### Key derivation

The user will supply a password and somehow I need to turn that into a
128-bit key.

1. What if the password is shorter than 128 bits?
2. What if the password is longer than 128 bits?
3. It's safer for the cipher if the raw password isn't used directly.
4. I'd like offline, brute force attacks to be expensive.

The first three can be resolved by running the passphrase through the hash
function, using it as key derivation function. What about the last item?
Rather than hash the password once, I concatenate it, including null
terminator, repeatedly until it reaches a certain number of bytes
(hardcoded to 64 MiB, see `COST`), and hash that. That's a computational
workload that attackers must repeat when guessing passwords.

To avoid timing attacks based on the password length, I precompute all
possible block arrangements before starting the hash — all the different
ways the password might appear concatenated across 16-byte blocks. Blocks
may be redundantly computed if necessary to make this part constant time.
The hash is fed entirely from these precomputed blocks.

To defend against rainbow tables and the like — as well as make it harder
to attack other parts of the message construction — the initialization
vector is used as a salt, fed into the hash before the password
concatenation.

Unfortunately this KDF isn't *memory-hard*, and attackers can use economy
of scale to strengthen their attacks (GPUs, custom hardware). However, a
memory-hard KDF requires lots of memory to compute the key, making memory
an expensive and limiting factor for attackers. Memory-hard KDFs are
complex and difficult to design, and I made the trade-off for simplicity.

### Authentication

When I say the encryption is *authenticated* I mean that it should not be
possible for anyone to tamper with the ciphertext undetected without
already knowing the key. This is typically accomplished by computing a
keyed hash digest and appending it to the message, *message authentication
code* (MAC). Since it's keyed, only someone who knows the key can compute
the digest, and so attackers can't spoof the MAC.

This is where length-extension attacks come into play: With an improperly
constructed MAC, an attacker could append input without knowing the key.
Fortunately my hash function isn't vulnerable to length-extension attacks!

An alternative is to use an authenticated block mode such as [GCM][gcm],
which is still CTR mode at its core. Unfortunately, this is complicated,
and, unlike plain CTR, it would take me a long time to convince myself I
got it right. So instead I used CTR mode and my hash function in a
straightforward way.

At this point there's a question of what exactly you input into the hash
function. Do you hash the plaintext or do you hash the ciphertext? It's
tempting to do the former since it's (generally) not available to
attackers, and would presumably make it harder to attack. This is a
mistake. Always compute the MAC over the ciphertext, a.k.a. encrypt then
authenticate.

This is the called [the Doom Principle][doom]. Computing the MAC on the
plaintext means that recipients must decrypt untrusted ciphertext before
authenticating it. This is bad because messages should be authenticated
before decryption. So that's exactly what xxtea does. It also happens to
be the simplest option.

We have a hash function, but to compute a MAC we need a keyed hash
function. Again, I do the simplest thing that I believe isn't broken:
concatenate the key with the ciphertext. Or more specifically:

    MAC = hash(key || ctr || ciphertext)

**Update**: [Dimitrije Erdeljan explains why this is broken][broken] and
how to fix it. Given a valid MAC, attackers can forge arbitrary messages.

The counter is because xxtea uses chunked authentication with one megabyte
chunks. It can authenticate a chunk at a time, which allows it to decrypt,
with authentication, arbitrary amounts of ciphertext in a fixed amount of
memory. The worst that can happen is truncation between chunks — an
acceptable trade-off. The counter ensures each chunk MAC is uniquely
keyed, that they appear in order.

It's also important to note that the counter is appended *after* the key.
The counter is under hostile control — they can choose the IV — and having
the key there first means they have no information about the hash state.

All chunks are one megabyte except for the last chunk, which is always
shorter, signaling the end of the message. It may even be just a MAC and
zero-length ciphertext. This avoids nasty issues with parsing potentially
unauthenticated length fields and whatnot. Just stop successfully at the
first short, authenticated chunk.

Some will likely have spotted it, but a potential weakness is that I'm
using the same key for both encryption and authentication. These are
normally two different keys. This is disastrous in certain cases [like
CBC-MAC][cbc], but I believe it's alright here. It would be easy to
compute a separate MAC key, but I opted for simple.

### File format

In my usual style, encrypted files have no distinguishing headers or
fields. They just look like a random block of data. A file begins with the
16-byte IV, then a sequence of zero or more one megabyte chunks, ending
with a short chunk. It's indistinguishable from `/dev/random`.

    [IV][lMiB || MAC][1MiB || MAC][<1 MiB || MAC]

If the user types the incorrect password, it will be discovered when
authenticating the first chunk (read: immediately). This saves on a
dedicated check at the beginning of the file, though it means it's not
possible to distinguish between a bad password and a modified file.

I know my design has weaknesses as a result of artificial, self-imposed
constraints and deliberate trade-offs, but I'm curious if I've made any
glaring mistakes with practical consequences.


[birthday]: /blog/2019/07/22/
[blowfish]: /blog/2017/09/15/
[broken]: https://lists.sr.ht/~skeeto/public-inbox/%3C5b3ef28a-c8b7-2835-9a56-6968aca5606c%40gmail.com%3E
[cbc]: https://blog.cryptographyengineering.com/2013/02/15/why-i-hate-cbc-mac/
[doom]: https://moxie.org/2011/12/13/the-cryptographic-doom-principle.html
[gcm]: https://en.wikipedia.org/wiki/Galois/Counter_Mode
[getopt]: /blog/2020/08/01/
[md]: https://en.wikipedia.org/wiki/Merkle%E2%80%93Damg%C3%A5rd_construction
[repo]: https://github.com/skeeto/xxtea/tree/v0.1
[right]: https://latacora.micro.blog/2018/04/03/cryptographic-right-answers.html
[utf8]: /blog/2020/05/04/
[w64]: https://github.com/skeeto/w64devkit
[xxtea]: https://en.wikipedia.org/wiki/XXTEA
