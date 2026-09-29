---
title: LZMA Tarballs Are Coming
layout: post
tags: [compression]
uuid: 8abde298-3c30-3a06-a978-679230b0b3dc
---

<!-- 16 March 2009 -->

<img src="/img/diagram/compress.png" class="right"/>
Any developer that uses a non-toy operating system will be familiar
with [gzip](http://www.gzip.org/) and [bzip2](http://www.bzip.org/) tarballs (.tar.gz, .tgz, and
.tar.bz2). Most places will provide both versions so that the user can
use his preferred decompresser.

Both types are useful because they make tradeoffs at different points:
gzip is very fast with low memory requirements and bzip2 has much
better compression ratios at the cost of more memory and CPU
time. Users of older hardware will prefer gzip, because the benefits
of bzip2 are negated by the long decompression times, around 6 times
longer. This is why [ OpenBSD prefers
gzip](http://www.openbsd.org/faq/faq1.html#HowAbout).

But there is a new compression algorithm in town. Well, it has been
around for about 10 years now, but, if I understand correctly, was
patent encumbered (aka useless) for awhile. It is called the [Lempel-Ziv-Markov chain
algorithm](http://en.wikipedia.org/wiki/LZMA) (LZMA). It is still maturing and different software that
uses LZMA still can't quite talk to each other. [7-zip](http://www.7-zip.org/) and [LZMA Utils](http://tukaani.org/lzma/) are a couple examples.

GNU tar [added an
`--lzma` option](http://www.gnu.org/software/tar/) just last April, and finally gave it a
short option, `-J`, this past December. I take this as a
sign that LZMA tarballs (.tar.lzma) are going to become common over
the next several years. It also would seem that the GNU project has
officially blessed LZMA.

And not only that, I think LZMA tarballs will supplant bzip2
tarballs. The reason is because it is even more asymmetric than bzip2.

According to the LZMA Utils page, LZMA compression ratios are 15%
better than those of bzip2, but at the cost of being 4 to 12 times
slower on compression. In many applications, including tarball
distribution, this is completely acceptable because *decompression
is faster than bzip2*! There is an extreme asymmetry here that can
readily be exploited.

So, when a developer has a new release he tells his version control
system, or maybe his build system, to make a tar archive and compress
it with LZMA. If he has a computer from this millennium, it won't take
a lifetime to do, but it will still take some time. Since it could
take as much as two orders of magnitude longer to make than a gzip
tarball, he could make a gzip tarball first and put it up for
distribution. When the LZMA tarball is done, it will be about 30%
smaller and decompress almost as fast as the gzip tarball (but while
using a large amount of memory).

At this point, why would someone download a bzip2 archive? It's bigger
*and* slower. Right now possible reasons may be a lack of an LZMA
decompresser and/or lack of familiarity. Over time, these will both be
remedied.

Don't get me wrong. I don't hate bzip2. It is a very interesting
algorithm. In fact, I was breathless when I first understood the [ Burrows-Wheeler transform](http://en.wikipedia.org/wiki/Burrows-Wheeler_transform), which bzip2 uses at one stage. I would
argue that bzip2 is more elegant than gzip and LZMA because it is less
arbitrary. But I do think it will become obsolete.

Unfortunately, the confused zip archive is here to stay for now
because it is the only compression tool that a certain popular, but
inferior, operating system ships with. I say "confused" because it
makes the mistake of combining three tools into one: archive,
compression, and encryption. As a result, instead of doing one thing
well it does three things poorly. Cell phone designers also make the
same mistake. Fortunately I don't have to touch zip archives often.

Finally, don't forget that LZMA is mostly useful where the asymmetry
can be exploited: data is compressed once and decompressed many
times. Take the gitweb interface, which provides access to a git
repository through a browser. It will provide a gzip tarball of any
commit on the fly. It doesn't do this by having all these tarballs
lying around, but creates them on demand. Data is compressed once and
decompressed once. Because of this, gzip is, and will remain, the best
option for this setting.

In conclusion, consider creating LZMA tarballs next time, and don't be
afraid to use them when you come across them.
