---
title: Modifying the Middle of a zlib Stream
layout: post
date: 2016-09-09T03:37:03Z
tags: [c, compression]
uuid: 804f0de4-93c7-3a70-d0d5-2b3b7192491f
---

I recently ran into problem where I needed to modify bytes at the
beginning of an existing [zlib][zlib] stream. My program creates a
file in a format I do not control, and the file format has a header
indicating the total, uncompressed data size, followed immediately by
the data. The tricky part is that the **header and data are zlib
compressed together**, and I don't know how much data there will be
until I've collected it all. Sometimes it's many gigabytes. I don't
know how to fill out the header when I start, and I can't rewrite it
when I'm done since it's compressed in the zlib stream … *or so I
thought*.

<svg version="1.1" height="50" width="600">
  <rect fill="#dfd" width="149"  height="48" x="1"   y="1"
        stroke="black" stroke-width="2"/>
  <rect fill="#ddf" width="449" height="48" x="150" y="1"
        stroke="black" stroke-width="2"/>
  <text x="75" y="25" text-anchor="middle" dominant-baseline="central"
        font-size="22px" font-family="sans-serif">
    nelem
  </text>
  <text x="170" y="25" text-anchor="start" dominant-baseline="central"
        font-size="22px" font-family="sans-serif">
    samples[nelem]
  </text>
</svg>

My original solution was not to compress anything until it gathered
the entirety of the data. The input would get concatenated into a huge
buffer, then finally compressed and written out. It's not ideal,
because the program uses a lot more memory than it theoretical could,
especially if the data is highly compressible. It would be far better
to compress the data as it arrives and somehow update the header
later.

My first thought was to ask zlib to leave the header uncompressed,
then enable compression (`deflateParams()`) for the data. I'd work out
the magic offset and overwrite the uncompressed header bytes once I'm
done. There are two major issues with this, and I'll address each:

* zlib includes a checksum ([adler32][adler32]) at the end of the
  data, and editing the stream would cause a mismatch. This fairly
  easy to correct thanks to adler32's properties.

* zlib is an LZ77-family compressor and [compression comes from
  back-references][quine] into past (and sometimes future) bytes of
  decompressed output. Up to 32kB of data following the header could
  reference bytes in the header as a dictionary. I would need to ask
  zlib not to reference these bytes. Fortunately the zlib API is
  intentionally designed for this, though for different purposes.

### Fixing the checksum

Ignoring the second problem for a moment, I could fix the checksum by
computing it myself. When I overwrite my uncompressed header bytes, I
could also overwrite the checksum at the end of the compressed stream.
For illustration, here's an simple example implementation of adler32
(from Wikipedia):

~~~c
#define MOD_ADLER 65521

uint32_t
example_adler32(uint8_t *data, size_t len)
{
    uint32_t a = 1;
    uint32_t b = 0;
    for (size_t i = 0; i < len; i++) {
        a = (a + data[i]) % MOD_ADLER;
        b = (b + a) % MOD_ADLER;
    }
    return (b << 16) | a;
}
~~~

If you think about this for a moment, you may notice this puts me back
at square one. If I don't know the header, then I don't know the
checksum value at the end of the header, going into the data buffer.
I'd need to buffer all the data to compute the checksum. Fortunately
adler32 has the nice property that **two checksums can be concatenated
as if they were one long stream**. In a malicious context this is
known as a [length extension attack][len], but it's a real benefit
here.

It's like the zlib authors anticipated my needs, because the zlib
library has a function *exactly* for this:

~~~c
uint32_t adler32_combine(uint32_t adler1, uint32_t adler2, size_t len2);
~~~

I just have to keep track of the data checksum `adler2` and I can
compute the proper checksum later.

~~~c
uint64_t total = 0;
uint32_t data_adler = adler32(0, 0, 0); // initial value
while (processing_input) {
    // ...
    data_adler = adler32(data_adler, data, size);
    total += size;
}
// ...
uint32_t header_adler = adler32(0, 0, 0);
header_adler = adler32(header_adler, header, header_size);
uint32_t adler = adler32_combine(header_adler, data_adler, total);
~~~

### Preventing back-references

This part is more complicated and it helps to have some familiarity
with zlib. Every time zlib is asked to compress data, it's given a
[flush parameter][flush]. Under normal operation, this value is always
`Z_NO_FLUSH` until the end of the stream, in which case it's finalized
with `Z_FINISH`. Other flushing options force it to emit data sooner
at the cost of reduced compression ratio. This would primarily be used
to eliminate output latency on interactive streams (e.g. compressed
SSH sessions).

The necessary flush option for this situation is `Z_FULL_FLUSH`. It
forces out all output data and resets the dictionary: a fence.
**Future inputs cannot reference anything before a full flush.** Since
the header is uncompressed, it will not reference itself either.
Ignoring the checksum problem, I can safely modify these bytes.

### Putting it all together

To fully demonstrate all of this, I've put together an example using
one of my favorite image formats, [Netpbm P6][netpbm].

* <https://github.com/skeeto/zlib-mutate-demo>

In the P6 format, the image header is an ASCII description of the
image's dimensions followed immediately by raw pixel data.

    P6
    width height
    depth
    [RGB bytes]

It's a bit contrived, but it's the project I used to work it all out.
The demo reads arbitrary raw byte data on standard input and uses it
to produce a zlib-compressed PPM file on standard output. It doesn't
know the size of the input ahead of time, nor does it naively buffer
it all. There's no dynamic allocation (except for what zlib does
internally), but the program can process arbitrarily large input. The
only requirement is that **standard output is seekable**. Using the
technique described above, it patches the header within the zlib
stream with the final image dimensions after the input has been
exhausted.

If you're on a Debian system, you can use `zlib-flate` to decompress
raw zlib streams (gzip wraps zlib, but can't raw zlib). Alternatively
your system's `openssl` program may have zlib support. Here's running
it on itself as input. Remember, you can't pipe it into zlib-flate
because the output needs to be seekable in order to write the header.

    $ ./zppm < zppm > out.ppmz
    $ zlib-flate -uncompress < out.ppmz > out.ppm

![](/img/zppm.png)

Unfortunately due to the efficiency-mindedness of zlib, its use
requires careful bookkeeping that's easy to get wrong. It's a little
machine that at each step needs to be either fed more input or its
output buffer cleared. Even with all the error checking stripped away,
it's still too much to go over in full here, but I'll summarize the
parts.

First I process an empty buffer with compression disabled. The output
buffer will be discarded, so input buffer could be left uninitialized,
but I don't want to [upset anyone][valgrind]. All I need is the output
size, which I use to seek over the to-be-written header. I use
`Z_FULL_FLUSH` as described, and there's no loop because I presume my
output buffer is easily big enough for this.

~~~c
char bufin[4096];
char bufout[4096];

z_stream z = {
    .next_in = (void *)bufin,
    .avail_in = HEADER_SIZE,
    .next_out = (void *)bufout,
    .avail_out = sizeof(bufout),
};
deflateInit(&z, Z_NO_COMPRESSION);
memset(bufin, 0, HEADER_SIZE);
deflate(&z, Z_FULL_FLUSH);
fseek(stdout, sizeof(bufout) - z.avail_out, SEEK_SET);
~~~

Next I enable compression and reset the checksum. This makes zlib
track the checksum for all of the non-header input. Otherwise I'd be
throwing away all its checksum work and repeating it myself.

~~~c
deflateParams(&z, Z_BEST_COMPRESSION, Z_DEFAULT_STRATEGY);
z.adler = adler32(0, 0, 0);
~~~

I won't include it in this article, but what follows is a standard
zlib compression loop, consuming all the input data. There's one key
difference compared to a normal zlib compression loop: when the input
is exhausted, instead of `Z_FINISH` I use `Z_SYNC_FLUSH` to force
everything out. The problem with `Z_FINISH` is that it will write the
checksum, but we're not ready for that.

With all the input processed, it's time to go back to rewrite the
header. Rather than mess around with magic byte offsets, I start a
second, temporary zlib stream and do the `Z_FULL_FLUSH` like before,
but this time with the real header. In deciding the header size, I
reserved 6 characters for the width and 10 characters for the height.

~~~c
sprintf(bufin, "P6\n%-6lu\n%-10lu\n255\n", width, height);
uint32_t adler = adler32(0, 0, 0);
adler = adler32(adler, (void *)bufin, HEADER_SIZE);

z_stream zh = {
    .next_in = (void *)bufin,
    .avail_in = HEADER_SIZE,
    .next_out = (void *)bufout,
    .avail_out = sizeof(bufout),
};
deflateInit(&zh, Z_NO_COMPRESSION);
deflate(&zh, Z_FULL_FLUSH);
fseek(stdout, 0, SEEK_SET);
fwrite(bufout, 1, sizeof(bufout) - zh.avail_out, stdout);
fseek(stdout, 0, SEEK_END);
deflateEnd(&zh);
~~~

The header is now complete, so I can go back to finish the original
compression stream. Again, I assume the output buffer is big enough
for these final bytes.

~~~c
z.adler = adler32_combine(adler, z.adler, z.total_in - HEADER_SIZE);
z.next_out = (void *)bufout;
z.avail_out = sizeof(bufout);
deflate(&z, Z_FINISH);
fwrite(bufout, 1, sizeof(bufout) - z.avail_out, stdout);
deflateEnd(&z);
~~~

It's a lot more code than I expected, but it wasn't too hard to work
out. If you want to get into the nitty gritty and *really* hack a zlib
stream, check out [RFC 1950][rfc1950] and [RFC 1951][rfc1951].


[zlib]: http://www.zlib.net/
[quine]: /blog/2014/11/22/
[adler32]: https://en.wikipedia.org/wiki/Adler-32
[len]: https://en.wikipedia.org/wiki/Length_extension_attack
[flush]: http://www.bolet.org/~pornin/deflate-flush.html
[netpbm]: https://en.wikipedia.org/wiki/Netpbm_format
[valgrind]: http://valgrind.org/
[rfc1950]: https://tools.ietf.org/html/rfc1950
[rfc1951]: https://tools.ietf.org/html/rfc1951
