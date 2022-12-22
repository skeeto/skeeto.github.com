---
title: QOI is now my favorite asset format
layout: post
date: 2022-12-18T03:45:44Z
tags: [c, compression]
uuid: 184bb5f6-3c31-4faf-9a15-3a693b8f4c7d
excerpt_separator: <!--more-->
---

*This article was discussed [on Hacker News][hn].*

The [Quite OK Image (QOI) format][qoi] was announced late last year and
finalized into a specification a month later. Initially dismissive, a
revisit has shifted my opinion to impressed. The format hits a sweet spot
in the trade-off space between complexity, speed, and compression ratio.
Also considering its alpha channel support, QOI has become my default
choice for embedded image assets. It's not perfect, but at the very least
it's a solid foundation.

<!--more-->

Since I'm now working with QOI images, I need a good QOI viewer, and so I
added support to my ill-named [pbmview][pbmview] tool, which I wrote to
serve the same purpose for [Netpbm][pbm]. I will [continue to use Netpbm
as an output format][showcase], especially for raw video output, but no
longer will I use it for an embedded asset (nor re-invent yet another
[RLE][] over Netpbm).

I was dismissive because the website claimed, and still claims today, QOI
images are "a similar size" to PNG. However, for the typical images where
I would use PNG, QOI is around 3x larger, and some outliers are far worse.
The 745 PNGs on my blog — a perfect test corpus for my own needs — convert
to QOIs 2.8x larger on average. The official QOI benchmark has much better
results, 1.3x larger, but that's because it includes a lot of photography
where PNG and QOI both do poorly, making QOI seem more comparable.

However, as I said, QOI's strength is its trade-off sweet spot. The
[specification is one page][spec], and an experienced developer can write
a complete implementation from scratch in a single sitting. [My own
implementation is about 100 lines of libc-free C][c] for each of the
encoder and decoder. With error checking removed, my decoder is ~600 bytes
of x86 object code — a great story for embedding alongside assets. It's
more complex than Netpbm or [farbfeld][], but it's far simpler than BMP.
I've already begun [experimenting with converting assets to QOI][chess],
and the results have so far exceeded my expectations.

To my surprise, the encoder was easier to write than the decoder. The
format is so straightforward such that two different encoders will produce
the identical files. There's little room for specialized optimization, and
no meaningful "compression level" knob.

### Criticism

There are a [lot of dimensions][bikeshed] on which QOI could be improved,
but most cases involve trade-offs, e.g. more complexity for better
compression. The areas where QOI could have been strictly better, the
dimensions on which it is not on the Pareto frontier, are more meaningful
criticisms — missed opportunities. My criticisms of this kind:

* Big endian fields are an odd choice for a 2020s file format. Little
  endian dominates the industry, and it would have made for a slightly
  smaller decoder footprint on typical machines today if QOI used little
  endian.

* The header has two flags and spends an entire byte on each. It should
  have instead had a flag byte, with two bits assigned to these flags. One
  flag indicates if the alpha channel is important, and the other selects
  between two color spaces (sRGB, linear). Both flags are only advisory.

* The 4-channel encoded pixel format is ABGR (or RGBA), placing the alpha
  channel next to the blue channel. This is somewhat unconventional. A
  decoder is likely to use a single load into 32-bit integer, and ideally
  it's already in the desired format or close to it. A few times already
  I've had to shuffle the RGB bytes within the 32-bit sample to be
  compatible with some other format. QOI channel ordering is arbitrary,
  and I would have chosen ARGB (when viewed as little endian).

* The QOI hash function operates on channels individually, with individual
  overflow, making it slower and larger than necessary. The hash function
  should have been [over a packed 32-bit sample][hash]. I would have used
  [a multiplication][msi] by a carefully-chosen 32-bit integer, then a
  right shift using the highest 6 bits of the result for the index.

More subjective criticisms that might count as having trade-offs:

* Given a "flag byte" (mentioned above) it would have been free to assign
  another flag bit indicating pre-multiplied alpha, also still advisory.
  [You want to use pre-multiplied alpha][pma] for your assets, and the
  option store them this way would help.

* There's an 8-byte end-of-stream marker — a bit excessive — deliberately
  an invalid encoding so that reads past the end of the image will result
  in a decoding error. I probably would have chosen a dead simple 32-bit
  checksum of packed 32-bit images samples, even if literally a sum.

Of course, you're not obligated to follow QOI exactly to spec for your own
assets, so you could always use a modified QOI with one or more of these
tweaks. That's what I meant about it being a solid foundation: You don't
have to start from scratch with some custom RLE. Since the format is so
simple, you can easily build your own tools — as I've already begun doing
myself — so you don't need to rely on tools supporting your QOI fork.

### Minimalist API

I'm really happy with my QOI implementation, particularly since it's
another example of [a minimalist C API][mini]: no allocating, no input or
output, and no standard library use. As usual, the expectation is that
it's in the same translation unit where it's used, so it's likely inlined
into callers.

The encoder is streaming — it accepts and returns only a little bit of
input and output at a time. It has three functions and one struct with no
"public" fields:

```c
struct qoiencoder qoiencoder(void *buf, int w, int h, const char *flags);
int qoiencode(struct qoiencoder *, void *buf, unsigned color);
int qoifinish(struct qoiencoder *, void *buf);
```

The first function initializes an encoder and writes a fixed-length header
into the QOI buffer. The `flags` field is a mode string, like `fopen`. I
would normally use bit flags, but this is [a little experiment][str]. The
second function encodes a single pixel into the QOI buffer, returning the
number of bytes written (possibly zero). The last flushes any encoding
state and writes the end-of-stream marker. There are no errors. My typical
use so far looks like:

```c
char buf[16];
struct qoiencoder q = qoiencoder(buf, width, height, "a");
fwrite(buf, QOIHDRLEN, 1, file);
for (int y = 0; y < height; y++) {
    for (int x = 0; x < width; x++) {
        // ... compute 32-bit ABGR sample at (x, y) ...
        fwrite(buf, qoiencode(&q, buf, abgr), 1, file);
    }
}
fwrite(buf, qoifinish(&q, buf), 1, file);
fflush(file);
return ferror(file);
```

This appends encoder outputs to a buffered stream, but it could just as
well accumulate directly into a larger buffer, advancing the write pointer
a little after each call.

The decoder is two functions, but its struct has some "public" fields.

```c
struct qoidecoder {
    int width, height;
    _Bool alpha, srgb, error;
    // ...
};
struct qoidecoder qoidecoder(const void *buf, int len);
static unsigned qoidecode(struct qoidecoder *);
```

The input is not streamed and the entire buffer must be loaded into memory
at once — not too bad since it's compressed, and perhaps even already
loaded as part of the executable image — but the output *is* streamed,
delivering one packed 32-bit ABGR sample per call. The decoder makes no
assumptions about the output format, and the caller unpacks samples and
stores them in whatever format is appropriate (shader texture, etc.).

To make it easier to use, my decoder range checks to guarantee that width
and height [can be multiplied without overflow][int]. Unlike encoding,
there may be errors due to invalid input, including that failed range
check. The decoder error flag is "sticky" and the decoder returns zero
samples when in an error state, so callers can wait to check for errors
until the end. (Though if you're only decoding embedded assets, then there
are no practical errors, and checks can be removed/ignored.)

Example usage, copied almost verbatim from a real program:

```c
int loadimage(Image *image, const uint8_t *qoi, int len)
{
    struct qoidecoder q = qoidecoder(qoi, len);
    if (/* image dimensions too large */) {
        return 0;
    }
    image->width  = q.width;
    image->height = q.height;
    int count = q.width * q.height;
    for (int i = 0; i < count; i++) {
        unsigned abgr = qoidecode(&q);
        image->data[4*i+0] = abgr >> 16;
        image->data[4*i+1] = abgr >>  8;
        image->data[4*i+2] = abgr >>  0;
        image->data[4*i+3] = abgr >> 24;
    }
    return !q.error;
}
```

Note the aforementioned awkward RGB shuffle.

It's safe to say that I'm excited about QOI, and that it now has a
permanent slot on my developer toolbelt.


[bikeshed]: https://github.com/nigeltao/qoi2-bikeshed
[c]: https://github.com/skeeto/scratch/blob/master/parsers/qoi.c
[chess]: https://github.com/skeeto/chess/commit/5c123b3
[farbfeld]: https://tools.suckless.org/farbfeld/
[hash]: /blog/2018/07/31/
[hn]: https://news.ycombinator.com/item?id=34035024
[int]: /blog/2017/07/19/
[mini]: /blog/2018/06/10/
[msi]: /blog/2022/08/08/#hash-functions
[pbm]: https://netpbm.sourceforge.net/doc/ppm.html
[pbmview]: https://github.com/skeeto/scratch/tree/master/pbmview
[pma]: https://www.adriancourreges.com/blog/2017/05/09/beware-of-transparent-pixels/
[qoi]: https://qoiformat.org/
[RLE]: https://en.wikipedia.org/wiki/Run-length_encoding
[showcase]: /blog/2020/06/29/
[spec]: https://qoiformat.org/qoi-specification.pdf
[str]: https://flak.tedunangst.com/post/string-interfaces
