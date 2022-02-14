---
title: A new protocol and tool for PNG file attachments
layout: post
date: 2021-12-31T22:17:26Z
tags: [c]
uuid: 30ae498b-881d-428e-97e5-7ea3cc332973
---

When my articles include diagrams to illustrate a concept, such as a
[state machine][sm], I will check the [Graphviz][], [gnuplot][], or SVG
source into source control alongside the image in case I need to make
changes in the future. Sometimes I even make the image itself a link to
its source file. I've thought it would be convenient if the raster image
somehow contained its own source as metadata so that they don't get
separated. I looked around and wasn't satisfied with the solutions I
found, so I wrote one: **[pngattach][]**.

My approach introduces a new private chunk type `atCh` ("attachment")
which contains a file name, a flag to indicate if the attachment is
compressed, and an optionally DEFLATE-compressed blob of file contents. I
tried to follow the spirit of PNG chunk formatting, but without the
constraints I hoped to avoid. A single PNG can contain multiple
attachments, e.g. source file, Makefile, README, license file, etc. The
protocol places constraints on the file names to keep it simple and to
avoid shenanigans: no control bytes (anything below ASCII space), no
directories, and cannot start with a period (no special hidden files). If
that's too constraining, you could attach a ZIP or TAR.

### PNG chunk format

PNG files begin with a fixed 8-byte header followed by of a series of
chunks. Each chunk has an 8-byte header and 4-byte footer. The chunk
header is a 32-bit big endian chunk length (not counting header or footer)
and a 4-byte tag identifying its type. The length allows implementations
to skip chunks it doesn't recognize.

    LLLL TTTT ...chunk... CCCC

The footer is a big endian CRC-32 checksum of the 4-byte type tag and the
chunk body itself.

Chunk tags are interpreted as 4 ASCII characters, where the capitalization
of each letter encodes 4 additional boolean flags. The flags in my tag,
`atCh`, indicate it's a non-critical private chunk which doesn't depend on
the image data.

PNG always ends with a zero-length `IEND` chunk, which works out to a kind
of 12-byte constant footer.

### Existing chunk types

The PNG standard currently defines three kinds of chunks for storing text
metadata: `tEXt`, `iTXt`, `zTXt`. The first is limited to Latin-1 with LF
newlines, and so cannot store UTF-8 source text. The latter two were
introduced in the PNG 1.2 specification (November 1999), and allow (only)
UTF-8 content with LF newlines. All three have a 1 to 79-byte Latin-1
"key" field, and the latter two some additional fields describing the
language of the text.

The key field is null-terminated, making it 80 bytes maximum when treated
as a null-terminated string. I believe this constraint exists to aid
implementations, which can rely on this hard upper limit for the key
lengths they're expected to handle. Otherwise a key could have been up to
4GiB in length.

I had considered using part of the key as a file name, prefixed with a
custom namespace (ex. `attachment:FILENAME`) to distinguish it from other
text chunks. However, I didn't like the constraints this placed on the
file name, plus I wanted to support arbitrary file content, not limited to
a particular subformat.

As prior art, there's a draw.io/diagrams.net format which embeds a source
string without file name. The source string is encoded in base64 (i.e.
unconstrained by PNG), wrapped in XML, then incorrectly encoded as an
`iTXt` chunk. The XML alone was enough to keep me away from using this
format.

### pngattach details

In my attachment protocol, the file name is an arbitrary length,
null-terminated byte string (preferably UTF-8), much like a key field,
with the previously-mentioned anti-shenanigans restrictions. The file name
is followed by a byte, 0 or 1, indicating if the content is compressed
using PNG's officially-supported compression format. The rest is the
arbitrary content bytes, which presumably the recipient will know how to
use.

    LLLL atCh example.txt 0 F ...contents... CCCC

I expect any experienced programmer could write a basic attachment
extractor in their language of choice inside of 30 or so minutes. Hooking
up a DEFLATE library for decompression would be the most difficult part.

Since it supports multiple attachments and behaves like an archive format,
my tool supports flags much like `tar`: `-c` to create attachments
(default and implicit), `-t` to list attachments, and `-x` to extract
attachments. PNG data is always passed on standard input and standard
output.

For example, to render a Graphviz diagram and attach the source all at
once:

    $ dot -Tpng graph.dot | pngattach graph.dot >graph.png

Later on someone might extract it and tweak it, like so (`-v` verbose,
lists files as they're extracted, like `tar`):

    $ pngattach -xv <graph.png
    graph.dot
    $ vi graph.dot
    $ dot -Tpng graph.dot >graph.png

Like `tar`, it can also write attachments to standard output with `-O`.
For example, to re-render the image as an SVG:

    $ pngattach -xO <graph.png | dot -Tsvg >graph.svg

Strictly processing standard input to output, rather than taking the input
as an argument, is something I've been trying lately. I'm pretty happy
with my [command line design][cmd] for `pngattach`. The real test will
happen in the future, when I've forgotten the details and have to figure
it out again from my own documentation.

Curiously, lots of common software refuses to handle PNGs containing large
chunks, and so your PNG may not display if you attach a file even as small
as a few MiB. A defense against denial of service?

### Example PNG

I haven't gone back and embedded attachments in any older articles, but I
may do so in future articles. If you wanted to try it out for yourself,
either with my tool or writing your own for fun, this PNG contains a
compressed attachment:

![](/img/atch-test.png)

I produced it like so (with the help of [ImageMagick][]):

    $ echo P3 1 1 1 0 1 0 |
          convert ppm:- resize 200 png:- |
          pngattach message.txt >atch-test.png

### Error handling (addendum)

Another technique I've been trying is Go-style error value returns in C
programs, where the errors-as-values are `const char *` pointers to static
string buffers. The contents contain an error message to be displayed to
the user, and errors may be wrapped in more context (what file, what
operation, etc.) as the stack unwinds. A null pointer means no error, i.e.
nil. I've used this extensively in `pngattach`. Examples of the style:

```c
    int *p;

    if (nelem > (size_t)-1/sizeof(*p)) {
        return "out of memory";  // overflow
    }

    p = malloc(nelem*sizeof(*p));
    if (!p) {
        return "out of memory";
    }

    // ...

    if (!fwrite(buf, len, 1, stdout)) {
        free(p);
        return "write error";
    }
```

An `errwrap()` function builds a new error string in a static buffer. This
simple solution wouldn't work in a multi-threaded program, but that's not
the case here. Mine toggles between two static buffers so that it can wrap
recursively.

```c
const char *
errwrap(const char *pre, const char *post)
{
    static char errtmp[2][256], i;
    int n = i = !i;  // toggle between two static buffers
    snprintf(errtmp[n], sizeof(errtmp[n]), "%s: %s", pre, post);
    return errtmp[n];
}
```

Then I can do stuff like:

```c
    FILE *f = fopen(path, "wb");
    if (!f) {
        return errwrap("failed to open file", path);
    }
```

And that can keep being wrapped on the way up:

```c
    err = png_write(path);
    if (err) {
        return errwrap("writing PNG", err);
    }
```

So that ultimately the user sees something like:

    pngattach: writing PNG: failed to open file: example.png

That's always printed by a single error printout block at the top level,
where all errors are ultimately routed.

```c
int main(int argc, char **argv)
{
    // ...

    err = run(options);
    if (err) {
        fprintf(stderr, "pngattach: %s\n", err);
        return 1;
    }
    return 0;
}
```


[Graphviz]: https://graphviz.org/
[ImageMagick]: https://www.imagemagick.org/
[cmd]: /blog/2020/08/01/
[gnuplot]: http://www.gnuplot.info/
[pngattach]: https://github.com/skeeto/scratch/tree/master/pngattach
[sm]: /blog/2020/12/31/
