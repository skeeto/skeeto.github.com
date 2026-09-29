---
title: Lossless Optimizers
layout: post
tags: [compression]
uuid: 644ecd27-0f43-358b-e405-9553f79ae7e1
---

<!-- 23 August 2009 -->

I've been using lossless optimizers for awhile now for PNGs, but more
recently I have found some for other formats. Here's the ones I know
about. These are all intended to be lossless, so running them should
result in no information loss (well, except the SVG one).

For **PNG**, there are a number of choices, but my favorite is [OptiPNG](http://optipng.sourceforge.net/). It adjust the PNG
parameters and recompresses to find the optimal parameters. I run it
on almost all my images around here, and I tend to get around 10% to
30% reduction for images fresh off [ Gimp](http://www.gimp.org/), [ Kolourpaint](http://kolourpaint.sourceforge.net/), and [ ImageMagick](http://imagemagick.org/).

For **JPEG**, I use [ jpegoptim](http://freshmeat.net/projects/jpegoptim/). It
works by optimizing the Huffman tables (the lossless part of JPEG
compression). I only found this one recently, but I will be using it
all the time, like on our new thousands of wedding reception photos.

For **PDF**, I found something called [ QPDF](http://qpdf.sourceforge.net/). It's designed more for
other PDF transformations, but without any other parameters it will
simply losslessly optimize a PDF. From what I've seen so far it cuts
PDFs down by about a third.

For **SVG**, [Scour](http://codedread.com/scour/) is a
young project, only a few months old. I've been looking for an SVG
optimizer for some time, so this was exciting to find. Due to the type
of file it's working with, it's not quite entirely lossless. Visually,
it is lossless, but it will toss all metadata (comments, etc.), which
may be important. If you hand-crafted your SVG, you won't want to use
this tool. It's good for removing [ Inkscape](http://inkscape.org/) and Illustrator cruft, though.

I have yet to find a good (Free) GIF optimizer. Animated GIFs, with
lots of redundancy between frames, have a lot of potential for
optimization too. A video optimizer (for, say, Theora) would be
interesting; I imagine it might work similarly to jpegoptim. Audio
files (like Vorbis, FLAC, or MP3) probably don't have any room for
optimization. I could be wrong. For XHTML there is [tidy](http://www.w3.org/People/Raggett/tidy/) if you want to
count that. All the other XML formats (ODF, RSS, etc.) could have
their own too. Or optimizers for archives, like zip and tar. For tar
it might rearrange things to better suit gzip, bzip2, or
lzma. Executable optimizers? Postscript optimizers? It goes on and on.

If you know about any more, especially for other file formats, let me
know.
