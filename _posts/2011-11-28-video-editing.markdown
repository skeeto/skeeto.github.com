---
title: Poor Man's Video Editing
layout: post
---

I've done all my video editing in a very old-school, unix-style way. I
actually have no experience with real video editing software, which
may explain why I tolerate the manual process. Instead, I use several
open source tools, none of which are designed specifically for video
editing.

* [MPlayer](http://www.mplayerhq.hu/)
* [ImageMagick](http://www.imagemagick.org/) (or any batch image editing tool)
* [ppmtoy4m](http://mjpeg.sourceforge.net/)
* The [WebM encoder](http://www.webmproject.org/) (or your preferred encoder)

The first three are usually available from your Linux distribution
repositories, making them trivial to obtain. The last one is easy to
obtain and compile.

If you're using a modern browser, you should have noticed my portrait
on the left-hand side changed recently. That's an HTML5 WebM video --
currently with Ogg Theora fallback due to a GitHub issue. To cut the
video down to that portrait size, I used the above four tools on the
original video.

WebM seems to be becoming the standard HTML5 video format. Google is
pushing it and it's supported by all the major browsers, except
Safari. So, unless something big happens, I plan on going with WebM
for web video in the future.

To begin, [as I've done before](/blog/2007/12/11/), split the video
into its individual frames,

    mplayer -vo jpeg -ao dummy -benchmark video_file

The `-benchmark` option hints for `mplayer` to go as fast as possible,
rather than normal playback speed.

Next look through the output frames and delete any unwanted frames to
keep, such as the first and last few seconds of video. With the
desired frames remaining, use ImageMagick, or any batch image editing
software, to crop out the relevant section of the images. This can be
done in parallel with `xargs`' `-P` option -- to take advantage of
multiple cores if disk I/O isn't being the bottleneck.

    ls *.jpg | xargs -I{} -P5 convert {} 312x459+177+22 {}.ppm

That crops out a 312 by 459 section of the image, with the top-left
corner at (177, 22). Any other `convert` filters can be stuck in there
too. Notice the output format is the
[portable pixmap](http://en.wikipedia.org/wiki/Netpbm_format) (`ppm`),
which is significant because it won't introduce any additional loss
and, most importantly, it is required by the next tool.

If I'm happy with the result, I use `ppmtoy4m` to pipe the new frames
to the encoder,

    cat *.ppm | ppmtoy4m | vpxenc --best -o output.webm -

As the name implies, `ppmtoy4m` converts a series of portable pixmap
files into a
[YUV4MPEG2](http://wiki.multimedia.cx/index.php?title=YUV4MPEG2)
(`y4m`) video stream. YUV4MPEG2 is the bitmap of the video world:
gigantic, lossless, uncompressed video. It's exactly the kind of thing
you want to hand to a video encoder. If you need to specify any
video-specific parameters, `ppmtoy4m` is the tool that needs to know
it. For example, to set the framerate to 10 FPS,

    ... | ppmtoy4m -F 10:1 | ...

`ppmtoy4m` is a classically-trained unix tool: stdin to stdout. No
need to dump that raw video to disk, just pipe it right into the WebM
encoder. If you choose a different encoder, it might not support
reading from stdin, especially if you do multiple passes. A possible
workaround would be a named pipe,

    mkfifo video.y4m
    cat *.ppm | ppmtoy4m > video.y4m &
    otherencoder video.4pm

I have attempted to do two passes with the WebM encoder
(`--passes=2`), but there's always been a floating-point bug that
prevents it. For now I just use the `--best` option, telling the
encoder to takes its time to do a good job.

To produce Ogg Theora instead of WebM,
[ffmpeg2theora](http://v2v.cc/~j/ffmpeg2theora/) is a great tool. It's
well-behaved on the command line and can be dropped in place of
`vpxenc`.

If you want to do audio *and* video ... that's a lot more work. I've
never done it myself, but how I suspect I'd handle it is with
[Audacity](http://audacity.sourceforge.net/) and lots of
synchronization trial-and-error.
