---
title: Rolling Shutter Simulation in C
layout: post
date: 2017-07-02T18:35:16Z
tags: [c, media, tutorial, trick]
---

The most recent [Smarter Every Day (#172)][sed] explains a phenomenon
that results from *rolling shutter*. You've likely seen this effect in
some of your own digital photographs. When a CMOS digital camera
captures a picture, it reads one row of the sensor at a time. If the
subject of the picture is a fast-moving object (relative to the
camera), then the subject will change significantly while the image is
being captured, giving strange, unreal results:

[![][rst]][rs]

In the *Smarter Every Day* video, Destin illustrates the effect by
simulating rolling shutter using a short video clip. In each frame of
the video, a few additional rows are locked in place, showing the
effect in slow motion, making it easier to understand.

<video src="https://nullprogram.s3.amazonaws.com/rolling-shutter/rolling-shutter-5.mp4"
       width="500" height="500"
       loop="loop" controls="controls" autoplay="autoplay">
</video>

At the end of the video he thanks a friend for figuring out how to get
After Effects to simulate rolling shutter. After thinking about this
for a moment, I figured I could easily accomplish this myself with
just a bit of C, without any libraries. The video above this paragraph
is the result.

I [previously described a technique][poor] to edit and manipulate
video without any formal video editing tools. A unix pipeline is
sufficient for doing minor video editing, especially without sound.
The program at the front of the pipe decodes the video into a raw,
uncompressed format, such as YUV4MPEG or [PPM][netpbm]. The tools in
the middle losslessly manipulate this data to achieve the desired
effect (watermark, scaling, etc.). Finally, the tool at the end
encodes the video into a standard format.

    $ decode video.mp4 | xform-a | xform-b | encode out.mp4

For the "decode" program I'll be using ffmpeg now that it's [back in
the Debian repositories][deb]. You can throw a video in virtually any
format at it and it will write PPM frames to standard output. For the
encoder I'll be using the `x264` command line program, though ffmpeg
could handle this part as well. Without any filters in the middle,
this example will just re-encode a video:

    $ ffmpeg -i input.mp4 -f image2pipe -vcodec ppm pipe:1 | \
        x264 -o output.mp4 /dev/stdin

The filter tools in the middle only need to read and write in the raw
image format. They're a little bit like shaders, and they're easy to
write. In this case, I'll write C program that simulates rolling
shutter. The filter could be written in any language that can read and
write binary data from standard input to standard output.

*Update*: It appears that input PPM streams are a rather recent
feature of libavformat (a.k.a lavf, used by `x264`). Support for PPM
input first appeared in libavformat 3.1 (released June 26th, 2016). If
you're using an older version of libavformat, you'll need to stick
`ppmtoy4m` in front of `x264` in the processing pipeline.

    $ ffmpeg -i input.mp4 -f image2pipe -vcodec ppm pipe:1 | \
        ppmtoy4m | \
        x264 -o output.mp4 /dev/stdin

### Video filtering in C

In the past, my go to for raw video data has been loose PPM frames and
YUV4MPEG streams (via `ppmtoy4m`). Fortunately, over the years a lot
of tools have gained the ability to manipulate streams of PPM images,
which is a much more convenient format. Despite being raw video data,
YUV4MPEG is still a fairly complex format with lots of options and
annoying colorspace concerns. [PPM is simple RGB][ppm] without
complications. The header is just text:

    P6
    <width> <height>
    <maxdepth>
    <width * height * 3 binary RGB data>

The maximum depth is virtually always 255. A smaller value reduces the
image's dynamic range without reducing the size. A larger value involves
byte-order issues (endian). For video frame data, the file will
typically look like:

    P6
    1920 1080
    255
    <frame RGB>

Unfortunately the format is actually a little more flexible than this.
Except for the new line (LF, 0x0A) after the maximum depth, the
whitespace is arbitrary and comments starting with `#` are permitted.
Since the tools I'm using won't produce comments, I'm going to ignore
that detail. I'll also assume the maximum depth is always 255.

Here's the structure I used to represent a PPM image, just one frame
of video. I'm using a *flexible array member* to pack the data at the
end of the structure.

~~~c
struct frame {
    size_t width;
    size_t height;
    unsigned char data[];
};
~~~

Next a function to allocate a frame:

~~~c
static struct frame *
frame_create(size_t width, size_t height)
{
    struct frame *f = malloc(sizeof(*f) + width * height * 3);
    f->width = width;
    f->height = height;
    return f;
}
~~~

We'll need a way to write the frames we've created.

~~~c
static void
frame_write(struct frame *f)
{
    printf("P6\n%zu %zu\n255\n", f->width, f->height);
    fwrite(f->data, f->width * f->height, 3, stdout);
}
~~~

Finally, a function to read a frame, reusing an existing buffer if
possible. The most complex part of the whole program is just parsing
the PPM header. The `%*c` in the `scanf()` specifically consumes the
line feed immediately following the maximum depth.

~~~c
static struct frame *
frame_read(struct frame *f)
{
    size_t width, height;
    if (scanf("P6 %zu%zu%*d%*c", &width, &height) < 2) {
        free(f);
        return 0;
    }
    if (!f || f->width != width || f->height != height) {
        free(f);
        f = frame_create(width, height);
    }
    fread(f->data, width * height, 3, stdin);
    return f;
}
~~~

Since this program will only be part of a pipeline, I'm not worried
about checking the results of `fwrite()` and `fread()`. The process
will be killed by the shell if something goes wrong with the pipes.
However, if we're out of video data and get an EOF, `scanf()` will
fail, indicating the EOF, which is normal and can be handled cleanly.

#### An identity filter

That's all the infrastructure we need to built an identity filter that
passes frames through unchanged:

~~~c
int main(void)
{
    struct frame *frame = 0;
    while ((frame = frame_read(frame)))
        frame_write(frame);
}
~~~

Processing a frame is just matter of adding some stuff to the body of
the `while` loop.

#### A rolling shutter filter

For the rolling shutter filter, in addition to the input frame we need
an image to hold the result of the rolling shutter. Each input frame
will be copied into the rolling shutter frame, but a little less will be
copied from each frame, locking a little bit more of the image in place.

~~~c
int
main(void)
{
    int shutter_step = 3;
    size_t shutter = 0;
    struct frame *f = frame_read(0);
    struct frame *out = frame_create(f->width, f->height);
    while (shutter < f->height && (f = frame_read(f))) {
        size_t offset = shutter * f->width * 3;
        size_t length = f->height * f->width * 3 - offset;
        memcpy(out->data + offset, f->data + offset, length);
        frame_write(out);
        shutter += shutter_step;
    }
    free(out);
    free(f);
}
~~~

The `shutter_step` controls how many rows are capture per frame of
video. Generally capturing one row per frame is too slow for the
simulation. For a 1080p video, that's 1,080 frames for the entire
simulation: 18 seconds at 60 FPS or 36 seconds at 30 FPS. If this
program were to accept command line arguments, controlling the shutter
rate would be one of the options.

Putting it all together:

    $ ffmpeg -i input.mp4 -f image2pipe -vcodec ppm pipe:1 | \
        ./rolling-shutter | \
        x264 -o output.mp4 /dev/stdin

Here are some of the results for different shutter rates: 1, 3, 5, 8,
10, and 15 rows per frame. Feel free to right-click and "View Video"
to see the full resolution video.

<div class="grid">
<video src="https://nullprogram.s3.amazonaws.com/rolling-shutter/rolling-shutter-1.mp4"
       width="300" height="300"
       controls="controls">
</video>
<video src="https://nullprogram.s3.amazonaws.com/rolling-shutter/rolling-shutter-3.mp4"
       width="300" height="300"
       controls="controls">
</video>
<video src="https://nullprogram.s3.amazonaws.com/rolling-shutter/rolling-shutter-5.mp4"
       width="300" height="300"
       controls="controls">
</video>
<video src="https://nullprogram.s3.amazonaws.com/rolling-shutter/rolling-shutter-8.mp4"
       width="300" height="300"
       controls="controls">
</video>
<video src="https://nullprogram.s3.amazonaws.com/rolling-shutter/rolling-shutter-10.mp4"
       width="300" height="300"
       controls="controls">
</video>
<video src="https://nullprogram.s3.amazonaws.com/rolling-shutter/rolling-shutter-15.mp4"
       width="300" height="300"
       controls="controls">
</video>
</div>

### Source and original input

This post contains the full source in parts, but here it is all together:

* [rshutter.c](/download/rshutter.c){: .download}

Here's the original video, filmed by my wife using her Nikon D5500, in
case you want to try it for yourself:

<video src="https://nullprogram.s3.amazonaws.com/rolling-shutter/original.mp4"
       width="300" height="300" controls="controls">
</video>

It took much longer to figure out the string-pulling contraption to
slowly spin the fan at a constant rate than it took to write the C
filter program.

### Followup Links

On Hacker News, [morecoffee shared a video of the second order
effect][hnvid] ([direct link][dirvid]), where the rolling shutter
speed changes over time.

A deeper analysis of rolling shutter: [*Playing detective with rolling
shutter photos*][analysis].


[analysis]: http://danielwalsh.tumblr.com/post/54400376441/playing-detective-with-rolling-shutter-photos
[deb]: https://lwn.net/Articles/650816/
[dirvid]: http://antidom.com/fan.webm
[hnvid]: https://news.ycombinator.com/item?id=14684793
[netpbm]: https://en.wikipedia.org/wiki/Netpbm_format
[poor]: /blog/2011/11/28/
[ppm]: http://netpbm.sourceforge.net/doc/ppm.html
[rs]: /img/rolling-shutter/rolling-shutter.jpg
[rst]: /img/rolling-shutter/rolling-shutter-thumb.jpg
[sed]: https://www.youtube.com/watch?v=dNVtMmLlnoE
