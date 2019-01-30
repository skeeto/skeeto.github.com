---
title: Making Your Own GIF Image Macros
layout: post
tags: [media, video, tutorial, reddit]
uuid: dc4ca81c-6c35-33f6-58c5-a77a645f3fbf
---

This tutorial is very similar to my [video editing tutorial][vtut].
That's because the process is the same up until the encoding stage,
where I encode to GIF rather than WebM.

So you want to make your own animated GIFs from a video clip? Well,
it's a pretty easy process that can be done almost entirely from the
command line. I'm going to show you how to turn the clip into a GIF
and add an image macro overlay. Like this,

![](https://s3.amazonaws.com/nullprogram/calvin/calvin-macro.gif)

The key tool here is going to be Gifsicle, a very excellent
command-line tool for creating and manipulating GIF images. So, the
full list of tools is,

  * [MPlayer](http://www.mplayerhq.hu/)
  * [ImageMagick](http://www.imagemagick.org/)
  * [GIMP](http://www.gimp.org/)
  * [Gifsicle](http://www.lcdf.org/gifsicle/)

Here's the source video for the tutorial. It's an awkward video my
wife took of our confused cats, Calvin and Rocc.

<video src="https://s3.amazonaws.com/nullprogram/calvin/calvin-dummy.webm"
       width="480" height="360" controls="controls">
</video>

My goal is to cut after Calvin looks at the camera, before he looks
away. From roughly 3 seconds to 23 seconds. I'll have mplayer give me
the frames as JPEG images.

    mplayer -vo jpeg -ss 3 -endpos 23 -benchmark calvin-dummy.webm

This tells mplayer to output JPEG frames between 3 and 23 seconds,
doing it as fast as it can (`-benchmark`). This output almost 800
images. Next I look through the frames and delete the extra images at
the beginning and end that I don't want to keep. I'm also going to
throw away the even numbered frames, since GIFs can't have such a high
framerate in practice.

    rm *[0,2,4,6,8].jpg

There's also dead space around the cats in the image that I want to
crop. Looking at one of the frames in GIMP, I've determined this is a
450 by 340 box, with the top-left corner at (136, 70). We'll need
this information for ImageMagick.

Gifsicle only knows how to work with GIFs, so we need to batch convert
these frames with ImageMagick's `convert`. This is where we need the
crop dimensions from above, which is given in ImageMagick's notation.

    ls *.jpg | xargs -I{} -P4 \
        convert {} -crop 450x340+136+70 +repage -resize 300 {}.gif

This will do four images at a time in parallel. The `+repage` is
necessary because ImageMagick keeps track of the original image
"canvas", and it will simply drop the section of the image we don't
want rather than completely crop it away. The repage forces it to
resize the canvas as well. I'm also scaling it down slightly to save
on the final file size.

We have our GIF frames, so we're almost there! Next, we ask Gifsicle
to compile an animated GIF.

    gifsicle --loop --delay 5 --dither --colors 32 -O2 *.gif > ../out.gif

I've found that using 32 colors and dithering the image gives very
nice results at a reasonable file size. Dithering adds noise to the
image to remove the banding that occurs with small color palettes.
I've also instructed it to optimize the GIF as fully as it can
(`-O2`). If you're just experimenting and want Gifsicle to go faster,
turning off dithering goes a long way, followed by disabling
optimization.

The delay of 5 gives us the 15-ish frames-per-second we want — since
we cut half the frames from a 30 frames-per-second source video. We
also want to loop indefinitely.

![](https://s3.amazonaws.com/nullprogram/calvin/calvin-dummy.gif)

The result is this 6.7 MB GIF. A little large, but good enough. It's
basically what I was going for. Next we add some macro text.

In GIMP, make a new image with the same dimensions of the GIF frames,
with a transparent background.

![](/img/gif-tutorial/blank.png)

Add your macro text in white, in the Impact Condensed font.

![](/img/gif-tutorial/text1.png)

Right click the text layer and select "Alpha to Selection," then under
Select, grow the selection by a few pixels — 3 in this case.

![](/img/gif-tutorial/text2.png)

Select the background layer and fill the selection with black, giving
a black border to the text.

![](/img/gif-tutorial/text3.png)

Save this image as text.png, for our text overlay.

![](/img/gif-tutorial/text.png)

Time to go back and redo the frames, overlaying the text this
time. This is called compositing and ImageMagick can do it without
breaking a sweat. To composite two images is simple.

    convert base.png top.png -composite out.png

List the image to go on top, then use the `-composite` flag, and it's
placed over top of the base image. In my case, I actually don't want
the text to appear until Calvin, the orange cat, faces the camera.
This happens quite conveniently at just about frame 500, so I'm only
going to redo those frames.

    ls 000005*.jpg | xargs -I{} -P4 \
        convert {} -crop 450x340+136+70 +repage \
                   -resize 300 text.png -composite {}.gif

Run Gifsicle again and this 6.2 MB image is the result. The text
overlay compresses better, so it's a tiny bit smaller.

![](https://s3.amazonaws.com/nullprogram/calvin/calvin-macro.gif)

Now it's time to [post it on reddit][reddit1] and
[reap that tasty, tasty karma][reddit2].
([Over 400,000 views!][imgur])


[vtut]: /blog/2011/11/28/
[reddit1]: http://old.reddit.com/r/funny/comments/s481d/
[reddit2]: http://old.reddit.com/r/lolcats/comments/s47qa/
[imgur]: http://imgur.com/2WhBf
