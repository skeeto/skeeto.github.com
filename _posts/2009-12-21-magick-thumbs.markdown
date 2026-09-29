---
title: Magick Thumbnails
layout: post
tags: [trick]
uuid: 54f0b0cc-56d2-3260-9bfc-339c1c7fc0fb
---

<!-- 21 December 2009 -->

For a long time I couldn't figure out how to make decent thumbnails
with [ ImageMagick](http://www.imagemagick.org/script/index.php). Specifically, I wanted to create uniform sized
thumbnails from arbitrary images. Over the weekend I came across the
[ ImageMagick Examples page](http://www.imagemagick.org/Usage/thumbnails/#cut), which shows exactly how to do
this. Here's the command for a 150x150 thumbnail,

```
convert orig.jpg -thumbnail 150x150^ -gravity center \
        -extent 150x150 thumb.jpg
```

It cuts out the largest square possible from the center of the image
and resizes that to 150x150. This capability has actually only been
available for 2 years now! It wasn't there last time I needed it.

I can think of one way to improve it: instead of selecting the center,
it selects the area with the highest information density. This could
be measured by edge detection, corner detection, or some other
statistical method. It would be selected by changing the gravity
option to, say, "entropy".

I'm listing this here mostly for my own future reference. :-)
