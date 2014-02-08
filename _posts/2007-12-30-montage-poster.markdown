---
title: Movie Montage Poster
layout: post
tags: [meatspace]
uuid: ff6143e3-0521-3016-422c-9fc8c04cbcef
---

I wanted to try making one of those movie montage things I
[wrote about earlier][prev] into a nice poster that could be hung on a
wall. Now, I prefer a Spartan environment whenever possible, so *I*
really did not want to have my own poster. No decorations for me,
please. I just wanted to make one. My solution? Make one for my
sister, who has lots of junk and would enjoy having one. Her favorite
movie is *Pirates of the Caribbean*, so I used this movie, which she
conveniently already had on DVD.

As before, I used mplayer to rip all of the frames I needed. To get a
poster-quality version I would need better resolution. To do this, I
changed the frame output image size to 160x90 (100 times bigger than
before).

    framestep=30,scale=160:90

Next, I used my [own montage script][script] to put these frames
together. My script took about 2 minutes to put together one of these
larger montages. Finally, I used the Gimp to add a black border and
simple title at the top. Since I don't have any more than 512MB of
memory on my computers, I actually had to scale the image down to 1/4
of its original size to do this. Before scaling, the GIMP was spending
hours just adding the border because it was thrashing the hard drive.
It needed about 2G of memory and it was using the hard drive to get
it.

I took my giant image to FedEx Kinko's where they printed it to a
2'x2' poster for 30 bucks. Here are the results, taken using my
sister's crappy Kodak camera (never buy Kodak digital cameras, as they
all suck!). To help see what is going on, I provided a glare version
and a non-glare version. Each shows different details.

[![](/img/cinrdx/poster-glare-thumb.jpg)](/img/cinrdx/poster-glare.jpg)

[![](/img/cinrdx/poster-noglare-thumb.jpg)](/img/cinrdx/poster-noglare.jpg)

Here is the normal version from before for comparison.

[![](/img/cinrdx/pirates-thumb.jpg)](/img/cinrdx/pirates.jpg)


[prev]: /blog/2007/12/11
[script]: /blog/2007/12/26
