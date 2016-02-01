---
title: YouTube with Free Software
layout: post
tags: [rant]
uuid: a7994b60-4be3-3f07-8dbd-96e2a3ed0908
---

**Update 2009-6-30**: *Thanks to HTML 5 and the `video` tag I will be
self-hosting videos from now on. This is information is only
historical.*

As I have stated previously, I love [free software][fsf] and I try to
use free software exclusively whenever I can (it is very difficult to
find employment in computer engineering where no proprietary software
is used). This can pose a problem when I want to watch
[YouTube][youtube] videos because I do not use the proprietary,
non-free Flash player. The free Flash players currently either handle
YouTube poorly or not at all. I also find Flash annoying enough that I
am not interested in using these free Flash players anyway (fewer ads
automatically!).

Like everyone else with an e-mail address, I get links to videos on
YouTube from my friends. I also post videos there myself under the
name "throwaway0" as it is convenient not only for me, but also for
anyone who wants to watch the videos. Now, if the only way to watch
these videos was with proprietary software, I would not encourage
this. In fact, not too long ago this was true of most online video,
which was limited to a "choose your poison" type situation between the
proprietary, worthless Windows Media and QuickTime formats. No poison
for me, thanks.

I have discovered several solutions to watching YouTube with free
software. There are two steps involved: getting the video and playing
the video. For the first you have [youtube-dl][youtube-dl] <del>and
then you have [Firefox][firefox] [Fast Video Download][fvd]</del>.

youtube-dl is a Python script that you can easily install on your
system. You just give it a YouTube URL and it does all the work. It
feels a bit like wget. The Firefox add-on will add a nice little icon
like this,

![](/img/fast-video-download.png)

Clicking this icon will download the video. With either tool, you will
have this .flv file somewhere that you want to watch.

To watch the video file, you can use [mplayer][mplayer] or [VLC][vlc].
As far as I know, these videos are handled with free software only
when using these players. They play fine (except without seeking) on
my system. I use [Debian GNU/Linux][debian] so I am pretty confident
that my system is strictly free software.

Now you can watch YouTube without having to fall victim to proprietary
software.


[fsf]: http://www.fsf.org/
[youtube]: http://www.youtube.com/
[youtube-dl]: http://rg3.github.io/youtube-dl/
[firefox]: http://www.mozilla.com/en-US/firefox/
[fvd]: https://addons.mozilla.org/en-US/firefox/addon/3590
[vlc]: http://www.videolan.org/vlc/
[mplayer]: http://www.mplayerhq.hu/
[debian]: http://www.debian.org/
