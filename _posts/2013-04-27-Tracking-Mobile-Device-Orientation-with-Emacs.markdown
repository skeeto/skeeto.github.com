---
title: Tracking Mobile Device Orientation with Emacs
layout: post
tags: [emacs, javascript, web]
uuid: 3e015231-d0f9-3d53-72a1-ec7d4a30c941
---

Nine years ago I bought my first laptop computer. For the first time I
could carry my computer around and do productive things at places
beyond my desk. In the meantime a new paradigm of mobile computing has
arrived. Following a similar pattern, this month I bought a Samsung
Galaxy Note 10.1, an Android tablet computer. Having never owned a
smartphone, this is my first taste of modern mobile computing.

[![](/img/misc/tablet-thumb.jpg)](/img/misc/tablet.jpg)

Once the technology caught up, laptops were capable enough to fully
replace desktops. However, this tablet is no replacement for my
laptop. [Mobile devices are purely for consumption][luke], so I will
continue to use desktops and laptops for the majority of my computing.
I'm writing this post on my laptop, not my tablet, for example.

Owning a tablet has opened up a whole new platform for me to explore
as a programmer. I'm not particularly interested in writing Android
apps, though. I'm obviously not alone in this, as I've found that
nearly all Android software available right now is somewhere between
poor and mediocre in quality. The hardware was worth the cost of the
device, but the software still has a long way to go. I'm optimistic
about this so I have no regrets.

### A New Web Platform

Instead, I'm interested in mobile devices as a web platform. One of
the few high-quality pieces of software on Android are the web
browsers (Chrome and Firefox), and I'm already familiar with
developing for these. Even more, I can develop software live on the
tablet remotely from my laptop using [Skewer](/blog/2012/10/31/) —
i.e. the exact same development tools and workflow I'm already using.

What's new and challenging is the user interface. Instead of
traditional clicking and typing, mobile users tap, hold, swipe, and
even tilt the screen. Most challenging of all is probably
accommodating both kinds of interfaces at once.

One of the first things I wanted to play with after buying the tablet
was the gyro. The tablet knows its acceleration and orientation at all
times. This information can be accessed in JavaScript using
[a fairly new API][spec]. The two events of interest are
`ondevicemotion` and `ondeviceorientation`. Using
[simple-httpd](/blog/2012/08/20/) I can transmit all this information
to Emacs as it arrives.

Instead of writing a new servlet for this, to try it out I used
`skewer.log()`. Connect a web page viewed on the tablet to Skewer
hosted on the laptop, then evaluate this in a `js2-mode` buffer on the
laptop.

~~~javascript
window.addEventListener('devicemotion', function(event) {
    var a = event.accelerationIncludingGravity;
    skewer.log([a.x, a.y, a.z]);
});
~~~

Or for orientation,

~~~javascript
window.addEventListener('deviceorientation', function(event) {
    skewer.log([event.alpha, event.beta, event.gamma]);
});
~~~

These orientation values appeared in my `*skewer-repl*` buffer as I
casually rolled the tablet on one axis. The units are obviously
degrees.

    [157.4155398727678, 0.38583511837777246, -44.61023992234689]
    [155.4477623728871, -0.6438986350040569, -44.69645057005079]
    [154.32208572596647, -0.7516393196323073, -45.79730289443301]
    [155.437674183483, -0.48375529832044045, -46.406449900466015]
    [156.2974174150692, 0.21938214098430556, -47.482812581579154]
    [154.85869270791937, 0.11046702400456986, -48.67378583696511]
    [153.3284161451347, -0.9344782009891125, -48.61755630462298]
    [154.11860073021347, -0.6553947505116874, -49.949668589018074]
    [155.85919247792117, 0.05473832995756562, -49.84400214746339]
    [156.92487274317241, 0.4946305069438346, -49.86369016774595]
    [158.06542554210534, 0.712759801803332, -49.61875275392013]
    [159.356905031128, 1.3387109941852697, -49.9372717956745]

It would be neat to pump these into a 3D plot display as they come in,
such that my laptop displays the current tablet orientation on the
screen as I move it around, but I didn't see any quick way to do this.

Here are some acceleration values at rest. Since I took these samples
on Earth the units are obviously in meters per second per second.

    [-0.009576806798577309, 0.31603461503982544, 9.816226959228516]
    [-0.047884032130241394, 0.3064578175544739, 9.806650161743164]
    [-0.009576806798577309, 0.28730419278144836, 9.787496566772461]
    [0.009576806798577309, 0.3064578175544739, 9.816226959228516]
    [-0.06703764945268631, 0.3256114423274994, 9.797073364257812]
    [-0.047884032130241394, 0.2968810200691223, 9.864110946655273]
    [-0.028730420395731926, 0.2968810200691223, 9.576807022094727]
    [-0.019153613597154617, 0.363918662071228, 9.691728591918945]
    [-0.05746084079146385, 0.3734954595565796, 10.199298858642578]

Now that I have the hardware for it, I really want to use this API to
do something interesting in a web application. I just don't have any
specific ideas yet.


[luke]: http://www.terminally-incoherent.com/blog/2012/06/13/ipad/
[spec]: http://dev.w3.org/geo/api/spec-source-orientation.html
