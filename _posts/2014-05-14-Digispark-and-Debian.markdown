---
title: Digispark and Debian
layout: post
date: 2014-05-14T17:57:31Z
tags: [meatspace, tutorial]
uuid: 154af36e-272c-3e4d-d1f9-91341ec65b5a
---

Following [Brian's][brian] lead, I recently picked up a couple of
[Digispark USB development boards][digispark]. It's a cheap, tiny,
Arduino-like microcontroller. There are a couple of interesting
project ideas that I have in mind for these. It's [been over 6
years][robot] since I last hacked on a microcontroller.

![](/img/misc/digispark-small.jpg)

Unfortunately, support for the Digispark on Linux is spotty. Just as
with any hardware project, the details are irreversibly messy. It
can't make use of the standard Arduino software for programming the
board, so you have to download a customized toolchain. This download
includes files that have the incorrect vendor ID, requiring a manual
fix. Worse, [the fix listed in their documentation][ts] is incomplete,
at least for Debian and Debian-derived systems.

The main problem is that Linux will *not* automatically create a
`/dev/ttyACM0` device like it normally does for Arduino devices.
Instead it gets a long, hidden, unpredictable device name. The fix is
to ask udev to give it a predictable name by appending the following
to the first line in the provided udev rules file (`49-micronucleus.rules`),

    SYMLINK+="ttyACM%n"

The whole uncommented portion of the rules file should look like this:

* [49-micronucleus.rules][paste] (pastebin since it's a long line)

The `==` is a conditional operator, indicating that the rule only
applies when the condition is met. The `:=` and `+=` are assignment
operators, evaluated when all of the conditions are met. The `SYMLINK`
part tells udev put a softlink to the device in `/dev` under a
predictable name.


[brian]: http://www.50ply.com/
[digispark]: http://digistump.com/products/1
[ts]: http://digistump.com/wiki/digispark/tutorials/linuxtroubleshooting
[paste]: http://pastebin.com/2XxmvEaS
[robot]: /blog/2008/02/04/
