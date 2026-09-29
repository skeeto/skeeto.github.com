---
title: Introducing Java Mode Plus
layout: post
tags: [java, emacs]
uuid: 5cf6344b-537b-36eb-53d8-6ca6b24fd492
---

<!-- 15 October 2010 -->

There's an extension to Emacs
called [ JDEE](http://jdee.sourceforge.net/) which tries to
turn Emacs into a heavyweight IDE for Java. I've never had any success
with it, and I don't know anyone else who has either. It's difficult
to set up, the dependencies are even worse, poorly documented, and
then it doesn't seem to work very well anyway. I think it's too
divorced from Emacs'
core [ composable](http://en.wikipedia.org/wiki/Composability) functionality to be of much use. I may as well be using
a big IDE.

So, instead, as [I've
posted](/blog/2009/12/06/) [about](/blog/2010/09/30/)
[over time](/blog/2010/10/14/), I've started with the basic
Emacs Java functionality and tweaked my way up from there. I've
extended it enough that I decided to package it up on it's own, and
hopefully others will find it useful too. I call
it `java-mode-plus`!

<pre>
git clone <a href="https://github.com/skeeto/emacs-java">git://github.com/skeeto/emacs-java.git</a>
</pre>

Specifically: [java-mode-plus.el](https://github.com/skeeto/emacs-java/blob/master/java-mode-plus.el)

It provides a hook into `java-mode` that creates a bunch of
new bindings. It also creates some new globally-available functions
like `open-java-project`. It's all heavily Ant-based since
that's what I like to use. It wouldn't be very hard to modify it to
use Maven, if that's what your thing.

**My very thorough documentation is in a large header comment in the
source file itself.** I cover my whole workflow from top to
bottom. If you're interested in making Emacs more Java-friendly take a
look at it. It's not a lot of code, but each line has been
thoughtfully added after hours and hours of Java development.
