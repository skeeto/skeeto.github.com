---
title: Java Hot Code Replacement
layout: post
tags: [java]
uuid: fa8ed0d0-5edc-3f0b-a0bc-f6dce98378f7
---

I finally started taking advantage of a JVM feature that's been around
for almost a decade: hot code replacement. HCR was introduced in 1.4
as part of the Java Platform Debugger Architecture (JPDA). It provides
the ability for code to be updated seamlessly in a running Java
program. This can really cut down on the development cycle because the
program doesn't need to be started over from the beginning for every
little change.

This is not actually a new concept or development style for
me. Updating live, running code is fundamentally how Lisp programs are
developed. This is how I [wrote my Emacs web
server](/blog/2009/05/17/). I opened a socket to listen on 8080, which
originally did nothing but accept connections, and built up a web
server behind the socket without ever taking it down. It's be unusual
to develop Lisp any other way.

Eclipse has directly supported HCR for some years now. I had heard
about it, but never investigated the issue until I saw Markus "Notch"
Persson using it while coding his
[Ludum Dare](http://www.ludumdare.com/compo/)
[entry](http://s3.amazonaws.com/ld48/index.html)
([source](https://s3.amazonaws.com/ld48/PoC_source.zip)) on a
live-stream. It was really neat to see it in use on a graphical
program.

If you know anything about my professional habits, you know I'm not a
fan of giant IDEs like Eclipse. I hack Java with a combination of Ant
and Emacs, [with my own custom extensions](/blog/2010/10/15/). So any
hotswap solution has to be built on that. Luckily, I found this great
Ant extension: [hotswap](http://code.google.com/p/hotswap/). It
provides a `hotswap` task for performing HCR.

It relies on Ant to prepare replacements and determine *which* files
are to be swapped. They provide an example target on their website
demonstrating how to set it up. Personally, I think it's a sloppy way
to do it. It writes a timestamp to a string (the only Ant data
structure available for this), redundantly performs compilation, then
parses the time string to compare it to all of the files, picking out
new ones.

There's actually an `modified` selector exactly for this type of
job. Here's my solution (as seen in my
[sample-java-project](/blog/2010/10/04/)),

~~~xml
<target name="hotswap" depends="compile">
  <taskdef name="hotswap" classname="dak.ant.taskdefs.Hotswap"/>
  <hotswap verbose="true" port="9000">
    <fileset dir="${build.classes.dir}" includes="**/*.class">
      <modified/>
    </fileset>
  </hotswap>
</target>
~~~

The `modified` filter is not timestamp based. It creates a
`cache.properties` file containing the hash of all of your class
files. Trivial changes, such as to comments or whitespace, will not
trigger for replacement. Because that's taken care of without the
timestamp business, no need to write out the compile task again. We
can just call the original as a dependency of this target.

An easy way to try this out for yourself now is with the
[`hotswap-demo`
branch](https://github.com/skeeto/october-chess-engine/tree/hotswap-demo)
of my chess engine. (This branch is special because it forces the GUI
to redraw every second, causing changes to take effect immediately.)
Check out that branch, run the program with the `run` target, then in
`BoardPanel.java` change the colors of the board in `paintComponent()`
— change `LIGHT` to `Color.WHITE` and `DARK` to `Color.GRAY`, for
example. Then, without stopping the program, run the `hotswap` target.
Ant will inject the new code into the running program and the board
will change before your eyes.

I look forward to making good use of this in the future. Expect it to
be a typical Ant target in all of my Java projects from now on.
