---
title: Introducing NativeGuide
layout: post
tags: [java]
uuid: 7aa0c9ab-b5c3-3924-0422-4ae51fb80822
---

See it in action:
[CubeDemo.jar](https://github.com/downloads/skeeto/sample-java-project/CubeDemo.jar) (4.7 MB) (Linux, Windows, and Mac OS X) ([source](https://github.com/skeeto/sample-java-project/tree/lwjgl-cube))

NativeGuide is a Java utility library I wrote that makes native
libraries easier to manage. Really, it's a very small and simple
library, but it provides some critical functionality I desire and I
feel many libraries are sorely lacking. Since I would absolutely love
for other Java libraries to start using it, I went through the [extra
effort](http://docs.sonatype.org/x/SQBl) to have it placed
conveniently in the central Maven repository:
[com.nullprogram:native-guide](http://search.maven.org/#artifactdetails%7Ccom.nullprogram%7Cnative-guide%7C0.2%7Cjar).

As a side note, I actually worked out my own staging solution rather
than rely on Maven — I always go out of my way to avoid Maven. For
reuse-ability, I added it to my [sample-java-project](/blog/2010/10/04/)
as the "bundle" target. It generates all of the required artifacts,
digitally signs them, and bundles them up.

The demo at the top of this post contains native libraries for running
OpenGL programs on seven platforms. NativeGuide takes care of getting
the proper library in place for the JVM to load. All of this is
hopefully transparent to you, the user, so that this appears to be a
regular run-anywhere Java application.

The issue that NativeGuide solves is a special case of the
distribution problem. Distribution is one of the major problems every
language/platform must solve. That is, how do you get your application
to the end user so that they can run it? In the case of a C or C++
application, you compile it for the user's native system, either
statically link or pack up the dependencies, and wrap it all up [in an
installer](http://nsis.sourceforge.net/Main_Page). Python and Perl
have working solutions for different platforms. Ruby must have
something figured out, though I don't know what it is.

I believe this is one of Lisp's biggest problems, because there's no
real solution yet. Assuming the user already has a Lisp system
installed — a very big assumption — what do you deliver? FASL files
aren't compatible between Lisps and sometimes even between versions of
the same Lisp. You're left either targeting their specific Lisp system
or distributing source — which either you can't do or you really
can't expect the user to compile. That's a long ways away from a
simple double-click shortcut. If you somehow got that work, then how
to you make it convenient to launch?

If the user doesn't have a Lisp system installed you then have to
package your own. [Maxima](http://maxima.sourceforge.net/) does this,
but their solution takes a lot of work and maintenance. It's
unreasonable for every application to do
this. [SBCL](http://www.sbcl.org/) is pretty close to making that part
work well, through a feature to save executable images. However,
[until very recently](http://xach.livejournal.com/295584.html) this
came with lots of overhead. A "Hello, World" program would be around
60 MB, since it includes an entire Common Lisp system. Another problem
is the program is entirely integrated to the particular SBCL with
which it was compiled. The user can't install a newer, potentially
more secure, SBCL and run it with the new one.

I think Java's method of application distribution is really nice. The
JRE is easy to install so it's available almost
everywhere. Unfortunately, when it's not installed and you try to
package a JRE with your application, the situation starts to look like
SBCL. Fortunately, that's not a very common problem. Applications can
be packed up into a single `.jar` file, including all the art assets,
configuration XML, and so on, and distributed as a single file, which
is *usually* easy to launch. Java provides special support for
accessing those packed up resources right from the packaging:
[`Class.getResource()`](http://download.oracle.com/javase/7/docs/api/java/lang/Class.html#getResource(java.lang.String%29)
and
[`Class.getResourceAsStream()`](http://download.oracle.com/javase/7/docs/api/java/lang/Class.html#getResourceAsStream(java.lang.String%29).

*However*, the same courtesy is not provided for packed up native
libraries. There's no support for loading them from within a `.jar`
file. What sometimes happens is these libraries end up being delivered
alongside the application. They have to be installed *somewhere* and
there has to be some kind of OS/architecture detection in the
launcher, the hardest place to do it, to determine the right libraries
to use. In a few cases they're packed up with the application, which
copies them out to a temporary directory for loading. That's even more
effort duplication, having to detect the OS and architecture again,
figuring out where exactly to copy them, what to do if they've already
been copied, maybe updating the `java.library.path` which isn't as
sample as just setting the system property.

Libraries often don't worry about that part, leaving it to the users
of the libraries to figure something out. Everyone using the library
makes their own solutions in parallel when it would have been better
if the work was done just once by the library. (A [few
libraries](http://jline.sourceforge.net/#installation) *have* done the
right thing.)

The goal of NativeGuide is to make all of that mess go away. You pack
up your native libraries as resources and register them with
NativeGuide at run-time. NativeGuide detects which library the JRE can
use, copies the correct ones to a temporary directory, and appends
that directory to the `java.library.path` if needed. For example, this
is what registration looks like,

~~~java
NativeGuide.prepare(Arch.LINUX_32, "/x86/libexample.so");
NativeGuide.prepare(Arch.LINUX_64, "/amd64/libexample.so");
NativeGuide.prepare(Arch.WINDOWS_32, "/x86/example.dll");
NativeGuide.prepare(Arch.WINDOWS_64, "/amd64/example.dll");
~~~

Outside of this registration, the whole thing is transparent. I've
already used it with two projects allowing me to package them up into
a single `.jar` file as if no native library was being used. One is a
project at work which uses
[RXTX](http://rxtx.qbang.org/wiki/index.php/Main_Page) and the other
is a branch of my sample-java-project:
[lwjgl](https://github.com/skeeto/sample-java-project/tree/lwjgl). This
allows me to produce a single `.jar` OpenGL application that users on
Linux, Windows, and Mac OS X can just double-click to run.

So my hope is to get libraries using NativeGuide to save everyone
time. The users of the library would probably be unaware that
NativeGuide is there. Just like all of my code, I made it public
domain, so there can be no license objections. However, if the library
maintainers don't choose use it, I made it flexible enough that you
can work around them with little trouble.
