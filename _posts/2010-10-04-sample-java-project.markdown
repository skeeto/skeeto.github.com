---
title: Sample Java Project
layout: post
tags: [java, tutorial]
uuid: e92e3985-6680-335c-2c69-dc95781f42bd
---

<!-- 4 October 2010 -->

Here's a little on-going project I put together recently. It's mostly
for my own future reference, but perhaps someone else may find it
useful.

<pre>
git clone <a href="https://github.com/skeeto/sample-java-project">git://github.com/skeeto/sample-java-project.git</a>
</pre>

If you couldn't guess already, I'm strongly against tying a project's
development to a particular IDE. It happens too much: someone starts
the project by firing up their favorite IDE, clicking "Create new
project", and checks in whatever it spits out. It usually creates a
build system integrated tightly into that particular IDE. At work I've
seen it happen on two different large Java projects. There are some
ways around it, like maintaining two build systems side-by-side, but
it's not very pretty. Sometimes the Java IDE can spit out some Ant
build files for the sake of continuous integration, but it remains a
second-class citizen for development.

I prefer the other direction: start with a standalone build system,
then stick your own development environment on top of that. Each
developer picks and is responsible for whatever IDE or editor they
want, with the standalone build system providing the canonical build
(and, in my experience, if you *must* use an IDE, NetBeans has
the smoothest integration with Ant). So in the case of Java, this
means setting up an Ant-based build.

I've said before that [I like the Java
platform](/blog/2010/08/13/), I just find the primary language
disappointing. Similarly, I like Ant, I just find the build script
language disappointing (XML). It seems other people like it too, at
least for Java development,
because [I haven't
been able to find any serious criticisms](http://www.google.com/search?q=ant+sucks) of it outside of hating
the XML (notice the first result in that search is written by someone
who is Doing It All Wrong). I love that it works on filesets and not
files. It's like getting atomic commits for my build system. If I add
a new source file to my project I don't need to adjust the Ant build
script in any way.

One downside of Ant is that, while it's commonly used in a very
standard way, it doesn't guide you in that direction or provide
special shortcuts to make the common cases easier. It's typical to
have a `src/` directory containing all your source and
a `build/` directory, created by Ant, that contains all the
built and generated files. With Ant you basically say, "Compile these
sources to here, then jar that directory up." Ant alone doesn't make
this very obvious. Give it to someone standed on a desert island and I
bet they won't derive the same best practice as the rest of the world.

Take `make`, for example. Because building object files
from source is so common, (depending on the implementation) it has
built-in rules for it. This is all you need to say,
and `make` knows how to do the rest.

```make
file.o : file.c
```

Same for linking, it's so common you don't have to type anything more
than necessary.

```make
program : main.o common.o file.o
```

It guides you in creating good Makefiles. If you want to learn the
best practice for Ant, you have to either buy a book on Ant or look at
what lots of other people are doing. And so I provide my
sample-java-project for this exact purpose.

You can use that as a skeleton when creating your own project, and
you'll barely have to customize the build file. It's a big mass of
boilerplate, the kind of stuff that Ant should have built-in by
default. I'll be expanding it over time as I learn more about how to
effectively use Ant.

So far, I included two things that you normally won't see: a target to
run a Java indenter
([AStyle](http://astyle.sourceforge.net/)) on your code,
and a target to run the
bureaucratic [ Checkstyle](http://checkstyle.sourceforge.net/) on your code.
