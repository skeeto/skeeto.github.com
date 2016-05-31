---
title: Emacs Javadoc Lookups Get a Facelift
layout: post
tags: [emacs, java, clojure]
uuid: 18159666-2379-359b-35dd-edf5c4bdbf60
---

Ever since
[I started using the Emacs package archive](/blog/2012/08/12/),
specifically [MELPA][melpa], I'd been wanting to tidy up
[my Emacs Java extensions](/blog/2010/10/14/), java-mode-plus, into a
nice, official package. Observing my own attitude after the switch, I
noticed that if a package isn't available on ELPA or MELPA, it
practically doesn't exist for me. Manually installing anything now
seems like so much trouble in comparison, and getting a package on
MELPA is so easy that there's little excuse for package authors not to
have their package in at least one of the three major Elisp
archives. This is exactly the attitude my own un-archived package
would be facing from other people, and rightfully so.

Before I dive in, this is what the user configuration now looks like,

~~~cl
(javadoc-add-artifacts [org.lwjgl.lwjg lwjgl "2.8.2"]
                       [com.nullprogram native-guide "0.2"]
                       [org.apache.commons commons-math3 "3.0"])
~~~

That's right: it knows how to find, fetch, and index documentation on
its own. Keep reading if this sounds useful to you.

### The Problem

The problem was that java-mode-plus was doing two unrelated things:

 * Supporting Ant-oriented Java projects. Not
 [being a fan of Maven][maven], I've used Ant for all of my own
 personal projects. (However, I really do like the Maven
 infrastructure, so I use Apache Ivy.) It seems Maven is a lot more
 popular, so this part isn't useful for many people.

 * Quick Javadoc referencing, which I was calling java-docs. I think
 this is generally useful for anyone writing Java in Emacs, even if
 they're using another suite like JDEE or writing in another JVM
 language. It would be nice for people to be able to use this without
 pulling in all of java-mode-plus — which was somewhat intrusive.

I also didn't like the names I had picked. java-mode-plus wasn't even
a mode until recently and its name isn't conventional. And "java-docs"
is just stupid. I recently solved all this by splitting the
java-mode-plus into two new packages,

 * [*ant-project-mode*][ant-project-mode] — A minor mode that
 performs the duties of the first task above. Since I've
 [phased Java out](/blog/2012/08/12/) from my own personal projects
 and no longer intend to write Java anymore, this part isn't very
 useful to me personally at the moment. If I do need to write Java for
 work again I'll probably dust this off. It's by no means
 un-maintained, it's just in maintenance mode for now. Because of
 this, this is not in any Emacs package archive

 * [*javadoc-lookup*][javadoc-lookup] — This is java-docs renamed and
 **with some new goodies!** I also **put this on MELPA**, where it's
 easy for anyone to use. This is continues to be useful for me as I
 use Clojure.

### javadoc-lookup

This is used like java-docs before it, just under a different
name. The function `javadoc-lookup` asks for a Java class for
documentation. I like to bind this to `C-h j`.

The function `javadoc-add-roots` provides filesystem paths to be
indexed for lookup.

~~~cl
(javadoc-add-roots "/usr/share/doc/openjdk-6-jdk/api"
                   "~/src/project/doc")
~~~

Also, as before, if you don't provide a root for the core Java API, it
will automatically load an index of the official Javadoc hosted
online. This means it can be installed from MELPA and used immediately
without any configuration. Good defaults and minimal required
configuration [is something I highly value](/blog/2012/10/31/).

Back in the java-docs days, when I started using a new library I'd
track down the Javadoc jar, unzip it somewhere on my machine, and add
it to be indexed. I regularly do development on four different
computers, so this gets tedious fast. Since the Javadoc jars are
easily available from the Maven repository, I maintained a small Ant
project within my .emacs.d for awhile just to do this fetching, but it
was a dirty hack.

#### Finally, the Goodies

Here's the cool new part: I built this functionality into
javadoc-lookup. **It can fetch all your documentation for you!**
Instead of providing a path on your filesystem, you name an artifact
that Maven can find. javadoc-lookup will call Maven to fetch the
Javadoc jar, unzip it into a cache directory, and index it for
lookups. You will need Maven installed either on your `$PATH` or at
`maven-program-name` (Elisp variable).

Here's a sample configuration. It's group, artifact, version provided
as a sequence. I say "sequence" because it can be either a list or a
vector and those names can be either strings or symbols. I prefer the
vector/symbol method because it requires
[the least quoting](/blog/2012/07/17/), plus it looks Clojure-ish.

~~~cl
(javadoc-add-artifacts [org.lwjgl.lwjg lwjgl "2.8.2"]
                       [com.nullprogram native-guide "0.2"]
                       [org.apache.commons commons-math3 "3.0"])
~~~

Put that in your initialization and all this documentation will appear
in the lookup index. It only needs to fetch from Maven once per
artifact per system — a very very slow process. After that it
operates entirely from its own cache which is very fast, so it won't
slow down your startup.

This has been extremely convenient for me so I hope other people find
it useful, too.

As a final note, javadoc-lookup also exploits structural sharing in
its tables, using a lot less memory than java-docs. Not that it was a
problem before; it's a feel-good feature.


[melpa]: http://melpa.milkbox.net/
[maven]: http://kent.spillner.org/blog/work/2009/11/14/java-build-tools.html
[ant-project-mode]: https://github.com/skeeto/ant-project-mode
[javadoc-lookup]: https://github.com/skeeto/javadoc-lookup
