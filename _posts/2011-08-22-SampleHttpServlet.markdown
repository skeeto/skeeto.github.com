---
title: SampleHttpServlet
layout: post
---

Very similar to my [SampleJavaProject](/blog/2010/10/04/), I recently
put together a SampleHttpServlet project,

<pre>
git clone <a href="https://github.com/skeeto/SampleHttpServlet">git://github.com/skeeto/SampleHttpServlet.git</a>
</pre>

It's a barebones Java HttpServlet project. All you need is a JDK,
[Ant](http://ant.apache.org/), and
[Ivy](http://ant.apache.org/ivy/). You don't actually need to install
a servlet container ahead of time. Instead,
[Jetty](http://jetty.codehaus.org/jetty/) is fetched by Ivy and run
locally from the repository's working directory. Once you clone, you
can see all this in action with the `run` target.

    ant run

When it's all done spewing build logs at you, visit your local machine
on port 8080 with your browser. It demonstrates session tracking by a
cookie, a visitor counter, and an example JavaServer Pages (JSP)
page. That touches all the fundamentals.

I had spent awhile working out a development workflow with Java
servlets until I came down to this one. I started out with Jetty,
since it seemed like a simpler, cleaner solution than
[Apache Tomcat](http://tomcat.apache.org/). I quickly found out that
Jetty doesn't really directly support hot deployment: updating the
servlets without restarting the entire server. That's not a good
workflow. Adding any additional steps to the development loop is just
asking for headaches. Tomcat supports hot deploy, but it requires
significant ahead-of-time configuration for each machine I would
develop on. The whole point of using Ivy is that it sets up the
project's environment for me, so I (or someone else) can quickly get
going on a new machine once Ivy and Ant are in place.

So I ended up with something very much like my normal Java workflow. I
restart the server every time I want to see my changes, but I do it
directly from Ant -- meaning I can
[trigger it from within Emacs](/blog/2010/10/15/). It's really no
different from developing a normal application, except that a browser
is involved.

I created this two months ago and have yet to create any servlet
projects from it. That's the point of these Sample*Xxxx*Project
projects. When I start a new Java project I just fork the sample
project and I have a working build system set up for me. I didn't have
any particular servlet ideas in mind, except the possibility of
[rewriting my blog backend](/blog/2011/08/05/) with it, which didn't
happen. As with *so* much of web development, I think it's just too
clunky for me to be interesting in doing anything interesting with
it. If I ever need to do something like this at work, I'll at least
have a head start.
