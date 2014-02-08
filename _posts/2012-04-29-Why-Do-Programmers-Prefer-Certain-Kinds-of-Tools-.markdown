---
title: Why Do Developers Prefer Certain Kinds of Tools?
layout: post
tags: [rant, emacs, git]
uuid: 4dd7c07d-982d-3ff6-5cdd-70db7c3800bb
---

In my experience, software developers generally prefer some flavor of
programmer's tools when it comes to getting things done. We like plain
text, text editors, command line programs, source control, markup, and
shells. In contrast, non-developer computer users generally prefer
WYSIWYG word processors and GUIs. Developers often have somewhere
between a distaste and a
[revulsion](http://terminally-incoherent.com/blog/2008/10/16/wysiwyg-is-a-lie/)
to WYSIWYG editors.

Why is this? What are programmers looking for that other users aren't?
What I believe it really comes down to is one simple idea: **clean
state transformations**. I'm talking about modifying data, text or
binary, in a precise manner with the possibility of verifying the
modification for correctness in the future.

Think of a file produced by a word processor. It may be some
proprietary format, like a Word's old .doc format, or, more likely as
we move into the future, it's in some bloated XML format that's dumped
into a .zip file. In either case, it's a blob of data that requires a
complex word processor to view and manipulate. It's opaque to source
control, so even merging documents requires a capable, full word
processor.

For example, say you've received such a document from a colleague by
e-mail, for editing. You've read it over and think it looks good,
except you want to italicize a few words in the document. To do that,
you open up the document in a word processor and go through looking
for the words you want to modify. When you're done you click save.

The problem is did you accidentally make any other changes? Maybe you
had to reply to an email while you were in the middle of it and you
accidentally typed an extra letter into the document. It would be easy
to miss and you're probably not set up to easily to check what changes
you've made.

I am aware that modern word processors have a feature that can show
changes made, which can then be committed to the document. This is
really crude compared to a good source control management system. Due
to the nature of WYSIWYG, you're still not seeing all of the
changes. There could be invisible markup changes and there's no way to
know. It's an example of a single program trying to do too many
unrelated things, so that it ends up do many things poorly.

With source code, the idea of patches come up frequently. The program
`diff`, given two text files, can produce a patch file describing
their differences. The complimentary program is `patch`, which can
take the output from `diff` and one of the original files, and use it
to produce the other file. As an example, say you have this source
file `example.c`,

~~~c
int main()
{
    printf("Hello, world.");
    return 0;
}
~~~

If you change the string and save it as a different file, then run
`diff -u` (`-u` for unified, producing a diff with extra context), you
get this output,

~~~udiff
--- example.c  2012-04-29 21:50:00.250249543 -0400
+++ example2.c   2012-04-29 21:50:09.514206233 -0400
@@ -1,5 +1,5 @@
 int main()
 {
+    printf("Hello, world.");
-    printf("Goodbye, world.");
     return 0;
 }
~~~

This is very human readable. It states what two files are being
compared, where they differ, some context around the difference
(beginning with a space), and shows which lines were removed
(beginning with `+` and `-`). A diff like this is capable of
describing any number of files and changes in a row, so it can all fit
comfortably in a single patch file.

If you made changes to a codebase and calculated a diff, you could
send the patch (the diff) to other people with the same codebase and
they could use it to reproduce your exact changes. By looking at it,
they know exactly what changed, so it's not some mystery to them. This
patch is a *clean transformation* from one source code state to
another.

More than that: you can send it to people with a similar, but not
exactly identical, codebase and they could still likely apply your
changes. This process is really what source control is all about: an
easy way to coordinate and track patches from many people. A good
version history is going to be a tidy set of patches that take the
source code in its original form and add a feature or fix a bug
through a series of concise changes.

On a side note, you could efficiently store a series of changes to a
file by storing the original document along with a series of
relatively small patches. This is called delta encoding. This is how
both source control and video codecs usually store data on disk.

Anytime I'm outside of this world of precision I start to get
nervous. I feel sloppy and become distrustful of my tools, because I
generally can't verify that they're doing what I think they're
doing. This applies not just to source code, but also writing. I'm
typing this article in Emacs and when I'm done I'll commit it to
Git. If I make any corrections, I'll verify that my changes are what I
wanted them to be (via [Magit](http://philjackson.github.com/magit/))
before committing and publishing them.

One of my longterm goals with my work is to try to do as much as
possible with my precision developer tools. I've already got
[basic video editing](/blog/2011/11/28/) and
[GIF creation](/blog/2012/04/10/) worked out. I'm still working out a
happy process for documents (i.e. LaTeX and friends) and
presentations.
