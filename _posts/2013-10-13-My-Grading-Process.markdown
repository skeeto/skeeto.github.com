---
title: My Grading Process
layout: post
date: 2013-10-13T02:56:31Z
tags: [java, rant, reddit]
uuid: 8c5aecd0-c7f0-314d-d4a8-88014a195e08
---

My GitHub activity, including this blog, has really slowed down for
the past month because I've spent a lot of free time grading homework
for a [design patterns class][class], taught by a colleague at the
[Whiting School of Engineering][wse]. Conveniently for me, all of my
interaction with the students is through e-mail. It's been a great
exercise of [my new e-mail setup][mail], which itself has definitely
made this job easier. It's kept me very organized through the whole
process.

![](/img/screenshot/github-dropoff.png)

Each assignment involves applying two or three design patterns to a
crude (in my opinion) XML parsing library. Students are given a
tarball containing the source code for the library, in both Java and
C++. They pick a language, modify the code to use the specified
patterns, zip/archive up the result, and e-mail me their
zipfile/tarball.

It took me the first couple of weeks to work out an efficient grading
workflow, and, at this point, I can accurately work my way through
most new homework submissions rapidly. On my end I already know the
original code base. All I really care about is the student's changes.
In software development this sort of thing is expressed a *diff*,
preferably in the [*unified diff*][unidiff] format. This is called a
*patch*. It describes precisely what was added and removed, and
provides a bit of context around each change. The context greatly
increases the readability of the patch and, as a bonus, allows it to
be applied to a slightly different source. Here's a part of a patch
recently submitted to Elfeed:

~~~diff
diff --git a/tests/elfeed-tests.el b/tests/elfeed-tests.el
index 31d5ad2..fbb78dd 100644
--- a/tests/elfeed-tests.el
+++ b/tests/elfeed-tests.el
@@ -144,15 +144,15 @@
   (with-temp-buffer
     (insert elfeed-test-rss)
     (goto-char (point-min))
-    (should (eq (elfeed-feed-type (xml-parse-region)) :rss)))
+    (should (eq (elfeed-feed-type (elfeed-xml-parse-region)) :rss)))
   (with-temp-buffer
     (insert elfeed-test-atom)
     (goto-char (point-min))
-    (should (eq (elfeed-feed-type (xml-parse-region)) :atom)))
+    (should (eq (elfeed-feed-type (elfeed-xml-parse-region)) :atom)))
   (with-temp-buffer
     (insert elfeed-test-rss1.0)
     (goto-char (point-min))
-    (should (eq (elfeed-feed-type (xml-parse-region)) :rss1.0))))
+    (should (eq (elfeed-feed-type (elfeed-xml-parse-region)) :rss1.0))))

 (ert-deftest elfeed-entries-from-x ()
   (with-elfeed-test
~~~

I'd *really* prefer to receive patches like this as homework
submissions but this is probably too sophisticated for most students.
Instead, the first thing I do is create a patch for them from their
submission. Most students work off of their previous submission, so I
just run `diff` between their last submission and the current one.
While I've got a lot of the rest of the process automated with
scripts, I unfortunately cannot script patch generation. Each
student's submission follows a unique format for that particular
student and some students are not even consistent between their own
assignments. About half the students also include generated files
alongside the source so I need to clean this up too. Generating the
patch is by far the messiest part of the whole process.

I grade almost entirely from the patch. 100% correct submissions are
usually only a few hundred lines of patch and I can spot all of the
required parts within a few minutes. Very easy. It's the incorrect
submissions that consume most of my time. I have to figure out what
they're doing, determine what they *meant* to do, and distill that
down into discrete discussion items along with point losses. In either
case I'll also add some of my own opinions on their choice of style,
though this has no effect on the final grade.

For each student's submission, I commit to a private Git repository
the raw, submitted archive file, the generated patch, and a grade
report written in Markdown. After the due date and once all the
submitted assignments are graded, I reply to each student with their
grade report. On a few occasions there's been a back and forth
clarification dialog that has resulted in the student getting a higher
score. (That's a hint to any students who happen to read this!)

Even ignoring the time it takes to generate a patch, there are still
disadvantages to not having students submit patches. One is the size:
about 60% of my current e-mail storage, which goes all the way back to
2006, is from this class alone from the past one month. It's been a
lot of bulky attachments. I'll delete all of the attachments once the
semester is over.

Another is that the students are unaware of the amount of changes they
make. Some of these patches contain a significant number of trivial
changes — breaking long lines in the original source, changing
whitespace within lines, etc. If students focused on crafting a tidy
patch they might try to avoid including these types of changes in
their submissions. I like to imagine this process being similar to
submitting a patch to an open source project. Patches should describe
a concise set of changes, and messy patches are rejected outright. The
Git staging area is all about crafting clean patches like this.

If there was something else I could change it would be to severely
clean up the original code base. When compiler warnings are turned on,
compiling it emits a giant list of warnings. The students are already
starting at an unnecessary disadvantage, missing out on a very
valuable feature: because of all the existing noise they can't
effectively use compiler warnings themselves. Any new warnings would
be lost in the noise. This has also lead to many of those
trivial/unrelated changes: some students are spending time fixing the
warnings.

I want to go a lot further than warnings, though. I'd make sure the
original code base had absolutely no issues listed by [PMD][pmd],
[FindBugs][findbugs], or [Checkstyle][checkstyle] (for the Java
version, that is). Then I could use all of these static analysis tools
on student's submissions to quickly spot issues. It's as simple as
[using my starter build configuration][build]. In fact, I've used
these tools a number of times in the past to perform detailed code
reviews for free ([1][cr0], [2][cr1], [3][cr2]). Providing an
extensive code analysis for each student for each assignment would
become a realistic goal.

I've expressed all these ideas to the class's instructor, my
colleague, so maybe some things will change in future semesters. If
I'm offered the opportunity again — assuming I didn't screw this
semester up already — I'm still unsure if I would want to grade a
class again. It's a lot of work for, optimistically, what amounts to
the same pay rate I received as an engineering intern in college. This
first experience at grading has been very educational, making me
appreciate those who graded my own sloppy assignments in college, and
that's provided value beyond the monetary compensation. Next time
around wouldn't be as educational, so my time could probably be better
spent on other activities, even if it's writing open source software
for free.


[class]: http://apps.ep.jhu.edu/courses/605/707
[wse]: http://engineering.jhu.edu/
[mail]: /blog/2013/09/03/
[unidiff]: http://en.wikipedia.org/wiki/Diff#Unified_format
[pmd]: http://pmd.sourceforge.net/
[findbugs]: http://findbugs.sourceforge.net/
[checkstyle]: http://checkstyle.sourceforge.net/
[build]: https://github.com/skeeto/sample-java-project/blob/master/build.xml
[cr0]: http://old.reddit.com/r/javahelp/comments/1inzs7/_/cb6ojr2
[cr1]: http://old.reddit.com/r/reviewmycode/comments/1a2fty/_/c8tpme2
[cr2]: http://old.reddit.com/r/javahelp/comments/1balsp/_/c958num
