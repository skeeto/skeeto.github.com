---
title: The Missing Computer Skills of High School Students
layout: post
date: 2018-10-31T22:02:36Z
tags: [meatspace]
uuid: 96cc0dde-1524-3030-8bfc-016439796c16
excerpt_separator: <!--more-->
---

*This article was [discussed on Hacker News][hn] and [discussed on
Reddit][reddit].*

It's been just over fours years since I started [mentoring high school
students at work][two], and I recently began mentoring my fourth such
student. That's enough students for me to start observing patterns. Of
course, each arrives with different computer knowledge and experience,
but there have been two consistent and alarming gaps. One is a concept
and the other is a skill, both of which I expect an advanced high
schooler, *especially* one interested in computers, to have before
they arrive. This gap persists despite students taking computer
classes at school.

<!--more-->

### File, Directories, and Paths

The vital gap in concepts is files, directories, or, broadly speaking,
*paths*. Students do initially arrive with a basic notion of files and
directories (i.e. "folders") and maybe some rough idea that there's a
hierarchy to it all. But they've never learned the notation: a location
to a file specified by a sequence of directory components which may be
either *relative* or *absolute*. More specifically, they've never been
exposed to the concepts of `.` (current directory) nor `..` (parent
directory).

The first thing I do with my students is walk them though a Linux
installation process and then sit them in front of a terminal. Since
most non-option arguments are file paths, shell commands are rather
limited if you don't know anything about paths. You can't navigate
between directories or talk about files outside of your home
directory. So one of the first things I have to teach is how paths
work. We go through exercises constructing and reasoning about paths,
and it takes some time and practice for students to really "get" them.

And this takes longer than you might think! Even once the student has
learned the basic concepts, it still takes practice to internalize and
reason about them. It's been a consistent enough issue that I really
should assemble a booklet to cover it, and perhaps some sort of
interactive practice. I could just hand this to the student so they
can learn on their own as they do with other materials.

Paths aren't just imporant for command line use. They come up in every
day programming when programs need to access files. In some contexts it
even has security implications regardless of the programming language.
For example, care must be taken handling and validating paths supplied
from an untrusted source. A web application may need to translate a
path-like string in a query into a file path, and not understanding how
`..` works can make this dangerous. Or not understanding how paths need
to be normalized before being compared.

I consider paths as falling under file and directory basics, and it's
part of the baseline for a person to be considered computer literate.

### Touch Typing

The other major gap is touch typing. None of my students have been
touch typists, and it slows them all down far more than they realize.
I spend a lot of time next to them at the keyboard as they drive, so
I've seen the costs myself. In a couple of cases, the students have to
finger peck while looking at the keyboard.

An important step in mastering the use of computers is quickly
iterating on new ideas and concepts — trying out and playing with
things as they are learned. Being a slow typist not only retards this
process, the tedium of poor, plodding keyboarding actively discourages
experimentation, becoming a barrier. Advanced computer use isn't much
fun if you can't type well.

To be fair, I've only been a proper touch typist [for under two
years][tt]. I wish I had learned it much earlier in life, and I really
only have myself to blame that it took so long. Fortunately I *had*
developed my own pseudo touch touching style that required neither
finger pecking nor looking at the keyboard. My main issue was accuracy,
not that typing was tedious or slow.

The bad news is that, unlike paths, this is completely outside my
control. First, one of the major guidelines of the mentoring program
is that we're not supposed to spend a lot of time on basic skills.
Learning to touch type takes several weeks of daily effort. That's
just too much time that we don't have. Second, this won't work anyway
unless the student is motivated to learn it. I have no idea how to
provide that sort of motivation. (And if the student *is* motivated,
they'll do it on their own time anyway.) I think that's where schools
get stuck.

The really bad news is that this problem is going to get worse. The
mobile revolution happened, and, for most people, mobile devices are
gradually replacing the home computer, even laptops. I already know
one student who doesn't have access to a general purpose computer at
home. The big difference between a tablet and a laptop is that a
tablet is purely for consumption.

In the future, kids will be less and less exposed to keyboards, and
productive computing in general. Keyboards are, and will continue to be
for the foreseeable future, a vital tool for professionals. I wonder if
the future will be a bit like, say, the 1980s again, where only a small
fraction of kids will be exposed to a proper computer. Only instead of a
PC clone, Commodore, or Apple II, it's a Raspberry Pi.

### Conclusions

I want to make something really clear: I'm not blaming the students for
these gaps. It's not in any way their fault. What they're taught and
exposed to is, at this point in life, largely outside of their control.

I lay most of the blame on the schools. My mentees have all taken high
school programming classes of some sort, but these classes somehow
manage to skip over the fundamentals. Instead it's rote learning some
particular IDE without real understanding. Finally I can relate to all
those mathematicians' complaining about how math class is taught!

What can be done? If you're a parent, make sure your kid has access to a
general purpose computer, even if it's only a Raspberry Pi or one of its
clones, along with a keyboard and mouse. (Of course, if you're reading
this article you're not one of the parents that needs this advice.) It's
good exposure all around.

After reflecting on this recently, I think one of the shortcomings of
my mentoring is that I don't spend enough time — generally none at all
— at the keyboard driving with my mentee as the passenger, where they
can really observe me in action. Usually it's me approaching them to
check on their progress, and the opportunity just isn't there. Perhaps
it would be motivating to see how efficient and fun computing can be
at higher levels of proficiency — to show them how touch typing and a
powerful text editor can lead to such a dramatic difference in
experience. It would be the answer to that question of, "Why should I
learn this?"


[hn]: https://news.ycombinator.com/item?id=18349847
[reddit]: https://redd.it/9td4qx
[tt]: /blog/2017/04/01/
[two]: /blog/2016/09/02/
