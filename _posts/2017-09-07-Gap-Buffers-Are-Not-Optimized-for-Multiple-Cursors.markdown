---
title: Gap Buffers Are Not Optimized for Multiple Cursors
layout: post
date: 2017-09-07T01:34:04Z
tags: [emacs, c, vim]
uuid: 8c80d068-2342-356a-9b78-f180806418a4
---

Gap buffers are a common data structure for representing a text buffer
in a text editor. Emacs famously uses gap buffers — long-standing proof
that gap buffers are a perfectly sufficient way to represent a text
buffer.

* Gap buffers are *very* easy to implement. A bare minimum
  implementation is about 60 lines of C.

* Gap buffers are especially efficient for the majority of typical
  editing commands, which tend to be clustered in a small area.

* Except for the gap, the content of the buffer is contiguous, making
  the search and display implementations simpler and more efficient.
  There's also the potential for most of the gap buffer to be
  memory-mapped to the original file, though typical encoding and
  decoding operations prevent this from being realized.

* Due to having contiguous content, saving a gap buffer is basically
  just two `write(2)` system calls. (Plus [`fsync(2)`, etc.][fsync])

A gap buffer is really a pair of buffers where one buffer holds all of
the content before the cursor (or *point* for Emacs), and the other
buffer holds the content after the cursor. When the cursor is moved
through the buffer, characters are copied from one buffer to the
other. Inserts and deletes close to the gap are very efficient.

Typically it's implemented as a single large buffer, with the
pre-cursor content at the beginning, the post-cursor content at the
end, and the gap spanning the middle. Here's an illustration:

![](/img/gap-buffer/intro.gif)

The top of the animation is the display of the text content and cursor
as the user would see it. The bottom is the gap buffer state, where
each character is represented as a gray block, and a literal gap for
the cursor.

Ignoring for a moment more complicated concerns such as undo and
Unicode, a gap buffer could be represented by something as simple as
the following:

~~~c
struct gapbuf {
    char *buf;
    size_t total;  /* total size of buf */
    size_t front;  /* size of content before cursor */
    size_t gap;    /* size of the gap */
};
~~~

This is close to [how Emacs represents it][emacs]. In the structure
above, the size of the content after the cursor isn't tracked directly,
but can be computed on the fly from the other three quantities. That is
to say, this data structure is *normalized*.

As an optimization, the cursor could be tracked separately from the
gap such that non-destructive cursor movement is essentially free. The
difference between cursor and gap would only need to be reconciled for
a destructive change — an insert or delete.

A gap buffer certainly isn't the only way to do it. For example, the
original [vi used an array of lines][editors], which sort of explains
some of its quirky [line-oriented idioms][bs]. The BSD clone of vi, nvi,
[uses an entire database][nvi] to represent buffers. Vim uses a fairly
complex [rope][rope]-like [data structure][memline] with [page-oriented
blocks][vim], which may be stored out-of-order in its swap file.

### Multiple cursors

[*Multiple cursors*][er] is fairly recent text editor invention that
has gained a lot of popularity recent years. It seems every major
editor either has the feature built in or a readily-available
extension. I myself used Magnar Sveen's [well-polished package][mc]
for several years. Though obviously the concept didn't originate in
Emacs or else it would have been called *multiple points*, which
doesn't quite roll off the tongue quite the same way.

The concept is simple: If the same operation needs to done in many
different places in a buffer, you place a cursor at each position, then
drive them all in parallel using the same commands. It's super flashy
and great for impressing all your friends.

However, as a result of [improving my typing skills][typing], I've
come to the conclusion that [multiple cursors is all hat and no
cattle][one]. It doesn't compose well with other editing commands, it
doesn't scale up to large operations, and it's got all sorts of flaky
edge cases (off-screen cursors). Nearly anything you can do with
multiple cursors, you can do better with old, well-established editing
paradigms.

Somewhere around 99% of my multiple cursors usage was adding a common
prefix to a contiguous serious of lines. As similar brute force
options, Emacs already has rectangular editing, and Vim already has
visual block mode.

The most sophisticated, flexible, and robust alternative is a good old
macro. You can play it back anywhere it's needed. You can zip it across
a huge buffer. The only downside is that it's less flashy and so you'll
get invited to a slightly smaller number of parties.

But if you don't buy my arguments about multiple cursors being
tasteless, there's still a good technical argument: **Gap buffers are
not designed to work well in the face of multiple cursors!**

For example, suppose we have a series of function calls and we'd like to
add the same set of arguments to each. It's a classic situation for a
macro or for multiple cursors. Here's the original code:

    foo();
    bar();
    baz();

The example is tiny so that it will fit in the animations to come.
Here's the desired code:

    foo(x, y);
    bar(x, y);
    baz(x, y);

With multiple cursors you would place a cursor inside each set of
parenthesis, then type `x, y`. Visually it looks something like this:

![](/img/gap-buffer/illusion.gif)

Text is magically inserted in parallel in multiple places at a time.
However, if this is a text editor that uses a gap buffer, the
situation underneath isn't quite so magical. The entire edit doesn't
happen at once. First the `x` is inserted in each location, then the
comma, and so on. The edits are not clustered so nicely.

From the gap buffer's point of view, here's what it looks like:

![](/img/gap-buffer/multicursors.gif)

For every individual character insertion the buffer has to visit each
cursor in turn, performing lots of copying back and forth. The more
cursors there are, the worse it gets. For an edit of length `n` with
`m` cursors, that's `O(n * m)` calls to `memmove(3)`. Multiple cursors
scales badly.

Compare that to the old school hacker who can't be bothered with
something as tacky and *modern* (eww!) as multiple cursors, instead
choosing to record a macro, then play it back:

![](/img/gap-buffer/macros.gif)

The entire edit is done locally before moving on to the next location.
It's perfectly in tune with the gap buffer's expectations, only needing
`O(m)` calls to `memmove(3)`. Most of the work flows neatly into the
gap.

So, don't waste your time with multiple cursors, especially if you're
using a gap buffer text editor. Instead get more comfortable with your
editor's macro feature. If your editor doesn't have a good macro
feature, get a new editor.

If you want to make your own gap buffer animations, here's the source
code. It includes a tiny gap buffer implementation:

* [https://github.com/skeeto/gap-buffer-animator][repo]


[bs]: http://vimhelp.appspot.com/options.txt.html#'backspace'
[editors]: https://ecc-comp.blogspot.com/2015/05/a-brief-glance-at-how-5-text-editors.html
[emacs]: http://git.savannah.gnu.org/cgit/emacs.git/tree/src/buffer.h?h=emacs-25.2#n425
[er]: http://emacsrocks.com/e13.html
[mc]: https://github.com/magnars/multiple-cursors.el
[memline]: https://github.com/vim/vim/blob/e723c42836d971180d1bf9f98916966c5543fff1/src/memline.c
[nvi]: https://en.wikipedia.org/wiki/Nvi
[one]: https://medium.com/@schtoeffel/you-don-t-need-more-than-one-cursor-in-vim-2c44117d51db
[repo]: https://github.com/skeeto/gap-buffer-animator
[rope]: https://en.wikipedia.org/wiki/Rope_(data_structure)
[typing]: /blog/2017/04/01/
[vim]: http://www.free-soft.org/FSM/english/issue01/vim.html
[fsync]: https://stackoverflow.com/a/7433149
