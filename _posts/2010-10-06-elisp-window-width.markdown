---
title: Emacs Set Window to 80 Columns
layout: post
tags: [emacs, lisp]
uuid: 90343c39-62b8-35c2-d9a7-8b3b29cb8338
---

<!-- 6 October 2010 -->

When I'm coding, I maximize Emacs and enable
[ `winner-mode`](http://www.emacswiki.org/emacs/WinnerMode), turning my display into something much
like a [ tiling window manager](http://en.wikipedia.org/wiki/Tiling_window_manager). Then I try not to leave Emacs until it's
necessary. It's a really nice way to work: no mouse touching needed.

At work they gave me a nice 24" monitor, 1920 pixels across. That's
just about enough to fit three Emacs' windows side-by-side at 78
columns each. The leftmost one contains my active work buffer where I
do most of my typing. The center one is usually split
horizontally. The top half is the `*compilation*` buffer
and the bottom half is either [Emacs
calculator](/blog/2009/06/23/) or an `*ansi-term*` buffer. The rightmost
buffer contains something more static, like some sort of reference
material.

However, I like my main editing window to be 80 columns wide. 78
columns cuts just too short. For awhile I was creating 80 dashes
(`C-u 80 -`) and adjusting the window width manually to
size. After doing it a few times I decided to extend Emacs to do it
instead. First define a function to set the current window width.

```cl
(defun set-window-width (n)
  "Set the selected window's width."
  (adjust-window-trailing-edge (selected-window) (- n (window-width)) t))
```

Wrap it with an interactive function and bind it.

```cl
(defun set-80-columns ()
  "Set the selected window to 80 columns."
  (interactive)
  (set-window-width 80))

(global-set-key "\C-x~" 'set-80-columns)
```

For those paying extra attention: instead of writing the extra
function, you could
use [my `expose` function from
the other day](/blog/2010/09/29/).

```cl
(global-set-key "\C-x~" (expose (apply-partially 'set-window-width 80)))
```

The problem with this, though, is the dynamically generated function
doesn't have a name or a docstring. Someone
using `describe-key` would have little information to go
on.
