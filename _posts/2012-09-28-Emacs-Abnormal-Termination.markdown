---
title: Emacs Abnormal Termination
layout: post
tags: [emacs, elisp, debian]
uuid: 81986f77-1a4a-3b00-ec3a-a6517f9ca4ca
---

*Update: This bug was fixed in Emacs 24.4 (released October 2014).*

A few months ago I [filed a bug report for Emacs][debian]
([upstream][list]) when I stumbled across Emacs aborting under *very*
specific circumstances. I was editing in [markdown-mode][mode] and a
regular expression replacement on lists would reliably, and
frustratingly, cause Emacs to crash.

Through a sort-of binary search I only loaded only half of
markdown-mode to see in which half it would trigger, then I cut that
half in half again and repeated recursively until I had it down to a
small expression that causes a `--no-init-file` (`-q`) Emacs to
abort. It almost looks like I found it through fuzz testing. Change or
remove anything even slightly and it no longer triggers the abort.

To trigger it, there's an `after-change-functions` hook that performs
a regular expression search immediately after a `replace-regexp`. A
peek at the backtrace with gdb shows that this somehow causes the
point to leave the bounds of the buffer. Emacs detects this as an
assertion before dereferencing anything, and it aborts, thus
preventing a buffer overflow vulnerability. This is important for
[my Emacs web server](/blog/2009/05/17/) because if there's a way to
trigger this bug in the web server I'd much rather have it abort than
run arbitrary shellcode injected in by a malicious HTTP request.

My bug report has seen no activity since I posted it. I can understand
why. The circumstances to trigger it are unlikely and it's a very old
bug, so it's low priority. It's also a huge pain to debug. Hacking on
Emacs from Lisp is pleasant but hacking on Emacs from C is not. The
bug likely sits in the bowels of the complicated regular expression
engine, making it even more unpleasant. I personally have no interest
in trying to fix it myself.

So, since it looks like it's here for the long haul it's kind of fun
to implement an `abort` function on top of it, allowing Elisp programs
to terminate Emacs abnormally — you know, in case `kill-emacs` isn't
fun enough.

~~~cl
(defun abort ()
  "Ask Emacs to abnormally terminate itself (bug#12077)."
  (interactive)
  (with-temp-buffer
    (insert "#\n*\n")
    (goto-char (point-min))
    (add-hook 'after-change-functions
              (lambda (a b c) (re-search-forward "")))
    (replace-regexp "^\\*" " *")))
~~~

It's interactive so you could even bind a key to it.

[debian]: http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=682995
[list]: http://lists.gnu.org/archive/html/bug-gnu-emacs/2012-07/msg01071.html
[mode]: http://jblevins.org/projects/markdown-mode/
