---
title: Viewing Java Class Files in Emacs
layout: post
tags: [emacs, java]
uuid: 1bb9f8a9-61eb-34eb-bb62-83e93166cbea
---

One of the users of [my Emacs java extensions](/blog/2010/10/15/)
e-mailed me with a question/suggestion about viewing .class files in
Emacs. Emacs has automatic compression, encryption, and archive modes
which allow certain non-text files to be viewed within Emacs in a
sensible text form. He wanted to do the same with Java byte-compiled
.class files: when opening a .class file, Emacs should automatically
and transparently decompile the bytecode into Java source.

He mentioned
[JAD](http://en.wikipedia.org/wiki/JAD_(JAva_Decompiler%29)
specifically, a popular, proprietary, but unmaintained and outdated
Java bytecode decompiler. I've never used it and honestly I see no
reason to start using it. Unfortunately there are no other decompilers
in the Debian package archives and I know nothing else about Java
decompiling, so this left me kind of stuck. Instead I decided to build
a proof-of-concept using `javap`, the Java disassembler, which comes
with JDKs.

Here it is: [javap-handler.el](https://gist.github.com/3178747). With
these forms evaluated, try opening a .class file in Emacs. Rather than
a screen full of junk, you'll (hopefully) be presented with a
read-only buffer containing detailed information about the class.

~~~cl
(add-to-list 'file-name-handler-alist '("\\.class$" . javap-handler))

(defun javap-handler (op &rest args)
  "Handle .class files by putting the output of javap in the buffer."
  (cond
   ((eq op 'get-file-buffer)
    (let ((file (car args)))
      (with-current-buffer (create-file-buffer file)
        (call-process "javap" nil (current-buffer) nil "-verbose"
                      "-classpath" (file-name-directory file)
                      (file-name-sans-extension
                       (file-name-nondirectory file)))
        (setq buffer-file-name file)
        (setq buffer-read-only t)
        (set-buffer-modified-p nil)
        (goto-char (point-min))
        (java-mode)
        (current-buffer))))
   ((javap-handler-real op args))))

(defun javap-handler-real (operation args)
  "Run the real handler without the javap handler installed."
  (let ((inhibit-file-name-handlers
         (cons 'javap-handler
               (and (eq inhibit-file-name-operation operation)
                    inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation))
    (apply operation args)))
~~~

[![](/img/emacs/javap-junk-thumb.png)](/img/emacs/javap-junk.png)

[![](/img/emacs/javap-clear-thumb.png)](/img/emacs/javap-clear.png)

This was harder to do than I thought it would be. To make a new
"magic" file mode requires the use of a half-documented, hackish
file-name-handler API. There's
[a page on it](http://www.gnu.org/software/emacs/manual/html_node/elisp/Magic-File-Names.html)
in the *GNU Emacs Lisp Reference Manual* but I mostly figured it out
by reading the source code around auto-compression-mode and
auto-encryption-mode.

It works by installing a handler function in `file-name-handler-alist`
— similar to `auto-mode-alist`. The handler has complete control over
how a particularly-named class of files is handled by Emacs. For
example, the most useful part is instead of actually providing the
contents of a file, the handler can present any contents it wants. In
this case, rather than read in the actual bytecode, the handler
executes `javap` on the file and uses the output for the buffer
content.

The hackish part is when the handler wants to let Emacs handle an
operation the normal way, which is pretty much every case except for
`get-file-buffer`. The handler has to disable itself by temporarily
setting a dynamically-scoped variable (one of the many legacy areas
that prevents Emacs from being lexically-scoped by default), then ask
Emacs to try the operation again.

As I said, this is just a proof-of-concept so there are two issues
remaining. The first was something requested specifically: viewing
.class files inside .jar archives. It could do this if it was just a
little bit smarter about the classpath. I leave that as an exercise to
the reader. :-)

The second is finding a well-behaved, reasonable decompiler (GUI-less
Unix filter) and replacing `javap` with it. Given that assumption,
this should be as simple as replacing a couple of strings in the
`call-process`.

This is interesting enough that, if I were to fix it up for
correctness sometime, I may include it as part of java-mode-plus
someday.
