---
title: Turning Asynchronous into Synchronous in Elisp
layout: post
tags: [elisp, emacs]
uuid: 61c72b82-371c-304f-7e0e-f5ea4a990ef3
---

As a [new user of nREPL](/blog/2013/01/07/) I was poking around
nrepl.el, seeing what sorts of Elisp tricks I could learn. Even though
it was written 6 months before Skewer, and I was completely unaware of
nREPL's existence until two weeks ago, there's a lot of similarity
between nrepl.el and [Skewer](/blog/2012/10/31/). Due to serving the
same purpose for different platforms, this isn't very surprising.

In particular, Skewer has `skewer-eval` for sending a string to the
browser for evaluation. Like JavaScript, Emacs Lisp is
single-threaded: there's only one execution context at a time and it
has to return to the top-level before a new context can execute. There
are no continuations or coroutines. `skewer-eval` requires
coordination with an external process (the browser) making it
inherently asynchronous. So as a second, optional argument, a callback
can be provided for receiving the result.

~~~cl
;; Echo the result in the minibuffer.
(skewer-eval "Math.pow(2.1, 3.1)"
             (lambda (r) (message (cdr (assoc 'value r)))))
~~~

However, **the equivalent function in nrepl.el, `nrepl-eval`, is
synchronous!** It *returns* the evaluation result. "That's not true!
That's impossible!"

~~~cl
;; !!!
(plist-get (nrepl-eval "(Math/pow 2.1 3.1)") :value)
;; => "9.97423999265871"
~~~

Well, it turns out what I said above about execution contexts wasn't
completely true. There's exactly *one* sneaky function that breaks the
rule: `accept-process-output`. It blocks the current execution context
allowing some other execution contexts to run, including timers and
I/O. However, it will lock up Emacs' interface. `nrepl-eval` uses this
function to poll for a response from the nREPL process.

When I saw this, a lightbulb went off in my head. This lone loophole
in Emacs execution model can be abused to provide interesting
benefits. Specifically, it can be used to create a ***latch***
synchronization primitive.

The full source code is here if you want to dive right in. I'll be
going over a simplified version piece-by-piece below.

 * [https://github.com/skeeto/elisp-latch][latch.el]

### The Latch Primitive

The idea of a latch is that a thread can *wait* on the latch, blocking
its execution. It will remain in that state until another thread
*notifies* the latch, releasing any threads blocked on the
latch. Here's how it might look in Lisp.

~~~cl
(defvar result nil)

(defvar my-latch (make-latch))

(defun get-result ()
  (if result
      result
    (wait my-latch) ; Block, waiting for the result
    result))

(defun set-result (value)
  (setf result value)
  (notify my-latch)) ; Release anyone waiting on my-latch
~~~

The pattern above is similar to a ***promise***, which we will later
implement on top of latches. In our latch implementation I'd also like
to optionally pass a value from `notify` to anyone `wait`ing, which
would make the above simpler.

Emacs doesn't have threads but instead non-preemptive execution
contexts. Ignoring the Emacs UI lockup, we can mostly ignore that
distinction for now.

To exploit `accept-process-output` each latch needs to have its own
process object. When blocking on a latch it will simply wait for that
process to receive input. To notify a latch, we need to send data to
that process.

For the process, we'll ask Emacs to make a pseudo-terminal "process."
It's basically just a pipe for Emacs to talk to itself. It's possible
to literally make a pipe, which is better for this purpose, but
[that's currently broken][bug]. To make such a process, we call
`start-process` with `nil` as the program name (third argument).

Let's start by making a new class called `latch`.

~~~cl
(require 'eieio)

(defclass latch ()
  ((process :initform (start-process "latch" nil nil))
   (value :initform nil)))
~~~

This class has two slots, `process` and `value`. The process slot
holds the aforementioned process we'll be blocking on. The `value`
slot will be used to pass a value from `notify` to `wait`. The
`process` slot is initialized with a brand new process object upon
instantiation.

~~~cl
(defmethod wait ((latch latch))
  (accept-process-output (slot-value latch 'process))
  (slot-value latch 'value))

(defmethod notify ((latch latch) &optional value)
  (setf (slot-value latch 'value) value)
  (process-send-string (slot-value latch 'process) "\n"))
~~~

To wait, call `accept-process-output` on the latch's private
process. This function won't return until data is sent to the
process. By that time, the `value` slot will be filled in with the
value from `notify`.

To notify, send a newline with `process-send-string`. The data to send
is arbitrary, but I wanted to send as little as possible (one byte)
and I figure a newline might be safer when it comes to flushing any
sort of buffer. Buffers tend to flush on newlines. Before sending
data, we set the `value` slot to the value that `wait` will return.

That's basically it! However, processes are not garbage collected by
Emacs, so we need a `destroy` destructor method. The name `destroy`
here is not special to Emacs. It's something for the user of the
library to call.

~~~cl
(defmethod destroy ((latch latch))
  (ignore-errors
    (delete-process (slot-value latch 'process))))

(defun make-latch ()
  (make-instance 'latch))
~~~

I also made a convenience constructor function `make-latch`, with the
conventional name `make-`, since users shouldn't have to call
`make-instance` for our classes.

That's enough to turn `skewer-eval` into a synchronous function.

~~~cl
(defun skewer-eval-synchronously (js-code)
  (lexical-let ((latch (make-latch)))
    (skewer-eval js-code (apply-partially #'notify latch))
    (prog1 (wait latch)
      (destroy latch))))
~~~

In combination with `lexical-let`, `apply-partially` returns a closure
that will notify the latch with the return value passed to it from
skewer. We need to get the return value from `wait`, destroy the
latch, then return the value, so I use a `prog1` for this.

### One-use Latches

In my experimenting, I noticed the `prog1` pattern coming up a
lot. Having to destroy my latch after a single use was really
inconvenient. Fortunately this pattern can be captured by a subclass:
one-time-latch.

~~~cl
(defclass one-time-latch (latch)
  ())

(defun make-one-time-latch ()
  (make-instance 'one-time-latch))

(defmethod wait :after ((latch one-time-latch))
  (destroy latch))
~~~

This subclass destroys the latch after the superclass's `wait` is
done, through an `:after` method (purely for side-effects). CLOS is
fun, isn't it?

~~~cl
(defun skewer-eval-synchronously (js-code)
  (lexical-let ((latch (make-one-time-latch)))
    (skewer-eval js-code (apply-partially #'notify latch))
    (wait latch)))
~~~

There, that's a lot more elegant.

If eieio was a more capable mini-CLOS I could also demonstrate a
`countdown-latch`, but this would require an `:around` method. Most
uses of `notify` would need to skip over the superclass method.

### Promises

We can build promises on top of our latch implementation. Basically, a
promise is a one-time-latch where we can query the `notify` value more
than once. In a one-time-latch we can only `wait` once.

Our promise will have two similar methods, `deliver` (like notify),
and `retrieve` (like wait). If a value has been delivered already,
`retrieve` will return that value. Otherwise, it will block and wait
until a value is delivered,

~~~cl
(defclass promise ()
  ((latch :initform (make-one-time-latch))
   (delivered :initform nil)
   (value :initform nil)))

(defun make-promise ()
  (make-instance 'promise))
~~~

It has three slots, the one-time-latch used for blocking, a Boolean
determining the delivery status, and the `value` of the promise.

~~~cl
(defmethod deliver ((promise promise) value)
  (if (slot-value promise 'delivered)
      (error "Promise has already been delivered.")
    (setf (slot-value promise 'value) value)
    (setf (slot-value promise 'delivered) t)
    (notify (slot-value promise 'latch) value)))

(defmethod retrieve ((promise promise))
  (if (slot-value promise 'delivered)
      (slot-value promise 'value)
    (wait (slot-value promise 'latch))))
~~~

A promise can only be delivered once, so it throws an error if it is
attempted more than once. Otherwise it updates the promise state and
releases anything waiting on it.

### What to do with this?

Locking up Emacs' UI really limits the usefulness of this
library. Since Emacs' primary purpose is being a text editor, it needs
to remain very lively or else the user will become annoyed. If I used
a synchronous version of `skewer-eval`, Emacs would completely lock up
(easily interrupted with `C-g`) until the browser responds — which
would be never if no browser is connected. That's unacceptable.

Also, not very many Emacs functions have the callback pattern. The
only core function I'm aware of that does is `url-retrieve`, but it
already has a `url-retrieve-synchronously` counterpart.

Please tell me if you have a neat use of any of this!


[latch.el]: https://github.com/skeeto/elisp-latch
[bug]: http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=698096
