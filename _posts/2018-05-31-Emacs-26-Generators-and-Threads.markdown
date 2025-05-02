---
title: Emacs 26 Brings Generators and Threads
layout: post
date: 2018-05-31T17:45:16Z
tags: [emacs, elisp, lisp, lang]
uuid: 395c5e11-2088-32fa-53c8-0c749dca2064
excerpt_separator: <!--more-->
---

Emacs 26.1 was [recently released][e26]. As you would expect from a
major release, it comes with lots of new goodies. Being [a bit of an
Emacs Lisp enthusiast][tags], the two most interesting new features
are [generators][miter] (`iter`) and [native threads][mthread]
(`thread`).

**Correction**: Generators were actually introduced in Emacs 25.1
(Sept. 2016), not Emacs 26.1. Doh!

**Update**: [ThreadSanitizer (TSan)][tsan] quickly shows that Emacs'
threading implementation has many data races, making it [completely
untrustworthy][race]. Until this is fixed, ***nobody* should use Emacs
threads for any purpose**, and threads should disabled at compile time.

<!--more-->

### Generators

Generators are one of those cool language features that provide a lot of
power at a small implementation cost. They're like a constrained form of
coroutines, but, unlike coroutines, they're typically built entirely on
top of first-class functions (e.g. closures). This means *no additional
run-time support is needed* in order to add generators to a language.
The only complications are the changes to the compiler. Generators are
not compiled the same way as normal functions despite looking so
similar.

What's perhaps coolest of all about lisp-family generators, including
Emacs Lisp, is that the compiler component can be *implemented
entirely with macros*. The compiler need not be modified at all,
making generators no more than a library, and not actually part of the
language. That's exactly how they've been implemented in Emacs Lisp
(`emacs-lisp/generator.el`).

So what's a generator? It's a function that returns an *iterator
object*. When an iterator object is invoked (e.g. `iter-next`) it
evaluates the body of the generator. Each iterator is independent.
What makes them unusual (and useful) is that the evaluation is
*paused* in the middle of the body to return a value, saving all the
internal state in the iterator. Normally pausing in the middle of
functions isn't possible, which is what requires the special compiler
support.

Emacs Lisp generators appear to be most closely modeled after [Python
generators][py], though it also shares some similarities to
[JavaScript generators][js]. What makes it most like Python is the use
of signals for flow control — something I'm [not personally enthused
about][fc]. When a Python generator
completes, it throws a `StopItertion` exception. In Emacs Lisp, it's
an `iter-end-of-sequence` signal. A signal is out-of-band and avoids
the issue relying on some special in-band value to communicate the end
of iteration.

In contrast, JavaScript's solution is to return a "rich" object wrapping
the actual yield value. This object has a `done` field that communicates
whether iteration has completed. This avoids the use of exceptions for
flow control, but the caller has to unpack the rich object.

Fortunately the flow control issue isn't normally exposed to Emacs Lisp
code. Most of the time you'll use the `iter-do` macro or (my preference)
the new `cl-loop` keyword `iter-by`.

To illustrate how a generator works, here's a really simple iterator
that iterates over a list:

```cl
(iter-defun walk (list)
  (while list
    (iter-yield (pop list))))
```

Here's how it might be used:

```cl
(setf i (walk '(:a :b :c)))

(iter-next i)  ; => :a
(iter-next i)  ; => :b
(iter-next i)  ; => :c
(iter-next i)  ; error: iter-end-of-sequence
```

The iterator object itself is *opaque* and you shouldn't rely on any
part of its structure. That being said, I'm a firm believer that we
should understand how things work underneath the hood so that we can
make the most effective use of at them. No program should rely on the
particulars of the iterator object internals for *correctness*, but a
well-written program should employ them in a way that [best exploits
their expected implementation][fast].

Currently iterator objects are closures, and `iter-next` invokes the
closure with its own internal protocol. It asks the closure to return
the next value (`:next` operation), and `iter-close` asks it to clean
itself up (`:close` operation).

Since they're just closures, another *really* cool thing about Emacs
Lisp generators is that [iterator objects are generally readable][read].
That is, you can serialize them out with `print` and bring them back to
life with `read`, even in another instance of Emacs. They exist
independently of the original generator function. This will not work if
one of the values captured in the iterator object is not readable (e.g.
buffers).

How does pausing work? Well, one of other exciting new features of
Emacs 26 is the introduction of a jump table opcode, `switch`. I'd
lamented in the past that large `cond` and `cl-case` expressions could
be a lot more efficient if Emacs' byte code supported jump tables. It
turns an O(n) sequence of comparisons into an O(1) lookup and jump.
It's essentially the perfect foundation for a generator since it can
be used to [jump straight back to the position][jump] where evaluation
was paused.

*Buuut*, generators do not currently use jump tables. The generator
library predates the new `switch` opcode, and, being independent of it,
its author, Daniel Colascione, went with the best option at the time.
Chunks of code between yields are packaged as individual closures. These
closures are linked together a bit like nodes in a graph, creating a
sort of state machine. To get the next value, the iterator object
invokes the closure representing the next state.

I've *manually* macro expanded the `walk` generator above into a form
that *roughly* resembles the expansion of `iter-defun`:

```cl
(defun walk (list)
  (let (state)
    (cl-flet* ((state-2 ()
                 (signal 'iter-end-of-sequence nil))
               (state-1 ()
                 (prog1 (pop list)
                   (when (null list)
                     (setf state #'state-2))))
               (state-0 ()
                 (if (null list)
                     (state-2)
                   (setf state #'state-1)
                   (state-1))))
      (setf state #'state-0)
      (lambda ()
        (funcall state)))))
```

This omits the protocol I mentioned, and it doesn't have yield results
(values passed to the iterator). The actual expansion is a whole lot
messier and less optimal than this, but hopefully my hand-rolled
generator is illustrative enough. Without the protocol, this iterator is
stepped using `funcall` rather than `iter-next`.

The `state` variable keeps track of where in the body of the generator
this iterator is currently "paused." Continuing the iterator is
therefore just a matter of invoking the closure that represents this
state. Each state closure may update `state` to point to a new part of
the generator body. The terminal state is obviously `state-2`. Notice
how state transitions occur around branches.

I had said generators can be implemented as a library in Emacs Lisp.
Unfortunately theres a hole in this: `unwind-protect`. It's not valid to
yield inside an `unwind-protect` form. Unlike, say, a throw-catch,
there's no mechanism to trap an unwinding stack so that it can be
restarted later. The state closure needs to return and fall through the
`unwind-protect`.

A jump table version of the generator might look like the following.
I've used `cl-labels` since it allows for recursion.

```cl
(defun walk (list)
  (let ((state 0))
    (cl-labels
        ((closure ()
           (cl-case state
             (0 (if (null list)
                    (setf state 2)
                  (setf state 1))
                (closure))
             (1 (prog1 (pop list)
                  (when (null list)
                    (setf state 2))))
             (2 (signal 'iter-end-of-sequence nil)))))
      #'closure)))
```

When byte compiled on Emacs 26, that `cl-case` is turned into a jump
table. This "switch" form is closer to how generators are implemented in
other languages.

Iterator objects can [share state between themselves][clos] if they
close over a common environment (or, of course, use the same global
variables).

```cl
(setf foo
      (let ((list '(:a :b :c)))
        (list
         (funcall
          (iter-lambda ()
            (while list
              (iter-yield (pop list)))))
         (funcall
          (iter-lambda ()
            (while list
              (iter-yield (pop list))))))))

(iter-next (nth 0 foo))  ; => :a
(iter-next (nth 1 foo))  ; => :b
(iter-next (nth 0 foo))  ; => :c
```

For years there has been a *very* crude way to "pause" a function and
allow other functions to run: `accept-process-output`. It only works in
the context of processes, but five years ago this was [sufficient for me
to build primitives on top of it][latch]. Unlike this old process
function, generators do not block threads, including the user interface,
which is really important.

### Threads

Emacs 26 also bring us threads, which have been attached in a very
bolted on fashion. It's not much more than a subset of pthreads: shared
memory threads, recursive mutexes, and condition variables. The
interfaces look just like they do in pthreads, and there hasn't been
much done to integrate more naturally into the Emacs Lisp ecosystem.

This is also only the first step in bringing threading to Emacs Lisp.
Right now there's effectively a global interpreter lock (GIL), and
threads only run one at a time cooperatively. Like with generators, the
Python influence is obvious. In theory, sometime in the future this
interpreter lock will be removed, making way for actual concurrency.

This is, again, where I think it's useful to contrast with JavaScript,
which was also initially designed to be single-threaded. Low-level
threading primitives weren't exposed — though mostly because
JavaScript typically runs sandboxed and there's no safe way to expose
those primitives. Instead it got a [web worker API][ww] that exposes
concurrency at a much higher level, along with an efficient interface
for thread coordination.

For Emacs Lisp, I'd prefer something safer, more like the JavaScript
approach. Low-level pthreads are now a great way to wreck Emacs with
deadlocks (with no `C-g` escape). Playing around with the new
threading API for just a few days, I've already had to restart Emacs a
bunch of times. Bugs in Emacs Lisp are normally a lot more forgiving.

One important detail that has been designed well is that dynamic
bindings are thread-local. This is really essential for correct
behavior. This is also an easy way to create thread-local storage
(TLS): dynamically bind variables in the thread's entrance function.

```cl
;;; -*- lexical-binding: t; -*-

(defvar foo-counter-tls)
(defvar foo-path-tls)

(defun foo-make-thread (path)
  (make-thread
   (lambda ()
     (let ((foo-counter-tls 0)
           (foo-name-tls path))
       ...))))
```

However, **`cl-letf` "bindings" are *not* thread-local**, which makes
this [otherwise incredibly useful macro][flet] quite dangerous in the
presence of threads. This is one way that the new threading API feels
bolted on.

#### Building generators on threads

In [my stack clashing article][clash] I showed a few different ways to
add coroutine support to C. One method spawned per-coroutine threads,
and coordinated using semaphores. With the new threads API in Emacs,
it's possible to do exactly the same thing.

Since generators are just a limited form of coroutines, this means
threads offer another, *very* different way to implement them. The
threads API doesn't provide semaphores, but condition variables can fill
in for them. To "pause" in the middle of the generator, just wait on a
condition variable.

So, naturally, I just had to see if I could make it work. I call it a
"thread iterator" or "thriter." The API is *very* similar to `iter`:

**<https://github.com/skeeto/thriter>**

This is merely a proof of concept so don't actually use this library
for anything. These thread-based generators are about 5x slower than
`iter` generators, and they're a lot more heavy-weight, needing an
entire thread per iterator object. This makes `thriter-close` all the
more important. On the other hand, these generators have no problem
yielding inside `unwind-protect`.

Originally this article was going to dive into the details of how
these thread-iterators worked, but `thriter` turned out to be quite a
bit more complicated than I anticipated, especially as I worked
towards feature matching `iter`.

The gist of it is that each side of a next/yield transaction gets its
own condition variable, but share a common mutex. Values are passed
between the threads using slots on the iterator object. The side that
isn't currently running waits on a condition variable until the other
side frees it, after which the releaser waits on its own condition
variable for the result. This is similar to [asynchronous requests in
Emacs dynamic modules][sem].

Rather than use signals to indicate completion, I modeled it after
JavaScript generators. Iterators return a cons cell. The car indicates
continuation and the cdr holds the yield result. To terminate an
iterator early (`thriter-close` or garbage collection), `thread-signal`
is used to essentially "cancel" the thread and knock it off the
condition variable.

Since threads aren't (and shouldn't be) garbage collected, failing to
run a thread-iterator to completion would normally cause a memory leak,
as the thread [sits there forever waiting on a "next" that will never
come][bark]. To deal with this, there's a finalizer is attached to the
iterator object in such a way that it's not visible to the thread. A
lost iterator is eventually cleaned up by the garbage collector, but, as
usual with finalizers, this is [only a last resort][cw].

#### The future of threads

This thread-iterator project was my initial, little experiment with
Emacs Lisp threads, similar to why I [connected a joystick to Emacs
using a dynamic module][joy]. While I don't expect the current thread
API to go away, it's not really suitable for general use in its raw
form. Bugs in Emacs Lisp programs should virtually never bring down
Emacs and require a restart. Outside of threads, the few situations
that break this rule are very easy to avoid (and very obvious that
something dangerous is happening). Dynamic modules are dangerous by
necessity, but concurrency doesn't have to be.

There really needs to be a safe, high-level API with clean thread
isolation. Perhaps this higher-level API will eventually build on top of
the low-level threading API.


[bark]: https://www.youtube.com/watch?v=AK3PWHxoT_E
[clash]: /blog/2017/06/21/
[clos]: /blog/2017/12/14/
[cw]: https://utcc.utoronto.ca/~cks/space/blog/programming/GoFinalizersStopLeaks
[e26]: https://lists.gnu.org/archive/html/emacs-devel/2018-05/msg00765.html
[fast]: /blog/2017/01/30/
[fc]: http://wiki.c2.com/?DontUseExceptionsForFlowControl
[final]: /blog/2014/01/27/
[flet]: /blog/2017/10/27/
[joy]: /blog/2016/11/05/
[js]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Iterators_and_Generators
[jump]: https://www.chiark.greenend.org.uk/~sgtatham/coroutines.html
[latch]: /blog/2013/01/14/
[miter]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Generators.html
[mthread]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Threads.html
[py]: https://wiki.python.org/moin/Generators
[race]: https://hboehm.info/boehm-hotpar11.pdf
[read]: /blog/2013/12/30/
[sem]: /blog/2017/02/14/
[tags]: /tags/emacs/
[tsan]: https://github.com/google/sanitizers/wiki/ThreadSanitizerCppManual
[ww]: /blog/2013/01/26/
