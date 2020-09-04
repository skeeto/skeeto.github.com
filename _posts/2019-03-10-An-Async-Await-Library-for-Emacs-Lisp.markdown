---
title: 'An Async / Await Library for Emacs Lisp'
layout: post
date: 2019-03-10T20:57:03Z
tags: [emacs, elisp, lisp, python, javascript, lang, asyncio]
uuid: 5d1462fa-a30d-432e-9a4f-827eb67862b2
---

As part of [building my Python proficiency][py], I've learned how to
use [asyncio][asyncio]. This new language feature [first appeared in
Python 3.5][new] ([PEP 492][pep], September 2015). JavaScript grew [a
nearly identical feature][js] in ES2017 (June 2017). An async function
can pause to await on an asynchronously computed result, much like a
generator pausing when it yields a value.

In fact, both Python and JavaScript async functions are essentially just
fancy generator functions with some specialized syntax and semantics.
That is, they're [stackless coroutines][ss]. Both languages already had
generators, so their generator-like async functions are a natural
extension that — unlike [*stackful* coroutines][sf] — do not require
significant, new runtime plumbing.

Emacs [officially got generators in 25.1][iter] (September 2016),
though, unlike Python and JavaScript, it didn't require any additional
support from the compiler or runtime. It's implemented entirely using
Lisp macros. In other words, it's just another library, not a core
language feature. In theory, the generator library could be easily
backported to the first Emacs release to [properly support lexical
closures][lex], Emacs 24.1 (June 2012).

For the same reason, stackless async/await coroutines can also be
implemented as a library. So that's what I did, letting Emacs' generator
library do most of the heavy lifting. The package is called `aio`:

* **<https://github.com/skeeto/emacs-aio>**

It's modeled more closely on JavaScript's async functions than Python's
asyncio, with the core representation being *promises* rather than a
coroutine objects. I just have an easier time reasoning about promises
than coroutines.

I'm definitely [not the first person to realize this was
possible][aa], and was beaten to the punch by two years. Wanting to
[avoid fragmentation][curse], I set aside all formality in my first
iteration on the idea, not even bothering with namespacing my
identifiers. It was to be only an educational exercise. However, I got
quite attached to my little toy. Once I got my head wrapped around the
problem, everything just sort of clicked into place so nicely.

In this article I will show step-by-step one way to build async/await
on top of generators, laying out one concept at a time and then
building upon each. But first, some examples to illustrate the desired
final result.

### aio example

Ignoring [all its problems][url] for a moment, suppose you want to use
`url-retrieve` to fetch some content from a URL and return it. To keep
this simple, I'm going to omit error handling. Also assume that
`lexical-binding` is `t` for all examples. Besides, lexical scope
required by the generator library, and therefore also required by `aio`.

The most naive approach is to fetch the content synchronously:

```cl
(defun fetch-fortune-1 (url)
  (let ((buffer (url-retrieve-synchronously url)))
    (with-current-buffer buffer
      (prog1 (buffer-string)
        (kill-buffer)))))
```

The result is returned directly, and errors are communicated by an error
signal (e.g. Emacs' version of exceptions). This is convenient, but the
function will block the main thread, locking up Emacs until the result
has arrived. This is obviously very undesirable, so, in practice,
everyone nearly always uses the asynchronous version:

```cl
(defun fetch-fortune-2 (url callback)
  (url-retrieve url (lambda (_status)
                      (funcall callback (buffer-string)))))
```

The main thread no longer blocks, but it's a whole lot less
convenient. The result isn't returned to the caller, and instead the
caller supplies a callback function. The result, whether success or
failure, will be delivered via callback, so the caller must split
itself into two pieces: the part before the callback and the callback
itself. Errors cannot be delivered using a error signal because of the
inverted flow control.

The situation gets worse if, say, you need to fetch results from two
different URLs. You either fetch results one at a time (inefficient),
or you manage two different callbacks that could be invoked in any
order, and therefore have to coordinate.

*Wouldn't it be nice for the function to work like the first example,
but be asynchronous like the second example?* Enter async/await:

```cl
(aio-defun fetch-fortune-3 (url)
  (let ((buffer (aio-await (aio-url-retrieve url))))
    (with-current-buffer buffer
      (prog1 (buffer-string)
        (kill-buffer)))))
```

A function defined with `aio-defun` is just like `defun` except that
it can use `aio-await` to pause and wait on any other function defined
with `aio-defun` — or, more specifically, any function that returns a
promise. Borrowing Python parlance: Returning a promise makes a
function *awaitable*. If there's an error, it's delivered as a error
signal from `aio-url-retrieve`, just like the first example. When
called, this function returns immediately with a promise object that
represents a future result. The caller might look like this:

```cl
(defcustom fortune-url ...)

(aio-defun display-fortune ()
  (interactive)
  (message "%s" (aio-await (fetch-fortune-3 fortune-url))))
```

How wonderfully clean that looks! And, yes, it even works with
`interactive` like that. I can `M-x display-fortune` and a fortune is
printed in the minibuffer as soon as the result arrives from the
server. In the meantime Emacs doesn't block and I can continue my
work.

You can't do anything you couldn't already do before. It's just a
nicer way to organize the same callbacks: *implicit* rather than
*explicit*.

### Promises, simplified

The core object at play is the *promise*. Promises are already a
rather simple concept, but `aio` promises have been distilled to their
essence, as they're only needed for this singular purpose. More on
this later.

As I said, a promise represents a future result. In practical terms, a
promise is just an object to which one can subscribe with a callback.
When the result is ready, the callbacks are invoked. Another way to
put it is that *promises [reify][re] the concept of callbacks*. A
callback is no longer just the idea of extra argument on a function.
It's a first-class *thing* that itself can be passed around as a
value.

Promises have two slots: the final promise *result* and a list of
*subscribers*. A `nil` result means the result hasn't been computed
yet. It's so simple I'm not even [bothering with `cl-struct`][struct].

```cl
(defun aio-promise ()
  "Create a new promise object."
  (record 'aio-promise nil ()))

(defsubst aio-promise-p (object)
  (and (eq 'aio-promise (type-of object))
       (= 3 (length object))))

(defsubst aio-result (promise)
  (aref promise 1))
```

To subscribe to a promise, use `aio-listen`:

```cl
(defun aio-listen (promise callback)
  (let ((result (aio-result promise)))
    (if result
        (run-at-time 0 nil callback result)
      (push callback (aref promise 2)))))
```

If the result isn't ready yet, add the callback to the list of
subscribers. If the result is ready *call the callback in the next
event loop turn* using `run-at-time`. This is important because it
keeps all the asynchronous components isolated from one another. They
won't see each others' frames on the call stack, nor frames from
`aio`. This is so important that the [Promises/A+ specification][pa]
is explicit about it.

The other half of the equation is resolving a promise, which is done
with `aio-resolve`. Unlike other promises, `aio` promises don't care
whether the promise is being *fulfilled* (success) or *rejected*
(error). Instead a promise is resolved using a *value function* — or,
usually, a *value closure*. Subscribers receive this value function
and extract the value by invoking it with no arguments.

Why? This lets the promise's resolver decide the semantics of the
result. Instead of returning a value, this function can instead signal
an error, propagating an error signal that terminated an async function.
Because of this, the promise doesn't need to know how it's being
resolved.

When a promise is resolved, subscribers are each scheduled in their own
event loop turns in the same order that they subscribed. If a promise
has already been resolved, nothing happens. (Thought: Perhaps this
should be an error in order to catch API misuse?)

```cl
(defun aio-resolve (promise value-function)
  (unless (aio-result promise)
    (let ((callbacks (nreverse (aref promise 2))))
      (setf (aref promise 1) value-function
            (aref promise 2) ())
      (dolist (callback callbacks)
        (run-at-time 0 nil callback value-function)))))
```

If you're not an async function, you might subscribe to a promise like
so:

```cl
(aio-listen promise (lambda (v)
                      (message "%s" (funcall v))))
```

The simplest example of a non-async function that creates and delivers
on a promise is a "sleep" function:

```cl
(defun aio-sleep (seconds &optional result)
  (let ((promise (aio-promise))
        (value-function (lambda () result)))
    (prog1 promise
      (run-at-time seconds nil
                   #'aio-resolve promise value-function))))
```

Similarly, here's a "timeout" promise that delivers a special timeout
error signal at a given time in the future.

```cl
(defun aio-timeout (seconds)
  (let ((promise (aio-promise))
        (value-function (lambda () (signal 'aio-timeout nil))))
    (prog1 promise
      (run-at-time seconds nil
                   #'aio-resolve promise value-function))))
```

That's all there is to promises.

### Evaluate in the context of a promise

Before we get into pausing functions, lets deal with the slightly
simpler matter of delivering their return values using a promise. What
we need is a way to evaluate a "body" and capture its result in a
promise. If the body exits due to a signal, we want to capture that as
well.

Here's a macro that does just this:

```cl
(defmacro aio-with-promise (promise &rest body)
  `(aio-resolve ,promise
                (condition-case error
                    (let ((result (progn ,@body)))
                      (lambda () result))
                  (error (lambda ()
                           (signal (car error) ; rethrow
                                   (cdr error)))))))
```

The body result is captured in a closure and delivered to the promise.
If there's an error signal, it's "*rethrown*" into subscribers by the
promise's value function.

This is where Emacs Lisp has a serious weak spot. There's not really a
concept of rethrowing a signal. Unlike a language with explicit
exception objects that can capture a snapshot of the backtrace, the
original backtrace is completely lost where the signal is caught.
There's no way to "reattach" it to the signal when it's rethrown. This
is unfortunate because it would greatly help debugging if you got to see
the full backtrace on the other side of the promise.

### Async functions

So we have promises and we want to pause a function on a promise.
Generators have `iter-yield` for pausing an iterator's execution. To
tackle this problem:

1. Yield the promise to pause the iterator.
2. Subscribe a callback on the promise that continues the generator
   (`iter-next`) with the promise's result as the yield result.

All the hard work is done in either side of the yield, so `aio-await` is
just a simple wrapper around `iter-yield`:

```cl
(defmacro aio-await (expr)
  `(funcall (iter-yield ,expr)))
```

Remember, that `funcall` is here to extract the promise value from the
value function. If it signals an error, this propagates directly into
the iterator just as if it had been a direct call — minus an accurate
backtrace.

So `aio-lambda` / `aio-defun` needs to wrap the body in a generator
(`iter-lamba`), invoke it to produce a generator, then drive the
generator using callbacks. Here's a simplified, unhygienic definition of
`aio-lambda`:

```cl
(defmacro aio-lambda (arglist &rest body)
  `(lambda (&rest args)
     (let ((promise (aio-promise))
           (iter (apply (iter-lambda ,arglist
                          (aio-with-promise promise
                            ,@body))
                        args)))
       (prog1 promise
         (aio--step iter promise nil)))))
```

The body is evaluated inside `aio-with-promise` with the result
delivered to the promise returned directly by the async function.

Before returning, the iterator is handed to `aio--step`, which drives
the iterator forward until it delivers its first promise. When the
iterator yields a promise, `aio--step` attaches a callback back to
itself on the promise as described above. Immediately driving the
iterator up to the first yielded promise "primes" it, which is
important for getting the ball rolling on any asynchronous operations.

If the iterator ever yields something other than a promise, it's
delivered right back into the iterator.

```cl
(defun aio--step (iter promise yield-result)
  (condition-case _
      (cl-loop for result = (iter-next iter yield-result)
               then (iter-next iter (lambda () result))
               until (aio-promise-p result)
               finally (aio-listen result
                                   (lambda (value)
                                     (aio--step iter promise value))))
    (iter-end-of-sequence)))
```

When the iterator is done, nothing more needs to happen since the
iterator resolves its own return value promise.

The definition of `aio-defun` just uses `aio-lambda` with `defalias`.
There's nothing to it.

That's everything you need! Everything else in the package is merely
useful, awaitable functions like `aio-sleep` and `aio-timeout`.

### Composing promises

Unfortunately `url-retrieve` doesn't support timeouts. We can work
around this by composing two promises: a `url-retrieve` promise and
`aio-timeout` promise. First define a promise-returning function,
`aio-select` that takes a list of promises and returns (as another
promise) the first promise to resolve:

```cl
(defun aio-select (promises)
  (let ((result (aio-promise)))
    (prog1 result
      (dolist (promise promises)
        (aio-listen promise (lambda (_)
                              (aio-resolve
                               result
                               (lambda () promise))))))))
```

We give `aio-select` both our `url-retrieve` and `timeout` promises, and
it tells us which resolved first:

```cl
(aio-defun fetch-fortune-4 (url timeout)
  (let* ((promises (list (aio-url-retrieve url)
                         (aio-timeout timeout)))
         (fastest (aio-await (aio-select promises)))
         (buffer (aio-await fastest)))
    (with-current-buffer buffer
      (prog1 (buffer-string)
        (kill-buffer)))))
```

Cool! Note: This will not actually cancel the URL request, just move
the async function forward earlier and prevent it from getting the
result.

### Threads

Despite `aio` being entirely about managing concurrent, asynchronous
operations, it has nothing at all to do with threads — as in Emacs 26's
support for kernel threads. All async functions and promise callbacks
are expected to run *only* on the main thread. That's not to say an
async function can't await on a result from another thread. It just must
be [done very carefully][mod].

### Processes

The package also includes two functions for realizing promises on
processes, whether they be subprocesses or network sockets.

* `aio-process-filter`
* `aio-process-sentinel`

For example, this function loops over each chunk of output (typically
4kB) from the process, as delivered to a filter function:

```cl
(aio-defun process-chunks (process)
  (cl-loop for chunk = (aio-await (aio-process-filter process))
           while chunk
           do (... process chunk ...)))
```

Exercise for the reader: Write an awaitable function that returns a line
at at time rather than a chunk at a time. You can build it on top of
`aio-process-filter`.

I considered wrapping functions like `start-process` so that their `aio`
versions would return a promise representing some kind of result from
the process. However there are *so* many different ways to create and
configure processes that I would have ended up duplicating all the
process functions. Focusing on the filter and sentinel, and letting the
caller create and configure the process is much cleaner.

Unfortunately Emacs has no asynchronous API for writing output to a
process. Both `process-send-string` and `process-send-region` will block
if the pipe or socket is full. There is no callback, so you cannot await
on writing output. Maybe there's a way to do it with a dedicated thread?

Another issue is that the `process-send-*` functions [are
preemptible][sync], made necessary because they block. The
`aio-process-*` functions leave a gap (i.e. between filter awaits)
where no filter or sentinel function is attached. It's a consequence
of promises being single-fire. The gap is harmless so long as the
async function doesn't await something else or get preempted. This
needs some more thought.

***Update***: These process functions no longer exist and have been
replaced by a small framework for building chains of promises. See
`aio-make-callback`.

### Testing aio

The test suite for `aio` is a bit unusual. Emacs' built-in test suite,
ERT, doesn't support asynchronous tests. Furthermore, tests are
generally run in batch mode, where Emacs invokes a single function and
then exits rather than pump an event loop. Batch mode can only handle
asynchronous process I/O, not the async functions of `aio`. So it's
not possible to run the tests in batch mode.

Instead I hacked together a really crude callback-based test suite. It
runs in non-batch mode and writes the test results into a buffer
(run with `make check`). Not ideal, but it works.

One of the tests is a sleep sort (with reasonable tolerances). It's a
pretty neat demonstration of what you can do with `aio`:

```cl
(aio-defun sleep-sort (values)
  (let ((promises (mapcar (lambda (v) (aio-sleep v v)) values)))
    (cl-loop while promises
             for next = (aio-await (aio-select promises))
             do (setf promises (delq next promises))
             collect (aio-await next))))
```

To see it in action (`M-x sleep-sort-demo`):

```cl
(aio-defun sleep-sort-demo ()
  (interactive)
  (let ((values '(0.1 0.4 1.1 0.2 0.8 0.6)))
    (message "%S" (aio-await (sleep-sort values)))))
```

### Async/await is pretty awesome

I'm quite happy with how this all came together. Once I had the
concepts straight — particularly resolving to value functions —
everything made sense and all the parts fit together well, and mostly
by accident. That feels good.


[aa]: https://github.com/chuntaro/emacs-async-await
[asyncio]: https://docs.python.org/3/library/asyncio.html
[curse]: http://www.winestockwebdesign.com/Essays/Lisp_Curse.html
[iter]: /blog/2018/05/31/
[js]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/async_function
[lex]: /blog/2016/12/22/
[mod]: /blog/2017/02/14/
[new]: https://docs.python.org/3/whatsnew/3.5.html#whatsnew-pep-492
[pa]: https://promisesaplus.com/
[pep]: https://www.python.org/dev/peps/pep-0492/
[py]: /blog/2019/02/24/
[re]: https://en.wikipedia.org/wiki/Reification_(computer_science)
[sf]: /blog/2017/06/21/
[ss]: https://blog.varunramesh.net/posts/stackless-vs-stackful-coroutines/
[struct]: /blog/2018/02/14/
[sync]: /blog/2013/01/14/
[url]: /blog/2016/06/16/
