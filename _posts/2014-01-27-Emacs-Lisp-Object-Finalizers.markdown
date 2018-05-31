---
title: Emacs Lisp Object Finalizers
layout: post
date: 2014-01-27T05:24:16Z
tags: [emacs, elisp]
uuid: 48023a80-358c-39b4-371b-d74dfb248897
---

***Update**: Emacs 25.1 (released Sept. 2016) formally introduced
finalizers to Emacs Lisp. This article is left here for historical
purposes.

**Problem**: You have a special resource, such as a buffer or process,
associated with an Emacs Lisp object which is not managed by the
garbage collector. You want this resource to be cleaned up when the
owning lisp object is garbage collected. Unlike some other languages,
Elisp doesn't provide [finalizers][finalizer] for this job, so what do
you do?

**Solution**: This is Emacs Lisp. We can just add this feature to the
language ourselves!

I've already implemented this feature as a package called `finalize`,
available on MELPA. I will be using it as part of a larger, upcoming
project.

 * [https://github.com/skeeto/elisp-finalize](https://github.com/skeeto/elisp-finalize)

In this article I will describe how it works.

### Processes and Buffers

Process and buffers are special types of objects. Immediately after
instantiation these objects are added to a global list. They will
never become unreachable without explicitly being killed. The garbage
collector will never manage them for you.

This is a problem for APIs like those provided by the url package. The
functions `url-retrieve` and `url-retrieve-synchronously` create
buffers and hand them back to their callers. Ownership is transfered
to the caller and the caller must be careful to kill the buffer, or
transfer ownership again, before it returns. Otherwise the buffer is
"leaked." The url package tries to manage this a little bit with
`url-gc-dead-buffers`, but this can't be relied upon.

Another issue is when a process is started and is stored in a struct
or some other kind of object. There is probably a "close" function
that accepts one of these structs and kills the process. But if that
function isn't called, due to a bug or an error condition, it will
become a "dangling" process. If the struct is completely lost, it will
probably be inconvenient to deal with the process — the "close"
function is no longer useful.

### With Macros

A common way to deal with this problem is using a `with-` macro. This
macro establishes a resource, evaluates a body, and ensures the
resource is properly cleaned up regardless of the body's termination
state. The latter is accomplished using `unwind-protect`. For example,
`with-temp-buffer`,

~~~cl
;; Fetch the first 10 bytes of foo.txt
(with-temp-buffer
  (insert-file-contents "foo.txt" nil 0 10)
  (buffer-string))
~~~

This expands (roughly) to the following expression.

~~~cl
(let ((temp-buffer (generate-new-buffer "*temp*")))
  (with-current-buffer temp-buffer
    (unwind-protect
        (progn
          (insert-file-contents "foo.txt" nil 0 10)
          (buffer-string))
      (and (buffer-live-p temp-buffer)
           (kill-buffer temp-buffer)))))
~~~

For dealing with open files, Common Lisp has `with-open-stream`. It
establishes a binding for a new stream over its body and ensures the
stream is closed when the body is complete. There's no chance for a
stream to be left open, leaking a system resource.

However, `with-` macros aren't useful in asynchronous situations. In
Emacs this would be the case for asynchronous sub-processes, such as
an attached language interpreter. The extent of the process goes
beyond a single body.

### Finalizers

What would really be useful is to have a callback — a finalizer —
that runs when an object is garbage collected. This ensures that the
resource will not outlive its owner, restoring management back to the
garbage collector. However, Emacs provides no such hook.

Fortunately this feature can be built using weak hash tables and the
`post-gc-hook`, a list of functions that are run immediately after
garbage collection.

#### Weak References

I've discussed before [how to create weak references in Elisp][weak].
The only weak references in Emacs are built into weak hash tables.
Normally the language provides weak references first and hash tables
are built on top of them. With Emacs we do this backwards.

The `make-hash-table` function accepts a key argument `:weakness` to
specify how strongly keys and values should be held by the table. To
make a weak reference just create a hash table of size 1 and set
`:weakness` to t.

~~~cl
(defun weak-ref (thing)
  (let ((ref (make-hash-table :size 1 :weakness t :test 'eq)))
    (prog1 ref
      (setf (gethash t ref) thing))))

(defun deref (ref)
  (gethash t ref))
~~~

The same trick can be used to detect when an object is garbage
collected. If the result of `deref` is nil, then the object was
garbage collected. (Or the weakly-referenced object *is* nil, but this
object will never be garbage collected anyway.)

To check if we need to run a finalizer all we have to do is create a
weak reference to the object, then check the reference after garbage
collection. This check can be done in a `post-gc-hook` function.

#### Registration

To avoid cluttering up `post-gc-hook` with one closure per object
we'll keep a register of all watched objects.

~~~cl
(defvar finalizable-objects ())

(defun register (object callback)
  (push (cons (weak-ref object) callback) finalizable-objects))
~~~

Now a function to check for missing objects, `try-finalize`.

~~~cl
(defun try-finalize ()
  (let ((alive (cl-remove-if-not #'deref finalizable-objects :key #'car))
        (dead (cl-remove-if #'deref finalizable-objects :key #'car)))
    (setf finalizable-objects alive)
    (mapc #'funcall (mapcar #'cdr dead))))

(add-hook 'post-gc-hook #'try-finalize)
~~~

Now to try it out. Create a process, stuff it in a vector (like a
defstruct), register `delete-process` as a finalizer, and, for the
sake of demonstration, immediately forget the vector.

~~~cl
;;; -*- lexical-binding: t; -*-
(let ((process (start-process "ping" nil "ping" "localhost")))
  (register (vector process) (lambda () (delete-process process))))

;; Assuming the garbage collector has not already run.
(get-process "ping")
;; => #<process ping>

;; Force garbage collection.
(garbage-collect)

(get-process "ping")
;; => nil
~~~

The garbage collector killed the process for us!

There are some problems with this implementation. Using `cl-remove-if`
is unwise in a `post-gc-hook` function. It allocates lots of new cons
cells but garbage collection is inhibited while the function is run.
The docstring warns us:

> Garbage collection is inhibited while the hook functions run, so be
> careful writing them.

Similarly, all of the finalizers are run within the context of this
memory-sensitive hook. Instead they should be delayed until the next
evaluation turn (i.e. `run-at-time` of 0). Some of the finalizers
could also fail, which would cause the remaining finalizers to never
run. The real implementation deals with all of these issues.

A major drawback to these Emacs Lisp finalizers compared to other
languages is that the actual object is not available. We don't know
it's getting collected until after it's already gone. This solves the
object resurrection problem, but it's darn inconvenient. One possible
workaround in the case of defstructs and EIEIO objects is to make a
copy of the original object (`copy-sequence` or `clone`) and run the
finalizer on the copy as if it was the original.

### The Real Implementation

The real implementation is more carefully namespaced and its API has
just one function: `finalize-register`. It works just like `register`
above but it accepts `&rest` arguments to be passed to the finalizer.
This makes the registration call simpler and avoids some
[significant problems with closures][closure].

~~~cl
(let ((process (start-process "ping" nil "ping" "localhost")))
  (finalize-register (vector process) #'delete-process process))
~~~

Here's a more formal example of how it might really be used.

~~~cl
(cl-defstruct (pinger (:constructor pinger--create))
  process host)

(defun pinger-create (host)
  (let* ((process (start-process "pinger" nil "ping" host))
         (object (pinger--create :process process :host host)))
    (finalize-register object #'delete-process process)
    object))
~~~

To make things cleaner for EIEIO classes there's also a `finalizable`
mixin class that ensures the `finalize` generic function is called on
a copy of the object (the original object is gone) when it's garbage
collected.

Here's how it would be used for the same "pinger" concept, this time
as an EIEIO class. An advantage here is that anyone can manually call
`finalize` early if desired.

~~~cl
(require 'eieio)
(require 'finalizable)

(defclass pinger (finalizable)
  ((process :initarg :process :reader pinger-process)
   (host :initarg :host :reader pinger-host)))

(defun pinger-create (host)
  (make-instance 'pinger
                 :process (start-process "ping" nil "ping" host)
                 :host host))

(defmethod finalize ((pinger pinger))
  (delete-process (pinger-process pinger)))
~~~

It's a small package but I think it can be quite handy.


[finalizer]: http://en.wikipedia.org/wiki/Finalizer
[weak]: /blog/2012/12/17/
[closure]: /blog/2013/12/30/#the_readable_closures_catch
