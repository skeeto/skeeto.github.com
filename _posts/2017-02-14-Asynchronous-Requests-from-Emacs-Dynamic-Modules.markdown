---
title: Asynchronous Requests from Emacs Dynamic Modules
layout: post
date: 2017-02-14T02:30:00Z
tags: [emacs, elisp, c, linux, win32]
uuid: 00a59e4f-268c-343f-e6c6-bb23cde265de
---

A few months ago I had a discussion with Vladimir Kazanov about his
[Orgfuse][orgfuse] project: a Python script that exposes an Emacs
Org-mode document as a [FUSE filesystem][fuse]. It permits other
programs to navigate the structure of an Org-mode document through the
standard filesystem APIs. I suggested that, with the new dynamic
modules in Emacs 25, Emacs *itself* could serve a FUSE filesystem. In
fact, support for FUSE services in general could be an package of his
own.

So that's what he did: [**Elfuse**][elfuse]. It's an old joke that
Emacs is an operating system, and here it is handling system calls.

However, there's a tricky problem to solve, an issue also present [my
joystick module][joymacs]. Both modules handle asynchronous events —
filesystem requests or joystick events — but Emacs runs the event loop
and owns the main thread. The external events somehow need to feed
into the main event loop. It's even more difficult with FUSE because
FUSE *also* wants control of its own thread for its own event loop.
This requires Elfuse to spawn a dedicated FUSE thread and negotiate a
request/response hand-off.

When a filesystem request or joystick event arrives, how does Emacs
know to handle it? The simple and obvious solution is to poll the
module from a timer.

~~~c
struct queue requests;

emacs_value
Frequest_next(emacs_env *env, ptrdiff_t n, emacs_value *args, void *p)
{
    emacs_value next = Qnil;
    queue_lock(requests);
    if (queue_length(requests) > 0) {
        void *request = queue_pop(requests, env);
        next = env->make_user_ptr(env, fin_empty, request);
    }
    queue_unlock(request);
    return next;
}
~~~

And then ask Emacs to check the module every, say, 10ms:

~~~cl
(defun request--poll ()
  (let ((next (request-next)))
    (when next
      (request-handle next))))

(run-at-time 0 0.01 #'request--poll)
~~~

Blocking directly on the module's event pump with Emacs' thread would
prevent Emacs from doing important things like, you know, *being a
text editor*. The timer allows it to handle its own events
uninterrupted. It gets the job done, but it's far from perfect:

1. It imposes an arbitrary latency to handling requests. Up to the
   poll period could pass before a request is handled.

2. Polling the module 100 times per second is inefficient. Unless you
   really enjoy recharging your laptop, that's no good.

The poll period is a sliding trade-off between latency and battery
life. If only there was some mechanism to, ahem, *signal* the Emacs
thread, informing it that a request is waiting…

### SIGUSR1

Emacs Lisp programs can handle the POSIX SIGUSR1 and SIGUSR2 signals,
which is exactly the mechanism we need. The interface is a "key"
binding on `special-event-map`, the keymap that handles these kinds of
events. When the signal arrives, Emacs queues it up for the main event
loop.

~~~cl
(define-key special-event-map [sigusr1]
  (lambda ()
    (interactive)
    (request-handle (request-next))))
~~~

The module blocks on its own thread on its own event pump. When a
request arrives, it queues the request, rings the bell for Emacs to
come handle it (`raise()`), and waits on a semaphore. For illustration
purposes, assume the module reads requests from and writes responses
to a file descriptor, like a socket.

~~~c
int event_fd = /* ... */;
struct request request;
sem_init(&request.sem, 0, 0);

for (;;) {
    /* Blocking read for request event */
    read(event_fd, &request.event, sizeof(request.event));

    /* Put request on the queue */
    queue_lock(requests);
    queue_push(requests, &request);
    queue_unlock(requests);
    raise(SIGUSR1);  // TODO: Should raise() go inside the lock?

    /* Wait for Emacs */
    while (sem_wait(&request.sem))
        ;

    /* Reply with Emacs' response */
    write(event_fd, &request.response, sizeof(request.response));
}
~~~

The `sem_wait()` is in a loop because signals will wake it up
prematurely. In fact, it may even wake up due to its own signal on the
line before. This is the only way this particular use of `sem_wait()`
might fail, so there's no need to check `errno`.

If there are multiple module threads making requests to the same
global queue, the lock is necessary to protect the queue. The
semaphore is only for blocking the thread until Emacs has finished
writing its particular response. Each thread has its own semaphore.

When Emacs is done writing the response, it releases the module thread
by incrementing the semaphore. It might look something like this:

~~~c
emacs_value
Frequest_complete(emacs_env *env, ptrdiff_t n, emacs_value *args, void *p)
{
    struct request *request = env->get_user_ptr(env, args[0]);
    if (request)
        sem_post(&request->sem);
    return Qnil;
}
~~~

The top-level handler dispatches to the specific request handler,
calling `request-complete` above when it's done.

~~~cl
(defun request-handle (next)
  (condition-case e
      (cl-ecase (request-type next)
        (:open  (request-handle-open  next))
        (:close (request-handle-close next))
        (:read  (request-handle-read  next)))
    (error (request-respond-as-error next e)))
  (request-complete))
~~~

This SIGUSR1+semaphore mechanism is roughly how Elfuse currently
processes requests.

### Making it work on Windows

Windows doesn't have signals. This isn't a problem for Elfuse since
Windows doesn't have FUSE either. Nor does it matter for Joymacs since
XInput isn't event-driven and always requires polling. But someday
someone will need this mechanism for a dynamic module on Windows.

Fortunately there's a solution: *input language change* events,
`WM_INPUTLANGCHANGE`. It's also on `special-event-map`:

~~~cl
(define-key special-event-map [language-change]
  (lambda ()
    (interactive)
    (request-process (request-next))))
~~~

Instead of `raise()` (or `pthread_kill()`), broadcast the window event
with `PostMessage()`. Outside of invoking the `language-change` key
binding, Emacs will ignore the event because WPARAM is 0 — it doesn't
belong to any particular window. We don't *really* want to change the
input language, after all.

~~~c
PostMessageA(HWND_BROADCAST, WM_INPUTLANGCHANGE, 0, 0);
~~~

Naturally you'll also need to replace the POSIX threading primitives
with the Windows versions (`CreateThread()`, `CreateSemaphore()`,
etc.). With a bit of abstraction in the right places, it should be
pretty easy to support both POSIX and Windows in these asynchronous
dynamic module events.


[joymacs]: /blog/2016/11/05/
[orgfuse]: https://github.com/vkazanov/toy-orgfuse
[elfuse]: https://github.com/vkazanov/elfuse
[fuse]: https://en.wikipedia.org/wiki/Filesystem_in_Userspace
