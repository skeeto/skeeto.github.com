---
title: Userspace Threading in JavaScript
layout: post
tags: [javascript, lisp]
uuid: 5f81e774-9521-30a6-f816-62a7a3edcf10
---

There was an interesting Daily Programmer problem posted a couple of
weeks ago: [write a userspace threading library][dp]. I decided to do
it in JavaScript, building it on top of `setTimeout`. Remember that
JavaScript is single-threaded by specification, so this will be a
nonpreemptive, cooperative system.

Start by creating the Thread prototype. As thread constructors usually
work, it accepts the function to be run in that thread.

~~~javascript
function Thread(f) {
    this.alive = true;
    this.schedule(f);
}
~~~

The `schedule` method schedules a function to be run in that thread.
It's not really meant for users to use directly. I'll define it in a
moment.

Only one thread actually runs at a time, so globally keep track of the
which one is running at the moment.

~~~javascript
Thread.current = null;
~~~

Now here's the core method that makes everything work, `runner`. It
accepts a function of arbitrary arity and returns a function that runs
the provided function in `this` thread.

~~~javascript
Thread.prototype.runner = function(f) {
    var _this = this;
    return function() {
        if (_this.alive) {
            try {
                Thread.current = _this;
                f.apply(this, arguments);
            } finally {
                Thread.current = null;
            }
        }
    };
};
~~~

The runner sets the current thread to the proper value, calls the
function, then clears the current thread. If the thread is no longer
active, nothing happens.

With that in place, `schedule` is defined like this,

~~~javascript
Thread.prototype.schedule = function(f) {
    setTimeout(this.runner(f), 0);
};
~~~

It creates a runner function for `f` and schedules it to run as soon
as possible on JavaScript's event loop using `setTimeout`. Queuing up
on the event loop is the cooperative part of all this. Other threads
and events may already be queued with a timeout of 0, so they run
first.

Technically this is all that's needed. To yield, schedule a function
and return.

~~~javascript
function() {
    // ... do some work ...
    Thread.current.schedule(function() {
        // ... do more work ...
    });
}
~~~

I don't want the user to need to think about `Thread.current`, so
here's a convenience `yield` function.

~~~javascript
Thread.yield = function(f) {
    Thread.current.schedule(f);
};
~~~

Now to use it,

~~~javascript
function() {
    // ... do some work ...
    Thread.yield(function() {
        // ... do more work ...
    });
}
~~~

Halting a thread is easy. Any scheduled functions for this thread will
not be invoked, as specified in the `runner` method.

~~~javascript
Thread.prototype.destroy = function() {
    this.alive = false;
};
~~~

There's one more situation to worry about: callbacks. Imagine an
asynchronous storage API.

~~~javascript
// ... in thread context ...
storage.getValue(function(value) {
    // doesn't run in thread context
});
~~~

In order to run in the thread the library user would need to create a
`runner` function for the current thread. To avoid making them worry
about `Thread.current` and `runner`, provide another convenience
function, `wrap`. There may be a better name for it, but I couldn't
think of it.

~~~javascript
Thread.wrap = function(f) {
    return Thread.current.runner(f);
};
~~~

Fixing the callback,

~~~javascript
// ... in thread context ...
storage.getValue(Thread.wrap(function(value) {
    // ... also in thread context ...
}));
~~~

### Threading Demo

To demonstrate threading I'll make a thread that continuously fetches
random numbers from a server and displays them.

Here's a [simple-httpd](/blog/2012/08/20/) servlet for generating
numbers. The route for this servlet will be `/random`.

~~~cl
(defservlet random text/plain ()
  (princ (random* 1.0)))
~~~

Since I'm doing this interactively with [Skewer](/blog/2012/10/31/) on
the blank demo page, make a tag for displaying the number.

~~~javascript
var h1 = document.createElement('h1');
document.body.appendChild(h1);
~~~

Here's the function that will run in the thread. It fetches a number
asynchronously, displays it, then recurses. Notice that
`Thread.yield()` acts like a trampoline, providing free tail-call
optimization! This is because the stack is cleared before the provided
function is invoked.

~~~javascript
function random() {
    var xhr = new XMLHttpRequest();
    xhr.open('GET', '/random', true);
    xhr.send();
    xhr.onload = Thread.wrap(function() {
        h1.innerHTML = xhr.responseText;
        Thread.yield(random);
    });
};
~~~

I set `onload` after calling `send` just for code organization
purposes. That code is evaluated *after* `send` is called. As far as I
know this should work fine.

Now to create a thread!

~~~javascript
var foo = new Thread(random);
~~~

The heading flashes with random numbers as soon as the thread is
created. Even though this thread is continuously running, it's
frequently yielding. Everything remains responsive, including the
ability to stop the thread.

~~~javascript
foo.destroy();
~~~

As soon as this is evaluated, the heading stops being updated. I think
that's pretty neat!

### Performance

I haven't tested performance, but I imagine it's awful. Especially
because of that frequent use of the `apply` method. You wouldn't want
CPU-intensive operations to cooperate like this. Fortunately, in my
demo above I'm manipulating the DOM and waiting on a server response,
so the performance penalties of threading should be negligible.


[dp]: http://redd.it/1ceai7
