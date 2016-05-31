---
title: Memory Leaks with XMLHttpRequest Objects
layout: post
tags: [javascript]
uuid: 190774d0-ca74-39df-f51a-6618a0ab45b4
---

<p class="abstract">
I'm writing this post because I am not aware of any other article that
gets it right. All my searches result in misleading and factually
incorrect information. If you know of an article that does get it
right, please share it.
</p>

I really love jQuery. It is *by far* my favorite XML library — the
only one that's enjoyable to use, really. It cleans up a lot of the
Document Object Model's ugliness, along with a few other important
browser APIs. One such API is the misnamed HTTP request object,
[XMLHttpRequest (XHR)][xhr].

For those who are unfamiliar, this object is used to make HTTP
requests after the page has loaded, with direct access to the response
data. Here's the simplest use case for asynchronously fetching
JSON-encoded data from the server. As written, it will only work in
modern browsers. For backwards compatibility, feature sniffing would be
required and the `onreadystatechange` event would be used instead.

~~~javascript
var xhr = new XMLHttpRequest();
xhr.addEventListener('load', function() {
    var data = JSON.parse(xhr.responseText);
    // ...
});
xhr.open('GET', '/widgets/id/443424805.json', true);
xhr.send();
~~~

Here's what the jQuery version looks like. Notice how it's a lot more
functional, passing the server response data as an argument rather
than gluing it to a mutable object. Of course, underneath it's just
using an XHR object, performing lots of feature sniffing to normalize
its behavior across different browsers.

~~~javascript
$.getJSON('/widgets/id/443424805.json', function(data) {
    // ...
});

// Or using the generic jQuery AJAX API:

$.ajax({
    dataType: 'json',
    url: '/widgets/id/443424805.json',
    success: function(data) {
        // ...
    }
});
~~~

This is what the core AJAX API *should* have looked like. The
XMLHttpRequest API has a critical flaw: it's at odds with garbage
collection. It's a strange, special object that gets to survive after
all JavaScript references to it are lost. Normal JavaScript objects
don't behave like this.

Also strange is that, while many people have observed XHR memory
leaks, **very few people understand what's going on**! Try doing some
searches for XHR memory leaks. You'll see answers talking about
**closures and reference cycles**, but they **have nothing to do with
XHR-related memory leaks**. It's [the blind leading the blind][blind].

### Closures

Let's quickly review what is *not* the problem. A closure is a
function that retains its lexical environment, *closing over* its
non-local variables.

~~~javascript
function makeCounter() {
    var x = 0;
    return function() {
        return ++x;
    };
}
~~~

When `makeCounter()` is called, a *binding* named `x` is established,
initially bound to the value 0 — an *assignment*. Then a closure is
created by the `function` expression, capturing this binding, and the
closure is returned. This would normally be the end of life for the
newly established binding, open for garbage collection, but it was
captured by the closure. This entire process happens on *each*
invocation of `makeCounter()`.

~~~javascript
var counterA = makeCounter();
var counterB = makeCounter();
counterA();  // => 1
counterA();  // => 2
counterA();  // => 3
counterB();  // => 1
~~~

When the returned closure, here assigned to `counterA` and another to
`counterB`, is invoked, it reassigns `x` to a new value, then returns
that value. `x` has become a truly private variable for each closure,
completely inaccessible except through this single call.

Closures *can* capture more values than the programmer intended, which
will cause the captures values to live longer than expected — a
leak. Fortunately, this is unusual. Consider this function.

~~~javascript
function makeGreeter(name) {
    var greeting = "Hello, " + name;
    return function() {
        return greeting;
    };
}
~~~

The body of `makeGreeter()` has two bindings, `name` and
`greeting`. Theoretically, the closure will capture `name` as well as
`greeting` because they're both part of its lexical environment. The
value assigned to `name` could live longer than intended. In practice,
this is not the case. Compilers are smart enough to see that the
closure makes no reference to `name` —
[so long as eval isn't present][eval].

### Circular References

With closures in mind, consider the typical use case for an XHR.

~~~javascript
function getText(url, callback) {
    var xhr = new XMLHttpRequest();
    xhr.onload = function() {
        callback(xhr.responseText);
    }
    xhr.open('get', url, true);
    xhr.send();
}
~~~

A binding named `xhr` is established and a closure is created which
references `xhr` as a *free variable*, so it gets captured. This
closure is assigned to a property on the XHR. This is a *circular
reference*. The XHR references the closure through `onload` and the
closure references the XHR through the closed-over variable
`xhr`.

Under some forms of memory management, such as reference counting,
this could be an issue. Fortunately, JavaScript implementations can
handle this situation just fine ([well, except before IE8][ie]).
Garbage collectors operate on *reachability*. Cycles don't matter, the
collector only cares if any part of the cycle is reachable by a
*root*, a hard reference from where the collector begins its search.

Browser JavaScript can't afford to get hung up on cycles. The DOM is
loaded with circularity; parent nodes reference their children and
child nodes reference their parents.

### What's Really Going On

Take another close look at `getText()`. Two objects are created, an
XHR and a closure, they're not assigned to anywhere outside of the
function, and *nothing is returned*. Under normal circumstances, this
means these two objects are free to be garbage collected. A compiler
could determine this through [*escape analysis*][escape] and perform
extra optimizations, such as stack allocation. However, XHR instances
are special objects so these are not normal circumstances.

This is an asynchronous request and JavaScript is
single-threaded. When `getText()` is invoked, no HTTP request is
actually made until sometime after `getText()` exits. What would
happen if the XHR and closure were garbage collected before the server
responded? Since the callback doesn't exist any more, *at best* the
response would be lost. If this was the case, users would need to be
careful to maintain a reference to the XHR, lest they risk losing
data. This is not how it works.

Instead, **the browser keeps an inaccessible, internal reference to
the XHR** (i.e. a garbage collection *root*). This keeps not only the
XHR alive for the duration of the request, but also the closure that
it references. After the response comes in, it's also keeping the
possibly-large response data alive as well. This, ladies and
gentlemen, is the dreaded XHR leak. It's now completely up to the
browser to decide when to free these objects. Older versions of
Internet Explorer, all the way up through IE7, appear to keep these
references around much, much longer than necessary, possibly forever!

### Experimenting with XHRs

At the time of this writing, the specialness of the XHR object can be
demonstrated by the current browsers. I'm going to use Chrome/Chromium
here since it's got the best tools for observing the internals.

~~~javascript
function makeMany(type) {
    for (var i = 0; i < 1000000; i++) {
        new type();
    }
}
~~~

The function `makeMany()` creates a million objects of a given type,
but retains no reference to any of them. In theory, they're free for
garbage collection as soon as they're created.

~~~javascript
function Point(x, y, z) {
    this.x = x;
    this.y = y;
    this.z = z;
}
~~~

According to Chrome's heap profiler, an XHR instance is 24 bytes and
instances of this Point prototype are each 56 bytes. When I ask
`makeMany()` to generate a million Point objects, the browser does it
trivially.

~~~javascript
makeMany(Point);
// Chrome easily asks, "Do you even lift?"
~~~

Let's try the same thing with XHR, which according to the profiler
should use even less memory. If XHR wasn't a special object, the
result would be the same. Note that there are no closures or circular
references created here.

~~~javascript
makeMany(XMLHttpRequest);
// Chrome starts thrashing my craptop now
~~~

If I run this a few times in a row Chromium's memory usage blows up
past a gigabyte and my whole computer starts thrashing. I begin to
worry about when I last saved this blog post's buffer. If it manages
to survive the thrashing, Chromium does eventually free these XHR
instances. There's some logic buried in there to determine if they're
safe to let go, but it doesn't make this check until very late.

To work around this, the [Closure Library pools XHRs][pool].
Disciplined re-use of XHR objects can free the developer from relying
on the browser to dispose of old XHR objects. Browsers limit the
number of simultaneous HTTP requests to somewhere between 2 and 5, so
XHR pools should typically stay very small.

### Conclusion

I hope this clears up some of the confusion on this subject. I'd like
to learn a lot more about what's going on, but this all appears to
only be documented as source code (if that), which is a slow way to
absorb this sort of information. If you know of any informed articles
or documentation on the subject, please share!


[xhr]: https://developer.mozilla.org/en-US/docs/DOM/XMLHttpRequest
[blind]: http://stackoverflow.com/questions/10673530/
[eval]: /blog/2012/11/14/
[escape]: http://en.wikipedia.org/wiki/Escape_analysis
[pool]: http://closure-library.googlecode.com/svn/docs/class_goog_net_XhrManager.html
[ie]: http://msdn.microsoft.com/en-us/library/dd361842(v=vs.85).aspx
