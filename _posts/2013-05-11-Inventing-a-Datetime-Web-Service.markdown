---
title: Inventing a Datetime Web Service
layout: post
tags: [javascript, web]
uuid: 578a8f22-8748-3dbf-e927-2cf43954fd2f
---

Recently I wanted to experiment with dates in a JavaScript web app.
The [JavaScript Date object][date] is a fairly decent tool for working
with dates. Unfortunately, it has some annoyances,

 * It doesn't play well with JSON. JSON.stringify() flattens it into a
   string, so the JSON.parse() on the other size doesn't turn it back
   into a Date object. I made a library, [ResurrectJS][ressurect], to
   deal with this.

 * Dates are mutable. The same mistake was made in Java in the last
   century. However, in the JavaScript world this isn't really a big
   deal. The language doesn't really support immutability well at the
   moment anyway. There is [Object.freeze()][freeze] but JavaScript
   engines don't optimize for it yet.

 * Inconsistent indexing. Months are 0-indexed while days are
   1-indexed. The date "2013-05-11" is awkwardly instantiated with the
   arguments `new Date(2013, 4, 11)`. This is another repeat of an
   early Java design mistake.

 * Date objects have timezones and there's no way to set the timezone.
   A Date represents an instance in time, regardless of the local
   timezone, and the timezone only matters when the Date is being
   formatted as a human-readable string. When formatting a Date into a
   string there's no way to specify the timezone. There's a
   `getTimezoneOffset()` method for asking about the Date's timezone,
   but no corresponding `setTimezoneOffset()`.

 * It relies on the local computer's time. This isn't actually a flaw
   in Date. Where *else* would it get the time? This just happened to
   be an obstacle for my particular experiment. This issue is also the
   purpose of this post.

### Existing Datetime Services

So if I don't trust the local system time to be precise, where can I
get a more accurate time? Surely there are web services out there for
it, right? NIST operates [time.gov](http://time.gov/) and maybe that
has a web API for web applications. I don't need to be super precise
— a web API could never be — just within a couple of seconds.

Turns out there isn't any such web service, at least not a reliable
one. Yahoo used to [provide one called getTime][yahoo], but it's been
shut down. In my searches I also came across this:

 * [http://json-time.appspot.com/time.json](http://json-time.appspot.com/time.json) ([GitHub][json-time])

It supports JSONP, which is almost exactly what I need. Unfortunately,
it's just a free Google App Engine app, so it's unavailable most of
the time due to being over quota. In fact, at the time of this writing
it is down.

I could stand up my own server for the task, but that costs both time
and money, so I'm not really interested in doing that. It's liberating
to build web apps that [don't require that I run a server][flu]. There
are so many nice web APIs out there that do the hard part for me. I
can just put my app on GitHub's free static hosting, like this blog.
The biggest obstacle is dealing with the same-origin policy. JSONP
isn't always supported and very few of these APIs support CORS, even
though they easily could. This is part of the web that's still
maturing. My personal guess is that WebSockets will end up filling
this role rather than CORS.

### Deriving a Datetime Service

So I was thinking about how I could get around this. Surely some API
out there includes a date in its response and I could just piggyback
off that. This is when the lightbulb went off: **web servers hand out
date strings all the time**! It's a standard HTTP header: `Date`! Even
[my own web server does this][simple-httpd].

~~~js
function getServerDate() {
    var xhr = new XMLHttpRequest();
    xhr.open('HEAD', '/?nocache=' + Math.random(), false);
    xhr.send();
    return new Date(xhr.getResponseHeader('Date'));
}
~~~

This makes a synchronous XMLHttpRequest to the page's host, being
careful to cache bust so that I'm not handed a stale date. I'm also
using a HEAD request to minimize the size of the response. Personally,
I trust the server's clock precision more than the client's. Here it
is in action.

<div>
Local: <b><span id="time-local" style="float: right;">---</span></b>
</div>
<div>
Server: <b><span id="time-server" style="float: right;">---</span></b>
</div>

This is probably not too exciting because you should be within a
couple of seconds of the server. If you're feeling ambitious, change
your local system time by a few minutes and refresh the page. The
server time should still be accurate while your local time is whatever
incorrect time you set.

<script>
var Demo = Demo || {};

Demo.getServerDate = function() {
    var xhr = new XMLHttpRequest();
    xhr.open('HEAD', '/?nocache=' + Math.random(), false);
    xhr.send();
    return new Date(xhr.getResponseHeader('Date'));
};

Demo.setDate = function(id, date) {
    document.getElementById(id).innerHTML = date;
};

Demo.offset = Demo.getServerDate() - Date.now();

setInterval(function() {
    var date = new Date();
    Demo.setDate('time-local', date);
    Demo.setDate('time-server', new Date(Demo.offset + date.valueOf()));
}, 1000 / 15);
</script>

Here's the code for these clocks:

~~~js
var Demo = Demo || {};

Demo.setDate = function(id, date) {
    document.getElementById(id).innerHTML = date;
};

Demo.offset = Demo.getServerDate() - Date.now();

setInterval(function() {
    var date = new Date();
    Demo.setDate('time-local', date);
    Demo.setDate('time-server', new Date(Demo.offset + date.valueOf()));
}, 1000 / 15);
~~~

You know what? I think this is better than some random datetime
web service anyway.


[date]: https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Global_Objects/Date
[freeze]: https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Global_Objects/Object/freeze
[ressurect]: /blog/2013/03/28/
[json-time]: https://github.com/simonw/json-time
[yahoo]: http://developer.yahoo.com/util/timeservice/V1/getTime.html
[flu]: /blog/2013/01/13/
[simple-httpd]: /blog/2009/05/17/
