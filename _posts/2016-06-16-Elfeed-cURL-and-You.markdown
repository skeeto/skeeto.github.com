---
title: Elfeed, cURL, and You
layout: post
date: 2016-06-16T18:22:16Z
tags: [emacs, elisp, elfeed]
uuid: 76942398-f693-3127-fd45-19d508b5c044
---

This morning I pushed out an important update to [Elfeed][elfeed], my
web feed reader for Emacs. The update should be available in MELPA by
the time you read this. Elfeed now has support for fetching feeds
using a [cURL][curl] through a `curl` inferior process. You'll need
the program in your PATH or configured through
`elfeed-curl-program-name`.

I've been using it for a couple of days now, but, while I work out the
remaining kinks, it's disabled by default. So in addition to having
cURL installed, you'll need to set `elfeed-use-curl` to non-nil.
Sometime soon it will be enabled by default whenever cURL is
available. The original `url-retrieve` fetcher will remain in place
for time time being. However, cURL *may* become a requirement someday.

Fetching with a `curl` inferior process has some huge advantages.

### It's much faster

The most obvious change is that you should experience a huge speedup
on updates and better responsiveness during updates after the first
cURL run. There are important two reasons:

**Asynchronous DNS and TCP**: Emacs 24 and earlier performs DNS
queries synchronously even for asynchronous network processes. This is
being fixed on some platforms (including Linux) in Emacs 25, but now
we don't have to wait.

On Windows it's even worse: the TCP connection is also established
synchronously. This is especially bad when fetching relatively small
items such as feeds, because the DNS look-up and TCP handshake dominate
the overall fetch time. It essentially makes the whole process
synchronous.

**Conditional GET**: HTTP has two mechanism to avoid transmitting
information that a client has previously fetched. One is the
Last-Modified header delivered by the server with the content. When
querying again later, the client echos the date back [like a
token][token] in the If-Modified-Since header.

The second is the "entity tag," an arbitrary server-selected token
associated with each version of the content. The server delivers it
along with the content in the ETag header, and the client hands it
back later in the If-None-Match header, sort of like a cookie.

This is highly valuable for feeds because, unless the feed is
particularly active, most of the time the feed hasn't been updated
since the last query. This avoids sending anything other hand a
handful of headers each way. In Elfeed's case, it means **it doesn't
have to parse the same XML over and over again**.

Both of these being outside of cURL's scope, Elfeed has to manage
conditional GET itself. I had no control over the HTTP headers until
now, so I couldn't take advantage of it. Emacs' `url-retrieve`
function allows for sending custom headers through dynamically binding
`url-request-extra-headers`, but this isn't available when calling
`url-queue-retrieve` since the request itself is created
asynchronously.

Both the ETag and Last-Modified values are stored in the database and
persist across sessions. This is the reason the full speedup isn't
realized until the second fetch. The initial cURL fetch doesn't have
these values.

### Fewer bugs

As mentioned previously, Emacs has a built-in URL retrieval library
called `url`. The central function is `url-retrieve` which
asynchronously fetches the content at an arbitrary URL (usually HTTP)
and delivers the buffer and status to a callback when it's ready.
There's also a queue front-end for it, `url-queue-retrieve` which
limits the number of parallel connections. Elfeed hands this function
a pile of feed URLs all at once and it fetches them N at a time.

Unfortunately both these functions are *incredibly* buggy. It's been a
thorn in my side for years.

Here's what the interface looks like for both:

~~~cl
(url-retrieve URL CALLBACK &optional CBARGS SILENT INHIBIT-COOKIES)
~~~

It takes a URL and a callback. Seeing this, the sane, unsurprising
expectation is the callback will be invoked *exactly once* for time
`url-retrieve` was called. In any case where the request fails, it
should report it through the callback. [This is not the case][invoke].
The callback may be invoked any number of times, *including zero*.

In this example, suppose you have a webserver that will return an HTTP
404 for a requested URL. Below, I fire off 10 asynchronous requests in a
row.

~~~cl
(defvar results ())
(dotimes (i 10)
  (url-retrieve "http://127.0.0.1:8080/404"
                (lambda (status) (push (cons i status) results))))
~~~

What would you guess is the length of `results`? It's initially 0
before any requests complete and over time (a very short time) I would
expect this to top out at 10. On Emacs 24, here's the real answer:

~~~cl
(length results)
;; => 46
~~~

The same error is reported multiple times to the callback. At least
the pattern is obvious.

~~~cl
(cl-count 0 results :key #'car)
;; => 9
(cl-count 1 results :key #'car)
;; => 8
(cl-count 2 results :key #'car)
;; => 7

(cl-count 9 results :key #'car)
;; => 1
~~~

Here's another one, this time to the non-existent foo.example. The DNS
query should never resolve.

~~~cl
(setf results ())
(dotimes (i 10)
  (url-retrieve "http://foo.example/"
                (lambda (status) (push (cons i status) results))))
~~~

What's the length of `results`? This time it's zero. Remember how DNS
is synchronous? Because of this, DNS failures are reported
synchronously as a signaled error. This gets a lot worse with
`url-queue-retrieve`. Since the request is put off until later, DNS
doesn't fail until later, and you get neither a callback nor an error
signal. This also puts the queue in a bad state and necessitated
`elfeed-unjam` for manually clear it. This one should get fixed in
Emacs 25 when DNS is asynchronous.

This last one assumes you don't have anything listening on port 57432
(pulled out of nowhere) so that the connection fails.

~~~cl
(setf results ())
(dotimes (i 10)
  (url-retrieve "http://127.0.0.1:57432/"
                (lambda (status) (push (cons i status) results))))
~~~

On Linux, we finally get the sane result of 10. However, on Windows,
it's zero. The synchronous TCP connection will fail, signaling an
error just like DNS failures. Not only is it broken, it's broken in
different ways on different platforms.

There are many more cases of callback weirdness which depend on the
connection and HTTP session being in various states when thing go
awry. These were just the easiest to demonstrate. By using cURL, I get
to bypass this mess.

### No more GnuTLS issues

At compile time, Emacs can optionally be linked against GnuTLS, giving
it robust TLS support so long as the shared library is available.
`url-retrieve` uses this for fetching HTTPS content. Unfortunately,
this library is noisy and will occasionally echo non-informational
messages in the minibuffer and in `*Messages*` that cannot be
suppressed.

When not linked against GnuTLS, Emacs will instead run the GnuTLS
command line program as an inferior process, just like Elfeed now does
with cURL. Unfortunately this interface is very slow and frequently
fails, basically preventing Elfeed from fetching HTTPS feeds. I
suspect it's in part due to an improper `coding-system-for-read`.

cURL handles all the TLS negotation itself, so both these problems
disappear. The compile-time configuration doesn't matter.

### Windows is now supported

Emacs' Windows networking code is so unstable, even in Emacs 25, that
I couldn't make any practical use of Elfeed on that platform. Even the
Cygwin emacs-w32 version couldn't cut it. It hard crashes Emacs every
time I've tried to fetch feeds. Fortunately the inferior process code
is a whole lot more stable, meaning fetching with cURL works great. As
of today, you can now use Elfeed on Windows. The biggest obstable is
getting cURL installed and configured.

### Interface changes

With cURL, obviously the values of `url-queue-timeout` and
`url-queue-parallel-processes` no longer have any meaning to Elfeed.
If you set these for yourself, you should instead call the functions
`elfeed-set-timeout` and `elfeed-set-max-connections`, which will do
the appropriate thing depending on the value of `elfeed-use-curl`.
Each also comes with a getter so you can query the current value.

The deprecated `elfeed-max-connections` has been removed.

Feed objects now have meta tags `:etag`, `:last-modified`, and
`:canonical-url`. The latter can identify feeds that have been moved,
though it needs a real UI.

### See any bugs?

If you use Elfeed, grab the current update and give the cURL fetcher a
shot. Please open a ticket if you find problems. Be sure to report
your Emacs version, operating system, and cURL version.

As of this writing there's just one thing missing compared to
url-queue: connection reuse. cURL supports it, so I just need to code
it up.


[elfeed]: https://github.com/skeeto/elfeed
[curl]: https://curl.haxx.se/
[invoke]: http://debbugs.gnu.org/cgi/bugreport.cgi?bug=20159
[token]: https://utcc.utoronto.ca/~cks/space/blog/web/IfModifiedSinceHowNot
