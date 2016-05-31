---
title: simple-httpd and impatient-mode
layout: post
tags: [emacs, elisp]
uuid: 627e438d-36e5-3a9d-dd35-6a9a3914a63a
---

After [settling in with MELPA](/blog/2012/08/12/) I wanted to see
about into turning [my Emacs web server](/blog/2009/05/17/) into an
installable package. Someone had already uploaded my code to
[Marmalade](http://marmalade-repo.org/) after taking credit for all
the work and slapping the GPL on it (my version is public domain). So,
due to that and because the name `httpd.el` is already overloaded as
it is, I renamed it to `simple-httpd`. That's the name of the package
in MELPA.

I did more than rename the package; it got an overhaul. I rewrote a
few functions, tossed a whole bunch of functions, created
[a test suite](/blog/2012/08/15/), and **finally added directory
listings** — a feature that had long been on the TODO list. To keep
with the name "simple", I ripped out the
[clunky servlet system](/blog/2009/11/03/) (sorry Chunye). This new
version was leaner, cleaner, and more useful.

I've definitely improved my software development skill over the last
three years since I originally wrote it. In my refactor I made it
buffer oriented. When a request comes in, the server fills a buffer
with the response and sends it back. This means I could send a
`Content-Length` header and use keep-alive to serve multiple requests
over one connection. It also suggested a new servlet paradigm — the
servlet prepares a buffer and the server sends it to the client.

### Servlets

So I ended up adding servlet support again, from scratch. This time
it's really easy to use. Here's a "Hello, World" servlet,

~~~cl
(defservlet hello-world text/plain ()
  (insert "Hello, World"))
~~~

The "function name" part is the path to the servlet. This one would be
found at `/hello-world`. The second is the MIME type as a
symbol. We're just sending plain text in this example. The third is
the argument list. A servlet takes up to three arguments: the path,
the query alist, and the full request object (which includes the first
two). Unless a more specific servlet is defined, this servlet handles
everything under its root. In this case `/hello-world`, including
`/hello-world/foo` and `/hello-world/foo/bar.txt`. This is why the
path argument is relevant.

This servlet uses the path to get a name,

~~~cl
(defservlet hello text/plain (path)
  (insert "hello, " (file-name-nondirectory path)))
~~~

If you visit `/hello/Chris` it will send you "Hello, Chris". Servlets
are trivial to write!

This one serves the contents of the `*scratch*` buffer,

~~~cl
(defservlet scratch text/plain ()
  (insert-buffer-substring (get-buffer "*scratch*")))
~~~

In the background I continue to use Chunye's symbol dispatch
technique, so all servlets are actually functions that begin with
`httpd/` (`http/hello-world` and `httpd/hello`). For a more advanced
servlet, the function can be written directly. There's another macro,
`with-httpd-buffer` to help keep this simple. The server will always
pass four arguments (the three servlet arguments plus one more), so
when creating the function directly it needs to accept at least four
arguments.

~~~cl
(defun httpd/hello (proc path &rest args)
  (with-httpd-buffer proc "text/plain"
    (insert "hello, " (file-name-nondirectory path))))
~~~

The `proc` object here is the network connection process, providing
more exclusive access to the client. This allows the servlet to do
more interesting things like respond in the future (long polls). The
`with-httpd-buffer` macro creates a temporary buffer and, when the
body completes, sends an HTTP header and the buffer as the content,
similar to `defservlet`.

With access to the process, the servlet can do more specialized things
like send custom headers with `httpd-send-header`, send files with
`httpd-send-file`, send an error page with `httpd-error`, or do
redirects with `httpd-redirect`. The file server part of the server is
actually just another a servlet as well: `httpd/`. This could be
redefined to redirect the browser to our example servlet (HTTP 301).

~~~cl
(defun httpd/ (proc &rest args)
  (httpd-redirect proc "/hello-world"))
~~~

### impatient-mode

I showed this to [Brian](http://50ply.com), like I do everything, and
he found my servlet concept to be compelling, especially the
buffer-serving servlet. I believe his exact words were, "That's so
simple." He found it interesting enough that
[he wrote a mode based on it called `impatient-mode`](http://www.50ply.com/blog/2012/08/13/introducing-impatient-mode/)!

It serves a buffer's content live to the web browser, including syntax
highlighting (via htmlize). Updates to the buffer are communicated by
a long-poll. The browser initiates a request in the background for an
update. Emacs adds the request to a list. A hook in
`after-change-functions` updates all the browsers waiting for an
update.

Enabling `impatient-mode`, a minor mode, publishes the buffer. If the
server's running, the list of published buffers can be found under
`/imp` —
i.e. [http://localhost:8080/imp](http://localhost:8080/imp). The
buffer can be accessed directly at `/imp/live/<buffer-name>`, which is
where `/imp` will link.

Perhaps the coolest thing is serving an HTML buffer *without*
htmlize. That is, send the raw buffer as `text/html`. Brian has a demo
of this in the linked post. You can tweak CSS and HTML and watch it
update live in the browser as you edit. It's a really neat way to edit
CSS, since it's often unintuitive (at least for me).

`impatient-mode` can also be installed through MELPA.
