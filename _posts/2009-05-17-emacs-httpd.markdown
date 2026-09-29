---
title: Emacs Web Server
layout: post
tags: [emacs, elisp]
uuid: 1e0a3639-6df2-3c5b-3b16-bb29ac300602
---

<!-- 17 May 2009 -->

As part of my quest of developing solid knowledge of [GNU Emacs](http://www.gnu.org/software/emacs/) lisp, I have
implemented a pseudo-HTTP/1.0 web server within Emacs. Behold,

<pre>
git clone <a href="https://github.com/skeeto/emacs-http-server">git://github.com/skeeto/emacs-http-server.git</a>
</pre>

To all other non-emacsen text editors, can your text editor do that?!
Ha! Even though elisp is a slow, closure-less, dynamically scoped,
ugly cousin of more popular lisps, it's still a lot of fun to write.

To fire it up, load it into Emacs and run the extended command
(`M-x`) `httpd-start`. By default it will serve
files from "`~/public_html`". To change this, change the
variable `httpd-root` to the desired web root. You can stop
the server with `httpd-stop`.

It's about 200 lines of code and can serve static websites made of
small, static files. I say small files because it serves files from
buffers, meaning it has to read the entire file in first.

For a simple, text editor based server it can hold up to a pretty
decent load. At one point I hit it with 8 `wget` instances all making
rapid recursive downloads and my manual navigation wasn't slowed down
noticeably. Despite running in the slow elisp interpreter, I think it
can have much better performance by caching commonly served files in
buffers.

It *should* run, unmodified, anywhere a modern Emacs can run, so
I expect that it's already very portable. I can imagine it being
useful in a situation where someone needs to temporarily host some
files but there isn't a web server on the machine. Just grab this
script and throw it at Emacs.

Well, it only does IPv4 right now, though I expect IPv6 only requires
changing one number (namely, 4 to 6). I don't have any IPv6 systems to
test it on.

When writing it I also had security in mind so, as far as I know, it
should be safe to use. It cleans up the `GET` from the
client so that no files underneath the serving root can be accessed.

The server log is lisp itself. Here is an example log starting the
server, serving one request, and halting,

```
'(log
  (start "Wed May 13 23:33:34 2009")
  (connection
   (date "Wed May 13 23:36:25 2009")
   (address "192.168.0.3")
   (get "/0001.html")
   (req
    ("Referer" "http://192.168.0.2:8080/")
    ("Connection" "keep-alive")
    ("Keep-Alive" "300")
    ("Accept-Charset" "ISO-8859-1,utf-8;q=0.7,*;q=0.7")
    ("Accept-Encoding" "gzip,deflate")
    ("Accept-Language" "en-us,en;q=0.5")
    ("Accept" "image/png,image/*;q=0.8,*/*;q=0.5")
    ("User-Agent" "Mozilla/5.0 [...] Iceweasel/3.0.9 (Debian-3.0.9-1)")
    ("Host" "192.168.0.2:8080")
    ("GET" "/0001.html" "HTTP/1.1"))
   (path "~/public_html/0001.html")
   (status 200))
  (stop "Wed May 13 23:38:17 2009"))
```

The log is alists of alists, making a hierarchical tree structure that
can be explored with some simple lisp functions. Normally this sort of
thing is done with XML, but lisp already has its own structured
format: lists!

When `GET` is a directory, it looks for
"`index.html`" and serves that if it exists. More indexes
can be added to the variable `httpd-indexes`. This can
actually be done in a special "`.htaccess.el`" file.

If a "`.htaccess.el`" exists in the directory from which a
file is being served, Emacs will first load/execute it. You see, it's
just a lisp program. If you wanted to add a new index file name, the
hypertext access file could contain this,

```cl
(add-to-list 'httpd-indexes "0001.html")
```

It's a bit like a `.emacs` file.

But I think one of the coolest things about having a lisp-based server
is that the server can be modified in place without disrupting or
restarting it. In my Emacs web server, the only change that requires a
restart is changing the server port. In fact, I wrote most of it while
the server was running and tested my changes from a browser right as I
made them — all on the same instance of the server.

If you want to look into the AI side of this, the server could modify
its own code in response to its use.

I also had the idea of creating dynamic websites with elisp, in the
same way PHP or Perl does. If a `.el` file (or
`.elc`) is accessed, the server would pass the
`GET`/`POST` arguments as an alist to a function
in the elisp file. The server would also provide some nifty HTML
generation macros. A dynamic script might look like this,

```cl
(defun script (get)
  (html
   (head
    (title "My Script"))
   (body
    (h1 "Your Query")
    (p (concat "Your query was "
               (html-sanitize (cdr (assoc "q" get)) "."))))))
```

However, this is not (yet?) implemented. Just an idea.

I will continue to work on it, though I don't expect to add much more
to it. I will mostly improve the code and documentation.
