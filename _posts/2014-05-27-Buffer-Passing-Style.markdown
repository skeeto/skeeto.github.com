---
title: Emacs Lisp Buffer Passing Style
layout: post
date: 2014-05-27T01:58:09Z
tags: [elisp]
uuid: f61fa819-f174-3147-1bfe-3493c1c18bc6
---

Emacs Lisp strings are mutable, fixed-length character (multibyte) or
byte (unibyte) arrays. Any operation that would change its length
requires allocating a new string object. This is common in many
programming languages' strings. Python, Java, and JavaScript go even
further, with strings being completely immutable.

In these languages, performing many string operations at a time,
especially with the `+=` operator, allocates many temporary strings.
It's also awkward. For these situations, Java provides a class,
[StringBuilder][sb], so that these operations can be done with a
temporary, efficient, mutable data structure that will emit the final
string when complete.

~~~java
java.util.Collection<T> collection;

public String toString() {
    StringBuilder sb = new StringBuilder();
    for (T element : collection) {
        sb.append(element);
    }
    return sb.toString();
}
~~~

In JavaScript a popular string building idiom is to use an array. Push
the components onto an array and join() the result.

~~~javascript
function toString(object) {
    var output = [];
    for (var k in object) {
        output.push(k);
        output.push(' -> ');
        output.push(object[k]);
        output.push('\n');
    }
    return output.join('');
}

toString({a: 1, b: 2});
// => "a -> 1\nb -> 2\n"
~~~

### Emacs Lisp

What character sequence data structure already exists in Elisp that's
efficient at insert, update, and delete? Buffers, of course! I know
it's easy to forget, but editing sequences of characters is *the*
primary purpose of Emacs, after all. To make use of a buffer as a
string builder, use one of my favorite macros: `with-temp-buffer`. I
like to combine this with setting `standard-output` so that all of the
printing functions go there.

~~~cl
(defun to-string (alist)
  (with-temp-buffer
    (let ((standard-output (current-buffer)))
      (dolist (pair alist)
        (princ (cl-first pair))
        (princ " -> ")
        (princ (cl-second pair))
        (princ "\n")))
    (buffer-string)))
~~~

Update: Jon O. pointed out that Emacs has a `with-output-to-string`
macro available to do this more concisely.

Internally Elisp buffers are [gap buffers][gap], a rather simple data
structure where the data is split into two sequences with a "gap" in
between. Insertion and deletion occurs at the gap, which is slid up
and down the overall sequence. This makes gap buffers efficient for
making lots of edits localized in a single area, just as a human would
do while editing text.

Each character in a buffer is a full Unicode code point and can have
an arbitrary set of properties associated with it (font-lock-face,
read-only, nonstickiness, etc.). Along with inline image objects, this
makes buffers rich enough to display rendered HTML (to a limited
extent).

### The Catch

There's an important caveat to using buffers as mutable strings:
they're [not managed by the garbage collector][finalize]. Each buffer
goes into the global buffer list, implemented internally as an
intrusive linked list. If a buffer is not on this list, it's a dead
buffer.

Ultimately this makes buffer objects poor return values. It's an
impedance mismatch. The caller has to be careful to free ("kill") the
buffer. It's easy to miss if an error is signaled. For example,
`url-retrieve` and `url-retrieve-synchronously` return a buffer with
the response from a web server. It's not uncommon for Elisp programs
to leak these buffers during normal operation.

~~~cl
(with-current-buffer (url-retrieve-synchronously some-url)
  (setf (point) url-http-end-of-headers)
  (prog1 (json-read)
    (kill-buffer)))
~~~

If `json-read` fails, the buffer is leaked.

As a side note: alternatively you could use [my finalize
package][elisp-finalize] to associate the buffer with an object that
is subject to garbage collection. The buffer will be killed
immediately when the object is garbage collected.

#### Buffer Passing Style

To deal with this, my preferred idiom is what I call *buffer-passing
style*. Rather than have the callee instantiate the buffer, the caller
instantiates the buffer and "passes" it implicitly as the *current
buffer*. The callee fills it with something. The caller should use
something like `with-temp-buffer` so that the buffer has a clean
life-cycle, fully managed by the caller.

Imagine instead of returning a buffer, `url-retrieve-synchronously`
puts the result in the current buffer instead of returning a buffer.
If anything goes wrong, the buffer will be automatically killed by
`with-temp-buffer`.

~~~cl
(with-temp-buffer
  (url-retrieve-synchronously some-url)
  (setf (point) url-http-end-of-headers)
  (json-read))
~~~

Buffer-passing style is what I settled on for [simple-httpd][httpd].
Servlets are called with the output buffer as the current buffer and
with `standard-output` set to this buffer. The servlet is only
responsible for filling this buffer with content. Thanks to
`process-send-region`, the content is never actually copied into a
string.

~~~cl
(defservlet* search :application/json (q)
  (princ (json-encode (search-results q))))
~~~

I didn't recognize buffer-passing style until much later. As a result,
far too much of simple-httpd is still string oriented when it
shouldn't be.


[gap]: http://en.wikipedia.org/wiki/Gap_buffer
[finalize]: /blog/2014/01/27/
[sb]: http://docs.oracle.com/javase/7/docs/api/java/lang/StringBuffer.html
[elisp-finalize]: https://github.com/skeeto/elisp-finalize
[httpd]: https://github.com/skeeto/emacs-web-server
