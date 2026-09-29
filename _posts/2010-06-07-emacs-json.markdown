---
title: Elisp Printed Hash Tables
layout: post
tags: [emacs, javascript]
uuid: 3a623665-57e7-3a74-cb71-fef1573e0e09
---

<!-- 7 June 2010 -->

A printed hash table representation is pretty new to Elisp, and a bit
late. As far as I know Elisp didn't come with a way to print, and read
back in, a hash table without rolling your own
(like [ Jared Dilettante was doing with a Data::Dumper style output](http://curiousprogrammer.wordpress.com/2010/06/07/data-dumper-in-emacs-lisp/)),
until 23.1 in July 2009. This is
when [ `json.el` was first included with Emacs](http://www.gnu.org/software/emacs/NEWS.23.1), for dumping to
and reading from [JSON](http://en.wikipedia.org/wiki/JSON).

```cl
(require 'json)

(setq hash (make-hash-table))
(puthash "key1" "data1" hash)
(puthash "key2" "data2" hash)

(insert "\n;; " (json-encode hash))
;; {"key2":"data2", "key1":"data1"}
```

Just a month ago Emacs 23.2 came out, very silently
including [a new
printed representation for hash tables](http://www.gnu.org/software/emacs/NEWS.23.2) with a `#s` hash
notation.

```cl
#s(hash-table data ("key1" "data1" "key2" "data2"))
```

With this hash tables can be printed and read as part of normal
s-expressions with the standard lisp reader and printer functions. It
seems heavy, having to write out "`hash-table`" in there,
but I think it's because the `#s` notation will be used to
create printed forms of other lisp objects that currently do not have
one.
