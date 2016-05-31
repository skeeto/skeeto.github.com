---
title: Elisp Weak References
layout: post
tags: [elisp]
uuid: 45be87f8-bd8f-3012-09fb-336698c25046
---

Today I added a `skewer-eval-print-last-expression` function to
[Skewer](/blog/2012/10/31/), functionality I've been sorely missing
for awhile. To properly support it I needed a hash table with
automatically expiring entries. Specifically, I needed to keep track
of state in Emacs that I couldn't trust the (untrusted) browser to
track for me. The alternative would be to send an encrypted blob to
the browser along with code to evaluate, which would send back with
the result. Instead of getting into questionable, hand-rolled
encryption I wrote an [expiring hash table][cache-table]
implementation.

This had me take a careful look over
[Elisp's hash table documentation][hash-table], which reminded me of a
cool feature they have: key/value weakness. The hash table can be
configured such that it doesn't prevent its keys and values from being
garbage collected. Elisp's hash tables are *really* flexible in this
regard; any combination of key and value weakness is supported. This
is more flexible than Java's [WeakHashMap][WeakHashMap], which only
supports weak keys. For example, to make a hash table that weakly
holds its values,

~~~cl
(make-hash-table :weakness 'value)
~~~

Oddly, Elisp lacks functionality to use weak references more
generally. Fortunately [this can be fixed][weak-ref]!

~~~cl
(defun weak-ref (thing)
  (let ((ref (make-hash-table :size 1 :weakness t :test 'eq)))
    (prog1 ref
      (puthash t thing ref))))

(defun deref (ref)
  (gethash t ref))
~~~

`weak-ref` wraps an object in a weak hash table of size 1 under the
key `t`. The second function, `deref`, fetches the object from the
hash table if it's still there. Otherwise it returns `nil`. Here it is
in action,

~~~cl
(setq ref (weak-ref (list 1 2 3)))

;; It's still there.
(deref ref)  ; => (1 2 3)

;; Now run garbage collection.
(garbage-collect)

;; The list has been garbage collected.
(deref ref)  ; => nil
~~~

I had to use `setq` here instead of `defvar` because garbage
collection seems to always get triggered after `defvar`.

I don't have a use-case for this at the moment. Weak references are
mostly useful in hash tables (caches), and these functions would be
entirely redundant in that case. I originally implemented these as
macros, but I feel it made them too inflexible — they couldn't be
passed as a function.


[weak-ref]: https://github.com/skeeto/elisp-weak-ref
[cache-table]: https://github.com/skeeto/skewer-mode/blob/master/cache-table.el
[hash-table]: http://www.gnu.org/software/emacs/manual/html_node/elisp/Hash-Tables.html
[WeakHashMap]: http://docs.oracle.com/javase/7/docs/api/java/util/WeakHashMap.html
