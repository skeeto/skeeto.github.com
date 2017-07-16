---
title: The Elfeed Database
layout: post
date: 2013-09-09T05:53:41Z
tags: [emacs, elisp, elfeed]
uuid: 8aba2e49-22a0-330b-e664-54fb50ecdd00
---

The design of [Elfeed's][elfeed] database took some experimentation
before any part of it was settled. A major design constraint was
Emacs' very limited file input/output. There's no random access and,
without the aid of an external program, files must always be read and
written wholesale. That's not database-friendly at all! In the end I
settled on a design that minimized the size of the frequently
rewritten parts, an index with two different data models, by storing
immutable data in a loose-file, content-addressable database.

At the moment there really aren't any pure-Elisp database solutions
for Emacs. This is almost certainly due to the aforementioned I/O
limitations. I ran into this same problem last year when I created
[an Emacs pastebin server][pastebin]. I attempted, and failed, to
interface with a SQLite database through it's command line program.
Nic Ferrier has published a [generic database interface][emacs-db],
but it lacks concrete implementations.

As a bit of good news, as far as I know Emacs *does* properly handle
atomic file updates across all platforms, so a pure-Elisp database
developer would never have to worry about only writing half the
database. It's always a safe operation. Worst case scenario you're
left with an old version of data rather than no data at all.

A real possibility for a database would be connecting to an
established database server via TCP with an Emacs network process. If
the server has a specified wire protocol Elisp could talk to it
efficiently. In fact, there's exists [pg.el][pg] that does *exactly*
this for PostgreSQL. Unfortunately I was not able to get this working
with my pastebin, nor is this solution appropriate for Elfeed. It
would be unreasonable to require users to first set up a PostgreSQL
server just to read web feeds!

Ultimately it would seem that any efficient Emacs database requires
the help of an external program. The [notmuch][notmuch] mail client,
which inspired Elfeed, does this. To access the notmuch database a
command line program is run once for each request. A query is passed
as a program argument and the output of the program is parsed into the
result.

### The Early Database

For the first few days of its existence Elfeed only had an in-memory
database. Closing Emacs would lose everything. For my personal usage
patterns, where I read, or at least address, all entries that arrive
— and especially because I use Elfeed on a couple of different
computers — I don't really *need* to track things long term. I could
easily mark everything after a certain date as read and forget about
them. However, it would be nice to have and, more importantly, many
people wouldn't use Elfeed without persistence between Emacs sessions.

So, for the first database I did what I always do: dumped the data
structure to a file using the printer and parsed it back in later
using the reader. This is dead simple in Lisp, it's very fast, and it
even works for circular data structures. It's something I missed so
much with the much-less-capable JSON format earlier this year that I
[wrote a JavaScript library to do it][resurrectjs].

~~~cl
(defun save-data (file data)
  (with-temp-file file
    (let ((standard-output (current-buffer))
          (print-circle t))  ; Allow circular data
      (prin1 data))))

(defun load-data (file)
  (with-temp-buffer
    (insert-file-contents file)
    (read (current-buffer))))

(save-data "demo.dat" '(a b c ["1" 2 3]))
(load-data "demo.dat")
;; => (a b c ["1" 2 3])
~~~

Anything with a printed representation can be serialized and stored
this way, including symbols, string, numbers, lists, vectors (structs,
objects), hash tables, and even compiled functions (.elc files).
Basically every Emacs library that stores data on disk uses this
technique.

Unfortunately, this is where I hit another serious database
constraint: [**`print-circle` is broken in Emacs 24.3**][bug15190],
the current stable release. This means Elfeed cannot take advantage of
this useful feature, at least not for a long time, as I had been
counting on. The final database is slightly slower and larger than
strictly required as a result.

### The Content Database

After breaking the circular references of the in-memory database I
finally had persistence for the first time. With the naive
printer/reader approach it was slow, almost 1 second to write just a
few thousand entries on my 6-year-old laptop (my minimum requirements
target machine). I wanted Elfeed to support hundreds of thousands of
entries, if not millions, so this was much too slow.

The big slowdown was writing out all the entry content each time the
database is saved. These large strings containing HTML that rarely
change. There's no reason to write these out every time, nor is there
a reason to even keep them in memory all the time, as it's rarely
accessed. The solution is a loose-file, content-addressable database,
very similar to an unpacked Git object database.

The content database stores immutable sequences of characters — not
just raw bytes, but rather multibyte strings — using an unspecified
coding system (right now it's UTF-8 for all platforms). The filename
for the content is the content hashed with SHA-1
("content-addressable"). To limit the number of files per directory,
these files are stored in subdirectories named by the first
hex-encoded byte of the hash (just like Git). A database of 4 items
might look like this:

    data/
       18/
          18ff6f11945b1e9f3e3c4cae8b5275d36b9944e1
          184c06a83f0bc73a8345c6d886f9043bcae095f8
       6b/
          6b59ae257f2bea24703d8adf5747049c138dfc82
       cc/
          cc47d53872ae2a9186151ef1a68392a94e1f091f

Something really neat about the content database is that it's
completely agnostic about Elfeed. If it weren't for Elfeed's garbage
collector, anyone could use it to store arbitrary content. The
function `elfeed-ref` accepts a string and returns a reference into
the database. Because of the hash, providing the same string in the
future will return the same reference without actually performing a
write. References are dereferenced with `elfeed-deref`.

~~~cl
(setf ref (elfeed-ref "Hello, world!"))
;; => [cl-struct-elfeed-ref "943a702d06f34599aee1f8da8ef9f7296031d699"]

(elfeed-deref ref)
;; => "Hello, world"
~~~

With content stored elsewhere, entries are a struct containing only
some small metadata: title, link, date, and a content database
reference. Writing out many of them at once is much, much faster.

I don't expect it happens often, but this also means content is
de-duplicated. If two entries happen to have the same content they'll
share content database storage. A small savings.

At this point it's really tempting to get fancier and really put this
content database to use. The core index itself could be stored as raw
content, and the root to accessing the database would be a single
SHA-1 hash referencing it — again, *very* similar to Git. If an index
stores a reference to the previously written index, then the the
Elfeed database would be an immutable structure tracking its entire
history. Such a change would cost virtually nothing in performance,
just disk space.

### Multiple Representations

With all the content out of the way, the database is now just a lean
index. At this point it's a hash table mapping feed IDs to feeds.
Feeds contain a list of its entries. To build the entry listing for
the elfeed-search buffer, Elfeed needs to visit each feed in the hash
table, gather its entries into one giant list, then finally sort that
list by date. At around O(n log n), that sort operation is a real
performance killer. Completely unacceptable. To fix this we need to
think about how the data is updated and used.

First, **entries are *always* viewed in date order**, no exceptions.
From my experience of using web feeds for the last six years I *never*
had a reason to list feed entries by any other order. The vast
majority of the time, newer entries are most relevant, and if I need
to look for something specific I can search for it.

We definitely want to store entries in date-order so we can create
entry listings without performing a sort: something around O(n) or so.
Inserting new entries into this structure should also be efficient.

Second, **entries are never *removed* from the database**. This isn't
e-mail. Even if a user doesn't want to see an entry again, we have to
keep track of it. Otherwise it will show up as new if it's discovered
in a feed again, which is likely. Things are added to the database and
never removed. In Elfeed, I use a `junk` tag to completely hide
entries I don't want to see, and I always have a `-junk` element in my
filter.

There's an important caveat to this one that I had missed until after
the public release: entry dates can change! When a previously
discovered entry is read from a feed, Elfeed updates (read: mutates)
the entry struct to reflect the new state. This includes the date.
It's very likely that a date-sorted representation won't tolerate date
changes underneath it since it's keying off of them. Either we refuse
to update the entry date, or we remove the entry, update the date, and
then re-insert it (how it currently works).

Third, **entries are generally added with a recent date**. After the
database is initially populated, it's only picking up new items. We
should prefer adding recently-dated entries be faster than adding
older entries. I didn't get a chance to take advantage of this, but
it's something to keep in mind.

Fourth, **entries need to be keyed by an ID string**. Each entry has a
unique, unchanging identifier string, either provided by the feed
itself (RSS's `guid` or Atom's `id`) or generated intelligently by
Elfeed. Especially because of the `print-circle` bug, we need to be
able to talk about feeds in terms of their ID — an indirect pointer.

(Actually, even when RSS `guid` tags are present, they're permalinks
by default. So, unfortunately, RSS IDs are not at all resistant to
collisions across feeds. To work around this, entry identifiers are a
*pair* of strings: feed ID and entry ID. Atom doesn't have this
problem, but we're stuck with the lowest common denominator.)

A date-oriented representation would be unable to efficiently look up
an entry by its ID, so it needs to be supplemented by an ID-oriented
representation. This means we need two representations in our
database: date-oriented and ID-oriented.

So what do we use? Well, for keeping entries sorted by date we want
some sort of balanced tree. A B-tree is probably a good choice. Rather
than write one I went with an AVL tree since Emacs comes with a
library for it (`avl-tree`). It's already debugged and optimized! The
bad news is that the internal structure is unspecified, so there are
no guarantees that it can be serialized. A future update to the
library may break the Elfeed database. I also had to hack into it to
work around a security issue. The comparison function is embedded in
the tree. After deserializing the database, Elfeed needs to ensure
that no one stuck a malicious function in there.

The choice for an ID database was super-easy: a hash table. Due to the
`print-circle` bug, this is actually the main representation. The AVL
tree only stores IDs and it has to reach into the hash table to do any
date comparisons. If `print-circle` was working I could store the same
exact entry objects in the AVL tree as the hash table, so mutating
them would update them in all representations. However, with
`print-circle` off, on deserialization these would become unique
objects and updates would break.

### The Future

That's where the database is today. I put in a few extra fields that
aren't actually used yet, so that there's room to make a few changes
without breaking the database. Perhaps someday I'll work out a whole
new database structure, or maybe a proper database library will come
into existence, and this post will simply document the old database.


[elfeed]: /blog/2013/09/04/
[emacs-db]: https://github.com/nicferrier/emacs-db
[pastebin]: /blog/2012/12/29/
[simple-httpd]: /blog/2012/08/20/
[pg]: http://www.online-marketwatch.com/pgel/pg.html
[notmuch]: http://notmuchmail.org/
[resurrectjs]: /blog/2013/03/28/
[bug15190]: http://lists.gnu.org/archive/html/bug-gnu-emacs/2013-08/msg00860.html
