---
title: Faster Elfeed Search Through JIT Byte-code Compilation
layout: post
date: 2016-12-11T23:16:42Z
tags: [emacs, elfeed, optimization, elisp]
uuid: 47002cc3-816a-3cb8-b462-327364e3f943
---

Today I pushed an update for [Elfeed][elfeed] that doubles the speed
of the search filter in the worse case. This is the user-entered
expression that dynamically narrows the entry listing to a subset that
meets certain criteria: published after a particular date,
with/without particular tags, and matching/non-matching zero or more
regular expressions. The filter is live, applied to the database as
the expression is edited, so it's important for usability that this
search completes under a threshold that the user might notice.

![](/img/elfeed/filter.gif)

The typical workaround for these kinds of interfaces is to make
filtering/searching asynchronous. It's possible to do this well, but
it's usually a terrible, broken design. If the user acts upon the
asynchronous results — say, by typing the query and hitting enter to
choose the current or expected top result — then the final behavior is
non-deterministic, a race between the user's typing speed and the
asynchronous search. Elfeed will keep its synchronous live search.

For anyone not familiar with Elfeed, here's a filter that finds all
entries from within the past year tagged "youtube" (`+youtube`) that
mention Linux or Linus (`linu[sx]`), but aren't tagged "bsd" (`-bsd`),
limited to the most recent 15 entries (`#15`):

    @1-year-old +youtube linu[xs] -bsd #15

The database is primarily indexed over publication date, so filters on
publication dates are the most efficient filters. Entries are visited
in order starting with the most recently published, and the search can
bail out early once it crosses the filter threshold. Time-oriented
filters have been encouraged as the solution to keep the live search
feeling lively.

### Filtering Overview

The first step in filtering is parsing the filter text entered by the
user. This string is broken into its components using the
`elfeed-search-parse-filter` function. Date filter components are
converted into a unix epoch interval, tags are interned into symbols,
regular expressions are gathered up as strings, and the entry limit is
parsed into a plain integer. Absence of a filter component is
indicated by nil.

~~~cl
(elfeed-search-parse-filter "@1-year-old +youtube linu[xs] -bsd #15")
;; => (31557600.0 (youtube) (bsd) ("linu[xs]") nil 15)
~~~

Previously, the next step was to apply the `elfeed-search-filter`
function with this structured filter representation to the database.
Except for special early-bailout situations, it works left-to-right
across the filter, checking each condition against each entry. This is
analogous to an interpreter, with the filter being a program.

Thinking about it that way, what if the filter was instead compiled
into an Emacs byte-code function and executed directly by the Emacs
virtual machine? That's what this latest update does.

### Benchmarks

With six different filter components, the actual filtering routine is
a bit too complicated for an article, so I'll set up a simpler, but
roughly equivalent, scenario. With a reasonable cut-off date, the
filter was already sufficiently fast, so for benchmarking I'll focus
on the worst case: no early bailout opportunities. An entry will be
just a list of tags (symbols), and the filter will have to test every
entry.

My [real-world Elfeed database][an] currently has 46,772 entries with
36 distinct tags. For my benchmark I'll round this up to a nice
100,000 entries, and use 26 distinct tags (A–Z), which has the nice
alphabet property and more closely reflects the number of tags I still
care about.

First, here's `make-random-entry` to generate a random list of 1–5
tags (i.e. an entry). The `state` parameter is the random state,
allowing for deterministic benchmarks on a randomly-generated
database.

~~~cl
(cl-defun make-random-entry (&key state (min 1) (max 5))
  (cl-loop repeat (+ min (cl-random (1+ (- max min)) state))
           for letter = (+ ?A (cl-random 26 state))
           collect (intern (format "%c" letter))))
~~~

The database is just a big list of entries. In Elfeed this is actually
an AVL tree. Without dates, the order doesn't matter.

~~~cl
(cl-defun make-random-database (&key state (count 100000))
  (cl-loop repeat count collect (make-random-entry :state state)))
~~~

Here's [my old time macro][time]. An important change I've made since
years ago is to call `garbage-collect` before starting the clock,
eliminating bad samples from unlucky garbage collection events.
Depending on what you want to measure, it may even be worth disabling
garbage collection during the measurement by setting
`gc-cons-threshold` to a high value.

~~~cl
(defmacro measure-time (&rest body)
  (declare (indent defun))
  (garbage-collect)
  (let ((start (make-symbol "start")))
    `(let ((,start (float-time)))
       ,@body
       (- (float-time) ,start))))
~~~

Finally, the benchmark harness. It uses a hard-coded seed to generate
the same pseudo-random database. The test is run against the a filter
function, `f`, 100 times in search for the same 6 tags, and the timing
results are averaged.

~~~cl
(cl-defun benchmark (f &optional (n 100) (tags '(A B C D E F)))
  (let* ((state (copy-sequence [cl-random-state-tag -1 30 267466518]))
         (db (make-random-database :state state)))
    (cl-loop repeat n
             sum (measure-time
                   (funcall f db tags))
             into total
             finally return (/ total (float n)))))
~~~

The baseline will be `memq` (test for membership using identity,
`eq`). There are two lists of tags to compare: the list that is the
entry, and the list from the filter. This requires a nested loop for
each entry, one explicit (`cl-loop`) and one implicit (`memq`), both
with early bailout.

~~~cl
(defun memq-count (db tags)
  (cl-loop for entry in db count
           (cl-loop for tag in tags
                    when (memq tag entry)
                    return t)))
~~~

Byte-code compiling everything and running the benchmark on my laptop
I get:

~~~cl
(benchmark #'memq-count)
;; => 0.041 seconds
~~~

That's actually not too bad. One of the advantages of this definition
is that there are no function calls. The `memq` built-in function has
its own opcode (62), and the rest of the definition is special forms
and macros expanding to special forms (`cl-loop`). It's exactly the
thing I need to exploit to make filters faster.

As a sanity check, what would happen if I used `member` instead of
`memq`? In theory it should be slower because it uses `equal` for
tests instead of `eq`.

~~~cl
(defun member-count (db tags)
  (cl-loop for entry in db count
           (cl-loop for tag in tags
                    when (member tag entry)
                    return t)))
~~~

It's only slightly slower because `member`, [like many other
built-ins][advice], also has an opcode (157). It's just a tiny bit
more overhead.

~~~cl
(benchmark #'member-count)
;; => 0.047 seconds
~~~

To test function call overhead while still using the built-in (e.g.
written in C) `memq`, I'll alias it so that the byte-code compiler is
forced to emit a function call.

~~~cl
(defalias 'memq-alias 'memq)

(defun memq-alias-count (db tags)
  (cl-loop for entry in db count
           (cl-loop for tag in tags
                    when (memq-alias tag entry)
                    return t)))
~~~

To verify that this is doing what I expect, I `M-x disassemble` the
function and inspect the byte-code disassembly. Here's a simple
example.

~~~cl
(disassemble
 (byte-compile (lambda (list) (memq :foo list))))
~~~

When compiled under lexical scope (`lexical-binding` is true), here's
the disassembly. To understand what this means, see [*Emacs Byte-code
Internals*][internals].

    0       constant  :foo
    1       stack-ref 1
    2       memq
    3       return

Notice the `memq` instruction. Try using `memq-alias` instead:

~~~cl
(disassemble
 (byte-compile (lambda (list) (memq-alias :foo list))))
~~~

Resulting in a function call:

    0       constant  memq-alias
    1       constant  :foo
    2       stack-ref 2
    3       call      2
    4       return

And the benchmark:

~~~cl
(benchmark #'memq-alias-count)
;; => 0.052 seconds
~~~

So the function call adds about 27% overhead. This means it would be a
good idea to **avoid calling functions in the filter** if I can help
it. I should rely on these special opcodes.

Suppose `memq` was written in Emacs Lisp rather than C. How much would
that hurt performance? My version of `my-memq` below isn't quite the
same since it returns t rather than the sublist, but it's good enough
for this purpose. (I'm using `cl-loop` because writing early bailout
in plain Elisp without recursion is, in my opinion, ugly.)

~~~cl
(defun my-memq (needle haystack)
  (cl-loop for element in haystack
           when (eq needle element)
           return t))

(defun my-memq-count (db tags)
  (cl-loop for entry in db count
           (cl-loop for tag in tags
                    when (my-memq tag entry)
                    return t)))
~~~

And the benchmark:

~~~cl
(benchmark #'my-memq-count)
;; => 0.137 seconds
~~~

Oof! It's more than 3 times slower than the opcode. This means **I
should use built-ins as much as possible** in the filter.

### Dynamic vs. lexical scope

There's one last thing to watch out for. Everything so far has been
compiled with lexical scope. You should really turn this on by default
for all new code that you write. It has three important advantages:

1. It allows the compiler to catch more mistakes.
2. It eliminates a class of bugs related to dynamic scope: Local
   variables are exposed to manipulation by callees.
3. [Lexical scope has better performance][lex].

Here are all the benchmarks with the default dynamic scope:

~~~cl
(benchmark #'memq-count)
;; => 0.065 seconds

(benchmark #'member-count)
;; => 0.070 seconds

(benchmark #'memq-alias-count)
;; => 0.074 seconds

(benchmark #'my-memq-count)
;; => 0.256 seconds
~~~

It halves the performance in this benchmark, and for no benefit. Under
dynamic scope, local variables use the `varref` opcode — a global
variable lookup — instead of the `stack-ref` opcode — a simple array
index.

~~~cl
(defun norm (a b)
  (* (- a b) (- a b)))
~~~

Under dynamic scope, this compiles to:

    0       varref    a
    1       varref    b
    2       diff
    3       varref    a
    4       varref    b
    5       diff
    6       mult
    7       return

And under lexical scope (notice the variable names disappear):

    0       stack-ref 1
    1       stack-ref 1
    2       diff
    3       stack-ref 2
    4       stack-ref 2
    5       diff
    6       mult
    7       return

### JIT-compiled filters

So far I've been moving in the wrong direction, making things slower
rather than faster. How can I make it faster than the straight `memq`
version? By compiling the filter into byte-code.

I won't write the byte-code directly, but instead generate Elisp code
and use the byte-code compiler on it. This is safer, will work
correctly in future versions of Emacs, and leverages the optimizations
performed by the byte-compiler. This sort of thing recently [got a bad
rap on Emacs Horrors][horrors], but I was happy to see that this
technique is already established.

~~~cl
(defun jit-count (db tags)
  (let* ((memq-list (cl-loop for tag in tags
                             collect `(memq ',tag entry)))
         (function `(lambda (db)
                      (cl-loop for entry in db
                               count (or ,@memq-list))))
         (compiled (byte-compile function)))
    (funcall compiled db)))
~~~

It dynamically builds the code as an s-expression, runs that through
the byte-code compiler, executes it, and throws it away. It's
"just-in-time," though compiling to byte-code and not [native
code][native]. For the benchmark tags of `(A B C D E F)`, this builds
the following:

~~~cl
(lambda (db)
  (cl-loop for entry in db
           count (or (memq 'A entry)
                     (memq 'B entry)
                     (memq 'C entry)
                     (memq 'D entry)
                     (memq 'E entry)
                     (memq 'F entry))))
~~~

Due to its short-circuiting behavior, `or` is a special form, so this
function is just special forms and `memq` in its opcode form. It's as
fast as Elisp can get.

Having s-expressions is a real strength for lisp, since the
alternative (in, say, JavaScript) would be to assemble the function by
concatenating code strings. By contrast, this looks a lot like a
regular lisp macro. Invoking the byte-code compiler does add some
overhead compared to the interpreted filter, but it's insignificant.

How much faster is this?

~~~cl
(benchmark #'jit-count)
;; => 0.017s
~~~

**It's more than twice as fast!** The big gain here is through *loop
unrolling*. The outer loop has been unrolled into the `or` expression.
That section of byte-code looks like this:

    0       constant  A
    1       stack-ref 1
    2       memq
    3       goto-if-not-nil-else-pop 1
    6       constant  B
    7       stack-ref 1
    8       memq
    9       goto-if-not-nil-else-pop 1
    12      constant  C
    13      stack-ref 1
    14      memq
    15      goto-if-not-nil-else-pop 1
    18      constant  D
    19      stack-ref 1
    20      memq
    21      goto-if-not-nil-else-pop 1
    24      constant  E
    25      stack-ref 1
    26      memq
    27      goto-if-not-nil-else-pop 1
    30      constant  F
    31      stack-ref 1
    32      memq
    33:1    return

In Elfeed, not only does it unroll these loops, it completely
eliminates the overhead for unused filter components. Comparing to
this benchmark, I'm seeing roughly matching gains in Elfeed's worst
case. In Elfeed, I also bind `lexical-binding` around the
`byte-compile` call to force lexical scope, since otherwise it just
uses the buffer-local value (usually nil).

Filter compilation can be toggled on and off by setting
`elfeed-search-compile-filter`. If you're up to date, try out live
filters with it both enabled and disabled. See if you can notice the
difference.

### Result summary

Here are the results in a table, all run with Emacs 24.4 on x86-64.

    (ms)      memq      member    memq-alias my-memq   jit
    lexical   41        47        52         137       17
    dynamic   65        70        74         256       21

And the same benchmarks on Aarch64 (Emacs 24.5, ARM Cortex-A53), where
I also occasionally use Elfeed, and where I have been very interested
in improving performance.

    (ms)      memq      member    memq-alias my-memq   jit
    lexical   170       235       242        614       79
    dynamic   274       340       345        1130      92

And here's how you can run the benchmarks for yourself, perhaps with
different parameters:

* [jit-bench.el](/download/jit-bench.el)

The header explains how to run the benchmark in batch mode:

    $ emacs -Q -batch -f batch-byte-compile jit-bench.el
    $ emacs -Q -batch -l jit-bench.elc -f benchmark-batch


[time]: /blog/2009/05/28/
[internals]: /blog/2014/01/04/
[an]: /blog/2016/08/12/
[advice]: /blog/2013/01/22/
[horrors]: http://emacshorrors.com/posts/when-data-becomes-code.html
[native]: /blog/2015/03/19/
[elfeed]: https://github.com/skeeto/elfeed
[lex]: /blog/2016/12/22/
