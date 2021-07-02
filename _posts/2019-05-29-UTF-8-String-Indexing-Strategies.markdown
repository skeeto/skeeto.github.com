---
title: UTF-8 String Indexing Strategies
layout: post
date: 2019-05-29T21:52:06Z
tags: [elisp, emacs, go, lang]
uuid: 12e9ed44-b5c1-495f-8750-dfaf1ab008e2
excerpt_separator: <!--more-->
---

*This article was discussed [on Hacker News][hn].*

When designing or, in some cases, implementing a programming language
with built-in support for Unicode strings, an important decision must be
made about how to represent or encode those strings in memory. Not all
representations are equal, and there are trade-offs between different
choices.

<!--more-->

One issue to consider is that strings typically feature random access
indexing of code points with a time complexity resembling constant
time (`O(1)`). However, not all string representations actually
support this well. Strings using variable length encoding, such as
UTF-8 or UTF-16, have `O(n)` time complexity indexing, ignoring
special cases (discussed below). The most obvious choice to achieve
`O(1)` time complexity — an array of 32-bit values, as in UCS-4 —
makes very inefficient use of memory, especially with typical strings.

Despite this, UTF-8 is still chosen in a number of programming
languages, or at least in their implementations. In this article I'll
discuss three examples — Emacs Lisp, Julia, and Go — and how each takes a
slightly different approach.

### Emacs Lisp

Emacs Lisp has two different types of strings that generally can be used
interchangeably: *unibyte* and *multibyte*. In fact, the difference
between them is so subtle that I bet that most people writing Emacs Lisp
don't even realize there are two kinds of strings.

Emacs Lisp uses UTF-8 internally to encode all "multibyte" strings and
buffers. To fully support arbitrary sequences of bytes in the files
being edited, Emacs uses [its own extension of Unicode][ext] to
precisely and unambiguously represent raw bytes intermixed with text.
Any arbitrary sequence of bytes can be decoded into Emacs' internal
representation, then losslessly re-encoded back into the exact same
sequence of bytes.

Unibyte strings and buffers are really just byte-strings. In practice,
they're essentially ISO/IEC 8859-1, a.k.a. *Latin-1*. It's a Unicode
string where all code points are below 256. Emacs prefers the smallest
and simplest string representation when possible, [similar to CPython
3.3+][pep].

```cl
(multibyte-string-p "hello")
;; => nil

(multibyte-string-p "π ≈ 3.14")
;; => t
```

Emacs Lisp strings are mutable, and therein lies the kicker: As soon as
you insert a code point above 255, Emacs quietly converts the string to
multibyte.

```cl
(defvar fish "fish")

(multibyte-string-p fish)
;; => nil

(setf (aref fish 2) ?ŝ
      (aref fish 3) ?o)

fish
;; => "fiŝo"

(multibyte-string-p fish)
;; => t
```

Constant time indexing into unibyte strings is straightforward, and
Emacs does the obvious thing when indexing into unibyte strings. It
helps that most strings in Emacs are probably unibyte, even when the
user isn't working in English.

Most buffers are multibyte, even if those buffers are generally just
ASCII. Since [Emacs uses gap buffers][gap] it generally doesn't matter:
Nearly all accesses are tightly clustered around the point, so O(n)
indexing doesn't often matter.

That leaves multibyte strings. Consider these idioms for iterating
across a string in Emacs Lisp:

```cl
(dotimes (i (length string))
  (let ((c (aref string i)))
    ...))

(cl-loop for c being the elements of string
         ...)
```

The latter expands into essentially the same as the former: An
incrementing index that uses `aref` to index to that code point. So is
iterating over a multibyte string — a common operation — an O(n^2)
operation?

The good news is that, at least in this case, no! It's essentially just
as efficient as iterating over a unibyte string. Before going over why,
consider this little puzzle. Here's a little string comparison function
that compares two strings a code point at a time, returning their first
difference:

```cl
(defun compare (string-a string-b)
  (cl-loop for a being the elements of string-a
           for b being the elements of string-b
           unless (eql a b)
           return (cons a b)))
```

Let's examine benchmarks with some long strings (100,000 code points):

```cl
(benchmark-run
    (let ((a (make-string 100000 0))
          (b (make-string 100000 0)))
      (compare a b)))
;; => (0.012568031 0 0.0)
```

With using two, zeroed unibyte strings it takes 13ms. How about changing
the last code point in one of them to 256, converting it to a multibyte
string:

```cl
(benchmark-run
    (let ((a (make-string 100000 0))
          (b (make-string 100000 0)))
      (setf (aref a (1- (length a))) 256)
      (compare a b)))
;; => (0.012680513 0 0.0)
```

Same running time, so that multibyte string cost nothing more to iterate
across. Let's try making them both multibyte:

```cl
(benchmark-run
    (let ((a (make-string 100000 0))
          (b (make-string 100000 0)))
      (setf (aref a (1- (length a))) 256
            (aref b (1- (length b))) 256)
      (compare a b)))
;; => (2.327959762 0 0.0)
```

That took 2.3 seconds: about 2000x longer to run! Iterating over two
multibyte strings concurrently seems to have broken an optimization.
Can you reason about what's happened?

To avoid the O(n) cost on this common indexing operating, Emacs keeps
a "bookmark" for the last indexing location into a multibyte string.
If the next access is nearby, it can starting looking from this
bookmark, forwards or backwards. Like a gap buffer, this gives a big
advantage to clustered accesses, including iteration.

However, this string bookmark is *global*, one per Emacs instance, not
once per string. In the last benchmark, the two multibyte strings are
constantly fighting over a single string bookmark, and indexing in
comparison function is reduced to O(n^2) time complexity.

So, Emacs *pretends* it has constant time access into its UTF-8 text
data, but it's only faking it with some simple optimizations. This
usually works out just fine.

### Julia

Another approach is to not pretend at all, and to make this limitation
of UTF-8 explicit in the interface. Julia took this approach, and it
[was one of my complaints about the language][julia]. I don't think
this is necessarily a bad choice, but I do still think it's
inappropriate considering Julia's target audience (i.e. Matlab users).

Julia strings are explicitly byte strings containing valid UTF-8 data.
All indexing occurs on bytes, which is trivially constant time, and
always decodes the multibyte code point starting at that byte. *But*
it is an error to index to a byte that doesn't begin a code point.
That error is also trivially checked in constant time.

```julia
s = "π"

s[1]
# => 'π'

s[2]
# ERROR: UnicodeError: invalid character index
#  in getindex at ./strings/basic.jl:37
```

Slices are still over bytes, but they "round up" to the end of the
current code point:

```julia
s[1:1]
# => "π"
```

Iterating over a string requires helper functions which keep an internal
"bookmark" so that each access is constant time:

```julia
for i in eachindex(string)
    c = string[i]
    # ...
end
```

So Julia doesn't pretend, it makes the problem explicit.

### Go

Go is very similar to Julia, but takes an even more explicit view of
strings. All strings are byte strings and there are no restrictions on
their contents. Conventionally strings contain UTF-8 encoded text, but
this is not strictly required. There's a `unicode/utf8` package for
working with strings containing UTF-8 data.

Beyond convention, the `range` clause also assumes the string contains
UTF-8 data, and it's not an error if it does not. Bytes not containing
valid UTF-8 data appear as a `REPLACEMENT CHARACTER` (U+FFFD).

```go
func main() {
    s := "π\xff"
    for _, r := range s {
        fmt.Printf("U+%04x\n", r)
    }
}

// U+03c0
// U+fffd
```

A further case of the language favoring UTF-8 is that casting a string
to `[]rune` decodes strings into code points, like UCS-4, again using
`REPLACEMENT CHARACTER`:

```go
func main() {
    s := "π\xff"
    r := []rune(s)
    fmt.Printf("U+%04x\n", r[0])
    fmt.Printf("U+%04x\n", r[1])
}

// U+03c0
// U+fffd
```

So, like Julia, there's no pretending, and the programmer explicitly
must consider the problem.

### Preferences

All-in-all I probably prefer how Julia and Go are explicit with
UTF-8's limitations, rather than Emacs Lisp's attempt to cover it up
with an internal optimization. Since the abstraction is leaky, it may
as well be made explicit.


[ext]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Text-Representations.html
[gap]: /blog/2017/09/07/
[hn]: https://news.ycombinator.com/item?id=20049491
[julia]: /blog/2014/03/06/
[pep]: https://www.python.org/dev/peps/pep-0393/
