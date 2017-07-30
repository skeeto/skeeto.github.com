---
title: Domain-Specific Language Compilation in Elfeed
layout: post
date: 2016-12-27T21:46:30Z
tags: [elfeed, emacs, elisp, optimization]
uuid: 6a6cd6a2-b44d-35b5-503c-c496d9094ac0
---

Last night I pushed another performance enhancement for Elfeed, this
time reducing the time spent parsing feeds. It's accomplished by
compiling, during macro expansion, a jQuery-like domain-specific
language within Elfeed.

### Heuristic parsing

Given the nature of the domain — [an under-specified standard][avr]
and a lack of robust adherence — feed parsing is much more heuristic
than strict. Sure, everyone's feed XML is strictly conforming since
virtually no feed reader tolerates invalid XML (thank you, XML
libraries), but, for the schema, the situation resembles the *de
facto* looseness of HTML. Sometimes important or required information
is missing, or is only available in [a different namespace][dc].
Sometimes, especially in the case of timestamps, it's in the wrong
format, or encoded incorrectly, or ambiguous. It's real world data.

To get a particular piece of information, Elfeed looks in a number of
different places within the feed, starting with the preferred source
and stopping when the information is found. For example, to find the
date of an Atom entry, Elfeed first searches for elements in this
order:

1. `<published>`
2. `<updated>`
3. `<date>`
4. `<modified>`
5. `<issued>`

Failing to find any of these elements, or if no parsable date is
found, it settles on the current time. Only the `updated` element is
required, but `published` usually has the desired information, so it
goes first. The last three are only valid for another namespace, but
are useful fallbacks.

Before Elfeed even starts this search, the XML text is parsed into an
s-expression using `xml-parse-region` — a pure Elisp XML parser
included in Emacs. The search is made over the resulting s-expression.

For example, here's a sample [from the Atom specification][atom].

~~~xml
<?xml version="1.0" encoding="utf-8"?>
<feed xmlns="http://www.w3.org/2005/Atom">

  <title>Example Feed</title>
  <link href="http://example.org/"/>
  <updated>2003-12-13T18:30:02Z</updated>
  <author>
    <name>John Doe</name>
  </author>
  <id>urn:uuid:60a76c80-d399-11d9-b93C-0003939e0af6</id>

  <entry>
    <title>Atom-Powered Robots Run Amok</title>
    <link rel="alternate" href="http://example.org/2003/12/13/atom03"/>
    <id>urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a</id>
    <updated>2003-12-13T18:30:02Z</updated>
    <summary>Some text.</summary>
  </entry>

</feed>
~~~

Which is parsed to into this s-expression.

~~~cl
((feed ((xmlns . "http://www.w3.org/2005/Atom"))
       (title () "Example Feed")
       (link ((href . "http://example.org/")))
       (updated () "2003-12-13T18:30:02Z")
       (author () (name () "John Doe"))
       (id () "urn:uuid:60a76c80-d399-11d9-b93C-0003939e0af6")
       (entry ()
              (title () "Atom-Powered Robots Run Amok")
              (link ((rel . "alternate")
                     (href . "http://example.org/2003/12/13/atom03")))
              (id () "urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a")
              (updated () "2003-12-13T18:30:02Z")
              (summary () "Some text."))))
~~~

Each XML element is converted to a list. The first item is a symbol
that is the element's name. The second item is an alist of attributes
— cons pairs of symbols and strings. And the rest are its children,
both string nodes and other elements. I've trimmed the extraneous
string nodes from the sample s-expression.

A subtle detail is that `xml-parse-region` doesn't just return the
root element. It returns a *list of elements*, which always happens to
be a single element list, which is the root element. I don't know why
this is, but I've built everything to assume this structure as input.

Elfeed strips all namespaces stripped from both elements and
attributes to make parsing simpler. As I said, it's heuristic rather
than strict, so namespaces are treated as noise.

### A domain-specific language

Coding up Elfeed's s-expression searches in straight Emacs Lisp would
be tedious, error-prone, and difficult to understand. It's a lot of
loops, `assoc`, etc. So instead I invented a jQuery-like, CSS
selector-like, domain-specific language (DSL) to express these
searches concisely and clearly.

For example, all of the entry links are "selected" using this
expression:

~~~cl
(feed entry link [rel "alternate"] :href)
~~~

Reading right-to-left, this matches every `href` attribute under every
`link` element with the `rel="alternate"` attribute, under every
`entry` element, under the `feed` root element. Symbols match element
names, two-element vectors match elements with a particular attribute
pair, and keywords (which must come last) narrow the selection to a
specific attribute value.

Imagine hand-writing the code to navigate all these conditions for
each piece of information that Elfeed requires. The RSS parser makes
up to 16 such queries, and the Atom parser makes as many as 24. That
would add up to a lot of tedious code.

The package (included with Elfeed) that executes this query is called
"xml-query." It comes in two flavors: `xml-query` and `xml-query-all`.
The former returns just the first match, and the latter returns all
matches. The naming parallels the `querySelector()` and
`querySelectorAll()` DOM methods in JavaScript.

~~~cl
(let ((xml (elfeed-xml-parse-region)))
  (xml-query-all '(feed entry link [rel "alternate"] :href) xml))

;; => ("http://example.org/2003/12/13/atom03")
~~~

That date search I mentioned before looks roughly like this. The `*`
matches text nodes within the selected element. It must come last just
like the keyword matcher.

~~~cl
(or (xml-query '(feed entry published *))
    (xml-query '(feed entry updated *))
    (xml-query '(feed entry date *))
    (xml-query '(feed entry modified *))
    (xml-query '(feed entry issued *))
    (current-time))
~~~

Over the past three years, Elfeed has gained more and more of these
selectors as it collects more and more information from feeds. Most
recently, Elfeed collects author and category information provided by
feeds. Each new query slows feed parsing a little bit, and it's a
perfect example of a program slowing down as it gains more features
and capabilities.

But I don't want Elfeed to slow down. I want it to get *faster*!

### Optimizing the domain-specific language

Just like the primary jQuery function (`$`), both `xml-query` and
`xml-query-all` are functions. The xml-query engine processes the
selector from scratch on each invocation. It examines the first
element, dispatches on its type/value to apply it to the input, and
then recurses on the rest of selector with the narrowed input,
stopping when it hits the end of the list. That's the way it's worked
from the start.

However, every selector argument in Elfeed is a static, quoted list.
[Unlike user-supplied filters][jit], I know exactly what I want to
execute ahead of time. It would be much better if the engine didn't
have to waste time reparsing the DSL for each query.

This is the classic split between interpreters and compilers. An
interpreter reads input and immediately executes it, doing what the
input tells it to do. A compiler reads input and, rather than execute
it, produces output, usually in a simpler language, that, when
evaluated, has the same effect as executing the input.

Rather than interpret the selector, it would be better to compile it
into Elisp code, compile that [into byte-code][bc], and then have the
Emacs byte-code virtual machine (VM) execute the query each time it's
needed. The extra work of parsing the DSL is performed ahead of time,
the dispatch is entirely static, and the selector ultimately executes
on a much faster engine (byte-code VM). This should be a lot faster!

So I wrote a function that accepts a selector expression and emits
Elisp source that implements that selector: a compiler for my DSL.
Having a readily-available syntax tree is one of the [big advantages
of homoiconicity][hi], and this sort of function makes perfect sense
in a lisp. For the external interface, this compiler function is
called by a new pair of macros, `xml-query*` and `xml-query-all*`.
These macros consume a static selector and expand into the compiled
Elisp form of the selector.

To demonstrate, remember that link query from before? Here's the macro
version of that selection, but only returning the first match. Notice
the selector is no longer quoted. This is because it's consumed by the
macro, not evaluated.

~~~cl
(xml-query* (feed entry title [rel "alternate"] :href) xml)
~~~

This will expand into the following code.

~~~cl
(catch 'done
  (dolist (v xml)
    (when (and (consp v) (eq (car v) 'feed))
      (dolist (v (cddr v))
        (when (and (consp v) (eq (car v) 'entry))
          (dolist (v (cddr v))
            (when (and (consp v) (eq (car v) 'title))
              (let ((value (cdr (assq 'rel (cadr v)))))
                (when (equal value "alternate")
                  (let ((v (cdr (assq 'href (cadr v)))))
                    (when v
                      (throw 'done v))))))))))))
~~~

As soon as it finds a match, it's thrown to the top level and
returned. Without the DSL, the expansion is essentially what would
have to be written by hand. **This is *exactly* the sort of leverage
you should be getting from a compiler.** It compiles to around 130
byte-code instructions.

The `xml-query-all*` form is nearly the same, but instead of a
`throw`, it pushes the result into the return list. Only the prologue
(the outermost part) and the epilogue (the innermost part) are
different.

Parsing feeds is a hot spot for Elfeed, so I wanted the compiler's
output to be as efficient as possible. I had three goals for this:

* **No extraneous code.** It's easy for the compiler to emit
  unnecessary code. The byte-code compiler might be able to eliminate
  some of it, but I don't want to rely on that. Except for the
  identifiers, it should basically look like a human wrote it.

* **Avoid function calls.** I don't want to pay function call
  overhead, and, with some care, it's easy to avoid. In the
  `xml-query*` expansion, the only function call is `throw`, which is
  unavoidable. The `xml-query-all*` version makes no function calls
  whatsoever. Notice that I used `assq` rather than `assoc`. First, it
  only needs to match symbols, so it should be faster. Second, `assq`
  has its own byte-code instruction (158) and `assoc` does not.

* **No unnecessary memory allocations**. The `xml-query*` expansion
  makes *no* allocations. The `xml-query-all*` version only conses
  once per output, which is the minimum possible.

The end result is at least as optimal as hand-written code, but
without the chance of human error (typos, fat fingering) and sourced
from an easy-to-read DSL.

### Performance

In my tests, the **xml-query macros are a full order of magnitude
faster than the functions**. Yes, ten times faster! It's an even
bigger gain than I expected.

In the full picture, xml-query is only one part of parsing a feed.
Measuring the time starting from raw XML text (as [delivered by
cURL][cg]) to a list of database entry objects, I'm seeing an
**overall 25% speedup** with the macros. The remaining time is
dominated by `xml-parse-region`, which is mostly out of my control.

With xml-query so computationally cheap, I don't need to worry about
using it more often. Compared to parsing XML text, it's virtually
free.

When it came time to validate my DSL compiler, I was *really* happy
that Elfeed had a test suite. I essentially rewrote a core component
from scratch, and passing all of the unit tests was a strong sign that
it was correct. Many times that test suite has provided confidence in
changes made both by me and by others.

I'll end by describing another possible application: Apply this
technique to regular expressions, such that static strings containing
regular expressions are compiled into Elisp/byte-code via macro
expansion. I wonder if situationally this would be faster than Emacs'
own regular expression engine.


[jit]: /blog/2016/12/11/
[cg]: /blog/2016/06/16/
[avr]: /blog/2013/09/23/
[dc]: https://www.intertwingly.net/wiki/pie/DublinCore
[atom]: https://tools.ietf.org/html/rfc4287
[bc]: /blog/2014/01/04/
[hi]: https://en.wikipedia.org/wiki/Homoiconicity
