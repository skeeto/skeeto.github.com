---
title: JavaScript Strings as Arrays
layout: post
tags: [javascript, lisp]
uuid: 4e3a5238-827b-38f0-242e-caae33f379d8
---

### Lisp

One thing I enjoy about Common Lisp is its general treatment of
sequences. (In fact, I wish it went further with it!) Functions that
don't depend on list-specific features generally work with any kind of
sequence. For example, `remove-duplicates` doesn't just work with lists,
it works on any sequence.

~~~cl
(remove-duplicates '(a b b c))  ; list
=> (A B C)

(remove-duplicates #(a b b c))  ; array
=> #(A B C)
~~~

Functions like `member` and `mapcar` require lists because their
behavior explicitly uses them. The general sequence version of these
are `find` and `map`. Writing a new sequence function means sticking
to these generic sequence functions, particularly `elt` and `subseq`
rather than the more specialized accessors.

A string is just a one-dimensional array — a vector — with elements
of the type *character*. This means all sequence functions also work
on strings.

~~~cl
(make-array 10 :element-type 'character :initial-element #\a)
=> "aaaaaaaaaa"

(remove-duplicates "abbc")
=> "abc"

(map 'string #'char-upcase "foo")
=> "FOO"

(reverse "foo")
=> "oof"
~~~

There is no special set of functions just for operating on strings
(except those for string-specific operations). Strings are as powerful
and flexible as any other sequence. This is very convenient.

### JavaScript

Unfortunately, JavaScript strings aren't *quite* arrays. They look and
act a little bit like arrays, but they're missing a few of the useful
methods.

~~~javascript
var foo = "abcdef";

foo[1]
=> "b"

foo.length
=> 6

foo.reverse()  // error, no method 'reverse'
~~~

Notice that, when indexing, it returns a one-character string, not a
single character. This is because there's no character type in
JavaScript. It would have been interesting if JavaScript had gone the
Elisp route, where there's no character type but instead characters
are represented by integers, with some sort of character literal for
using characters in code. This sort of thing can be emulated with the
`charCodeAt()` method.

To work around the strings-are-not-arrays thing, strings can be
converted to arrays with `split()`, manipulated as an array, and
restored with `join()`.

~~~javascript
foo.split('').reverse().join('')
=> "fedcba"
~~~

The string method `replace` can act as a stand-in for `map` and
`filter`. The replacement argument can be a function, which will be
called on each match. If a single character at a time is selected for
replacement then what's left is the `map` method.

~~~javascript
// Map over each character
foo.replace(/./g, function(c) {
    return String.fromCharCode(c.charCodeAt(0) + 10);
});
=> "klmnop"
~~~

For `filter`, an empty string would be returned in the case of the
predicate returning `false` and the original match in the case of
`true`.

~~~javascript
foo.replace(/./g, function(c) {
    if ("xyeczd".indexOf(c) >= 0)
        return c;
    else
        return '';
});
=> "cde"
~~~

In most cases, typical use of regular expressions would serve the need
for the `filter()` method, so this is mostly unnecessary. For example,
the above could also be done like so,

~~~javascript
foo.replace(/[^xyeczd]/g, '');
~~~

Another way to fix the missing methods would be to simply implement
the Array methods for strings and add them to the String prototype,
but that's generally considered bad practice.
