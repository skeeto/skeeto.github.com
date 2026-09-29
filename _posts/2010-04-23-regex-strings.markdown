---
title: The Problem with String Stored Regex
layout: post
tags: [lisp, rant]
uuid: b821299d-d1f4-307c-4d93-d8a829b7e5c2
---

<!-- 23 April 2010 -->

While regular expressions
have [ limited usefulness](http://en.wikipedia.org/wiki/Regular_expression#Patterns_for_non-regular_languages), especially in larger programs, they're still
very handy to have from time to time. It's usually difficult to write
a lexer or tokenizer without one. Because of this several languages
build them right into the language itself, rather than tacked on as a
library. It allows the regular expressions to be stored literally in
the code, treated as its own type, rather than inside a string. The
problem with storing a regular expression inside a string is that it
can easily make an already complex regular expression much more
complex. This is **because there are two levels of parsing going
on**.

Consider this regular expression where we match an alphanumeric word
inside of quotes. I'm going to use slashes to delimit the regular
expression itself.

```
/"\w+"/
```

Notice there is no escaping going on. The backslash is there is
indicate a special sequence `\w`, which is equal
to `[a-zA-Z0-9_]`. This will
get [ parsed and
compiled into some form in memory](http://swtch.com/~rsc/regexp/regexp1.html) before it is run by a
program. If the language doesn't directly support regular expressions
then we usually can't put it in the code as is, since the language
parser won't know how to deal with it. The solution is to store it
inside of a string.

However, our regular expression contains quotes and these will need to
be escaped when in a quote delimited string. But I no longer need
slashes to delimit my regular expression.

```
"\"\w+\""
```

Did you notice the error yet? If not, stop and think about it for a
minute. Our special sequence `\w` will not make it intact
to the regular expression compiler. That backslash will escape
the `w` during the string parsing step, leaving only
the `w`. The string we typed will get parsed into a series
of characters in memory, performing escapes along the way, and
then *that* sequence will be handed to the regular expression
compiler. So we have to fix it,

```
"\"\\w+\""
```

That's getting hard to understand, compared to the original. Now let's
throw a curve-ball into this: let's match a backslash at the beginning
of the word. The normal regular expression looks like this now,

```
/"\\\w+"/
```

We have to escape our backslash to make it a literal backslash, so it
takes two of them. Now, when we want to do this in a string-stored
regular expression we have to escape both of those backslashes
again. It looks like this,

```
"\"\\\\\\w+\""
```

Now to match a single backslash we have to insert four backslashes!
Quite unfortunately, [ Emacs Lisp doesn't
directly support regular expressions](/blog/2009/05/29/) even though the language has
a lot of emphasis on text parsing, so a lot of Elisp code is riddled
with this sort of thing. Elisp is especially difficult because
sometimes, such as during prompts, you can enter a regular expression
directly and can ignore the layer of string parsing. It's a very
conscious effort to remember which situation you're in at different
times.

Perl, Ruby, and JavaScript have regular expressions as part of the
language and it makes a lot of sense for these languages; they tend to
do a lot of text parsing. Python does it partially, with
its `r'` syntax. Any string preceded with an `r`
loses its escape rules, but it also means you can't match both single
or double quotes without falling back to a normal string with
escaping. Common Lisp may be able to do it with a
[ reader macro](http://www.lispworks.com/documentation/lw51/CLHS/Body/02_b.htm), but I've never seen it done.

Remember those two levels of parsing when writing string stored
regex. It helps avoid hair-pulling annoying mistakes.
