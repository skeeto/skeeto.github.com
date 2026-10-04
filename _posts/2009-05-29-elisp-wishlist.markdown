---
title: Elisp Wishlist
layout: post
tags: [rant, emacs, elisp, lisp]
uuid: 41fa774c-1f9e-3ef1-1029-69b775475150
---

<!-- 29 May 2009 -->

*<strong>Update:</strong> It looks like all these wishes, except the last one,
may actually be coming
true! [ Guile can run Elisp better than Emacs](http://lists.gnu.org/archive/html/emacs-devel/2010-04/msg00665.html)! The idea is that the Elisp
engine is replaced with Guile — the GNU project's Scheme
implementation designed to be used as an extension language — and
written in Scheme is an Elisp compiler that targets Guile's VM. The
extension language of Emacs then becomes Scheme, but Emacs is still
able to run all the old Elisp code. At the same time Elisp itself,
which I'm sure many people will continue to use, gets an upgrade of
arbitrary precision, closures, and better performance.*

I've been using elisp a lot lately, but unfortunately it's missing a
lot of features that one would find in a more standard lisp. The
following are some features I wish elisp had. Many of these could be
fit into a generic "be more like Scheme or Common Lisp". Some of these
features would break the existing mountain of elisp code out there,
requiring a massive rewrite, which is likely the main reason they are
being held back.

**Closures**, and maybe continuations. Closures are one of the
features I miss the most when writing elisp. They would allow the
implementation of Scheme-style lazy evaluation with `delay`
and `force`, among other neat tools. Continuations would
just be a neat thing to have, though they come with a performance
penalty.

Closures would also pretty much require Emacs switch to lexical
scoping.

**Arbitrary precision**. Really, any higher order language's
numbers should be bignums. Emacs 22 *does* come with the Calc
package which provides arbitrary precision via
`defmath`. Perl does something like this with the bignum
module.

**Packages/namespaces**. Without namespaces all of the Emacs
packages prefix their functions and variables with its name
(i.e. `dired-`). Some real namespaces would be useful for
large projects.

**C interface**. This is something GNU Emacs will never have
because Richard Stallman considers Emacs shared libraries support to
be [ a GPL threat](http://www.emacswiki.org/emacs/DynamicallyExtendingEmacs). If Emacs could be dynamically extended some useful
libraries could be linked in and exposed to elisp.

**Concurrency**. If some elisp is being executed Emacs will lock
up. This is a particular problem for Gnus. Again, Emacs would really
need to switch to lexical scoping before this could happen. Threading
would be nice.

**Speed**. Emacs lisp is pretty slow, even when compiled. Lexical
scoping would help with performance (compile time vs. run time
binding).

**Regex type**. I mention this last because I think this would be
really cool, and I am not aware of any other lisps that do it. Emacs
does regular expressions with strings, which is silly and
cumbersome. Backslashes need extra escaping, for example. Instead, I
would rather have a regex type like Perl and Javascript have. So
instead of,

```elisp
(string-match "\\w[0-9]+" "foo525")
```

we have,

```elisp
(string-match /\w[0-9]+/ "foo525")
```

Naturally there would be a `regexpp` predicate for checking
its type. There could also be a function for compiling a regexp from a
string into a regexp object. As a bonus, I would also like to use it
directly as a function,

```elisp
(/\w[0-9]+/ "foo525")
```

I think a regexp price would really give elisp an edge, and would be
entirely appropriate for a text editor. It could also be done without
breaking anything (keep string-style regexp support).

There is more commentary over at EmacsWiki: [ Why Does Elisp
Suck](http://www.emacswiki.org/emacs/WhyDoesElispSuck).
