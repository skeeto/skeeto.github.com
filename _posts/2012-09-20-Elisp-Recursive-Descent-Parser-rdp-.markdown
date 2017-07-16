---
title: Elisp Recursive Descent Parser (rdp)
layout: post
tags: [emacs, elisp, lisp]
uuid: 0cb87ff3-6862-3772-6d64-3222ff8e56fe
---

I recently developed a recursive descent parser, named rdp, for use in
Emacs Lisp programs. I've already used it to write a compiler.

 * [https://github.com/skeeto/rdp](https://github.com/skeeto/rdp)

It's available as a package on [MELPA](http://melpa.milkbox.net/).

### The Long Story

Last month [Brian](http://www.50ply.com/) invited me to take
[a free, online programming languages course][cs173] with him. You
may recall that [we developed a programming language
together](/blog/2011/01/11/) so it was only natural we would take
this class.

[cs173]: (http://www.cs.brown.edu/courses/cs173/2012/)

The first part of the class is oriented around a small programming
language created just for this class called [ParselTongue][spec].  It
looks like this:

    deffun evenp(x)
        if ==(x, 0) then
            true
        else if ==(x, 1) then
                false
            else evenp(-(x, 2))
    in defvar x = 14 in {
        while (evenp(x)) { x--; };   # Make sure x odd
        print("This is an odd number: ");
        print(x);
        ""; # No output
    }

[spec]: http://www.cs.brown.edu/courses/cs173/2012/Assignments/ParselTest/

I've gotten so used to having a solid Emacs major mode when coding
that I can't stand writing code without the support of a major
mode. Since this language was invented recently *just* for this class
there was no mode for it, nor would there be unless someone stepped up
to make one. I ended up taking that role. It was an opportunity to
learn how to create a major mode, something I had never done before.

It's called [psl-mode](https://github.com/skeeto/psl-mode).

At first it was just some syntax highlighting (very easy) and some
poor automatic indentation. The indentation function would get
confused by anything non-trivial. It's actually *really* hard to get
it right. I've grown a much better appreciation for automatic
indentation in other modes.

In an attempt to improve this I decided I would try to fully parse the
language and use the resulting parse tree to determine indentation —
something like the depth of the pointer in the
tree. [My experience with Perl's Parse::RecDescent](/blog/2009/01/04/)
some years ago was very positive and I wanted to reproduce that
effect. However, rather than write the grammar in a separate language
that mixes in the programming language, which I find extremely messy,
instead I wanted to use pure s-expressions. A grammar looks very nice
as an alist of symbols.

#### Arithmetic Parser

For example, here's a grammar for simple arithmetic expressions,
including operator precedence and grouping (i.e. "4 + 5 * 2.5",
"(4 + 5) * 2.5", etc.).

~~~cl
(defvar arith-tokens
  '((sum       prod  [([+ -] sum)  no-sum])
    (prod      value [([* /] prod) no-prod])
    (num     . "-?[0-9]+\\(\\.[0-9]*\\)?")
    (+       . "\\+")
    (-       . "-")
    (*       . "\\*")
    (/       . "/")
    (pexpr     "(" [sum prod num pexpr] ")")
    (value   . [pexpr num])
    (no-prod . "")
    (no-sum  . "")))
~~~

Strings are regular expressions , the only thing to actually match
input text (*terminals*). Lists are *sequences*, where each element in
the list must match in order. Vectors (in brackets) are *choices*
where one of the elements must match. Symbols name an expression so
that it can be referred to by other expression recursively.

Give this alist to the parser and it will return an s-expression of
the parse tree of the current buffer. Due to the way the grammar must
be written this parse tree isn't really pleasant to handle
directly. For example, a series of multiplications ("1 * 2 * 3 * 4")
wouldn't parse to a nice flat list but with further depth for each
additional operand.

To help squash these, the parser will accept an alist of symbols and
functions which process the parse tree at parse time. For example,
these corresponding functions will make sure `"4 * 5 * 6"` gets parsed
into `(* 4 (* 5 (* 6 1)))`.

~~~cl
(defun arith-op (expr)
  (destructuring-bind (a (op b)) expr
    (list op a b)))

(defvar arith-funcs
  `((sum     . ,#'arith-op)
    (prod    . ,#'arith-op)
    (num     . ,#'string-to-number)
    (+       . ,#'intern)
    (-       . ,#'intern)
    (*       . ,#'intern)
    (/       . ,#'intern)
    (pexpr   . ,#'cadr)
    (value   . ,#'identity)
    (no-prod . ,(lambda (e) '(* 1)))
    (no-sum  . ,(lambda (e) '(+ 0)))))
~~~

Notice how normal Emacs functions could be supplied directly in most
cases! That makes this approach so elegant in my opinion.

Also, in `arith-op` note the use of `destructuring-bind`. I've found
that macro to be invaluable when writing these syntax tree functions.

In this case, we can be even more clever. Rather than build a nice
parse tree, the expression can be evaluated directly. All it takes is
one small change,

~~~cl
(defun arith-op (expr)
  (destructuring-bind (a (op b)) expr
    (funcall op a b)))
~~~

With this, the parser returns the computed value directly. So this
evaluates to 120.

~~~cl
(rdp-parse-string "4 * 5 * 6" arith-tokens arith-funcs)
~~~

#### ParselTongue Compiler

I discovered this useful side effect while making my ParselTongue
parser. The original intention was that I'd parse the buffer for use
in indentation, then maybe I'd create an interpreter to evaluate the
parser output. However, the resulting parse tree was looking a lot
like Elisp. In an epiphany I realized I could simply emit valid Elisp
directly and forgo writing the interpreter altogether. And so I
accidentally created a ParselTongue compiler! This was incredibly
exciting for me to realize.

This ParselTongue program,

    defvar obj = {x: 1} in { obj.x }

Compiles to this Elisp,

~~~cl
(let ((obj (list (cons 'x 1))))
  (progn (cdr (assq 'x obj))))
~~~

Because it compiles to such a high level language, and because
ParselTongue is very Lisp-like semantically, it's a bit
unconventional: the compiler emits code *during* parsing. In fact,
when the parser backtracks, some emitted code is thrown away.

By the end of the first evening I had implemented the majority of the
compiler, which quickly took precedence over indentation. The compiler
is now integrated as part of psl-mode. The current buffer can be
evaluated at any time with `psl-eval-buffer`. This function compiles
the buffer and has Emacs `eval` the result, printing the output in the
minibuffer. Compiler output can be viewed with
`psl-show-elisp-compilation` (mostly for my own debugging).

After a few days I integrated indentation with parsing, which required
modifying the parser (changes included in rdp itself). The parser
needed to keep track of where the point is in the parse tree. For
indentation it basically counts the depth into the parse tree, plus a
few more checks for special cases.

The parser was intentionally isolated from the rest of psl-mode so
that it could be separated for general use, which I have now
done. It's been a *really* handy general purpose tool since then. That
arithmetic parser is only 35 lines of code and took about half-an-hour
to create.

#### Future Directions

I also [wrote a bencode parser][bencode.el] — *only* the
`bencode-tokens` and `bencode-funcs` alists are needed to parse
bencode, about 30 LOC. Careful observation will reveal that I cheated
and the result is a little hackish. Due to the way strings work,
bencode is *not* context-free so it can't be parsed purely by the
grammar. I can work around it by having the parse tree function for
strings consume input, since it's called during parsing.

[bencode.el]: (https://github.com/skeeto/emacs-torrent/blob/master/bencode.el)

I'll be using rdp to parse many more things in the future, I'm
sure. It's much more powerful than I expected.
