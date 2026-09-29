---
title: BrianScheme
layout: post
tags: [lisp, lang]
uuid: 6d2dc892-9e47-3b94-dd9b-a1027340fef9
---

<!-- 11 January 2011 -->

Remember back a year ago [I tried my hand
at a Lisp implementation called Wisp](/blog/2010/01/24/)? Well, currently a co-worker
of mine, [Brian Taylor](http://wubo.org/), is similarly
working on his own Scheme implementation — but he knows more about
what he's doing than I did, so it's more interesting. However, that
expertise doesn't extend to inventing a clever name (Zing!): it's
unsubtly called [ BrianScheme](https://github.com/netguy204/brianscheme).

```
git clone git://github.com/netguy204/brianscheme.git
```

I've been [ hacking at it a little myself](https://github.com/skeeto/brianscheme), cheering from the sidelines.

```
git remote add wellons git://github.com/skeeto/brianscheme.git
```

Like Wisp, it's written from scratch in C from the bottom up. Unlike
Wisp, it has closures, lexical scoping, mark-and-sweep garbage
collection, object system, and compiles to a bytecode (in
memory). Continuations are still a ways off, but planned. One of the
most powerful features so far is the foreign function interface
(FFI). Now that he's implemented it
with [ libffi](http://sourceware.org/libffi/) he's barely
had to touch the C code base. In fact, thanks to the FFI, the the C
portion of BrianScheme will be shrinking.

For example, BrianScheme currently lacks floating point numbers, and
its integers are currently just native fixnums. Sometime soon it will,
like Wisp, use the GNU Multi-Precision Library (GMP) to provide
bignums. Adding this will not require making *any* changes
whatsoever to the C code. Using the object system
([Tiny-CLOS](http://community.schemewiki.org/?Tiny-CLOS)),
hooks in the reader and printer, and the FFI, this can be entirely
implemented in the language itself.

Just-in-time compilation (JIT) has begun to be implemented without
touching C. Again, done by pulling in
in [ libjit](http://freshmeat.net/projects/libjit/) with
the FFI.

Because I wrote Wisp to be embeddable and a library, I was able to run
Wisp in BrianScheme, via the FFI, and expose some bindings. For
example, I can send it s-expressions to evaluate,

```
> (require 'wisp)
> (wisp:eval '(expt 6 56))
37711171281396032013366321198900157303750656
```

BrianScheme doesn't currently support threading, mainly because the
garbage collector isn't ready for it. But remember
how [I mentioned GNU Pth last month](/blog/2010/12/21/)?
Again, I was able to load Pth with the FFI to add userspace threading,
which *is* safe for the garbage collector because it's
effectively an atomic operation. (Once continuations are implemented,
this could actually be implemented without Pth, just by making good
use of those continuations.) The current hangup is the REPL, which
doesn't know about Pth and so it never yields. To take advantage of
threading you have to suspend the REPL (with `pth:join`).

This REPL issue should be solved with the long term goal for
BrianScheme. The C component of BrianScheme will merely exist for the
purposes of bootstrapping the full system. During initialization, just
about everything will be redefined in BrianScheme, with the original C
definitions only living long enough to load what's needed. This
includes reimplementing the reader itself in BrianScheme, which
enables all sorts of possibilities, like the previously mentioned
bignums implemented in the language
itself, [ inline regular expressions](/blog/2010/04/23/),
and proper yielding to the userspace thread scheduler.

So go ahead and clone Brian's repository (and add mine as a remote,
too! :-D) and poke around at it. To compare to Wisp again, it's not
quite as stable at the moment. It exits very easily from runtime
errors, due to lacking error handling, so an instance generally
doesn't live very long at the moment. This will probably be resolved
sometime soon. Except for that, it does play well with Emacs as
an `inferior-lisp`.
