---
title: A Handy Emacs Package Configuration Macro
layout: post
tags: [emacs, elisp]
uuid: 3e64c259-69a6-3500-0e87-3a93d61c1644
---

*Update April 2015*: I now use [use-package][use-package] instead of
the `with-package` macro explained below. It's cleaner, nicer, and
better maintained.

I was inspired by [a post recently written by Milkypostman][after]
(the M in MELPA). He describes some of his `init.el` configuration,
specifically focusing on an `after` macro that wraps the misdesigned
`eval-after-load` function. I wanted to take this macro further in
three ways:

 * The delayed expression should be [properly byte-compiled][compile],
   which doesn't happen by default with `eval-after-load`.

 * In a few cases my expression depends on multiple, independent
   packages but `eval-after-load` only accepts one.

 * If I'm specifying packages when using my macro, why bother listing
   them at the top of my initialize file? I could DRY things up by
   learning what packages to install when the macro is used. Here's
   the kicker: **I can pretend that every available package is already
   installed like built-in packages!**

The result is a pair of macros `with-package` and `with-package*`
which can be found in [package-helper.el][helper]. The latter form
doesn't wait but immediately loads the specified packages with
`require`. It's shaped just like Milkypostman's `after` macro, except
that it can accept a list of packages in place of a single symbol.
Also, the package names aren't quoted; they don't need to be since
this is a macro instead of a function.

Here's a typical use case for each macro. That `expose` higher-order
function is [from my personal `utility` library][expose]. The
expressions to be evaluated depend on both packages and neither needs
to be loaded immediately, so I'm using the first form of the macro.

~~~cl
(with-package (skewer-mode utility)
  (skewer-setup)
  (define-key skewer-mode-map (kbd "C-c $")
    (expose #'skewer-bower-load "jquery" "1.9.1")))

(with-package* smex
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex))
~~~

For the second one, I'm going to be using smex right away (takes over
`M-x`), so I use the second form, which immediately loads smex. The
macro isn't really necessary at all here since I could just use
`require` and follow it with these expressions, but I *really* like
how this organizes my `init.el`. It creates a domain-specific language
(DSL) just for Emacs configuration. Each package configuration is
grouped up in a clean `let`-like form. Since I've added syntax
highlighting to `with-package` it looks very elegant. Normal syntax
highlighters aren't going to do this, so here's a screenshot of my
buffer.

![](/img/emacs/with-package.png)

JavaScript developers with a keen eye may notice a familiar pattern
here. This macro is shaped a bit like the
[Asynchronous Module Definition (AMD)][amd], with asynchronousy in
mind. Since this is Lisp with a powerful macro system, I get to hide
away the function wrapper part.

Using this macro has caused me to use `eval-after-load` with just
about everything. This has cut my initialization time down to about
10% of what it was before! On those occasions that I *do* restart
Emacs, it's really nice that it's back to under 1 second (0.6 seconds
vs 6 seconds).

### The problem of eval-after-load

I'm calling `eval-after-load` poorly designed because it's a perfect
example of an inappropriate use of `eval`. In function form it
*should* have accepted a function as its second argument instead of an
s-expression, so it would work like a hook. This is even more
inappropriate now that Emacs has proper lexical closures, which is the
perfect mechanism for delayed evaluation. **The whole point of
`eval-after-load` is to speed up Emacs initialization time, but *using
`eval` is slow***. To the compiler, this isn't code, just data. This
means no byte-compilation and no compiler warnings.

A possible alternative design for `eval-after-load` would be a hook
named something like `<package>-load-hook`. Then when `load` or
`require` loads a file, it runs the hook with the matching name. This
removes `eval-after-load` as its own standalone language concept.

~~~cl
(add-hook 'skewer-mode-load-hook (lambda () ...))
~~~

The problem here is when the package is already loaded the hook is
never run. In contrast, when `eval-after-load` is used on an
already-loaded package, the expression is immediately evaluated.

Given this, if there was something I could change about this it would
simply be for `eval-after-load`, whatever it would be called, to take
a function for the second argument. I would also provide a simple
macro just like `after` that wraps this function. Why not just a
macro? The function form would be really useful for a situation like
this,

~~~cl
(eval-after-load 'skewer-mode #'skewer-setup)
~~~

Here there's no need to instantiate a new anonymous function or
s-expression. If all it's doing is calling a zero-arity function, that
function can be passed in directly.


[after]: http://milkbox.net/note/single-file-master-emacs-configuration/
[compile]: http://lunaryorn.com/blog/2013/05/31/byte-compiling-eval-after-load/
[helper]: https://github.com/skeeto/.emacs.d/blob/master/lisp/package-helper.el
[expose]: /blog/2010/09/29/
[amd]: https://github.com/amdjs/amdjs-api/wiki/AMD
[use-package]: https://github.com/jwiegley/use-package
