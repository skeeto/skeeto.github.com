---
title: Elisp Higher-order Conversion to Interactive
layout: post
tags: [emacs, elisp]
uuid: 0677cf0d-4ba9-300f-3bf0-795a821b1287
---

<!-- 29 September 2010 -->

For those not familiar with extending Emacs, when you create a
function in Elisp it cannot be called directly by the user
("interactively") without declaring the function interactive. The
simplest way to do this is by adding `(interactive)` to the
top of the function definition. The `interactive` call can
be made more complex, if needed, to ask the user interactively for
input.

```cl
(defun hello-world ()
  "Example function."
  (interactive)
  (message "hello"))
```

There are some handy higher-order functions in Elisp, such
as `compose` and `apply-partially`. Today I
wanted to bind the output of `apply-partially` to a key. My
situation was this: I use `revert-buffer` often enough that
it needs a binding. Also because I use it so much, I wanted it to
stop asking me for confirmation. (Yes,
there [ are other
ways to do this](http://www.emacswiki.org/emacs/YesOrNoP) including `revert-without-query`, but I
wanted a general solution.) Using `apply-partially` I could
supply the needed function arguments at keybind time.

The problem is that you can only bind interactive functions, and the
output of `apply-partially` is not interactive. A quick way
to work around this is to wrap it in an anonymous function, which also
takes away the need for `apply-partially`.

```cl
(lambda () (interactive) (revert-buffer nil t))
```

I'd rather there be *another* higher-order function that takes a
non-interactive function and creates an interactive version. Here it is,

```cl
;; ID: c7db6dec-e7ab-3b0f-bf26-0fa268674c6c
(defun expose (function)
  "Return an interactive version of FUNCTION."
  (lexical-let ((lex-func function))
    (lambda ()
      (interactive)
      (funcall lex-func))))
```

Now the binding looks like this,

```cl
(global-set-key [f2] (expose (apply-partially 'revert-buffer nil t)))
```

I think this more clearly expresses my intention than
the `lambda` wrapper would. Maybe?
