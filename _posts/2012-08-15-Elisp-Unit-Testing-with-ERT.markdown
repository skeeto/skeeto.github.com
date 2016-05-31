---
title: Elisp Unit Testing with ERT
layout: post
tags: [emacs, elisp]
uuid: f5798f49-155b-3038-a8d9-4f5a5f1c2d0c
---

Emacs 24 comes with a unit testing library, ERT (Emacs Lisp
Regression Testing). I learned about it after watching
[Extending Emacs Rocks!](http://emacsrocks.com/) and I've been using
it ever since. It's been a pleasant experience; enough so that
[I made a key binding for it](https://github.com/skeeto/.emacs.d/commit/59d3eac73edbad8a5be72a81c7d6c5b1193bbb90)
so that I can effortlessly run tests at any time. When I recently made
a major overhaul to my Emacs web server I added
[a small test suite](https://github.com/skeeto/emacs-http-server/blob/master/simple-httpd-test.el)
using ERT.

Emacs also comes with the ERT manual so it's easy to start learning,
but here's the gist of it. There are essentially two macros to worry
about: `ert-deftest` and `should`. The first is used to create tests
and the second behaves like `assert` but with nicer behavior. Here's
an example,

~~~cl
(ert-deftest example-test ()
  (should (= (+ 9 2) 11)))
~~~

`ert-deftest` is what you'd expect from every other `def*`. The empty
parameter list does nothing at the moment other than to make it feel
like writing a `defun`. The body is evaluated as normal. This is all
turned into an anonymous function which is stuffed in the *plist* of
the symbol `example-test`. When it comes time to running tests, they
are found by searching the plists of every interned symbol.

The other macro, `should`, takes one argument: a form that *should*
evaluate to true. There is also a `should-not` and a `should-error`,
which do what you would expect.

Tests are run with `M-x ert`. It will ask for a *test selector*, where
`t` selects all defined tests. There are many ways to select a subset
of all tests (`:new`, `:passed`, `:failed`, etc.) but I usually just
run all of them (as my key binding makes obvious). The results are
displayed in a separate pop-up buffer which, as usual, can be
dismissed with `q`.

### Running ERT

What makes `should` special is error reporting. When tests fail you
will be provided with the forms that failed and their return
values. For example, if we modify the test above to fail.

~~~cl
(ert-deftest example-test ()
  (should (= (+ 9 2) 100)))
~~~

Then run the test and it will note the failure. There is also some red
coloring not captured here.

~~~cl
F example-test
    (ert-test-failed
     ((should
       (=
        (+ 9 2)
        100))
      :form
      (= 11 100)
      :value nil))
~~~

Displayed are the forms we were comparing — `(+ 9 2)` and `100` — and
what they evaluated to: `(= 11 100)`. If I put the point at the test
result and type `.` it will take me to the test definition so that I
can start looking further. Or I can press `b` to see a backtrace, `m`
to see all output messages from that test, or, if I'm in disbelief,
`r` to rerun that test.

### Mocking

Elisp's dynamic bindings really come in handy when functions need to
be mocked. For example, say I have a function that, at some point,
needs to check whether or not a particular file exists. This would be
done using `file-exists-p`. Creating or removing the file in the
filesystem before the test isn't a well-contained unit test. Tests
running in parallel could interfere and there are a number of ways
something could go wrong.

Instead I'll temporarily override the definition of `file-exists-p`
with a *mock* function using `let`'s cousin, `flet`. Note that
`file-exists-p` is a C source function but I can still override it as
if it was any regular lisp function.

~~~cl
(defun determine-next-action ()
  (if (file-exists-p "death-star-plans.org")
      'bring-him-the-passengers
    'tear-this-ship-apart))

(ert-deftest file-check-test ()
  (flet ((file-exists-p (file) t))
    (should (eq (determine-next-action) 'bring-him-the-passengers)))
  (flet ((file-exists-p (file) nil))
    (should (eq (determine-next-action) 'tear-this-ship-apart))))
~~~

This is a very simple mock. For a real unit test I might want the mock
to return `t` for some filename patterns and `nil` for others. There's
an extension to ERT, `el-mock.el`, which assists in creating more
complex mocks, but I haven't used or needed it yet.

Since it's so convenient I'm going to be using ERT more and more until
it becomes second-nature.
