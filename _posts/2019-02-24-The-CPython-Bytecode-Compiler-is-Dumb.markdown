---
title: The CPython Bytecode Compiler is Dumb
layout: post
date: 2019-02-24T21:56:35Z
tags: [python, lua, lang, elisp, optimization]
uuid: 4348d611-858b-4f48-a6f5-6e4b93f71a34
excerpt_separator: <!--more-->
---

*This article was [discussed on Hacker News][hn].*

Due to sheer coincidence of several unrelated tasks converging on
Python at work, I recently needed to brush up on my Python skills. So
far for me, Python has been little more than [a fancy extension
language for BeautifulSoup][bs4], though I also used it to participate
in the recent tradition of [writing one's own static site
generator][qualbum], in this case for [my wife's photo blog][photo].
I've been reading through *Fluent Python* by Luciano Ramalho, and it's
been quite effective at getting me up to speed.

<!--more-->

As I write Python, [like with Emacs Lisp][emacs], I can't help but
consider what exactly is happening inside the interpreter. I wonder if
the code I'm writing is putting undue constraints on the bytecode
compiler and limiting its options. Ultimately I'd like the code I
write [to drive the interpreter efficiently and effectively][fast].
[The Zen of Python][zen] says there should "only one obvious way to do
it," but in practice there's a lot of room for expression. Given
multiple ways to express the same algorithm or idea, I tend to prefer
the one that compiles to the more efficient bytecode.

Fortunately CPython, the main and most widely used implementation of
Python, is very transparent about its bytecode. It's easy to inspect
and reason about its bytecode. The disassembly listing is easy to read
and understand, and I can always follow it without consulting the
documentation. This contrasts sharply with modern JavaScript engines
and their opaque use of JIT compilation, where performance is guided
by obeying certain patterns ([hidden classes][hc], etc.), helping the
compiler [understand my program's types][fround], and being careful
not to unnecessarily constrain the compiler.

So, besides just catching up with Python the language, I've been
studying the bytecode disassembly of the functions that I write. One
fact has become quite apparent: **the CPython bytecode compiler is
pretty dumb**. With a few exceptions, it's a very literal translation
of a Python program, and there is almost [no optimization][peep].
Below I'll demonstrate a case where it's possible to detect one of the
missed optimizations without inspecting the bytecode disassembly
thanks to a small abstraction leak in the optimizer.

To be clear: This isn't to say CPython is bad, or even that it should
necessarily change. In fact, as I'll show, **dumb bytecode compilers
are par for the course**. In the past I've lamented how the Emacs Lisp
compiler could do a better job, but CPython and Lua are operating at
the same level. There are benefits to a dumb and straightforward
bytecode compiler: the compiler itself is simpler, easier to maintain,
and more amenable to modification (e.g. as Python continues to
evolve). It's also easier to debug Python (`pdb`) because it's such a
close match to the source listing.

*Update*: [Darius Bacon points out][dragon] that Guido van Rossum
himself said, "[Python is about having the simplest, dumbest compiler
imaginable.][quote]" So this is all very much by design.

The consensus seems to be that if you want or need better performance,
use something other than Python. (And if you can't do that, at least use
[PyPy][pypy].) That's a fairly reasonable and healthy goal. Still, if
I'm writing Python, I'd like to do the best I can, which means
exploiting the optimizations that *are* available when possible.

### Disassembly examples

I'm going to compare three bytecode compilers in this article: CPython
3.7, Lua 5.3, and Emacs 26.1. Each of these languages are dynamically
typed, are primarily executed on a bytecode virtual machine, and it's
easy to access their disassembly listings. One caveat: CPython and Emacs
use a stack-based virtual machine while Lua uses a register-based
virtual machine.

For CPython I'll be using the `dis` module. For Emacs Lisp I'll use `M-x
disassemble`, and all code will use lexical scoping. In Lua I'll use
`lua -l` on the command line.

### Local variable elimination

Will the bytecode compiler eliminate local variables? Keeping the
variable around potentially involves allocating memory for it, assigning
to it, and accessing it. Take this example:

```py
def foo():
    x = 0
    y = 1
    return x
```

This function is equivalent to:

```py
def foo():
    return 0
```

Despite this, CPython completely misses this optimization for both `x`
and `y`:

```
  2           0 LOAD_CONST               1 (0)
              2 STORE_FAST               0 (x)
  3           4 LOAD_CONST               2 (1)
              6 STORE_FAST               1 (y)
  4           8 LOAD_FAST                0 (x)
             10 RETURN_VALUE
```

It assigns both variables, and even loads again from `x` for the return.
Missed optimizations, but, as I said, by keeping these variables around,
debugging is more straightforward. Users can always inspect variables.

How about Lua?

```lua
function foo()
    local x = 0
    local y = 1
    return x
end
```

It also misses this optimization, though it matters a little less due to
its architecture (the return instruction references a register
regardless of whether or not that register is allocated to a local
variable):

```
        1       [2]     LOADK           0 -1    ; 0
        2       [3]     LOADK           1 -2    ; 1
        3       [4]     RETURN          0 2
        4       [5]     RETURN          0 1
```

Emacs Lisp also misses it:

```cl
(defun foo ()
  (let ((x 0)
        (y 1))
    x))
```

Disassembly:

```
0	constant  0
1	constant  1
2	stack-ref 1
3	return
```

All three are on the same page.

### Constant folding

Does the bytecode compiler evaluate simple constant expressions at
compile time? This is simple and everyone does it.

```py
def foo():
    return 1 + 2 * 3 / 4
```

Disassembly:

```
  2           0 LOAD_CONST               1 (2.5)
              2 RETURN_VALUE
```

Lua:

```lua
function foo()
    return 1 + 2 * 3 / 4
end
```

Disassembly:

```
        1       [2]     LOADK           0 -1    ; 2.5
        2       [2]     RETURN          0 2
        3       [3]     RETURN          0 1
```

Emacs Lisp:

```cl
(defun foo ()
  (+ 1 (/ (* 2 3) 4.0))
```

Disassembly:

```
0	constant  2.5
1	return
```

That's something we can count on so long as the operands are all
numeric literals (or also, for Python, string literals) that are
visible to the compiler. Don't count on your operator overloads to
work here, though.

### Allocation optimization

Optimizers often perform *escape analysis*, to determine if objects
allocated in a function ever become visible outside of that function. If
they don't then these objects could potentially be stack-allocated
(instead of heap-allocated) or even be eliminated entirely.

None of the bytecode compilers are this sophisticated. However CPython
does have a trick up its sleeve: tuple optimization. Since tuples are
immutable, in certain circumstances CPython will reuse them and avoid
both the constructor and the allocation.

```py
def foo():
    return (1, 2, 3)
```

Check it out, the tuple is used as a constant:

```
  2           0 LOAD_CONST               1 ((1, 2, 3))
              2 RETURN_VALUE
```

Which we can detect by evaluating `foo() is foo()`, which is `True`.
Though deviate from this too much and the optimization is disabled.
Remember how CPython can't optimize away variables, and that they
break constant folding? The break this, too:

```py
def foo():
    x = 1
    return (x, 2, 3)
```

Disassembly:

```
  2           0 LOAD_CONST               1 (1)
              2 STORE_FAST               0 (x)
  3           4 LOAD_FAST                0 (x)
              6 LOAD_CONST               2 (2)
              8 LOAD_CONST               3 (3)
             10 BUILD_TUPLE              3
             12 RETURN_VALUE
```

This function might document that it always returns a simple tuple,
but we can tell if its being optimized or not using `is` like before:
`foo() is foo()` is now `False`! In some future version of Python with
a cleverer bytecode compiler, that expression might evaluate to
`True`. (Unless the [Python language specification][spec] is specific
about this case, which I didn't check.)

Note: Curiously PyPy replicates this exact behavior when examined with
`is`. Was that deliberate? I'm impressed that PyPy matches CPython's
semantics so closely here.

Putting a mutable value, such as a list, in the tuple will also break
this optimization. But that's not the compiler being dumb. That's a
hard constraint on the compiler: the caller might change the mutable
component of the tuple, so it must always return a fresh copy.

Neither Lua nor Emacs Lisp have a language-level concept equivalent of
an immutable tuple, so there's nothing to compare.

Other than the tuples situation in CPython, none of the bytecode
compilers eliminate unnecessary intermediate objects.

```py
def foo():
    return [1024][0]
```

Disassembly:

```
  2           0 LOAD_CONST               1 (1024)
              2 BUILD_LIST               1
              4 LOAD_CONST               2 (0)
              6 BINARY_SUBSCR
              8 RETURN_VALUE
```

Lua:

```lua
function foo()
    return ({1024})[1]
end
```

Disassembly:

```
        1       [2]     NEWTABLE        0 1 0
        2       [2]     LOADK           1 -1    ; 1024
        3       [2]     SETLIST         0 1 1   ; 1
        4       [2]     GETTABLE        0 0 -2  ; 1
        5       [2]     RETURN          0 2
        6       [3]     RETURN          0 1
```

Emacs Lisp:

```cl
(defun foo ()
  (car (list 1024)))
```

Disassembly:

```
0	constant  1024
1	list1
2	car
3	return
```

### Don't expect too much

I could go on with lots of examples, looking at loop optimizations and
so on, and each case is almost certainly unoptimized. The general rule
of thumb is to simply not expect much from these bytecode compilers.
They're very literal in their translation.

Working so much in C has put me in the habit of expecting all obvious
optimizations from the compiler. This frees me to be more expressive
in my code. Lots of things are cost-free thanks to these
optimizations, such as breaking a complex expression up into several
variables, naming my constants, or not using a local variable to
manually cache memory accesses. I'm confident the compiler will
optimize away my expressiveness. The catch is that [clever compilers
can take things too far][c], so I've got to be mindful of how it might
undermine my intentions — i.e. when I'm doing something unusual or not
strictly permitted.

These bytecode compilers will never truly surprise me. The cost is
that being more expressive in Python, Lua, or Emacs Lisp may reduce
performance at run time because it shows in the bytecode. Usually this
doesn't matter, but sometimes it does.


[bs4]: /blog/2017/05/15/
[c]: /blog/2018/05/01/
[dragon]: https://codewords.recurse.com/issues/seven/dragon-taming-with-tailbiter-a-bytecode-compiler
[emacs]: /blog/2014/01/04/
[fast]: /blog/2017/01/30/
[fround]: https://blog.mozilla.org/javascript/2013/11/07/efficient-float32-arithmetic-in-javascript/
[hc]: https://www.youtube.com/watch?v=UJPdhx5zTaw
[hn]: https://news.ycombinator.com/item?id=19241545
[peep]: https://legacy.python.org/workshops/1998-11/proceedings/papers/montanaro/montanaro.html
[photo]: http://photo.nullprogram.com/
[pypy]: https://pypy.org/
[quote]: https://books.google.com/books?id=bIxWAgAAQBAJ&pg=PA26&lpg=PA26&dq=%22Python+is+about+having+the+simplest,+dumbest+compiler+imaginable.%22&source=bl&ots=2OfDoWX321&sig=ACfU3U32jKZBE3VkJ0gvkKbxRRgD0bnoRg&hl=en&sa=X&ved=2ahUKEwjZ1quO89bgAhWpm-AKHfckAxUQ6AEwAHoECAkQAQ#v=onepage&q=%22Python%20is%20about%20having%20the%20simplest%2C%20dumbest%20compiler%20imaginable.%22&f=false
[qualbum]: https://github.com/skeeto/qualbum
[spec]: https://docs.python.org/3/reference/
[zen]: https://www.python.org/dev/peps/pep-0020/
