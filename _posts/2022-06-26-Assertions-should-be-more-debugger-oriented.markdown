---
title: Assertions should be more debugger-oriented
layout: post
date: 2022-06-26T18:51:04Z
tags: [c, cpp, go, python, java]
uuid: 22ae914c-971b-4cee-ba48-a189db1b6df6
---

Prompted by [a 20 minute video][rbg], over the past month I've improved my
debugger skills. I'd shamefully acquired a bad habit: avoiding a debugger
until exhausting dumber, insufficient methods. My *first* choice should be
a debugger, but I had allowed a bit of friction to dissuade me. With some
thoughtful practice and deliberate effort clearing the path, my bad habit
is finally broken — at least when a good debugger is available. It feels
like I've leveled up and, [like touch typing][tt], this was a skill I'd
neglected far too long. One friction point was the less-than-optimal
`assert` feature in basically every programming language implementation.
It ought to work better with debuggers.

An assertion verifies a program invariant, and so if one fails then
there's undoubtedly a defect in the program. In other words, assertions
make programs more sensitive to defects, allowing problems to be caught
more quickly and accurately. Counter-intuitively, crashing early and often
makes for more robust and reliable software in the long run. For exactly
this reason, assertions go especially well with [fuzzing][fuzz].

```c
assert(i >= 0 && i < len);   // bounds check
assert((ssize_t)size >= 0);  // suspicious size_t
assert(cur->next != cur);    // circular reference?
```

They're sometimes abused for error handling, which is a reason they've
also been (wrongfully) discouraged at times. For example, failing to open
a file is an error, not a defect, so an assertion is inappropriate.

Normal programs have implicit assertions all over, even if we don't
usually think of them as assertions. In some cases they're checked by the
hardware. Examples of implicit assertion failures:

* Out-of-bounds indexing
* Dereferencing null/nil/None
* Dividing by zero
* Certain kinds of integer overflow (e.g. `-ftrapv`)

Programs are generally not intended to recover from these situations
because, had they been anticipated, the invalid operation wouldn't have
been attempted in the first place. The program simply crashes because
there's no better alternative. Sanitizers, including Address Sanitizer
(ASan) and Undefined Behavior Sanitizer (UBSan), are in essence
additional, implicit assertions, checking invariants that aren't normally
checked.

Ideally a failing assertion should have these two effects:

* Execution should *immediately* stop. The program is in an unknown state,
  so it's neither safe to "clean up" nor attempt to recover. Additional
  execution will only make debugging more difficult, and may obscure the
  defect.

* When run under a debugger — or visited as a core dump — it should break
  exactly at the failed assertion, ready for inspection. I should not need
  to dig around the call stack to figure out where the failure occurred. I
  certainly shouldn't need to manually set a breakpoint and restart the
  program hoping to fail the assertion a second time. The whole reason for
  using a debugger is to save time, so if it's wasting my time then it's
  failing at its primary job.

I examined standard `assert` features across various language
implementations, and none strictly meet the criteria. Fortunately, in some
cases, it's trivial to build a better assertion, and you can substitute
your own definition. First, let's discuss the way assertions disappoint.

### A test assertion

My test for C and C++ is minimal but establishes some state and gives me a
variable to inspect:

```c
#include <assert.h>

int main(void)
{
    for (int i = 0; i < 10; i++) {
        assert(i < 5);
    }
}
```

Then I compile and debug in the most straightforward way:

    $ cc -g -o test test.c
    $ gdb test
    (gdb) r
    (gdb) bt

The `r` in GDB stands for `run`, which immediately breaks because of the
`assert`. The `bt` prints a backtrace. On a typical Linux distribution
that shows this backtrace:

    #0  __GI_raise
    #1  __GI_abort
    #2  __assert_fail_base
    #3  __GI___assert_fail
    #4  main

Well, actually, it's much messier than this, but I manually cleaned it up:

    #0  __GI_raise (sig=sig@entry=6) at ../sysdeps/unix/sysv/linu
    x/raise.c:50
    #1  0x00007ffff7df4537 in __GI_abort () at abort.c:79
    #2  0x00007ffff7df440f in __assert_fail_base (fmt=0x7ffff7f5d
    128 "%s%s%s:%u: %s%sAssertion `%s' failed.\n%n", assertion=0x
    55555555600b "i < 5", file=0x555555556004 "test.c", line=6, f
    unction=<optimized out>) at assert.c:92
    #3  0x00007ffff7e03662 in __GI___assert_fail (assertion=0x555
    55555600b "i < 5", file=0x555555556004 "test.c", line=6, func
    tion=0x555555556011 <__PRETTY_FUNCTION__.0> "main") at assert
    .c:101
    #4  0x0000555555555178 in main () at test.c:6

That's a lot to take in at a glance, and about 95% of it is noise that
will never contain useful information. Most notably, GDB didn't stop at
the failing assertion. Instead there's *four stack frames* of libc junk I
have to navigate before I can even begin debugging.

    (gdb) up
    (gdb) up
    (gdb) up
    (gdb) up

I must wade through this for every assertion failure. This is some of the
friction that made me avoid the debugger in the first place. glibc loves
indirection, so maybe the other libc implementations do better? How about
musl?

    #0  setjmp
    #1  raise
    #2  ??
    #3  ??
    #4  ??
    #5  ??
    #6  ??
    #7  ??
    #8  ??
    #9  ??
    #10 ??
    #11 ??

Oops, without musl debugging symbols I can't debug assertions at all
because GDB can't read the stack, so it's lost. If you're on Alpine you
can install `musl-dbg`, but otherwise you'll probably need to build your
own from source. With debugging symbols, musl is no better than glibc:

    #0  __restore_sigs
    #1  raise
    #2  abort
    #3  __assert_fail
    #4  main

Same with FreeBSD:

    #0  thr_kill
    #1  in raise
    #2  in abort
    #3  __assert
    #4  main

OpenBSD has one fewer frame:

    #0  thrkill
    #1  _libc_abort
    #2  _libc___assert2
    #3  main

How about on Windows with Mingw-w64?

    [Inferior 1 (process 7864) exited with code 03]

Oops, on Windows GDB doesn't break at all on `assert`. You must first set
a breakpoint on `abort`:

    (gdb) b abort

Besides that, it's the most straightforward so far:

    #0 msvcrt!abort
    #1 msvcrt!_assert
    #2 main

With MSVC (default CRT) I get something slightly different:

    #0 abort
    #1 common_assert_to_stderr
    #2 _wassert
    #3 main
    #4 __scrt_common_main_seh

RemedyBG leaves me at the `abort` like GDB does elsewhere. Visual Studio
recognizes that I don't care about its stack frames and instead puts the
focus on the assertion, ready for debugging. The other stack frames are
there, but basically invisible. It's the only case that practically meets
all my criteria!

I can't entirely blame these implementations. The C standard requires that
`assert` print a diagnostic and call `abort`, and that `abort` raises
`SIGABRT`. There's not much implementations can do, and it's up to the
debugger to be smarter about it.

### Sanitizers

ASan doesn't break GDB on assertion failures, which is yet another source
of friction. You can work around this with an environment variable:

    export ASAN_OPTIONS=abort_on_error=1:print_legend=0

This works, but it's the worst case of all: I get 7 junk stack frames on
top of the failed assertion. It's also very noisy when it traps, so the
`print_legend=0` helps to cut it down a bit. I want this variable so often
that I set it in my shell's `.profile` so that it's always set.

With UBSan you can use `-fsanitize-undefined-trap-on-error`, which behaves
like the improved assertion. It traps directly on the defect with no junk
frames, though it prints no diagnostic. As a bonus, it also means you
don't need to link `libubsan`. Thanks to the bonus, it fully supplants
`-ftrapv` for me on all platforms.

**Update November 2022**: This "stop" hook eliminates ASan friction by
popping runtime frames — functions with the reserved `__` prefix — from
the call stack so that they're not in the way when GDB takes control. It
requires Python support, which is the purpose of the feature-sniff outer
condition.

    if !$_isvoid($_any_caller_matches)
        define hook-stop
            while $_thread && $_any_caller_matches("^__")
                up-silently
            end
        end
    end

This is now part of my `.gdbinit`.

### A better assertion

At least when under a debugger, here's a much better assertion macro for
GCC and Clang:

```c
#define assert(c) if (!(c)) __builtin_trap()
```

`__builtin_trap` inserts a trap instruction — a built-in breakpoint. By
not calling a function to raise a signal, there are no junk stack frames
and no need to breakpoint on `abort`. It stops exactly where it should as
quickly as possible. This definition works reliably with GCC across all
platforms, too. On MSVC the equivalent is `__debugbreak`. If you're really
in a pinch then do whatever it takes to trigger a fault, like
dereferencing a null pointer. A more complete definition might be:

```c
#ifdef DEBUG
#  if __GNUC__
#    define assert(c) if (!(c)) __builtin_trap()
#  elif _MSC_VER
#    define assert(c) if (!(c)) __debugbreak()
#  else
#    define assert(c) if (!(c)) *(volatile int *)0 = 0
#  endif
#else
#  define assert(c)
#endif
```

None of these print a diagnostic, but that's unnecessary when a debugger
is involved.

### Other languages

Unfortunately the situation [mostly gets worse][rs] with other language
implementations, and it's generally not possible to build a better
assertion. Assertions typically have exception-like semantics, if not
literally just another exception, and so they are far less reliable. If a
failed assertion raises an exception, then the program won't stop until
it's unwound the stack — running destructors and such along the way — all
the way to the top level looking for a handler. It only knows there's a
problem when nobody was there to catch it.

[Go officially doesn't have assertions][go], though panics are a kind of
assertion. However, panics have exception-like semantics, and so suffer
the problems of exceptions. A Go version of my test:

```go
func main() {
    defer fmt.Println("DEFER")
    for i := 0; i < 10; i++ {
        if i >= 5 {
            panic(i)
        }
    }
}
```

If I run this under Go's premier debugger, [Delve][dlv], the unrecovered
panic causes it to break. So far so good. However, I get two junk frames:

    #0 runtime.fatalpanic
    #1 runtime.gopanic
    #2 main.main
    #3 runtime.main
    #4 runtime.goexit

It only knows to stop because the Go runtime called `fatalpanic`, but the
backtrace is a fiction: The program continued to run after the panic,
enough to run all the registered defers (including printing "DEFER"),
unwinding the stack to the top level, and only then did it `fatalpanic`.
Fortunately it's still possible to inspect all those stack frames even if
some variables may have changed while unwinding, but it's more like
inspecting a core dump than a paused process.

The situation in Python is similar: `assert` raises AssertionError — a
plain old exception — and `pdb` won't break until the stack has unwound,
exiting context managers and such. Only once the exception reaches the top
level does it enter "post mortem debugging," like a core dump. At least
there are no junk stack frames on top. If you're using asyncio then your
program may continue running for quite awhile before the right tasks are
scheduled and the exception finally propagates to the top level, if ever.

The worst offender of all is Java. First `jdb` never breaks for unhandled
exceptions. It's up to you to set a breakpoint before the exception is
thrown. But it gets worse: assertions are disabled under `jdb`. The Java
`assert` statement is worse than useless.

### Addendum: Don't exit the debugger

The largest friction-reducing change I made is never exiting the debugger.
Previously I would enter GDB, run my program, exit, edit/rebuild, repeat.
However, there's no reason to exit GDB! It automatically and reliably
reloads symbols and updates breakpoints on symbols. It remembers your run
configuration, so re-running is just `r` rather than interacting with
shell history.

My workflow on all platforms ([including Windows][w64]) is a vertically
maximized Vim window and a vertically maximized terminal window. The new
part for me: The terminal runs a long-term GDB session exclusively, with
`file` set to the program I'm writing, usually set by initial the command
line.

    $ gdb myprogram
    gdb>

Alternatively use `file` after starting GDB. Occasionally useful if my
project has multiple binaries, and I want to examine a different program.

    gdb> file myprogram

I use `make` and Vim's `:mak` command for building from within the editor,
so I don't need to change context to build. The quickfix list takes me
straight to warnings/errors. Often I'm writing something that takes input
from standard input. So I use the `run` (`r`) command to set this up
(along with any command line arguments).

    gdb> r <test.txt

You can redirect standard output as well. It remembers these settings for
plain `run` later, so I can test my program by entering `r` and nothing
else.

    gdb> r

My usual workflow is edit, `:mak`, `r`, repeat. If I want to test a
different input or use different options, change the run configuration
using `run` again:

    gdb> r -a -b -c <test2.txt

On Windows you cannot recompile while the program is running. If GDB is
sitting on a breakpoint but I want to build, use `kill` (`k`) to stop it
without exiting GDB.

    gdb> k

GDB has an annoying, flow-breaking yes/no prompt for this, so I recommend
`set confirm no` in your `.gdbinit` to disable it.

Sometimes a program is stuck in a loop and I need it to break in the
debugger. I try to avoid CTRL-C in the terminal it since it can confuse
GDB. A safer option is to signal the process from Vim with `pkill`, which
GDB will catch (except on Windows):

    :!pkill myprogram

I suspect many people don't know this, but if you're on Windows and
[developing a graphical application][ast], you can [press F12][f12] in the
debuggee's window to immediately break the program in the attached
debugger. This is a general platform feature and works with any native
debugger. I've been using it quite a lot.

On that note, you can run commands from GDB with `!`, which is another way
to avoid having an extra terminal window around:

    gdb> !git diff

In any case, GDB will re-read the binary on the next `run` and update
breakpoints, so it's mostly seamless. If there's a function I want to
debug, I set a breakpoint on it, then run.

    gdb> b somefunc
    gdb> r

Alternatively I'll use a line number, which I read from Vim. Though GDB,
not being involved in the editing process, cannot track how that line
moves between builds.

An empty command repeats the last command, so once I'm at a breakpoint,
I'll type `next` (`n`) — or `step` (`s`) to enter function calls — then
press enter each time I want to advance a line, often with my eye on the
context in Vim in the other window:

    gdb> n
    gdb>
    gdb>

(~~I wish GDB could print a source listing around the breakpoint as
context, like Delve, but no such feature exists. The woeful `list` command
is inadequate.~~ **Update**: GDB's TUI is a reasonable compromise for GUI
applications or terminal applications running under a separate tty/console
with either `tty` or `set new-console`. I can access it everywhere since
w64devkit now supports GDB TUI.)

If I want to advance to the next breakpoint, I use `continue` (`c`):

    gdb> c

If I'm walking through a loop, I want to see how variables change, but
it's tedious to keep `print`ing (`p`) the same variables again and again.
So I use `display` (`disp`) to display an expression with each prompt,
much like the "watch" window in Visual Studio. For example, if my loop
variable is `i` over some string `str`, this will show me the current
character in character format (`/c`).

    gdb> disp/c str[i]

You can accumulate multiple expressions. Use `undisplay` to remove them.

Too many breakpoints? Use `info breakpoints` (`i b`) to list them, then
`delete` (`d`) the unwanted ones by ID.

    gdb> i b
    gdb> d 3 5 8

GDB has many more feature than this, but 10 commands cover 99% of use
cases: `r`, `c`, `n`, `s`, `disp`, `k`, `b`, `i`, `d`, `p`.


[ast]: /blog/2021/03/11/
[dlv]: https://github.com/go-delve/delve
[f12]: https://docs.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-registerhotkey
[fuzz]: /blog/2019/01/25/
[go]: https://go.dev/doc/faq#assertions
[rbg]: https://www.youtube.com/watch?v=r9eQth4Q5jg
[rs]: https://github.com/rust-lang/rust/issues/21102
[tt]: /blog/2017/04/01/
[w64]: /blog/2020/05/15/
