---
title: Two handy GDB breakpoint tricks
layout: post
date: 2024-01-28T21:56:07Z
tags: [c, cpp]
uuid: e56cce3b-8e70-497b-a13a-e609bacdde88
---

Over the past couple months I've discovered a couple of handy tricks for
working with GDB breakpoints. I figured these out on my own, and I've not
seen either discussed elsewhere, so I really ought to share them.

### Continuable assertions

The `assert` macro in typical C implementations [leaves a lot to be
desired][assert], as does `raise` and `abort`, so I've suggested
alternative definitions that behave better under debuggers:

```c
#define assert(c)  while (!(c)) __builtin_trap()
#define assert(c)  while (!(c)) __builtin_unreachable()
#define assert(c)  while (!(c)) *(volatile int *)0 = 0
```

Each serves a slightly [different purpose][style] but still has the most
important property: Immediately halt the program *directly on the defect*.
None have an occasionally useful secondary property: Optionally allow the
program to continue through the defect. If the program reaches the body of
any of these macros then there is no reliable continuation. Even manually
nudging the instruction pointer over the assertion isn't enough. Compilers
assume that the program cannot continue through the condition and generate
code accordingly.

The MSVC ecosystem has a solution for this on x86: `int3`. The portable
name is `__debugbreak`, a name [I've borrowed elsewhere][db].

```c
#define assert(c)  do if (!(c)) __debugbreak(); while (0)
```

On x86 it inserts an `int3` instruction, which fires an interrupt,
trapping in the attached debugger, or otherwise abnormally terminating the
program. Because it's an interrupt, it's expected that the program might
continue. It even leaves the instruction pointer on the next instruction.
As of this writing, GCC has no matching intrinsic, but Clang recently
added `__builtin_debugtrap`. In GCC you need some less portable inline
assembly: `asm ("int3")`.

However, regardless of how you get an `int3` in your program, GDB does not
currently understand it. The problem is that feature I mentioned: The
instruction pointer does not point at the `int3` but the next instruction.
This confuses GDB, causing it to break in the wrong places, possibly even
in the wrong scope. For example:

```c
for (int i = 0; i < n; i++) {
    // ...
    int3_assert(...);
}
```

With `int3` at the very end of the loop, GDB will break at the *top* of
the next loop iteration, because that's where the instruction pointer
lands by the time GDB is involved. It's a similar story when placed at the
end of a function, leaving GDB to break in the caller. To resolve this, we
need the instruction pointer to still be "inside" the breakpoint after the
interrupt fires. Easy! Add a `nop`:

```c
#define breakpoint()  asm ("int3; nop")
```

This behaves beautifully, eliminating all the problems GDB has with a
plain `int3`. Not only is this a solid basis for a continuable assertion,
it's also useful as a fast conditional breakpoint, where conventional
conditional breakpoints are far too slow.

```c
for (int i = 0; i < 1000000000; i++) {
    if (/* rare condition */) breakpoint();
    // ...
}
```

As far as I know there is no ARM equivalent compatible with GDB (or even
LLDB). The closest instruction, `brk #0x1`, does not behave as needed.

### Named positions

GDB's built-in user interface understands three classes of breakpoint
positions: symbols, context-free line numbers, and absolute addresses.
When you set some breakpoints and (re)start a program under GDB, each kind
of breakpoint is handled differently:

* Resolve each symbol, placing a breakpoint on its run-time address.

* Map each file+lineno tuple to a run-time address, and place a breakpoint
  on that address. If the line does not exist (i.e. the file is shorter),
  skip it.

* Place breakpoints exactly on each absolute address. If it's not a mapped
  address, don't start the program.

The first is the best case because it adapts to program changes. Modify
the code, recompile, and the breakpoint generally remains where you want
it.

The third is the least useful. These breakpoints rarely survive across
rebuilds, and sometimes not even across reruns.

The second is in the middle between useful and useless. If you edit the
source file which has the breakpoint — likely, because you placed the
breakpoint there for a reason — chances are high that the line number is
no longer correct. Instead it drifts, requiring manual replacement. This
is tedious and GDB ought to do better. Think that's unreasonable? The
Visual Studio debugger does exactly that [quite effectively][vs] through
external code edits! GDB front ends tend to handle it better, especially
when they're also the code editor and so directly observe all edits.

As a workaround we can get the first kind by temporarily *naming* a line
number. This requires editing the source, but remember, the very reason we
need it is because the source in question is actively changing. How to
name a line? C and C++ labels give a name to program position:

```c
void example(double *nums, int n, ...)
{
    for (int i = 0; i < n; i++) {
        loop:  // named position at the start of the loop
        // ...
    }
}
```

The name `loop` is local to `example`, but the qualified `example:loop` is
a global name, as suitable as any other symbol. I could, say, reliably
trace the progress of this loop despite changes to its position in the
source.

    (gdb) dprintf example:loop,"nums[%d] = %g\n",i,nums[i]

One downside is dealing with `-Wunused-label` (enabled by `-Wall`), and so
I've considered disabling the warning in [my defaults][fav]. I more often
use an assembly label, usually named `b` for convenience:

```c
    for (int i = 0; i < n; i++) {
        asm ("b:");
        // ...
    }
```

Like `int3`, sometimes it's necessary to give it a `nop` so that GDB has
something on which to break. "Enabling" it at any time is quick:

    (gdb) b b

Because it's not [`.globl`][as], it's a weak symbol, and I can place up to
one per translation unit, all covered by the same GDB breakpoint item
(less useful than it sounds). I haven't actually checked, but I probably
more often use `dprintf` with such named lines than actual breakpoints.

If you have similar tips and tricks of your own, I'd like to learn about
them!


[as]: https://sourceware.org/binutils/docs/as/Global.html
[assert]: /blog/2022/06/26/
[db]: /blog/2022/07/31/
[fav]: /blog/2023/04/29/
[style]: /blog/2023/10/08/#macros
[vs]: https://lists.sr.ht/~skeeto/public-inbox/%3C2d3d7662a361ddd049f7dc65b94cecdd%40disroot.org%3E#%3C20240112210447.mxhvo7bg4mjp4jyz@nullprogram.com%3E
