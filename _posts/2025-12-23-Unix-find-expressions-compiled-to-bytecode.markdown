---
title: 'Unix "find" expressions compiled to bytecode'
layout: post
date: 2025-12-23T04:20:22Z
tags: [c, compsci]
uuid: bbe2671b-378d-40b1-9564-c3a3b798dfb4
---

In preparation for a future project, I was thinking about at the [unix
`find` utility][find]. It operates a file system hierarchies, with basic
operations selected and filtered using a specialized expression language.
Users compose operations using unary and binary operators, grouping with
parentheses for precedence. `find` may apply the expression to a great
many files, so compiling it into a bytecode, resolving as much as possible
ahead of time, and minimizing the per-element work, seems like a prudent
implementation strategy. With some thought, I worked out a technique to do
so, which was simpler than I expected, and I'm pleased with the results. I
was later surprised all the real world `find` implementations I examined
use [tree-walk interpreters][tw] instead. This article describes how my
compiler works, with a runnable example, and lists ideas for improvements.

For a quick overview, the syntax looks like this:

    $ find [-H|-L] path... [expression...]

Technically at least one path is required, but most implementations imply
`.` when none are provided. If no expression is supplied, the default is
`-print`, e.g. print everything under each listed path. This prints the
whole tree, including directories, under the current directory:

    $ find .

To only print files, we could use `-type f`:

    $ find . -type f -a -print

Where `-a` is the logical AND binary operator. `-print` always evaluates
to true. It's never necessary to write `-a`, and adjacent operations are
implicitly joined with `-a`. We can keep chaining them, such as finding
all executable files:

    $ find . -type f -executable -print

If no `-exec`, `-ok`, or `-print` (or similar side-effect extensions like
`-print0` or `-delete`) are present, the whole expression is wrapped in an
implicit `( expr ) -print`. So we could also write this:

    $ find . -type f -executable

Use `-o` for logical OR. To print all files with the executable bit *or*
with a `.exe` extension:

    $ find . -type f \( -executable -o -name '*.exe' \)

I needed parentheses because `-o` has lower precedence than `-a`, and
because parentheses are shell metacharacters I also needed to escape them
for the shell. It's a shame `find` didn't use `[` and `]` instead! There's
also a unary logical NOT operator, `!`. To print all non-executable files:

    $ find . -type f ! -executable

Binary operators are short-circuiting, so this:

    $ find -type d -a -exec du -sh {} +

Only lists the sizes of directories, as the `-type d` fails causing the
whole expression to evaluate to false without evaluating `-exec`. Or
equivalently with `-o`:

    $ find ! -type d -o -exec du -sh {} +

If it's not a directory then the left-hand side evaluates to true, and the
right-hand side is not evaluated. All three implementations I examined
(GNU, BSD, BusyBox) have a `-regex` extension, and eagerly compile the
regular expression even if the operation is never evaluated:

    $ find . -print -o -regex [
    find: bad regex '[': Invalid regular expression

I was surprised by this because it doesn't seem to be in the spirit of the
original utility ("The second expression shall not be evaluated if the
first expression is true."), and I'm used to the idea of short-circuit
validation for the right-hand side of a logical expression. Recompiling
for each evaluation would be unwise, but it could happen lazily such that
an invalid regular expression only causes an error if it's actually used.
No big deal, just a curiosity.

### Bytecode design

A bytecode interpreter needs to track just one result at a time, making it
a single register machine, with a 1-bit register at that. I came up with
these five opcodes:

    halt
    not
    braf   LABEL
    brat   LABEL
    action NAME [ARGS...]

Obviously `halt` stops the program. While I could just let it "run off the
end" it's useful to have an actual instruction so that I can attach a
label and jump to it. The `not` opcode negates the register. `braf` is
"branch if false", jumping (via relative immediate) to the labeled (in
printed form) instruction if the register is false. `brat` is "branch if
true". Together they implement the `-a` and `-o` operators. In practice
there are no loops and jumps are always forward: `find` is [not Turing
complete][tc].

In a real implementation each possible action (`-name`, `-ok`, `-print`,
`-type`, etc.) would get a dedicated opcode. This requires implementing
each operator, at least in part, in order to correctly parse the whole
`find` expression. For now I'm just focused on the bytecode compiler, so
this opcode is a stand-in, and it kind of pretends based on looks. Each
action sets the register, and actions like `-print` always set it to true.
My compiler is [called **`findc` ("find compiler")**][src].

**Update**: Or try [the **online demo**][demo] via Wasm! This version
includes a peephole optimizer I wrote after publishing this article.

I assume readers of this program are familiar with [`push` macro][push]
and [`Slice` macro][slice]. Because of the latter it requires a very
recent C compiler, like GCC 15 (e.g. via [w64devkit][]) or Clang 22. Try
out some `find` commands and see how they appear as bytecode. The simplest
case is also optimal:

    $ findc
    // path: .
            action  -print
            halt

Print the path then halt. Simple. Stepping it up:

    $ findc -type f -executable
    // path: .
            action  -type f
            braf    L1
            action  -executable
    L1:     braf    L2
            action  -print
    L2:     halt

If the path is not a file, it skips over the rest of the program by way of
the second branch instruction. It's correct, but already we can see room
for improvement. This would be better:

            action  -type f
            braf    L1
            action  -executable
            braf    L1
            action  -print
    L1:     halt

More complex still:

    $ findc -type f \( -executable -o -name '*.exe' \)
    // path: .
            action  -type f
            braf    L1
            action  -executable
            brat    L1
            action  -name *.exe
    L1:     braf    L2
            action  -print
    L2:     halt

Inside the parentheses, if `-executable` succeeds, the right-hand side is
skipped. Though the `brat` jumps straight to a `braf`. It would be better
to jump ahead one more instruction:

            action  -type f
            braf    L2
            action  -executable
            brat    L1
            action  -name *.exe
            braf    L2
    L1      action  -print
    L2:     halt

Silly things aren't optimized either:

    $ findc ! ! -executable
    // path: .
            action  -executable
            not
            not
            braf    L1
            action  -print
    L1:     halt

Two `not` in a row cancel out, and so these instructions could be
eliminated. Overall this compiler could benefit from a [peephole
optimizer][peep], scanning over the program repeatedly, making small
improvements until no more can be made:

* Delete `not`-`not`.
* A `brat` to a `braf` re-targets ahead one instruction, and vice versa.
* Jumping onto an identical jump adopts its target for itself.
* A `not`-`braf` might convert to a `brat`, and vice versa.
* Delete side-effect-free instructions before `halt` (e.g. `not`-`halt`).
* Exploit always-true actions, e.g. `-print`-`braf` can drop the branch.

Writing a bunch of peephole pattern matchers sounds kind of fun. Though my
compiler would first need a slightly richer representation in order to
detect and fix up changes to branches. One more for the road:

    $ findc -type f ! \( -executable -o -name '*.exe' \)
    // path: .
            action  -type f
            braf    L1
            action  -executable
            brat    L2
            action  -name *.exe
    L2:     not
    L1:     braf    L3
            action  -print
    L3:     halt

The unoptimal jumps hint at my compiler's structure. If you're feeling up
for a challenge, pause here to consider how you'd build this compiler, and
how it might produce these particular artifacts.

### Parsing and compiling

Before I even considered the shape of the bytecode I knew I needed to
convert `find` infix into a compiler-friendly postfix. That is, this:

    -type f -a ! ( -executable -o -name *.exe )

Becomes:

    -type f -executable -name *.exe -o ! -a

Which, importantly, erases the parentheses. This comes in as an `argv`
array, so it's already tokenized for us by the shell [or runtime][ww]. The
classic [shunting-yard algorithm][sya] solves this problem easily enough.
We have an output queue that goes into the compiler, and a token stack for
tracking `-a`, `-o`, `!`, and `(`. Then we walk `argv` in order:

* Actions go straight into the output queue.

* If we see one of the special stack tokens we push it onto the stack,
  first popping operators with greater precedence into the queue, stopping
  at `(`.

*  If we see `)` we pop the stack into the output queue until we see `(`.

When we're out of tokens, pop the remaining stack into the queue. My
parser synthesizes `-a` where it's implied, so the compiler always sees
logical AND. If the expression contains no `-exec`, `-ok`, or `-print`,
after processing is complete the parser puts `-print` then `-a` into the
queue, which effectively wraps the whole expression in `( expr ) -print`.
By clearing the stack first, the real expression is effectively wrapped in
parentheses, so no parenthesis tokens need to be synthesized.

I've used the shunting-yard algorithm many times before, so this part was
easy. The new part was coming up with an algorithm to convert a series of
postfix tokens into bytecode. My solution is the compiler **maintains a
stack of bytecode fragments**. That is, each stack element is a sequence
of one or more bytecode instructions. Branches use relative addresses, so
they're position-independent, and I can concatenate code fragments without
any branch fix-ups. It takes the following actions from queue tokens:

* For an action token, create an `action` instruction, and push it onto
  the fragment stack as a new fragment.

* For a `!` token, pop the top fragment, append a `not` instruction, and
  push it back onto the stack.

* For a `-a` token, pop the top two fragments, join then with a `braf` in
  the middle which jumps just beyond the second fragment. That is, if the
  first fragment evaluates to false, skip over the second fragment into
  whatever follows.

* For a `-o` token, just like `-a` but use `brat`. If the first fragment
  is true, we skip over the second fragment.

If the expression is valid, at the end of this process the stack contains
exactly one fragment. Append a `halt` instruction to this fragment, and
that's our program! If the final fragment contained a branch just beyond
its end, this `halt` is that branch target. A few peephole optimizations
and could probably be an optimal program for this instruction set.


[demo]: https://nullprogram.com/scratch/findc/
[find]: https://pubs.opengroup.org/onlinepubs/9799919799/utilities/find.html
[peep]: https://en.wikipedia.org/wiki/Peephole_optimization
[push]: /blog/2025/01/19/
[slice]: /blog/2025/06/26/
[src]: https://github.com/skeeto/scratch/blob/c142e729/parsers/findc.c
[sya]: https://en.wikipedia.org/wiki/Shunting_yard_algorithm
[tc]: /blog/2016/04/30/
[tw]: https://craftinginterpreters.com/a-tree-walk-interpreter.html
[w64devkit]: https://github.com/skeeto/w64devkit
[ww]: /blog/2022/02/18/
