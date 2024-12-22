---
title: Rules to avoid common extended inline assembly mistakes
layout: post
date: 2024-12-20T19:46:48Z
tags: [c, cpp]
uuid: 594e546f-15c7-4834-bece-9c9f24122a01
---

GCC and Clang inline assembly is an interface between high and low level
programming languages. It is subtle and treacherous. Many are ensnared in
its traps, usually unknowingly. As such, the `asm` keyword is essentially
the `unsafe` keyword of C and C++. Nearly every inline assembly tutorial,
including [the awful ibilio page][ibiblio] at the top of search engines for
decades, propagate fundamental, serious mistakes, and *most examples are
incorrect*. The dangerous part is that the examples *usually* produce the
expected results! The situation is dire. This article isn't a tutorial,
but basic rules to avoid the most common mistakes, or to spot them in code
review.

**The focus is entirely *extended assembly*, and not *basic assembly***,
which has different rules. The former is any inline assembly statement
with constraints or clobbers. That is, there's a colon `:` token between
the `asm` parenthesis. Basic assembly is blunt and has fewer uses, mostly
at the top level or in ["naked" functions][naked], making misuse less
likely.

### (1) Avoid inline assembly if possible

Because it's so treacherous, the first rule is to avoid it if at all
possible. Modern compilers are loaded with intrinsics and built-ins that
replace nearly all the old inline assembly use cases. They allow access to
low level features from the high level language. No need to bridge the gap
between low and high yourself when there's an intrinsic.

Compilers do not have built-ins for system calls, and occasionally [lack a
useful intrinsic][int3]. Other times you might be building [foundational
infrastructure][gc]. These remaining cases are mostly about interacting
with external interfaces, not optimization nor performance.

### (2) It should nearly always be volatile

Falling right out of rule (1), the remaining inline assembly cases nearly
always have side effects beyond output constraints. That includes memory
accesses, and it certainly includes system calls. Because of this, inline
assembly should usually have the `volatile` qualifier.

```c
asm volatile ( ... );
```

This prevents compilers from eliding or re-ordering the assembly. As a
special rule, inline assembly lacking output constraints is implicitly
volatile. Despite this, *please use `volatile` anyway!* When I do not see
`volatile` it's likely a defect. Stopping to consider if it's this special
case slows understanding and impedes code review.

Tutorials often use `__volatile__`. Do not do this. It is an ancient alias
keyword to support pre-standard compilers lacking the `volatile` keyword.
This is not your situation. When I see `__volatile__` it likely means you
copy-pasted the inline assembly from somewhere without understanding it.
It's a red flag that draws my attention for even more careful review.

Side note: `__asm` or `__asm__` is fine, and even required in some cases
(e.g. `-std=cXX`). I usually write it `asm`.

### (3) It probably needs a memory clobber

The `"memory"` clobber is orthogonal to `volatile`, each serving different
purposes. It's less often needed than `volatile`, but typical remaining
inline assembly cases require it. If memory is accessed in any way while
executing the assembly, you need a memory clobber. This includes most
system calls, and definitely a generic `syscall` wrapper.

```c
    asm volatile (... : "memory");
```

In code review, if you do not see a `"memory"` clobber, give it extra
scrutiny. It's probably missing. If it's truly unnecessary, I suggest
documenting such in a comment so that reviewers know the omission is
considered and intentional.

The constraint prevents compilers from re-ordering loads and stores around
the assembly. It would be disastrous, for example, if a `write(2)` system
call occurred before the program populated the output buffer! In this
case, `volatile` would prevent followup `write(2)` from being optimized
out while `"memory"` forces memory stores to occur before the system call.

### (4) Never modify input constraints

It's easy not to modify inputs, so this is mostly about ignorance, but
this rule is broken with shocking frequency. Most of the time you can get
away with it, right up until certain configurations have a heisenbug. In
most cases this can be fixed by changing an input into read-write output
constraint with `"+"`:

```c
asm volatile ("..." :: "r"(x) : ...);  // before
asm volatile ("..." : "+r"(x) : ...);  // after
```

If you hadn't been using `volatile` (in violation of rule 2) then now
suddenly you'd need it because there's an output constraint. This happens
often.

### (5) Never call functions from inline assembly

Many things can go wrong because the semantics cannot be expressed using
inline assembly constraints. The stack may not be aligned, and you'll
clobber the redzone. (Yes, there's a `"redzone"` constraint, but its
insufficient to actually make a function call.) Do not do it. Tutorials
like to show it because it makes for a simple demonstration, but all those
examples are littered with defects.

System calls are fine. Basic assembly may call functions when used outside
of non-naked functions. The `goto` qualifier, used correctly, allows jumps
to be safely expressed to the compiler. Just don't use `call` in extended
assembly.

### (6) Do not define absolute assembly labels

That is, if you need to jump within your assembly block, such as for a
loop, do not write a named label:

    myloop:
        ...
        jz myloop

Your inline assembly is part of a function, and that function may be
cloned or inlined, in which case there will be *multiple copies of your
assembly block* in the translation unit. The assembler will see duplicate
label names and reject the program. Until that function is inlined,
perhaps at a high optimization level, this will likely work as expected.
On the plus side it's a loud compile time error when it doesn't work.

In inline assembly you can have the compiler generate a unique label with
`%=`, but my preferred solution is the [local labels][ll] feature of the
assembler:

    0:
        ...
        jz 0b

In this case the assembler generates unique labels, and the number `0`
isn't the literal label name. `0b` ("backward") refers to the previous `0`
label, and `0f` ("forward") would refer to the next `0` label. Perfectly
unambiguous.

### Naturally occurring practice problems

Now that you've made it this far, here's an exercise for practice: Search
online for "inline assembly tutorial" and count the defects you find by
applying my 6 rules. You'll likely find at least one per result that isn't
[official compiler documentation][docs]. Besides tutorials and reviewing
real programs, you could [ask an LLM to generate inline assembly][llm], as
they've been been trained to produce these common defects.


[docs]: https://gcc.gnu.org/onlinedocs/gcc/Extended-Asm.html
[gc]: https://github.com/skeeto/scratch/blob/fbd3260e/misc/buddy.c#L594-#L616
[ibiblio]: https://web.archive.org/web/20241216071150/https://www.ibiblio.org/gferg/ldp/GCC-Inline-Assembly-HOWTO.html
[int3]: /blog/2024/01/28/
[ll]: https://sourceware.org/binutils/docs/as/Symbol-Names.html
[llm]: /blog/2024/11/10/
[naked]: /blog/2023/03/23/
