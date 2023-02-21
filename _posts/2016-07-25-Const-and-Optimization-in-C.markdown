---
title: Const and Optimization in C
layout: post
date: 2016-07-25T02:06:04Z
tags: [c, x86, optimization]
uuid: f785bc3b-dd3d-3952-2696-91eafe6b019d
---

Today there was a [question on /r/C_Programming][reddit] about the
effect of C's `const` on optimization. Variations of this question
have been asked many times over the past two decades. Personally, I
blame naming of `const`.

Given this program:

~~~c
void foo(const int *);

int
bar(void)
{
    int x = 0;
    int y = 0;
    for (int i = 0; i < 10; i++) {
        foo(&x);
        y += x;  // this load not optimized out
    }
    return y;
}
~~~

The function `foo` takes a pointer to const, which is a promise from
the author of `foo` that it won't modify the value of `x`. Given this
information, it would seem the compiler may assume `x` is always zero,
and therefore `y` is always zero.

However, inspecting the assembly output of several different compilers
shows that `x` is loaded each time around the loop. Here's gcc 4.9.2
at -O3, with annotations, for x86-64,

~~~nasm
bar:
     push   rbp
     push   rbx
     xor    ebp, ebp              ; y = 0
     mov    ebx, 0xa              ; loop variable i
     sub    rsp, 0x18             ; allocate x
     mov    dword [rsp+0xc], 0    ; x = 0

.L0: lea    rdi, [rsp+0xc]        ; compute &x
     call   foo
     add    ebp, dword [rsp+0xc]  ; y += x  (not optmized?)
     sub    ebx, 1
     jne    .L0

     add    rsp, 0x18             ; deallocate x
     mov    eax, ebp              ; return y
     pop    rbx
     pop    rbp
     ret
~~~

The output of clang 3.5 (with -fno-unroll-loops) is the same, except
ebp and ebx are swapped, and the computation of `&x` is hoisted out of
the loop, into `r14`.

Are both compilers failing to take advantage of this useful
information? Wouldn't it be undefined behavior for `foo` to modify
`x`? Surprisingly, the answer is no. *In this situation*, this would
be a perfectly legal definition of `foo`.

~~~c
void
foo(const int *readonly_x)
{
    int *x = (int *)readonly_x;  // cast away const
    (*x)++;
}
~~~

The key thing to remember is that [**`const` doesn't mean
constant**][const]. Chalk it up as a misnomer. It's not an
optimization tool. It's there to inform programmers — not the compiler
— as a tool to catch a certain class of mistakes at compile time. I
like it in APIs because it communicates how a function will use
certain arguments, or how the caller is expected to handle returned
pointers. It's usually not strong enough for the compiler to change
its behavior.

Despite what I just said, occasionally the compiler *can* take
advantage of `const` for optimization. The C99 specification, in
§6.7.3¶5, has one sentence just for this:

> If an attempt is made to modify an object defined with a
> const-qualified type through use of an lvalue with
> non-const-qualified type, the behavior is undefined.

The original `x` wasn't const-qualified, so this rule didn't apply.
And there aren't any rules against casting away `const` to modify an
object that isn't itself `const`. This means the above (mis)behavior
of `foo` isn't undefined behavior *for this call*. Notice how the
undefined-ness of `foo` depends on how it was called.

With one tiny tweak to `bar`, I can make this rule apply, allowing the
optimizer do some work on it.

~~~c
    const int x = 0;
~~~

The compiler may now assume that **`foo` modifying `x` is undefined
behavior, therefore *it never happens***. For better or worse, this is
a major part of how a C optimizer reasons about your programs. The
compiler is free to assume `x` never changes, allowing it to optimize
out both the per-iteration load and `y`.

~~~nasm
bar:
     push   rbx
     mov    ebx, 0xa            ; loop variable i
     sub    rsp, 0x10           ; allocate x
     mov    dword [rsp+0xc], 0  ; x = 0

.L0: lea    rdi, [rsp+0xc]      ; compute &x
     call   foo
     sub    ebx, 1
     jne    .L0

     add    rsp, 0x10           ; deallocate x
     xor    eax, eax            ; return 0
     pop    rbx
     ret
~~~

The load disappears, `y` is gone, and the function always returns
zero.

Curiously, the specification *almost* allows the compiler to go
further. Consider what would happen if `x` were allocated somewhere
off the stack in read-only memory. That transformation would look like
this:

~~~c
static const int __x = 0;

int
bar(void)
{
    for (int i = 0; i < 10; i++)
        foo(&__x);
    return 0;
}
~~~

We would see a few more instructions shaved off ([-fPIC, small code
model][memory]):

~~~nasm
section .rodata
x:   dd     0

section .text
bar:
     push   rbx
     mov    ebx, 0xa        ; loop variable i

.L0: lea    rdi, [rel x]    ; compute &x
     call   foo
     sub    ebx, 1
     jne    .L0

     xor    eax, eax        ; return 0
     pop    rbx
     ret
~~~

Because the address of `x` is taken and "leaked," this last transform
is not permitted. If `bar` is called recursively such that a second
address is taken for `x`, that second pointer would compare equally
(`==`) with the first pointer depsite being semantically distinct
objects, which is forbidden (§6.5.9¶6).

Even with this special `const` rule, stick to using `const` for
yourself and for your fellow human programmers. Let the optimizer
reason for itself about what is constant and what is not.

Travis Downs nicely summed up this article in the comments:

> In general, `const` *declarations* can't help the optimizer, but
> `const` *definitions* can.


[reddit]: https://redd.it/4udqwj
[const]: http://yarchive.net/comp/const.html
[memory]: http://eli.thegreenplace.net/2012/01/03/understanding-the-x64-code-models
[github]: https://github.com/blog/2100-github-pages-now-faster-and-simpler-with-jekyll-3-0
