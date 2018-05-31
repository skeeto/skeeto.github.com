---
title: When FFI Function Calls Beat Native C
layout: post
date: 2018-05-27T20:03:15Z
tags: [c, x86, linux, optimization]
uuid: cb339e3b-382e-3762-4e5c-10cf049f7627
---

*Update: There's a good discussion on [Hacker News][hn].*

Over on GitHub, David Yu has an interesting performance benchmark for
function calls of various Foreign Function Interfaces ([FFI][ffi]):

<https://github.com/dyu/ffi-overhead>

He created a shared object (`.so`) file containing a single, simple C
function. Then for each FFI he wrote a bit of code to call this function
many times, measuring how long it took.

For the C "FFI" he used standard dynamic linking, not `dlopen()`. This
distinction is important, since it really makes a difference in the
benchmark. There's a potential argument about whether or not this is a
fair comparison to an actual FFI, but, regardless, it's still
interesting to measure.

The most surprising result of the benchmark is that
**[LuaJIT's][luajit] FFI is substantially faster than C**. It's about
25% faster than a native C function call to a shared object function.
How could a weakly and dynamically typed scripting language come out
ahead on a benchmark? Is this accurate?

It's actually quite reasonable. The benchmark was run on Linux, so the
performance penalty we're seeing comes the *Procedure Linkage Table*
(PLT). I've put together a really simple experiment to demonstrate the
same effect in plain old C:

<https://github.com/skeeto/dynamic-function-benchmark>

Here are the results on an Intel i7-6700 (Skylake):

    plt: 1.759799 ns/call
    ind: 1.257125 ns/call
    jit: 1.008108 ns/call

These are three different types of function calls:

1. Through the PLT
2. An indirect function call (via `dlsym(3)`)
3. A direct function call (via a JIT-compiled function)

As shown, the last one is the fastest. It's typically not an option
for C programs, but it's natural in the presence of a JIT compiler,
including, apparently, LuaJIT.

In my benchmark, the function being called is named `empty()`:

```c
void empty(void) { }
```

And to compile it into a shared object:

    $ cc -shared -fPIC -Os -o empty.so empty.c

Just as in my [PRNG shootout][prng], the benchmark calls this function
repeatedly as many times as possible before an alarm goes off.

### Procedure Linkage Tables

When a program or library calls a function in another shared object,
the compiler cannot know where that function will be located in
memory. That information isn't known until run time, after the program
and its dependencies are loaded into memory. These are usually at
randomized locations — e.g. *Address Space Layout Randomization*
(ASLR).

How is this resolved? Well, there are a couple of options.

One option is to make a note about each such call in the binary's
metadata. The run-time dynamic linker can then *patch* in the correct
address at each call site. How exactly this would work depends on the
particular [code model][mm] used when compiling the binary.

The downside to this approach is slower loading, larger binaries, and
less [sharing of code pages][map] between different processes. It's
slower loading because every dynamic call site needs to be patched
before the program can begin execution. The binary is larger because
each of these call sites needs an entry in the relocation table. And the
lack of sharing is due to the code pages being modified.

On the other hand, the overhead for dynamic function calls would be
eliminated, giving JIT-like performance as seen in the benchmark.

The second option is to route all dynamic calls through a table. The
original call site calls into a stub in this table, which jumps to the
actual dynamic function. With this approach the code does not need to
be patched, meaning it's [trivially shared][reloc] between processes.
Only one place needs to be patched per dynamic function: the entries
in the table. Even more, these patches can be performed *lazily*, on
the first function call, making the load time even faster.

On systems using ELF binaries, this table is called the Procedure
Linkage Table (PLT). The PLT itself doesn't actually get patched —
it's mapped read-only along with the rest of the code. Instead the
*Global Offset Table* (GOT) gets patched. The PLT stub fetches the
dynamic function address from the GOT and *indirectly* jumps to that
address. To lazily load function addresses, these GOT entries are
initialized with an address of a function that locates the target
symbol, updates the GOT with that address, and then jumps to that
function. Subsequent calls use the lazily discovered address.

![](/img/diagram/plt.svg)

The downside of a PLT is extra overhead per dynamic function call,
which is what shows up in the benchmark. Since the benchmark *only*
measures function calls, this appears to be pretty significant, but in
practice it's usually drowned out in noise.

Here's the benchmark:

```c
/* Cleared by an alarm signal. */
volatile sig_atomic_t running;

static long
plt_benchmark(void)
{
    long count;
    for (count = 0; running; count++)
        empty();
    return count;
}
```

Since `empty()` is in the shared object, that call goes through the PLT.

### Indirect dynamic calls

Another way to dynamically call functions is to bypass the PLT and
fetch the target function address within the program, e.g. via
`dlsym(3)`.

```c
void *h = dlopen("path/to/lib.so", RTLD_NOW);
void (*f)(void) = dlsym("f");
f();
```

Once the function address is obtained, the overhead is smaller than
function calls routed through the PLT. There's no intermediate stub
function and no GOT access. (Caveat: If the program has a PLT entry for
the given function then `dlsym(3)` may actually return the address of
the PLT stub.)

However, this is still an *indirect* function call. On conventional
architectures, *direct* function calls have an immediate relative
address. That is, the target of the call is some hard-coded offset from
the call site. The CPU can see well ahead of time where the call is
going.

An indirect function call has more overhead. First, the address has to
be stored somewhere. Even if that somewhere is just a register, it
increases register pressure by using up a register. Second, it
provokes the CPU's branch predictor since the call target isn't
static, making for extra bookkeeping in the CPU. In the worst case the
function call may even cause a pipeline stall.

Here's the benchmark:

```c
volatile sig_atomic_t running;

static long
indirect_benchmark(void (*f)(void))
{
    long count;
    for (count = 0; running; count++)
        f();
    return count;
}
```

The function passed to this benchmark is fetched with `dlsym(3)` so the
compiler can't [do something tricky][bite] like convert that indirect
call back into a direct call.

If the body of the loop was complicated enough that there was register
pressure, thereby requiring the address to be spilled onto the stack,
this benchmark might not fare as well against the PLT benchmark.

### Direct function calls

The first two types of dynamic function calls are simple and easy to
use. *Direct* calls to dynamic functions is trickier business since it
requires modifying code at run time. In my benchmark I put together a
[little JIT compiler][jit] to generate the direct call.

There's a gotcha to this: on x86-64 direct jumps are limited to a 2GB
range due to a signed 32-bit immediate. This means the JIT code has to
be placed virtually nearby the target function, `empty()`. If the JIT
code needed to call two different dynamic functions separated by more
than 2GB, then it's not possible for both to be direct.

To keep things simple, my benchmark isn't precise or very careful
about picking the JIT code address. After being given the target
function address, it blindly subtracts 4MB, rounds down to the nearest
page, allocates some memory, and writes code into it. To do this
correctly would mean inspecting the program's own memory mappings to
find space, and there's no clean, portable way to do this. On Linux
this [requires parsing virtual files under `/proc`][proc].

Here's what my JIT's memory allocation looks like. It assumes
[reasonable behavior for `uintptr_t` casts][ptr]:

```c
static void
jit_compile(struct jit_func *f, void (*empty)(void))
{
    uintptr_t addr = (uintptr_t)empty;
    void *desired = (void *)((addr - SAFETY_MARGIN) & PAGEMASK);
    /* ... */
    unsigned char *p = mmap(desired, len, prot, flags, fd, 0);
    /* ... */
}
```

It allocates two pages, one writable and the other containing
non-writable code. Similar to [my closure library][closure], the lower
page is writable and holds the `running` variable that gets cleared by
the alarm. It needed to be nearby the JIT code in order to be an
efficient RIP-relative access, just like the other two benchmark
functions. The upper page contains this assembly:

```nasm
jit_benchmark:
        push  rbx
        xor   ebx, ebx
.loop:  mov   eax, [rel running]
        test  eax, eax
        je    .done
        call  empty
        inc   ebx
        jmp   .loop
.done:  mov   eax, ebx
        pop   rbx
        ret
```

The `call empty` is the only instruction that is dynamically generated
— necessary to fill out the relative address appropriately (the minus
5 is because it's relative to the *end* of the instruction):

```c
    // call empty
    uintptr_t rel = (uintptr_t)empty - (uintptr_t)p - 5;
    *p++ = 0xe8;
    *p++ = rel >>  0;
    *p++ = rel >>  8;
    *p++ = rel >> 16;
    *p++ = rel >> 24;
```

If `empty()` wasn't in a shared object and instead located in the same
binary, this is essentially the direct call that the compiler would have
generated for `plt_benchmark()`, assuming somehow it didn't inline
`empty()`.

Ironically, calling the JIT-compiled code requires an indirect call
(e.g. via a function pointer), and there's no way around this. What
are you going to do, JIT compile another function that makes the
direct call? Fortunately this doesn't matter since the part being
measured in the loop is only a direct call.

### It's no mystery

Given these results, it's really no mystery that LuaJIT can generate
more efficient dynamic function calls than a PLT, *even if they still
end up being indirect calls*. In my benchmark, the non-PLT indirect
calls were 28% faster than the PLT, and the direct calls 43% faster
than the PLT. That's a small edge that JIT-enabled programs have over
plain old native programs, though it comes at the cost of absolutely
no code sharing between processes.


[bite]: /blog/2018/05/01/
[closure]: /blog/2017/01/08/
[ffi]: https://en.wikipedia.org/wiki/Foreign_function_interface
[hn]: https://news.ycombinator.com/item?id=17171252
[jit]: /blog/2015/03/19/
[luajit]: http://luajit.org/
[map]: /blog/2016/04/10/
[mm]: https://eli.thegreenplace.net/2012/01/03/understanding-the-x64-code-models
[prng]: /blog/2017/09/21/
[proc]: /blog/2016/09/03/
[ptr]: /blog/2016/05/30/
[reloc]: /blog/2016/12/23/
