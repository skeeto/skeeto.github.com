---
title: Closures as Win32 window procedures
layout: post
date: 2025-12-12T19:52:10Z
tags: [c, win32, x86]
uuid: 7bf46ec6-a8b2-4ffa-857a-86c040357702
---

Back in 2017 I wrote [about a technique for creating closures in C][cl]
using [JIT-compiled][jit] wrapper. It's neat, though rarely necessary in
real programs, so I don't think about it often. I applied it to `qsort`,
which [sadly][libc] accepts no context pointer. More practical would be
working around [insufficient custom allocator interfaces][custom], to
create allocation functions at run-time bound to a particular allocation
region. I've learned a lot since I last wrote about this subject, and [a
recent article][lua] had me thinking about it again, and how I could do
better than before. In this article I will enhance Win32 window procedure
callbacks with a fifth argument, allowing us to more directly pass extra
context. I'm using [w64devkit][] on x64, but the everything here should
work out-of-the-box with any x64 toolchain that speaks GNU assembly.

A [window procedure][wndproc] has this prototype:

```c
LRESULT Wndproc(
  HWND hWnd,
  UINT Msg,
  WPARAM wParam,
  LPARAM lParam,
);
```

To create a window we must first register a class with `RegisterClass`,
which accepts a set of properties describing a window class, including a
pointer to one of these functions.

```c
    MyState *state = ...;

    RegisterClassA(&(WNDCLASSA){
        // ...
        .lpfnWndProc   = my_wndproc,
        .lpszClassName = "my_class",
        // ...
    });

    HWND hwnd = CreateWindowExA("my_class", ..., state);
```

The thread drives a message pump with events from the operating system,
dispatching them to this procedure, which then manipulates the program
state in response:

```c
    for (MSG msg; GetMessageW(&msg, 0, 0, 0);) {
        TranslateMessage(&msg);
        DispatchMessageW(&msg);  // calls the window procedure
    }
```

All four `WNDPROC` parameters are determined by Win32. There is no context
pointer argument. So how does this procedure access the program state? We
generally have two options:

1. Global variables. Yucky but easy. Frequently seen in tutorials.
2. A `GWLP_USERDATA` pointer attached to the window.

The second option takes some setup. Win32 passes the last `CreateWindowEx`
argument to the window procedure when the window created, via `WM_CREATE`.
The procedure attaches the pointer to its window as `GWLP_USERDATA`. This
pointer is passed indirectly, through a `CREATESTRUCT`. So ultimately it
looks like this:

```c
    case WM_CREATE:
        CREATESTRUCT *cs = (CREATESTRUCT *)lParam;
        void *arg = (struct state *)cs->lpCreateParams;
        SetWindowLongPtr(hwnd, GWLP_USERDATA, (LONG_PTR)arg);
        // ...
```

In future messages we can retrieve it with `GetWindowLongPtr`. Every time
I go through this I wish there was a better way. What if there was a fifth
window procedure parameter though which we could pass a context?

```
typedef LRESULT Wndproc5(HWND, UINT, WPARAM, LPARAM, void *);
```

We'll build just this as a trampoline. The [x64 calling convention][x64]
passes the first four arguments in registers, and the rest are pushed on
the stack, including this new parameter. Our trampoline cannot just stuff
the extra parameter in the register, but will actually have to build a
stack frame. Slightly more complicated, but barely so.

### Allocating executable memory

In previous articles, and in the programs where I've applied techniques
like this, I've allocated executable memory with `VirtualAlloc` (or `mmap`
elsewhere). This introduces a small challenge for solving the problem
generally: Allocations may be arbitrarily far from our code and data, out
of reach of relative addressing. If they're further than 2G apart, we need
to encode absolute addresses, and in the simple case would just assume
they're always too far apart.

These days I've more experience with executable formats, and allocation,
and I immediately see a better solution: Request a block of writable,
executable memory from the loader, then allocate our trampolines from it.
Other than being executable, this memory isn't special, and [allocation
works the usual way][arena], using functions unaware it's executable. By
allocating through the loader, this memory will be part of our loaded
image, guaranteed to be close to our other code and data, allowing our JIT
compiler to assume [a small code model][cm].

There are a number of ways to do this, and here's one way to do it with
GNU-styled toolchains targeting COFF:

```nasm
        .section .exebuf,"bwx"
        .globl exebuf
exebuf:	.space 1<<21
```

This assembly program defines a new section named `.exebuf` containing 2M
of writable (`"w"`), executable (`"x"`) memory, allocated at run time just
like `.bss` (`"b"`). We'll treat this like an arena out of which we can
allocate all trampolines we'll probably ever need. With careful use of
`.pushsection` this could be basic inline assembly, but I've left it as a
separate source. On the C side I retrieve this like so:

```c
typedef struct {
    char *beg;
    char *end;
} Arena;

Arena get_exebuf()
{
    extern char exebuf[1<<21];
    Arena r = {exebuf, exebuf+sizeof(exebuf)};
    return r;
}
```

Unfortunately I have to repeat myself on the size. There are different
ways to deal with this, but this is simple enough for now. I would have
loved to define the array in C with the GCC [`section` attribute][attr],
but as is usually the case with this attribute, it's not up to the task,
lacking the ability to set section flags. Besides, by not relying on the
attribute, any C compiler could compile this source, and we only need a
GNU-style toolchain to create the tiny COFF object containing `exebuf`.

While we're at it, a reminder of some other basic definitions we'll need:

```c
#define S(s)            (Str){s, sizeof(s)-1}
#define new(a, n, t)    (t *)alloc(a, n, sizeof(t), _Alignof(t))

typedef struct {
    char     *data;
    ptrdiff_t len;
} Str;

Str clone(Arena *a, Str s)
{
    Str r = s;
    r.data = new(a, r.len, char);
    memcpy(r.data, s.data, (size_t)r.len);
    return r;
}
```

Which have been discussed at length in previous articles.

### Trampoline compiler

From here the plan is to create a function that accepts a `Wndproc5` and a
context pointer to bind, and returns a classic `WNDPROC`:

```c
WNDPROC make_wndproc(Arena *, Wndproc5, void *arg);
```

Our window procedure now gets a fifth argument with the program state:

```c
LRESULT my_wndproc(HWND, UINT, WPARAM, LPARAM, void *arg)
{
    MyState *state = arg;
    // ...
}
```

When registering the class we wrap it in a trampoline compatible with
`RegisterClass`:

```c
    RegisterClassA(&(WNDCLASSA){
        // ...
        .lpfnWndProc   = make_wndproc(a, my_wndproc, state),
        .lpszClassName = "my_class",
        // ...
    });
```

All windows using this class will readily have access to this state object
through their fifth parameter. It turns out setting up `exebuf` was the
more complicated part, and `make_wndproc` is quite simple!

```c
WNDPROC make_wndproc(Arena *a, Wndproc5 proc, void *arg)
{
    Str thunk = S(
        "\x48\x83\xec\x28"      // sub   $40, %rsp
        "\x48\xb8........"      // movq  $arg, %rax
        "\x48\x89\x44\x24\x20"  // mov   %rax, 32(%rsp)
        "\xe8...."              // call  proc
        "\x48\x83\xc4\x28"      // add   $40, %rsp
        "\xc3"                  // ret
    );
    Str r   = clone(a, thunk);
    int rel = (int)((uintptr_t)proc - (uintptr_t)(r.data + 24));
    memcpy(r.data+ 6, &arg, sizeof(arg));
    memcpy(r.data+20, &rel, sizeof(rel));
    return (WNDPROC)r.data;
}
```

The assembly allocates a new stack frame, with callee shadow space, and
with room for the new argument, which also happens to re-align the stack.
It stores the new argument for the `Wndproc5` just above the shadow space.
Then calls into the `Wndproc5` without touching other parameters. There
are two "patches" to fill out, which I've initially filled with dots: the
context pointer itself, and a 32-bit signed relative address for the call.
It's going to be very near the callee. The only thing I don't like about
this function is that I've manually worked out the patch offsets.

It's probably not useful, but it's easy to update the context pointer at
any time if hold onto the trampoline pointer:

```c
void set_wndproc_arg(WNDPROC p, void *arg)
{
    memcpy((char *)p+6, &arg, sizeof(arg));
}
```

So, for instance:

```c
    MyState *state[2] = ...;  // multiple states
    WNDPROC proc = make_wndproc(a, my_wndproc, state[0]);
    // ...
    set_wndproc_arg(proc, state[1]);  // switch states
```

Though I expect the most common case is just creating multiple procedures:

```c
    WNDPROC procs[] = {
        make_wndproc(a, my_wndproc, state[0]),
        make_wndproc(a, my_wndproc, state[1]),
    };
```

To my slight surprise these trampolines still work with an active [Control
Flow Guard][cfg] system policy. Trampolines do not have stack unwind
entries, and I thought Windows might refuse to pass control to them.

Here's a complete, runnable example if you'd like to try it yourself:
[`main.c` and `exebuf.s`][gist]

### Better cases

This is more work than going through `GWLP_USERDATA`, and real programs
have a small, fixed number of window procedures — typically one — so this
isn't the best example, but I wanted to illustrate with a real interface.
Again, perhaps the best real use is a library with a weak custom allocator
interface:

```c
typedef struct {
    void *(*malloc)(size_t);   // no context pointer!
    void  (*free)(void *);     // "
} Allocator;

void *arena_malloc(size_t, Arena *);

// ...

    Allocator perm_allocator = {
        .malloc = make_trampoline(exearena, arena_malloc, perm);
        .free   = noop_free,
    };
    Allocator scratch_allocator = {
        .malloc = make_trampoline(exearena, arena_malloc, scratch);
        .free   = noop_free,
    };
```

Something to keep in my back pocket for the future.


[arena]: /blog/2025/01/19/
[attr]: https://gcc.gnu.org/onlinedocs/gcc-3.2/gcc/Variable-Attributes.html
[cfg]: https://learn.microsoft.com/en-us/windows/win32/secbp/control-flow-guard
[cl]: /blog/2017/01/08/
[cm]: https://eli.thegreenplace.net/2012/01/03/understanding-the-x64-code-models#small-code-model
[custom]: /blog/2023/12/17/
[gist]: https://gist.github.com/skeeto/13363b78489b26bed7485ec0d6b2c7f8
[jit]: /blog/2015/03/19/
[libc]: /blog/2023/02/11/
[lua]: https://lowkpro.com/blog/creating-c-closures-from-lua-closures.html
[w64devkit]: https://github.com/skeeto/w64devkit
[wndproc]: https://learn.microsoft.com/en-us/windows/win32/api/winuser/nc-winuser-wndproc
[x64]: https://learn.microsoft.com/en-us/cpp/build/x64-calling-convention
