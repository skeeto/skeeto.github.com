---
title: Lessons learned from my first dive into WebAssembly
layout: post
date: 2025-04-04T04:01:20Z
tags: [c, tutorial]
uuid: 9881d125-2f2c-4fee-a959-222c9449399b
---

It began as a [water sort puzzle][rules] solver, constructed similarly to
[my British Square solver][bs]. It was nearly playable, so I added a user
interface [with SDL2][sdl]. My wife enjoyed it on her desktop, but wished
to play on her phone. So then I needed to either rewrite it in JavaScript
and hope the solver was still fast enough for real-time use, or figure out
WebAssembly (WASM). I succeeded, and now [my game runs in browsers][game]
([source][]). Like [before][bsd], next I ported [my pkg-config clone][u]
to the WASM System Interface ([WASI][]), whipped up a proof-of-concept UI,
and [it too runs in browsers][demo]. Neither use a language runtime,
resulting in little 8kB and 28kB WASM binaries respectively. In this
article I share my experiences and techniques.

WASM is a [specification][spec] defining an abstract stack machine with a
Harvard architecture, and related formats. There are just four types, i32,
i64, f32, and f64. It also has "linear" octet-addressable memory starting
at zero, with no alignment restrictions on loads and stores. Address zero
is a valid, writable address, which resurfaces some, old school, high
level language challenges regarding null pointers. There are 32-bit and
64-bit flavors, though the latter remains experimental. That suits me: I
appreciate smaller pointers on 64-bit hosts, and I wish I could opt into
more often (e.g. x32).

As browser tech goes, they chose an apt name: WebAssembly is to the web as
JavaScript is to Java.

There are distinct components at play, and much of the online discussion
doesn't do a great job drawing lines between them:

* WASM module: A compiled and linked image — like ELF or PE — containing
  sections for code, types, globals, import table, export table, and so
  on. The export table lists the module's entry points. It has an optional
  *start section* indicating which function initializes a loaded image.
  (In practice almost nobody actually uses the start section.) A WASM
  module can only affect the outside world through imported functions.
  WASM itself defines no external interfaces for WASM programs, not even
  printing or logging.

* WASM runtime: Loads WASM modules, linking import table entries into the
  module. Because WASM modules include types, the runtime can type check
  this linkage at load time. With imports resolved, it executes the start
  function, if any, then executes zero or more of its entry points, which
  hopefully invokes import functions such a way as to produce useful
  results, or perhaps simply return useful outputs.

* WASM compiler: Converts a high-level language to low-level WASM. In
  order to do so, it requires some kind of Application Binary Interface
  (ABI) to map the high-level language concepts onto the machine. This
  typically introduces additional execution elements, and it's important
  that we distinguish them from the abstract machine's execution elements.
  Clang is the only compiler we'll be discussing in this article, though
  there are many. During compilation the *function indices* are yet
  unknown and so references will need to be patched in by a linker.

* WASM linker: Settles the shape of the WASM module and links up the
  functions emitted by the compiler. LLVM comes with `wasm-ld`, and it
  goes hand-in-hand with Clang as a compiler.

* Language runtime: Unless you're hand-writing raw WASM, your high-level
  language probably has a standard library with operating system
  interfaces. C standard library, POSIX interfaces, etc. This runtime
  likely maps onto some standardized set of imports, most likely the
  aforementioned WASI, which defines a set of POSIX-like functions that
  WASM modules may import. Because I [think we could do better][review],
  [as usual][crt] [around here][thr], in this article we're going to
  eschew the language runtime and code directly against raw WASI. You
  still have [easy access hash tables and dynamic arrays][ex].

A combination of compiler-linker-runtime is conventionally called a
*toolchain*. However, because almost any Clang installation can target
WASM out-of-the-box, and we're skipping the language runtime, you can
compile any of programs discussed in this article, including my game, with
nothing more than Clang (invoking `wasm-ld` implicitly). If you have a
WASM runtime, which includes your browser, you can run them, too! Though
this article will mostly focus on WASI, and you'll need a WASI-capable
runtime to run those examples, which doesn't include browsers (short of
implementing the API with JavaScript).

I wasn't particularly happy with the WASM runtimes I tried, so I cannot
enthusiastically recommend one. I'd love if I could point to one and say,
"Use the same Clang to compile the runtime that you're using to compile
WASM!" Alas, I had issues compiling, the runtime was buggy, or WASI was
incomplete. However, [wazero][] (Go) was the easiest for me to use and it
worked well enough, so I will use it in examples:

    $ go install github.com/tetratelabs/wazero/cmd/wazero@latest

The WASM Binary Toolkit ([WABT][]) is good to have on hand when working
with WASM, particularly `wasm2wat` to inspect WASM modules, sort of like
`objdump` or `readelf`. It converts WASM to the WebAssembly Text Format
(WAT).

Learning WASM I had quite some difficultly finding information. Outside of
the WASM specification, which, despite its length, is merely a narrow
slice of the ecosystem, important technical details are scattered all over
the place. Some is only available as source code, some buried comments in
GitHub issues, and some lost behind dead links as repositories have moved.
Large parts of LLVM are undocumented beyond an mention of existence. WASI
has no documentation in a web-friendly format — so I have nothing to link
from here when I mention its system calls — just some IDL sources in a Git
repository. An old [`wasi.h`][wasi.h] was the most readable, complete
source of truth I could find.

Fortunately WASM is old enough that [LLMs][llm] are well-versed in it, and
simply asking questions, or for usage examples, was more effective than
searching online. If you're stumped on how to achieve something in the
WASM ecosystem, try asking a state-of-the-art LLM for help.

### Example programs

Let's go over concrete examples to lay some foundations. Consider this
simple C function:

```c
float norm(float x, float y)
{
    return x*x + y*y;
}
```

To compile to WASM (32-bit) with Clang, we use the `--target=wasm32`:

    $ clang -c --target=wasm32 -O example.c

The object file `example.o` is in WASM format, so WABT can examine it.
Here's the output of `wasm2wat -f`, where `-f` produces output in the
"folded" format, which is how I prefer to read it.

```racket
(module
  (type (;0;) (func (param f32 f32) (result f32)))
  (import "env" "__linear_memory" (memory (;0;) 0))
  (func $norm (type 0) (param f32 f32) (result f32)
    (f32.add
      (f32.mul
        (local.get 0)
        (local.get 0))
      (f32.mul
        (local.get 1)
        (local.get 1)))))
```

We can see [the ABI][abi] taking shape: Clang has predictably mapped
`float` into `f32`. It similarly maps `char`, `short`, `int` and `long`
onto `i32`. In 64-bit WASM, the Clang ABI is LP64 and maps `long` onto
`i64`. There's a also `$norm` function which takes two `f32` parameters
and returns an `f32`.

Getting a little more complex:

```c
__attribute((import_name("f")))
void f(int *);

__attribute((export_name("example")))
void example(int x)
{
    f(&x);
}
```

The `import_name` function attribute indicates the module will not define
it, even in another translation unit, and that it intends to import it.
That is, `wasm-ld` will place it in the import table. The `export_name`
function attribute indicates it's an entry point, and so `wasm-ld` will
list it in the export table. Linking it will make things a little clearer:

    $ clang --target=wasm32 -nostdlib -Wl,--no-entry -O example.c

The `-nostdlib` is because we won't be using a language runtime, and
`--no-entry` to tell the linker not to implicitly export a function
(default: `_start`) as an entry point. You might think this is connected
with the WASM *start function*, but `wasm-ld` does not support the *start
section* at all! We'll have use for an entry point later. The folded WAT:

```racket
(module $a.out
  (type (;0;) (func (param i32)))
  (import "env" "f" (func $f (type 0)))
  (func $example (type 0) (param i32)
    (local i32)
    (global.set $__stack_pointer
      (local.tee 1
        (i32.sub
          (global.get $__stack_pointer)
          (i32.const 16))))
    (i32.store offset=12
      (local.get 1)
      (local.get 0))
    (call $f
      (i32.add
        (local.get 1)
        (i32.const 12)))
    (global.set $__stack_pointer
      (i32.add
        (local.get 1)
        (i32.const 16))))
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global $__stack_pointer (mut i32) (i32.const 66560))
  (export "memory" (memory 0))
  (export "example" (func $example)))
```

There's a lot to unfold:

* Pointers were mapped onto `i32`. Pointers are a high-level concept, and
  linear memory is addressed by an integral offset. This is typical of
  assembly after all.

* There's now a `__stack_pointer`, which is part of the Clang ABI, not
  WASM. The WASM abstract machine is a stack machine, but that stack
  doesn't exist in linear memory. So you cannot take the address of values
  on the WASM stack. There are lots of things C needs from a stack that
  WASM doesn't provide. So, *in addition to the WASM stack*, Clang
  maintains another downward-growing stack in linear memory for these
  purposes, and the `__stack_pointer` global is the stack register of its
  ABI. We can see it's allocated something like 64kB for the stack. (It's
  a little more because program data is placed below the stack.)

* It should be mostly readable without knowing WASM: The function
  subtracts a 16-byte stack frame, stores a copy of the argument in it,
  then uses its memory offset for the first parameter to the import `f`.
  Why 16 bytes when it only needs 4? Because the stack is kept 16-byte
  aligned. Before returning, the function restores the stack pointer.

As mentioned earlier, address zero is valid as far as the WASM runtime is
concerned, though dereferences are still undefined in C. This makes it
more difficult to catch bugs. Given a null pointer this function would
most likely read a zero at address zero and the program keeps running:

```c
int get(int *p)
{
    return *p;
}
```

In WAT:

```racket
(func $get (type 0) (param i32) (result i32)
  (i32.load
    (local.get 0)))
```

Since the "hardware" won't fault for us, ask Clang to do it instead:

    $ clang ... -fsanitize=undefined -fsanitize-trap ...

Now in WAT:

```racket
(module
  (type (;0;) (func (param i32) (result i32)))
  (import "env" "__linear_memory" (memory (;0;) 0))
  (func $get (type 0) (param i32) (result i32)
    (block  ;; label = @1
      (block  ;; label = @2
        (br_if 0 (;@2;)
          (i32.eqz
            (local.get 0)))
        (br_if 1 (;@1;)
          (i32.eqz
            (i32.and
              (local.get 0)
              (i32.const 3)))))
      (unreachable))
    (i32.load
      (local.get 0))))
```

Given a null pointer, `get` executes the `unreachable` instruction,
causing the runtime to trap. In practice this is unrecoverable. Consider:
nothing will restore `__stack_pointer`, and so the stack will "leak" the
existing frames. (This can be worked around by exporting `__stack_pointer`
and `__stack_high` via the `--export` linker flag, then restoring the
stack pointer in the runtime after traps.)

WASM was extended with [bulk memory operations][bulk], and so there are
single instructions for `memset` and `memmove`, which Clang maps onto the
built-ins:

```c
void clear(void *buf, long len)
{
    __builtin_memset(buf, 0, len);
}
```

([Below LLVM 20][llvm] you will need the undocumented `-mbulk-memory`
option.) In WAT we see this as `memory.fill`:

```racket
(module
  (type (;0;) (func (param i32 i32)))
  (import "env" "__linear_memory" (memory (;0;) 0))
  (func $clear (type 0) (param i32 i32)
    (block  ;; label = @1
      (br_if 0 (;@1;)
        (i32.eqz
          (local.get 1)))
      (memory.fill
        (local.get 0)
        (i32.const 0)
        (local.get 1)))))
```

That's great! I wish this worked so well outside of WASM. It's one reason
[w64devkit][] has `-lmemory`, after all. Similarly `__builtin_trap()` maps
onto the `unreachable` instruction, so we can reliably generate those as
well.

What about structures? They're passed by address. Parameter structures go
on the stack, then its address passed. To return a structure, a function
accepts an implicit *out* parameter in which to write the return. This
isn't unusual, except that it's challenging to manage across module
boundaries, i.e. in imports and exports, because caller and callee are in
different address spaces. It's especially tricky to return a structure
from an export, as the caller must somehow allocate space in the callee's
address space for the result. The [multi-value extension][multi-value]
solves this, but using it in C involves an ABI change, which is still
experimental.

### Water Sort Game

Something you might not have expected: My water sort game imports no
functions! It only exports three functions:

```c
void      game_init(i32 seed);
DrawList *game_render(i32 width, i32 height, i32 mousex, i32 mousey);
void      game_update(i32 input, i32 mousex, i32 mousey, i64 now);
```

The game uses [IMGUI-style][imgui] rendering. The caller passes in the
inputs, and the game returns a kind of *display list* telling it what to
draw. In the SDL version these turn into SDL renderer calls. In the web
version, these turn into canvas draws, and "mouse" inputs may be touch
events. It plays and feels the same on both platforms. Simple!

I didn't realize it at the time, but building the SDL version first was
critical to my productivity. **Debugging WASM programs is really dang
hard!** WASM tooling has yet to catch up with 1995, let alone 2025.
Source-level debugging is still experimental and impractical. Developing
applications on the WASM platform. It's about as ergonomic as [developing
in MS-DOS][blast]. Instead, develop on a platform much better suited for
it, then *port* your application to WASM after you've [got the issues
worked out][fuzz]. The less WASM-specific code you write, the better, even
if it means writing more code overall. Treat it as you would some weird
embedded target.

The game comes with 10,000 seeds. I generated ~200 million puzzles, sorted
them by difficulty, and skimmed the top 10k most challenging. In the game
they're still sorted by increading difficulty, so it gets harder as you
make progress.

### WASM System Interface

WASI allows us to get a little more hands on. Let's start with a Hello
World program. A WASI application exports a traditional `_start` entry
point which returns nothing and takes no arguments. I'm also going to set
up some basic typedefs:

```c
typedef unsigned char       u8;
typedef   signed int        i32;
typedef   signed long long  i64;
typedef   signed long       iz;

void _start(void)
{
}
```

`wasm-ld` will automatically export this function, so we don't need an
`export_name` attribute. This program successfully does nothing:

    $ clang --target=wasm32 -nostdlib -o hello.wasm hello.c
    $ wazero run hello.wasm && echo ok
    ok

To write output WASI defines `fd_write()`:

```c
typedef struct {
    u8 *buf;
    iz  len;
} IoVec;

#define WASI(s) __attribute((import_module("wasi_unstable"),import_name(s)))
WASI("fd_write")  i32  fd_write(i32, IoVec *, iz, iz *);
```

Technically those `iz` variables are supposed to be `size_t`, passed
through WASM as `i32`, but this is a foreign function, I know the ABI, and
so [I can do as I please][win32]. I absolutely love that WASI barely uses
null-terminated strings, not even for paths, which is a breath of fresh
air, but they still [marred the API with unsigned sizes][ssize]. Which I
choose to ignore.

This function is shaped like [POSIX `writev()`][writev]. I've also set it
up for import, including a module name. The oldest, most stable version of
WASI is called `wasi_unstable`. (I suppose it shouldn't be surprising that
finding information in this ecosystem is difficult.)

Every returning WASI function returns an `errno` value, with zero as
success rather than some kind of [in-band signaling][band]. Hence the
final out parameter unlike POSIX `writev()`.

Armed with this function, let's use it:

```c
void _start(void)
{
    u8    msg[] = "hello world\n";
    IoVec iov   = {msg, sizeof(msg)-1};
    iz    len   = 0;
    fd_write(1, &iov, 1, &len);
}
```

Then:

    $ clang --target=wasm32 -nostdlib -o hello.wasm hello.c
    $ wazero run hello.wasm
    hello world

Keep going and you'll have [something like `printf`][buf] before long. If
the write fails, we should probably communicate the error with at least
the exit status. Because `_start` doesn't return a status, we need to
exit, for which we have `proc_exit`. It doesn't return, so no `errno`
return value.

```c
WASI("proc_exit") void proc_exit(i32);

void _start(void)
{
    // ...
    i32 err = fd_write(1, &iov, 1, &len);
    proc_exit(!!err);
}
```

To get the command line arguments, call `args_sizes_get` to get the size,
allocate some memory, then `args_get` to read the arguments. Same goes for
the environment with a similar pair of functions. The sizes do not include
a null pointer terminator, which is sensible.

Now that you know how to find and use these functions, you don't need me
to go through each one. However, *opening files* is a special, complicated
case:

```c
WASI("path_open") i32 path_open(i32,i32,u8*,iz,i32,i64,i64,i32,i32*);
```

That's 9 parameters — and I had thought [Win32 `CreateFileW`][cfw] was
over the top. It's even more complex than it looks. It works more like
[POSIX `openat()`][openat], except there's no current working directory
and so no `AT_FDCWD`. Every file and directory is opened *relative to*
another directory, and absolute paths are invalid. If there's no
`AT_FDCWD`, how does one open the *first* directory? That's called a
*preopen* and it's core to the file system security mechanism of WASI.

The WASM runtime preopens zero or more directories before starting the
program and assigns them the lowest numbered file descriptors starting at
file descriptor 3 (after standard input, output, and error). A program
intending to use `path_open` must first traverse the file descriptors,
probing for preopens with `fd_prestat_get` and retrieving their path name
with `fd_prestat_dir_name`. This name may or may not map back onto a real
system path, and so this is a kind of virtual file system for the WASM
module. The probe stops on the first error.

To open an absolute path, it must find a matching preopen, then from it
construct a path relative to that directory. This part I much dislike, as
the module must contain complex path parsing functionality even in the
simple case. Opening files is the most complex piece of the whole API.

I mentioned before that program data is below the Clang stack. With the
stack growing down, this sounds like a bad idea. A stack overflow quietly
clobbers your data, and is difficult to recognize. More sensible to put
the stack at the bottom so that it overflows off the bottom of memory and
causes a fast fault. Fortunately there's a switch for that:

    $ clang --target=wasm32 ... -Wl,--stack-first ...

This is what you want by default. The actual default layout is left over
from an early design flaw in `wasm-ld`, and it's an oversight that it has
not yet been corrected.

### u-config

The above is in action in the [u-config WASM port][main]. You can download
the WASM module, [pkg-config.wasm][], used in the web demo to run it in
your favorite WASI-capable WASM runtime:

    $ wazero run pkg-config.wasm --modversion pkg-config
    0.33.3

Though there are no preopens, so it cannot read any files. The `-mount`
option maps real file system paths to preopens. This mounts the entire
root file system read-only (`ro`) as `/`.

    $ wazero run -mount /::ro pkg-config.wasm --cflags sdl2
    -I/usr/include/SDL2 -D_REENTRANT

I doubt this is useful for anything, but it was a vehicle for learning and
trying WASM, and the results are pretty neat.

In the next article I discuss [allocating the allocator][next].


[WABT]: https://github.com/WebAssembly/wabt
[WASI]: https://wasi.dev/
[abi]: https://github.com/WebAssembly/tool-conventions/blob/main/BasicCABI.md
[band]: /blog/2016/09/23/
[blast]: /blog/2018/04/13/
[bs]: /blog/2020/10/19/
[bsd]: /blog/2025/03/06/
[buf]: /blog/2023/02/13/
[bulk]: https://github.com/WebAssembly/bulk-memory-operations
[cfw]: https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-createfilew
[crt]: /blog/2023/02/15/
[demo]: https://skeeto.github.io/u-config/
[ex]: /blog/2025/01/19/
[fuzz]: /blog/2025/02/05/
[game]: /water-sort/
[imgui]: https://www.youtube.com/watch?v=DYWTw19_8r4
[llm]: /blog/2024/11/10/
[llvm]: https://releases.llvm.org/20.1.0/docs/ReleaseNotes.html#changes-to-the-webassembly-backend
[main]: https://github.com/skeeto/u-config/blob/0c86829e/main_wasm.c
[multi-value]: https://github.com/WebAssembly/multi-value/blob/master/proposals/multi-value/Overview.md
[next]: /blog/2025/04/19/
[openat]: https://pubs.opengroup.org/onlinepubs/9799919799/functions/openat.html
[pkg-config.wasm]: https://skeeto.github.io/u-config/pkg-config.wasm
[review]: /blog/2023/02/11/
[rules]: https://www.coolmathgames.com/blog/how-to-play-lipuzz-water-sort
[sdl]: /blog/2023/01/08/
[source]: https://github.com/skeeto/scratch/tree/master/water-sort
[spec]: https://webassembly.github.io/spec/
[ssize]: https://www.youtube.com/watch?v=wvtFGa6XJDU
[thr]: /blog/2023/03/23/
[u]: /blog/2023/01/18/
[w64devkit]: https://github.com/skeeto/w64devkit
[wasi.h]: https://github.com/WebAssembly/wasi-libc/blob/e9524a09/libc-bottom-half/headers/public/wasi/api.h
[wazero]: https://wazero.io/
[win32]: /blog/2023/05/31/
[writev]: https://pubs.opengroup.org/onlinepubs/9799919799/functions/writev.html
