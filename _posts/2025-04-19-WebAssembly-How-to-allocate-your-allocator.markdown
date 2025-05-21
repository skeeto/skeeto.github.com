---
title: 'WebAssembly: How to allocate your allocator'
layout: post
date: 2025-04-19T03:18:20Z
tags: [c]
uuid: dc2863e4-9601-4e42-bbd2-3cb4a5315d4d
---

An early, small hurdle [diving into WebAssembly][wasm] was allocating my
allocator. On a server or desktop with virtual memory, the allocator asks
the operating system to map fresh pages into its address space ([sbrk][],
anonymous mmap, [VirtualAlloc][]), which it then dynamically allocates to
different purposes. In an embedded context, dynamic allocation memory is
typically a fixed, static region chosen at link time. The Wasm execution
environment more resembles an embedded system, but both kinds of obtaining
raw memory are viable and useful in different situations.

For the purposes of this discussion, the actual allocator isn't important.
It could be [a simple arena allocator][arena], or a more general purpose
[buddy allocator][buddy]. It could even be garbage collected with [Boehm
GC][boehm]. Though WebAssembly's linear memory is a poor fit for such a
[conservative][gc] garbage collector. In a compact address space starting
at zero, and which doesn't include code, memory addresses will be small
numbers, and less distinguishable from common integer values. There's also
the issue that the garbage collector cannot scan the Wasm stack, which is
hidden from Wasm programs by design. Only the ABI stack is visible. So a
garbage collector requires cooperation from the compiler — essentially as a
distinct calling convention — to spill all heap pointers on the ABI stack
before function calls. Wasm C and C++ toolchains do not yet support this
in a practical capacity.

### Exporting a static heap

Let's start with the embedded case because it's simpler, and reserve a
dynamic memory region at link time. WebAssembly has just reached 8 years
old, so it's early, and as we keep discovering, Wasm tooling is still
immature. `wasm-ld` doesn't understand linker scripts, and there's no
stable, low-level assembly language on which to build, e.g. to reserve
space, define symbols, etc. WAT is too high level and inflexible for this
purpose, as we'll soon see. So our only option is to brute force it in a
high-level language:

```c
char heap[16<<20];  // 16MiB
```

Plugging it into an arena allocator:

```c
typedef struct {
    char *beg;
    char *end;
} Arena;

Arena getarena(void)
{
    Arena a = {0};
    a.beg = heap;
    a.end = heap + sizeof(heap);
    return a;
}
```

Unfortunately `heap` isn't generic memory, but a high-level variable with
a specific, fixed type. That's why it would have been nice to reserve the
memory outside the high-level language. In practice this works fine so
long as everything is aligned, but strictly speaking, allocating any
variable except `char` from this arena involves incompatible loads and
stores on a `char` array. Clang doesn't document any [inline assembly
interface][asm] for Wasm, but neither does Clang forbid it. That leaves
just enough room to launder the pointer if you're worried about this
technicality:

```c
Arena getarena(void)
{
    Arena a = {0};
    a.beg = heap;
    asm ("" : "+r"(a.beg));  // launder
    a.end = a.beg + sizeof(heap);
    return a;
}
```

The `+r` means `a.beg` is both input and output. The address of the heap
goes into the black box, and as far as the compiler is concerned, *some
mystery address comes out* which, critically, has no effective type. The
assembly block is empty (`""`), so it's just a no-op, and we know (*wink
wink*) it's really the same address. Because the heap was "used" by the
black box, Clang won't optimize the heap out of existence beneath us. Also
note that `a.end` was derived from the laundered pointer.

**Update**: The next C standard [will improve this situation][n3238] and
so pointer laundering will no longer be unnecessary. A straight `char`
array could be used as an arena.

This static variable technique works well only in an *exported* memory
configuration, which is what `wasm-ld` uses by default. When a module
exports its memory, it indicates how much linear memory it requires on
start, and the Wasm runtime allocates and zero-initializes it at module
initialization time. C and C++ toolchains depend on that runtime zeroing
to initialize static and global variables, which are defined to be so
initialized. Compilers generate code assuming these variables are zero
initialized. This same paradigm is used for [.bss sections][bss] in hosted
environments.

In an *imported* memory configuration, linear memory is uninitialized. The
memory may be re-used from, say, a destroyed module without zeroing, and
may contain arbitrary data. In that case, C and C++ toolchains must zero
the memory explicitly. It could potentially be done with a `memory.fill`
instruction in the *start section*, but LLVM does not support start
sections. Instead it uses an [*active data segment*][bulk] — a chunk of
data copied into linear memory by the Wasm runtime during initialization,
before running the start function.

That is, when importing memory, LLVM *actually stores all those
zeros in the Wasm module* so that the runtime can copy it into linear
memory. Wasm has no built-in compression, so **your Wasm module will be at
least as large as your heap**! Exporting or importing memory is determined
at *link-time*, so at *compile-time* the compiler must assume the worst
case. If you compile the example above, you get a 16MiB "object" file (in
Wasm format):

    $ clang --target=wasm32 -c -O example.c
    $ du -h example.o
    16.0M   example.o

The WAT version of this file is 48MiB — clearly unsuitable as a low-level
assembler. If linking with exported memory, `wasm-ld` discards all-zero
active data segments. If using an imported memory configuration, it's
copied into the final image, producing a huge Wasm image, though highly
compressible. As a rule, avoid importing memory when using an LLVM
toolchain. Regardless, large heaps created this way will have a
significant compile-time cost.

    $ time echo 'char heap[256<<20];' | clang --target=wasm32 -c -xc -
    real    0m0.334s
    user    0m0.013s
    sys     0m0.262s

(If only Clang had some sort of "noinit" variable attribute in order to
allow `heap` to be uninitialized…)

### Growing a dynamic heap

Wasm programs can grow linear memory using an [sbrk][]-like [`memory.grow`
instruction][grow]. It operates in quantities of pages (64kB), and returns
the old memory size. Because memory starts at zero, the old memory size is
also the base address of the new allocation. Clang provides access to this
instruction via an undocumented built-in:

```c
size_t __builtin_wasm_memory_grow(int, size_t);
```

The first parameter selects a memory because [someday there might be more
than one][multi-memory]. From this built-in we can define `sbrk`:

```c
void *sbrk(ptrdiff_t size)
{
    size_t npages = (size + 0xffffu) >> 16;  // round up
    size_t old    = __builtin_wasm_memory_grow(0, npages);
    if (old == -1ul) {
        return 0;
    }
    return (void *)(old << 16);
}
```

To which Clang compiles (note the `memory.grow`):

```racket
(func $sbrk (param i32) (result i32)
  (select
    (i32.const 0)
    (i32.shl
      (local.tee 0
        (memory.grow
          (i32.shr_u
            (i32.add
              (local.get 0)
              (i32.const 65535))
            (i32.const 16))))
      (i32.const 16))
    (i32.eq
      (local.get 0)
      (i32.const -1))))
```

Applying that to create an arena like before:

```c
Arena newarena(ptrdiff_t cap)
{
    Arena a = {0};
    a.beg = sbrk(cap);
    if (a.beg) {
        a.end = a.beg + cap;
    }
    return a;
}
```

Now we can choose the size of the arena, and we can use this to create
multiple arenas (e.g. permanent, scratch, etc.). We could even continue
growing the last-created arena in-place when it's full.

If there was no `memory.grow` instruction, it could be implemented as a
request through an imported function. The embedder using the Wasm runtime
[can grow the memory on the module's behalf][mdn] in the same manner. But
as that documentation indicates, either way growing the memory comes with
a downside in the most common Wasm runtimes, browsers: It "detaches" the
memory from references, which complicates its use for the embedder. If a
Wasm module may grow its memory at any time, the embedder must reacquire
the memory handle after every call. It's not difficult, but it's easy to
forget, and mistakes are likely to go unnoticed until later.

### Importing a dynamic heap

There's a middle ground where a Wasm module imports a dynamic-sized heap.
That is, linear memory beyond the module's base initialization. This might
be the case, for instance, in a programming competition, where contestants
submit Wasm modules which must complete a task using the supplied memory.
In that case we don't reserve a static heap, so we're not facing the
storing-zeros issue. However, how do we "find" the memory? Linear memory
layout will look something like so:

    0 <-- stack | data | heap --> ?
    |-----------------------------|

This diagram reflects the more sensible `wasm-ld --stack-first` layout,
where the ABI stack overflows off the bottom end of memory. The heap is
just excess memory beyond the data. To find the upper bound, Wasm has a
[`memory.size`][grow] instruction to query linear memory size, which again
Clang provides as an undocumented built-in:

```c
size_t __builtin_wasm_memory_size(int);
```

Like before, this returns the result in number of 64k pages. That's the
high end. How do we find the low end? Similar to `__stack_pointer`, the
linker creates a `__heap_base` constant, which is the address delineating
data and heap in the diagram above. To use it, we need to declare it:

```c
extern char __heap_base[];
```

Notice how it's an array, not a pointer. It doesn't *hold* an address, it
*is* an address. In an ELF context this would called an *absolute symbol*.
That's everything we need to find the bounds of the heap:

```c
Arena getarena(void)
{
    Arena a = {0};
    a.beg = __heap_base;
    a.end = (char *)(__builtin_wasm_memory_size(0) << 16);
    return a;
}
```

Then we continue forward using whatever memory the embedder deigned to
provide. Hopefully it's enough!


[VirtualAlloc]: https://learn.microsoft.com/en-us/windows/win32/api/memoryapi/nf-memoryapi-virtualalloc
[arena]: /blog/2023/09/27/
[asm]: /blog/2024/12/20/
[boehm]: https://www.hboehm.info/gc/
[bss]: https://en.wikipedia.org/wiki/.bss
[buddy]: https://github.com/skeeto/scratch/blob/master/misc/buddy.c
[bulk]: https://github.com/WebAssembly/bulk-memory-operations/blob/master/proposals/bulk-memory-operations/Overview.md
[gc]: https://en.wikipedia.org/wiki/Tracing_garbage_collection#Precise_vs._conservative_and_internal_pointers
[grow]: https://webassembly.github.io/spec/core/bikeshed/#syntax-instr-memory
[mdn]: https://developer.mozilla.org/en-US/docs/WebAssembly/Reference/JavaScript_interface/Memory/grow
[multi-memory]: https://github.com/WebAssembly/multi-memory/blob/main/proposals/multi-memory/Overview.md
[n3238]: https://www.open-std.org/jtc1/sc22/wg14/www/docs/n3238.pdf
[sbrk]: https://pubs.opengroup.org/onlinepubs/7908799/xsh/brk.html
[wasm]: /blog/2025/04/04/
