---
title: WebAssembly as a Python extension platform
layout: post
date: 2026-01-01T21:21:19Z
tags: [c]
uuid: 91e7555d-950f-47c6-84b8-bee0070f61a9
---

Software above some complexity level tends to sport an extension language,
becoming a kind of software platform itself. Lua fills this role well, and
of course there's JavaScript for web technologies. [WebAssembly][wasm]
generalizes this, and any Wasm-targeting programming language can extend a
Wasm-hosting application. It has more friction than supplying a script in
a text file, but extension authors can write in their language of choice,
and use more polished development tools — debugging, [testing][fuzz], etc.
— than typically available for a typical extension language. Python is
traditionally extended through native code behind a C interface, but it's
recently become practical to extend Python with Wasm. That is we can ship
an architecture-independent Wasm blob inside a Python library, and use it
without requiring a native toolchain on the host system. Let's discuss two
different use cases and their pitfalls.

Normally we'd extend Python in order to access an external interface that
Python cannot access on its own. Wasm runs in a sandbox with no access to
the outside world whatsoever, so it obviously isn't useful for that case.
Extensions may also grant Python more speed, which is one of Wasm's main
selling points. We can also use Wasm to access *embeddable capabilities*
written in a different programming language which do not require external
access.

For preferred non-WASI Wasm runtime is Volodymyr Shymanskyy's [wasm3][].
It's plain old C and very friendly to embedding in the same was as, say,
SQLite. Performance is middling, though a C program running on wasm3 is
still quite a bit faster than an equivalent Python program. It has Python
bindings, [pywasm3][], but it's distributed only in source code form. That
is, the host machine must have a C toolchain in order to use pywasm3,
which defeats my purposes here. If there's a C toolchain, I might as well
just use that instead of going through Wasm.

For the use cases in this article, the best option is [wasmtime-py][]. The
distribution includes binaries for Windows, macOS, and Linux on x86-64 and
ARM64, which covers nearly all Python installations. Hosts require nothing
more than a Python interpreter, no native toolchains. It's almost as good
as having Wasm built into Python itself. In my tests it's 3x–10x faster
than wasm3, so for my first use case the situation is even better. The
catch is that it currently weighs ~18MiB (installed), and in the future
will likely rival the Python interpreter itself. The API also breaks on a
monthly basis, so you're signing up for the upgrade treadmill lest your
own program perishes to bitrot after a couple of years. This article is
about version 40.

### Usage examples and gotchas

The [official examples][ex] don't do anything non-trivial or interesting,
and so to figure things out I had to study [the documentation][pydoc],
which does not offer many hints. Basic setup looks like this:

```py
import functools
import wasmtime

store    = wasmtime.Store()
module   = wasmtime.Module.from_file(store.engine, "example.wasm")
instance = wasmtime.Instance(store, module, ())
exports  = instance.exports(store)

memory = exports["memory"].get_buffer_ptr(store)
func1  = functools.partial(exports["func1"], store)
func2  = functools.partial(exports["func2"], store)
func3  = functools.partial(exports["func3"], store)
```

A store is an allocation region from which we allocate all Wasm objects.
It is not possible to free individual objects except to discard the whole
store. Quite sensible, honestly. What's *not* sensible is how often I have
to repeat myself, passing the store back into every object in order to use
it. These objects are associated with exactly one store and cannot be used
with different stores. [Use the wrong store and it panics][panic]: It's
already keeping track internally! I do not understand why the interface
works this way. So to make things simpler, I use `functools.partial` to
bind the `store` parameter and so get the interface I expect.

The `get_buffer_ptr` object is a buffer protocol object, and if you're
moving anything other than bytes that's probably what you want to use to
access memory. The usual caveats apply for this object: If you [change the
memory size][alloc] you probably want to grab a fresh buffer object. For
bytes (e.g. buffers and strings) I prefer the `read` and `write` methods.

Because [multi-value][mv] is still in an experimental state in the Wasm
ecosystem, you will likely not pass structs with Wasm. Anything more
complicated than scalars will require pointers and copying data in and out
of Wasm linear memory. This involves the usual trap that catches nearly
everyone: Wasm interfaces make no distinction between pointers and
integers, and Wasm runtimes interpret generally interpret all integers as
signed. What that means is **your pointers are signed unless you take
action**. Addresses start at 0, so this is bad, bad news.

```py
malloc = functools.partial(exports["func1"], store)

hello = b"hello"
pointer = malloc(len(hello))
assert pointer
memory = exports["memory"].write(store, hello, pointer)  # WRONG!
```

To make matters worse, wasmtime-py adds its own footgun: The `read` and
`write` methods adopt the questionable Python convention of negative
indices acting from the end. If `malloc` returns a pointer in the upper
half of memory, the negative pointer will pass the bounds check inside
`write` because negative is valid, then quietly store to the wrong
address! Doh!

I wondered how common this error, so I searched online. I could find only
one non-trivial wasmtime-py use in the wild, in a sandboxed PDF reader. It
falls into the negative pointer trap as I expected. Not only that, it's [a
buffer overflow into Python's memory space][ovf]:

```py
            buf_ptr = malloc(store, len(pdf_data))
            mem_data = memory.data_ptr(store)

            for i, byte in enumerate(pdf_data):
                mem_data[buf_ptr + i] = byte
```

The `data_ptr` method returns a non-bounds-checked raw `ctypes` pointer,
so this is actually a double mistake. First, it shouldn't trust pointers
coming out of Wasm if it cares at all about sandboxing. The second is the
potential negative pointer, which in this case would write outside of the
Wasm memory and in Python's memory, hopefully seg-faulting.

What's one to do? **Every pointer coming out of Wasm must be truncated**
with a mask:

```py
pointer = malloc(...) & 0xffffffff   # correct for wasm32!
```

This interprets the result as unsigned. 64-bit Wasm needs a 64-bit mask,
though in practice you will never get a valid negative pointer from 64-bit
Wasm. This rule applies to JavaScript as well, where the idiom is:

```js
let pointer = malloc(...) >>> 0
```

Wasm runtimes cannot help — they lack the necessary information — and this
is perhaps a fundamental flaw in Wasm's design. Once you know about it you
see this mistake happening everywhere.

Now that you have a proper address, you can apply it to a buffer protocol
view of memory. If you're using NumPy there are various ways to interact
with this memory by wrapping it in NumPy types, though only if you're on a
little endian host. (If you're on a big endian machine, just give up on
running Wasm anyway.) The first use case I have in mind typically involves
copying plain Python values in and out. The [`struct` package][struct] is
quite handy here:

```py
vec2   = malloc(...) & 0xffffffff
memory = exports["memory"].get_buffer_ptr(store)
struct.pack_into("<ii", memory, vec2, x, y)
```

It fills a similar role to [JavaScript `DataView`][dv]. If you're copying
lots of numbers, with CPython it's faster to construct a custom format
string rather than use a loop:

```py
nums: list[int] = ...
struct.pack_into(f"<{len(nums)}i", memory, buf, *nums)
```

To copy structures back out, use `struct.unpack_from`. If you're moving
strings, you'll need to `.encode()` and `.decode()` to convert to and from
`bytes`, which are well-suited to `read` and `write`.

In practice with real Wasm programs you're going to be interacting with
the "guest" allocator from the outside, to request memory into which you
copy inputs for a function. In my examples I've used `malloc` because it
requires no elaboration, but as usual [a bump allocator][arena] solves
this so much better, especially because it doesn't require stuffing a
whole general purpose allocator inside the Wasm program. Have one global
arena — no other threads will sharing that Wasm instance — rapid fire a
bunch of allocations as needed without any concern for memory management
in the "host", call the function, which might allocate a result from that
arena, then reset the arena to clean up. In essence a stack for passing
values in and out.

### WebAssembly as faster Python

Suppose we noticed a computational hot spot in our Python program in a
pure Python function (e.g. not calling out to an extension). Optimizing
this function would be wise. Based on my experiments if I re-implement
that function in C, compile it to Wasm, then run that bit of Wasm in place
of the original function, I can expect around a 10x speed-up. In general C
is more like 100x faster than Python, and the overhead of interfacing with
Wasm — copying stuff in and out, etc. — can be high, but not so high as to
not be profitable. This improves further if I can change the interface,
e.g. require callers to use the buffer protocol.

Thanks to wasmtime-py, I could introduce this change without fussing with
cross-compilers to build distribution binaries, nor require a toolchain on
the target, just a hefty Python package. Might be worth it.

My [main experimental benchmark][bench] is a variation on [my solution to
the "Two Sum" problem][two], which I originally wrote for JavaScript, then
extended to pywasm3 and later wasmtime-py. It's simple, just interesting
enough, and representative of the sort of Wasm drop-in I have in mind. It
has the same interface, but implements it with Wasm.

```py
# Original Pythonic interface
def twosum(nums: list[int], target: int) -> tuple[int, int] | None:
    ...

# Stateful Wasm interface
class TwoSumWasm():
    def __init__(self):
        store    = wasmtime.Store()
        module   = wasmtime.Module.from_file(store.engine, ...)
        instance = wasmtime.Instance(store, module, ())
        ...

    def twosum(self, nums, target):
        # ... use wasm instance ...
```

There's some state to it with the Wasm instance in tow. If you hide that
by making it global you'll need to synchronize your threads around it. In
a multi-threaded program perhaps these would be lazily-constructed thread
locals. I haven't had to solve this yet.

However, the weakness of the wasmtime "store" really shows: Notice how
compilation and instantiation are bound together in one store? I cannot
compile once and then create disposable instances on the fly, e.g. as
required for each run of a WASI program. Every instance permanently
extends the compilation store. In practice we must wastefully re-compile
the Wasm program for each disposable instance. Despite appearances,
compilation and instantiation are not actually distinct steps, as they are
in JavaScript's Wasm API. `wasmtime.Instance` accepts a store as its first
argument, *suggesting* use of a different store for instantiation. That
would solve this problem, but as of this writing it *must* be the same
store used to compile the module. This is a fatal flaw for certain real
use cases, particularly WASI.

### WebAssembly as embedded capabilities

Loup Vaillant's [Monocypher][mc] is a wonderful cryptography library.
Lean, efficient, and embedding-friendly, so much so it's distributed in
amalgamated form. It requires no libc or runtime, so we can compile it
straight to Wasm with almost any Clang toolchain:

```
$ clang --target=wasm32 -nostdlib -O2 -Wl,--no-entry -Wl,--export-all
        -o monocypher.wasm monocypher.c
```

It's not "Wasm-aware" so I need `--export-all` to expose the interface.
This is swell because, as single translation unit, anything with external
linkage is the interface. Though remember what I said about interacting
with the guest allocator? This has no allocator, nor should it. It's not
so usable in this form because we'd need to manage memory from the
outside. Do-able, but it's easy to improve by adding a couple more
functions, sticking to a single translation unit:

```c
#include "monocypher.c"

extern char  __heap_base[];
static char *heap_used;
static char *heap_high;

void *bump_alloc(ptrdiff_t size)
{
    // ...
}

void bump_reset()
{
    ptrdiff_t len = heap_used - __heap_base;
    __builtin_memset(__heap_base, 0, len);  // wipe keys, etc.
    heap_used = __heap_base;
}
```

I've [discussed `__heap_base` before][alloc], which is part of the ABI.
We'll push keys, inputs, etc. onto this "stack", run our cryptography
routine, copy out the result, then reset the bump allocator, which wipes
out all sensitive data. Often `memset` is insufficient — typically it's
zero-then-free, and compilers see the [lifetime][] about to end — but no
lifetime ends here, and stores to this "heap" memory externally observable
as far as the abstract machine can tell. (Otherwise we couldn't reliably
copy out our results!)

There's a lot to this API, but I'm only going to look at [the AEAD
interface][doc]. We "lock" up some data in an encrypted box, write any
unencrypted label we'd like on the outside. Then later we can unlock the
box, which will only open for us if neither the contents of the box nor
the label were tampered with. That's some solid API design:

```c
void crypto_aead_lock(uint8_t       *cipher_text,
                      uint8_t        mac  [16],
                      const uint8_t  key  [32],
                      const uint8_t  nonce[24],
                      const uint8_t *ad,         size_t ad_size,
                      const uint8_t *plain_text, size_t text_size);
int crypto_aead_unlock(uint8_t       *plain_text,
                       const uint8_t  mac  [16],
                       const uint8_t  key  [32],
                       const uint8_t  nonce[24],
                       const uint8_t *ad,          size_t ad_size,
                       const uint8_t *cipher_text, size_t text_size);
```

By compiling to Wasm we can access this functionality from Python almost
like it was pure Python, and interact with other systems using Monocypher.

Since Monocypher does not interact with the outside world on its own, it
relies on callers to use their system's CSPRNG to create those nonces and
keys, which we'll do using [the `secrets` built-in package][secrets]:

```py
class Monocypher:
    def __init__(self):
        ...
        self._read   = functools.partial(memory.read, store)
        self._write  = functools.partial(memory.write, store)
        self.__alloc = functools.partial(exports["bump_alloc"], store)
        self._reset  = functools.partial(exports["bump_reset"], store)
        self._lock   = functools.partial(exports["crypto_aead_lock"], store)
        self._unlock = functools.partial(exports["crypto_aead_unlock"], store)
        self._csprng = secrets.SystemRandom()

    def _alloc(self, n):
        return self.__alloc(n) & 0xffffffff

    def generate_key(self):
        return self._csprng.randbytes(32)

    def generate_nonce(self):
        return self._csprng.randbytes(24)

    ...
```

With a solid foundation, all that follows comes easily. A `finally`
guarantees secrets are always removed from Wasm memory, and the rest is
just about copying bytes around:

```py
    def aead_lock(self, text, key, ad = b""):
        assert len(key) == 32
        try:
            macptr   = self._alloc(16)
            keyptr   = self._alloc(32)
            nonceptr = self._alloc(24)
            adptr    = self._alloc(len(ad))
            textptr  = self._alloc(len(text))

            self._write(key, keyptr)
            nonce = self.generate_nonce()
            self._write(nonce, nonceptr)
            self._write(ad,    adptr)
            self._write(text,  textptr)

            self._lock(
                textptr,
                macptr,
                keyptr,
                nonceptr,
                adptr, len(ad),
                textptr, len(text),
            )
            return (
                self._read(macptr, macptr+16),
                nonce,
                self._read(textptr, textptr+len(text)),
            )
        finally:
            self._reset()
```

And `aead_unlock` is basically the same in reverse, but throws if the box
fails to unlock, perhaps due to tampering:

```py
    def aead_unlock(self, text, mac, key, nonce, ad = b""):
        assert len(mac) == 16
        assert len(key) == 32
        assert len(nonce) == 24
        try:
            macptr   = self._alloc(16)
            keyptr   = self._alloc(32)
            nonceptr = self._alloc(24)
            adptr    = self._alloc(len(ad))
            textptr  = self._alloc(len(text))

            self._write(mac, macptr)
            self._write(key, keyptr)
            self._write(nonce, nonceptr)
            self._write(ad, adptr)
            self._write(text, textptr)

            if self._unlock(
                textptr,
                macptr,
                keyptr,
                nonceptr,
                adptr, len(ad),
                textptr, len(text),
            ):
                raise ValueError("AEAD mismatch")
            return self._read(textptr, textptr+len(text))
        finally:
            self._reset()
```

Usage:

```py
mc = Monocypher()
key = mc.generate_key()
message = "Hello, world!"
mac, nonce, encrypted = mc.aead_lock(message.encode(), key)
```

Transmit `mac`, `nonce`, and `encrypted` to the other party (or your
future self), who already has the `key`:

```py
decrypted = mc.aead_unlock(encrypted, mac, key, nonce)
```

Find the **complete source [in my scratch repository][mce]**.

While I have a few reservations about wasmtime-py, it fascinates me how
well this all works. It's been my hammer in search of a nail for some time
now.


[alloc]: /blog/2025/04/19/
[arena]: /blog/2023/09/27/
[bench]: https://github.com/skeeto/scratch/tree/master/wasm-bench
[doc]: https://monocypher.org/manual/aead
[dv]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DataView
[ex]: https://github.com/bytecodealliance/wasmtime-py/tree/main/examples
[fuzz]: /blog/2025/02/05/
[lifetime]: /blog/2025/09/30/
[mc]: https://monocypher.org/
[mce]: https://github.com/skeeto/scratch/tree/master/wasm-monocypher
[mv]: https://github.com/WebAssembly/multi-value/blob/master/proposals/multi-value/Overview.md
[ovf]: https://github.com/paulocoutinhox/pdfium-lib/blob/139d5037/modules/wasm.py#L601-L606
[panic]: https://docs.wasmtime.dev/api/wasmtime/struct.Store.html#cross-store-usage-of-items
[pydoc]: https://bytecodealliance.github.io/wasmtime-py/
[pywasm3]:https://github.com/wasm3/pywasm3
[secrets]: https://docs.python.org/3/library/secrets.html
[struct]: https://docs.python.org/3/library/struct.html
[two]: /blog/2023/06/26/
[wasm3]: https://github.com/wasm3/wasm3
[wasm]: /blog/2025/04/04/
[wasmtime-py]: https://github.com/bytecodealliance/wasmtime-py
