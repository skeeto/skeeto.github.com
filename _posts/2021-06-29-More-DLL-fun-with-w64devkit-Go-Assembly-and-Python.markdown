---
title: 'More DLL fun with w64devkit: Go, assembly, and Python'
layout: post
date: 2021-06-29T21:50:30Z
tags: [c, cpp, go, win32, x86]
uuid: b2c53451-b12a-4f1a-a475-6c81096c9b5a
---

My previous article explained [how to work with dynamic-link libraries
(DLLs) using w64devkit][prev]. These techniques also apply to other
circumstances, including with languages and ecosystems outside of C and
C++. In particular, [w64devkit][] is a great complement to Go and reliably
fullfills all the needs of [cgo][] — Go's C interop — and can even
bootstrap Go itself. As before, this article is in large part an exercise
in capturing practical information I've picked up over time.

### Go: bootstrap and cgo

The primary Go implementation, confusingly [named "gc"][gc], is an
[incredible piece of software engineering][tech]. This is apparent when
building the Go toolchain itself, a process that is fast, reliable, easy,
and simple. It was originally written in C, but was re-written in Go
starting with Go 1.5. The C compiler in w64devkit can build the original C
implementation which then can be used to bootstrap any more recent
version. It's so easy that I personally never use official binary releases
and always bootstrap from source.

You will need the Go 1.4 source, [go1.4-bootstrap-20171003.tar.gz][bs].
This "bootstrap" tarball is the last Go 1.4 release plus a few additional
bugfixes. You will also need the source of the actual version of Go you
want to use, such as Go 1.16.5 (latest version as of this writing).

Start by building Go 1.4 using w64devkit. On Windows, Go is built using a
batch script and no special build system is needed. Since it shouldn't be
invoked with the BusyBox ash shell, I use [`cmd.exe`][cmd] explicitly.

    $ tar xf go1.4-bootstrap-20171003.tar.gz
    $ mv go/ bootstrap
    $ (cd bootstrap/src/ && cmd /c make)

In about 30 seconds you'll have a fully-working Go 1.4 toolchain. Next use
it to build the desired toolchain. You can move this new toolchain after
it's built if necessary.

    $ export GOROOT_BOOTSTRAP="$PWD/bootstrap"
    $ tar xf go1.16.5.src.tar.gz
    $ (cd go/src/ && cmd /c make)

At this point you can delete the bootstrap toolchain. You probably also
want to put Go on your PATH.

    $ rm -rf bootstrap/
    $ printf 'PATH="$PATH;%s/go/bin"\n' "$PWD" >>~/.profile
    $ source ~/.profile

Not only is Go now available, so is the full power of cgo. (Including [its
costs][cost] if used.)

### Vim suggestions

Since w64devkit is oriented so much around Vim, here's my personal Vim
configuration for Go. I don't need or want fancy plugins, just access to
`goimports` and a couple of corrections to Vim's built-in Go support (`[[`
and `]]` navigation). The included `ctags` understands Go, so tags
navigation works the same as it does with C. `\i` saves the current
buffer, runs `goimports`, and populates the quickfix list with any errors.
Similarly `:make` invokes `go build` and, as expected, populates the
quickfix list.

```vim
autocmd FileType go setlocal makeprg=go\ build
autocmd FileType go map <silent> <buffer> <leader>i
    \ :update \|
    \ :cexpr system("goimports -w " . expand("%")) \|
    \ :silent edit<cr>
autocmd FileType go map <buffer> [[
    \ ?^\(func\\|var\\|type\\|import\\|package\)\><cr>
autocmd FileType go map <buffer> ]]
    \ /^\(func\\|var\\|type\\|import\\|package\)\><cr>
```

Go only comes with `gofmt` but `goimports` is just one command away, so
there's little excuse not to have it:

    $ go install golang.org/x/tools/cmd/goimports@latest

Thanks to GOPROXY, all Go dependencies are accessible without (or before)
installing Git, so this tool installation works with nothing more than
w64devkit and a bootstrapped Go toolchain.

### cgo DLLs

The intricacies of cgo are beyond the scope of this article, but the gist
is that a Go source file contains C source in a comment followed by
`import "C"`. The imported `C` object provides access to C types and
functions. Go functions marked with an `//export` comment, as well as the
commented C code, are accessible to C. The latter means we can use Go to
implement a C interface in a DLL, and the caller will have no idea they're
actually talking to Go.

To illustrate, here's an little C interface. To keep it simple, I've
specifically sidestepped some more complicated issues, particularly
involving memory management.

```c
// Which DLL am I running?
int version(void);

// Generate 64 bits from a CSPRNG.
unsigned long long rand64(void);

// Compute the Euclidean norm.
float dist(float x, float y);
```

Here's a C implementation which I'm calling "version 1".

```c
#include <math.h>
#include <windows.h>
#include <ntsecapi.h>

__declspec(dllexport)
int
version(void)
{
    return 1;
}

__declspec(dllexport)
unsigned long long
rand64(void)
{
    unsigned long long x;
    RtlGenRandom(&x, sizeof(x));
    return x;
}

__declspec(dllexport)
float
dist(float x, float y)
{
    return sqrtf(x*x + y*y);
}
```

As discussed in the previous article, each function is exported using
`__declspec` so that they're available for import. As before:

    $ cc -shared -Os -s -o hello1.dll hello1.c

Side note: This could be trivially converted into a C++ implementation
just by adding `extern "C"` to each declaration. It disables C++ features
like name mangling, and follows the C ABI so that the C++ functions appear
as C functions. Compiling the C++ DLL is exactly the same.

Suppose we wanted to implement this in Go instead of C. We already have
all the tools needed to do so. Here's a Go implementation, "version 2":

```go
package main

import "C"
import (
	"crypto/rand"
	"encoding/binary"
	"math"
)

//export version
func version() C.int {
	return 2
}

//export rand64
func rand64() C.ulonglong {
	var buf [8]byte
	rand.Read(buf[:])
	r := binary.LittleEndian.Uint64(buf[:])
	return C.ulonglong(r)
}

//export dist
func dist(x, y C.float) C.float {
	return C.float(math.Sqrt(float64(x*x + y*y)))
}

func main() {
}
```

Note the use of C types for all arguments and return values. The `main`
function is required since this is the main package, but it will never be
called. The DLL is built like so:

    $ go build -buildmode=c-shared -o hello2.dll hello2.go

Without the `-o` option, the DLL will lack an extension. This works fine
since it's mostly only convention on Windows, but it may be confusing
without it.

What if we need an import library? This will be required when linking with
the MSVC toolchain. In the previous article we asked Binutils to generate
one using `--out-implib`. For Go we have to handle this ourselves via
`gendef` and `dlltool`.

    $ gendef hello2.dll
    $ dlltool -l hello2.lib -d hello2.def

The only way anyone upgrading would know version 2 was implemented in Go
is that the DLL is a lot bigger (a few MB vs. a few kB) since it now
contains an entire Go runtime.

### NASM assembly DLL

We could also go the other direction and implement the DLL using plain
assembly. It won't even require linking against a C runtime.

w64devkit includes two assemblers: GAS (Binutils) which is used by GCC,
and NASM which has [friendlier syntax][nasm]. I prefer the latter whenever
possible — exactly why I included NASM in the distribution. So here's how
I implemented "version 3" in NASM assembly.

```nasm
bits 64

section .text

global DllMainCRTStartup
export DllMainCRTStartup
DllMainCRTStartup:
	mov eax, 1
	ret

global version
export version
version:
	mov eax, 3
	ret

global rand64
export rand64
rand64:
	rdrand rax
	ret

global dist
export dist
dist:
	mulss  xmm0, xmm0
	mulss  xmm1, xmm1
	addss  xmm0, xmm1
	sqrtss xmm0, xmm0
	ret
```

The `global` directive is common in NASM assembly and causes the named
symbol to have the external linkage needed when linking the DLL. The
`export` directive is Windows-specific and is equivalent to `dllexport` in
C.

Every DLL must have an entrypoint, usually named `DllMainCRTStartup`. The
return value indicates if the DLL successfully loaded. So far this has
been handled automatically by the C implementation, but at this low level
we must define it explicitly.

Here's how to assemble and link the DLL:

    $ nasm -fwin64 -o hello3.o hello3.s
    $ ld -shared -s -o hello3.dll hello3.o

### Call the DLLs from Python

Python has a nice, built-in C interop, `ctypes`, that allows Python to
call arbitrary C functions in shared libraries, including DLLs, without
writing C to glue it together. To tie this all off, here's a Python
program that loads all of the DLLs above and invokes each of the
functions:

```python
import ctypes

def load(version):
    hello = ctypes.CDLL(f"./hello{version}.dll")
    hello.version.restype = ctypes.c_int
    hello.version.argtypes = ()
    hello.dist.restype = ctypes.c_float
    hello.dist.argtypes = (ctypes.c_float, ctypes.c_float)
    hello.rand64.restype = ctypes.c_ulonglong
    hello.rand64.argtypes = ()
    return hello

for hello in load(1), load(2), load(3):
    print("version", hello.version())
    print("rand   ", f"{hello.rand64():016x}")
    print("dist   ", hello.dist(3, 4))
```

After loading the DLL with `CDLL` the program defines each function
prototype so that Python knows how to call it. Unfortunately it's not
possible to build Python with w64devkit, so you'll also need to install
the standard CPython distribution in order to run it. Here's the output:

    $ python finale.py
    version 1
    rand    b011ea9bdbde4bdf
    dist    5.0
    version 2
    rand    f7c86ff06ae3d1a2
    dist    5.0
    version 3
    rand    2a35a05b0482c898
    dist    5.0

That output is the result of four different languages interfacing in one
process: C, Go, x86-64 assembly, and Python. Pretty neat if you ask me!


[bs]: https://dl.google.com/go/go1.4-bootstrap-20171003.tar.gz
[cgo]: https://golang.org/cmd/cgo/
[cmd]: /blog/2021/02/08/
[cost]: https://dave.cheney.net/2016/01/18/cgo-is-not-go
[gc]: https://golang.org/doc/faq#What_compiler_technology_is_used_to_build_the_compilers
[nasm]: https://elronnd.net/writ/2021-02-13_att-asm.html
[prev]: /blog/2021/05/31/
[tech]: /blog/2020/01/21/
[w64devkit]: /blog/2020/05/15/

