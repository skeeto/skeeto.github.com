---
title: Test cross-architecture without leaving home
layout: post
date: 2021-08-21T23:59:33Z
tags: [c, go, debian, trick]
uuid: ac34f8a0-af73-4301-b21b-5a47d48e3069
excerpt_separator: <!--more-->
---

I like to test my software across different environments, on [strange
platforms][w64], and with [alternative implementations][dos]. Each has its
own quirks and oddities that can shake bugs out earlier. C is particularly
good at this since it has such a wide selection of compilers and runs on
everything. For instance I count at least 7 distinct C compilers in Debian
alone. One advantage of [writing portable software][port] is access to a
broader testing environment, and it's one reason I prefer to target
standards rather than specific platforms.

However, I've long struggled with architecture diversity. My work and
testing has been almost entirely on x86, with ARM as a distant second
(Raspberry Pi and friends). Big endian hosts are particularly rare.
However, I recently learned a trick for quickly and conveniently accessing
many different architectures without even leaving my laptop: [QEMU User
Emulation][user]. Debian and its derivatives support this very well and
require almost no setup or configuration.

<!--more-->

### Cross-compilation Example

While there are many options, my main cross-testing architecture has been
PowerPC. It's 32-bit big endian, while I'm generally working on 64-bit
little endian, which is exactly the sort of mismatch I'm going for. I use
a Debian-supplied cross-compiler and qemu-user tools. The [binfmt][bf]
support is especially slick, so that's how I usually use it.

    # apt install gcc-powerpc-linux-gnu qemu-user-binfmt

`binfmt_misc` is a kernel module that teaches Linux how to recognize
arbitrary binary formats. For instance, there's a Wine binfmt so that
Linux programs can transparently `exec(3)` Windows `.exe` binaries. In the
case of QEMU User Mode, binaries for foreign architectures are loaded into
a QEMU virtual machine configured in user mode. In user mode there's no
guest operating system, and instead the virtual machine translates guest
system calls to the host operating system.

The first package gives me `powerpc-linux-gnu-gcc`. The prefix is the
[architecture tuple][tuple] describing the instruction set and system ABI.
To try this out, I have a little test program that inspects its execution
environment:

```c
#include <stdio.h>

int main(void)
{
    char *w = "?";
    switch (sizeof(void *)) {
    case 1: w = "8";  break;
    case 2: w = "16"; break;
    case 4: w = "32"; break;
    case 8: w = "64"; break;
    }

    char *b = "?";
    switch (*(char *)(int []){1}) {
    case 0: b = "big";    break;
    case 1: b = "little"; break;
    }

    printf("%s-bit, %s endian\n", w, b);
}
```

When I run this natively on x86-64:

    $ gcc test.c
    $ ./a.out
    64-bit, little endian

Running it on PowerPC via QEMU:

    $ powerpc-linux-gnu-gcc -static test.c
    $ ./a.out
    32-bit, big endian

Thanks to binfmt, I could execute it as though the PowerPC binary were a
native binary. With just a couple of environment variables in the right
place, I could pretend I'm developing on PowerPC — aside from emulation
performance penalties of course.

However, you might have noticed I pulled a sneaky on ya: `-static`. So far
what I've shown only works with static binaries. There's no dynamic loader
available to run dynamically-linked binaries. Fortunately this is easy to
fix in two steps. The first step is to install the dynamic linker for
PowerPC:

    # apt install libc6-powerpc-cross

The second is to tell QEMU where to find it since, unfortunately, it
cannot currently do so on its own.

    $ export QEMU_LD_PREFIX=/usr/powerpc-linux-gnu

Now I can leave out the `-static`:

    $ powerpc-linux-gnu-gcc test.c
    $ ./a.out
    32-bit, big endian

A practical example: Remember [binitools][bini]? I'm now ready to run its
[fuzz-generated test suite][fuzz] on this cross-testing platform.

    $ git clone https://github.com/skeeto/binitools
    $ cd binitools/
    $ make check CC=powerpc-linux-gnu-gcc
    ...
    PASS: 668/668

Or if I'm going to be running `make` often:

    $ export CC=powerpc-linux-gnu-gcc
    $ make -e check

Recall: [make's `-e` flag][make] passes the environment through, so I
don't need to pass `CC=...` on the command line each time.

When setting up a test suite for your own programs, consider how difficult
it would be to run the tests under customized circumstances like this. The
easier it is to run your tests, the more they're going to be run. I've run
into many projects with such overly-complex test builds that even enabling
sanitizers in the tests suite was a pain, let alone cross-architecture
testing.

Dependencies? There might be a way to use [Debian's multiarch support][ma]
to install these packages, but I haven't been able to figure it out. You
likely need to build dependencies yourself using the cross compiler.

### Testing with Go

None of this is limited to C (or even C++). I've also successfully used
this to test Go libraries and programs cross-architecture. This isn't
nearly as important since it's harder to write unportable Go than C — e.g.
[dumb pointer tricks][bof] are literally labeled "unsafe". However, Go
(gc) trivializes cross-compilation and is statically compiled, so it's
incredibly simple. Once you've installed `qemu-user-binfmt` it's entirely
transparent:

    $ GOARCH=mips64 go test

That's all there is to cross-platform testing. If for some reason binfmt
doesn't work (WSL) or you don't want to install it, there's just one extra
step (package named `example`):

    $ GOARCH=mips64 go test -c
    $ qemu-mips64-static example.test

The `-c` option builds a test binary but doesn't run it, instead allowing
you to choose where and how to run it.

It even works [with cgo][cgo] — if you're willing to jump through the same
hoops as with C of course:

```go
package main

// #include <stdint.h>
// uint16_t v = 0x1234;
// char *hi = (char *)&v + 0;
// char *lo = (char *)&v + 1;
import "C"
import "fmt"

func main() {
	fmt.Printf("%02x %02x\n", *C.hi, *C.lo)
}
```

With `go run` on x86-64:

    $ CGO_ENABLED=1 go run example.go
    34 12

Via QEMU User Mode:

    $ export CGO_ENABLED=1
    $ export GOARCH=mips64
    $ export CC=mips64-linux-gnuabi64-gcc
    $ export QEMU_LD_PREFIX=/usr/mips64-linux-gnuabi64
    $ go run example.go
    12 34

I was pleasantly surprised how well this all works.

### One dimension

Despite the variety, all these architectures are still "running" the same
operating system, Linux, and so they only vary on one dimension. For most
programs primarily targeting x86-64 Linux, PowerPC Linux is practically
the same thing, while x86-64 OpenBSD is foreign territory despite sharing
an architecture and ABI ([System V][sysv]). Testing across operating
systems still requires spending the time to install, configure, and
maintain these extra hosts. That's an article for another time.


[bf]: https://en.wikipedia.org/wiki/Binfmt_misc
[bini]: https://github.com/skeeto/binitools
[bof]: https://commandcenter.blogspot.com/2012/04/byte-order-fallacy.html
[cgo]: /blog/2021/06/29/
[dos]: /blog/2018/04/13/
[fuzz]: /blog/2019/01/25/
[ma]: https://wiki.debian.org/Multiarch/HOWTO
[make]: /blog/2017/08/20/
[port]: /blog/2017/03/30/
[tuple]: https://wiki.debian.org/Multiarch/Tuples
[user]: https://wiki.debian.org/QemuUserEmulation
[wsl]: /blog/2017/11/30/
[sysv]: /blog/2016/11/17/
[w64]: /blog/2020/05/15/
