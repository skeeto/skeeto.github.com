---
title: 'Meet the new xxd for w64devkit: rexxd'
layout: post
date: 2025-02-17T00:49:49Z
tags: [c, win32]
uuid: a3ad2465-f53c-43d3-acc7-988d9d4d3989
---

xxd is a versatile hexdump utility with a "reverse" feature, originally
written between 1990–1996. The Vim project soon adopted it, and it's lived
there ever since. If you have Vim, you also have xxd. Its primary use
cases are (1) the basis for a hex editor due to its `-r` reverse option
that can *unhexdump* its previous output, and (2) a data embedding tool
for C and C++ (`-i`). The former provides Vim's rudimentary hex editor
functionality. The second case is of special interest to [w64devkit][]:
`xxd -i` appears in many builds that [embed arbitrary data][embed]. It's
important that w64devkit has a compatible implementation, and a freshly
rewritten, improved xxd, **[rexxd][]**, now replaces the original xxd (as
`xxd`).

For those unfamiliar with xxd, examples are in order. Its default hexdump
output looks like this:

    $ echo hello world | xxd | tee dump
    00000000: 6865 6c6c 6f20 776f 726c 640a            hello world.

Octets display in pairs with an ASCII text listing on the right. All
configurable. I can run this in reverse (`-r`), recovering the original
input:

    $ xxd -r dump
    hello world

The tool reads the offset before the colon, the hexadecimal octets, and
ignores the text column. By editing `dump` with a text editor, I can
change the raw octets of the original input. From this point of view, the
hexdump is actually a program of two alternating instructions: seek and
write. xxd *seeks* to the offset, *writes* the octets, then repeats. It
also doesn't truncate the output file, so a hexdump can express binary
patches as a seek/write program.

    $ echo hello world >hello
    $ echo 6: 65766572796f6e650a | xxd -r - hello
    $ cat hello
    hello everyone

That seeks to offset `0x6`, then writes the 9 octets. The xxd parser is
flexible, and I did not need to follow the default format. It figured out
the format on its own, and rexxd further improves on this. We can use it
to create large files out of thin air, too:

    $ echo 3fffffff: 00 | xxd -r - >1G

This command creates an all-zero, 1GiB file, `1G`, by seeking to just
before 1GiB then writing a zero. I used `>1G` so that the shell would
truncate the file before starting `xxd` — in case it was larger or
contained non-zeros.

This is a "smart seek" of course, and its not literally seeking on every
line. The tool tracks its file position and only seeks when necessary. If
seeking fails, it simulates the seek using a write if possible. When would
it not be possible? Lines need not be in order, of course, and so it may
need to seek backwards. Lines can also overlap in contents. If it weren't
for buffering — or if rexxd had a [unified buffer cache][cache] — then by
using the same file for input and output an "xxd program" could write new
instructions for itself and [accidentally become Turing-complete][make].

The other common mode, `-i`, looks like this:

    $ echo hello world >hello
    $ xxd -i hello hello.c

Which produces this `hello.c`:

```c
unsigned char hello[] = {
  0x68, 0x65, 0x6c, 0x6c, 0x6f, 0x20, 0x77, 0x6f, 0x72, 0x6c, 0x64, 0x0a
};
unsigned int hello_len = 12;
```

Note how it converted the file name into variable names. Characters
disallowed in variable names become underscores `_`. When reading from
standard input, xxd only emits the octets. Unless the new-ish `-n` name
option is given, in which case that becomes the variable name. This
remains popular because, [`#embed`][cppref] notwithstanding, as of this
writing all major toolchains remain stubborn about embedding data on their
own.

### The case for replacement

The idea of replacing it began with backporting the `-n` name option to
Vim 9.0 xxd. The feature did not appear in a release until a year ago, 28
years after `-i`, despite its obviousness. I've also felt that `xxd` is
slower than it could be, and a momentary examination reveals it's buggier
than it ought to be. [As expected][fuzz], a few seconds of fuzz testing
`xxd -r` reveals bugs, and it doesn't even require writing a single line
of code:

    $ afl-gcc -fsanitize=address,undefined xxd.c
    $ mkdir inputs
    $ echo >inputs/sample
    $ afl-fuzz -i inputs/ -o fuzzout/ ./a.out -r

The Windows port is lacking in the usual ways, unable to handle Unicode
paths. The new Vim 9.1 xxd `-R` color feature broke the Windows port, and
if w64devkit included Vim 9.1 then I'd need to patch out the new bugs. As
demonstrated above, at least it's trivial to compile! It's a single source
file, `xxd.c`, and requires no configuration. I love that.

The more I looked, the more problems I found. It's not doing anything
terribly complex, so I expected it wouldn't be difficult to rewrite it
with a better foundation. So I did. Ignoring tests and documentation, my
rewrite is about twice as long. In exchange, it's *substantially faster*:

    $ dd if=/dev/urandom of=bigfile bs=1M count=64

    $ time orig-xxd bigfile dump
    real    0m 4.40s
    user    0m 2.89s
    sys     0m 1.46s

    $ time rexxd bigfile dump
    real    0m 0.31s
    user    0m 0.07s
    sys     0m 0.21s

Same in reverse:

    $ time orig-xxd -r dump nul
    real    0m 5.81s
    user    0m 5.67s
    sys     0m 0.07s

    $ time rexxd -r dump nul
    real    0m 0.33s
    user    0m 0.23s
    sys     0m 0.09s

Or embedding data with rexxd:

    $ time orig-xxd -i bigfile bigfile.c
    real    0m 10.32s
    user    0m 9.85s
    sys     0m 0.37s

    $ time rexxd -i bigfile bigfile.c
    real    0m 0.40s
    user    0m 0.07s
    sys     0m 0.34s

I wanted to keep it portable and simple, so that's without [fancy SIMD
processing][simd]. Just [SWAR parsing][swar], [branch avoidance][branch],
no division on hot paths, and sound architecture. I also optimized for the
typical case at the cost of the atypical case. It's a little unfair to
compare it to a program probably first written on a 16-bit machine, but
there was time for it to pick up these techniques over the decades, too.

Unicode support works well:

    $ cat π
    3.14159265358979323846264338327950288419716939937510582097494
    $ rexxd -i π π.c

Producing this source with Unicode variables:

```c
unsigned char π[] = {
  0x33, 0x2e, 0x31, 0x34, 0x31, 0x35, 0x39, 0x32, 0x36, 0x35, 0x33, 0x35,
  // ...
  0x34, 0x0a
};
unsigned int π_len = 62;
```

Whereas the original xxd on Windows has the [usual CRT problems][cmd]:

    $ orig-xxd -i π
    orig-xxd: p: No such file or directory

It also struggles with 64-bit offsets, particularly on 32-bit hosts and
LLP64 hosts like Windows. In contrast, I designed rexxd to robustly
process file offsets as 64-bit on all hosts. Its tests operate on a
virtual file system with virtual files at those sizes, so those paths
really have been tested, too.

The original xxd only uses static allocation, which places small range
limits on the configuration:

    $ orig-xxd -c 1000
    orig-xxd: invalid number of columns (max. 256).

In rexxd everything is [arena allocated][arena] of course, and options are
limited only by the available memory, so the above, and more, would work.
The arena helps make the SWAR tricks possible, too, providing a fast
runway to load more data at a time.

While reverse engineering the original, I documented bugs I discovered and
noted them with a `BUG:` comment if you wanted to see more. I'm not aiming
for bug compatibility, so these are not present in rexxd.

### Platform layer

The [xxd man page][man] suggests using strace to examine the execution of
`-r` reverse. That is, to monitor the seeks and writes of a binary patch
in order to debug it. That's so insightful that I decided to build that as
a new `-x` option (think `sh -x`). That is, *rexxd has a built-in strace
on all platforms!* The trace is expressed in terms of unix system calls,
even on Windows:

    $ printf '00:41 \n02:42 \n04:43' | rexxd -x -r - data.bin
    open("data.bin", O_CREAT|O_WRONLY, 0666) = 1
    read(0, ..., 4096) = 19
    write(1, "A", 1) = 1
    lseek(1, 2, SEEK_SET) = 2
    read(0, ..., 4096) = 0
    write(1, "B", 1) = 1
    lseek(1, 4, SEEK_SET) = 4
    write(1, "C", 1) = 1
    exit(0) = ?

Is this doing some kind of self-[ptrace][] debugger voodoo? Nope. Like
[u-config][], it has a *platform layer*, and it simply logs the platform
layer calls — except for the trace printout itself of course. While the
intention is to debug binary patches, it was also quite insightful in
examining rexxd itself. It helped me spot that rexxd flushed more often
than strictly necessary.

To port rexxd to any system, define `Plt` as needed, implement these five
`plt_` functions, then call `xxd`. The five functions mostly have the
expected unix-like semantics:

```c
typedef struct Plt Plt;
b32  plt_open(Plt *, i32 fd, u8 *path, b32 trunc, Arena *);
i64  plt_seek(Plt *, i32 fd, i64 off, i32 whence);
i32  plt_read(Plt *, u8 *buf, i32 len);
b32  plt_write(Plt *, i32 fd, u8 *buf, i32 len);
void plt_exit(Plt *, i32);
i32  xxd(i32 argc, u8 **argv, Plt *, byte *heap, iz heapsize);
```

If the platform wants these functions to be "virtual" then it can put
function pointers in the `Plt` struct. Otherwise it stores anything it
might need in `Plt`. Global variables are never necessary. The application
layer doesn't use the standard library except (indirectly) `memset` and
`memcpy`, and it allocates everything it uses from the provided `heap`
parameter.

`plt_open` is a little unusual in that it picks the file descriptor: 0 to
replace standard input, or 1 to replace standard output. All platforms
currently use a virtual file descriptor table, and these do not map onto
the real process file descriptors. But they could! Calls are straced in
the application layer, so they log virtual file descriptors as seen by
rexxd. The arena parameter offers scratch space for the Windows platform
layer to convert paths from narrow to wide for `CreateFileW`, so it can
handle [long path names][long] with ease.

`plt_read` doesn't accept a file descriptor because there's only one from
which to read, 0. `plt_write` on the other hand allows writing to standard
error, 2.

`plt_exit` doesn't return, of course. In tests it [longjmps][setjmp] back
to the top level, as though returning from `xxd` with a status. This lets
me skip allocation null pointer checks, with OOM unwinding safely back to
the top level. Since rexxd allocates everything from the arena, it's all
automatically deallocated, so it's a clean exit.

On Windows, `plt_seek` calls [`SetFilePointerEx`][sfpe]. I learned the
hard way that the behavior of calling it on a non-file is undefined, not
an error, so at least one `GetFileType` call is mandatory. I also learned
that Windows will successfully seek all the way to `INT64_MAX`. If the
file system doesn't support that offset, it's a write failure *later*. For
correct operation, rexxd must take care not to overflow its own internal
file position tracking near these offsets with Windows allowing seeks to
operate at the edge until the first flush. Tests run on a virtual file
system thanks to the platform layer, and some tests permit huge seeks and
simulate impossibly enormous files in order to probe behavior at the
extremes.

This is in contrast to Linux, where seeks beyond the underlying file
system's supported file size is a seek error. For example, on ext4 with
the default configuration:

    $ echo ffffffff000: 00 | rexxd -x -r - somefile
    open("somefile", O_CREAT|O_WRONLY, 0666) = 1
    read(0, ..., 4096) = 16
    lseek(1, 17592186040320, SEEK_SET) = 17592186040320
    read(0, ..., 4096) = 0
    write(1, "\0", 1) = -1
    exit(3) = ?

We can see the seek succeeded then the write failed because it went one
byte beyond the file system limit. While seeking one byte further will
cause the seek to fail (22 `EINVAL`), and rexxd falls back on write until
it fills the storage and runs out of space:

    $ echo ffffffff001: 00 | rexxd -x -r - somefile
    open("somefile", O_CREAT|O_WRONLY, 0666) = 1
    read(0, ..., 4096) = 16
    lseek(1, 17592186040321, SEEK_SET) = -1
    write(1, "\0\0\0\0\0\0...\0\0\0\0\0\0", 4096) = 4096
    write(1, "\0\0\0\0\0\0...\0\0\0\0\0\0", 4096) = 4096
    ...

Mostly for fun, I wrote a libc-free platform layer using [raw Linux system
calls][raw], and it maps *almost* perfectly onto the kernel interface:

```c
struct Plt { int fds[3]; };

b32 plt_open(Plt *plt, i32 fd, u8 *path, b32 trunc, Arena *)
{
    i32 mode = fd ? O_CREAT|O_WRONLY : 0;
    mode |= trunc ? O_TRUNC : 0;
    plt->fds[fd] = (i32)syscall3(SYS_open, (uz)path, mode, 0666);
    return plt->fds[fd] >= 0;
}

i64 plt_seek(Plt *plt, i32 fd, i64 off, i32 whence)
{
    return syscall3(SYS_lseek, plt->fds[fd], off, whence);
}

i32 plt_read(Plt *plt, u8 *buf, i32 len)
{
    return (i32)syscall3(SYS_read, plt->fds[0], (uz)buf, len);
}

b32 plt_write(Plt *plt, i32 fd, u8 *buf, i32 len)
{
    return len == syscall3(SYS_write, plt->fds[fd], (uz)buf, len);
}

void plt_exit(Plt *, i32 r)
{
    syscall3(SYS_exit, r, 0, 0);
}
```

On Windows I use the [artisanal function prototypes][win32] of which I've
grown so fond. It's also my first time using w64devkit's `-lmemory` in a
serious application. I'm using [`-lchkstk`][chkstk] in the "xxd as a DLL"
platform layer, too, but that one's just a toy. In that one I use `alloca`
to allocate an arena, which is a rather novel combination, and the large
stack frame requires a stack probe. Otherwise none of rexxd requires stack
probes.

w64devkit's new `xxd.exe` is delightfully tidy as viewed by [peports][]:

    $ du -h xxd.exe
    28.0K   xxd.exe
    $ peports xxd.exe
    KERNEL32.dll
            0       CreateFileW
            0       ExitProcess
            0       GetCommandLineW
            0       GetFileType
            0       GetStdHandle
            0       MultiByteToWideChar
            0       ReadFile
            0       SetFilePointerEx
            0       VirtualAlloc
            0       WideCharToMultiByte
            0       WriteFile
    SHELL32.dll
            0       CommandLineToArgvW

### Other notes

[Buffered output][buf] and buffered input is custom tailored for rexxd.
When parsing line-oriented input, like `-r`, it attempts to parse from of
a *view* of the input buffer, no copying. The view is the [usual string
representation][str]:

    typedef struct {
        u8 *data;
        iz  len;
    } Str;

Does it fail if the line is longer than the buffer? If it straddles reads,
does that hurt efficiency? The answer to both is "no" due to the spillover
arena. `Input` is the buffered input struct, and here's the interface to
get the next line:

    Str nextline(Input *, Arena *);

If the line isn't entirely contained in the input buffer, the complete
line is [concatenated][concat] in the arena. So it comfortably handles
huge lines while no-copy optimizing for typical short, non-straddling
lines. With a per-iteration arena, any arena-backed line is automatically
freed at the end of the iteration, so it's all transparent:

```c
    for (;;) {
        Arena scratch = perm;
        Str line = nextline(b, &scratch);
        // ... line may point into an Input or scratch ...
    }
```

If the line doesn't fit in the arena, it triggers OOM handling. That is,
it calls `plt_exit` and something platform-appropriate happens without
returning. Beats the pants off [old `getline`][getline]!

I came up with a `maxof` macro that evaluates the maximum of any integral
type, signed or unsigned. It appears in [overflow checks][size] and more,
I really like how it turned out. For example:

```c
    if (pos > maxof(i64) - off) {
        // overflow
    }
    pos += off;
```

Or:

```c
i32 trunc32(iz n)
{
    return n>maxof(i32) ? maxof(i32) : (i32)n;
}
```

Now that I have `-lmemory` and generally solved string function issues for
myself, I leaned into `__builtin_memset` and `__builtin_memcpy` for this
project. Despite `restrict`, it's surprisingly difficult to get compilers
to optimize loops into semantically equivalent string function calls. An
explicit built-in solves that. It also produces faster debug builds, which
is what I run while I work. At `-O0`, rexxd is about half the speed of a
release build.

Other than `-x`, I don't plan on inventing new features. I'd like to
maintain compatibility with the `xxd` found everywhere else, and I don't
expect adoption beyond w64devkit. Overall the project took about twice as
long as I anticipated — two weekends instead of one — but it turned out
better than I expected and I'm very pleased with the results.


[arena]: /blog/2023/09/27/
[branch]: /blog/2017/10/06/
[buf]: /blog/2023/02/13/
[cache]: https://utcc.utoronto.ca/~cks/space/blog/unix/UnifiedBufferCache
[chkstk]: /blog/2024/02/05/
[cmd]: /blog/2021/12/30/
[concat]: /blog/2024/05/25/
[cppref]: https://en.cppreference.com/w/c/preprocessor/embed
[embed]: /blog/2016/11/15/
[fuzz]: /blog/2025/02/05/
[getline]: https://man7.org/linux/man-pages/man3/getline.3.html
[long]: https://learn.microsoft.com/en-us/windows/win32/fileio/maximum-file-path-limitation
[make]: /blog/2016/04/30/
[man]: https://manpages.debian.org/bookworm/xxd/xxd.1.en.html
[peports]: /blog/2024/06/30/
[ptrace]: /blog/2018/06/23/
[raw]: /blog/2023/03/23/
[rexxd]: https://github.com/skeeto/w64devkit/blob/master/src/rexxd.c
[setjmp]: /blog/2023/02/12/
[sfpe]: https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-setfilepointerex
[simd]: /blog/2021/12/04/
[size]: /blog/2024/05/24/
[str]: /blog/2025/01/19/
[swar]: /blog/2022/04/30/
[u-config]: /blog/2023/01/18/
[w64devkit]: https://github.com/skeeto/w64devkit
[win32]: /blog/2023/05/31/
