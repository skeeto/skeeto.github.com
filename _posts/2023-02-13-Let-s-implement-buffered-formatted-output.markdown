---
title: "Let's implement buffered, formatted output"
layout: post
date: 2023-02-13T00:00:00Z
tags: [c]
uuid: 4a4af83f-4fd8-4b3b-99aa-089d01f90fad
excerpt_separator: <!--more-->
---

*This article was discussed [on reddit][r].*

When [not using the C standard library][libc], how does one deal with
formatted output? Re-implementing the entirety of `printf` from scratch
seems like a lot of work, and indeed it would be. Fortunately it's rarely
necessary. With the right mindset, and considering your program's *actual*
formatting needs, it's not as difficult as it might appear. Since it goes
hand-in-hand with buffering, I'll cover both topics at once, including
`sprintf`-like capabilities, which is where we'll start.

<!--more-->

### The print-is-append mindset

Buffering amortizes the costs of write (and read) system calls. Many small
writes are queued via the buffer into a few large writes. This isn't just
an implementation detail. It's key in the mindset to tackle formatted
output: **Printing is appending.**

The mindset includes the reverse: *Appending is like printing*. Consider
this next time you reach for `strcat` or similar. Is this the appropriate
destination for this data, or am I just going to print it — i.e. append it
to another, different buffer — afterward?

This concept may sound obvious, but consider that there are major, popular
programming paradigms where the norm is otherwise. I'll pick on Python to
illustrate, but it's not alone.

```py
print(f"found {count} items")
```

This line of code allocates a buffer; formats the value of the variable
`count` into it; allocates a second buffer; copies into it the prefix
(`"found "`), the first buffer, and the suffix (`" items"`); copies the
contents of this second buffer into the standard output buffer; then
discards the two temporary buffers. To see for yourself, use the [CPython
bytecode disassembler][dis] on it. (It *is* pretty neat that string
formatting is partially implemented in the compiler and partially parsed
at compile time.)

With the print-is-append mindset, you know it's ultimately being copied
into the standard output buffer, and that you can skip the intermediate
appending and copying. Avoiding that pessimization isn't just about the
computer's time, it's even more about saving your own time implementing
formatted output.

In C that line looks like:

```c
printf("found %d items\n", count);
```

The format string is a domain-specific language (DSL) that is (usually)
parsed and evaluated at run time. In essence it's a little program that
says:

1. Append `"found "` to the output buffer
2. Format the given integer into the output buffer
3. Append `" items\n"` to the output buffer

For `sprintf` the output buffer is caller-supplied instead of a buffered
stream.

In this implementation we're doing to skip the DSL and express such
"format programs" in C itself. It's more verbose at the call site, but it
simplifies the implementation. As a bonus, it's also faster since the
format program is itself compiled by the C compiler. In your own formatted
output implementation you could write a `printf` that, following the
format string, calls the append primitives we'll build below.

### Buffer implementation

Let's begin by defining an output buffer. An output buffer tracks the
total capacity and how much has been written. I'll include a sticky error
flag to simplify error checks. For a first pass we'll start with a
`sprintf` rather than full-blown `printf` because there's nowhere yet for
the data to go.

```c
#define MEMBUF(buf, cap) {buf, cap, 0, 0}
struct buf {
    unsigned char *buf;
    int cap;
    int len;
    _Bool error;
};
```

I'm using `unsigned char` since these are *bytes*, best understood as
unsigned (0–255), particularly important when dealing with encodings. I
also wrote a "constructor" macro, `MEMBUF`, to help with initialization.
Next we need a function to append bytes — the core operation:

```c
void append(struct buf *b, unsigned char *src, int len)
{
    int avail = b->cap - b->len;
    int amount = avail<len ? avail : len;
    for (int i = 0; i < amount; i++) {
        b->buf[b->len+i] = src[i];
    }
    b->len += amount;
    b->error |= amount < len;
}
```

If there wasn't room, it copies as much as possible and sets the error
flag to indicate truncation. It doesn't return the error. Rather than
check after each append, the caller will check after multiple appends,
effectively batching the checks into one check. The typical, expected case
is that there is no error, so make that path fast.

Since it's an easy point to miss: `append` is the only place in the entire
implementation where bounds checking comes into play. Everything else can
confidentially throw bytes at the buffer without worrying if it fits. If
it doesn't, the sticky error flag will indicate such at a more appropriate
time.

I could have used `memcpy` for the loop, but the goal is not to use libc.
Besides, not using `memcpy` means we can pass a null pointer without
making it a special exception.

```c
append(b, 0, 0);  // append nothing (no-op)
```

I expect that static strings are common sources for append, so I'll add a
helper macro which gets the length as a compile-time constant. The null
terminator will not be used.

```c
#define APPEND_STR(b, s) append(b, s, sizeof(s)-1)
```

If that's not clear yet, it will be once you see an example. It's also
useful to append single bytes:

```c
void append_byte(struct buf *b, unsigned char c)
{
    append(b, &c, 1);
}
```

With primitive appends done, we can build ever "higher-level" appends. For
example, to append a formatted `long` to the buffer:

```c
void append_long(struct buf *b, long x)
{
    unsigned char tmp[64];
    unsigned char *end = tmp + sizeof(tmp);
    unsigned char *beg = end;
    long t = x>0 ? -x : x;
    do {
        *--beg = '0' - t%10;
    } while (t /= 10);
    if (x < 0) {
        *--beg = '-';
    }
    append(b, beg, end-beg);
}
```

By working from the negative end — recall that the negative range is
larger than the positive — it supports the full range of signed `long`,
whatever it happens to be on this host. With less than 50 lines of code we
now have enough to format the example:

```c
char message[256];
struct buf b = MEMBUF(message, sizeof(message));

APPEND_STR(&b, "found ");
append_long(&b, count);
APPEND_STR(&b, "items\n");
if (b.error) {
    // truncated
}
```

We can continue defining append functions for whatever types we need.

```c
void append_ptr(struct buf *b, void *p)
{
    APPEND_STR(b, "0x");
    uintptr_t u = (uintptr_t)p;
    for (int i = 2*sizeof(u) - 1; i >= 0; i--) {
        append_byte(b, "0123456789abcdef"[(u>>(4*i))&15]);
    }
}

struct vec2 { int x, y; };

void append_vec2(struct buf *b, struct vec2 v)
{
    APPEND_STR(&b, "vec2{");
    append_long(&b, v.x);
    APPEND_STR(&b, ", ");
    append_long(&b, v.y);
    append_byte(&b, '}');
}
```

Perhaps you want features like field width? Add a parameter for it… but
only if you need it!

### Float formatting

As mentioned before, [precise float formatting is challenging][dtoa]
because it's full of edge cases. However, if you only need to output a
simple format at reduced precision, it's not difficult. To illustrate,
this nearly matches `%f`, built atop `append_long`:

```c
void append_double(struct buf *b, double x)
{
    long prec = 1000000;  // i.e. 6 decimals

    if (x < 0) {
        append_byte(b, '-');
        x = -x;
    }

    x += 0.5 / prec;  // round last decimal
    if (x >= (double)(-1UL>>1)) {  // out of long range?
        APPEND_STR(b, "inf");
    } else {
        long integral = x;
        long fractional = (x - integral)*prec;
        append_long(b, integral);
        append_byte(b, '.');
        for (long i = prec/10; i > 1; i /= 10) {
            if (i > fractional) {
                append_byte(b, '0');
            }
        }
        append_long(b, fractional);
    }
}
```

### Output to a handle

So far this writes output to a buffer and truncates when it runs out of
space. Usually we want this going to a sink, like a kernel object whether
that be a file, pipe, socket, etc. to which we have a handle like a file
descriptor. Instead of truncating, we *flush* the buffer to this sink, at
which point there's room for more output. The error flag is set if the
flush fails, but this is essentially the same concept as before.

In these examples I will use a file descriptor `int`, but you can use
whatever sort of handle is appropriate. I'll add an `fd` field to the
buffer and a new constructor macro:

```c
#define MEMBUF(buf, cap) {buf, cap, 0, -1, 0}
#define FDBUF(fd, buf, cap) {buf, cap, 0, fd, 0}

struct buf {
    unsigned char *buf;
    int cap;
    int len;
    int fd;
    Bool error;
};
```

The buffered stream will be polymorphic: Output can go to a memory buffer
or to an operating system handle using the same append interface. This is
a handy feature standard C doesn't even have, though POSIX does in the
form of [`fmemopen`][fmemopen]. Nothing else changes except `append`,
which, if given a valid handle, will flush when full. Attempting to flush
a memory buffer sets the error flag.

```c
_Bool os_write(int fd, void *, int);

void flush(struct buf *b)
{
    b->error |= b->fd < 0;
    if (!b->error && b->len) {
        b->error |= !os_write(b->fd, b->buf, b->len);
        b->len = 0;
    }
}
```

I've arranged so that output stops when there's an error. Also I'm using a
hypothetical `os_write` in the platform layer as a full, unbuffered write.
Note that unix `write(2)` experiences partial writes and so must be used
in a loop. Win32 `WriteFile` doesn't have partial writes, so on Windows an
`os_write` could pass its arguments directly to the operating system.

The program will need to call `flush` directly when it's done writing
output, or to display output early, e.g. line buffering. In `append` we'll
use a loop to continue appending and flushing until the input is consumed
or an error occurs.

```c
void append(struct buf *b, unsigned char *src, int len)
{
    unsigned char *end = src + len;
    while (!b->error && src<end) {
        int left = end - src;
        int avail = b->cap - b->len;
        int amount = avail<left ? avail : left;

        for (int i = 0; i < amount; i++) {
            b->buf[b->len+i] = src[i];
        }
        b->len += amount;
        src += amount;

        if (amount < left) {
            flush(b);
        }
    }
}
```

That completes formatted output! We can now do stuff like:

```c
int main(void)
{
    unsigned char mem[1<<10];  // arbitrarily-chosen 1kB buffer
    struct buf stdout = FDBUF(1, mem, sizeof(mem));
    for (long i = 0; i < 1000000; i++) {
        APPEND_STR(&stdout, "iteration ");
        append_long(&stdout, i);
        append_byte(&stdout, '\n');
        // ...
    }
    flush(&stdout);
    return stdout.error;
}
```

Except for the lack of format DSL, this should feel familiar.


[dis]: https://docs.python.org/3/library/dis.html
[dtoa]: https://netlib.org/fp/dtoa.c
[fmemopen]: https://man7.org/linux/man-pages/man3/fmemopen.3.html
[libc]: /blog/2023/02/11/
[r]: https://old.reddit.com/r/C_Programming/comments/111238u/lets_implement_buffered_formatted_output/
