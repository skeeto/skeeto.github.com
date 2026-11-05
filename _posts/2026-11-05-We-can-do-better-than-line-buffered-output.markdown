---
title: We can do better than line-buffered output
date: 2026-11-05T00:25:25Z
tags: [c]
uuid: b62a4635-27cc-8d16-8e4a-6e39942cdcc4
---

[Buffered output][bufio] typically comes in two flavors: fully buffered
and line buffered. If output goes to a terminal then the implementation
automatically selects line buffering, flushing its buffer when a line is
completed so that it reaches the user immediately. Otherwise it fully
buffers, holding output until the buffer is full or an explicit flush
event at some arbitrary point in the future. Line buffering is a common
bottleneck in non-interactive terminal applications such that it's worth
redirecting output to a file or pipe in order to avoid it. *It doesn't
have to be this way*. We can have our cake and eat it, too: promptly
delivering lines to a terminal at fully-buffered speeds.

For example, Info-ZIP `unzip` prints file names as it extract them from an
archive. This printout dominates extraction time of archives with many
small files. Sometimes I run `unzip` without options, then my terminal
floods with file names and I know that my extraction is bottlenecked. So I
cancel and restart with `-q` (quiet) to avoid the bottleneck, because it's
faster to start over than wait. Conscious of this experience, I avoided
the problem in [tugz][] `unzip` without changing the user interface. You
get a listing but you don't have to pay for it.

    $ unzip example.zip
    Archive:  example.zip
      inflating: example.bmp
      inflating: example.svg
      inflating: example.txt

Consider this C program that prints three lines:

```c
#include <stdio.h>

int main()
{
    puts("a\nb");
    puts("c");
}
```

If we examine the writes with, say, [strace][]:

    $ strace -o log -e write ./a.out
    a
    b
    c

We might see (here glibc):

```strace
write(1, "a\n", 2)                      = 2
write(1, "b\n", 2)                      = 2
write(1, "c\n", 2)                      = 2
```

Yes, glibc takes line buffering *quite literally*, even splitting writes
on a string. Some implementations are a little more sophisticated:

```strace
write(1, "a\nb\n", 2)                   = 4
write(1, "c\n", 2)                      = 2
```

CPython makes exactly those writes for this program:

```python
import sys

sys.stdout.write("a\nb\n")
sys.stdout.write("c\n")
```

Its rule is to flush if its buffer contains a newline after appending the
new output, with the consequence of possibly outputting partial lines.
Building on [the previous example][bufio]:

```c
enum {
    BUF_ERR  = 1 << 0,
    BUF_LINE = 1 << 1,
};

typedef struct {
    uint8_t  *buf;
    ptrdiff_t cap;
    ptrdiff_t len;
    int       fd;
    int       flags;
} Buf;

bool os_write(int fd, void *, ptrdiff_t);

void flush(Buf *b)
{
    if (!(b->flags & BUF_ERR) && b->len) {
        if (!os_write(b->fd, b->buf, b->len)) {
            b->flags |= BUF_ERR;
        }
        b->len = 0;
    }
}
```

Such line buffering might be achieved with a test-then-flush:

```c
bool has_newline(Buf *b)
{
    int r = 0;
    for (ptrdiff_t i = 0; i < b->len; i++) {
        r |= b->buf[i] == '\n';
    }
    return r;
}

void append(Buf *b, uint8_t *src, ptrdiff_t len)
{
    uint8_t *end = src + len;

    while (!(b->flags & BUF_ERR) && src<end) {
        ptrdiff_t left   = end - src;
        ptrdiff_t avail  = b->cap - b->len;
        ptrdiff_t amount = avail<left ? avail : left;

        memcpy(b->buf+b->len, src, amount);
        b->len += amount;
        src += amount;

        if (amount < left) {
            flush(b);  // make room
        }
    }

    // test-then-flush if line buffered
    if ((b->flags & BUF_LINE) && has_newline(b)) {
        flush(b);
    }
}
```

If we've got a bunch of lines to print and append them one after another
into the buffer, that's a lot of writes which could have been one.

### Eat all the cake

Let's think about our goal: If the output is a terminal then we don't want
to hold back lines indefinitely. That's not the same as a write on every
newline. There's already a solution for the problem where we're in a hurry
to send out data, but the underlying, *buffer-oblivious* caller makes lots
of small writes: [Nagle's algorithm][].

That is, **flush the buffer if it's been idle a short time**. We don't
actually care about newlines in particular. The catch is that it requires
tweaking the interface. Add a deadline to the buffer:

```c
typedef struct {
    int64_t   deadline;
    // ...
} Buf;

int64_t os_monotonic_clock();

void deadline_flush(Buf *b)
{
    if ((b->flags & BUF_LINE) && b->len) {
        int64_t now = os_monotonic_clock();
        if (b->deadline && now>b->deadline) {
            flush(b);
            b->deadline = 0;
        } else {
            b->deadline = now + DEADLINE;
        }
    }
}
```

Where `DEADLINE` is something appropriate for human perception, like 100ms
or so. We could poke this concurrently in a thread, awoken via futex when
data is written to the buffer. But better to poke it between short stints
of work, which is how tugz works. For example (using [my strings][str]):

```c
    for (/* filename in filenames */) {
        Str pre = S("  inflating: ");
        Str suf = S("\n");
        append(&stdout, pre.data, pre.len);
        append(&stdout, filename.data, filename.len);
        append(&stdout, suf.data, suf.len);

        while (/* not done inflating */) {
            // 1. inflate block
            // 2. write block
            deadline_flush(&stdout);
        }
    }
```

If it processes multiple files within the deadline, their names write out
together. No threads. The behavior difference compared to traditional
flush-on-newlines is imperceptible to a humans, aside from the fact their
program runs faster. One more reason not to bother with [crummy C
stdio][libc].


[Nagle's algorithm]: https://en.wikipedia.org/wiki/Nagle%27s_algorithm
[bufio]: /blog/2023/02/13/
[libc]: /blog/2023/02/11/
[str]: /blog/2025/01/19/#strings
[strace]: /blog/2018/06/23/
[tugz]: /blog/2026/10/13/
