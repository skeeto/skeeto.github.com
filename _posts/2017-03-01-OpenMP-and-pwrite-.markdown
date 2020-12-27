---
title: OpenMP and pwrite()
layout: post
date: 2017-03-01T21:22:24Z
tags: [c, posix, win32, optimization]
uuid: dfdf8ca6-51aa-3a15-6bf0-98b39f20652a
---

The most common way I introduce multi-threading to [small C
programs][simd] is with OpenMP (Open Multi-Processing). It's typically
used as compiler pragmas to parallelize computationally expensive
loops — iterations are processed by different threads in some
arbitrary order.

Here's an example that computes the [frames of a video][video] in
parallel. Despite being computed out of order, each frame is written
in order to a large buffer, then written to standard output all at
once at the end.

~~~c
size_t size = sizeof(struct frame) * num_frames;
struct frame *output = malloc(size);
float beta = DEFAULT_BETA;

/* schedule(dynamic, 1): treat the loop like a work queue */
#pragma omp parallel for schedule(dynamic, 1)
for (int i = 0; i < num_frames; i++) {
    float theta = compute_theta(i);
    compute_frame(&output[i], theta, beta);
}

write(STDOUT_FILENO, output, size);
free(output);
~~~

Adding OpenMP to this program is much simpler than introducing
low-level threading semantics with, say, Pthreads. With care, there's
often no need for explicit thread synchronization. It's also fairly
well supported by many vendors, even Microsoft (up to OpenMP 2.0), so
a multi-threaded OpenMP program is quite portable without `#ifdef`.

There's real value this pragma API: **The above example would still
compile and run correctly even when OpenMP isn't available.** The
pragma is ignored and the program just uses a single core like it
normally would. It's a slick fallback.

When a program really *does* require synchronization there's
`omp_lock_t` (mutex lock) and the expected set of functions to operate
on them. This doesn't have the nice fallback, so I don't like to use
it. Instead, I prefer `#pragma omp critical`. It nicely maintains the
OpenMP-unsupported fallback.

~~~c
/* schedule(dynamic, 1): treat the loop like a work queue */
#pragma omp parallel for schedule(dynamic, 1)
for (int i = 0; i < num_frames; i++) {
    struct frame *frame = malloc(sizeof(*frame));
    float theta = compute_theta(i);
    compute_frame(frame, theta, beta);
    #pragma omp critical
    {
        write(STDOUT_FILENO, frame, sizeof(*frame));
    }
    free(frame);
}
~~~

This would append the output to some output file in an arbitrary
order. The critical section [prevents interleaving of
outputs][append].

There are a couple of problems with this example:

1. Only one thread can write at a time. If the write takes too long,
   other threads will queue up behind the critical section and wait.

2. The output frames will be out of order, which is probably
   inconvenient for consumers. If the output is seekable this can be
   solved with `lseek()`, but that only makes the critical section
   even more important.

There's an easy fix for both, and eliminates the need for a critical
section: POSIX `pwrite()`.

~~~c
ssize_t pwrite(int fd, const void *buf, size_t count, off_t offset);
~~~

It's like `write()` but has an offset parameter. Unlike `lseek()`
followed by a `write()`, multiple threads and processes can, in
parallel, safely write to the same file descriptor at different file
offsets. The catch is that **the output must be a file, not a pipe**.

~~~c
#pragma omp parallel for schedule(dynamic, 1)
for (int i = 0; i < num_frames; i++) {
    size_t size = sizeof(struct frame);
    struct frame *frame = malloc(size);
    float theta = compute_theta(i);
    compute_frame(frame, theta, beta);
    pwrite(STDOUT_FILENO, frame, size, size * i);
    free(frame);
}
~~~

There's no critical section, the writes can interleave, and the output
is in order.

If you're concerned about standard output not being seekable (it often
isn't), keep in mind that it will work just fine when invoked like so:

    $ ./compute_frames > frames.ppm

### Windows Portability

I talked about OpenMP being really portable, then used POSIX
functions. Fortunately the Win32 `WriteFile()` function has an
"overlapped" parameter that works just like `pwrite()`. Typically
rather than call either directly, I'd wrap the write like so:

~~~c
#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

static int
write_frame(struct frame *f, int i)
{
    HANDLE out = GetStdHandle(STD_OUTPUT_HANDLE);
    DWORD written;
    OVERLAPPED offset = {.Offset = sizeof(*f) * i};
    return WriteFile(out, f, sizeof(*f), &written, &offset);
}

#else /* POSIX */
#include <unistd.h>

static int
write_frame(struct frame *f, int i)
{
    size_t count = sizeof(*f);
    size_t offset = sizeof(*f) * i;
    return pwrite(STDOUT_FILENO, buf, count, offset) == count;
}
#endif
~~~

Except for switching to `write_frame()`, the OpenMP part remains
untouched.

### Real World Example

Here's an example in a real program:

[julia.c][gist]{: .download}

Notice because of `pwrite()` there's no piping directly into
`ppmtoy4m`:

    $ ./julia > output.ppm
    $ ppmtoy4m -F 60:1 < output.ppm > output.y4m
    $ x264 -o output.mp4 output.y4m

[output.mp4][alt]{: .download}

<video src="https://skeeto.s3.amazonaws.com/share/julia-256.mp4"
       controls="" loop="" crossorigin="anonymous">
</video>


[append]: /blog/2016/08/03/
[simd]: /blog/2015/07/10/
[video]: /blog/2011/11/28/
[gist]: https://gist.github.com/skeeto/d7e17bb2aa40907a3405c3933cb1f936
[alt]: /video/?v=julia-256
