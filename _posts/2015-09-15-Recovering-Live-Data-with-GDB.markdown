---
title: Recovering Live Data with GDB
layout: post
date: 2015-09-15T14:53:44Z
tags: [c, cpp]
uuid: 5fa83dc1-2d5c-3313-b2b9-f4fb73ef5d9e
---

I recently ran into a problem where [long-running program][long]
output was trapped in a C `FILE` buffer. The program had been running
for two days straight printing its results, but the last few kilobytes
of output were missing. It wouldn't output these last bytes until the
program completed its day-long (or worse!) cleanup operation and
exited. This is easy to fix — and, honestly, the cleanup step was
unnecessary anyway — but I didn't want to start all over and wait
two more days to recompute the result.

Here's a minimal example of the situation. The first loop represents
the long-running computation and the infinite loop represents a
cleanup job that will never complete.

~~~c
#include <stdio.h>

int
main(void)
{
    /* Compute output. */
    for (int i = 0; i < 10; i++)
        printf("%d/%d ", i, i * i);
    putchar('\n');

    /* "Slow" cleanup operation ... */
    for (;;)
        ;
    return 0;
}
~~~

### Buffered Output Review

Both `printf` and `putchar` are C library functions and are usually
buffered in some way. That is, each call to these functions doesn't
necessarily send data out of the program. This is in contrast to the
POSIX functions `read` and `write`, which are unbuffered system calls.
Since system calls are relatively expensive, buffered input and output
is used to change a large number of system calls on small buffers into
a single system call on a single large buffer.

Typically, stdout is *line-buffered* if connected to a terminal. When
the program completes a line of output, the user probably wants to see
it immediately. So, if you compile the example program and run it at
your terminal you will probably see the output before the program
hangs on the infinite loop.

    $ cc -std=c99 example.c
    $ ./a.out
    0/0 1/1 2/4 3/9 4/16 5/25 6/36 7/49 8/64 9/81

However, when stdout is connected to a file or pipe, it's generally
buffered to something like 4kB. For this program, the output will
remain empty no matter how long you wait. It's trapped in a `FILE`
buffer in process memory.

    $ ./a.out > output.txt

The primary way to fix this is to use the `fflush` function, to force
the buffer empty before starting a long, non-output operation.
Unfortunately for me I didn't think of this two days earlier.

### Debugger to the Rescue

Fortunately there *is* a way to interrupt a running program and
manipulate its state: a debugger. First, find the process ID of the
running program (the one writing to `output.txt` above).

    $ pgrep a.out
    12934

Now attach GDB, which will pause the program's execution.

    $ gdb ./a.out
    Reading symbols from ./a.out...(no debugging symbols found)...done.
    gdb> attach 12934
    Attaching to program: /tmp/a.out, process 12934
    ... snip ...
    0x0000000000400598 in main ()
    gdb>

From here I could examine the stdout `FILE` struct and try to extract
the buffer contents by hand. However, the easiest thing is to do is
perform the call I forgot in the first place: `fflush(stdout)`.

    gdb> call fflush(stdout)
    $1 = 0
    gdb> quit
    Detaching from program: /tmp/a.out, process 12934

The program is still running, but the output has been recovered.

    $ cat output.txt
    0/0 1/1 2/4 3/9 4/16 5/25 6/36 7/49 8/64 9/81

### Why Cleanup?

As I said, in my case the cleanup operation was entirely unnecessary,
so it would be safe to just kill the program at this point. It was
taking a really long time to tear down a humongous data structure (on
the order of 50GB) one little node at a time with `free`. Obviously,
the memory would be freed much more quickly by the OS when the program
exited.

Freeing memory in the program was only to satisfy [Valgrind][vg],
since it's so incredibly useful for debugging. Not freeing the data
structure would hide actual memory leaks in Valgrind's final report.
For the real "production" run, I should have disabled cleanup.


[long]: https://github.com/skeeto/reddit-related
[vg]: http://valgrind.org/
