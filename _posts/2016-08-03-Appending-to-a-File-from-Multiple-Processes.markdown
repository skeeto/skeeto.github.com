---
title: Appending to a File from Multiple Processes
layout: post
date: 2016-08-03T16:17:44Z
tags: [c, linux, posix, win32]
uuid: 93473b6d-3be3-3d0c-d7d5-6ad485c1e9a0
---

Suppose you have multiple processes appending output to the same file
without explicit synchronization. These processes might be working in
parallel on different parts of the same problem, or these might be
threads blocked individually reading different external inputs. There
are two concerns that come into play:

1) **The append must be atomic** such that it doesn't clobber previous
    appends by other threads and processes. For example, suppose a
    write requires two separate operations: first moving the file
    pointer to the end of the file, then performing the write. There
    would be a race condition should another process or thread
    intervene in between with its own write.

2) **The output will be interleaved.** The primary solution is to
   design the data format as atomic records, where the ordering of
   records is unimportant — like rows in a relational database. This
   could be as simple as a text file with each line as a record. The
   concern is then ensuring records are written atomically.

This article discusses processes, but the same applies to threads when
directly dealing with file descriptors.

### Appending

The first concern is solved by the operating system, with one caveat.
On POSIX systems, opening a file with the `O_APPEND` flag will
guarantee that [writes always safely append][write].

> If the `O_APPEND` flag of the file status flags is set, the file
> offset shall be set to the end of the file prior to each write and
> no intervening file modification operation shall occur between
> changing the file offset and the write operation.

However, this says nothing about interleaving. **Two processes
successfully appending to the same file will result in all their bytes
in the file in order, but not necessarily contiguously.**

The caveat is that not all filesystems are POSIX-compatible. Two
famous examples are NFS and the Hadoop Distributed File System (HDFS).
On these networked filesystems, appends are simulated and subject to
race conditions.

On POSIX systems, fopen(3) with the `a` flag [will use
`O_APPEND`][fopen], so you don't necessarily need to use open(2). On
Linux this can be verified for any language's standard library with
strace.

~~~c
#include <stdio.h>

int main(void)
{
    fopen("/dev/null", "a");
    return 0;
}
~~~

And the result of the trace:

    $ strace -e open ./a.out
    open("/dev/null", O_WRONLY|O_CREAT|O_APPEND, 0666) = 3

For Win32, the equivalent is the `FILE_APPEND_DATA` access right, and
similarly [only applies to "local files."][rights]

### Interleaving and Pipes

The interleaving problem has two layers, and gets more complicated the
more correct you want to be. Let's start with pipes.

On POSIX, a pipe is unseekable and doesn't have a file position, so
appends are the only kind of write possible. When writing to a pipe
(or FIFO), writes less than the system-defined `PIPE_BUF` are
guaranteed to be atomic and non-interleaving.

> Write requests of `PIPE_BUF` bytes or less shall not be interleaved
> with data from other processes doing writes on the same pipe. Writes
> of greater than `PIPE_BUF` bytes may have data interleaved, on
> arbitrary boundaries, with writes by other processes, [...]

The minimum value for `PIPE_BUF` for POSIX systems is 512 bytes. On
Linux it's 4kB, and on other systems [it's as high as 32kB][pipe-buf].
As long as each record is less than 512 bytes, a simple write(2) will
due. None of this depends on a filesystem since no files are involved.

If more than `PIPE_BUF` bytes isn't enough, the POSIX writev(2) can be
used to [atomically write up to `IOV_MAX` buffers][writev] of
`PIPE_BUF` bytes. The minimum value for `IOV_MAX` is 16, but is
typically 1024. This means the maximum safe atomic write size for
pipes — and therefore the largest record size — for a perfectly
portable program is 8kB (16✕512). On Linux it's 4MB.

That's all at the system call level. There's another layer to contend
with: buffered I/O in your language's standard library. Your program
may pass data in appropriately-sized pieces for atomic writes to the
I/O library, but it may be undoing your hard work, concatenating all
these writes into a buffer, splitting apart your records. For this
part of the article, I'll focus on single-threaded C programs.

Suppose you're writing a simple space-separated format with one line
per record.

~~~c
int foo, bar;
float baz;
while (condition) {
    // ...
    printf("%d %d %f\n", foo, bar, baz);
}
~~~

Whether or not this works depends on how `stdout` is buffered. C
standard library streams (`FILE *`) have three buffering modes:
unbuffered, line buffered, and fully buffered. Buffering is configured
through setbuf(3) and setvbuf(3), and the initial buffering state of a
stream depends on various factors. For buffered streams, the default
buffer is at least `BUFSIZ` bytes, itself at least 256 (C99
§7.19.2¶7). Note: threads share this buffer.

Since each record in the above program easily fits inside 256 bytes,
if stdout is a line buffered pipe then this program will interleave
correctly on any POSIX system without further changes.

If instead your output is comma-separated values (CSV) and [your
records may contain new line characters][csv], there are two
approaches. In each, the record must still be no larger than
`PIPE_BUF` bytes.

* Unbuffered pipe: construct the record in a buffer (i.e. sprintf(3))
  and output the entire buffer in a single fwrite(3). While I believe
  this will always work in practice, it's not guaranteed by the C
  specification, which defines fwrite(3) as a series of fputc(3) calls
  (C99 §7.19.8.2¶2).

* Fully buffered pipe: set a sufficiently large stream buffer and
  follow each record with a fflush(3). Unlike fwrite(3) on an
  unbuffered stream, the specification says the buffer will be
  "transmitted to the host environment as a block" (C99 §7.19.3¶3),
  so this should be perfectly correct on any POSIX system.

If your situation is more complicated than this, you'll probably have
to bypass your standard library buffered I/O and call write(2) or
writev(2) yourself.

#### Practical Application

If interleaving writes to a pipe stdout sounds contrived, here's the
real life scenario: GNU xargs with its `--max-procs` (`-P`) option to
process inputs in parallel.

    xargs -n1 -P$(nproc) myprogram < inputs.txt | cat > outputs.csv

The `| cat` ensures the output of each `myprogram` process is
connected to the same pipe rather than to the same file.

A non-portable alternative to `| cat`, especially if you're
dispatching processes and threads yourself, is the splice(2) system
call on Linux. It efficiently moves the output from the pipe to the
output file without an intermediate copy to userspace. GNU Coreutils'
cat doesn't use this.

#### Win32 Pipes

On Win32, [anonymous pipes][anon] have no semantics regarding
interleaving. [Named pipes][named] have per-client buffers that
prevent interleaving. However, the pipe buffer size is unspecified,
and requesting a particular size is only advisory, so it comes down to
trial and error, though the unstated limits should be comparatively
generous.

### Interleaving and Files

Suppose instead of a pipe we have an `O_APPEND` file on POSIX. Common
wisdom states that the same `PIPE_BUF` atomic write rule applies.
While this often works, especially on Linux, this is not correct. The
POSIX specification doesn't require it and [there are systems where it
doesn't work][test].

If you know the particular limits of your operating system *and*
filesystem, and you don't care much about portability, then maybe you
can get away with interleaving appends. For full portability, pipes
are required.

On Win32, writes on local files up to the underlying drive's sector
size (typically 512 bytes to 4kB) are atomic. Otherwise the only
options are deprecated Transactional NTFS (TxF), or manually
synchronizing your writes. All in all, it's going to take more work to
get correct.

### Conclusion

My true use case for mucking around with clean, atomic appends is to
compute giant CSV tables in parallel, with the intention of later
loading into a SQL database (i.e. SQLite) for analysis. A more robust
and traditional approach would be to write results directly into the
database as they're computed. But I like the platform-neutral
intermediate CSV files — good for archival and sharing — and the
simplicity of programs generating the data — concerned only with
atomic write semantics rather than calling into a particular SQL
database API.


[write]: http://pubs.opengroup.org/onlinepubs/9699919799/functions/write.html
[pipe-buf]: http://ar.to/notes/posix
[test]: http://www.notthewizard.com/2014/06/17/are-files-appends-really-atomic/
[rights]: https://msdn.microsoft.com/en-us/library/windows/desktop/gg258116(v=vs.85).aspx
[fopen]: http://pubs.opengroup.org/onlinepubs/9699919799/functions/fopen.html
[writev]: http://pubs.opengroup.org/onlinepubs/9699919799/functions/writev.html
[csv]: https://tools.ietf.org/html/rfc4180
[named]: https://msdn.microsoft.com/en-us/library/windows/desktop/aa365150(v=vs.85).aspx
[anon]: https://msdn.microsoft.com/en-us/library/windows/desktop/aa365152(v=vs.85).aspx
