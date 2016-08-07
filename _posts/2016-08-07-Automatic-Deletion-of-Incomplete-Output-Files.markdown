---
title: Automatic Deletion of Incomplete Output Files
layout: post
date: 2016-08-07T02:00:37Z
tags: [c, cpp, win32, linux]
uuid: 431fafe9-6630-363e-4596-85eb3a289ec2
---

Conventionally, a program that creates an output file will delete its
incomplete output should an error occur while writing the file. It's
risky to leave behind a file that the user may rightfully confuse for
a valid file. They might not have noticed the error.

For example, compression programs such as gzip, bzip2, and xz when
given a compressed file as an argument will create a new file with the
compression extension removed. They write to this file as the
compressed input is being processed. If the compressed stream contains
an error in the middle, the partially-completed output is removed.

There are exceptions of course, such as programs that download files
over a network. The partial result has value, especially if the
transfer can be [continued from where it left off][range]. The
convention is to append another extension, such as ".part", to
indicate a partial output.

The straightforward solution is to always delete the file as part of
error handling. A non-interactive program would report the error on
standard error, delete the file, and exit with an error code. However,
there are at least two situations where error handling would be unable
to operate: unhandled signals (usually including a segmentation fault)
and power failures. A partial or corrupted output file will be left
behind, possibly looking like a valid file.

A common, more complex approach is to name the file differently from
its final name while being written. If written successfully, the
completed file is renamed into place. This is already [required for
durable replacement][fsync], so it's basically free for many
applications. In the worst case, where the program is unable to clean
up, the obviously incomplete file is left behind only wasting space.

Looking to be more robust, I had the following misguided idea: **Rely
completely on the operating system to perform cleanup in the case of a
failure.** Initially the file would be configured to be automatically
deleted when the final handle is closed. This takes care of all
abnormal exits, and possibly even power failures. The program can just
exit on error without deleting the file. Once written successfully,
the automatic-delete indicator is cleared so that the file survives.

The target application for this technique supports both Linux and
Windows, so I would need to figure it out for both systems. On
Windows, there's the flag `FILE_FLAG_DELETE_ON_CLOSE`. I'd just need
to find a way to clear it. On POSIX, file would be unlinked while
being written, and linked into the filesystem on success. The latter
turns out to be a lot harder than I expected.

### Solution for Windows

I'll start with Windows since the technique actually works fairly well
here — ignoring the usual, dumb Win32 filesystem caveats. This is a
little surprising, since it's usually Win32 that makes these things
far more difficult than they should be.

The primary Win32 function for opening and creating files is
[CreateFile][createfile]. There are many options, but the key is
`FILE_FLAG_DELETE_ON_CLOSE`. Here's how an application might typically
open a file for output.

~~~c
DWORD access = GENERIC_WRITE;
DWORD create = CREATE_ALWAYS;
DWORD flags = FILE_FLAG_DELETE_ON_CLOSE;
HANDLE f = CreateFile("out.tmp", access, 0, 0, create, flags, 0);
~~~

This special flag asks Windows to delete the file as soon as the last
handle to to *file object* is closed. Notice I said file object, not
file, since [these are different things][chen]. The catch: This flag
is a property of the file object, not the file, and cannot be removed.

However, the solution is simple. Create a new link to the file so that
it survives deletion. This even works for files residing on a network
shares.

~~~c
CreateHardLink("out", "out.tmp", 0);
CloseHandle(f);  // deletes out.tmp file
~~~

The gotcha is that the underlying filesystem must be NTFS. FAT32
doesn't support hard links. Unfortunately, since FAT32 remains the
least common denominator and is still widely used for removable media,
depending on the application, your users may expect support for saving
files to FAT32. A workaround is probably required.

### Solution for Linux

This is where things really fall apart. It's just *barely* possible on
Linux, it's messy, and it's not portable anywhere else. There's no way
to do this for POSIX in general.

My initial thought was to create a file then unlink it. Unlike the
situation on Windows, files can be unlinked while they're currently
open by a process. These files are finally deleted when the last file
descriptor (the last reference) is closed. Unfortunately, using
unlink(2) to remove the last link to a file prevents that file from
being linked again.

Instead, the solution is to use the relatively new (since Linux 3.11),
Linux-specific `O_TMPFILE` flag when creating the file. Instead of a
filename, this variation of open(2) takes a directory and creates an
unnamed, temporary file in it. These files are special in that they're
permitted to be given a name in the filesystem at some future point.

For this example, I'll assume the output is relative to the current
working directory. If it's not, you'll need to open an additional file
descriptor for the parent directory, and also use openat(2) to avoid
possible race conditions (since paths can change from under you). The
number of ways this can fail is already rapidly multiplying.

~~~c
int fd = open(".", O_TMPFILE|O_WRONLY, 0600);
~~~

The catch is that only a handful of filesystems support `O_TMPFILE`.
It's like the FAT32 problem above, but worse. You could easily end up
in a situation where it's not supported, and will almost certainly
require a workaround.

Linking a file from a file descriptor is where things get messier. The
file descriptor must be linked with linkat(2) from its name on the
/proc virtual filesystem, constructed as a string. The following
snippet comes straight from the Linux open(2) manpage.

~~~c
char buf[64];
sprintf(buf, "/proc/self/fd/%d", fd);
linkat(AT_FDCWD, buf, AT_FDCWD, "out", AT_SYMLINK_FOLLOW);
~~~

Even on Linux, /proc isn't always available, such as within a chroot
or a container, so this part can fail as well. In theory there's a way
to do this with the Linux-specific `AT_EMPTY_PATH` and avoid /proc,
but I couldn't get it to work.

~~~c
// Note: this doesn't actually work for me.
linkat(fd, "", AT_FDCWD, "out", AT_EMPTY_PATH);
~~~

Given the poor portability (even within Linux), the number of ways
this can go wrong, and that a workaround is definitely needed anyway,
I'd say this technique is worthless. I'm going to stick with the
tried-and-true approach for this one.


[range]: https://tools.ietf.org/html/rfc7233
[fsync]: http://blog.httrack.com/blog/2013/11/15/everything-you-always-wanted-to-know-about-fsync/
[chen]: https://blogs.msdn.microsoft.com/oldnewthing/20160108-00/?p=92821
[createfile]: https://msdn.microsoft.com/en-us/library/windows/desktop/aa363858(v=vs.85).aspx
