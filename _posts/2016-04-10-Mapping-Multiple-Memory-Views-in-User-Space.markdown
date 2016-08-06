---
title: Mapping Multiple Memory Views in User Space
layout: post
date: 2016-04-10T21:59:16Z
tags: [c, linux, win32, posix]
uuid: 373e602e-0d43-3e03-f02c-2d169eb14df5
---

Modern operating systems run processes within *virtual memory* using a
piece of hardware called a *memory management unit* (MMU). The MMU
contains a *page table* that defines how virtual memory maps onto
*physical memory*. The operating system is responsible for maintaining
this page table, mapping and unmapping virtual memory to physical
memory as needed by the processes it's running. If a process accesses
a page that is not currently mapped, it will trigger a *page fault*
and the execution of the offending thread will be paused until the
operating system maps that page.

This functionality allows for a neat hack: A physical memory address
can be mapped to multiple virtual memory addresses at the same time. A
process running with such a mapping will see these regions of memory
as aliased — views of the same physical memory. A store to one of
these addresses will simultaneously appear across all of them.

Some useful applications of this feature include:

* An extremely fast, large memory "copy" by mapping the source memory
  overtop the destination memory.
* Trivial interoperability between code instrumented with [baggy
  bounds checking][baggy] [PDF] and non-instrumented code. A few bits
  of each pointer are reserved to tag the pointer with the size of its
  memory allocation. For compactness, the stored size is rounded up to
  a power of two, making it "baggy." Instrumented code checks this tag
  before making a possibly-unsafe dereference. Normally, instrumented
  code would need to clear (or set) these bits before dereferencing or
  before passing it to non-instrumented code. Instead, the allocation
  could be mapped simultaneously at each location for every possible
  tag, making the pointer valid no matter its tag bits.
* Two responses to [my last post on hotpatching][hotpatch] suggested
  that, instead of modifying the instruction directly, memory
  containing the modification could be mapped over top of the code. I
  would copy the code to another place in memory, safely modify it in
  private, switch the page protections from write to execute (both for
  W^X and for [other hardware limitations][amd64]), then map it over
  the target. Restoring the original behavior would be as simple as
  unmapping the change.

Both POSIX and Win32 allow user space applications to create these
aliased mappings. The original purpose for these APIs is for shared
memory between processes, where the same physical memory is mapped
into two different processes' virtual memory. But the OS doesn't stop
us from mapping the shared memory to a different address within the
same process.

### POSIX Memory Mapping

On POSIX systems (Linux, *BSD, OS X, etc.), the three key functions
are `shm_open(3)`, `ftruncate(2)`, and `mmap(2)`.

First, create a file descriptor to shared memory using `shm_open`. It
has very similar semantics to `open(2)`.

~~~c
int shm_open(const char *name, int oflag, mode_t mode);
~~~

The `name` works much like a filesystem path, but is actually a
different namespace (though on Linux it *is* a tmpfs mounted at
`/dev/shm`). Resources created here (`O_CREAT`) will persist until
explicitly deleted (`shm_unlink(3)`) or until the system reboots. It's
an oversight in POSIX that a name is required even if we never intend
to access it by name. File descriptors can be shared with other
processes via `fork(2)` or through UNIX domain sockets, so a name
isn't strictly required.

OpenBSD introduced [`shm_mkstemp(3)`][mkstemp] to solve this problem,
but it's not widely available. On Linux, as of this writing, the
`O_TMPFILE` flag may or may not provide a fix ([it's
undocumented][tmpfile]).

The portable workaround is to attempt to choose a unique name, open
the file with `O_CREAT | O_EXCL` (either atomically create the file or
fail), `shm_unlink` the shared memory object as soon as possible, then
cross our fingers. The shared memory object will still exist (the file
descriptor keeps it alive) but will not longer be accessible by name.

~~~c
int fd = shm_open("/example", O_RDWR | O_CREAT | O_EXCL, 0600);
if (fd == -1)
    handle_error(); // non-local exit
shm_unlink("/example");
~~~

The shared memory object is brand new (`O_EXCL`) and is therefore of
zero size. `ftruncate` sets it to the desired size. This does *not*
need to be a multiple of the page size. Failing to allocate memory
will result in a bus error on access.

~~~c
size_t size = sizeof(uint32_t);
ftruncate(fd, size);
~~~

Finally `mmap` the shared memory into place just as if it were a file.
We can choose an address (aligned to a page) or let the operating
system choose one for use (NULL). If we don't plan on making any more
mappings, we can also close the file descriptor. The shared memory
object will be freed as soon as it completely unmapped (`munmap(2)`).

~~~c
int prot = PROT_READ | PROT_WRITE;
uint32_t *a = mmap(NULL, size, prot, MAP_SHARED, fd, 0);
uint32_t *b = mmap(NULL, size, prot, MAP_SHARED, fd, 0);
close(fd);
~~~

At this point both `a` and `b` have different addresses but point (via
the page table) to the same physical memory. Changes to one are
reflected in the other. So this:

~~~c
*a = 0xdeafbeef;
printf("%p %p 0x%x\n", a, b, *b);
~~~

Will print out something like:

    0x6ffffff0000 0x6fffffe0000 0xdeafbeef

It's also possible to do all this only with `open(2)` and `mmap(2)` by
mapping the same file twice, but you'd need to worry about where to
put the file, where it's going to be backed, and the operating system
will have certain obligations about syncing it to storage somewhere.
Using POSIX shared memory is simpler and faster.

### Windows Memory Mapping

Windows is very similar, but directly supports anonymous shared
memory. The key functions are `CreateFileMapping`, and
`MapViewOfFileEx`.

First create a file mapping object from an invalid handle value. Like
POSIX, the word "file" is used without actually involving files.

~~~c
size_t size = sizeof(uint32_t);
HANDLE h = CreateFileMapping(INVALID_HANDLE_VALUE,
                             NULL,
                             PAGE_READWRITE,
                             0, size,
                             NULL);
~~~

There's no truncate step because the space is allocated at creation
time via the two-part size argument.

Then, just like `mmap`:

~~~c
uint32_t *a = MapViewOfFile(h, FILE_MAP_ALL_ACCESS, 0, 0, size);
uint32_t *b = MapViewOfFile(h, FILE_MAP_ALL_ACCESS, 0, 0, size);
CloseHandle(h);
~~~

If I wanted to choose the target address myself, I'd call
`MapViewOfFileEx` instead, which takes the address as additional
argument.

From here on it's the same as above.

### Generalizing the API

Having some fun with this, I came up with a general API to allocate an
aliased mapping at an arbitrary number of addresses.

~~~c
int  memory_alias_map(size_t size, size_t naddr, void **addrs);
void memory_alias_unmap(size_t size, size_t naddr, void **addrs);
~~~

Values in the address array must either be page-aligned or NULL to
allow the operating system to choose, in which case the map address is
written to the array.

It returns 0 on success. It may fail if the size is too small (0), too
large, too many file descriptors, etc.

Pass the same pointers back to `memory_alias_unmap` to free the
mappings. When called correctly it cannot fail, so there's no return
value.

The full source is here: [memalias.c](/download/memalias.c)

#### POSIX

Starting with the simpler of the two functions, the POSIX
implementation looks like so:

~~~c
void
memory_alias_unmap(size_t size, size_t naddr, void **addrs)
{
    for (size_t i = 0; i < naddr; i++)
        munmap(addrs[i], size);
}
~~~

The complex part is creating the mapping:

~~~c
int
memory_alias_map(size_t size, size_t naddr, void **addrs)
{
    char path[128];
    snprintf(path, sizeof(path), "/%s(%lu,%p)",
             __FUNCTION__, (long)getpid(), addrs);
    int fd = shm_open(path, O_RDWR | O_CREAT | O_EXCL, 0600);
    if (fd == -1)
        return -1;
    shm_unlink(path);
    ftruncate(fd, size);
    for (size_t i = 0; i < naddr; i++) {
        addrs[i] = mmap(addrs[i], size,
                        PROT_READ | PROT_WRITE, MAP_SHARED,
                        fd, 0);
        if (addrs[i] == MAP_FAILED) {
            memory_alias_unmap(size, i, addrs);
            close(fd);
            return -1;
        }
    }
    close(fd);
    return 0;
}
~~~

The shared object name includes the process ID and pointer array
address, so there really shouldn't be any non-malicious name
collisions, even if called from multiple threads in the same process.

Otherwise it just walks the array setting up the mappings.

#### Windows

The Windows version is very similar.

~~~c
void
memory_alias_unmap(size_t size, size_t naddr, void **addrs)
{
    (void)size;
    for (size_t i = 0; i < naddr; i++)
        UnmapViewOfFile(addrs[i]);
}
~~~

Since Windows tracks the size internally, it's unneeded and ignored.

~~~c
int
memory_alias_map(size_t size, size_t naddr, void **addrs)
{
    HANDLE m = CreateFileMapping(INVALID_HANDLE_VALUE,
                                 NULL,
                                 PAGE_READWRITE,
                                 0, size,
                                 NULL);
    if (m == NULL)
        return -1;
    DWORD access = FILE_MAP_ALL_ACCESS;
    for (size_t i = 0; i < naddr; i++) {
        addrs[i] = MapViewOfFileEx(m, access, 0, 0, size, addrs[i]);
        if (addrs[i] == NULL) {
            memory_alias_unmap(size, i, addrs);
            CloseHandle(m);
            return -1;
        }
    }
    CloseHandle(m);
    return 0;
}
~~~

In the future I'd like to find some unique applications of these
multiple memory views.


[baggy]: https://www.usenix.org/legacy/event/sec09/tech/full_papers/akritidis.pdf
[hotpatch]: /blog/2016/03/31/
[amd64]: http://stackoverflow.com/a/18905927
[tmpfile]: http://comments.gmane.org/gmane.linux.man/9815
[mkstemp]: http://man.openbsd.org/OpenBSD-current/man3/shm_mkstemp.3
