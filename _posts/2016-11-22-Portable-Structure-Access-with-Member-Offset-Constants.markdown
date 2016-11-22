---
title: Portable Structure Access with Member Offset Constants
layout: post
date: 2016-11-22T12:55:29Z
tags: [c, posix, x86]
uuid: 81ff4064-17f1-3a9b-a5ec-61acb03385b9
---

Suppose you need to write a C program to access a long sequence of
structures from a binary file in a specified format. These structures
have different lengths and contents, but also a common header
identifying its type and size. Here's the definition of that header
(no padding):

~~~c
struct event {
    uint64_t time;   // unix epoch (microseconds)
    uint32_t size;   // including this header (bytes)
    uint16_t source;
    uint16_t type;
};
~~~

The `size` member is used to find the offset of the next structure in
the file without knowing anything else about the current structure.
Just add `size` to the offset of the current structure.

The `type` member indicates what kind of data follows this structure.
The program is likely to `switch` on this value.

The actual structures might look something like this (in the spirit of
[X-COM][xcom]). Note how each structure begins with `struct event` as
header. All angles are expressed using [binary scaling][bs].

~~~c
#define EVENT_TYPE_OBSERVER            10
#define EVENT_TYPE_UFO_SIGHTING        20
#define EVENT_TYPE_SUSPICIOUS_SIGNAL   30

struct observer {
    struct event event;
    uint32_t latitude;   // binary scaled angle
    uint32_t longitude;  //
    uint16_t source_id;  // later used for event source
    uint16_t name_size;  // not including null terminator
    char name[];
};

struct ufo_sighting {
    struct event event;
    uint32_t azimuth;    // binary scaled angle
    uint32_t elevation;  //
};

struct suspicious_signal {
    struct event event;
    uint16_t num_channels;
    uint16_t sample_rate;  // Hz
    uint32_t num_samples;  // per channel
    int16_t samples[];
};
~~~

If all integers are stored in little endian byte order (least
significant byte first), there's a strong temptation to lay the
structures directly over the data. After all, this will work correctly
on most computers.

~~~c
struct event header;
fread(buffer, sizeof(header), 1, file);
switch (header.type) {
    // ...
}
~~~

This code will not work correctly when:

1. The host machine doesn't use little endian byte order, though this
   is now uncommon. Sometimes developers will attempt to detect the
   byte order at compile time and use the preprocessor to byte-swap if
   needed. This is a mistake.

2. The host machine has different alignment requirements and so
   introduces additional padding to the structure. Sometimes this can
   be resolved with a non-standard [`#pragma pack`][pack].

### Integer extraction functions

Fortunately it's easy to write fast, correct, portable code for this
situation. First, define some functions to extract little endian
integers from an octet buffer (`uint8_t`). These will work correctly
regardless of the host's alignment and byte order.

~~~c
static inline uint16_t
extract_u16le(const uint8_t *buf)
{
    return (uint16_t)buf[1] << 8 |
           (uint16_t)buf[0] << 0;
}

static inline uint32_t
extract_u32le(const uint8_t *buf)
{
    return (uint32_t)buf[3] << 24 |
           (uint32_t)buf[2] << 16 |
           (uint32_t)buf[1] <<  8 |
           (uint32_t)buf[0] <<  0;
}

static inline uint64_t
extract_u64le(const uint8_t *buf)
{
    return (uint64_t)buf[7] << 56 |
           (uint64_t)buf[6] << 48 |
           (uint64_t)buf[5] << 40 |
           (uint64_t)buf[4] << 32 |
           (uint64_t)buf[3] << 24 |
           (uint64_t)buf[2] << 16 |
           (uint64_t)buf[1] <<  8 |
           (uint64_t)buf[0] <<  0;
}
~~~

The big endian version is identical, but with shifts in reverse order.

A common concern is that these functions are a lot less efficient than
they could be. On x86 where alignment is very relaxed, each could be
implemented as a single load instruction. However, on GCC 4.x and
earlier, `extract_u32le` compiles to something like this:

~~~nasm
extract_u32le:
        movzx   eax, [rdi+3]
        sal     eax, 24
        mov     edx, eax
        movzx   eax, [rdi+2]
        sal     eax, 16
        or      eax, edx
        movzx   edx, [rdi]
        or      eax, edx
        movzx   edx, [rdi+1]
        sal     edx, 8
        or      eax, edx
        ret
~~~

It's tempting to fix the problem with the following definition:

~~~c
// Note: Don't do this.
static inline uint32_t
extract_u32le(const uint8_t *buf)
{
    return *(uint32_t *)buf;
}
~~~

It's unportable, it's undefined behavior, and worst of all, it [might
not work correctly even on x86][x86]. Fortunately I have some great
news. On GCC 5.x and above, the correct definition compiles to the
desired, fast version. It's the best of both worlds.

~~~c
extract_u32le:
        mov     eax, [rdi]
        ret
~~~

It's even smart about the big endian version:

~~~c
static inline uint32_t
extract_u32be(const uint8_t *buf)
{
    return (uint32_t)buf[0] << 24 |
           (uint32_t)buf[1] << 16 |
           (uint32_t)buf[2] <<  8 |
           (uint32_t)buf[3] <<  0;
}
~~~

Is compiled to exactly what you'd want:

~~~nasm
extract_u32be:
        mov     eax, [rdi]
        bswap   eax
        ret
~~~

Or, even better, if your system supports `movbe` (`gcc -mmovbe`):

~~~nasm
extract_u32be:
        movbe   eax, [rdi]
        ret
~~~

Unfortunately, Clang/LLVM is *not* this smart as of 3.9, but I'm
betting it will eventually learn how to do this, too.

### Member offset constants

For this next technique, that `struct event` from above need not
actually be in the source. It's purely documentation. Instead, let's
define the structure in terms of *member offset constants* — a term I
just made up for this article. I've included the integer types as part
of the name to aid in their correct use.

~~~c
#define EVENT_U64LE_TIME    0
#define EVENT_U32LE_SIZE    8
#define EVENT_U16LE_SOURCE  12
#define EVENT_U16LE_TYPE    14
~~~

Given a buffer, the integer extraction functions, and these offsets,
structure members can be plucked out on demand.

~~~c
uint8_t *buf;
// ...
uint64_t time   = extract_u64le(buf + EVENT_U64LE_TIME);
uint32_t size   = extract_u32le(buf + EVENT_U32LE_SIZE;
uint16_t source = extract_u16le(buf + EVENT_U16LE_SOURCE);
uint16_t type   = extract_u16le(buf + EVENT_U16LE_TYPE);
~~~

On x86 with GCC 5.x, each member access will be inlined and compiled
to a one-instruction extraction. As far as performance is concerned,
it's identical to using a structure overlay, but this time the C code
is clean and portable. A slight downside is the lack of type checking
on member access: it's easy to mismatch the types and accidentally
read garbage.

### Memory mapping and iteration

There's a real advantage to memory mapping the input file and using
its contents directly. On a system with a huge virtual address space,
such as x86-64 or AArch64, this memory is almost "free." Already being
backed by a file, paging out this memory costs nothing (i.e. it's
discarded). The input file can comfortably be much larger than
physical memory without straining the system.

Unportable structure overlay can take advantage of memory mapping this
way, but has the previously-described issues. An approach with member
offset constants will take advantage of it just as well, all while
remaining clean and portable.

I like to wrap the memory mapping code into a simple interface, which
makes porting to non-POSIX platforms, such Windows, easier. Caveat:
This won't work with files whose size exceeds the available contiguous
virtual memory of the system — a real problem for 32-bit systems.

~~~c
#include <fcntl.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/stat.h>

uint8_t *
map_file(const char *path, size_t *length)
{
    int fd = open(path, O_RDONLY);
    if (fd == -1)
        return 0;

    struct stat stat;
    if (fstat(fd, &stat) == -1) {
        close(fd);
        return 0;
    }

    *length = stat.st_size;  // TODO: possible overflow
    uint8_t *p = mmap(0, *length, PROT_READ, MAP_PRIVATE, fd, 0);
    close(fd);
    return p != MAP_FAILED ? p : 0;
}

void
unmap_file(uint8_t *p, size_t length)
{
    munmap(p, length);
}
~~~

Next, here's an example that iterates over all the structures in
`input_file`, in this case counting each. The `size` member is
extracted in order to stride to the next structure.

~~~c
size_t length;
uint8_t *data = map_file(input_file, &length);
if (!data)
    FATAL();

size_t event_count = 0;
uint8_t *p = data;
while (p < data + length) {
    event_count++;
    uint32_t size = extract_u32le(p + EVENT_U32LE_SIZE);
    if (size > length - (p - data))
        FATAL();  // invalid size
    p += size;
}
printf("I see %zu events.\n", event_count);

unmap_file(data, length);
~~~

This is the basic structure for navigating this kind of data. A deeper
dive would involve a `switch` inside the loop, extracting the relevant
members for whatever use is needed.

Fast, correct, simple. Pick three.


[pack]: http://gcc.gnu.org/onlinedocs/gcc-4.4.4/gcc/Structure_002dPacking-Pragmas.html
[x86]: http://pzemtsov.github.io/2016/11/06/bug-story-alignment-on-x86.html
[xcom]: http://openxcom.org/
[bs]: https://en.wikipedia.org/wiki/Binary_scaling
