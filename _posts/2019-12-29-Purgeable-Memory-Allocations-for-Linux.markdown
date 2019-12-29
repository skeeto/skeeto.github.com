---
title: Purgeable Memory Allocations for Linux
layout: post
date: 2019-12-29T00:25:49Z
tags: [c, linux, optimization]
uuid: 50300bbe-0939-4bcf-96ff-8fb96a9b12d5
---

I saw (part of) a video, [OS hacking: Purgeable memory][video], by
Andreas Kling who's writing an operating system called [Serenity][sos]
and recording videos his progress. In the video he implements
*purgeable memory* as [found on some Apple platforms][pm] by adding
special support in the kernel. A process tells the kernel that a
particular range of memory isn't important, and so the kernel can
reclaim if it the system is under memory pressure — the memory is
purgeable.

Linux has a mechanism like this, [`madvise(2)`][man], that allows
processes to provide hints to the kernel on how memory is expected to be
used. The flag of interest is `MADV_FREE`:

> The application no longer requires the pages in the range specified by
> `addr` and `len`. The kernel can thus free these pages, but the
> freeing could be delayed until memory pressure occurs. For each of the
> pages that has been marked to be freed but has not yet been freed, the
> free operation will be canceled if the caller writes into the page.

So, given this, I built a proof of concept / toy on top of `MADV_FREE`
that provides this functionality for Linux:

**<https://github.com/skeeto/purgeable>**

It [allocates anonymous pages][jit] using `mmap(2)`. When the allocation
is "unlocked" — i.e. the process isn't actively using it — its pages are
marked with `MADV_FREE` so that the kernel can reclaim them at any time.
To lock the allocation so that the process can safely make use of them,
the `MADV_FREE` is canceled. This is all a little trickier than it sounds,
and that's the subject of this article.

Note: There's also `MADV_DONTNEED` which seems like it would fit the
bill, but [it's implemented incorrectly in Linux][bc]. It *immediately*
frees the pages, and so it's useless for implementing purgeable memory.

### Purgeable API

Before diving into the implementation, here's the API. It's [just four
functions][min] with no structure definitions. The pointer used by the
API is the memory allocation itself. All the bookkeeping [associated
with that pointer][closure] is hidden away, out of sight from the API's
consumer. The full documentation is in [`purgeable.h`][h].

```c
void *purgeable_alloc(size_t);
void  purgeable_unlock(void *);
void *purgeable_lock(void *);
void  purgeable_free(void *);
```

The semantics are much like a C++ `weak_ptr` in that locking both
validates that the allocation is still available and creates a "strong"
reference to it that prevents it from being purged. Though unlike a weak
reference, the allocation is stickier. It will remain until the system is
actually under pressure, not just when the garbage collector happens to
run or the last strong reference is gone.

Here's how it might be used to, say, store decoded PNG data that can
decompressed again if needed:

```c
uint32_t *texture = 0;
struct png *png = png_load("texture.png");
if (!png) die();

/* ... */

for (;;) {
    if (!texture) {
        texture = purgeable_alloc(png->width * png->height * 4);
        if (!texture) die();
        png_decode_rgba(png, texture);
    } else if (!purgeable_lock(texture)) {
        purgeable_free(texture);
        texture = 0;
        continue;
    }
    glTexImage2D(
        GL_TEXTURE_2D, 0,
        GL_RGBA, png->width, png->height, 0,
        GL_RGBA, GL_UNSIGNED_BYTE, texture
    );
    purgeable_unlock(texture);
    break;
}
```

Memory is allocated in a locked state since it's very likely to be
immediately filled with data. The application should unlock it before
moving on with other tasks. The purgeable memory must always be freed
using `purgeable_free()`, even if `purgeable_lock()` failed. This not only
frees the bookkeeping, but also releases the now-zero pages and the
mapping itself. Originally I had `purgeable_lock()` free the purgeable
memory on failure, but I felt this was clearer. There's no technical
reason it couldn't, though.

### Purgeable Implementation

The main challenge is that the kernel doesn't necessarily treat the
`MADV_FREE` range contiguously. It might reclaim just some pages, and do
so in an arbitrary order. In order to lock the region, each page must be
handled individually. Per the man page quoted above, reversing
`MADV_FREE` requires a write to each page — to either trigger a page
fault or set [a dirty bit][db].

The only way to tell if a page has been purged is to check if it's been
filled with zeros. That's easy if we're sure a particular byte in the
page should be zero, but, since this is a library, the caller might just
store *anything* on these pages.

So here's my solution: To unlock a page, look at the first byte on the
page. Remember whether or not it's zero. If it's zero, write a 1 into
that byte. Once this has been done for all pages, use `madvise(2)` to
mark them all `MADV_FREE`.

With this approach, the library only needs to track one bit of information
per page regardless of the page's contents. Assuming 4kB pages, each 32kB
of allocation has 1 byte of overhead (amortized) — or ~0.003% overhead.
Not too bad!

Locking purgeable memory is a little trickier. Again, each page must be
visited in turn, and if any page was purged, then the whole allocation is
considered lost. If the first byte was non-zero when unlocking, the
library checks that it's still non-zero. If the first byte was zero when
unlocking, then it prepares to write a zero back into that byte, which
must currently be non-zero.

In either case, the `MADV_FREE` needs to be canceled using a write, so
the library [does an atomic compare-and-swap][lf] (CAS) to write the
correct byte into the page, *even if it's the same value* in the
non-zero case. The atomic CAS is essential because **it ensures the page
wasn't purged between the check and the write, as both are done
together, atomically**. If every page has the expected first byte, and
every CAS succeeded, then the purgeable memory has been successfully
locked.

As an optimization, the library could consider more than just the first
byte, and look at, say, the first `long int` on each page. The library
does less work when the page contains a non-zero value, and the chance of
an arbitrary 8-byte value being zero is much lower. However, I wanted to
avoid [potential aliasing issues][alias], especially if this library were
to be embedded, so I passed on the idea.

#### Bookkeeping

The bookkeeping data is stored just before the buffer returned as the
purgeable memory, and it's never marked with `MADV_FREE`. Assuming 4kB
pages, for each 128MB of purgeable memory the library allocates one extra
anonymous page to track it. The number of pages in the allocation is
stored just before the purgeable memory as a `size_t`, and the rest is the
per-page bit table described above.

```c
size_t *p = purgeable_alloc(1<<14);
size_t numpages = p[-1];
```

So the library can immediately find it starting from the purgeable memory
address. Here's an illustration:

          ,--- p
          |
          v
    ----------------------------------------------
    |...Z|    |    |    |    |    |    |    |    |
    ----------------------------------------------
     ^  ^
     |  |
     |  `--- size_t numpages
     |
     `--- bit table

The downside is that buffer underflows in the application would easily
trample the `numpages` value because it's located immediately adjacent. It
would be safer to move it to the *beginning* of the first page before the
purgeable memory, but this would have made bit table access more
complicated. While the region is locked, the contents of the bit table
don't matter, so it won't be damaged by an underflow. Another idea: put a
checksum alongside `numpages`. It could just be a simple [integer
hash][hash].

This makes for a really slick API since the consumer doesn't need to track
anything more than a single pointer, the address of the purgeable memory
allocation itself.

### Worth using?

I'm not quite sure how often I'd actually use purgeable memory in real
programs, especially in software intended to be portable. Each operating
system needs its own implementation, and this library is not portable
since it relies on interfaces and behaviors specific to Linux.

It also has a not-so-unlikely pathological case: Imagine a program that
makes two purgeable memory allocation, and they're large enough that one
always evicts the other. The program would thrash back and forth
fighting itself as it used each allocation. Detecting this situation
might be difficult, especially as the number of purgeable memory
allocations increases.

Regardless, it's another tool for my software toolbelt.


[alias]: /blog/2018/07/20/#strict-aliasing
[bc]: https://www.youtube.com/watch?v=bg6-LVCHmGM#t=58m23s
[closure]: /blog/2017/01/08/
[db]: https://en.wikipedia.org/wiki/Dirty_bit
[h]: https://github.com/skeeto/purgeable/blob/master/purgeable.h
[hash]: /blog/2018/07/31/
[jit]: /blog/2018/11/15/
[lf]: /blog/2014/09/02/
[man]: http://man7.org/linux/man-pages/man2/madvise.2.html
[min]: /blog/2018/06/10/
[pm]: https://developer.apple.com/library/archive/documentation/Performance/Conceptual/ManagingMemory/Articles/CachingandPurgeableMemory.html
[sos]: https://github.com/SerenityOS/serenity
[video]: https://www.youtube.com/watch?v=9l0nWEUpg7s
