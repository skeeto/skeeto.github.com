---
title: Inspecting C's qsort Through Animation
layout: post
date: 2016-09-05T21:17:11Z
tags: [c, linux, media, video]
uuid: 7d86c669-ff40-3210-7e28-78b801e35e50
---

The C standard library includes a qsort() function for sorting
arbitrary buffers given a comparator function. The name comes from its
[original Unix implementation, "quicker sort,"][name] a variation of
the well-known quicksort algorithm. The C standard doesn't specify an
algorithm, except to say that it may be unstable (C99 §7.20.5.2¶4) —
equal elements have an unspecified order. As such, different C
libraries use different algorithms, and even when using the same
algorithm they make different implementation trade-offs.

I added a drawing routine to a comparison function to see what the
sort function was doing for different C libraries. Every time it's
called for a comparison, it writes out a snapshot of the array as a
Netpbm PPM image. It's [easy to turn concatenated PPMs into a GIF or
video][poor]. Here's my code if you want to try it yourself:

* [qsort-animate.c][source]

Adjust the parameters at the top to taste. Rather than call rand() in
the standard library, I included xorshift64star() with a hard-coded
seed so that the array will be shuffled exactly the same across all
platforms. This makes for a better comparison.

To get an optimized GIF on unix-like systems, run it like so.
(Microsoft's [UCRT currently has serious bugs][bug] with pipes, so it
was run differently in that case.)

    ./a.out | convert -delay 10 ppm:- gif:- | gifsicle -O3 > sort.gif

The number of animation frames reflects the efficiency of the sort,
but this isn't really a benchmark. The input array is fully shuffled,
and real data often not. For a benchmark, have a look at [a libc
qsort() shootout of sorts][shootout] instead.

To help you follow along, **clicking on any animation will restart it.**

### glibc

![](/img/qsort/glibc.gif){: .resetable title="glibc"}

Sorted in **307 frames**. glibc prefers to use mergesort, which,
unlike quicksort, isn't an in-place algorithm, so it has to allocate
memory. That allocation could fail for huge arrays, and, since qsort()
can't fail, it uses quicksort as a backup. You can really see the
mergesort in action: changes are made that we cannot see until later,
when it's copied back into the original array.

### dietlibc (0.32)

Sorted in **503 frames**. [dietlibc][diet] is an alternative C
standard library for Linux. It's optimized for size, which shows
through its slower performance. It looks like a quicksort that always
chooses the last element as the pivot.

![](/img/qsort/diet.gif){: .resetable title="diet"}

Update: Felix von Leitner, the primary author of dietlibc, has alerted
me that, as of version 0.33, it now chooses a random pivot. This
comment from the source describes it:

> We chose the rightmost element in the array to be sorted as pivot,
> which is OK if the data is random, but which is horrible if the data
> is already sorted. Try to improve by exchanging it with a random
> other pivot.

### musl

Sort in **637 frames**. [musl libc][musl] is another alternative C
standard library for Linux. It's my personal preference when I
statically link Linux binaries. Its qsort() looks a lot like a heapsort,
and with some research I see it's actually [smoothsort][smooth], a
heapsort variant.

![](/img/qsort/musl.gif){: .resetable title="musl"}

### BSD

Sorted in **354 frames**. I ran it on both OpenBSD and FreeBSD with
identical results, so, unsurprisingly, they share an implementation.
It's quicksort, and what's neat about it is at the beginning you can
see it searching for a median for use as the pivot. This helps avoid
the O(n^2) worst case.

![](/img/qsort/bsd-qsort.gif){: .resetable title="BSD qsort"}

BSD also includes a mergesort() with the same prototype, except with
an `int` return for reporting failures. This one sorted in **247
frames**. Like glibc before, there's some behind-the-scenes that isn't
captured. But even more, notice how the markers disappear during the
merge? It's running the comparator against copies, stored outside the
original array. Sneaky!

![](/img/qsort/bsd-mergesort.gif){: .resetable title="BSD mergesort"}

Again, BSD also includes heapsort(), so ran that too. It sorted in
**418 frames**. It definitely looks like a heapsort, and the worse
performance is similar to musl. It seems heapsort is a poor fit for
this data.

![](/img/qsort/bsd-heapsort.gif){: .resetable title="BSD heapsort"}

### Cygwin

It turns out Cygwin borrowed its qsort() from BSD. It's pixel
identical to the above. I hadn't noticed until I looked at the frame
counts.

![](/img/qsort/cygwin.gif){: .resetable title="Cygwin (BSD)"}

### MSVCRT.DLL (MinGW) and UCRT (Visual Studio)

MinGW builds against MSVCRT.DLL, found on every Windows system despite
its [unofficial status][msvcrt]. Until recently Microsoft didn't
include a C standard library as part of the OS, but that changed with
their [Universal CRT (UCRT) announcement][ucrt]. I thought I'd try
them both.

Turns out they borrowed their old qsort() for the UCRT, and the result
is the same: sorted in **417 frames**. It chooses a pivot from the
median of the ends and the middle, swaps the pivot to the middle, then
partitions. Looking to the middle for the pivot makes sorting
pre-sorted arrays much more efficient.

![](/img/qsort/ucrt.gif){: .resetable title="Microsoft UCRT"}

### Pelles C

Finally I ran it against [Pelles C][pellesc], a C compiler for
Windows. It sorted in **463 frames**. I can't find any information
about it, but it looks like some sort of hybrid between quicksort and
insertion sort. Like BSD qsort(), it finds a good median for the
pivot, partitions the elements, and if a partition is small enough, it
switches to insertion sort. This should behave well on mostly-sorted
arrays, but poorly on well-shuffled arrays (like this one).

![](/img/qsort/pellesc.gif){: .resetable title="Pelles C"}

### More Implementations

That's everything that was readily accessible to me. If you can run it
against something new, I'm certainly interested in seeing more
implementations.


<script type="text/javascript">
(function() {
    var r = document.querySelectorAll('.resetable');
    for (var i = 0; i < r.length; i++) {
        r[i].onclick = function() {
            var src = this.src;
            var height = this.height;
            this.src = "";
            this.height = height;
            // setTimeout() required for IE
            var _this = this;
            setTimeout(function() { _this.src = src; }, 0);
        };
    }
}());
</script>


[source]: /download/qsort-animate.c
[name]: http://citeseer.ist.psu.edu/viewdoc/summary?doi=10.1.1.14.8162
[shootout]: http://calmerthanyouare.org/2013/05/31/qsort-shootout.html
[poor]: /blog/2011/11/28/
[bug]: http://radiance-online.org:82/pipermail/radiance-dev/2016-March/001578.html
[diet]: https://www.fefe.de/dietlibc/
[musl]: https://www.musl-libc.org/
[ucrt]: https://blogs.msdn.microsoft.com/vcblog/2015/03/03/introducing-the-universal-crt/
[msvcrt]: https://blogs.msdn.microsoft.com/oldnewthing/20140411-00/?p=1273
[pellesc]: http://www.smorgasbordet.com/pellesc/
[smooth]: http://www.keithschwarz.com/smoothsort/
