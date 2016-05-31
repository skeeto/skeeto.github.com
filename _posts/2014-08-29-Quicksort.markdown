---
title: Stabilizing C's Quicksort
layout: post
date: 2014-08-29T18:39:01Z
tags: [c]
uuid: 078a1683-1484-3532-d231-5c5e805fc9a6
---

The C standard library includes a quicksort function called `qsort()`.
It sorts homogeneous arrays of arbitrary type. The interface is exactly
what you'd expect given the constraints of the language.

~~~c
void qsort(void *base, size_t nmemb, size_t size,
           int (*compar)(const void *, const void *));
~~~

It takes a pointer to the first element of the array, the number of
members, the size of each member, and a comparator function. The
comparator has to operate on `void *` pointers because C doesn't have
templates or generics or anything like that. That's two interfaces
where type safety is discarded: the arguments passed to `qsort()` and
again when it calls the comparator function.

One of the significant flaws of this interface is the lack of context
for the comparator. C doesn't have closures, which in other languages
would cover this situation. If the sort function depends on some
additional data, such as in [Graham scan][scan] where points are
sorted relative to a selected point, the extra information needs to be
smuggled in through a global variable. This is not reentrant and
wouldn't be safe in a multi-threaded environment. There's a GNU
extension here, `qsort_r()`, that takes an additional context
argument, allowing for reentrant comparators.

Quicksort has some really nice properties. It's in-place, so no
temporary memory needs to be allocated. If implemented properly it
only consumes O(log n) space, which is the stack growth during
recursion. Memory usage is localized, so it plays well with caching.

That being said, `qsort()` is also a classic example of an API naming
mistake. [Few implementations actually use straight
quicksort][shootout]. For example, glibc's `qsort()` is merge sort (in
practice), and the other major libc implementations use a [hybrid
approach][timsort]. Programs using their language's sort function
shouldn't be concerned with how it's implemented. All the matters is
the interface and whether or not it's a stable sort. OpenBSD made the
exact same mistake when they introduced [`arc4random()`][arc4random],
which [no longer uses RC4][chacha20].

Since quicksort is an unstable sort — there are multiple possible
results when the array contains equivalent elements — this means
`qsort()` is not guaranteed to be stable, even if internally the C
library *is* using a stable sort like merge sort. The C standard
library has no stable sort function.

### Comparator Composability

The unfortunate side effect of unstable sorts is that they hurt
composability. For example, let's say we have a `person` struct like
this,

~~~c
struct person {
    const char *first, *last;
    int age;
};
~~~

Here are a couple of comparators to sort either by name or by age. As
a side note, `strcmp()` automatically works correctly with UTF-8 so
this program isn't limited to old-fashioned ASCII names.

~~~c
#include <string.h>

int compare_name(const void *a, const void *b)
{
    struct person *pa = (struct person *) a;
    struct person *pb = (struct person *) b;
    int last = strcmp(pa->last, pb->last);
    return last != 0 ? last : strcmp(pa->first, pb->first);
}

int compare_age(const void *a, const void *b)
{
    struct person *pa = (struct person *) a;
    struct person *pb = (struct person *) b;
    return pa->age - pb->age;
}
~~~

And since we'll need it later, here's a `COUNT_OF` macro to get the
length of arrays at compile time. There's a [less error prone
version][countof] out there, but I'm keeping it simple.

~~~c
#define COUNT_OF(x) (sizeof(x) / sizeof(0[x]))
~~~

Say we want to sort by name, *then* age. When using a stable sort,
this is accomplished by sorting on each field separately in reverse
order of preference: a composition of individual comparators. Here's
an attempt at using quicksort to sort an array of people by age then
name.

~~~c
struct person people[] = {
    {"Joe", "Shmoe", 24},
    {"John", "Doe", 30},
    {"Alan", "Smithee", 42},
    {"Jane", "Doe", 30}
};

qsort(people, COUNT_OF(people), sizeof(struct person), compare_name);
qsort(people, COUNT_OF(people), sizeof(struct person), compare_age);
~~~

But this doesn't always work. J**a**ne should come before J**o**hn,
but the original sort was completely lost in the second sort.

    Joe Shmoe, 24
    John Doe, 30
    Jane Doe, 30
    Alan Smithee, 42

This could be fixed by defining a new comparator that operates on both
fields at once, `compare_age_name()`, and performing a single sort.
But what if later you want to sort by name then age? Now you need
`compare_name_age()`. If a third field was added, there would need to
be 6 (3!) different comparator functions to cover all the
possibilities. If you had 6 fields, you'd need 720 comparators!
Composability has been lost to a combinatorial nightmare.

### Pointer Comparison

The GNU libc documentation [claims that `qsort()` can be made
stable][gnu] by using pointer comparison as a fallback. That is, when
the relevant fields are equivalent, use their array position to
resolve the difference.

> If you want the effect of a stable sort, you can get this result by
> writing the comparison function so that, lacking other reason
> distinguish between two elements, it compares them by their
> addresses.

This is not only false, it's dangerous! Because elements may be sorted
in-place, even in glibc, their position will change during the sort.
The comparator will be using their *current* positions, not the
starting positions. What makes it dangerous is that the comparator
will return different orderings throughout the sort as elements are
moved around the array. This could result in an infinite loop, or
worse.

### Making it Stable

The most direct way to work around the unstable sort is to eliminate
any equivalencies. Equivalent elements can be distinguished by adding
an intrusive `order` field which is set after each sort. The
comparators will fall back on this sort to maintain the original
ordering.

~~~c
struct person {
    const char *first, *last;
    int age;
    size_t order;
};
~~~

And the new comparators.

~~~c
int compare_name_stable(const void *a, const void *b)
{
    struct person *pa = (struct person *) a;
    struct person *pb = (struct person *) b;
    int last = strcmp(pa->last, pb->last);
    if (last != 0)
        return last;
    int first = strcmp(pa->first, pb->first);
    if (first != 0)
        return first;
    return pa->order - pb->order;
}

int compare_age_stable(const void *a, const void *b)
{
    struct person *pa = (struct person *) a;
    struct person *pb = (struct person *) b;
    int age = pa->age - pb->age;
    return age != 0 ? age : pa->order - pb->order;
}
~~~

The first sort doesn't need to be stable, but there's not much reason
to keep around two definitions.

~~~c
qsort(people, COUNT_OF(people), sizeof(people[0]), compare_name_stable);
for (size_t i = 0; i < COUNT_OF(people); i++)
    people[i].order = i;
qsort(people, COUNT_OF(people), sizeof(people[0]), compare_age_stable);
~~~

And the result:

    Joe Shmoe, 24
    Jane Doe, 30
    John Doe, 30
    Alan Smithee, 42

Without defining any new comparators I can sort by name then age just
by swapping the calls to `qsort()`. At the cost of an extra
bookkeeping field, the number of comparator functions needed as fields
are added is O(n) and not O(n!) despite using an unstable sort.


[scan]: http://en.wikipedia.org/wiki/Graham_scan
[shootout]: http://calmerthanyouare.org/2013/05/31/qsort-shootout.html
[arc4random]: http://www.openbsd.org/cgi-bin/man.cgi?query=arc4random&sektion=3
[chacha20]: http://marc.info/?l=openbsd-cvs&m=138065251627052&w=2
[timsort]: http://en.wikipedia.org/wiki/Timsort
[countof]: http://stackoverflow.com/a/1598827
[gnu]: http://www.gnu.org/software/libc/manual/html_node/Array-Sort-Function.html
