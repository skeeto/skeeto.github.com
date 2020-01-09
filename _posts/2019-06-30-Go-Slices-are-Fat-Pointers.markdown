---
title: Go Slices are Fat Pointers
layout: post
date: 2019-06-30T21:27:19Z
tags: [c, go]
uuid: 5ba40d47-11e4-4f82-b805-f5e7825df44c
excerpt_separator: <!--more-->
---

*This article was discussed [on Hacker News][hn].*

One of the frequent challenges in C is that pointers are nothing but a
memory address. A callee who is passed a pointer doesn't truly know
anything other than the type of object being pointed at, which says some
things about alignment and how that pointer can be used… maybe. If it's
a pointer to void (`void *`) then not even that much is known.

<!--more-->

The number of consecutive elements being pointed at is also not known.
It could be as few as zero, so dereferencing would be illegal. This can
be true even when the pointer is not null. Pointers can go one past the
end of an array, at which point it points to zero elements. For example:

```c
void foo(int *);

void bar(void)
{
    int array[4];
    foo(array + 4);  // pointer one past the end
}
```

In some situations, the number of elements *is* known, at least to the
programmer. For example, the function might have a contract that says it
must be passed *at least* N elements, or *exactly* N elements. This
could be communicated through documentation.

```c
/** Foo accepts 4 int values. */
void foo(int *);
```

Or it could be implied by the function's prototype. Despite the
following function appearing to accept an array, that's actually a
pointer, and the "4" isn't relevant to the prototype.

```c
void foo(int[4]);
```

C99 introduced a feature to make this a formal part of the prototype,
though, unfortunately, I've never seen a compiler actually use this
information.

```c
void foo(int[static 4]);  // >= 4 elements, cannot be null
```

Another common pattern is for the callee to accept a count parameter.
For example, the POSIX `write()` function:

```c
ssize_t write(int fd, const void *buf, size_t count);
```

The necessary information describing the buffer is split across two
arguments. That can become tedious, and it's also a source of serious
bugs if the two parameters aren't in agreement (buffer overflow,
[information disclosure][disc], etc.). Wouldn't it be nice if this
information was packed into the pointer itself? That's essentially the
definition of a *fat pointer*.

### Fat pointers via bit hacks

If we assume some things about the target platform, we can encode fat
pointers inside a plain pointer with [some dirty pointer
tricks][uintptr], exploiting unused bits in the pointer value. For
example, currently on x86-64, only the lower 48 bits of a pointer are
actually used. The other 16 bits could carefully be used for other
information, like communicating the number of elements or bytes:

```c
// NOTE: x86-64 only!
unsigned char buf[1000];
uintptr addr = (uintptr_t)buf & 0xffffffffffff;
uintptr pack = (sizeof(buf) << 48) | addr;
void *fatptr = (void *)pack;
```

The other side can unpack this to get the components back out. Obviously
16 bits for the count will often be insufficient, so this would more
likely be used for [baggy bounds checks][bb].

Further, if we know something about the alignment — say, that it's
16-byte aligned — then we can also encode information in the least
significant bits, such as a type tag.

### Fat pointers via a struct

That's all fragile, non-portable, and rather limited. A more robust
approach is to lift pointers up into a richer, heavier type, like a
structure.

```c
struct fatptr {
    void *ptr;
    size_t len;
};
```

Functions accepting these fat pointers no longer need to accept a count
parameter, and they'd generally accept the fat pointer by value.

```c
fatptr_write(int fd, struct fatptr);
```

In typical C implementations, the structure fields would be passed
practically, if not exactly, same way as the individual parameters would
have been passed, so it's really no less efficient.

To help keep this straight, we might employ some macros:

```c
#define COUNTOF(array) \
    (sizeof(array) / sizeof(array[0]))

#define FATPTR(ptr, count) \
    (struct fatptr){ptr, count}

#define ARRAYPTR(array) \
    FATPTR(array, COUNTOF(array))

/* ... */

unsigned char buf[40];
fatptr_write(fd, ARRAYPTR(buf));
```

There are obvious disadvantages of this approach, like type confusion
due to that void pointer, the inability to use `const`, and just being
weird for C. I wouldn't use it in a real program, but bear with me.

Before I move on, I want to add one more field to that fat pointer
struct: capacity.

```c
struct fatptr {
    void *ptr;
    size_t len;
    size_t cap;
};
```

This communicates not how many elements are present (`len`), but how
much additional space is left in the buffer. This allows callees know
how much room is left for, say, appending new elements.

```c
// Fix the remainder of an int buffer with a value.
void
fill(struct fatptr ptr, int value)
{
    int *buf = ptr.ptr;
    for (size_t i = ptr.len; i < ptr.cap; i++) {
        buf[i] = value;
    }
}
```

Since the callee modifies the fat pointer, it should be returned:

```c
struct fatptr
fill(struct fatptr ptr, int value)
{
    int *buf = ptr.ptr;
    for (size_t i = ptr.len; i < ptr.cap; i++) {
        buf[i] = value;
    }
    ptr.len = ptr.cap;
    return ptr;
}
```

Congratulations, you've got slices! Except that in Go they're a proper
part of the language and so doesn't rely on hazardous hacks or tedious
bookkeeping. The `fatptr_write()` function above is nearly functionally
equivalent to the `Writer.Write()` method in Go, which accepts a slice:

```go
type Writer interface {
	Write(p []byte) (n int, err error)
}
```

The `buf` and `count` parameters are packed together as a slice, and
`fd` parameter is instead the *receiver* (the object being acted upon by
the method).

### Go slices

Go famously has pointers, including *internal pointers*, but not pointer
arithmetic. You can take the address of ([nearly][addr]) anything, but
you can't make that pointer point at anything else, even if you took the
address of an array element. Pointer arithmetic would undermine Go's
type safety, so it can only be done through special mechanisms in the
`unsafe` package.

But pointer arithmetic is really useful! It's handy to take an address
of an array element, pass it to a function, and allow that function to
modify a *slice* (wink, wink) of the array. **Slices are pointers that
support exactly this sort of pointer arithmetic, but safely.** Unlike
the `&` operator which creates a simple pointer, the slice operator
derives a fat pointer.

```go
func fill([]int, int) []int

var array [8]int

// len == 0, cap == 8, like &array[0]
fill(array[:0], 1)
// array is [1, 1, 1, 1, 1, 1, 1, 1]

// len == 0, cap == 4, like &array[4]
fill(array[4:4], 2)
// array is [1, 1, 1, 1, 2, 2, 2, 2]
```

The `fill` function could take a slice of the slice, effectively moving
the pointer around with pointer arithmetic, but without violating memory
safety due to the additional "fat pointer" information. In other words,
fat pointers can be derived from other fat pointers.

Slices aren't as universal as pointers, at least at the moment. You can
take the address of any variable using `&`, but you can't take a *slice*
of any variable, even if it would be logically sound.

```go
var foo int

// attempt to make len = 1, cap = 1 slice backed by foo
var fooslice []int = foo[:]   // compile-time error!
```

That wouldn't be very useful anyway. However, if you *really* wanted to
do this, the `unsafe` package can accomplish it. I believe the resulting
slice would be perfectly safe to use:

```go
// Convert to one-element array, then slice
fooslice = (*[1]int)(unsafe.Pointer(&foo))[:]
```

Update: [Chris Siebenmann speculated about why this requires
`unsafe`][cs].

Of course, slices are super flexible and have many more uses that look
less like fat pointers, but this is still how I tend to reason about
slices when I write Go.


[addr]: https://utcc.utoronto.ca/~cks/space/blog/programming/GoAddressableValues
[bb]: https://www.usenix.org/legacy/event/sec09/tech/full_papers/akritidis.pdf
[cs]: https://utcc.utoronto.ca/~cks/space/blog/programming/GoVariableToArrayConversion
[disc]: /blog/2017/07/19/
[hn]: https://news.ycombinator.com/item?id=20321116
[uintptr]: /blog/2016/05/30/
