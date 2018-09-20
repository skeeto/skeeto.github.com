---
title: You Can't Always Hash Pointers in C
layout: post
date: 2016-05-30T23:59:46Z
tags: [c, cpp]
uuid: 0fa3c99b-88ed-3a02-0342-4ee7536cc7ed
---

Occasionally I've needed to key a hash table with C pointers. I don't
care about the contents of the object itself — especially if it might
change — just its pointer identity. For example, suppose I'm using
null-terminated strings as keys and I know these strings will always
be interned in a common table. These strings can be compared directly
by their pointer values (`str_a == str_b`) rather than, more slowly,
by their contents (`strcmp(str_a, str_b) == 0`). The intern table
ensures that these expressions both have the same result.

As a key in a hash table, or other efficient map/dictionary data
structure, I'll need to turn pointers into numerical values. However,
**C pointers aren't integers**. Following certain rules it's permitted
to cast pointers to integers and back, but doing so will reduce the
program's portability. The most important consideration is that **the
integer form isn't guaranteed to have any meaningful or stable
value**. In other words, even in a conforming implementation, the same
pointer might cast to two different integer values. This would break
any algorithm that isn't keenly aware of the implementation details.

To show why this is, I'm going to be citing the relevant parts of the
C99 standard (ISO/IEC 9899:1999). The [draft for C99][draft] is freely
available (and what I use myself since I'm a cheapass). My purpose is
*not* to discourage you from casting pointers to integers and using
the result. The vast majority of the time this works fine and as you
would expect. I just think it's an interesting part of the language,
and C/C++ programmers should be aware of potential the trade-offs.

### Integer to pointer casts

What does the standard have to say about casting pointers to integers?
§6.3.2.3¶5:

> An integer may be converted to any pointer type. Except as
> previously specified, the result is implementation-defined, might
> not be correctly aligned, might not point to an entity of the
> referenced type, and might be a trap representation.

It also includes a footnote:

> The mapping functions for converting a pointer to an integer or an
> integer to a pointer are intended to be consistent with the
> addressing structure of the execution environment.

Casting an integer to a pointer depends entirely on the
implementation. This is intended for things like memory mapped
hardware. The programmer may need to access memory as a specific
physical address, which would be encoded in the source as an integer
constant and cast to a pointer of the appropriate type.

~~~c
int
read_sensor_voltage(void)
{
    return *(int *)0x1ffc;
}
~~~

It may also be used by a loader and dynamic linker to compute the
virtual address of various functions and variables, then cast to a
pointer before use.

Both cases are already dependent on implementation defined behavior,
so there's nothing lost in relying on these casts.

An integer constant expression of 0 is a special case. It casts to a
NULL pointer in all implementations (§6.3.2.3¶3). However, a NULL
pointer doesn't necessarily point to address zero, nor is it
necessarily a zero bit pattern (i.e. beware `memset` and `calloc` on
memory with pointers). It's just guaranteed never to compare equally
with a valid object, and it is undefined behavior to dereference.

### Pointer to integer casts

What about the other way around? §6.3.2.3¶6:

> Any pointer type may be converted to an integer type. Except as
> previously specified, the result is implementation-defined. If the
> result cannot be represented in the integer type, the behavior is
> undefined. The result need not be in the range of values of any
> integer type.

Like before, it's implementation defined. However, the negatives are a
little stronger: the cast itself may be undefined behavior. I
speculate this is tied to integer overflow. The last part makes
pointer to integer casts optional for an implementation. This is one
way that the hash table above would be less portable.

When the cast is always possible, an implementation can provide an
integer type wide enough to hold any pointer value. §7.18.1.4¶1:

> The following type designates a signed integer type with the
> property that any valid pointer to void can be converted to this
> type, then converted back to pointer to void, and the result will
> compare equal to the original pointer:
>
> `intptr_t`
>
> The following type designates an unsigned integer type with the
> property that any valid pointer to void can be converted to this
> type, then converted back to pointer to void, and the result will
> compare equal to the original pointer:
>
> `uintptr_t`
>
> These types are optional.

The take-away is that the integer has no meaningful value. The only
guarantee is that the integer can be cast back into a void pointer
that will compare equally. It would be perfectly legal for an
implementation to pass these assertions (and still sometimes fail).

~~~c
void
example(void *ptr_a, void *ptr_b)
{
    if (ptr_a == ptr_b) {
        uintptr_t int_a = (uintptr_t)ptr_a;
        uintptr_t int_b = (uintptr_t)ptr_b;
        assert(int_a != int_b);
        assert((void *)int_a == (void *)int_b);
    }
}
~~~

Since the bits don't have any particular meaning, arithmetic
operations involving them will also have no meaning. When a pointer
might map to two different integers, the hash values might not match
up, breaking hash tables that rely on them. Even with `uintptr_t`
provided, casting pointers to integers isn't useful without also
relying on implementation defined properties of the result.

### Reasons for this pointer insanity

What purpose could such strange pointer-to-integer casts serve?

A security-conscious implementation may choose to annotate pointers
with additional information by setting unused bits. It might be for
[baggy bounds checks][baggy] or, someday, in an [undefined behavior
sanitizer][ubsan]. Before dereferencing annotated pointers, the
metadata bits would be checked for validity, and cleared/set before
use as an address. Or it may [map the same object at multiple virtual
addresses][map]) to avoid setting/clearing the metadata bits,
providing interoperability with code unaware of the annotations. When
pointers are compared, these bits would be ignored.

When these annotated pointers are cast to integers, the metadata bits
will be present, but a program using the integer wouldn't know their
meaning without tying itself closely to that implementation.
Completely unused bits may even be filled with random garbage when
cast. It's allowed.

You may have been thinking before about using a union or `char *` to
bypass the cast and access the raw pointer bytes, but you'd run into
the same problems on the same implementations.

### Conforming programs

The standard makes a distinction between *strictly conforming
programs* (§4¶5) and *conforming programs* (§4¶7). A strictly
conforming program must not produce output depending on implementation
defined behavior nor exceed minimum implementation limits. Very few
programs fit in this category, including any program using `uintptr_t`
since it's optional. Here are more examples of code that isn't
strictly conforming:

~~~c
    printf("%zu", sizeof(int)); // §6.5.3.4
    printf("%d", -1 >> 1);      // §6.5¶4
    printf("%d", MAX_INT);      // §5.2.4.2.1
~~~

On the other hand, a *conforming program* is allowed to depend on
implementation defined behavior. Relying on meaningful, stable values
for pointers cast to `uintptr_t`/`intptr_t` is conforming even if your
program may exhibit bugs on some implementations.


[draft]: http://www.open-std.org/jtc1/sc22/WG14/www/docs/n1256.pdf
[baggy]: https://www.usenix.org/legacy/event/sec09/tech/full_papers/akritidis.pdf
[map]: /blog/2016/04/10/
[ubsan]: http://clang.llvm.org/docs/UndefinedBehaviorSanitizer.html
