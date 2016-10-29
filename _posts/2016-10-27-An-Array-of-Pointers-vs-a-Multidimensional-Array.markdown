---
title: An Array of Pointers vs. a Multidimensional Array
layout: post
date: 2016-10-27T21:01:33Z
tags: [c, linux, x86, optimization]
uuid: d1302ff9-f958-3486-134d-01c8ab84aa51
---

In a C program, suppose I have a table of color names of similar
length. There are two straightforward ways to construct this table.
The most common would be an array of `char *`.

~~~c
char *colors_ptr[] = {
    "red",
    "orange",
    "yellow",
    "green",
    "blue",
    "violet"
};
~~~

The other is a two-dimensional `char` array.

~~~c
char colors_2d[][7] = {
    "red",
    "orange",
    "yellow",
    "green",
    "blue",
    "violet"
};
~~~

The initializers are identical, and the syntax by which these tables
are used is the same, but the underlying data structures are very
different. For example, suppose I had a lookup() function that
searches the table for a particular color.

~~~c
int
lookup(const char *color)
{
    int ncolors = sizeof(colors) / sizeof(colors[0]);
    for (int i = 0; i < ncolors; i++)
        if (strcmp(colors[i], color) == 0)
            return i;
    return -1;
}
~~~

Thanks to array decay — array arguments are implicitly converted to
pointers (§6.9.1-10) — it doesn't matter if the table is `char
colors[][7]` or `char *colors[]`. It's a little bit misleading because
the compiler generates different code depending on the type.

### Memory Layout

Here's what `colors_ptr`, a *jagged array*, typically looks like in
memory.

![](/img/colortab/pointertab.png)

The array of six pointers will point into the program's string table,
usually stored in a separate page. The strings aren't in any
particular order and will be interspersed with the program's other
string constants. The type of the expression `colors_ptr[n]` is `char *`.

On x86-64, suppose the base of the table is in `rax`, the index of the
string I want to retrieve is `rcx`, and I want to put the string's
address back into `rax`. It's one load instruction.

~~~nasm
mov   rax, [rax + rcx*8]
~~~

Contrast this with `colors_2d`: six 7-byte elements in a row. No
pointers or addresses. Only strings.

![](/img/colortab/arraytab.png)

The strings are in their defined order, packed together. The type of
the expression `colors_2d[n]` is `char [7]`, an array rather than a
pointer. If this was a large table used by a hot function, it would
have friendlier cache characteristics — both in locality and
predictability.

In the same scenario before with x86-64, it takes two instructions to
put the string's address in `rax`, but neither is a load.

~~~nasm
imul  rcx, rcx, 7
add   rax, rcx
~~~

In this particular case, the generated code can be slightly improved
by increasing the string size to 8 (e.g. `char colors_2d[][8]`). The
multiply turns into a simple shift and the ALU no longer needs to be
involved, cutting it to one instruction. This looks like a load due to
the LEA (Load Effective Address), but it's not.

~~~nasm
lea   rax, [rax + rcx*8]
~~~

### Relocation

There's another factor to consider: relocation. Nearly every process
running on a modern system takes advantage of a security feature
called Address Space Layout Randomization (ASLR). The virtual address
of code and data is randomized at process load time. For shared
libraries, it's not just a security feature, it's essential to their
basic operation. Libraries cannot possibly coordinate their preferred
load addresses with every other library on the system, and so must be
relocatable.

If the program is compiled with GCC or Clang configured for position
independent code — `-fPIC` (for libraries) or `-fpie` + `-pie` (for
programs) — extra work has to be done to support `colors_ptr`. Those
are all addresses in the pointer array, but the compiler doesn't know
what those addresses will be. The compiler fills the elements with
temporary values and adds six relocation entries to the binary, one
for each element. The loader will fill out the array at load time.

However, `colors_2d` doesn't have any addresses other than the address
of the table itself. The loader doesn't need to be involved with each
of its elements. Score another point for the two-dimensional array.

On x86-64, in both cases the table itself typically doesn't need a
relocation entry because it will be *RIP-relative* (in the [small code
model][mm]). That is, code that uses the table will be at a fixed
offset from the table no matter where the program is loaded. It won't
need to be looked up using the Global Offset Table (GOT).

In case you're ever reading compiler output, in Intel syntax the
assembly for putting the table's RIP-relative address in `rax` looks
like so:

~~~nasm
;; NASM:
lea    rax, [rel address]
;; Some others:
lea    rax, [rip + address]
~~~

Or in AT&T syntax:

~~~gas
lea    address(%rip), %rax
~~~

### Virtual Memory

Besides (trivially) more work for the loader, there's another
consequence to relocations: Pages containing relocations are not
shared between processes (except after fork()). When loading a
program, the loader doesn't copy programs and libraries to memory so
much as it memory maps their binaries with copy-on-write semantics. If
another process is running with the same binaries loaded (e.g.
libc.so), they'll share the same physical memory so long as those
pages haven't been modified by either process. Modifying the page
creates a unique copy for that process.

Relocations modify parts of the loaded binary, so these pages aren't
shared. This means `colors_2d` has the possibility of being shared
between processes, but `colors_ptr` (and its entire page) definitely
does not. Shucks.

This is one of the reasons why the Procedure Linkage Table (PLT)
exists. The PLT is an array of function stubs for shared library
functions, such as those in the C standard library. Sure, the loader
*could* go through the program and fill out the address of every
library function call, but this would modify lots and lots of code
pages, creating a unique copy of large parts of the program. Instead,
the dynamic linker [lazily supplies jump addresses][plt] for PLT
function stubs, one per accessed library function.

However, as I've written it above, it's unlikely that even `colors_2d`
will be shared. It's still missing an important ingredient: const.

### Const

They say [const isn't for optimization][const] but, darnit, this
situation keeps coming up. Since `colors_ptr` and `colors_2d` are both
global, writable arrays, the compiler puts them in the same writable
data section of the program, and, in my test program, they end up
right next to each other in the same page. The other relocations doom
`colors_2d` to being a local copy.

Fortunately it's trivial to fix by adding a const:

~~~c
const char colors_2d[][7] = { /* ... */ };
~~~

Writing to this memory is now undefined behavior, so the compiler is
free to put it in read-only memory (`.rodata`) and separate from the
dirty relocations. On my system, this is close enough to the code to
wind up in executable memory.

Note, the equivalent for `colors_ptr` requires two const qualifiers,
one for the array and another for the strings. (Obviously the const
doesn't apply to the loader.)

~~~c
const char *const colors_ptr[] = { /* ... */ };
~~~

String literals are already effectively const, though the C
specification (unlike C++) doesn't actually define them to be this
way. But, like setting your relationship status on Facebook, declaring
it makes it official.

### It's just micro-optimization

These little details are all deep down the path of micro-optimization
and will rarely ever matter in practice, but perhaps you learned
something broader from all this. This stuff fascinates me.


[const]: /blog/2016/07/25/
[mm]: http://eli.thegreenplace.net/2012/01/03/understanding-the-x64-code-models
[plt]: https://www.technovelty.org/linux/plt-and-got-the-key-to-code-sharing-and-dynamic-libraries.html
