---
title: Baking Data with Serialization
layout: post
date: 2016-11-15T05:27:53Z
tags: [c, optimization]
uuid: 365d1301-72b9-39d1-8023-20fb83e046ab
---

Suppose you want to bake binary data directly into a program's
executable. It could be image pixel data (PNG, BMP, JPEG), a text
file, or some sort of complex data structure. Perhaps the purpose is
to build a single executable with no extraneous data files — easier to
install and manage, though harder to modify. Or maybe you're lazy and
don't want to worry about handling the various complications and
errors that arise when reading external data: Where to find it, and
what to do if you can't find it or can't read it. This article is
about two different approaches I've used a number of times for C
programs.

### The linker approach

The simpler, less portable option is to have the linker do it. Both
the GNU linker and the [gold linker][linkers] (ELF only) can create
object files from arbitrary files using the `--format` (`-b`) option
set to `binary` (raw data). It's combined with `--relocatable` (`-r`)
to make it linkable with the rest of the program. MinGW supports all
of this, too, so it's fairly portable so long as you stick to GNU
Binutils.

For example, to create an object file, `my_msg.o` with the
contents of the text file `my_msg.txt`:

    $ ld -r -b binary -o my_file.o my_msg.txt

(*Update*: [You probably also want to use `-z noexecstack`][execstack].)

The object file will have three symbols, each named after the input
file. Unfortunately there's no control over the symbol names, section
(.data), alignment, or protections (e.g. read-only). You're completely
at the whim of the linker, short of objcopy tricks.

    $ nm my_msg.o
    000000000000000e D _binary_my_msg_txt_end
    000000000000000e A _binary_my_msg_txt_size
    0000000000000000 D _binary_my_msg_txt_start

To access these in C, declare them as global variables like so:

~~~c
extern char _binary_my_msg_txt_start[];
extern char _binary_my_msg_txt_end[];
extern char _binary_my_msg_txt_size;
~~~

The size symbol, `_binary_my_msg_txt_size`, is misleading. The "A"
from nm means it's an absolute symbol, not relocated. It doesn't refer
to an integer that holds the size of the raw data. The value of the
symbol itself is the size of the data. That is, take the address of it
and cast it to an integer.

~~~c
size_t size = (size_t)&_binary_my_msg_txt_size;
~~~

Alternatively — and this is my own preference — just subtract the
other two symbols. It's cleaner and easier to understand.

~~~c
size_t size = _binary_my_msg_txt_end - _binary_my_msg_txt_start;
~~~

Here's the "Hello, world" for this approach (`hello.c`).

~~~c
#include <stdio.h>

extern char _binary_my_msg_txt_start[];
extern char _binary_my_msg_txt_end[];
extern char _binary_my_msg_txt_size;

int
main(void)
{
    size_t size = _binary_my_msg_txt_end - _binary_my_msg_txt_start;
    fwrite(_binary_my_msg_txt_start, size, 1, stdout);
    return 0;
}
~~~

The program has to use `fwrite()` rather than `fputs()` because the
data won't necessarily be null-terminated. That is, unless a null is
intentionally put at the end of the text file itself.

And for the build:

    $ cat my_msg.txt
    Hello, world!
    $ ld -r -b binary -o my_msg.o my_msg.txt
    $ gcc -o hello hello.c my_msg.o
    $ ./hello
    Hello, world!

If this was binary data, such as an image file, the program would
instead read the array as if it were a memory mapped file. In fact,
that's what it really is: the raw data memory mapped by the loader
before the program started.

#### How about a data structure dump?

This could be taken further to dump out some kinds of data structures.
For example, this program (`table_gen.c`) fills out a table of the
first 90 Fibonacci numbers and dumps it to standard output.

~~~c
#include <stdio.h>

#define TABLE_SIZE 90

long long table[TABLE_SIZE] = {1, 1};

int
main(void)
{
    for (int i = 2; i < TABLE_SIZE; i++)
        table[i] = table[i - 1] + table[i - 2];
    fwrite(table, sizeof(table), 1, stdout);
    return 0;
}
~~~

Build and run this intermediate helper program as part of the overall
build.

    $ gcc -std=c99 -o table_gen table_gen.c
    $ ./table_gen > table.bin
    $ ld -r -b binary -o table.o table.bin

And then the main program (`print_fib.c`) might look like:

~~~c
#include <stdio.h>

extern long long _binary_table_bin_start[];
extern long long _binary_table_bin_end[];

int
main(void)
{
    long long *start = _binary_table_bin_start;
    long long *end   = _binary_table_bin_end;
    for (long long *x = start; x < end; x++)
        printf("%lld\n", *x);
    return 0;
}
~~~

However, there are some good reasons not to use this feature in this
way:

1. The format of `table.bin` is specific to the host architecture
   (byte order, size, padding, etc.). If the host is the same as the
   target then this isn't a problem, but it will prohibit
   cross-compilation.

2. The linker has no information about the alignment requirements of
   the data. To the linker it's just a byte buffer. In the final
   program the `long long` array will not necessarily aligned properly
   for its type, meaning the above program might crash. The Right Way
   is to never dereference the data directly but rather `memcpy()` it
   into a properly-aligned variable, just as if the data was an
   unaligned buffer.

3. The data structure cannot use any pointers. Pointer values are
   meaningless to other processes and will be no different than
   garbage.

### Towards a more portable approach

There's an easy way to address all three of these problems *and*
eliminate the reliance on GNU linkers: serialize the data into C code.
*It's metaprogramming, baby.*

In the Fibonacci example, change the `fwrite()` in `table_gen.c` to
this:

~~~c
    printf("int table_size = %d;\n", TABLE_SIZE);
    printf("long long table[] = {\n");
    for (int i = 0; i < TABLE_SIZE; i++)
        printf("    %lldLL,\n", table[i]);
    printf("};\n");
~~~

The output of the program becomes text:

~~~c
int table_size = 90;
long long table[] = {
    1LL,
    1LL,
    2LL,
    3LL,
    /* ... */
    1779979416004714189LL,
    2880067194370816120LL,
}
~~~

And `print_fib.c` is changed to:

~~~c
#include <stdio.h>

extern int table_size;
extern long long table[];

int
main(void)
{
    for (int i = 0; i < table_size; i++)
        printf("%lld\n", table[i]);
    return 0;
}
~~~

Putting it all together:

    $ gcc -std=c99 -o table_gen table_gen.c
    $ ./table_gen > table.c
    $ gcc -std=c99 -o print_fib print_fib.c table.c

Any C compiler and linker could do all of this, no problem, making it
more portable. The intermediate metaprogram isn't a barrier to cross
compilation. It would be compiled for the host (typically identified
through `HOST_CC`) and the rest is compiled for the target (e.g.
`CC`).

The output of `table_gen.c` isn't dependent on any architecture,
making it cross-compiler friendly. There are also no alignment
problems because it's all visible to compiler. The type system isn't
being undermined.

### Dealing with pointers

The Fibonacci example doesn't address the pointer problem — it has no
pointers to speak of. So let's step it up to a trie using the [trie
from the previous article][last]. As a reminder, here it is:

~~~c
#define TRIE_ALPHABET_SIZE  4
#define TRIE_TERMINAL_FLAG  (1U << 0)

struct trie {
    struct trie *next[TRIE_ALPHABET_SIZE];
    struct trie *p;
    int i;
    unsigned flags;
};
~~~

Dumping these structures out raw would definitely be useless since
they're almost entirely pointer data. So instead, fill out an array of
these structures, referencing the array itself to build up the
pointers (later filled in by either the linker or the loader). This
code uses the [in-place breadth-first traversal technique][last] from
the previous article.

~~~c
void
trie_serialize(struct trie *t, const char *name)
{
    printf("struct trie %s[] = {\n", name);
    struct trie *head = t;
    struct trie *tail = t;
    t->p = NULL;
    size_t count = 0;
    while (head) {
        printf("    {​{");
        for (int i = 0; i < TRIE_ALPHABET_SIZE; i++) {
            struct trie *next = head->next[i];
            const char *comma = i ? ", " : "";
            if (next) {
                /* Add child to the queue. */
                tail->p = next;
                tail = next;
                next->p = NULL;
                /* Print the pointer to the child. */
                printf("%s%s + %zu", comma, name, ++count);
            } else {
                printf("%s0", comma);
            }
        }
        printf("}, 0, 0, %u},\n", head->flags & TRIE_TERMINAL_FLAG);
        head = head->p;
    }
    printf("};\n");
}
~~~

Remember that list of strings from before?

    AAAAA
    ABCD
    CAA
    CAD
    CDBD

Which looks like this?

![](/img/trie/trie.svg)

That serializes to this C code:

~~~c
struct trie root[] = {
    {​{root + 1, 0, root + 2, 0}, 0, 0, 0},
    {​{root + 3, root + 4, 0, 0}, 0, 0, 0},
    {​{root + 5, 0, 0, root + 6}, 0, 0, 0},
    {​{root + 7, 0, 0, 0}, 0, 0, 0},
    {​{0, 0, root + 8, 0}, 0, 0, 0},
    {​{root + 9, 0, 0, root + 10}, 0, 0, 0},
    {​{0, root + 11, 0, 0}, 0, 0, 0},
    {​{root + 12, 0, 0, 0}, 0, 0, 0},
    {​{0, 0, 0, root + 13}, 0, 0, 0},
    {​{0, 0, 0, 0}, 0, 0, 1},
    {​{0, 0, 0, 0}, 0, 0, 1},
    {​{0, 0, 0, root + 14}, 0, 0, 0},
    {​{root + 15, 0, 0, 0}, 0, 0, 0},
    {​{0, 0, 0, 0}, 0, 0, 1},
    {​{0, 0, 0, 0}, 0, 0, 1},
    {​{0, 0, 0, 0}, 0, 0, 1},
};
~~~

This trie can be immediately used at program startup without
initialization, and it can even have new nodes inserted into it. It's
not without its downsides, particularly because it's a trie:

1. It's *really* going to blow up the size of the binary, especially
   when it holds lots of strings. These nodes are anything but
   compact.

2. If the code is compiled to be position-independent (`-fPIC`), each
   of those nodes is going to hold multiple dynamic relocations,
   further exploding the size of the binary and [preventing the trie
   from being shared between processes][reloc]. It's 24 bytes per
   relocation on x86-64. This will also slow down program start up
   time. With just a few thousand strings, the simple test program was
   taking 5x longer to start (25ms instead of 5ms) than with an empty
   trie.

3. Even without being position-independent, the linker will have to
   resolve all the compile-time relocations. I was able to overwhelm
   linker and run it out of memory with just some tens of thousands of
   strings. This would make for a decent linker stress test.

This technique obviously doesn't scale well with trie data. You're
better off baking in the flat string list and building the trie at run
time — though you *could* compute the exact number of needed nodes at
compile time and statically allocate them (in .bss). I've personally
had much better luck with [other sorts of lookup tables][yavalath].
It's a useful tool for the C programmer's toolbelt.


[execstack]: /blog/2019/11/15/
[last]: /blog/2016/11/13/
[linkers]: http://www.airs.com/blog/archives/38
[reloc]: /blog/2016/10/27/
[yavalath]: https://github.com/skeeto/yavalath
