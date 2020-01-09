---
title: A Basic Just-In-Time Compiler
layout: post
date: 2015-03-19T04:57:55Z
tags: [c, tutorial, netsec, x86, posix, optimization]
uuid: 95e0437f-61f0-3932-55b7-f828e171d9ca
excerpt_separator: <!--more-->
---

*This article was discussed [on Hacker News][hn] and [on reddit][reddit].*

[Monday's /r/dailyprogrammer challenge][dp] was to write a program to
read a recurrence relation definition and, through interpretation,
iterate it to some number of terms. It's given an initial term
(`u(0)`) and a sequence of operations, `f`, to apply to the previous
term (`u(n + 1) = f(u(n))`) to compute the next term. Since it's an
easy challenge, the operations are limited to addition, subtraction,
multiplication, and division, with one operand each.

<!--more-->

For example, the relation `u(n + 1) = (u(n) + 2) * 3 - 5` would be
input as `+2 *3 -5`. If `u(0) = 0` then,

* `u(1) = 1`
* `u(2) = 4`
* `u(3) = 13`
* `u(4) = 40`
* `u(5) = 121`
* ...

Rather than write an interpreter to apply the sequence of operations,
for [my submission][mine] ([mirror][mirror]) I took the opportunity to
write a simple x86-64 Just-In-Time (JIT) compiler. So rather than
stepping through the operations one by one, my program converts the
operations into native machine code and lets the hardware do the work
directly. In this article I'll go through how it works and how I did
it.

**Update**: The [follow-up challenge][part2] uses Reverse Polish
notation to allow for more complicated expressions. I wrote another
JIT compiler for [my submission][mine2] ([mirror][mirror2]).

### Allocating Executable Memory

Modern operating systems have page-granularity protections for
different parts of [process memory][memory]: read, write, and execute.
Code can only be executed from memory with the execute bit set on its
page, memory can only be changed when its write bit is set, and some
pages aren't allowed to be read. In a running process, the pages
holding program code and loaded libraries will have their write bit
cleared and execute bit set. Most of the other pages will have their
execute bit cleared and their write bit set.

The reason for this is twofold. First, it significantly increases the
security of the system. If untrusted input was read into executable
memory, an attacker could input machine code (*shellcode*) into the
buffer, then exploit a flaw in the program to cause control flow to
jump to and execute that code. If the attacker is only able to write
code to non-executable memory, this attack becomes a lot harder. The
attacker has to rely on code already loaded into executable pages
([*return-oriented programming*][rop]).

Second, it catches program bugs sooner and reduces their impact, so
there's less chance for a flawed program to accidentally corrupt user
data. Accessing memory in an invalid way will causes a segmentation
fault, usually leading to program termination. For example, `NULL`
points to a special page with read, write, and execute disabled.

#### An Instruction Buffer

Memory returned by `malloc()` and friends will be writable and
readable, but non-executable. If the JIT compiler allocates memory
through `malloc()`, fills it with machine instructions, and jumps to
it without doing any additional work, there will be a segmentation
fault. So some different memory allocation calls will be made instead,
with the details hidden behind an `asmbuf` struct.

~~~c
#define PAGE_SIZE 4096

struct asmbuf {
    uint8_t code[PAGE_SIZE - sizeof(uint64_t)];
    uint64_t count;
};
~~~

To keep things simple here, I'm just assuming the page size is 4kB. In
a real program, we'd use `sysconf(_SC_PAGESIZE)` to discover the page
size at run time. On x86-64, pages may be 4kB, 2MB, or 1GB, but this
program will work correctly as-is regardless.

Instead of `malloc()`, the compiler allocates memory as an anonymous
memory map (`mmap()`). It's anonymous because it's not backed by a
file.

~~~c
struct asmbuf *
asmbuf_create(void)
{
    int prot = PROT_READ | PROT_WRITE;
    int flags = MAP_ANONYMOUS | MAP_PRIVATE;
    return mmap(NULL, PAGE_SIZE, prot, flags, -1, 0);
}
~~~

Windows doesn't have POSIX `mmap()`, so on that platform we use
`VirtualAlloc()` instead. Here's the equivalent in Win32.

~~~c
struct asmbuf *
asmbuf_create(void)
{
    DWORD type = MEM_RESERVE | MEM_COMMIT;
    return VirtualAlloc(NULL, PAGE_SIZE, type, PAGE_READWRITE);
}
~~~

Anyone reading closely should notice that I haven't actually requested
that the memory be executable, which is, like, the whole point of all
this! This was intentional. Some operating systems employ a security
feature called W^X: "write xor execute." That is, memory is either
writable or executable, but never both at the same time. This makes
the shellcode attack I described before even harder. For [well-behaved
JIT compilers][wx] it means memory protections need to be adjusted
after code generation and before execution.

The POSIX `mprotect()` function is used to change memory protections.

~~~c
void
asmbuf_finalize(struct asmbuf *buf)
{
    mprotect(buf, sizeof(*buf), PROT_READ | PROT_EXEC);
}
~~~

Or on Win32 (that last parameter is not allowed to be `NULL`),

~~~c
void
asmbuf_finalize(struct asmbuf *buf)
{
    DWORD old;
    VirtualProtect(buf, sizeof(*buf), PAGE_EXECUTE_READ, &old);
}
~~~

Finally, instead of `free()` it gets unmapped.

~~~c
void
asmbuf_free(struct asmbuf *buf)
{
    munmap(buf, PAGE_SIZE);
}
~~~

And on Win32,

~~~c
void
asmbuf_free(struct asmbuf *buf)
{
    VirtualFree(buf, 0, MEM_RELEASE);
}
~~~

I won't list the definitions here, but there are two "methods" for
inserting instructions and immediate values into the buffer. This will
be raw machine code, so the caller will be acting a bit like an
assembler.

~~~c
asmbuf_ins(struct asmbuf *, int size, uint64_t ins);
asmbuf_immediate(struct asmbuf *, int size, const void *value);
~~~

### Calling Conventions

We're only going to be concerned with three of x86-64's many
registers: `rdi`, `rax`, and `rdx`. These are 64-bit (`r`) extensions
of [the original 16-bit 8086 registers][old]. The sequence of
operations will be compiled into a function that we'll be able to call
from C like a normal function. Here's what it's prototype will look
like. It takes a signed 64-bit integer and returns a signed 64-bit
integer.

~~~c
long recurrence(long);
~~~

[The System V AMD64 ABI calling convention][call] says that the first
integer/pointer function argument is passed in the `rdi` register.
When our JIT compiled program gets control, that's where its input
will be waiting. According to the ABI, the C program will be expecting
the result to be in `rax` when control is returned. If our recurrence
relation is merely the identity function (it has no operations), the
only thing it will do is copy `rdi` to `rax`.

~~~nasm
mov   rax, rdi
~~~

There's a catch, though. You might think all the mucky
platform-dependent stuff was encapsulated in `asmbuf`. Not quite. As
usual, Windows is the oddball and has its own unique calling
convention. For our purposes here, the only difference is that the
first argument comes in `rcx` rather than `rdi`. Fortunately this only
affects the very first instruction and the rest of the assembly
remains the same.

The very last thing it will do, assuming the result is in `rax`, is
return to the caller.

~~~nasm
ret
~~~

So we know the assembly, but what do we pass to `asmbuf_ins()`? This
is where we get our hands dirty.

#### Finding the Code

If you want to do this the Right Way, you go download the x86-64
documentation, look up the instructions we're using, and manually work
out the bytes we need and how the operands fit into it. You know, like
they used to do [out of necessity][needle] back in the 60's.

Fortunately there's a much easier way. We'll have an actual assembler
do it and just copy what it does. Put both of the instructions above
in a file `peek.s` and hand it to `nasm`. It will produce a raw binary
with the machine code, which we'll disassemble with `nidsasm` (the
NASM disassembler).

    $ nasm peek.s
    $ ndisasm -b64 peek
    00000000  4889F8            mov rax,rdi
    00000003  C3                ret

That's straightforward. The first instruction is 3 bytes and the
return is 1 byte.

~~~c
asmbuf_ins(buf, 3, 0x4889f8);  // mov   rax, rdi
// ... generate code ...
asmbuf_ins(buf, 1, 0xc3);      // ret
~~~

For each operation, we'll set it up so the operand will already be
loaded into `rdi` regardless of the operator, similar to how the
argument was passed in the first place. A smarter compiler would embed
the immediate in the operator's instruction if it's small (32-bits or
fewer), but I'm keeping it simple. To sneakily capture the "template"
for this instruction I'm going to use `0x0123456789abcdef` as the
operand.

~~~nasm
mov   rdi, 0x0123456789abcdef
~~~

Which disassembled with `ndisasm` is,

    00000000  48BFEFCDAB896745  mov rdi,0x123456789abcdef
             -2301

Notice the operand listed little endian immediately after the
instruction. That's also easy!

~~~c
long operand;
scanf("%ld", &operand);
asmbuf_ins(buf, 2, 0x48bf);         // mov   rdi, operand
asmbuf_immediate(buf, 8, &operand);
~~~~

Apply the same discovery process individually for each operator you
want to support, accumulating the result in `rax` for each.

~~~c
switch (operator) {
    case '+':
        asmbuf_ins(buf, 3, 0x4801f8);   // add   rax, rdi
        break;
    case '-':
        asmbuf_ins(buf, 3, 0x4829f8);   // sub   rax, rdi
        break;
    case '*':
        asmbuf_ins(buf, 4, 0x480fafc7); // imul  rax, rdi
        break;
    case '/':
        asmbuf_ins(buf, 3, 0x4831d2);   // xor   rdx, rdx
        asmbuf_ins(buf, 3, 0x48f7ff);   // idiv  rdi
        break;
}
~~~

As an exercise, try adding support for modulus operator (`%`), XOR
(`^`), and bit shifts (`<`, `>`). With the addition of these
operators, you could define a decent PRNG as a recurrence relation. It
will also eliminate the [closed form solution][cf] to this problem so
that we actually have a reason to do all this! Or, alternatively,
switch it all to floating point.

### Calling the Generated Code

Once we're all done generating code, finalize the buffer to make it
executable, cast it to a function pointer, and call it. (I cast it as
a `void *` just to avoid repeating myself, since that will implicitly
cast to the correct function pointer prototype.)

~~~c
asmbuf_finalize(buf);
long (*recurrence)(long) = (void *)buf->code;
// ...
x[n + 1] = recurrence(x[n]);
~~~

That's pretty cool if you ask me! Now this was an extremely simplified
situation. There's no branching, no intermediate values, no function
calls, and I didn't even touch the stack (push, pop). The recurrence
relation definition in this challenge is practically an assembly
language itself, so after the initial setup it's a 1:1 translation.

I'd like to build a JIT compiler more advanced than this in the
future. I just need to find a suitable problem that's more complicated
than this one, warrants having a JIT compiler, but is still simple
enough that I could, on some level, justify not using LLVM.


[call]: http://en.wikipedia.org/wiki/X86_calling_conventions#x86-64_calling_conventions
[cf]: https://old.reddit.com/r/dailyprogrammer/comments/2z68di/_/cpgkcx7
[dp]: http://redd.it/2z68di
[hn]: https://news.ycombinator.com/item?id=17747759
[memory]: http://marek.vavrusa.com/c/memory/2015/02/20/memory/
[mine2]: https://gist.github.com/anonymous/f7e4a5086a2b0acc83aa
[mine]: https://gist.github.com/skeeto/3a1aa3df31896c9956dc
[mirror2]: /download/rpn-jit.c
[mirror]: /download/jit.c
[needle]: /blog/2016/11/17/
[old]: /blog/2014/12/09/
[part2]: http://redd.it/2zna5q
[reddit]: https://old.reddit.com/r/programming/comments/akxq8q/a_basic_justintime_compiler/
[rop]: http://en.wikipedia.org/wiki/Return-oriented_programming
[wx]: http://www.tedunangst.com/flak/post/now-or-never-exec
