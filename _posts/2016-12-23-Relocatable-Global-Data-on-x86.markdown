---
title: Relocatable Global Data on x86
layout: post
date: 2016-12-23T22:50:51Z
tags: [c, x86, optimization, linux]
uuid: 56be19e0-ce9a-3f37-dc85-578f397ed3e1
---

Relocatable code — program code that executes correctly from any
properly-aligned address — is an essential feature for shared
libraries. Otherwise all of a system's shared libraries would need to
coordinate their virtual load addresses. Loading programs and
libraries to random addresses is also a valuable security feature:
Address Space Layout Randomization (ASLR). But how does a compiler
generate code for a function that accesses a global variable if that
variable's address isn't known at compile time?

Consider this simple C code sample.

~~~c
static const float values[] = {1.1f, 1.2f, 1.3f, 1.4f};

float get_value(unsigned x)
{
    return x < 4 ? values[x] : 0.0f;
}
~~~

This function needs the base address of `values` in order to
dereference it for `values[x]`. The easiest way to find out how this
works, especially without knowing where to start, is to compile the
code and have a look! I'll compile for x86-64 with GCC 4.9.2 (Debian
Jessie).

    $ gcc -c -Os -fPIC get_value.c

I optimized for size (`-Os`) to make the disassembly easier to follow.
Next, disassemble this pre-linked code with `objdump`. Alternatively I
could have asked for the compiler's assembly output with `-S`, but
this will be good reverse engineering practice.

    $ objdump -d -Mintel get_value.o
    0000000000000000 <get_value>:
       0:   83 ff 03                cmp    edi,0x3
       3:   0f 57 c0                xorps  xmm0,xmm0
       6:   77 0e                   ja     16 <get_value+0x16>
       8:   48 8d 05 00 00 00 00    lea    rax,[rip+0x0]
       f:   89 ff                   mov    edi,edi
      11:   f3 0f 10 04 b8          movss  xmm0,DWORD PTR [rax+rdi*4]
      16:   c3                      ret

There are a couple of interesting things going on, but let's start
from the beginning.

1. [The ABI][abi] specifies that the first integer/pointer argument
   (the 32-bit integer `x`) is passed through the `edi` register. The
   function compares `x` to 3, to satisfy `x < 4`.

2. The ABI specifies that floating point values are returned through
   the [SSE2 SIMD register][simd] `xmm0`. It's cleared by XORing the
   register with itself — the conventional way to clear registers on
   x86 — setting up for a return value of `0.0f`.

3. It then uses the result of the previous comparison to perform a
   jump, `ja` ("jump if after"). That is, jump to the relative address
   specified by the jump's operand if the first operand to `cmp`
   (`edi`) comes after the first operand (`0x3`) as *unsigned* values.
   Its cousin, `jg` ("jump if greater"), is for signed values. If `x`
   is outside the array bounds, it jumps straight to `ret`, returning
   `0.0f`.

4. If `x` was in bounds, it uses a `lea` ("load effective address") to
   load *something* into the 64-bit `rax` register. This is the
   complicated bit, and I'll start by giving the answer: The value
   loaded into `rax` is the address of the `values` array. More on
   this in a moment.

5. Finally it uses `x` as an index into address in `rax`. The `movss`
   ("move scalar single-precision") instruction loads a 32-bit float
   into the first lane of `xmm0`, where the caller expects to find the
   return value. This is all preceded by a `mov edi, edi` which
   [*looks* like a hotpatch nop][hp], but it isn't. x86-64 always uses
   64-bit registers for addressing, meaning it uses `rdi` not `edi`.
   All 32-bit register assignments clear the upper 32 bits, and so
   this `mov` zero-extends `edi` into `rdi`. This is in case of the
   unlikely event that the caller left garbage in those upper bits.

### Clearing `xmm0`

The first interesting part: `xmm0` is cleared even when its first lane
is loaded with a value. There are two reasons to do this.

The obvious reason is that the alternative requires additional
instructions, and I told GCC to optimize for size. It would need
either an extra `ret` or an conditional `jmp` over the "else" branch.

The less obvious reason is that it breaks a *data dependency*. For
over 20 years now, x86 micro-architectures have employed an
optimization technique called [register renaming][rr]. *Architectural
registers* (`rax`, `edi`, etc.) are just temporary names for
underlying *physical registers*. This disconnect allows for more
aggressive out-of-order execution. Two instructions sharing an
architectural register can be executed independently so long as there
are no data dependencies between these instructions.

For example, take this assembly sample. It assembles to 9 bytes of
machine code.

~~~nasm
    mov  edi, [rcx]
    mov  ecx, 7
    shl  eax, cl
~~~

This reads a 32-bit value from the address stored in `rcx`, then
assigns `ecx` and uses `cl` (the lowest byte of `rcx`) in a shift
operation. Without register renaming, the shift couldn't be performed
until the load in the first instruction completed. However, the second
instruction is a 32-bit assignment, which, as I mentioned before, also
clears the upper 32 bits of `rcx`, wiping the unused parts of
register.

So after the second instruction, it's guaranteed that the value in
`rcx` has no dependencies on code that comes before it. Because of
this, it's likely a different physical register will be used for the
second and third instructions, allowing these instructions to be
executed out of order, *before* the load. Ingenious!

Compare it to this example, where the second instruction assigns to
`cl` instead of `ecx`. This assembles to just 6 bytes.

~~~nasm
    mov  edi, [rcx]
    mov  cl, 7
    shl  eax, cl
~~~

The result is 3 bytes smaller, but since it's not a 32-bit assignment,
the upper bits of `rcx` still hold the original register contents.
This creates a false dependency and may prevent out-of-order
execution, reducing performance.

By clearing `xmm0`, instructions in `get_value` involving `xmm0` have
the opportunity to be executed prior to instructions in the callee
that use `xmm0`.

### RIP-relative addressing

Going back to the instruction that computes the address of `values`.

       8:   48 8d 05 00 00 00 00    lea    rax,[rip+0x0]

Normally load/store addresses are absolute, based off an address
either in a general purpose register, or at some hard-coded base
address. The latter is not an option in relocatable code. With
*RIP-relative addressing* that's still the case, but the register with
the absolute address is `rip`, the instruction pointer. This
addressing mode was introduced in x86-64 to make relocatable code more
efficient.

That means this instruction copies the instruction pointer (pointing
to the next instruction) into `rax`, plus a 32-bit displacement,
currently zero. This isn't the right way to encode a displacement of
zero (unless you *want* a larger instruction). That's because the
displacement will be filled in later by the linker. The compiler adds
a *relocation entry* to the object file so that the linker knows how
to do this.

On platforms that [use ELF][elf] we can inspect relocations this with
`readelf`.

    $ readelf -r get_value.o

    Relocation section '.rela.text' at offset 0x270 contains 1 entries:
      Offset          Info           Type       Sym. Value
    00000000000b  000700000002 R_X86_64_PC32 0000000000000000 .rodata - 4

The relocation type is `R_X86_64_PC32`. In the [AMD64 Architecture
Processor Supplement][amd64], this is defined as "S + A - P".

* S: Represents the value of the symbol whose index resides in the
  relocation entry.

* A: Represents the addend used to compute the value of the
  relocatable field.

* P: Represents the place of the storage unit being relocated.

The symbol, S, is `.rodata` — the final address for this object file's
portion of `.rodata` (where `values` resides). The addend, A, is `-4`
since the instruction pointer points at the *next* instruction. That
is, this will be relative to four bytes after the relocation offset.
Finally, the address of the relocation, P, is the address of last four
bytes of the `lea` instruction. These values are all known at
link-time, so no run-time support is necessary.

Being "S - P" (overall), this will be the displacement between these
two addresses: the 32-bit value is relative. It's relocatable so long
as these two parts of the binary (code and data) maintain a fixed
distance from each other. The binary is relocated as a whole, so this
assumption holds.

### 32-bit relocation

Since RIP-relative addressing wasn't introduced until x86-64, how did
this all work on x86? Again, let's just see what the compiler does.
Add the `-m32` flag for a 32-bit target, and `-fomit-frame-pointer` to
make it simpler for explanatory purposes.

    $ gcc -c -m32 -fomit-frame-pointer -Os -fPIC get_value.c
    $ objdump -d -Mintel get_value.o
    00000000 <get_value>:
       0:   8b 44 24 04             mov    eax,DWORD PTR [esp+0x4]
       4:   d9 ee                   fldz
       6:   e8 fc ff ff ff          call   7 <get_value+0x7>
       b:   81 c1 02 00 00 00       add    ecx,0x2
      11:   83 f8 03                cmp    eax,0x3
      14:   77 09                   ja     1f <get_value+0x1f>
      16:   dd d8                   fstp   st(0)
      18:   d9 84 81 00 00 00 00    fld    DWORD PTR [ecx+eax*4+0x0]
      1f:   c3                      ret

    Disassembly of section .text.__x86.get_pc_thunk.cx:

    00000000 <__x86.get_pc_thunk.cx>:
       0:   8b 0c 24                mov    ecx,DWORD PTR [esp]
       3:   c3                      ret

Hmm, this one includes an extra function.

1. In this calling convention, arguments are passed on the stack. The
   first instruction loads the argument, `x`, into `eax`.

2. The `fldz` instruction clears the x87 floating pointer return
   register, just like clearing `xmm0` in the x86-64 version.

3. Next it calls `__x86.get_pc_thunk.cx`. The call pushes the
   instruction pointer, `eip`, onto the stack. This function reads
   that value off the stack into `ecx` and returns. In other words,
   calling this function copies `eip` into `ecx`. It's setting up to
   load data at an address relative to the code. Notice the function
   name starts with two underscores — a name which is reserved for
   exactly for these sorts of implementation purposes.

4. Next a 32-bit displacement is added to `ecx`. In this case it's
   `2`, but, like before, this is actually going be filled in later by
   the linker.

5. Then it's just like before: a branch to optionally load a value.
   The floating pointer load (`fld`) is another relocation.

Let's look at the relocations. There are three this time:

    $ readelf -r get_value.o

    Relocation section '.rel.text' at offset 0x2b0 contains 3 entries:
     Offset     Info    Type        Sym.Value  Sym. Name
    00000007  00000e02 R_386_PC32    00000000   __x86.get_pc_thunk.cx
    0000000d  00000f0a R_386_GOTPC   00000000   _GLOBAL_OFFSET_TABLE_
    0000001b  00000709 R_386_GOTOFF  00000000   .rodata

The first relocation is the call-site for the thunk. The thunk has
external linkage and may be merged with a matching thunk in another
object file, and so may be relocated. (Clang inlines its thunk.) Calls
are relative, so its type is `R_386_PC32`: a code-relative
displacement just like on x86-64.

The next is of type `R_386_GOTPC` and sets the second operand in that
`add ecx`. It's defined as "GOT + A - P" where "GOT" is the address of
the Global Offset Table — a table of addresses of the binary's
relocated objects. Since `values` is static, the GOT won't actually
hold an address for it, but the relative address of the GOT itself
will be useful.

The final relocation is of type `R_386_GOTOFF`. This is defined as
"S + A - GOT". Another displacement between two addresses. This is the
displacement in the load, `fld`. Ultimately the load adds these last
two relocations together, canceling the GOT:

      (GOT + A0 - P) + (S + A1 - GOT)
    = S + A0 + A1 - P

So the GOT isn't relevant in this case. It's just a mechanism for
constructing a custom relocation type.

### Branch optimization

Notice in the x86 version the thunk is called before checking the
argument. What if it's most likely that will `x` be out of bounds of
the array, and the function usually returns zero? That means it's
usually wasting its time calling the thunk. Without profile-guided
optimization the compiler probably won't know this.

The typical way to provide such a compiler hint is with a pair of
macros, `likely()` and `unlikely()`. With GCC and Clang, these would
be defined to use `__builtin_expect`. Compilers without this sort of
feature would have macros that do nothing instead. So I gave it a
shot:

~~~c
#define likely(x)    __builtin_expect((x),1)
#define unlikely(x)  __builtin_expect((x),0)

static const float values[] = {1.1f, 1.2f, 1.3f, 1.4f};

float get_value(unsigned x)
{
    return unlikely(x < 4) ? values[x] : 0.0f;
}
~~~

Unfortunately this makes no difference even in the latest version of
GCC. In Clang it changes branch fall-through (for [static branch
prediction][sbp]), but still always calls the thunk. It seems
compilers [have difficulty][pic] with [optimizing relocatable
code][pic2] on x86.

### x86-64 isn't just about more memory

It's commonly understood that the advantage of 64-bit versus 32-bit
systems is processes having access to more than 4GB of memory. But as
this shows, there's more to it than that. Even programs that don't
need that much memory can really benefit from newer features like
RIP-relative addressing.


[abi]: https://github.com/hjl-tools/x86-psABI/wiki/x86-64-psABI-secure.pdf
[simd]: /blog/2015/07/10/
[rr]: https://en.wikipedia.org/wiki/Register_renaming
[hp]: /blog/2016/03/31/
[elf]: /blog/2016/11/17/
[amd64]: http://math-atlas.sourceforge.net/devel/assembly/abi_sysV_amd64.pdf
[pic]: https://ewontfix.com/18/
[pic2]: https://gcc.gnu.org/bugzilla/show_bug.cgi?id=54232
[sbp]: http://www.agner.org/optimize/microarchitecture.pdf
