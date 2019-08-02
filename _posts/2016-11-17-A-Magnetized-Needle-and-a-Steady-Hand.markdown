---
title: A Magnetized Needle and a Steady Hand
layout: post
date: 2016-11-17T23:35:26Z
tags: [linux, x86]
uuid: 1abbb17d-9836-3efc-8493-52dd93a90736
---

Now they've gone an done it. An unidentified agency has spread a
potent computer virus across all the world's computers and deleted the
binaries for every copy of every software development tool. Even the
offline copies — it's *that* potent.

Most of the source code still exists, even for the compilers, and most
computer systems will continue operating without disruption, but no
new software can be developed unless it's written byte by byte in raw
machine code. Only *real programmers* can get anything done.

[![][comic]][xkcd]

The world's top software developers have been put to work
bootstrapping a C compiler (and others) completely from scratch so
that we can get back to normal. Without even an assembler, it's a
slow, tedious process.

In the mean time, rather than wait around for the bootstrap work to
complete, the rest of us have been assigned individual programs hit by
the virus. For example, many basic unix utilities have been wiped out,
and the bootstrap would benefit from having them. Having different
groups tackle each missing program will allow the bootstrap effort to
move forward somewhat in parallel. *At least that's what the compiler
nerds told us.* The real reason is that they're tired of being asked
if they're done yet, and these tasks will keep the rest of us quietly
busy.

Fortunately you and I have been assigned the easiest task of all:
**We're to write the `true` command from scratch.** We'll have to
figure it out byte by byte. The target is x86-64 Linux, which means
we'll need the following documentation:

1. [Executable and Linking Format (ELF) Specification][elf]. This is
   the binary format used by modern Unix-like systems, including
   Linux. A more convenient way to access this document is [`man 5
   elf`][man].

2. [Intel 64 and IA-32 Architectures Software Developer's
   Manual][intel] (Volume 2). This fully documents the instruction set
   and its encoding. It's all the information needed to write x86
   machine code by hand. The AMD manuals would work too.

3. [System V Application Binary Interface: AMD64 Architecture
   Processor Supplement][abi]. Only a few pieces of information are
   needed from this document, but more would be needed for a more
   substantial program.

4. Some magic numbers from header files.

### Manual Assembly

The program we're writing is `true`, whose behavior is documented as
"do nothing, successfully." All command line arguments are ignored and
no input is read. The program only needs to perform the exit system
call, immediately terminating the process.

According to the ABI document (3) Appendix A, the registers for system
call arguments are: `rdi`, `rsi`, `rdx`, `r10`, `r8`, `r9`. The system
call number goes in `rax`. The exit system call takes only one
argument, and that argument will be 0 (success), so `rdi` should be
set to zero. It's likely that it's already zero when the program
starts, but the ABI document says its contents are undefined (§3.4),
so we'll set it explicitly.

For Linux on x86-64, the system call number for exit is 60,
(/usr/include/asm/unistd_64.h), so `rax` will be set to 60, followed
by `syscall`.

~~~nasm
    xor  edi, edi
    mov  eax, 60
    syscall
~~~

There's no assembler available to turn this into machine code, so it
has to be assembled by hand. For that we need the Intel manual (2).

The first instruction is `xor`, so look up that mnemonic in the
manual. Like most x86 mnemonics, there are many different opcodes and
multiple ways to encode the same operation. For `xor`, we have 22
opcodes to examine.

![](/img/steady-hand/xor.png)

The operands are two 32-bit registers, so there are two options:
opcodes 0x31 and 0x33.

    31 /r      XOR r/m32, r32
    33 /r      XOR r32, r/m32

The "r/m32" means the operand can be either a register or the address
of a 32-bit region of memory. With two register operands, both
encodings are equally valid, both have the same length (2 bytes), and
neither is canonical, so the decision is entirely arbitrary. Let's
pick the first one, opcode 0x31, since it's listed first.

The "/r" after the opcode means the register-only operand ("r32" in
both cases) will be specified in the ModR/M byte. This is the byte
that immediately follows the opcode and specifies one of two of the
operands.

The ModR/M byte is broken into three parts: mod (2 bits), reg (3
bits), r/m (3 bits). This gets a little complicated, but if you stare
at Table 2-1 in the Intel manual for long enough it eventually makes
sense. In short, two high bits (11) for mod indicates we're working
with a register rather than a load. Here's where we're at for ModR/M:

    11 ??? ???

The order of the x86 registers is unintuitive: `ax`, `cx`, `dx`, `bx`,
`sp`, `bp`, `si`, `di`. With 0-indexing, that gives `di` a value of 7
(111 in binary). With `edi` as both operands, this makes ModR/M:

    11 111 111

Or, in hexadecimal, FF. And that's it for this instruction. With the
opcode (0x31) and the ModR/M byte (0xFF):

    31 FF

The encoding for `mov` is a bit different. Look it up and match the
operands. Like before, there are two possible options:

    B8+rd id   MOV r32, imm32
    C7 /0 id   MOV r/m32, imm32

In the `B8+rd` notation means the 32-bit register operand (*rd* for
"register double word") is added to the opcode instead of having a
ModR/M byte. It's followed by a 32-bit immediate value (*id* for
"integer double word"). That's a total of 5 bytes.

The "/0" in second means 0 goes in the "reg" field of ModR/M, and the
whole instruction is followed by the 32-bit immediate (id). That's a
total of 6 bytes. Since this is longer, we'll use the first encoding.

So, that's opcode `0xB8 + 0`, since `eax` is register number 0,
followed by 60 (0x3C) as a little endian, 4-byte value. Here's the
encoding for the second instruction:

    B8 3C 00 00 00

The final instruction is a cakewalk. There are no operands, it comes
in only one form of two opcode bytes.

    0F 05   SYSCALL

So the encoding for this instruction is:

    0F 05

Putting it all together the program is 9 bytes:

    31 FF B8 3C 00 00 00 0F 05

Aren't you glad you don't normally have to assemble entire programs by
hand?

### Constructing the ELF

Back in the old days you may have been able to simply drop these bytes
into a file and execute it. That's how [DOS COM programs worked][com].
But this definitely won't work if you tried it on Linux. Binaries must
be in the Executable and Linking Format (ELF). This format tells the
loader how to initialize the program in memory and how to start it.

Fortunately for this program we'll only need to fill out two
structures: the ELF header and one program header. The binary will be
the ELF header, followed immediately by the program header, followed
immediately by the program.

![](/img/steady-hand/elf.svg)

To fill this binary out, we'd use whatever method the virus left
behind for writing raw bytes to a file. For now I'll assume the `echo`
command is still available, and we'll use hexadecimal `\xNN` escapes
to write raw bytes. If this isn't available, you might need to use the
magnetic needle and steady hand method, or the butterflies.

The very first structure in an ELF file must be the ELF header, from
the ELF specification (1):

~~~c
    typedef struct {
        unsigned char e_ident[EI_NIDENT];
        uint16_t      e_type;
        uint16_t      e_machine;
        uint32_t      e_version;
        ElfN_Addr     e_entry;
        ElfN_Off      e_phoff;
        ElfN_Off      e_shoff;
        uint32_t      e_flags;
        uint16_t      e_ehsize;
        uint16_t      e_phentsize;
        uint16_t      e_phnum;
        uint16_t      e_shentsize;
        uint16_t      e_shnum;
        uint16_t      e_shstrndx;
    } ElfN_Ehdr;
~~~

No other data is at a fixed location because this header specifies
where it can be found. If you're writing a C program in the future,
once compilers have been bootstrapped back into existence, you can
access this structure in `elf.h`.

#### The ELF header

The `EI_NIDENT` macro is 16, so `e_ident` is 16 bytes. The first 4
bytes are fixed: 0x7F, E, L, F.

The 5th byte is called `EI_CLASS`: a 32-bit program (`ELFCLASS32` =
1) or a 64-bit program (`ELFCLASS64` = 2). This will be a 64-bit
program (2).

The 6th byte indicates the integer format (`EI_DATA`). The one we want
for x86-64 is `ELFDATA2LSB` (1), two's complement, little-endian.

The 7th byte is the ELF version (`EI_VERSION`), always 1 as of this
writing.

The 8th byte is the ABI (`ELF_OSABI`), which in this case is
`ELFOSABI_SYSV` (0).

The 9th byte is the version (`EI_ABIVERSION`), which is just 0 again.

The rest is zero padding.

So writing the ELF header:

    echo -ne '\x7FELF\x02\x01\x01\x00' > true
    echo -ne '\x00\x00\x00\x00\x00\x00\x00\x00' >> true

The next field is the `e_type`. This is an executable program, so it's
`ET_EXEC` (2). Other options are object files (`ET_REL` = 1), shared
libraries (`ET_DYN` = 3), and core files (`ET_CORE` = 4).

    echo -ne '\x02\x00' >> true

The value for `e_machine` is `EM_X86_64` (0x3E). This value isn't in
the ELF specification but rather the ABI document (§4.1.1). On BSD
this is instead named `EM_AMD64`.

    echo -ne '\x3E\x00' >> true

For `e_version` it's always 1, like in the header.

    echo -ne '\x01\x00\x00\x00' >> true

The `e_entry` field will be 8 bytes because this is a 64-bit ELF. This
is the virtual address of the program's entry point. It's where the
loader will pass control and so it's where we'll load the program. The
typical entry address is somewhere around 0x400000. For a reason I'll
explain shortly, our entry point will be 120 bytes (0x78) after that
nice round number, at 0x40000078.

    echo -ne '\x78\x00\x00\x40\x00\x00\x00\x00' >> true

The `e_phoff` field holds the offset of the program header table. The
ELF header is 64 bytes (0x40) and this structure will immediately
follow. It's also 8 bytes.

    echo -ne '\x40\x00\x00\x00\x00\x00\x00\x00' >> true

The `e_shoff` header holds the offset of the section table. In an
executable program we don't need sections, so this is zero.

    echo -ne '\x00\x00\x00\x00\x00\x00\x00\x00' >> true

The `e_flags` field has processor-specific flags, which in our case is
just 0.

    echo -ne '\x00\x00\x00\x00' >> true

The `e_ehsize` holds the size of the ELF header, which, as I said, is
64 bytes (0x40).

    echo -ne '\x40\x00' >> true

The `e_phentsize` is the size of one program header, which is 56 bytes
(0x38).

    echo -ne '\x38\x00' >> true

The `e_phnum` field indicates how many program headers there are. We
only need the one: the segment with the 9 program bytes, to be loaded
into memory.

    echo -ne '\x01\x00' >> true

The `e_shentsize` is the size of a section header. We're not using
this, but we'll do our due diligence. These are 64 bytes (0x40).

    echo -ne '\x40\x00' >> true

The `e_shnum` field is the number of sections (0).

    echo -ne '\x00\x00' >> true

The `e_shstrndx` is the index of the section with the string table. It
doesn't exist, so it's 0.

    echo -ne '\x00\x00' >> true

#### The program header

Next is our program header.

~~~c
    typedef struct {
        uint32_t   p_type;
        uint32_t   p_flags;
        Elf64_Off  p_offset;
        Elf64_Addr p_vaddr;
        Elf64_Addr p_paddr;
        uint64_t   p_filesz;
        uint64_t   p_memsz;
        uint64_t   p_align;
    } Elf64_Phdr;
~~~

The `p_type` field indicates the segment type. This segment will hold
the program and will be loaded into memory, so we want `PT_LOAD` (1).
Other kinds of segments set up dynamic loading and such.

    echo -ne '\x01\x00\x00\x00' >> true

The `p_flags` field gives the memory protections. We want executable
(`PF_X` = 1) and readable (`PF_R` = 4). These are ORed together to
make 5.

    echo -ne '\x05\x00\x00\x00' >> true

The `p_offset` is the file offset for the content of this segment.
This will be the program we assembled. It will immediately follow the
this header. The ELF header was 64 bytes, plus a 56 byte program
header, which is 120 (0x78).

    echo -ne '\x78\x00\x00\x00\x00\x00\x00\x00' >> true

The `p_vaddr` is the virtual address where this segment will be
loaded. This is the entry point from before. A restriction is that
this value must be congruent with `p_offset` modulo the page size.
That's why the entry point was offset by 120 bytes.

    echo -ne '\x78\x00\x00\x40\x00\x00\x00\x00' >> true

The `p_paddr` is unused for this platform.

    echo -ne '\x00\x00\x00\x00\x00\x00\x00\x00' >> true

The `p_filesz` is the size of the segment in the file: 9 bytes.

    echo -ne '\x09\x00\x00\x00\x00\x00\x00\x00' >> true

The `p_memsz` is the size of the segment in memory, also 9 bytes. It
might sound redundant, but these are allowed to differ, in which case
it's either truncated or padded with zeroes.

    echo -ne '\x09\x00\x00\x00\x00\x00\x00\x00' >> true

The `p_align` indicates the segment's alignment. We don't care about
alignment.

    echo -ne '\x00\x00\x00\x00\x00\x00\x00\x00' >> true

#### Append the program

Finally, append the program we assembled at the beginning.

    echo -ne '\x31\xFF\xB8\x3C\x00\x00\x00\x0F\x05' >> true

Set it executable (hopefully `chmod` survived!):

    chmod +x true

And test it:

    ./true && echo 'Success'

Here's the whole thing as a shell script:

* [make-true.sh](/download/make-true.sh)

Is the C compiler done bootstrapping yet?


[comic]: http://imgs.xkcd.com/comics/real_programmers.png
[xkcd]: http://xkcd.com/378/
[elf]: http://refspecs.linuxbase.org/elf/elf.pdf
[man]: http://man7.org/linux/man-pages/man5/elf.5.html
[intel]: http://www.intel.com/content/www/us/en/processors/architectures-software-developer-manuals.html
[abi]: https://github.com/hjl-tools/x86-psABI/wiki/x86-64-psABI-secure.pdf
[com]: /blog/2014/12/09/
