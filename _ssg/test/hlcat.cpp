// hlcat: highlight standard input, printing the HTML to standard output
// $ hlcat LANG <file
// Exits with 1 if LANG is unknown (the output is then plain escaped
// text). Debug builds check that the markup round-trips to the input.
// This is free and unencumbered software released into the public domain.
#include <stdlib.h>
#include <unistd.h>
#include "../base.cpp"
#include "../highlight.cpp"

static b32 writeall(i32 fd, Str s)
{
    for (iz off = 0, n; off < s.len; off += n) {
        n = write(fd, s.data+off, (uz)(s.len-off));
        if (n < 0) return 0;
    }
    return 1;
}

[[noreturn]] static void os_oom()
{
    writeall(2, "hlcat: out of memory\n");
    _exit(2);
}

int main(int argc, char **argv)
{
    if (argc != 2) {
        writeall(2, "usage: hlcat LANG <file\n");
        return 2;
    }
    Str lang = {(u8 *)argv[1], 0};
    while (lang.data[lang.len]) lang.len++;

    iz    cap = (iz)1<<28;
    byte *mem = (byte *)malloc((uz)cap);
    if (!mem) {
        return 2;
    }
    Arena perm    = {mem, mem+cap/2};
    Arena scratch = {mem+cap/2, mem+cap};

    Str code = {};
    code.data = allocbytes(&perm, cap/4);
    for (iz n; (n = read(0, code.data+code.len, (uz)(cap/4-code.len))) > 0;) {
        code.len += n;
    }
    perm.beg = (byte *)(code.data + code.len);

    Buf b(&perm, code.len*2 + 64);
    b32 known = highlight(&b, lang, code, scratch);
    if (!writeall(1, finish(&b))) {
        return 2;
    }
    return !known;
}
