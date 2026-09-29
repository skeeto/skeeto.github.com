// Unit tests: a platform layer with stubbed I/O
// $ cmake --build build && ./build/ssg_test
// This is free and unencumbered software released into the public domain.
#include <stdio.h>
#include <stdlib.h>
#include "ssg.cpp"

struct Os {};
static Str     os_read(Os *, Arena *, Str) { return {}; }
static b32     os_write(Os *, Arena, Str, Str) { return 0; }
static b32     os_mkdirs(Os *, Arena, Str) { return 0; }
static Dirent *os_list(Os *, Arena *, Str) { return 0; }
static i32     os_copy(Os *, Arena, Str, Str) { return COPY_FAILED; }
static i64     os_now(Os *) { return 0; }
static b32     os_print(Os *, i32 fd, Str s)
{
    return fwrite(s.data, 1, (uz)s.len, fd==1 ? stdout : stderr) == (uz)s.len;
}
[[noreturn]] static void os_oom() { __builtin_trap(); }
static void    os_sleep(Os *, i32) {}

struct Test {
    Arena perm;
    i32   run;
    i32   failed;
};

static void printstr(FILE *f, Str s)
{
    fwrite(s.data, 1, (uz)s.len, f);
}

// Compare strings, printing both on mismatch.
static b32 expect(Test *t, Str name, Str got, Str want)
{
    t->run++;
    if (got == want) {
        return 1;
    }
    t->failed++;
    fprintf(stderr, "FAIL: ");
    printstr(stderr, name);
    fprintf(stderr, "\n  got:  [");
    printstr(stderr, got);
    fprintf(stderr, "]\n  want: [");
    printstr(stderr, want);
    fprintf(stderr, "]\n");
    return 0;
}

#include "test/site_test.cpp"
#include "test/newpost_test.cpp"
#include "test/markdown_test.cpp"
#include "test/highlight_test.cpp"
#include "test/wide_test.cpp"

int main()
{
    iz    cap = (iz)1<<28;
    byte *mem = (byte *)malloc((uz)cap);
    Test  t   = {};
    t.perm = {mem, mem+cap};

    test_site(&t);
    test_newpost(&t);
    test_markdown(&t);
    test_highlight(&t);
    test_wide(&t);

    fprintf(stderr, "%d tests, %d failed\n", t.run, t.failed);
    return t.failed ? 1 : 0;
}
