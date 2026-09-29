// Win32 layer tests: UTF-16 conversion, command lines, extended paths
#include "../wide.cpp"

template<iz N>
static Str16 wide(Arena *a, c16 const (&s)[N])
{
    Str16 r = {alloc<c16>(a, N), N-1};
    for (iz i = 0; i < N; i++) {
        r.data[i] = s[i];
    }
    return r;
}

// Show UTF-16 as hex code units, for comparison.
static Str hex16(Arena *a, Str16 s)
{
    Buf b(a, 64);
    for (iz i = 0; i < s.len; i++) {
        if (i) putbyte(&b, ' ');
        printhex(&b, s.data[i], 4);
    }
    if (s.data[s.len]) {
        print(&b, " (unterminated)");
    }
    return finish(&b);
}

// Split with the Win32 rules, showing each argument in brackets.
static Str splitshow(Arena *a, Str cmd)
{
    Slice<Str> args = splitcmdline(a, clone(a, cmd));
    Buf        b(a, 64);
    for (iz i = 0; i < args.len; i++) {
        putbyte(&b, '[');
        print(&b, args[i]);
        putbyte(&b, ']');
    }
    return finish(&b);
}

static void test_wide(Test *t)
{
    Arena a = t->perm;

    // UTF-16 <-> WTF-8, including unpaired surrogates, which must
    // round-trip so that any file name can be opened again.
    struct { Str16 w; Str u; } conv[] = {
        {wide(&a, u"_posts"),        "_posts"},
        {wide(&a, u"caf\xe9"),       "caf\xc3\xa9"},
        {wide(&a, u"\x20ac"),        "\xe2\x82\xac"},
        {wide(&a, u"\xd83d\xde00"),  "\xf0\x9f\x98\x80"},
        {wide(&a, u"a\xd83d"),       "a\xed\xa0\xbd"},
        {wide(&a, u"\xde00z"),       "\xed\xb8\x80z"},
        {wide(&a, u"\xd83d" u"A\xde00"), "\xed\xa0\xbd" "A\xed\xb8\x80"},
        {wide(&a, u""),              ""},
        // Encoding length boundaries
        {wide(&a, u"\x7f\x80"),      "\x7f\xc2\x80"},
        {wide(&a, u"\x7ff\x800"),    "\xdf\xbf\xe0\xa0\x80"},
        {wide(&a, u"\xfffd\xffff"),  "\xef\xbf\xbd\xef\xbf\xbf"},
        {wide(&a, u"\xd800\xdc00"),  "\xf0\x90\x80\x80"},
        {wide(&a, u"\xdbff\xdfff"),  "\xf4\x8f\xbf\xbf"},
        {wide(&a, u"\xdc00\xd800"),  "\xed\xb0\x80\xed\xa0\x80"},  // reversed
    };
    for (iz i = 0; i < countof(conv); i++) {
        Str16 w = conv[i].w;
        expect(t, "fromwide", fromwide(&a, w.data, w.len), conv[i].u);
        expect(t, "towide", hex16(&a, towide(&a, conv[i].u)), hex16(&a, w));
    }
    // Invalid UTF-8 decodes as Latin-1, and beyond U+10FFFF is replaced
    expect(t, "towide.bad",  hex16(&a, towide(&a, "\xff")), "00ff");
    expect(t, "towide.high", hex16(&a, towide(&a, "\xf7\xbf\xbf\xbf")), "fffd");
    expect(t, "towide.trunc", hex16(&a, towide(&a, "a\xe2\x82")), "0061 00e2 0082");
    expect(t, "towide.cont", hex16(&a, towide(&a, "\x80z")), "0080 007a");

    // Command lines, following CommandLineToArgvW (differentially tested
    // against u-config's cmdline.c, which matches it exactly)
    struct { Str cmd, args; } cmds[] = {
        {"ssg",                          "[ssg]"},
        {"",                             "[]"},
        {"ssg -C src  -o\tout ",         "[ssg][-C][src][-o][out]"},
        {R"("C:\Program Files\ssg.exe" -o "my dir")",
         R"([C:\Program Files\ssg.exe][-o][my dir])"},
        {R"(ssg C:\path\ \\server\share)",  R"([ssg][C:\path\][\\server\share])"},
        {R"(ssg "a\"b" a\\\"b a\\\\"b c")", R"([ssg][a"b][a\"b][a\\b c])"},
        {R"(ssg "C:\dir\\" x)",          R"([ssg][C:\dir\][x])"},
        {R"(ssg "" "")",                 "[ssg][][]"},
        {R"(ssg "a""b" x)",              R"([ssg][a"b x])"},
        {R"(ssg """ "a"""b)",            R"([ssg]["][a"b])"},
        {R"(ssg a"b c"d)",               "[ssg][ab cd]"},
        {R"("ssg"x y)",                  "[ssg][x][y]"},
        {R"(ssg\"x y)",                  R"([ssg\"x][y])"},
        {R"("unterminated name)",        "[unterminated name]"},
        {R"(ssg "open quote)",           "[ssg][open quote]"},
        {" ssg",                         "[][ssg]"},
        {"ssg caf\xc3\xa9",              "[ssg][caf\xc3\xa9]"},
        // Checked against Wine's CommandLineToArgvW algorithm
        {R"(ssg a\\\\ a\)",              R"([ssg][a\\\\][a\])"},
        {R"(ssg "a\\" "\\\\")",          R"([ssg][a\][\\])"},
        {R"(ssg "a\\\"b" \")",           R"([ssg][a\"b]["])"},
        {R"(ssg """")",                  R"([ssg]["])"},
        {R"(ssg """""" x)",              R"([ssg][""][x])"},
        {R"(ssg "a b"c "d e")",          "[ssg][a bc][d e]"},
        {R"(ssg "a"b"c ""a)",            R"([ssg][abc "a])"},
        {"ssg\targ",                     "[ssg][arg]"},
        {R"("a b"c d)",                  "[a b][c][d]"},
        {R"("" x)",                      "[][x]"},
    };
    for (iz i = 0; i < countof(cmds); i++) {
        expect(t, cmds[i].cmd, splitshow(&a, cmds[i].cmd), cmds[i].args);
    }

    // Extended-length prefixes, applied to GetFullPathNameW output
    struct { Str16 full, want; } paths[] = {
        {wide(&a, u"C:\\a\\b"),           wide(&a, u"\\\\?\\C:\\a\\b")},
        {wide(&a, u"C:\\"),               wide(&a, u"\\\\?\\C:\\")},
        {wide(&a, u"\\\\host\\share\\x"), wide(&a, u"\\\\?\\UNC\\host\\share\\x")},
        {wide(&a, u"\\\\?\\C:\\x"),       wide(&a, u"\\\\?\\C:\\x")},
        {wide(&a, u"\\\\.\\NUL"),         wide(&a, u"\\\\.\\NUL")},
        {wide(&a, u"D:\\"),               wide(&a, u"\\\\?\\D:\\")},
        {wide(&a, u"\\\\?\\UNC\\h\\s"),   wide(&a, u"\\\\?\\UNC\\h\\s")},
        {wide(&a, u"\\\\h\\s\\"),         wide(&a, u"\\\\?\\UNC\\h\\s\\")},
    };
    for (iz i = 0; i < countof(paths); i++) {
        Str16 full = paths[i].full;
        c16  *buf  = alloc<c16>(&a, full.len+7);
        for (iz j = 0; j < full.len; j++) {
            buf[6+j] = full.data[j];
        }
        Str16 got = extendpath(buf, full.len);
        expect(t, "extendpath", hex16(&a, got), hex16(&a, paths[i].want));
    }

    struct { Str16 path; iz root; } roots[] = {
        {wide(&a, u"\\\\?\\C:\\a\\b"),              7},
        {wide(&a, u"\\\\?\\C:\\"),                  7},
        {wide(&a, u"\\\\?\\C:"),                    6},
        {wide(&a, u"\\\\?\\UNC\\host\\share\\x"),  19},
        {wide(&a, u"\\\\?\\UNC\\host"),            12},
        {wide(&a, u"\\\\?\\UNC\\host\\share"),     18},
        {wide(&a, u"\\\\.\\NUL"),                   7},
        {wide(&a, u"\\\\?\\Volume{1234}\\x"),      17},
        {wide(&a, u"a\\b"),                         0},
        {wide(&a, u""),                             0},
    };
    for (iz i = 0; i < countof(roots); i++) {
        Buf b(&a, 16);
        print(&b, (i64)rootlen(roots[i].path));
        Buf w(&a, 16);
        print(&w, (i64)roots[i].root);
        expect(t, hex16(&a, roots[i].path), finish(&b), finish(&w));
    }
}
