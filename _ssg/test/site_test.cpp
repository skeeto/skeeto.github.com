// Site model tests: dates, encoders, front matter, UUIDs
static Str render(Arena *a, void (*fn)(Buf *, i64), i64 v)
{
    Buf b(a, 64);
    fn(&b, v);
    return finish(&b);
}

static Str render(Arena *a, void (*fn)(Buf *, Str), Str s)
{
    Buf b(a, 64);
    fn(&b, s);
    return finish(&b);
}

static void test_site(Test *t)
{
    Arena a = t->perm;

    // Dates, checked against Jekyll's output
    i64 t1 = parsetimestamp("2026-09-20T01:49:36Z");
    expect(t, "iso",     render(&a, printiso, t1), "2026-09-20T01:49:36Z");
    expect(t, "rfc822",  render(&a, printrfc822, t1), "Sun, 20 Sep 2026 01:49:36 GMT");
    expect(t, "long",    render(&a, printlongdate, parseday("2026-01-01")), "January 01, 2026");
    expect(t, "ymd",     render(&a, printymd, parseday("2007-09-01")), "2007-09-01");
    expect(t, "rfc822b", render(&a, printrfc822, parseday("1970-01-01")), "Thu, 01 Jan 1970 00:00:00 GMT");
    expect(t, "rfc822c", render(&a, printrfc822, parseday("2000-02-29")), "Tue, 29 Feb 2000 00:00:00 GMT");
    expect(t, "badts",   parsetimestamp("2026-09-20 01:49:36") < 0 ? Str("bad") : Str("ok"), "bad");

    // Filters, checked against Jekyll's output
    struct { Str title, uri, url; } cases[] = {
        {"My take on \"where's all the code\"",
         "My%20take%20on%20%22where's%20all%20the%20code%22",
         "My+take+on+%22where%27s+all+the+code%22"},
        {"Blast from the Past: Borland C++ on Windows 98",
         "Blast%20from%20the%20Past:%20Borland%20C++%20on%20Windows%2098",
         "Blast+from+the+Past%3A+Borland+C%2B%2B+on+Windows+98"},
        {"When Parallel: Pull, Don't Push",
         "When%20Parallel:%20Pull,%20Don't%20Push",
         "When+Parallel%3A+Pull%2C+Don%27t+Push"},
        {"Building and Installing Software in $HOME",
         "Building%20and%20Installing%20Software%20in%20$HOME",
         "Building+and+Installing+Software+in+%24HOME"},
        {"Slim Reader/Writer Locks are neato",
         "Slim%20Reader/Writer%20Locks%20are%20neato",
         "Slim+Reader%2FWriter+Locks+are+neato"},
        {"How to Write Fast(er) Emacs Lisp",
         "How%20to%20Write%20Fast(er)%20Emacs%20Lisp",
         "How+to+Write+Fast%28er%29+Emacs+Lisp"},
        {"2026 has been the most pivotal year in my career\xe2\x80\xa6 and it's only March",
         "2026%20has%20been%20the%20most%20pivotal%20year%20in%20my%20career%E2%80%A6%20and%20it's%20only%20March",
         "2026+has+been+the+most+pivotal+year+in+my+career%E2%80%A6+and+it%27s+only+March"},
    };
    for (iz i = 0; i < countof(cases); i++) {
        expect(t, "uri_escape", render(&a, printuriescape, cases[i].title), cases[i].uri);
        expect(t, "url_encode", render(&a, printurlencode, cases[i].title), cases[i].url);
    }

    // Front matter
    FrontMatter fm = parsefrontmatter(
        "---\ntitle: 'My take on \"where''s all the code\"'\nlayout: post\n"
        "date: 2022-05-22T22:59:04Z\ntags: [c, cpp]\n"
        "uuid: 3c5b2b52-5e2f-4d57-8f3b-5b7b5a8f5e1d\n---\n\nBody\n", &a);
    expect(t, "fm.ok",    fm.ok ? Str("ok") : fm.err, "ok");
    expect(t, "fm.title", fm.title, "My take on \"where's all the code\"");
    expect(t, "fm.tags",  fm.tags.len==2 ? fm.tags[1] : Str{}, "cpp");
    expect(t, "fm.body",  fm.body, "Body\n");
    fm = parsefrontmatter("---\ntitle: \"A \\\"quoted\\\" title\"\ntags: []\n---\nx", &a);
    expect(t, "fm.dq",    fm.title, "A \"quoted\" title");
    expect(t, "fm.empty", fm.tags.len ? Str("tags") : Str("none"), "none");
    fm = parsefrontmatter("---\ntitle: x\nbogus: y\n---\n", &a);
    expect(t, "fm.bad",   fm.ok ? Str("ok") : Str("err"), "err");

    // Derived tag UUIDs are well-formed and stable
    Str u = deriveuuid(&a, "newtag");
    expect(t, "uuid.valid", validuuid(u) ? Str("ok") : u, "ok");
    expect(t, "uuid.ver",   takehead(cuthead(u, 14), 1), "8");
    expect(t, "uuid.same",  deriveuuid(&a, "newtag"), u);
}
