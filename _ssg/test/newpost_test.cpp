// New post tests: drafts, file names, YAML titles
static Str slug(Arena *a, Str title)
{
    Buf b(a, 64);
    printslug(&b, title);
    return finish(&b);
}

static Str yaml(Arena *a, Str s)
{
    Buf b(a, 64);
    printyaml(&b, s);
    return finish(&b);
}

static void test_newpost(Test *t)
{
    Arena a = t->perm;

    Draft d = parsedraft("\n\n# A *new* post #\n\n\nFirst paragraph.\n\n    code\n\n\n");
    expect(t, "draft.title", d.title, "A *new* post");
    expect(t, "draft.body",  d.body,  "First paragraph.\n\n    code");
    d = parsedraft("# C# tricks\nBody");
    expect(t, "draft.hash",  d.title, "C# tricks");
    expect(t, "draft.tight", d.body,  "Body");
    expect(t, "draft.none",  parsedraft("Intro\n# Title\n").title, "");
    expect(t, "draft.h2",    parsedraft("## Title\n").title, "");
    expect(t, "draft.empty", parsedraft("\n\n").title, "");

    // Matching existing post names
    expect(t, "slug.cpp",   slug(&a, "More speculations on arenas in C++"),
                            "More-speculations-on-arenas-in-C-");
    expect(t, "slug.quote", slug(&a, "My take on \"where's all the code\""),
                            "My-take-on-where-s-all-the-code-");
    expect(t, "slug.utf8",  slug(&a, "2026 has been the most pivotal year in my "
                                     "career\xe2\x80\xa6 and it's only March"),
                            "2026-has-been-the-most-pivotal-year-in-my-career-and-it-s-only-March");
    expect(t, "slug.colon", slug(&a, "Giving C++ std::regex a C makeover"),
                            "Giving-C-std-regex-a-C-makeover");
    expect(t, "slug.lead",  slug(&a, "\"Quoted\" start"), "Quoted-start");
    expect(t, "slug.none",  slug(&a, "++ --"), "");

    expect(t, "name", postname(&a, "Hello, world", parseday("2026-09-29")),
                      "2026-09-29-Hello-world.markdown");

    // Titles survive the trip through front matter
    Str titles[] = {
        "Plain title", "It's fine", "Title: subtitle", "'Quoted' start",
        "\"Double\" start", "Ends with:", "C# tricks", "A #hash", "- dash",
        "My take on \"where's all the code\"",
    };
    for (iz i = 0; i < countof(titles); i++) {
        Buf b(&a, 128);
        print(&b, "---\ntitle: ");
        printyaml(&b, titles[i]);
        print(&b, "\n---\n");
        FrontMatter fm = parsefrontmatter(finish(&b), &a);
        expect(t, "yaml.trip", fm.title, titles[i]);
    }
    expect(t, "yaml.plain", yaml(&a, "It's fine"), "It's fine");
    expect(t, "yaml.quote", yaml(&a, "'Quoted' start"), "'''Quoted'' start'");
}
