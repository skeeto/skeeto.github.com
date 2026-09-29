// New posts: turn a loose Markdown draft into a dated post in _posts
// This is free and unencumbered software released into the public domain.

struct Draft {
    Str title;
    Str body;
};

// The draft begins with a "# Title" heading, which is removed from the
// body along with surrounding blank lines. The title is empty when the
// first non-blank line is not such a heading.
static Draft parsedraft(Str src)
{
    Draft r    = {};
    Str   rest = src;
    for (;;) {
        Cut c    = cut(rest, '\n');
        Str line = trim(c.head);
        if (!line.len) {
            if (!c.ok) {
                return r;
            }
            rest = c.tail;
            continue;
        }
        if (line.len<2 || line[0]!='#' || !whitespace(line[1])) {
            return r;
        }
        Str title = trim(cuthead(line, 1));
        iz  n     = title.len;
        for (; n && title[n-1]=='#'; n--) {}
        if (!n || whitespace(title[n-1])) {
            title = trim(takehead(title, n));  // closing hashes
        }
        r.title = title;
        rest    = c.tail;
        break;
    }

    for (;;) {
        Cut c = cut(rest, '\n');
        if (!c.ok || trim(c.head).len) {
            break;
        }
        rest = c.tail;
    }
    r.body = trimright(rest);
    return r;
}

// Runs of non-alphanumerics become a dash. A leading dash is dropped, but
// a trailing dash is kept, as in existing names like "...-in-C-" (C++).
static void printslug(Buf *b, Str title)
{
    b32 dash = 0;
    b32 any  = 0;
    for (iz i = 0; i < title.len; i++) {
        u8 c = title[i];
        if (!alnum(c)) {
            dash = 1;
            continue;
        }
        if (dash && any) {
            putbyte(b, '-');
        }
        putbyte(b, c);
        dash = 0;
        any  = 1;
    }
    if (dash && any) {
        putbyte(b, '-');
    }
}

// A YAML scalar: plain when safe, otherwise single-quoted.
static void printyaml(Buf *b, Str s)
{
    b32 quote = !s.len || s[s.len-1]==':' || !(trim(s) == s) ||
                find(s, ": ")>=0 || find(s, " #")>=0 ||
                find("-?:,[]{}#&*!|>'\"%@`", takehead(s, 1))>=0;
    if (!quote) {
        print(b, s);
        return;
    }
    putbyte(b, '\'');
    for (iz i = 0; i < s.len; i++) {
        if (s[i] == '\'') {
            putbyte(b, '\'');
        }
        putbyte(b, s[i]);
    }
    putbyte(b, '\'');
}

static Str postname(Arena *perm, Str title, i64 now)  // or empty
{
    Buf name(perm, 64+title.len);
    printymd(&name, now);
    putbyte(&name, '-');
    iz prefix = name.len;
    printslug(&name, title);
    if (name.len == prefix) {
        return {};
    }
    print(&name, ".markdown");
    return finish(&name);
}

// Write the draft into _posts, dated now, and print its path.
static i32 newpost(Os *os, Options *opt, Log *log, Arena *perm, Arena scratch)
{
    Str draft = os_read(os, perm, opt->newpost);
    if (!draft.data) {
        error(log, opt->newpost, 0, "could not read file");
        return 1;
    }
    Draft d = parsedraft(normalize(draft));
    if (!d.title.len) {
        error(log, opt->newpost, 0, "draft must begin with a \"# Title\" heading");
        return 1;
    }
    Str name = postname(perm, d.title, opt->now);
    if (!name.len) {
        error(log, opt->newpost, 0, "no usable file name from the title");
        return 1;
    }

    // Post URLs are by date, so one post per day
    Str dir = join(perm, opt->src, "_posts");
    Str day = takehead(name, 11);  // "YYYY-MM-DD-"
    for (Dirent *e = os_list(os, &scratch, dir); e; e = e->next) {
        if (startswith(e->name, day)) {
            error(log, join(perm, dir, e->name), 0, "a post already has this date");
            return 1;
        }
    }

    Str uuid = deriveuuid(perm, name);
    Str path = join(perm, dir, name);
    Buf b(&scratch, 1<<12);
    print(&b, "---\ntitle: ");
    printyaml(&b, d.title);
    print(&b, "\ndate: ");
    printiso(&b, opt->now);
    print(&b, "\ntags: []\nuuid: ");
    print(&b, uuid);
    print(&b, "\n---\n\n");
    print(&b, d.body);
    putbyte(&b, '\n');
    Str post = finish(&b);
    if (!os_write(os, scratch, path, post)) {
        error(log, path, 0, "could not write file");
        return 1;
    }

    Buf out(&scratch, path.len+1);
    print(&out, path);
    putbyte(&out, '\n');
    os_print(os, 1, finish(&out));
    return 0;
}
