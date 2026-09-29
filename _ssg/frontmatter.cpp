// Front matter: the small YAML subset used by posts and pages
// This is free and unencumbered software released into the public domain.

struct FrontMatter {
    Str        title;
    Str        layout;
    Str        date;
    Str        uuid;
    Slice<Str> tags;
    Str        body;     // content after the front matter
    iz         bodyline; // line number where body begins
    b32        ok;
    Str        err;
    iz         errline;
};

// Parse a YAML scalar value: plain, 'single' ('' escapes), "double"
// (only \" and \\ escapes). Returns a null string on syntax error.
static Str yamlscalar(Str v, Arena *perm)
{
    v = trim(v);
    if (!v.len || (v[0] != '\'' && v[0] != '"')) {
        return v.len ? v : Str{(u8 *)"", 0};
    }
    u8  q = v[0];
    Buf b(perm, v.len);
    for (iz i = 1; i < v.len; i++) {
        u8 c = v[i];
        if (c == q) {
            if (q=='\'' && i+1<v.len && v[i+1]=='\'') {
                putbyte(&b, '\'');
                i++;
                continue;
            }
            if (i != v.len-1) {
                return {};  // trailing junk after closing quote
            }
            Str r = finish(&b);
            return r.len ? r : Str{(u8 *)"", 0};
        } else if (q=='"' && c=='\\') {
            if (i+1 >= v.len || (v[i+1] != '"' && v[i+1] != '\\')) {
                return {};  // unsupported escape
            }
            putbyte(&b, v[++i]);
        } else {
            putbyte(&b, c);
        }
    }
    return {};  // unterminated
}

static b32 tagchar(u8 c)
{
    return (c>='a' && c<='z') || digit(c);
}

static FrontMatter parsefrontmatter(Str src, Arena *perm)
{
    FrontMatter r = {};
    if (!startswith(src, "---\n")) {
        r.err = "missing front matter";
        return r;
    }
    Str rest = cuthead(src, 4);
    iz  line = 1;
    for (;;) {
        line++;
        if (!rest.len) {
            r.err = "unterminated front matter";
            r.errline = line;
            return r;
        }
        Cut c = cut(rest, '\n');
        rest = c.tail;
        Str ln = trimright(c.head);
        if (ln == "---" || ln == "...") {
            break;
        }
        if (!ln.len) {
            continue;
        }

        Cut kv = cut(ln, ':');
        Str key = kv.head;
        Str val = trim(kv.tail);
        if (!kv.ok || !key.len || whitespace(key[0])) {
            r.err = "expected 'key: value'";
            r.errline = line;
            return r;
        }

        if (key == "title") {
            r.title = yamlscalar(val, perm);
            if (!r.title.data) {
                r.err = "invalid title string";
                r.errline = line;
                return r;
            }
        } else if (key == "layout") {
            r.layout = val;
        } else if (key == "date") {
            r.date = val;
        } else if (key == "uuid") {
            r.uuid = val;
        } else if (key == "excerpt_separator") {
            // Ignored: <!--more--> is always the separator
        } else if (key == "tags") {
            if (val.len<2 || val[0]!='[' || val[val.len-1]!=']') {
                r.err = "tags must be an inline list: [a, b]";
                r.errline = line;
                return r;
            }
            Str list = trim(takehead(cuthead(val, 1), val.len-2));
            for (Cut t = {{}, list, list.len>0}; t.ok;) {
                t = cut(t.tail, ',');
                Str tag = trim(t.head);
                b32 valid = tag.len > 0;
                for (iz i = 0; i < tag.len; i++) {
                    valid &= tagchar(tag[i]);
                }
                if (!valid) {
                    r.err = "invalid tag name";
                    r.errline = line;
                    return r;
                }
                r.tags = push(perm, r.tags, tag);
            }
        } else {
            r.err = "unknown front matter key";
            r.errline = line;
            return r;
        }
    }

    // Like Jekyll, the blank lines after the closing marker are not
    // part of the body.
    for (;;) {
        Cut c = cut(rest, '\n');
        if (!c.ok || trim(c.head).len) {
            break;
        }
        rest = c.tail;
        line++;
    }
    r.body     = rest;
    r.bodyline = line + 1;
    r.ok       = 1;
    return r;
}
