// Site model: posts, tags, pages, static files, and the build driver
// This is free and unencumbered software released into the public domain.

struct Options {
    Str src;
    Str out;
    Str mdfile;
    Str newpost;
    i64 now;
    b32 fixedtime;
    b32 excerpts;
    b32 quiet;
    b32 watch;
};

static i64 parseint(Str s)
{
    i64 r = 0;
    b32 neg = s.len && s[0]=='-';
    for (iz i = neg; i < s.len && digit(s[i]); i++) {
        r = r*10 + (s[i] - '0');
    }
    return neg ? -r : r;
}


// Dates (proleptic Gregorian, UTC)

struct Tm {
    i32 year, mon, day, hour, min, sec, wday;
};

static i64 daysfromcivil(i64 y, i64 m, i64 d)
{
    y -= m <= 2;
    i64 era = (y >= 0 ? y : y-399) / 400;
    i64 yoe = y - era*400;
    i64 doy = (153*(m + (m > 2 ? -3 : 9)) + 2)/5 + d - 1;
    i64 doe = yoe*365 + yoe/4 - yoe/100 + doy;
    return era*146097 + doe - 719468;
}

static Tm gmtime(i64 t)
{
    Tm  r    = {};
    i64 days = t / 86400;
    i64 secs = t % 86400;
    if (secs < 0) {
        secs += 86400;
        days--;
    }
    r.hour = (i32)(secs / 3600);
    r.min  = (i32)(secs / 60 % 60);
    r.sec  = (i32)(secs % 60);
    r.wday = (i32)(((days % 7) + 11) % 7);  // 1970-01-01 was Thursday

    i64 z   = days + 719468;
    i64 era = (z >= 0 ? z : z - 146096) / 146097;
    i64 doe = z - era*146097;
    i64 yoe = (doe - doe/1460 + doe/36524 - doe/146096) / 365;
    i64 doy = doe - (365*yoe + yoe/4 - yoe/100);
    i64 mp  = (5*doy + 2) / 153;
    r.day   = (i32)(doy - (153*mp + 2)/5 + 1);
    r.mon   = (i32)(mp < 10 ? mp + 3 : mp - 9);
    r.year  = (i32)(yoe + era*400 + (r.mon <= 2));
    return r;
}

// Parse fixed-width digits, or -1 on error.
static i32 parsedigits(Str s, iz off, iz len)
{
    if (off+len > s.len) {
        return -1;
    }
    i32 r = 0;
    for (iz i = off; i < off+len; i++) {
        if (!digit(s[i])) return -1;
        r = r*10 + (s[i] - '0');
    }
    return r;
}

// Parse "YYYY-MM-DD" at the start of s into a time, or -1.
static i64 parseday(Str s)
{
    i32 y = parsedigits(s, 0, 4);
    i32 m = parsedigits(s, 5, 2);
    i32 d = parsedigits(s, 8, 2);
    if (y<0 || m<1 || m>12 || d<1 || d>31 || s[4]!='-' || s[7]!='-') {
        return -1;
    }
    return daysfromcivil(y, m, d) * 86400;
}

// Parse "YYYY-MM-DDTHH:MM:SSZ" into a time, or -1.
static i64 parsetimestamp(Str s)
{
    if (s.len != 20 || s[10]!='T' || s[13]!=':' || s[16]!=':' || s[19]!='Z') {
        return -1;
    }
    i64 day = parseday(s);
    i32 h   = parsedigits(s, 11, 2);
    i32 m   = parsedigits(s, 14, 2);
    i32 sec = parsedigits(s, 17, 2);
    if (day<0 || h<0 || h>23 || m<0 || m>59 || sec<0 || sec>60) {
        return -1;
    }
    return day + h*3600 + m*60 + sec;
}

static Str monthnames[] = {
    "January", "February", "March", "April", "May", "June", "July",
    "August", "September", "October", "November", "December",
};

static Str daynames[] = {"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"};

// %Y-%m-%d
static void printymd(Buf *b, i64 t)
{
    Tm tm = gmtime(t);
    printpad(b, tm.year, 4);
    putbyte(b, '-');
    printpad(b, tm.mon, 2);
    putbyte(b, '-');
    printpad(b, tm.day, 2);
}

// %B %d, %Y
static void printlongdate(Buf *b, i64 t)
{
    Tm tm = gmtime(t);
    print(b, monthnames[tm.mon-1]);
    putbyte(b, ' ');
    printpad(b, tm.day, 2);
    print(b, ", ");
    printpad(b, tm.year, 4);
}

static void printhms(Buf *b, Tm tm)
{
    printpad(b, tm.hour, 2);
    putbyte(b, ':');
    printpad(b, tm.min, 2);
    putbyte(b, ':');
    printpad(b, tm.sec, 2);
}

// %Y-%m-%dT%H:%M:%SZ
static void printiso(Buf *b, i64 t)
{
    printymd(b, t);
    putbyte(b, 'T');
    printhms(b, gmtime(t));
    putbyte(b, 'Z');
}

// %a, %d %b %Y %H:%M:%S GMT
static void printrfc822(Buf *b, i64 t)
{
    Tm tm = gmtime(t);
    print(b, daynames[tm.wday]);
    print(b, ", ");
    printpad(b, tm.day, 2);
    putbyte(b, ' ');
    print(b, takehead(monthnames[tm.mon-1], 3));
    putbyte(b, ' ');
    printpad(b, tm.year, 4);
    putbyte(b, ' ');
    printhms(b, tm);
    print(b, " GMT");
}


// URL encoders matching Liquid's uri_escape and url_encode filters

static void percent(Buf *b, u8 c)
{
    putbyte(b, '%');
    putbyte(b, "0123456789ABCDEF"[c>>4]);
    putbyte(b, "0123456789ABCDEF"[c&15]);
}

// Addressable::URI.normalize_component: keep unreserved and reserved.
// Output is escaped for use inside an HTML attribute.
static void printuriescape(Buf *b, Str s)
{
    for (iz i = 0; i < s.len; i++) {
        u8 c = s[i];
        if (c == '&') {
            print(b, "&amp;");
        } else if (alnum(c) || (c && c<0x80 && find(Str("-._~:/?#[]@!$'()*+,;="), Str{&c, 1})>=0)) {
            putbyte(b, c);
        } else {
            percent(b, c);
        }
    }
}

// CGI.escape: keep A-Za-z0-9 _.-~, space as +.
static void printurlencode(Buf *b, Str s)
{
    for (iz i = 0; i < s.len; i++) {
        u8 c = s[i];
        if (alnum(c) || c=='_' || c=='.' || c=='-' || c=='~') {
            putbyte(b, c);
        } else if (c == ' ') {
            putbyte(b, '+');
        } else {
            percent(b, c);
        }
    }
}


// Tag feed UUIDs. Tags not listed get a UUID derived from their name.

struct TagUuid {
    Str name;
    Str uuid;
};

static TagUuid taguuids[] = {
    {"ai",           "bbcac281-4091-4371-aaed-1061fd43ac26"},
    {"asyncio",      "702ad32f-4903-4692-9b8c-b8182b548416"},
    {"bsd",          "5e43aa24-aef5-4f63-b37e-42b2d7859c01"},
    {"c",            "0ba7b921-5597-4fbc-8ceb-88afb378c637"},
    {"clojure",      "cf08ecce-d0a4-4d39-a3fc-aacc66a4cf9b"},
    {"compression",  "1c53dce4-4b3d-4f09-8857-46c6a45c2c7b"},
    {"compsci",      "73aad984-a0c4-4870-b941-b92ebee6efda"},
    {"cpp",          "bdc867e4-b8f7-4cf0-8437-adafc8297129"},
    {"crypto",       "799d7efd-3418-409f-b602-5191d8b4fa4c"},
    {"debian",       "31ca31c3-9fcb-4be8-8278-f5e1373a4738"},
    {"elfeed",       "7a1e551e-16c1-4dc3-b77b-861a81651c99"},
    {"elisp",        "8f0d2f56-ad2a-4f37-b61c-16d975ffd79e"},
    {"emacs",        "3d01fe0a-7c1c-475c-b07c-47b7b19e8870"},
    {"fiction",      "88bc4ba3-35fa-4614-8dc1-4ccaf702b655"},
    {"game",         "da57a040-05d6-4aba-ae11-162617d3e9c4"},
    {"git",          "c95ccfd4-9b1b-4f28-877f-8f71751a9dde"},
    {"go",           "51c2a18d-6ae7-4e0c-b14e-a1424eb21dba"},
    {"gpgpu",        "e9f7d39e-ac47-4b56-91c1-796bd01500ad"},
    {"interactive",  "82f38864-6463-4eac-8a86-3eca11fc4fa5"},
    {"java",         "8bee72c7-ad28-4fe4-b67d-ca24449a1c60"},
    {"javascript",   "9d102329-9ecd-4318-8e36-179ecc067f75"},
    {"lang",         "a6275ef4-a851-4550-918f-4cfc74bc9568"},
    {"link",         "8c68d9f0-b18d-4cc3-a43c-3af369f36938"},
    {"linux",        "9d7de7b3-b357-464b-a963-da196bdd5954"},
    {"lisp",         "dffd8000-5e5e-41d1-ab8e-ee2a25ee30a9"},
    {"lua",          "8681fa33-3a8a-46e2-ba71-b2d6bb236a5f"},
    {"math",         "699ac152-8662-474a-aeb6-98c1ddd9febb"},
    {"meatspace",    "22d16006-121f-438a-be7a-f0619eca48b9"},
    {"media",        "2f2627c3-6116-413b-ba68-3a7a0cfeb8fb"},
    {"meta",         "22ce0838-6736-4d87-a83f-0f86c7f1d970"},
    {"netsec",       "7c40636f-98cb-4dbf-b2a1-ed73f8712af7"},
    {"octave",       "ae7db2fb-eac3-4621-bae3-876c87011475"},
    {"opengl",       "90c6162f-52ec-4018-b490-acd91621dc9a"},
    {"openpgp",      "f29b12d7-84e6-4cef-ab71-46e854ab1df6"},
    {"optimization", "6022e81a-277c-4339-b204-59145c368baf"},
    {"perl",         "dd44fb39-63bf-4290-8e5f-818e34348c1c"},
    {"posix",        "bba994f0-2941-4a75-b490-04cd48f44829"},
    {"python",       "7a5665a7-6c04-451e-a43c-f7284c9cfcca"},
    {"rant",         "e5fdd2c0-380e-4ce5-8730-bd443601b65f"},
    {"reddit",       "1cbcfbba-f648-40f5-92fa-de92dd9f262b"},
    {"story",        "1f7c4b50-51dd-40c2-b6dc-75c024e2071e"},
    {"trick",        "a2a6e1b2-9a41-443c-83ab-c457836a1a5d"},
    {"tutorial",     "3e3ec37f-9de8-40de-b725-2f1f16b203c8"},
    {"video",        "6ca8997d-4783-46f5-9390-8b615eac5b75"},
    {"vim",          "d06181fc-6127-4f82-ba7e-0ad17bf6a145"},
    {"web",          "01868576-f382-43f9-bde0-c4415f126084"},
    {"webgl",        "cba28fc1-4172-4bd0-87fd-7920c4fb01ee"},
    {"win32",        "33f52ccf-2d02-4f2f-b574-8dd1468b3e7b"},
    {"x86",          "763a3ddc-a1df-4bad-b03e-86513dc3c50c"},
};

// A deterministic version 8 (custom) UUID derived from a name.
static Str deriveuuid(Arena *perm, Str name)
{
    u64 hi = hash(name);
    u64 lo = hash(concat(perm, name, "\n"));
    hi = (hi & ~(u64)0xf000) | 0x8000;                // version 8
    lo = (lo & ~((u64)3<<62)) | ((u64)2<<62);         // RFC 9562 variant
    Buf b(perm, 36);
    printhex(&b, hi>>32, 8);
    putbyte(&b, '-');
    printhex(&b, hi>>16, 4);
    putbyte(&b, '-');
    printhex(&b, hi, 4);
    putbyte(&b, '-');
    printhex(&b, lo>>48, 4);
    putbyte(&b, '-');
    printhex(&b, lo, 12);
    return finish(&b);
}

static b32 validuuid(Str s)
{
    if (s.len != 36) {
        return 0;
    }
    for (iz i = 0; i < s.len; i++) {
        u8  c    = s[i];
        b32 dash = i==8 || i==13 || i==18 || i==23;
        if (dash ? c!='-' : !(digit(c) || (c>='a' && c<='f'))) {
            return 0;
        }
    }
    return 1;
}


// Site model

struct Post {
    Str        src;      // path relative to the source root
    Str        title;
    Str        uuid;
    Slice<Str> tags;
    i64        time;
    Str        url;      // /blog/YYYY/MM/DD/
    Str        body;     // Markdown
    iz         bodyline;
    Str        html;
    iz         excerpt;  // length of the excerpt prefix of html
    Post      *older;
    Post      *newer;
};

struct Tag {
    Str           name;
    Str           uuid;
    Slice<Post *> posts;  // newest first
};

struct Page {
    Str src;    // e.g. about/index.md
    Str out;    // e.g. about/index.html
    Str title;
    Str body;
    iz  bodyline;
    Str html;
};

struct Site {
    Slice<Post *> posts;    // newest first
    Slice<Tag *>  tags;     // sorted by name
    Slice<Page *> pages;
    Slice<Str>    statics;  // relative paths
    i64           now;
};

struct Ctx {
    Os       *os;
    Options  *opt;
    Log      *log;
    Arena    *perm;
    Map<b32> *outputs;
    i32       written;
    i32       copied;
    i32       skipped;
};

static Str join(Arena *a, Str dir, Str name)
{
    if (!dir.len || dir == ".") {
        return name;
    }
    Str r = concat(a, dir, "/");
    return concat(a, r, name);
}

static Str srcpath(Ctx *c, Arena *a, Str rel)
{
    return join(a, c->opt->src, rel);
}

// Normalize CRLF line endings in place.
static Str normalize(Str s)
{
    iz n = 0;
    for (iz i = 0; i < s.len; i++) {
        if (s[i]=='\r' && i+1<s.len && s[i+1]=='\n') {
            continue;
        }
        s.data[n++] = s[i];
    }
    s.len = n;
    return s;
}

static b32 postless(Post **a, Post **b)
{
    // Newest first; ties broken by path, as Jekyll does (reversed)
    if ((*a)->time != (*b)->time) {
        return (*a)->time > (*b)->time;
    }
    return compare((*a)->src, (*b)->src) > 0;
}

static b32 tagless(Tag **a, Tag **b)
{
    return compare((*a)->name, (*b)->name) < 0;
}

static Post *loadpost(Ctx *c, Str name, Arena scratch)
{
    Arena *perm = c->perm;
    Log   *log  = c->log;
    Str    rel  = join(perm, "_posts", name);

    // YYYY-MM-DD-slug.(markdown|md)
    i64 day = parseday(name);
    b32 md  = endswith(name, ".markdown") || endswith(name, ".md");
    if (day<0 || name.len<12 || name[10]!='-' || !md) {
        error(log, rel, 0, "post names must be YYYY-MM-DD-slug.markdown");
        return 0;
    }

    Str src = os_read(c->os, perm, srcpath(c, &scratch, rel));
    if (!src.data) {
        error(log, rel, 0, "could not read file");
        return 0;
    }
    src = normalize(src);

    FrontMatter fm = parsefrontmatter(src, perm);
    if (!fm.ok) {
        error(log, rel, fm.errline, fm.err);
        return 0;
    }

    Post *p = alloc<Post>(perm);
    p->src      = rel;
    p->title    = fm.title;
    p->uuid     = fm.uuid;
    p->tags     = fm.tags;
    p->body     = fm.body;
    p->bodyline = fm.bodyline;
    p->time     = day;

    if (!p->title.data) {
        error(log, rel, 0, "missing title");
        return 0;
    }
    if (fm.layout.len && fm.layout != "post") {
        warn(log, rel, 0, "ignoring layout other than 'post'");
    }
    if (!validuuid(p->uuid)) {
        error(log, rel, 0, "missing or invalid uuid");
    }
    if (fm.date.len) {
        i64 t = parsetimestamp(fm.date);
        if (t < 0) {
            error(log, rel, 0, "date must be YYYY-MM-DDTHH:MM:SSZ");
        } else if (t/86400*86400 != day) {
            error(log, rel, 0, "front matter date does not match file name");
        } else {
            p->time = t;
        }
    }

    Buf url(perm, 20);
    print(&url, "/blog/");
    Tm tm = gmtime(p->time);
    printpad(&url, tm.year, 4);
    putbyte(&url, '/');
    printpad(&url, tm.mon, 2);
    putbyte(&url, '/');
    printpad(&url, tm.day, 2);
    putbyte(&url, '/');
    p->url = finish(&url);
    return p;
}

static void loadposts(Ctx *c, Site *site, Arena scratch)
{
    Arena   *perm  = c->perm;
    Map<Post *> *urls = 0;

    Str dir = srcpath(c, perm, "_posts");
    for (Dirent *e = os_list(c->os, perm, dir); e; e = e->next) {
        if (e->isdir || !e->name.len || e->name[0]=='.' || endswith(e->name, "~")) {
            continue;
        }
        Post *p = loadpost(c, e->name, scratch);
        if (!p) {
            continue;
        }
        Post **prev = upsert(&urls, p->url, perm);
        if (*prev) {
            error(c->log, p->src, 0, "another post has the same date");
            continue;
        }
        *prev = p;
        site->posts = push(perm, site->posts, p);
    }

    sort(site->posts.data, site->posts.len, postless, scratch);
    for (iz i = 0; i < site->posts.len; i++) {
        Post *p  = site->posts[i];
        p->newer = i > 0 ? site->posts[i-1] : 0;
        p->older = i+1 < site->posts.len ? site->posts[i+1] : 0;
    }

    // Tags, collected newest first since posts are already sorted
    Map<Tag *> *tags = 0;
    for (iz i = 0; i < site->posts.len; i++) {
        Post *p = site->posts[i];
        for (iz j = 0; j < p->tags.len; j++) {
            Tag **t = upsert(&tags, p->tags[j], perm);
            if (!*t) {
                *t = alloc<Tag>(perm);
                (*t)->name = p->tags[j];
                for (iz k = 0; k < countof(taguuids); k++) {
                    if (taguuids[k].name == (*t)->name) {
                        (*t)->uuid = taguuids[k].uuid;
                    }
                }
                if (!(*t)->uuid.len) {
                    (*t)->uuid = deriveuuid(perm, (*t)->name);
                }
                site->tags = push(perm, site->tags, *t);
            }
            Tag *tag = *t;
            if (tag->posts.len && tag->posts[tag->posts.len-1] == p) {
                warn(c->log, p->src, 0, "duplicate tag");
                continue;
            }
            tag->posts = push(perm, tag->posts, p);
        }
    }
    sort(site->tags.data, site->tags.len, tagless, scratch);
}

static b32 skipname(Str name)
{
    return !name.len || name[0]=='.' || name[0]=='_' || name[0]=='#' ||
           name[0]=='~' || endswith(name, "~");
}

// Paths at the top of the source tree that are never published.
static Str excluded[] = {"CLAUDE.md"};

// Is the entry in directory rel (empty at the top) not published?
static b32 skipentry(Str rel, Str name)
{
    if (skipname(name)) {
        return 1;
    }
    for (iz i = 0; !rel.len && i < countof(excluded); i++) {
        if (excluded[i] == name) {
            return 1;
        }
    }
    return 0;
}

static void walk(Ctx *c, Site *site, Str rel, Str outdir, Arena scratch)
{
    Arena *perm = c->perm;
    Str    dir  = rel.len ? srcpath(c, perm, rel) : c->opt->src;
    for (Dirent *e = os_list(c->os, perm, dir); e; e = e->next) {
        if (skipentry(rel, e->name)) {
            continue;
        }

        Str child = join(perm, rel, e->name);
        if (e->isdir) {
            if (child == outdir) {
                continue;  // output directory inside the source tree
            }
            walk(c, site, child, outdir, scratch);
            continue;
        }

        if (endswith(child, ".md")) {
            Arena tmp = scratch;
            Str src = os_read(c->os, perm, srcpath(c, &tmp, child));
            if (!src.data) {
                error(c->log, child, 0, "could not read file");
                continue;
            }
            src = normalize(src);
            if (startswith(src, "---\n")) {
                FrontMatter fm = parsefrontmatter(src, perm);
                if (!fm.ok) {
                    error(c->log, child, fm.errline, fm.err);
                    continue;
                }
                Page *p = alloc<Page>(perm);
                p->src      = child;
                p->out      = concat(perm, cuttail(child, 3), ".html");
                p->title    = fm.title;
                p->body     = fm.body;
                p->bodyline = fm.bodyline;
                site->pages = push(perm, site->pages, p);
                continue;
            }
        }
        site->statics = push(perm, site->statics, child);
    }
}

// Output directory relative to the source directory, when inside it.
// Backslashes are separators too, for Windows.
static Str relativeout(Options *opt, Arena *perm)
{
    Str out = clone(perm, opt->out);
    Str src = clone(perm, opt->src);
    for (iz i = 0; i < out.len; i++) out[i] = out[i]=='\\' ? '/' : out[i];
    for (iz i = 0; i < src.len; i++) src[i] = src[i]=='\\' ? '/' : src[i];
    while (startswith(out, "./")) out = cuthead(out, 2);
    while (endswith(out, "/"))    out = cuttail(out, 1);
    if (src == ".") {
        return out;
    }
    while (endswith(src, "/")) src = cuttail(src, 1);
    if (startswith(out, src) && out.len>src.len && out[src.len]=='/') {
        return cuthead(out, src.len+1);
    }
    return {};
}

static void emit(Ctx *c, Str rel, Str data, Arena scratch)
{
    b32 *seen = upsert(&c->outputs, rel, c->perm);
    if (*seen) {
        error(c->log, rel, 0, "output path generated twice");
        return;
    }
    *seen = 1;

    Str path = join(&scratch, c->opt->out, rel);
    iz  slash = path.len;
    while (slash > 0 && path[slash-1] != '/') slash--;
    if (slash > 1 && !os_mkdirs(c->os, scratch, takehead(path, slash-1))) {
        error(c->log, path, 0, "could not create directory");
        return;
    }
    if (!os_write(c->os, scratch, path, data)) {
        error(c->log, path, 0, "could not write file");
        return;
    }
    c->written++;
}

static void copystatic(Ctx *c, Str rel, Arena scratch)
{
    if (upsert(&c->outputs, rel, 0)) {
        return;  // replaced by a generated file
    }
    Str src = srcpath(c, &scratch, rel);
    Str dst = join(&scratch, c->opt->out, rel);
    iz  slash = dst.len;
    while (slash > 0 && dst[slash-1] != '/') slash--;
    if (slash > 1 && !os_mkdirs(c->os, scratch, takehead(dst, slash-1))) {
        error(c->log, dst, 0, "could not create directory");
        return;
    }
    switch (os_copy(c->os, scratch, src, dst)) {
    case COPY_FAILED:  error(c->log, rel, 0, "could not copy file"); break;
    case COPY_DONE:    c->copied++;  break;
    case COPY_SKIPPED: c->skipped++; break;
    }
}
