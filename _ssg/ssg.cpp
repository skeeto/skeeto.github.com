// ssg: static site generator for nullprogram.com
//
// Unity build: a platform layer (main_posix.cpp, main_win32.cpp,
// main_test.cpp) includes this file and implements the os_* API.
//
// This is free and unencumbered software released into the public domain.
#include "base.cpp"


// Platform API

struct Os;

struct Dirent {
    Dirent *next;
    Str     name;
    i64     size;
    i64     mtime;  // platform units, only compared for changes
    b32     isdir;
};

enum {
    COPY_FAILED,
    COPY_DONE,
    COPY_SKIPPED,
};

// Read an entire file into the arena. Returns a null string on failure.
static Str     os_read(Os *, Arena *, Str path);
// Create or truncate a file with the given contents.
static b32     os_write(Os *, Arena, Str path, Str data);
// Create a directory and all its parents.
static b32     os_mkdirs(Os *, Arena, Str path);
// List a directory, excluding "." and "..", symlinks, and special files.
static Dirent *os_list(Os *, Arena *, Str path);
// Copy a file unless the destination has same size and is not older.
static i32     os_copy(Os *, Arena, Str src, Str dst);
// Current time in Unix epoch seconds.
static i64     os_now(Os *);
// Monotonic clock in milliseconds, only for measuring intervals.
static i64     os_clock(Os *);
// Write to standard output (1) or standard error (2).
static b32     os_print(Os *, i32 fd, Str);
// Pause for a number of milliseconds.
static void    os_sleep(Os *, i32 ms);
// Also: [[noreturn]] os_oom(), declared in base.cpp.


// Diagnostics

struct Log {
    Buf buf;
    i32 errors   = 0;
    i32 warnings = 0;
};

static void logmsg(Log *log, Str kind, Str file, iz line, Str msg)
{
    Buf *b = &log->buf;
    print(b, "ssg: ");
    if (file.len) {
        print(b, file);
        if (line > 0) {
            putbyte(b, ':');
            print(b, (i64)line);
        }
        print(b, ": ");
    }
    print(b, kind);
    print(b, msg);
    putbyte(b, '\n');
}

static void warn(Log *log, Str file, iz line, Str msg)
{
    log->warnings++;
    logmsg(log, "warning: ", file, line, msg);
}

static void error(Log *log, Str file, iz line, Str msg)
{
    log->errors++;
    logmsg(log, "error: ", file, line, msg);
}


#include "highlight.cpp"
#include "markdown.cpp"
#include "frontmatter.cpp"
#include "site.cpp"
#include "newpost.cpp"
#include "templates.cpp"


static Str outpath(Arena *a, Str url)  // "/x/y/" -> "x/y/index.html"
{
    return concat(a, cuthead(url, 1), "index.html");
}

static void render(Ctx *c, Site *site, Arena scratch)
{
    for (iz i = 0; i < site->posts.len; i++) {
        Post *p = site->posts[i];
        Markdown md = markdown(p->body, p->src, p->bodyline, c->perm, scratch, c->log);
        p->html    = md.html;
        p->excerpt = md.excerpt;
    }
    for (iz i = 0; i < site->pages.len; i++) {
        Page *p = site->pages[i];
        p->html = markdown(p->body, p->src, p->bodyline, c->perm, scratch, c->log).html;
    }
}

// Concatenate the stylesheet parts, formerly done with Liquid includes.
static Str stylesheet(Ctx *c, Arena *a)
{
    static Str parts[] = {"main.css", "print.css", "syntax.css", "dark.css"};
    Str data[countof(parts)] = {};
    iz  total = 0;
    for (iz i = 0; i < countof(parts); i++) {
        Str rel = join(a, "css", parts[i]);
        data[i] = os_read(c->os, a, srcpath(c, a, rel));
        if (!data[i].data) {
            error(c->log, rel, 0, "could not read file");
        }
        total += data[i].len + 1;
    }
    Buf b(a, total);
    for (iz i = 0; i < countof(parts); i++) {
        print(&b, data[i]);
        putbyte(&b, '\n');
    }
    return finish(&b);
}

static i32 build(Os *os, Options *opt, Log *log, Arena *perm, Arena scratch)
{
    i64 start = os_clock(os);
    Ctx c  = {};
    c.os   = os;
    c.opt  = opt;
    c.log  = log;
    c.perm = perm;

    Site site = {};
    site.now = opt->now;
    loadposts(&c, &site, scratch);
    walk(&c, &site, {}, relativeout(opt, perm), scratch);
    if (log->errors) {
        return 1;
    }
    render(&c, &site, scratch);

    #define EMIT(path, call) do { \
            Arena tmp = scratch; \
            Buf b(&tmp, 1<<16); \
            call; \
            emit(&c, path, finish(&b), tmp); \
        } while (0)

    for (iz i = 0; i < site.posts.len; i++) {
        Post *p = site.posts[i];
        EMIT(outpath(perm, p->url), postpage(&b, p));
    }
    for (iz i = 0; i < site.tags.len; i++) {
        Tag *t   = site.tags[i];
        Str  dir = concat(perm, concat(perm, "tags/", t->name), "/");
        EMIT(concat(perm, dir, "index.html"), tagpage(&b, t));
        dir = concat(perm, concat(perm, "tags/", t->name), "/");
        EMIT(concat(perm, dir, "feed/index.xml"), tagfeed(&b, &site, t));
    }
    for (iz i = 0; i < site.pages.len; i++) {
        Page *p = site.pages[i];
        EMIT(p->out, aboutpage(&b, p));
    }
    iz limit = opt->excerpts ? site.posts.len : 10;
    EMIT("index.html",       homepage(&b, &site, limit));
    EMIT("index/index.html", archivepage(&b, &site));
    EMIT("tags/index.html",  tagindexpage(&b, &site));
    EMIT("feed/index.xml",   atomfeed(&b, &site));
    EMIT("blog/index.rss",   rssfeed(&b, &site));
    {
        Arena tmp = scratch;
        Str css = stylesheet(&c, &tmp);
        emit(&c, "css/full.css", css, tmp);
    }
    #undef EMIT

    for (iz i = 0; i < site.statics.len; i++) {
        copystatic(&c, site.statics[i], scratch);
    }

    if (!opt->quiet) {
        Buf b(&scratch, 256);
        print(&b, "ssg: ");
        print(&b, (i64)site.posts.len);
        print(&b, " posts, ");
        print(&b, (i64)c.written);
        print(&b, " pages written, ");
        print(&b, (i64)c.copied);
        print(&b, " files copied, ");
        print(&b, (i64)c.skipped);
        print(&b, " unchanged in ");
        print(&b, os_clock(os) - start);
        print(&b, " ms\n");
        os_print(os, 2, finish(&b));
    }
    return log->errors ? 1 : 0;
}

// Debug: render a single Markdown file to standard output.
static i32 rendermd(Os *os, Options *opt, Log *log, Arena *perm, Arena scratch)
{
    Str src = os_read(os, perm, opt->mdfile);
    if (!src.data) {
        error(log, opt->mdfile, 0, "could not read file");
        return 1;
    }
    src = normalize(src);
    iz line = 1;
    if (startswith(src, "---\n")) {
        FrontMatter fm = parsefrontmatter(src, perm);
        if (!fm.ok) {
            error(log, opt->mdfile, fm.errline, fm.err);
            return 1;
        }
        src  = fm.body;
        line = fm.bodyline;
    }
    Markdown md = markdown(src, opt->mdfile, line, perm, scratch, log);
    os_print(os, 1, md.html);
    return 0;
}

// Summarize the source files a build reads, as walked by loadposts() and
// walk(): names, sizes, and modification times, order-independent.
static u64 fingerprint(Os *os, Options *opt, Str rel, Str outdir, Arena scratch)
{
    Str dir = rel.len ? join(&scratch, opt->src, rel) : opt->src;
    u64 sum = 0;
    for (Dirent *e = os_list(os, &scratch, dir); e; e = e->next) {
        b32 posts = !rel.len && e->isdir && e->name == "_posts";
        if (!posts && skipentry(rel, e->name)) {
            continue;
        }
        Str child = join(&scratch, rel, e->name);
        if (e->isdir) {
            if (child != outdir) {
                sum += fingerprint(os, opt, child, outdir, scratch);
            }
            continue;
        }
        u64 h = hash(child);
        h ^= (u64)e->size;
        h *= 1111111111111111111u;
        h ^= (u64)e->mtime;
        h *= 1111111111111111111u;
        sum += h ^ h>>32;
    }
    return sum;
}

// Rebuild whenever the source tree changes, until interrupted. Every
// build starts from the same arenas, so memory use does not grow, and
// each is identical to a fresh build.
[[noreturn]] static void watch(Os *os, Options *opt, Arena perm, Arena scratch)
{
    i32 interval = 250;  // ms
    Str outdir   = relativeout(opt, &perm);
    u64 last     = fingerprint(os, opt, {}, outdir, scratch);
    for (;;) {
        Arena p = perm;
        Log log = {};
        log.buf = Buf(&p, 1<<14);
        if (!opt->fixedtime) {
            opt->now = os_now(os);
        }
        build(os, opt, &log, &p, scratch);
        os_print(os, 2, finish(&log.buf));

        // Wait for a change, then for the tree to settle (e.g. mid-save)
        u64 fp = last;
        while (fp == last) {
            os_sleep(os, interval);
            fp = fingerprint(os, opt, {}, outdir, scratch);
        }
        for (u64 prev = last; fp != prev;) {
            prev = fp;
            os_sleep(os, interval);
            fp = fingerprint(os, opt, {}, outdir, scratch);
        }
        last = fp;
    }
}

static Str usage =
"usage: ssg [-C SRCDIR] [-o OUTDIR] [-t UNIXTIME] [-w] [-n FILE] [-E] [-m FILE]\n"
"  -C DIR    source directory (default: .)\n"
"  -o DIR    output directory (default: _site)\n"
"  -t TIME   build time in Unix seconds (default: now)\n"
"  -w        watch: rebuild whenever the source changes\n"
"  -n FILE   new post: insert a draft into _posts, dated now (or -t);\n"
"            its first line, a \"# Title\" heading, becomes the title\n"
"  -E        debug: list every post and its excerpt on the home page\n"
"  -m FILE   debug: render one Markdown file (after front matter) to stdout\n"
"  -q        quiet: only print warnings and errors\n"
"  -h        print this message\n";

static i32 ssg_main(Os *os, Arena mem, Str *args, i32 nargs)
{
    // Split memory: permanent storage and a scratch region reset per task.
    iz    cap     = mem.end - mem.beg;
    Arena perm    = mem;
    Arena scratch = mem;
    perm.end    = mem.beg + cap/4*3;
    scratch.beg = perm.end;

    Options opt = {};
    opt.src = ".";
    opt.out = "_site";
    for (i32 i = 1; i < nargs; i++) {
        Str a = args[i];
        Str v = i+1 < nargs ? args[i+1] : Str{};
        if (a == "-C" && v.data) {
            opt.src = v; i++;
        } else if (a == "-o" && v.data) {
            opt.out = v; i++;
        } else if (a == "-t" && v.data) {
            opt.now = parseint(v);
            opt.fixedtime = 1;
            i++;
        } else if (a == "-m" && v.data) {
            opt.mdfile = v; i++;
        } else if (a == "-n" && v.data) {
            opt.newpost = v; i++;
        } else if (a == "-w") {
            opt.watch = 1;
        } else if (a == "-E") {
            opt.excerpts = 1;
        } else if (a == "-q") {
            opt.quiet = 1;
        } else if (a == "-h") {
            os_print(os, 1, usage);
            return 0;
        } else {
            os_print(os, 2, usage);
            return 1;
        }
    }
    if (!opt.fixedtime) {
        opt.now = os_now(os);
    }

    if (opt.watch && !opt.mdfile.data && !opt.newpost.data) {
        watch(os, &opt, perm, scratch);
    }

    Log log = {};
    log.buf = Buf(&perm, 1<<14);

    i32 status = 0;
    if (opt.mdfile.data) {
        status = rendermd(os, &opt, &log, &perm, scratch);
    } else if (opt.newpost.data) {
        status = newpost(os, &opt, &log, &perm, scratch);
    } else {
        status = build(os, &opt, &log, &perm, scratch);
    }

    os_print(os, 2, finish(&log.buf));
    if (log.errors && !status) {
        status = 1;
    }
    return status;
}
