// Markdown to HTML: a port of the kramdown 2.5.1 GFM subset used by the blog
//
// Like kramdown there are two passes. The block pass builds a tree and
// collects link definitions. Then, while rendering, each paragraph,
// header, and list item gets a fresh span scanner. The span scanner stops
// where kramdown's does and tries the same parsers in the same order, so
// smart quotes see the same "preceding character". Regular expressions
// are emulated by hand, including backtracking where it matters. Output
// matches kramdown's HTML converter except for code blocks, which use lean
// markup. Unsupported syntax is reported as a warning.
//
// Ruby semantics to keep in mind: \s \w \d are ASCII, while [[:alpha:]],
// \p{Word}, and \b are Unicode. A StringScanner match treats the current
// position as the start of the string, so ^ matches there mid-line.
//
// This is free and unencumbered software released into the public domain.

struct Markdown {
    Str html;
    iz  excerpt;  // length of the excerpt prefix of html
};


// Characters

static u8 at(Str s, iz i)
{
    return i>=0 && i<s.len ? s.data[i] : 0;
}

static Str slice(Str s, iz beg, iz end)
{
    return {s.data+beg, end-beg};
}

static b32 wordbyte(u8 c)
{
    return alnum(c) || c=='_';
}

static b32 namebyte(u8 c)  // [\w.:-]
{
    return wordbyte(c) || c=='.' || c==':' || c=='-';
}

static b32 hexdigit(u8 c)
{
    return digit(c) || (lowercase(c)>='a' && lowercase(c)<='f');
}

static b32 inset(Str set, u8 c)
{
    for (iz i = 0; i < set.len; i++) {
        if (set.data[i] == c) return 1;
    }
    return 0;
}

static i32 cpat(Str s, iz i, iz *len)
{
    if (s.data[i] < 0x80) {
        *len = 1;
        return s.data[i];
    }
    iz  j = i;
    i32 r = utf8decode(s, &j);
    *len = j - i;
    return r;
}

static iz cplen(Str s, iz i)
{
    iz n = 1;
    if (s.data[i] >= 0xc0) cpat(s, i, &n);
    return n;
}

// Approximates \p{Word} beyond ASCII: letters, marks, and digits, but not
// the common punctuation and symbol blocks.
static b32 uniword(i32 c)
{
    // Letters, marks, and connectors among the punctuation and symbols
    static i32 words[][2] = {
        {0x203f, 0x2040}, {0x2054, 0x2054}, {0x2071, 0x2071}, {0x207f, 0x207f},
        {0x2090, 0x209c}, {0x20d0, 0x20f0}, {0x2102, 0x2102}, {0x2107, 0x2107},
        {0x210a, 0x2113}, {0x2115, 0x2115}, {0x2119, 0x211d}, {0x2124, 0x2124},
        {0x2126, 0x2126}, {0x2128, 0x2128}, {0x212a, 0x212d}, {0x212f, 0x2139},
        {0x213c, 0x213f}, {0x2145, 0x2149}, {0x214e, 0x214e}, {0x2160, 0x2188},
        {0x24b6, 0x24e9}, {0x2e2f, 0x2e2f}, {0x3005, 0x3007}, {0x3021, 0x302f},
        {0x3031, 0x3035}, {0x3038, 0x303c},
    };
    if (c < 0x80) return wordbyte((u8)c);
    if (c < 0xc0) return c==0xaa || c==0xb5 || c==0xba;
    if (c==0xd7 || c==0xf7) return 0;
    if (c>=0x2000 && c<0x3040) {
        for (iz i = 0; i < countof(words); i++) {
            if (c>=words[i][0] && c<=words[i][1]) return 1;
        }
        // punctuation, symbols, arrows, radicals, CJK punctuation
        return c>=0x2c00 && c<0x2e00;
    }
    if (c>=0xe000 && c<0xf900) return 0;  // private use
    if (c>=0xfe10 && c<0xfe70) return 0;  // vertical and small forms
    if (c>=0xff00 && c<0xff10) return 0;  // fullwidth punctuation
    if (c>=0xff1a && c<0xff21) return 0;
    if (c>=0xff3b && c<0xff41) return c == 0xff3f;
    if (c>=0xff5b && c<0xff66) return 0;
    if (c>=0xffe0 && c<0xfff0) return 0;  // fullwidth symbols
    if (c>=0x1f000) return 0;             // emoji and other symbols
    return 1;
}

// [[:alpha:]] and [[:alnum:]] beyond ASCII: \p{Word} except for connector
// punctuation and the combining marks among the symbols
static b32 uniletter(i32 c)
{
    b32 other = (c>=0x203f && c<=0x2040) || c==0x2054 || (c>=0x20d0 && c<=0x20f0) ||
                (c>=0x302a && c<=0x302f) || c==0xff3f;
    return !other && uniword(c);
}

static b32 unialpha(i32 c)
{
    return c<0x80 ? letter((u8)c) : uniletter(c);
}

static b32 unialnum(i32 c)
{
    return c<0x80 ? alnum((u8)c) : uniletter(c);
}

static b32 wordat(Str s, iz i)
{
    iz n;
    return i>=0 && i<s.len && uniword(cpat(s, i, &n));
}

// Ruby's String#downcase for Latin, Greek, Cyrillic, Armenian, and
// letter-like symbols, except for the rarest letters in Latin Extended-B,
// archaic Greek, and Greek Extended. Other scripts are unchanged. U+0130
// becomes two code points (0x69 then U+0307), which callers handle.
static i32 downcase(i32 c)
{
    if (c < 0x80) return lowercase((u8)c);
    if ((c>=0xc0 && c<=0xde && c!=0xd7) || (c>=0x391 && c<=0x3ab && c!=0x3a2) ||
            (c>=0x410 && c<=0x42f)) {
        return c + 0x20;
    }
    if (c>=0x400 && c<=0x40f) return c + 0x50;
    if (c>=0x531 && c<=0x556) return c + 0x30;
    if (c>=0x388 && c<=0x38a) return c + 37;
    if (c==0x38e || c==0x38f) return c + 63;
    if (c>=0x2160 && c<=0x216f) return c + 16;  // Roman numerals
    if (c>=0x24b6 && c<=0x24cf) return c + 26;  // circled letters
    if (c>=0xff21 && c<=0xff3a) return c + 32;  // fullwidth letters
    switch (c) {
    case 0x1c4: case 0x1c7: case 0x1ca: case 0x1f1: return c + 2;  // DŽ LJ NJ DZ
    case 0x1c5: case 0x1c8: case 0x1cb: case 0x1af: return c + 1;  // Dž Lj Nj Ư
    case 0x178:  return 0xff;
    case 0x386:  return 0x3ac;
    case 0x38c:  return 0x3cc;
    case 0x4c0:  return 0x4cf;
    case 0x1e9e: return 0xdf;
    case 0x2126: return 0x3c9;
    case 0x212a: return 'k';
    case 0x212b: return 0xe5;
    case 0x2132: return 0x214e;
    case 0x2183: return 0x2184;
    }
    // Ranges of alternating upper and lower case pairs
    b32 odd  = (c>=0x139 && c<=0x148) || (c>=0x179 && c<=0x17e) ||
               (c>=0x1cd && c<=0x1dc) || (c>=0x4c1 && c<=0x4ce);
    b32 even = (c>=0x100 && c<=0x137 && c!=0x130) || (c>=0x14a && c<=0x177) ||
               (c>=0x1a0 && c<=0x1a5) || (c>=0x1de && c<=0x1ef) ||
               (c>=0x1f2 && c<=0x1f5) || (c>=0x1f8 && c<=0x21f) ||
               (c>=0x222 && c<=0x233) || (c>=0x246 && c<=0x24f) ||
               (c>=0x3d8 && c<=0x3ef) || (c>=0x460 && c<=0x481) ||
               (c>=0x48a && c<=0x4bf) || (c>=0x4d0 && c<=0x52f) ||
               (c>=0x1e00 && c<=0x1e95) || (c>=0x1ea0 && c<=0x1eff);
    return (odd && (c&1)) || (even && !(c&1)) ? c+1 : c;
}

static b32 isquote(u8 c)
{
    return c=='"' || c=='\'';
}

static b32 blank(u8 c)  // [ \t]
{
    return c==' ' || c=='\t';
}

static iz lineend(Str s, iz i)
{
    for (; i<s.len && s.data[i]!='\n'; i++) {}
    return i;
}

static iz nextline(Str s, iz i)
{
    i = lineend(s, i);
    return i<s.len ? i+1 : i;
}

static iz skipspaces(Str s, iz i, iz max)
{
    for (iz n = 0; n<max && at(s, i)==' '; n++, i++) {}
    return i;
}

static iz skipblanks(Str s, iz i)  // [ \t]*
{
    for (; blank(at(s, i)); i++) {}
    return i;
}

static iz skipws(Str s, iz i)  // \s*
{
    for (; i<s.len && whitespace(s.data[i]); i++) {}
    return i;
}

// Is the rest of the line whitespace, ending in a newline (\s*?\n)?
static iz restblank(Str s, iz i)
{
    for (; i<s.len && whitespace(s.data[i]) && s.data[i]!='\n'; i++) {}
    return at(s, i)=='\n' ? i+1 : -1;
}

static b32 equalfold(Str a, Str b)
{
    if (a.len != b.len) return 0;
    for (iz i = 0; i < a.len; i++) {
        if (lowercase(a.data[i]) != lowercase(b.data[i])) return 0;
    }
    return 1;
}

static b32 startsfold(Str s, iz i, Str prefix)
{
    return i+prefix.len<=s.len && equalfold(slice(s, i, i+prefix.len), prefix);
}

static b32 startsat(Str s, iz i, Str prefix)
{
    return i+prefix.len<=s.len && slice(s, i, i+prefix.len)==prefix;
}

// Like find(), but fast: the first index of needle in s at or after i.
static iz search(Str s, iz i, Str needle)
{
    u8 c = needle.data[0];
    for (iz end = s.len-needle.len; i <= end; i++) {
        if (s.data[i]==c && startsat(s, i, needle)) return i;
    }
    return -1;
}

static void putcp(Buf *b, i32 c)
{
    if (c < 0x80) {
        putbyte(b, (u8)c);
    } else if (c < 0x800) {
        putbyte(b, (u8)(0xc0 | c>>6));
        putbyte(b, (u8)(0x80 | (c & 0x3f)));
    } else if (c < 0x10000) {
        putbyte(b, (u8)(0xe0 | c>>12));
        putbyte(b, (u8)(0x80 | (c>>6 & 0x3f)));
        putbyte(b, (u8)(0x80 | (c & 0x3f)));
    } else {
        putbyte(b, (u8)(0xf0 | c>>18));
        putbyte(b, (u8)(0x80 | (c>>12 & 0x3f)));
        putbyte(b, (u8)(0x80 | (c>>6 & 0x3f)));
        putbyte(b, (u8)(0x80 | (c & 0x3f)));
    }
}

static void putspaces(Buf *b, iz n)
{
    for (iz i = 0; i < n; i++) putbyte(b, ' ');
}


// Output escaping, matching kramdown's escape_html

// REXML's REFERENCE_RE at s[i]: &name; &#123; &#x7b;
static b32 isref(Str s, iz i)
{
    iz j = i + 1;
    u8 c = at(s, j);
    if (wordbyte(c) || c==':') {
        for (j++; namebyte(at(s, j)); j++) {}
    } else if (c=='#' && digit(at(s, j+1))) {
        for (j++; digit(at(s, j)); j++) {}
    } else if (c=='#' && at(s, j+1)=='x' && hexdigit(at(s, j+2))) {
        for (j += 2; hexdigit(at(s, j)); j++) {}
    } else {
        return 0;
    }
    return at(s, j) == ';';
}

// Escape < > and &, except & starting a reference. Attributes also
// escape the double quote.
static void printesc(Buf *b, Str s, b32 attr)
{
    iz last = 0;
    for (iz i = 0; i < s.len; i++) {
        u8 c = s.data[i];
        if (c!='<' && c!='>' && c!='&' && c!='"') continue;
        Str rep = c=='<' ? Str("&lt;") : c=='>' ? Str("&gt;") :
                  c=='&' ? (isref(s, i) ? Str() : Str("&amp;")) :
                  attr ? Str("&quot;") : Str();
        if (rep.len) {
            print(b, slice(s, last, i));
            print(b, rep);
            last = i + 1;
        }
    }
    print(b, cuthead(s, last));
}


// HTML knowledge, the element lists from kramdown's parser/html.rb

static Str html_span =
    " a abbr acronym b big bdo br button cite code del dfn em i img input"
    " ins kbd label mark option q rb rbc rp rt rtc ruby samp select small"
    " span strong sub sup time tt u var ";
static Str html_block =
    " address article aside applet body blockquote caption col colgroup dd"
    " div dl dt fieldset figcaption footer form h1 h2 h3 h4 h5 h6 header"
    " hgroup hr html head iframe legend menu li main map nav ol optgroup p"
    " pre section summary table tbody td th thead tfoot tr ul ";
static Str html_void =
    " area base br col command embed hr img input keygen link meta param"
    " source track wbr ";
static Str html_model_block =
    " address applet article aside blockquote body dd details div dl"
    " fieldset figure figcaption footer form header hgroup iframe li main"
    " map menu nav noscript object section summary td ";
static Str html_model_span =
    " a abbr acronym b bdo big button cite caption del dfn dt em h1 h2 h3"
    " h4 h5 h6 i ins label legend optgroup p q rb rbc rp rt rtc ruby select"
    " small span strong sub sup th tt ";
static Str html_model_raw =
    " script style math option textarea pre code kbd samp var ";

enum {
    H_SPAN  = 1<<0,  // HTML_SPAN_ELEMENTS
    H_BLOCK = 1<<1,  // HTML_BLOCK_ELEMENTS
    H_VOID  = 1<<2,  // HTML_ELEMENTS_WITHOUT_BODY
    H_PARSE = 1<<3,  // span or block content model: Markdown inside
    H_RAW   = 1<<4,  // explicitly raw content model
};

static b32 inlist(Str list, Str name, b32 fold)
{
    u8 buf[16];
    if (!name.len || name.len > countof(buf)-2) return 0;
    buf[0] = buf[name.len+1] = ' ';
    for (iz i = 0; i < name.len; i++) {
        buf[i+1] = fold ? lowercase(name.data[i]) : name.data[i];
    }
    Str key = {buf, name.len+2};
    for (iz i = 0; i+key.len <= list.len; i++) {
        if (list.data[i]==' ' && startsat(list, i, key)) return 1;
    }
    return 0;
}

// Flags for a tag name, ignoring case. Zero for unknown elements, which
// kramdown leaves in their original case.
static i32 htmlflags(Str name)
{
    b32 parse = inlist(html_model_block, name, 1) || inlist(html_model_span, name, 1);
    return (inlist(html_span,  name, 1) ? H_SPAN  : 0) |
           (inlist(html_block, name, 1) ? H_BLOCK : 0) |
           (inlist(html_void,  name, 1) ? H_VOID  : 0) |
           (inlist(html_model_raw, name, 1) ? H_RAW : 0) |
           (parse ? H_PARSE : 0);
}

static void printname(Buf *b, Str name, b32 known)
{
    for (iz i = 0; i < name.len; i++) {
        putbyte(b, known ? lowercase(name.data[i]) : name.data[i]);
    }
}

// XML NCName with Unicode classes: [[:alpha:]_][-[:alnum:]._]*
static iz ncname(Str s, iz i)
{
    iz n;
    if (i >= s.len) return -1;
    i32 c = cpat(s, i, &n);
    if (c!='_' && !unialpha(c)) return -1;
    for (i += n; i < s.len; i += n) {
        c = cpat(s, i, &n);
        if (c!='-' && c!='.' && c!='_' && !unialnum(c)) break;
    }
    return i;
}

// REXML UNAME_STR: an NCName with an optional prefix
static iz xmlname(Str s, iz i)
{
    iz j = ncname(s, i);
    if (j>=0 && at(s, j)==':') {
        iz k = ncname(s, j+1);
        if (k >= 0) return k;
    }
    return j;
}

struct HtmlTag {
    iz  end;        // just past the tag, or -1 when there is no match
    Str name;
    Str attrs;      // attribute source
    b32 selfclose;
};

// HTML_TAG_RE: a start tag whose attribute values are words or quoted.
static HtmlTag matchtag(Str s, iz i)
{
    HtmlTag r = {};
    r.end = -1;
    iz j = at(s, i)=='<' ? xmlname(s, i+1) : -1;
    if (j < 0) return r;
    iz k = j;
    for (;;) {
        iz a = skipws(s, k);
        iz e = a>k ? xmlname(s, a) : -1;
        if (e < 0) break;
        iz v = skipws(s, e);
        if (at(s, v) == '=') {
            v = skipws(s, v+1);
            if (wordat(s, v)) {
                for (; wordat(s, v); v += cplen(s, v)) {}
                e = v;
            } else if (isquote(at(s, v))) {
                iz w = v + 1;
                for (; w<s.len && s.data[w]!=s.data[v]; w++) {}
                if (w < s.len) e = w + 1;
            }
        }
        k = e;
    }
    iz end = skipws(s, k);
    b32 selfclose = at(s, end)=='/';
    end += selfclose;
    if (at(s, end) != '>') return r;
    r.end       = end + 1;
    r.name      = slice(s, i+1, j);
    r.attrs     = slice(s, j, k);
    r.selfclose = selfclose;
    return r;
}

// HTML_TAG_CLOSE_RE
static HtmlTag matchclose(Str s, iz i)
{
    HtmlTag r = {};
    r.end = -1;
    iz j = at(s, i)=='<' && at(s, i+1)=='/' ? xmlname(s, i+2) : -1;
    if (j < 0) return r;
    iz k = skipws(s, j);
    if (at(s, k) != '>') return r;
    r.end  = k + 1;
    r.name = slice(s, i+2, j);
    return r;
}

// Find the end of a delimited construct like <!-- ... -->, or -1.
static iz delimited(Str s, iz i, Str open, Str close)
{
    if (!startsat(s, i, open)) return -1;
    iz j = search(s, i+open.len, close);
    return j<0 ? -1 : j+close.len;
}

// Print attributes the way kramdown re-serializes them: lowercase names
// for known elements, first position but last value for duplicates,
// double-quoted escaped values, no empty id, and no markdown attribute.
static void printattrs(Buf *b, Str attrs, b32 known, b32 span)
{
    struct Attr { Str name, value; };
    Attr list[32];
    iz   n = 0;
    for (iz i = 0; i < attrs.len;) {
        iz a = skipws(attrs, i);
        iz e = xmlname(attrs, a);
        if (e < 0) break;
        Attr attr = {slice(attrs, a, e), {attrs.data+e, 0}};
        iz v = skipws(attrs, e);
        i = e;
        if (v<attrs.len && attrs.data[v]=='=') {
            v = skipws(attrs, v+1);
            if (wordat(attrs, v)) {
                iz w = v;
                for (; wordat(attrs, w); w += cplen(attrs, w)) {}
                attr.value = slice(attrs, v, w);
                i = w;
            } else if (v<attrs.len && isquote(attrs.data[v])) {
                iz w = v + 1;
                for (; w<attrs.len && attrs.data[w]!=attrs.data[v]; w++) {}
                if (w < attrs.len) {
                    attr.value = slice(attrs, v+1, w);
                    i = w + 1;
                }
            }
        }
        b32 dup = 0;
        for (iz k = 0; k < n; k++) {
            if (known ? equalfold(list[k].name, attr.name) : list[k].name==attr.name) {
                list[k].value = attr.value;
                dup = 1;
            }
        }
        if (!dup && n<countof(list)) {
            list[n++] = attr;
        }
    }
    for (iz k = 0; k < n; k++) {
        Str name = list[k].name;
        b32 id   = known ? equalfold(name, "id") : name=="id";
        b32 md   = known ? equalfold(name, "markdown") : name=="markdown";
        if (md || (id && !trim(list[k].value).len)) {
            continue;
        }
        putbyte(b, ' ');
        printname(b, list[k].name, known);
        print(b, "=\"");
        Str v = list[k].value;
        if (span) {
            // Newline runs in span tag attributes become one space
            iz last = 0;
            for (iz i = 0; i < v.len; i++) {
                if (v.data[i] == '\n') {
                    printesc(b, slice(v, last, i), 1);
                    putbyte(b, ' ');
                    for (; i+1<v.len && v.data[i+1]=='\n'; i++) {}
                    last = i + 1;
                }
            }
            v = cuthead(v, last);
        }
        printesc(b, v, 1);
        putbyte(b, '"');
    }
}

// LAZY_END_HTML_START and LAZY_END_HTML_STOP exclude span elements and
// script, compared case-sensitively up to a word boundary.
static b32 lazyspanname(Str s, iz i)
{
    iz j = i;
    for (; wordat(s, j); j += cplen(s, j)) {}
    Str w = slice(s, i, j);
    return w=="script" || inlist(html_span, w, 0);
}

static b32 lazyhtml(Str s, iz i)  // both start and stop, at s[i]
{
    if (at(s, i) != '<') return 0;
    if (at(s, i+1) != '/') {
        return !lazyspanname(s, i+1) && xmlname(s, i+1)>=0;
    }
    if (lazyspanname(s, i+2)) return 0;
    iz j = xmlname(s, i+2);
    return j>=0 && at(s, skipws(s, j))=='>';
}


// Block pass

enum {
    B_ROOT, B_BLANK, B_P, B_HEADER, B_CODE, B_QUOTE, B_UL, B_OL, B_LI,
    B_HR, B_HTML, B_COMMENT, B_EOB, B_TEXT,
};

// A block scanner. Nested sources (blockquotes, list items) map their
// lines one-to-one onto the parent starting at poff.
struct MdSrc {
    Str    s;
    iz     pos;
    MdSrc *parent;
    iz     poff;
    iz     line0;
    iz     noff;   // newlines counted before noff (a cache)
    iz     nls;
    iz     fences; // 1 + start of the last line that could close a fence
};

struct MdBlock {
    MdBlock *next;
    MdBlock *child;
    MdBlock *tail;
    MdSrc   *src;
    iz       off;          // start in src
    i32      type;
    i32      level;        // header level
    b32      transparent;  // paragraph without <p>
    b32      boundary;     // EOB marker, as opposed to a definition
    Str      text;         // raw text, code, or serialized HTML
    Str      lang;         // fenced code language
    Str      id;           // explicit header id
};

struct MdDef {
    Str url;
    Str title;  // null when absent
};

struct Md {
    Arena      *a;
    Log        *log;
    Str         name;
    Map<MdDef> *defs;
    Map<i32>   *ids;
    i32         depth;  // block nesting
    b32         deep;   // reported nesting beyond MAXDEPTH
};

// Nesting beyond this is treated as text, bounding recursion on
// pathological input like 10,000 unclosed <span> tags. Posts nest a few
// levels, and kramdown itself fails near 1,000. At this limit a Debug
// build needs under 1 MiB of stack, the Windows default, for block and
// span nesting together.
enum { MAXDEPTH = 100 };

static iz srcline(MdSrc *src, iz off)
{
    off = off<src->s.len ? off : src->s.len;
    if (off < src->noff) {
        src->noff = src->nls = 0;
    }
    for (; src->noff < off; src->noff++) {
        src->nls += src->s.data[src->noff] == '\n';
    }
    return (src->parent ? srcline(src->parent, src->poff) : src->line0) + src->nls;
}

static void mdwarn(Md *m, iz line, Str msg)
{
    warn(m->log, m->name, line, msg);
}

static MdBlock *addblock(Md *m, MdBlock *tree, i32 type, MdSrc *src, iz off)
{
    MdBlock *b = alloc<MdBlock>(m->a);
    b->type = type;
    b->src  = src;
    b->off  = off;
    if (tree->tail) {
        tree->tail->next = b;
    } else {
        tree->child = b;
    }
    tree->tail = b;
    return b;
}

static MdSrc *newsrc(Md *m, Str s, MdSrc *parent, iz poff)
{
    MdSrc *src  = alloc<MdSrc>(m->a);
    src->s      = s;
    src->parent = parent;
    src->poff   = poff;
    return src;
}

// BLANK_LINE: whitespace-only lines, returning the end or -1.
static iz blankend(Str s, iz i)
{
    iz last = -1;
    for (; i<s.len && whitespace(s.data[i]); i++) {
        if (s.data[i] == '\n') last = i;
    }
    return last<0 ? -1 : last+1;
}

static b32 indentat(Str s, iz i)  // INDENT
{
    return at(s, i)=='\t' || startsat(s, i, "    ");
}

// IAL_BLOCK: {:attributes} alone on a line
static iz ialblock(Str s, iz i)
{
    if (at(s, i)!='{' || at(s, i+1)!=':' || at(s, i+2)==':' || at(s, i+2)=='/') {
        return -1;
    }
    iz j = i + 2;
    for (; j<s.len && s.data[j]!='}'; j++) {
        j += s.data[j]=='\\' && at(s, j+1)=='}';
    }
    return j>i+2 && j<s.len ? restblank(s, j+1) : -1;
}

static iz eobmarker(Str s, iz i)  // EOB_MARKER: ^ alone on a line
{
    return at(s, i)=='^' ? restblank(s, i+1) : -1;
}

// LAZY_END: where blockquotes and lazy lines stop
static b32 lazyend(Str s, iz i)
{
    if (i>=s.len || (i==s.len-1 && s.data[i]=='\n')) return 1;
    if (blankend(s, i) >= 0) return 1;
    iz j = skipspaces(s, i, 3);
    return ialblock(s, j)>=0 || eobmarker(s, i)>=0 || lazyhtml(s, j);
}

// LIST_START_UL or LIST_START_OL with limited indentation: returns the
// index of the separator after the marker, or -1.
static iz liststart(Str s, iz i, i32 type, iz maxspaces)
{
    i = skipspaces(s, i, maxspaces);
    u8 c = at(s, i);
    if (type == B_UL) {
        if (c!='*' && c!='+' && c!='-') return -1;
        i++;
    } else {
        if (!digit(c)) return -1;
        for (; digit(at(s, i)); i++) {}
        if (at(s, i) != '.') return -1;
        i++;
    }
    c = at(s, i);
    return (blank(c) || c=='|') && lineend(s, i)<s.len ? i : -1;
}

static b32 anylist(Str s, iz i)
{
    return liststart(s, i, B_UL, 3)>=0 || liststart(s, i, B_OL, 3)>=0;
}

static iz atxlevel(Str s, iz i)  // ATX_HEADER_START (GFM)
{
    iz n = 0;
    for (; at(s, i+n)=='#'; n++) {}
    return n>=1 && n<=6 && blank(at(s, i+n)) && lineend(s, i)<s.len ? n : 0;
}

static iz fencerun(Str s, iz i)
{
    iz n = 0;
    for (; at(s, i+n)=='~' || at(s, i+n)=='`'; n++) {}
    return n;
}

static b32 dlstart(Str s, iz i)  // DEFINITION_LIST_START
{
    i = skipspaces(s, i, 3);
    u8 c = at(s, i+1);
    return at(s, i)==':' && (blank(c) || c=='|') && lineend(s, i)<s.len;
}

// PARAGRAPH_END_GFM
static b32 paraend(Str s, iz i)
{
    if (lazyend(s, i) || anylist(s, i) || atxlevel(s, i) || dlstart(s, i)) {
        return 1;
    }
    iz j = skipspaces(s, i, 3);
    return at(s, j)=='>' || fencerun(s, j)>=3;
}

static b32 hrule(Str s, iz i)  // HR_START
{
    i = skipspaces(s, i, 3);
    u8 c = at(s, i);
    if (c!='*' && c!='-' && c!='_') return 0;
    iz n = 0;
    for (; i<s.len && (s.data[i]==c || blank(s.data[i])); i++) {
        n += s.data[i] == c;
    }
    return n>=3 && at(s, i)=='\n';
}

static b32 afterboundary(MdBlock *tree)
{
    MdBlock *last = tree->tail;
    return !last || last->type==B_BLANK || (last->type==B_EOB && last->boundary);
}

static void parseblocks(Md *m, MdBlock *tree, MdSrc *src);

static b32 blankline(Md *m, MdBlock *tree, MdSrc *src)
{
    iz end = blankend(src->s, src->pos);
    if (end < 0) return 0;
    if (tree->tail && tree->tail->type==B_BLANK) {
        tree->tail->text = "\n\n";  // only compared against "\n"
    } else {
        MdBlock *b = addblock(m, tree, B_BLANK, src, src->pos);
        b->text = slice(src->s, src->pos, end);
    }
    src->pos = end;
    return 1;
}

// A line of indented code: INDENT[ \t]*\S.*\n
static b32 codeline(Str s, iz i)
{
    if (!indentat(s, i)) return 0;
    i += at(s, i)=='\t' ? 1 : 4;
    for (; blank(at(s, i)); i++) {}
    return i<s.len && !whitespace(s.data[i]) && lineend(s, i)<s.len;
}

// Does the line have a non-space character and a newline (.*\S.*\n)?
static b32 hastext(Str s, iz i)
{
    iz  e    = lineend(s, i);
    b32 text = 0;
    for (iz k = i; k < e; k++) {
        text |= !whitespace(s.data[k]);
    }
    return text && e<s.len;
}

// A lazy line continuing a code block or list item: non-blank, and not
// an IAL, EOB, or block HTML after up to maxspaces spaces.
static b32 lazyline(Str s, iz i, iz maxspaces, b32 eob)
{
    iz j = skipspaces(s, i, maxspaces);
    return hastext(s, i) && ialblock(s, j)<0 && !lazyhtml(s, j) &&
           (!eob || eobmarker(s, i)<0);
}

static b32 indented(Md *m, MdBlock *tree, MdSrc *src)
{
    Str s   = src->s;
    iz  pos = src->pos;
    if (!indentat(s, pos)) return 0;

    iz end = pos;
    for (;;) {
        iz i = blankend(s, end);
        i = i<0 ? end : i;
        if (!codeline(s, i)) break;
        for (; codeline(s, i); i = nextline(s, i)) {}
        for (; lazyline(s, i, 3, 1) && !whitespace(at(s, skipblanks(s, i)));
               i = nextline(s, i)) {}
        end = i;
    }
    if (end == pos) {
        return 0;  // e.g. "    \v": kramdown would loop forever
    }

    // Join lazy lines with a space, then remove one level of indentation
    Str data = slice(s, pos, end);
    Str code = {allocbytes(m->a, data.len), 0};
    for (iz i = 0; i < data.len;) {
        i += at(data, i)=='\t' ? 1 : startsat(data, i, "    ") ? 4 : 0;
        for (; i < data.len; i++) {
            u8 c = data.data[i];
            if (c == '\n') {
                iz j = skipspaces(data, i+1, 3);
                if (j<data.len && !whitespace(data.data[j])) {
                    c = ' ';
                } else {
                    code.data[code.len++] = c;
                    i++;
                    break;
                }
            }
            code.data[code.len++] = c;
        }
    }
    MdBlock *b = addblock(m, tree, B_CODE, src, pos);
    b->text  = code;
    src->pos = end;
    return 1;
}

// The fence lengths n that a line can close: after up to three spaces,
// fence[0..n) followed by more fence[n-1], then blanks (^[ ]{0,3}\1\2*\s*?\n).
// The range is empty when lo > hi.
struct FenceRange {
    iz lo;
    iz hi;
};

static FenceRange closerrange(Str s, iz k, Str fence)
{
    FenceRange r = {1, 0};
    iz c   = skipspaces(s, k, 3);
    iz len = fencerun(s, c);
    if (!len || restblank(s, c+len)<0) return r;
    iz p = 0;  // common prefix with the fence
    for (; p<len && p<fence.len && s.data[c+p]==fence.data[p]; p++) {}
    iz t = len - 1;  // start of the run's uniform tail
    for (; t>0 && s.data[c+t-1]==s.data[c+len-1]; t--) {}
    r.lo = t + 1;
    r.hi = p;
    return r;
}

static b32 fenced(Md *m, MdBlock *tree, MdSrc *src)
{
    Str s   = src->s;
    iz  pos = src->pos;
    iz  i   = skipspaces(s, pos, 3);
    iz  run = fencerun(s, i);
    if (run < 3) return 0;
    Str fence = slice(s, i, i+run);

    // Info string: \s*?((\S+?)(?:\?\S*)?)?\s*?\n
    // The regex backtracks over the fence length, so a longer opening
    // fence may pair with a shorter closer, its excess starting the info.
    iz  j    = i + run;
    for (; j<s.len && whitespace(s.data[j]) && s.data[j]!='\n'; j++) {}
    iz  wend = j;
    for (; wend<s.len && !whitespace(s.data[wend]); wend++) {}
    iz  pend = i + run;
    for (; pend<s.len && !whitespace(s.data[pend]); pend++) {}
    b32 full = restblank(s, wend) >= 0;  // the whole run as the fence
    b32 part = restblank(s, pend) >= 0;  // a shorter fence
    if (!full && !part) return 0;
    iz  body = nextline(s, i);

    // Skip the search when no closer can follow, e.g. many unclosed fences
    if (!src->fences) {
        src->fences = 1;
        for (iz k = 0; k < s.len; k = nextline(s, k)) {
            iz c = skipspaces(s, k, 3);
            iz n = fencerun(s, c);
            if (n>=3 && restblank(s, c+n)>=0) src->fences = 1 + k;
        }
    }
    if (body >= src->fences) return 0;

    // The longest usable fence, then the first line closing it
    iz best = 0;
    for (iz k = body; k < s.len; k = nextline(s, k)) {
        FenceRange r = closerrange(s, k, fence);
        iz n = full && r.hi==run ? run : part ? (r.hi<run ? r.hi : run-1) : 0;
        if (n>=3 && n>=r.lo && n>best) best = n;
        if (best == run) break;
    }
    if (!best) return 0;
    for (iz k = body; k < s.len; k = nextline(s, k)) {
        FenceRange r = closerrange(s, k, fence);
        if (r.lo<=best && best<=r.hi) {
            MdBlock *b = addblock(m, tree, B_CODE, src, pos);
            b->text  = slice(s, body, k);
            b->lang  = best==run ? slice(s, j, wend) : slice(s, i+best, pend);
            src->pos = nextline(s, k);
            return 1;
        }
    }
    assert(0);
    return 0;
}

static b32 blockquote(Md *m, MdBlock *tree, MdSrc *src)
{
    Str s   = src->s;
    iz  pos = src->pos;
    if (at(s, skipspaces(s, pos, 3)) != '>') return 0;

    iz end = nextline(s, pos);
    for (; !lazyend(s, end); end = nextline(s, end)) {}

    // Strip ^ {0,3}> ? from every line
    Str text = {allocbytes(m->a, end-pos), 0};
    for (iz i = pos; i < end;) {
        iz j = skipspaces(s, i, 3);
        if (at(s, j) == '>') {
            i = j + 1 + (at(s, j+1)==' ');
        }
        iz e = nextline(s, i);
        copybytes(text.data+text.len, s.data+i, e-i);
        text.len += e - i;
        i = e;
    }

    MdBlock *b = addblock(m, tree, B_QUOTE, src, pos);
    src->pos = end;
    parseblocks(m, b, newsrc(m, text, src, pos));
    return 1;
}

// HEADER_ID: a trailing {#id}
static Str headerid(Str *text)
{
    Str t = *text;
    if (!t.len || t.data[t.len-1]!='}') return {};
    iz i = t.len - 2;
    for (; i >= 0 && t.data[i] != '{'; i--) {}
    if (i<1 || !blank(t.data[i-1]) || at(t, i+1)!='#') return {};
    Str id = slice(t, i+2, t.len-1);
    for (iz k = 0; k < id.len; k++) {
        u8  c = id.data[k];
        b32 ok = letter(c) || c=='_' || c==':' || c>=0x80 ||
                 (k && (digit(c) || c=='-' || c=='.'));
        if (!ok) return {};
    }
    if (!id.len) return {};
    *text = trimright(slice(t, 0, i-1));
    return id;
}

static b32 atxheader(Md *m, MdBlock *tree, MdSrc *src)
{
    Str s     = src->s;
    iz  pos   = src->pos;
    iz  level = atxlevel(s, pos);
    if (!level) return 0;

    iz  i    = pos + level;
    for (; blank(at(s, i)); i++) {}
    iz  eol  = lineend(s, i);
    Str text = trimright(slice(s, i, eol));
    Str id   = headerid(&text);
    // text.sub!(/[\t ]#+\z/, '') && text.rstrip!
    iz  k    = text.len;
    for (; k>0 && text.data[k-1]=='#'; k--) {}
    if (k<text.len && k>0 && blank(text.data[k-1])) {
        text = trimright(takehead(text, k-1));
    }
    if (!text.len) return 0;

    MdBlock *b = addblock(m, tree, B_HEADER, src, pos);
    b->level = (i32)level;
    b->text  = text;
    b->id    = id;
    src->pos = eol + 1;
    return 1;
}

static b32 hrblock(Md *m, MdBlock *tree, MdSrc *src)
{
    if (!hrule(src->s, src->pos)) return 0;
    addblock(m, tree, B_HR, src, src->pos);
    src->pos = nextline(src->s, src->pos);
    return 1;
}

// List item content_re: indented by at least ind columns, non-blank
static b32 contentline(Str s, iz i, iz ind)
{
    for (i32 alt = 0; alt < 2; alt++) {
        iz  k  = i;
        b32 ok = 1;
        for (iz g = 0; ok && g<ind/4+alt; g++) {
            if (at(s, k) == '\t') {
                k++;
            } else if (startsat(s, k, "    ")) {
                k += 4;
            } else {
                ok = 0;
            }
        }
        for (iz n = 0; ok && !alt && n<ind%4; n++) {
            ok = at(s, k++) == ' ';
        }
        if (ok && hastext(s, k)) {
            return 1;
        }
    }
    return 0;
}

struct MdPiece {
    i32 kind;    // PIECE_*
    iz  spaces;  // leading spaces to insert
    Str text;
    iz  off;     // line start in the list source
};

enum { PIECE_ITEM, PIECE_CHUNK, PIECE_LINE };

static b32 list(Md *m, MdBlock *tree, MdSrc *src)
{
    Str s = src->s;
    i32 type = liststart(s, src->pos, B_UL, 3)>=0 ? B_UL :
               liststart(s, src->pos, B_OL, 3)>=0 ? B_OL : 0;
    if (!type) return 0;
    MdBlock *list = addblock(m, tree, type, src, src->pos);

    Slice<MdPiece> pieces = {};
    iz  ind       = 0;
    iz  maxspaces = 3;
    b32 nested    = 0;
    b32 lastblank = 0;
    b32 eob       = 0;
    while (src->pos < s.len) {
        iz pos = src->pos;
        iz e;
        if (lastblank && hrule(s, pos)) {
            break;
        } else if ((e = eobmarker(s, pos)) >= 0) {
            mdwarn(m, srcline(src, pos), "the end-of-block marker is not supported");
            src->pos = e;
            eob = 1;
            break;
        } else if ((e = liststart(s, pos, type, maxspaces)) >= 0) {
            if (type==B_UL && s.data[e-1]!='*') {
                mdwarn(m, srcline(src, pos), "only '*' bullets are supported");
            }
            // parse_first_list_line
            ind = e - pos;
            iz  eol   = lineend(s, e);
            Str rest  = slice(s, e, eol+1);
            b32 empty = 1;
            for (iz i = 0; i < rest.len; i++) {
                empty &= whitespace(rest.data[i]);
            }
            if (empty) {
                ind = 4;
            } else {
                iz lead = 0;
                for (iz i = 0; i<rest.len && blank(rest.data[i]); i++) {
                    if (rest.data[i] == ' ') {
                        lead++;
                    } else {
                        iz tabs = 0;
                        for (; at(rest, i+tabs)=='\t'; tabs++) {}
                        lead += 4 - (lead+ind)%4 + (tabs-1)*4;
                        i += tabs - 1;
                    }
                }
                ind += lead;
            }
            Str content = trimleft(rest);
            if (startswith(content, "{:")) {
                mdwarn(m, srcline(src, pos), "IAL syntax is not supported");
            }
            MdPiece p = {PIECE_ITEM, 0, content, pos};
            pieces    = push(m->a, pieces, p);
            maxspaces = ind<4 ? ind-1 : 3;
            nested    = anylist(content, 0);
            lastblank = 0;
            src->pos  = eol + 1;
        } else if (contentline(s, pos, ind) ||
                   (!lastblank && lazyline(s, pos, ind<3 ? ind : 3, 0))) {
            // Leading tabs become four spaces each, then remove ind spaces
            iz  eol   = lineend(s, pos);
            iz  tabs  = 0;
            for (; s.data[pos+tabs]=='\t'; tabs++) {}
            iz  sp    = 0;
            for (; s.data[pos+tabs+sp]==' '; sp++) {}
            iz  lead  = tabs*4 + sp;
            b32 found = lead >= ind;
            MdPiece p = {PIECE_LINE, 0, slice(s, pos+tabs+sp, eol+1), pos};
            p.spaces  = found ? lead-ind : lead;
            if (!found) {
                p.text   = slice(s, pos+tabs, eol+1);
                p.spaces = tabs*4;
            }
            // Does the line, as modified, start a list?
            iz  lsp  = p.spaces;
            for (iz i = 0; at(p.text, i)==' '; i++) lsp++;
            b32 islist = lsp<=3 && anylist(p.text, 0);
            if (!nested && found && islist) {
                MdPiece c = {PIECE_CHUNK, 0, {}, pos};
                pieces = push(m->a, pieces, c);
                nested = 1;
            } else if (nested && !found && islist) {
                p.spaces += ind + 4;
            }
            pieces    = push(m->a, pieces, p);
            lastblank = 0;
            src->pos  = eol + 1;
        } else if ((e = blankend(s, pos)) >= 0) {
            MdPiece p = {PIECE_LINE, 0, slice(s, pos, e), pos};
            pieces    = push(m->a, pieces, p);
            nested    = 1;
            lastblank = 1;
            src->pos  = e;
        } else {
            break;
        }
    }

    // Parse each item's chunks as blocks
    i32 nitems = 0;
    for (iz i = 0; i < pieces.len;) {
        MdBlock *li = addblock(m, list, B_LI, src, pieces[i].off);
        nitems++;
        do {
            iz start = i;
            iz len   = pieces[i].text.len;
            for (i++; i<pieces.len && pieces[i].kind==PIECE_LINE; i++) {
                len += pieces[i].spaces + pieces[i].text.len;
            }
            Str chunk = {allocbytes(m->a, len), 0};
            for (iz k = start; k < i; k++) {
                for (iz n = 0; n < pieces[k].spaces; n++) {
                    chunk.data[chunk.len++] = ' ';
                }
                copybytes(chunk.data+chunk.len, pieces[k].text.data, pieces[k].text.len);
                chunk.len += pieces[k].text.len;
            }
            parseblocks(m, li, newsrc(m, chunk, src, pieces[start].off));
        } while (i<pieces.len && pieces[i].kind==PIECE_CHUNK);
    }

    // Tight or loose (list.rb:125-141)
    MdBlock *last = 0;
    b32 anyloose = 0;  // an earlier item not starting with a tight paragraph
    for (MdBlock *li = list->child; li; li = li->next) {
        MdBlock *first = li->child;
        if (!first) {
            anyloose = 1;
            continue;
        }
        MdBlock *second = first->next;
        b32 islast = !li->next;
        if (first->type==B_P &&
                (!second || second->type!=B_BLANK || (islast && !second->next && !eob)) &&
                (!islast || nitems==1 || anyloose)) {
            if (second && second->type!=B_BLANK) {
                first->text = concat(m->a, first->text, "\n");
            }
            first->transparent = 1;
        }
        Str t = trimleft(first->text);
        if (first->type==B_P && (startsat(t, 0, "[ ]") || startsfold(t, 0, "[x]")) &&
                whitespace(at(t, 3))) {
            mdwarn(m, srcline(first->src, first->off), "task lists are not supported");
        }
        if (!islast) {
            anyloose |= first->type!=B_P || first->transparent;
        }
        last = 0;
        if (li->tail->type == B_BLANK) {
            last = li->tail;
            if (li->child == last) {
                li->child = li->tail = 0;
            } else {
                MdBlock *prev = li->child;
                for (; prev->next != last; prev = prev->next) {}
                prev->next = 0;
                li->tail   = prev;
            }
        }
    }
    if (last && !eob) {
        last->src = src;
        last->off = src->pos;
        tree->tail->next = last;
        tree->tail = last;
    }
    return 1;
}

// Emulate kramdown parsing raw block HTML and serializing it again.
struct MdRaw {
    Str  s;
    iz   pos;
    Buf *out;
    b32  eof;
    b32  mdattr;  // saw a markdown attribute
};

static void rawelement(MdRaw *r, HtmlTag t, b32 top);

static void rawtext(MdRaw *r, iz end)
{
    printesc(r->out, slice(r->s, r->pos, end), 0);
    r->pos = end;
}

// parse_raw_html: children until the element's own close tag
static void rawchildren(MdRaw *r, Str name, b32 known)
{
    Str s = r->s;
    for (;;) {
        // HTML_RAW_START: <(UNAME|/|!--|\?|!\[CDATA\[)
        iz i = r->pos;
        for (; i < s.len; i++) {
            if (s.data[i]!='<') continue;
            u8 c = at(s, i+1);
            if (c=='/' || c=='?' || startsat(s, i+1, "!--") ||
                    startsat(s, i+1, "![CDATA[") || xmlname(s, i+1)>=0) {
                break;
            }
        }
        rawtext(r, i);
        if (i >= s.len) {
            r->eof = 1;
            return;
        }

        iz e;
        HtmlTag t;
        if ((e = delimited(s, i, "<!--", "-->")) >= 0 ||
                (e = delimited(s, i, "<?", "?>")) >= 0) {
            print(r->out, slice(s, i, e));
            r->pos = e;
        } else if ((e = delimited(s, i, "<![CDATA[", "]]>")) >= 0) {
            printesc(r->out, slice(s, i+9, e-3), 0);
            r->pos = e;
        } else if ((t = matchtag(s, i)).end >= 0) {
            r->pos = t.end;
            rawelement(r, t, 0);
            if (r->eof) return;
        } else if ((t = matchclose(s, i)).end >= 0) {
            r->pos = t.end;
            if (known ? equalfold(t.name, name) : t.name==name) {
                return;
            }
            printesc(r->out, slice(s, i, t.end), 0);
        } else {
            rawtext(r, i+1);
        }
    }
}

static void rawelement(MdRaw *r, HtmlTag t, b32 top)
{
    Buf *b      = r->out;
    i32  fl     = htmlflags(t.name);
    b32  known  = fl != 0;
    b32  closed = t.selfclose || (fl & H_VOID);
    r->mdattr |= search(t.attrs, 0, "markdown") >= 0;
    putbyte(b, '<');
    printname(b, t.name, known);
    printattrs(b, t.attrs, known, 0);

    if (equalfold(t.name, "script") || equalfold(t.name, "style")) {
        // Raw text up to the close tag, matched without case
        iz i = r->pos;
        for (; i < r->s.len; i++) {
            HtmlTag c = matchclose(r->s, i);
            if (c.end>=0 && equalfold(c.name, t.name)) break;
        }
        putbyte(b, '>');
        print(b, slice(r->s, r->pos, i));
        r->pos = i;
        if (i < r->s.len) {
            r->pos = matchclose(r->s, i).end;
        } else {
            r->eof = 1;
        }
        print(b, "</");
        printname(b, t.name, known);
        putbyte(b, '>');
        return;
    }

    if (closed) {
        print(b, " />");
        return;
    }
    putbyte(b, '>');
    rawchildren(r, t.name, known);
    print(b, "</");
    printname(b, t.name, known);
    putbyte(b, '>');
    if (top && !r->eof) {
        // TRAILING_WHITESPACE
        iz i = r->pos;
        for (; blank(at(r->s, i)); i++) {}
        if (at(r->s, i) == '\n') r->pos = i + 1;
    }
}

static b32 blockhtml(Md *m, MdBlock *tree, MdSrc *src)
{
    Str s   = src->s;
    iz  pos = src->pos;
    iz  i   = skipspaces(s, pos, 3);
    if (at(s, i) != '<') return 0;

    iz e = delimited(s, pos, "<!--", "-->");
    if (e >= 0) {
        MdBlock *b = addblock(m, tree, B_COMMENT, src, pos);
        b->text = slice(s, pos, e);
        iz k = e;
        for (; blank(at(s, k)); k++) {}
        src->pos = at(s, k)=='\n' ? k+1 : e;
        return 1;
    }

    HtmlTag t = matchtag(s, i);
    if (t.end<0 || (htmlflags(t.name) & H_SPAN)) {
        return 0;
    }
    Buf   out(m->a, 256);
    MdRaw r = {s, t.end, &out, 0, 0};
    rawelement(&r, t, 1);
    if (r.mdattr) {
        mdwarn(m, srcline(src, pos), "the markdown attribute is not supported");
    }
    if (r.eof) {
        Str msg = "HTML block runs to the end of the document";
        error(m->log, m->name, srcline(src, pos), msg);
    }
    MdBlock *b = addblock(m, tree, B_HTML, src, pos);
    b->text  = finish(&out);
    src->pos = r.pos;
    return 1;
}

static b32 setext(Md *m, MdBlock *tree, MdSrc *src)
{
    Str s   = src->s;
    iz  pos = src->pos;
    iz  i   = skipspaces(s, pos, 3);
    iz  eol = lineend(s, pos);
    if (i>=eol || blank(s.data[i]) || eol>=s.len || !afterboundary(tree)) {
        return 0;
    }
    u8 c = at(s, eol+1);
    if (c!='-' && c!='=') return 0;
    iz k = eol + 1;
    for (; at(s, k)=='-' || at(s, k)=='='; k++) {}
    iz end = restblank(s, k);
    if (end < 0) return 0;

    mdwarn(m, srcline(src, pos), "setext headers are not supported");
    Str text = trimright(slice(s, i, eol));
    Str id   = headerid(&text);
    MdBlock *b = addblock(m, tree, B_HEADER, src, pos);
    b->level = c=='=' ? 1 : 2;
    b->text  = text;
    b->id    = id;
    src->pos = end;
    return 1;
}

// FOOTNOTE_DEFINITION_START: [^name]: where name is \w[\w-]*
static b32 footnotedef(Str s, iz i)
{
    if (!startsat(s, i, "[^") || !wordbyte(at(s, i+2))) return 0;
    iz j = i + 3;
    for (; wordbyte(at(s, j)) || at(s, j)=='-'; j++) {}
    return startsat(s, j, "]:");
}

static b32 istable(Md *, Str, iz);

// BLOCK_BOUNDARY at a line start: a blank line, EOB, IAL, or the end
static b32 beforeboundary(Str s, iz k)
{
    return k>=s.len || (k==s.len-1 && s.data[k]=='\n') || blankend(s, k)>=0 ||
           eobmarker(s, k)>=0 || ialblock(s, skipspaces(s, k, 3))>=0;
}

// Would kramdown parse a math block (math.rb)? When escaped, it instead
// drops the backslash, which also changes the output.
static b32 mathblock(MdBlock *tree, Str s, iz pos)
{
    iz  i   = skipspaces(s, pos, 3);
    b32 esc = at(s, i) == '\\';
    if (!afterboundary(tree) || !startsat(s, i+esc, "$$")) return 0;
    iz  k   = search(s, i+esc+2, "$$");
    if (k < 0) return 0;
    iz  nl  = restblank(s, k+2);  // (\s*?\n)?
    return esc ? nl>=0 : beforeboundary(s, nl>=0 ? nl : k+2);
}

// Can a definition list start here: after a paragraph, or after a
// paragraph and a single blank line?
static b32 afterpara(MdBlock *tree)
{
    MdBlock *last = tree->tail;
    if (!last) return 0;
    if (last->type == B_P) return 1;
    if (last->type!=B_BLANK || !(last->text=="\n")) return 0;
    MdBlock *prev = 0;
    for (MdBlock *c = tree->child; c != last; c = c->next) prev = c;
    return prev && prev->type==B_P;
}

// Checks for syntax that kramdown supports and this port does not. They
// warn but do not consume, so the text renders as a paragraph instead.
static void unsupported(Md *m, MdBlock *tree, MdSrc *src)
{
    Str s   = src->s;
    iz  pos = src->pos;
    iz  i   = skipspaces(s, pos, 3);
    if (footnotedef(s, i)) {
        mdwarn(m, srcline(src, pos), "footnotes are not supported");
    }
    // In kramdown's order, so that only the first to apply is reported
    if (dlstart(s, pos) && afterpara(tree)) {
        mdwarn(m, srcline(src, pos), "definition lists are not supported");
    } else if (mathblock(tree, s, pos)) {
        mdwarn(m, srcline(src, pos), "math blocks are not supported");
    } else if (afterboundary(tree) && istable(m, s, pos)) {
        mdwarn(m, srcline(src, pos), "tables are not supported");
    }
}

// The end of a link definition after its URL: an optional title, on this
// line after blanks or alone on the next line, then the end of the line.
static iz defrest(Str s, iz k, Str *title)
{
    iz t = k;
    for (; blank(at(s, t)); t++) {}
    if (at(s, t) == '\n') {
        for (t++; blank(at(s, t)); t++) {}
    } else if (t == k) {
        t = -1;
    }
    if (t>=0 && isquote(at(s, t))) {
        iz e = lineend(s, t);
        for (iz c = t+2; c < e; c++) {
            iz r = c + 1;
            for (; blank(at(s, r)); r++) {}
            if (s.data[c]==s.data[t] && at(s, r)=='\n') {
                *title = slice(s, t+1, c);
                return r + 1;
            }
        }
    }
    for (; blank(at(s, k)); k++) {}
    return at(s, k)=='\n' ? k+1 : -1;
}

// Normalize a link label: \s+ to one space, then lowercase
static Str normlabel(Arena *a, Str label)
{
    Buf key(a, 2*label.len);
    for (iz k = 0; k < label.len;) {
        iz  n;
        i32 c = cpat(label, k, &n);
        k += n;
        if (c < 0x80 && whitespace((u8)c)) {
            for (; k<label.len && whitespace(label.data[k]); k++) {}
            c = ' ';
        }
        c = downcase(c);
        putcp(&key, c==0x130 ? 'i' : c);
        if (c == 0x130) putcp(&key, 0x307);
    }
    return finish(&key);
}

// LINK_DEFINITION_START: the URL is the shortest that lets the rest of
// the line (and perhaps the next) match.
static b32 linkdef(Md *m, MdBlock *tree, MdSrc *src)
{
    Str s   = src->s;
    iz  pos = src->pos;
    iz  i   = skipspaces(s, pos, 3);
    iz  j   = i + 1;
    for (; j<s.len && s.data[j]!=']' && s.data[j]!='\n'; j++) {}
    if (at(s, i)!='[' || j==i+1 || at(s, j)!=']' || at(s, j+1)!=':') {
        return 0;
    }
    Str label = slice(s, i+1, j);
    iz  u     = j + 2;
    for (; blank(at(s, u)); u++) {}
    iz  eol   = lineend(s, u);

    Str url   = {};
    Str title = {};
    iz  end   = -1;
    for (iz g = u+1; at(s, u)=='<' && g<eol && end<0; g++) {
        if (s.data[g] == '>') {
            end = defrest(s, g+1, &title);
            url = slice(s, u+1, g);
        }
    }
    for (iz e = u+1; e<=eol && eol<s.len && end<0; e++) {
        if (e<eol && !blank(s.data[e])) continue;
        end = defrest(s, e, &title);
        url = slice(s, u, e);
        for (iz k = 0; end>=0 && k+1<url.len; k++) {
            if (blank(url.data[k]) && isquote(url.data[k+1])) return 0;
        }
    }
    if (end < 0) return 0;

    MdDef *d = upsert(&m->defs, normlabel(m->a, label), m->a);
    d->url   = url;
    d->title = title;
    addblock(m, tree, B_EOB, src, pos);
    src->pos = end;
    return 1;
}

static b32 extension(Md *m, MdBlock *tree, MdSrc *src)
{
    Str s   = src->s;
    iz  pos = src->pos;
    iz  i   = skipspaces(s, pos, 3);
    iz  e   = -1;
    if (startsat(s, i, "*[")) {
        // ABBREV_DEFINITION_START
        iz k = search(slice(s, i, lineend(s, i)), 0, "]:");
        if (k > 2) {
            mdwarn(m, srcline(src, pos), "abbreviations are not supported");
            e = nextline(s, i);
        }
    } else if (startsat(s, i, "{:")) {
        e = ialblock(s, i);
        if (e<0 && (at(s, i+2)==':' || at(s, i+2)=='/') && at(s, lineend(s, i)-1)=='}') {
            e = nextline(s, i);  // an extension tag, roughly
        }
        if (e >= 0) {
            mdwarn(m, srcline(src, pos), "IAL and extension syntax is not supported");
        }
    } else if ((e = eobmarker(s, pos)) >= 0) {
        mdwarn(m, srcline(src, pos), "the end-of-block marker is not supported");
        addblock(m, tree, B_EOB, src, pos)->boundary = 1;
        src->pos = e;
        return 1;
    }
    if (e < 0) return 0;
    addblock(m, tree, B_EOB, src, pos);
    src->pos = e;
    return 1;
}

static b32 paragraph(Md *m, MdBlock *tree, MdSrc *src)
{
    Str s   = src->s;
    iz  pos = src->pos;
    iz  i   = skipspaces(s, pos, 3);
    if (i>=s.len || blank(s.data[i])) return 0;

    iz end = nextline(s, pos);
    for (; !paraend(s, end); end = nextline(s, end)) {}
    Str text = trimright(slice(s, pos, end));

    MdBlock *last = tree->tail;
    if (last && last->type==B_P) {
        // Continues a paragraph that a failed block parser interrupted
        Str joiner = pos>=3 && slice(s, pos-3, pos)=="  \n" ? Str("  \n") : Str("\n");
        // concat() copies only when it cannot extend in place
        last->text = concat(m->a, concat(m->a, last->text, joiner), text);
    } else {
        MdBlock *b = addblock(m, tree, B_P, src, pos);
        b->text = trimleft(text);
    }
    src->pos = end;
    return 1;
}

static void parseblocks(Md *m, MdBlock *tree, MdSrc *src)
{
    Str s    = src->s;
    b32 deep = ++m->depth > MAXDEPTH;
    if (deep && !m->deep) {
        m->deep = 1;
        mdwarn(m, srcline(src, src->pos), "pathological nesting, rendered as text");
    }
    while (src->pos < s.len) {
        if (blankline(m, tree, src))  continue;
        if (indented(m, tree, src))   continue;
        if (fenced(m, tree, src))     continue;
        if (!deep && blockquote(m, tree, src)) continue;
        if (atxheader(m, tree, src))  continue;
        if (hrblock(m, tree, src))    continue;
        if (!deep && list(m, tree, src)) continue;
        if (blockhtml(m, tree, src))  continue;
        if (setext(m, tree, src))     continue;
        unsupported(m, tree, src);
        iz i = skipspaces(s, src->pos, 3);
        if (!footnotedef(s, i) && linkdef(m, tree, src)) continue;
        if (extension(m, tree, src))  continue;
        if (paragraph(m, tree, src))  continue;

        // No parser applies: kramdown keeps the line as raw text, which
        // is contiguous with the raw text before it
        iz end = nextline(s, src->pos);
        if (tree->tail && tree->tail->type==B_TEXT) {
            Str *text = &tree->tail->text;
            assert(text->data+text->len == s.data+src->pos);
            text->len += end - src->pos;
        } else {
            addblock(m, tree, B_TEXT, src, src->pos)->text = slice(s, src->pos, end);
        }
        src->pos = end;
    }
    m->depth--;
}


// Span pass: a flat list of nodes, so that failed parses roll back by
// truncation. Elements are bracketed by open and close nodes.

enum {
    S_TEXT, S_CODE, S_BR, S_CHAR, S_SYM, S_ENTITY, S_COMMENT, S_CDATA,
    S_WARN, S_EM, S_EMEND, S_STRONG, S_STRONGEND, S_A, S_AEND, S_IMG,
    S_DEL, S_DELEND, S_TAG, S_TAGEND,
};

enum {
    LSQUO  = 0x2018, RSQUO = 0x2019, LDQUO = 0x201c, RDQUO = 0x201d,
    HELLIP = 0x2026, MDASH = 0x2014, NDASH = 0x2013,
    LAQUO  = 0x00ab, RAQUO = 0x00bb, NBSP  = 0x00a0,
};

enum { TAG_KNOWN = 1<<0, TAG_VOID = 1<<1, MAILTO = 1<<2 };

struct MdSpan {
    i32 type;
    i32 aux;    // code point, symbol, or TAG_* flags
    Str text;   // text, code, href, src, or tag name
    Str attr;   // title, or tag attributes
    Str alt;
};

enum { STOP_NONE, STOP_EM, STOP_LINK, STOP_TAG };

// One parse_spans call: the element being filled and how it ends
struct MdFrame {
    i32 type;
    iz  first;  // index of the first child node
    i32 nimg;   // image children, for link bracket counting
    b32 raw;    // only HTML tags are recognized
    i32 stop;
    Str delim;  // emphasis delimiter or close tag name
    b32 known;  // close tag matched without case
    i32 count;  // link bracket depth
    b32 table;  // only code spans and HTML tags (table.rb)
};

struct MdSpans {
    Md            *m;
    Arena         *a;     // nodes only
    Arena         *misc;  // everything else
    Str            s;
    iz             pos;
    Slice<MdSpan>  nodes;
    i32            inem;
    i32            instrong;
    i32            inlink;
    i32            depth;
    iz             fuel;  // scanning budget against exponential backtracking
};

static void pushspan(MdSpans *sp, i32 type, Str text = {}, i32 aux = 0)
{
    MdSpan n = {};
    n.type   = type;
    n.text   = text;
    n.aux    = aux;
    sp->nodes = push(sp->a, sp->nodes, n);
}

static void addtext(MdSpans *sp, Str text)
{
    if (!text.len) return;
    Slice<MdSpan> *v = &sp->nodes;
    if (v->len) {
        MdSpan *last = &v->data[v->len-1];
        if (last->type==S_TEXT && last->text.data+last->text.len==text.data) {
            last->text.len += text.len;
            return;
        }
    }
    pushspan(sp, S_TEXT, text);
}

static void getch(MdSpans *sp)
{
    iz n = cplen(sp->s, sp->pos);
    addtext(sp, slice(sp->s, sp->pos, sp->pos+n));
    sp->pos += n;
}

static void spanwarn(MdSpans *sp, Str msg)
{
    MdSpan n = {};
    n.type = S_WARN;
    n.text = msg;
    n.attr = slice(sp->s, sp->pos, sp->pos);  // position, for the line
    sp->nodes = push(sp->a, sp->nodes, n);
}

// The span_start regex union: where the scanner stops for a parser
static b32 spanstart(Str s, iz i)
{
    u8 c = s.data[i], c1 = at(s, i+1);
    switch (c) {
    case '*': case '_': case '`': case '<': case '[': case '$': case '&':
    case '\\': case '"': case '\'':
        return 1;
    case '!': if (c1 == '[') return 1; break;
    case '{': if (c1 == ':') return 1; break;
    case '-': if (c1 == '-') return 1; break;
    case '.': if (c1=='.' && at(s, i+2)=='.') return 1; break;
    case '>': if (c1 == '>') return 1; break;
    case '~': if (c1 == '~') return 1; break;
    case ' ':
        if (c1==' ' && at(s, i+2)=='\n') return 1;
        if ((c1=='<' || c1=='>') && at(s, i+2)==c1) return 1;
        break;
    }
    return isquote(at(s, i + (c<0xc0 ? 1 : cplen(s, i))));  // [^\\]?["']
}

static b32 stopat(MdSpans *sp, MdFrame *f, iz i)
{
    Str s = sp->s;
    switch (f->stop) {
    case STOP_EM:
        return s.data[i]==f->delim.data[0] && startsat(s, i, f->delim);
    case STOP_LINK:
        return s.data[i]==']' || s.data[i]=='[' || (s.data[i]=='!' && at(s, i+1)=='[');
    case STOP_TAG:
        if (at(s, i)!='<' || at(s, i+1)!='/') return 0;
        if (f->known ? !startsfold(s, i+2, f->delim) : !startsat(s, i+2, f->delim)) {
            return 0;
        }
        return at(s, skipws(s, i+2+f->delim.len)) == '>';
    }
    return 0;
}

// The condition kramdown attaches to a stop regex match
static b32 stopcond(MdSpans *sp, MdFrame *f)
{
    Str s = sp->s;
    iz  p = sp->pos;
    switch (f->stop) {
    case STOP_EM: {
        u8 d = f->delim.data[0];
        iz n = f->delim.len;
        if (p>0 && whitespace(s.data[p-1])) return 0;
        if (f->type==S_EM && at(s, p+1)==d && at(s, p+2)!=d) return 0;
        if (d=='_' && p+n<s.len) {
            iz  k;
            i32 c = cpat(s, p+n, &k);
            if (unialnum(c)) return 0;
        }
        return sp->nodes.len > f->first;
    }
    case STOP_LINK:
        f->count += s.data[p]==']' ? -1 : 1;
        return f->count - f->nimg == 0;
    case STOP_TAG:
        return 1;
    }
    return 0;
}

static b32 parsespans(MdSpans *sp, MdFrame *f);

static void emphasis(MdSpans *sp, MdFrame *f)
{
    Str s      = sp->s;
    iz  saved  = sp->pos;
    u8  type   = s.data[saved];
    iz  n      = at(s, saved+1)==type ? 2 : 1;
    i32 elem   = n==2 ? S_STRONG : S_EM;
    Str result = slice(s, saved, saved+n);
    sp->pos += n;

    b32 literal = sp->pos<s.len && whitespace(s.data[sp->pos]);
    literal |= elem==S_EM ? sp->inem>0 : sp->instrong>0;
    if (type == '_') {
        // pre_match =~ /[[:alpha:]]-?[[:alpha:]]*_*\z/
        iz j = saved;
        for (; j>0 && s.data[j-1]=='_'; j--) {}
        iz  k;
        i32 c = utf8prev(s, j, &k);
        if (c=='-' && k>0) {
            c = utf8prev(s, k, &k);
        }
        literal |= c>=0 && unialpha(c);
    }
    if (literal) {
        addtext(sp, result);
        return;
    }

    iz mark = sp->nodes.len;
    for (i32 attempt = 0; attempt < 2; attempt++) {
        MdFrame g = {};
        g.type  = elem;
        g.stop  = STOP_EM;
        g.delim = elem==S_STRONG ? result : slice(s, saved, saved+1);
        pushspan(sp, elem, {}, type);
        g.first = sp->nodes.len;
        i32 *depth = elem==S_EM ? &sp->inem : &sp->instrong;
        ++*depth;
        b32 found = parsespans(sp, &g);
        --*depth;
        if (found) {
            sp->pos += g.delim.len;
            pushspan(sp, elem==S_EM ? S_EMEND : S_STRONGEND);
            return;
        }
        sp->nodes.len = mark;
        if (elem!=S_STRONG || f->type==S_EM) break;
        // Retry the second delimiter character as emphasis
        sp->pos = saved + 1;
        elem    = S_EM;
    }
    sp->pos = saved + n;
    addtext(sp, result);
}

static void codespan(MdSpans *sp)
{
    Str s     = sp->s;
    iz  start = sp->pos;
    iz  n     = 0;
    for (; at(s, start+n)=='`'; n++) {}
    Str delim = slice(s, start, start+n);
    sp->pos = start + n;

    if (n==1 && (!start || whitespace(s.data[start-1])) &&
            sp->pos<s.len && whitespace(s.data[sp->pos])) {
        addtext(sp, delim);
        return;
    }
    iz e = search(s, sp->pos, delim);
    sp->fuel -= (e<0 ? s.len : e) - sp->pos;
    if (e < 0) {
        addtext(sp, delim);
        return;
    }
    Str code = slice(s, sp->pos, e);
    sp->pos = e + n;
    if (n > 1) {
        if (code.len && code.data[0]==' ') code = cuthead(code, 1);
        if (code.len && code.data[code.len-1]==' ') code = cuttail(code, 1);
    }
    pushspan(sp, S_CODE, code);
}

static b32 autolink(MdSpans *sp)
{
    static Str schemes[] = {"mailto", "https", "http", "ftps", "ftp"};
    Str s = sp->s;
    iz  p = sp->pos;
    for (iz i = 0; i < countof(schemes); i++) {
        iz c = p + 1 + schemes[i].len;
        if (!startsat(s, p+1, schemes[i]) || at(s, c)!=':') continue;
        // .+? then >
        if (c+1>=s.len || s.data[c+1]=='\n') return 0;
        iz j = c + 2;
        for (; j<s.len && s.data[j]!='>' && s.data[j]!='\n'; j++) {}
        sp->fuel -= j - p;
        if (at(s, j) != '>') return 0;
        Str url  = slice(s, p+1, j);
        Str text = startswith(url, "mailto:") ? cuthead(url, 7) : url;
        pushspan(sp, S_A, url);
        addtext(sp, text);
        pushspan(sp, S_AEND);
        sp->pos = j + 1;
        return 1;
    }

    // Email: [[:alnum:]-_.]+?@[[:alnum:]-_.]+?>
    iz j = p + 1;
    for (i32 part = 0; part < 2; part++) {
        iz start = j;
        for (; j < s.len; j += cplen(s, j)) {
            iz  n;
            i32 c = cpat(s, j, &n);
            if (c!='-' && c!='_' && c!='.' && !unialnum(c)) break;
        }
        sp->fuel -= j - start;
        if (j==start || at(s, j)!=(part ? '>' : '@')) return 0;
        j++;
    }
    Str addr = slice(s, p+1, j-1);
    pushspan(sp, S_A, addr, MAILTO);
    addtext(sp, addr);
    pushspan(sp, S_AEND);
    sp->pos = j;
    return 1;
}

// HTML_SPAN_START: <(UNAME|!--|/|!\[CDATA\[)
static b32 spanhtmlstart(Str s, iz i)
{
    return at(s, i)=='<' && (at(s, i+1)=='/' || startsat(s, i+1, "!--") ||
           startsat(s, i+1, "![CDATA[") || xmlname(s, i+1)>=0);
}

static void spanhtml(MdSpans *sp, MdFrame *f)
{
    Str s = sp->s;
    iz  p = sp->pos;
    iz  e;
    if (startsat(s, p, "<!--") || startsat(s, p, "<![CDATA[")) {
        sp->fuel -= s.len - p;  // an upper bound on the search
    }
    if ((e = delimited(s, p, "<!--", "-->")) >= 0) {
        pushspan(sp, S_COMMENT, slice(s, p, e));
        sp->pos = e;
        return;
    }
    if ((e = delimited(s, p, "<![CDATA[", "]]>")) >= 0) {
        pushspan(sp, S_CDATA, slice(s, p+9, e-3));
        sp->pos = e;
        return;
    }
    HtmlTag t = matchclose(s, p);
    if (t.end >= 0) {
        addtext(sp, slice(s, p, t.end));
        sp->pos = t.end;
        return;
    }
    t = matchtag(s, p);
    if (t.end < 0) {
        getch(sp);
        return;
    }
    i32 fl = htmlflags(t.name);
    sp->pos = t.end;
    if (fl & H_BLOCK) {
        addtext(sp, slice(s, p, t.end));
        return;
    }
    if (search(t.attrs, 0, "markdown") >= 0) {
        spanwarn(sp, "the markdown attribute is not supported");
    }

    i32 flags = (fl ? TAG_KNOWN : 0) | (fl&H_VOID ? TAG_VOID : 0);
    pushspan(sp, S_TAG, t.name, flags);
    sp->nodes.data[sp->nodes.len-1].attr = t.attrs;
    if (fl & H_VOID) {
        return;
    }
    if (!t.selfclose) {
        MdFrame g = {};
        g.type  = S_TAG;
        g.first = sp->nodes.len;
        g.raw   = f->raw || !(fl & H_PARSE);
        g.stop  = STOP_TAG;
        g.delim = t.name;
        g.known = fl != 0;
        if (parsespans(sp, &g)) {
            sp->pos = matchclose(s, sp->pos).end;
        } else {
            spanwarn(sp, "HTML element has no end tag");
            addtext(sp, cuthead(s, sp->pos));
            sp->pos = s.len;
        }
    }
    pushspan(sp, S_TAGEND, t.name, flags);
}

// Remove backslashes from kramdown's (non-GFM) ESCAPED_CHARS
static Str unescape(Arena *a, Str s)
{
    if (search(s, 0, "\\") < 0) return s;
    Str r = {allocbytes(a, s.len), 0};
    for (iz i = 0; i < s.len; i++) {
        u8 c = s.data[i];
        if (c=='\\' && i+1<s.len && inset("\\.*_+`<>()[]{}#!:|\"'$=-", s.data[i+1])) {
            c = s.data[++i];
        }
        r.data[r.len++] = c;
    }
    return r;
}

// Complete a link or image whose open node is at mark (add_link)
static void addlink(MdSpans *sp, MdFrame *f, iz mark, Str url, Str title, Str alt)
{
    MdSpan *n = sp->nodes.data + mark;
    n->text = url;
    n->attr = title;
    if (n->type == S_IMG) {
        n->alt = alt;
        sp->nodes.len = mark + 1;
        f->nimg++;
    } else {
        pushspan(sp, S_AEND);
    }
}

static void parselink(MdSpans *sp, MdFrame *f)
{
    Str s      = sp->s;
    iz  start  = sp->pos;
    b32 img    = s.data[start] == '!';
    Str result = slice(s, start, start+1+img);
    sp->pos += result.len;
    iz  curpos = sp->pos;
    if (!img && sp->inlink) {
        addtext(sp, result);
        return;
    }

    // Link text, counting brackets
    iz mark = sp->nodes.len;
    pushspan(sp, img ? S_IMG : S_A);
    MdFrame g = {};
    g.type  = img ? S_IMG : S_A;
    g.first = sp->nodes.len;
    g.stop  = STOP_LINK;
    g.count = 1;
    sp->inlink++;
    b32 found = parsespans(sp, &g);
    sp->inlink--;
    Str alt       = {};
    b32 undefined = 0;
    if (found) {
        alt = unescape(sp->misc, slice(s, curpos, sp->pos));
        sp->pos++;  // ]
    }

    // Reference: [text][id], [text][], or [text] not followed by (
    iz  i    = skipws(s, sp->pos);
    iz  j    = i + 1;
    for (; at(s, i)=='[' && j<s.len && s.data[j]!=']'; j++) {}
    b32 isid = at(s, i)=='[' && j<s.len;
    sp->fuel -= j - sp->pos;
    if (found && (isid || at(s, sp->pos)!='(')) {
        Str id = alt;
        if (isid) {
            sp->pos = j + 1;
            id = j>i+1 ? slice(s, i+1, j) : alt;
        }
        MdDef *d = upsert(&sp->m->defs, normlabel(sp->misc, id), 0);
        if (d) {
            addlink(sp, f, mark, d->url, d->title, alt);
            return;
        }
        undefined = isid;
        found = 0;
    }

    Str url = {};
    if (found) {
        // Inline: (<url>) or balanced parentheses, then perhaps a title
        iz p = sp->pos;
        iz k = p + 2;
        for (; k<s.len && s.data[k]!='>' && s.data[k]!='\n'; k++) {}
        if (at(s, p+1)=='<' && at(s, k)=='>') {
            url = slice(s, p+2, k);
            sp->pos = k + 1;
            if (at(s, sp->pos) == ')') {
                sp->pos++;
                addlink(sp, f, mark, url, {}, alt);
                return;
            }
        } else {
            iz  last = p;
            i32 nr   = 0;
            for (k = p;;) {
                for (; k<s.len && s.data[k]!='(' && s.data[k]!=')' &&
                       !(whitespace(s.data[k]) && isquote(at(s, k+1))); k++) {}
                if (k >= s.len) break;
                u8 c = s.data[k];
                last = ++k;
                if (c == ')') {
                    if (!--nr) break;
                } else if (c == '(') {
                    nr++;
                } else {
                    break;
                }
            }
            sp->fuel -= k - p;
            url = trim(slice(s, p+1, last-1>p+1 ? last-1 : p+1));
            sp->pos = last;
            if (!nr) {
                addlink(sp, f, mark, url, {}, alt);
                return;
            }
        }

        // LINK_INLINE_TITLE_RE: \s*?(["'])(.+?)\1\s*?\)
        iz t = skipws(s, sp->pos);
        for (iz c = t+2; isquote(at(s, t)) && c<s.len; c++) {
            if (s.data[c] != s.data[t]) continue;
            iz r = skipws(s, c+1);
            if (at(s, r) == ')') {
                sp->pos = r + 1;
                addlink(sp, f, mark, url, slice(s, t+1, c), alt);
                return;
            }
        }
        sp->fuel -= isquote(at(s, t)) ? s.len-t : 0;
    }

    sp->nodes.len = mark;
    sp->pos = curpos;
    if (undefined) {
        spanwarn(sp, "undefined link reference");
    }
    addtext(sp, result);
}

// SQ_PUNCT: ASCII punctuation except &
static b32 sqpunct(u8 c)
{
    return c!='&' && ((c>='!' && c<='/') || (c>=':' && c<='@') ||
                      (c>='[' && c<='`') || (c>='{' && c<='~'));
}

static i32 openquote(u8 q)
{
    return q=='"' ? LDQUO : LSQUO;
}

static i32 closequote(u8 q)
{
    return q=='"' ? RDQUO : RSQUO;
}

// SQ_RULES from smart_quotes.rb, tried in order at the scanner position
static void smartquote(MdSpans *sp)
{
    Str s  = sp->s;
    iz  p  = sp->pos;
    u8  c  = s.data[p];
    u8  c1 = at(s, p+1);
    u8  c2 = at(s, p+2);

    // ("|')(?=[_*]{1,2}\S)
    if (isquote(c) && (c1=='_' || c1=='*') && p+2<s.len && !whitespace(c2)) {
        pushspan(sp, S_CHAR, {}, openquote(c));
        sp->pos = p + 1;
        return;
    }
    // ("|')(?=SQ_PUNCT(?!\.\.)\B)
    if (isquote(c) && sqpunct(c1) && !(c2=='.' && at(s, p+3)=='.') &&
            wordbyte(c1)==wordat(s, p+2)) {
        pushspan(sp, S_CHAR, {}, closequote(c));
        sp->pos = p + 1;
        return;
    }
    // (\s?)"'(?=\w), (\s?)'"(?=\w), (\s?)'(?=\d\ds)
    iz k = p + whitespace(c);
    u8 a = at(s, k), b = at(s, k+1);
    if ((a=='"' && b=='\'') || (a=='\'' && b=='"')) {
        if (wordbyte(at(s, k+2))) {
            addtext(sp, slice(s, p, k));
            pushspan(sp, S_CHAR, {}, openquote(a));
            pushspan(sp, S_CHAR, {}, openquote(b));
            sp->pos = k + 2;
            return;
        }
    }
    if (a=='\'' && digit(b) && digit(at(s, k+2)) && at(s, k+3)=='s') {
        addtext(sp, slice(s, p, k));
        pushspan(sp, S_CHAR, {}, RSQUO);
        sp->pos = k + 1;
        return;
    }
    // (\s)('|")(?=\w)
    if (whitespace(c) && isquote(c1) && wordbyte(c2)) {
        addtext(sp, slice(s, p, p+1));
        pushspan(sp, S_CHAR, {}, openquote(c1));
        sp->pos = p + 2;
        return;
    }
    // (SQ_CLOSE)('|")
    iz  n;
    i32 cp = cpat(s, p, &n);
    u8  q  = at(s, p+n);
    if (isquote(q) && cp!=' ' && cp!='\\' && cp!='\t' && cp!='\r' &&
            cp!='\n' && cp!='[' && cp!='{' && cp!='(' && cp!='-') {
        addtext(sp, slice(s, p, p+n));
        pushspan(sp, S_CHAR, {}, closequote(q));
        sp->pos = p + n + 1;
        return;
    }
    // ("|')(?=\s|s\b|$)
    if (isquote(c) && (p+1==s.len || whitespace(c1) || (c1=='s' && !wordat(s, p+2)))) {
        pushspan(sp, S_CHAR, {}, closequote(c));
        sp->pos = p + 1;
        return;
    }
    // (.?)' then (.?)"
    for (i32 r = 0; r < 2; r++) {
        u8 quote = r ? '"' : '\'';
        if (q == quote) {
            addtext(sp, slice(s, p, p+n));
            pushspan(sp, S_CHAR, {}, openquote(quote));
            sp->pos = p + n + 1;
            return;
        }
        if (c == quote) {
            pushspan(sp, S_CHAR, {}, openquote(quote));
            sp->pos = p + 1;
            return;
        }
    }
    getch(sp);  // unreachable
}

// HTML_ENTITY_RE, output as a character except for markup characters
static b32 entity(MdSpans *sp)
{
    Str s = sp->s;
    iz  p = sp->pos;
    iz  j = p + 1;
    i32 cp = -1;
    Str name = {};
    if (wordbyte(at(s, j)) || at(s, j)==':') {
        for (j++; namebyte(at(s, j)); j++) {}
        name = slice(s, p+1, j);
    } else if (at(s, j)=='#' && digit(at(s, j+1))) {
        cp = 0;
        for (j++; digit(at(s, j)); j++) {
            cp = cp>0x10ffff ? cp : cp*10 + (s.data[j] - '0');
        }
    } else if (at(s, j)=='#' && at(s, j+1)=='x' && hexdigit(at(s, j+2))) {
        cp = 0;
        for (j += 2; hexdigit(at(s, j)); j++) {
            u8 c = s.data[j];
            i32 v = digit(c) ? c-'0' : (lowercase(c)-'a'+10);
            cp = cp>0x10ffff ? cp : cp*16 + v;
        }
    } else {
        return 0;
    }
    if (at(s, j) != ';') return 0;
    Str orig = slice(s, p, j+1);
    sp->pos = j + 1;

    if (name.data) {
        // Without kramdown's table, names pass through as written.
        cp = name=="quot" ? '"' : name=="amp" ? '&' : name=="lt" ? '<' :
             name=="gt" ? '>' : -1;
    }
    if (cp=='"' || (cp>0 && cp<=0x10ffff && !(cp>=0xd800 && cp<0xe000) &&
            cp!='<' && cp!='>' && cp!='&')) {
        pushspan(sp, S_ENTITY, {}, cp);
    } else {
        pushspan(sp, S_ENTITY, orig, cp);
    }
    return 1;
}

static b32 typographic(MdSpans *sp)
{
    static struct { Str match; i32 type; i32 aux; } syms[] = {
        {"---", S_CHAR, MDASH}, {"--", S_CHAR, NDASH}, {"...", S_CHAR, HELLIP},
        {"\\<<", S_ENTITY, '<'}, {"\\>>", S_ENTITY, '>'},
        {"<< ", S_SYM, LAQUO}, {" >>", S_SYM, RAQUO},
        {"<<", S_CHAR, LAQUO}, {">>", S_CHAR, RAQUO},
    };
    for (iz i = 0; i < countof(syms); i++) {
        if (!startsat(sp->s, sp->pos, syms[i].match)) continue;
        sp->pos += syms[i].match.len;
        if (syms[i].type == S_ENTITY) {
            Str e = syms[i].aux=='<' ? Str("&lt;") : Str("&gt;");
            pushspan(sp, S_ENTITY, e, syms[i].aux);
            pushspan(sp, S_ENTITY, e, syms[i].aux);
        } else {
            pushspan(sp, syms[i].type, {}, syms[i].aux);
        }
        return 1;
    }
    return 0;
}

static b32 strikethrough(MdSpans *sp)
{
    // ~~(?!\s|~).*?[^\s~]~~
    Str s = sp->s;
    iz  p = sp->pos;
    u8  c = at(s, p+2);
    if (!startsat(s, p, "~~") || p+2>=s.len || whitespace(c) || c=='~') {
        return 0;
    }
    iz k = p + 2;
    for (; k+2 < s.len; k++) {
        c = s.data[k];
        if (!whitespace(c) && c!='~' && s.data[k+1]=='~' && s.data[k+2]=='~') break;
    }
    sp->fuel -= k - p;
    if (k+2 >= s.len) return 0;

    // The interior gets a fresh scanner and environment
    MdSpans save = *sp;
    pushspan(sp, S_DEL);
    sp->s        = slice(s, p+2, k+1);
    sp->pos      = 0;
    sp->inem     = sp->instrong = sp->inlink = 0;
    MdFrame g = {};
    g.type  = S_DEL;
    g.first = sp->nodes.len;
    parsespans(sp, &g);
    sp->s        = save.s;
    sp->pos      = k + 3;
    sp->inem     = save.inem;
    sp->instrong = save.instrong;
    sp->inlink   = save.inlink;
    pushspan(sp, S_DELEND);
    return 1;
}

// Span extensions and IALs: {:...}
static void spanext(MdSpans *sp, MdFrame *f)
{
    Str s = sp->s;
    iz  p = sp->pos;
    iz  j = p + 2;
    for (; j<s.len && s.data[j]!='}'; j++) {
        j += s.data[j]=='\\' && at(s, j+1)=='}';
    }
    sp->fuel -= j - p;
    if (j<s.len && j>p+2) {
        MdSpan *last = sp->nodes.len>f->first ? &sp->nodes.data[sp->nodes.len-1] : 0;
        b32     ial  = last && last->type!=S_TEXT && at(s, p+2)!=':' && at(s, p+2)!='/';
        spanwarn(sp, "IAL and extension syntax is not supported");
        if (ial) {
            sp->pos = j + 1;  // kramdown applies the attributes
            return;
        }
    }
    getch(sp);
}

static b32 tryparsers(MdSpans *sp, MdFrame *f)
{
    Str s  = sp->s;
    iz  p  = sp->pos;
    u8  c  = s.data[p];
    u8  c1 = at(s, p+1);
    if (f->raw || f->table) {
        if (f->table && c=='`') {
            codespan(sp);
            return 1;
        }
        if (spanhtmlstart(s, p)) {
            spanhtml(sp, f);
            return 1;
        }
        return 0;
    }
    if (c=='*' || c=='_') {
        emphasis(sp, f);
        return 1;
    }
    if (c == '`') {
        codespan(sp);
        return 1;
    }
    if (c=='<' && autolink(sp)) return 1;
    if (spanhtmlstart(s, p)) {
        spanhtml(sp, f);
        return 1;
    }
    if (c=='[' && c1=='^') {
        iz j = p + 2;
        for (; wordbyte(at(s, j)) || (j>p+2 && at(s, j)=='-'); j++) {}
        if (j>p+2 && at(s, j)==']') {
            spanwarn(sp, "footnotes are not supported");
            addtext(sp, slice(s, p, j+1));
            sp->pos = j + 1;
            return 1;
        }
    }
    // LINK_START: !?\[(?=[^^])
    if ((c=='[' && p+1<s.len && c1!='^') ||
            (c=='!' && c1=='[' && p+2<s.len && s.data[p+2]!='^')) {
        parselink(sp, f);
        return 1;
    }
    if (isquote(c) || (c!='\\' && isquote(at(s, p+cplen(s, p))))) {
        smartquote(sp);
        return 1;
    }
    if (c=='$' && c1=='$') {
        iz e = search(s, p+2, "$$");
        sp->fuel -= (e<0 ? s.len : e) - p;
        if (e >= 0) {
            spanwarn(sp, "math is not supported");
            addtext(sp, slice(s, p, e+2));
            sp->pos = e + 2;
            return 1;
        }
    }
    if (c=='{' && c1==':') {
        spanext(sp, f);
        return 1;
    }
    if (c=='&' && entity(sp)) return 1;
    if (typographic(sp)) return 1;
    if (((c==' ' && c1==' ') || (c=='\\' && c1=='\\')) && at(s, p+2)=='\n') {
        pushspan(sp, S_BR);
        sp->pos += 2;
        return 1;
    }
    if (c=='\\' && inset("\\.*_+`<>()[]{}#!:|\"'$=-~", c1) && p+1<s.len) {
        addtext(sp, slice(s, p+1, p+2));  // ESCAPED_CHARS_GFM
        sp->pos += 2;
        return 1;
    }
    if (c=='~' && strikethrough(sp)) return 1;
    return 0;
}

// parse_spans: returns true if the stop was found
static b32 parsespans(MdSpans *sp, MdFrame *f)
{
    b32 found = 0;
    if (++sp->depth > MAXDEPTH) {
        sp->fuel = -1;  // the rest is text, with a warning
    }
    for (;;) {
        Str s = sp->s;
        if (sp->pos>=s.len || (sp->fuel<0 && f->stop)) break;

        // scan_until: the leftmost stop or span start
        iz i = sp->pos;
        for (; i < s.len; i++) {
            u8 c = s.data[i];
            if (c>=0x80 && c<0xc0) continue;  // inside a code point
            if (f->stop && stopat(sp, f, i)) break;
            if (f->raw ? c=='<' : f->table ? c=='<' || c=='`' : spanstart(s, i)) break;
        }
        if (i >= s.len) {
            if (!f->stop) {
                addtext(sp, cuthead(s, sp->pos));
                sp->pos = s.len;
            }
            break;
        }
        sp->fuel -= i - sp->pos + 1;
        addtext(sp, slice(s, sp->pos, i));
        sp->pos = i;
        if (f->stop && stopat(sp, f, i) && stopcond(sp, f)) {
            found = 1;
            break;
        }
        if (sp->fuel<0 || !tryparsers(sp, f)) {
            getch(sp);
        }
    }
    sp->depth--;
    return found;
}


// Table detection (table.rb), only to warn: tables are not supported

// TABLE_PIPE_CHECK on one line: a leading pipe, or one after a character
// other than a backslash
static b32 pipecheck(Str line)
{
    if (at(line, 0) == '|') return 1;
    for (iz j = 1; j < line.len; j++) {
        if (line.data[j]=='|' && line.data[j-1]!='\\') return 1;
    }
    return 0;
}

// TABLE_SEP_LINE (c is '-') or TABLE_FSEP_LINE (c is '=')
static b32 sepline(Str line, u8 c)
{
    b32 found = 0;
    for (iz j = 0; j < line.len; j++) {
        u8 d = line.data[j];
        found |= d == c;
        if (d!=c && d!='+' && d!='|' && d!=':' && !blank(d)) return 0;
    }
    return found;
}

// Ruby's str.split("\n"), which drops trailing empty pieces
struct MdLines {
    iz  n;
    Str first;
    Str last;
};

static MdLines splitlines(Str v)
{
    MdLines r = {};
    for (; v.len && v.data[v.len-1]=='\n'; v.len--) {}
    if (!v.len) return r;
    r.n = 1;
    iz firstnl = -1, lastnl = -1;
    for (iz i = 0; i < v.len; i++) {
        if (v.data[i] != '\n') continue;
        r.n++;
        firstnl = firstnl<0 ? i : firstnl;
        lastnl  = i;
    }
    r.first = firstnl<0 ? v : takehead(v, firstnl);
    r.last  = cuthead(v, lastnl+1);
    return r;
}

// The check that every line has a pipe outside of code spans and HTML
// elements, one child of the parsed table text at a time.
struct MdPipes {
    b32 pipe;
    b32 done;
};

static void pipesegment(MdPipes *p, Str value, b32 code)
{
    if (p->done) return;
    MdLines l = splitlines(value);
    if (code) {
        if (l.n>2 || (l.n==2 && !p->pipe)) {
            p->done = 1;
        } else if (l.n == 2) {
            p->pipe = 0;
        }
    } else if (l.n>1 && !p->pipe && !pipecheck(l.first)) {
        p->done = 1;
    } else {
        p->pipe = (l.n>1 ? 0 : p->pipe) || (l.n && pipecheck(l.last));
    }
}

static b32 tablepipes(Md *m, Str text, Arena scratch)
{
    Arena misc = scratch;
    misc.beg = scratch.beg + (scratch.end - scratch.beg)/2;
    scratch.end = misc.beg;

    MdSpans sp = {};
    sp.m    = m;
    sp.a    = &scratch;
    sp.misc = &misc;
    sp.s    = text;
    sp.fuel = 64*text.len + 65536;
    MdFrame root = {};
    root.type  = -1;
    root.table = 1;
    parsespans(&sp, &root);

    // Adjacent text, including CDATA, is one child
    MdPipes p     = {};
    i32     depth = 0;
    Buf     run(&misc, 256);
    for (iz i = 0; i < sp.nodes.len; i++) {
        MdSpan *n = sp.nodes.data + i;
        if (n->type == S_WARN) continue;  // not in kramdown's tree
        if (!depth && (n->type==S_TEXT || n->type==S_CDATA)) {
            print(&run, n->text);
            continue;
        }
        if (run.len) {
            pipesegment(&p, finish(&run), 0);
            run.len = 0;
        }
        switch (n->type) {
        case S_CODE:    if (!depth) pipesegment(&p, n->text, 1); break;
        case S_COMMENT: if (!depth) pipesegment(&p, n->text, 0); break;
        case S_TAG:     depth += !(n->aux & TAG_VOID); break;
        case S_TAGEND:  depth--; break;
        }
    }
    pipesegment(&p, finish(&run), 0);
    return p.pipe;
}

// Would kramdown parse a table at pos? It must also follow a block
// boundary, which the caller checks.
static b32 istable(Md *m, Str s, iz pos)
{
    // TABLE_START: ^ {0,3}(?=\S) then a line with a pipe
    iz i = skipspaces(s, pos, 3);
    if (i>=s.len || whitespace(s.data[i]) || !pipecheck(slice(s, i, lineend(s, i)))) {
        return 0;
    }

    // Rows, until a line without a pipe, then a block boundary
    iz  k      = pos;
    iz  rows   = 0;
    b32 head   = 0;
    b32 footer = 0;
    b32 body   = 0;
    if (sepline(slice(s, k, lineend(s, k)), '-')) {
        k = nextline(s, k);
    }
    for (; k < s.len; k = nextline(s, k)) {
        iz  eol  = lineend(s, k);
        Str line = slice(s, k, eol);
        if (eol>=s.len || !pipecheck(line)) break;
        if (sepline(line, '-')) {
            if (!rows) {
                // ignored
            } else if (!head && !footer) {
                head = 1;
                rows = 0;
            } else if (!footer) {
                body = 1;
                rows = 0;
            }
        } else if (sepline(line, '=')) {
            body  |= rows > 0;
            rows   = 0;
            footer = 1;
        } else {
            rows++;
        }
    }
    body |= rows>0 && !footer;
    return beforeboundary(s, k) && body && tablepipes(m, slice(s, pos, k-1), *m->a);
}


// Rendering

struct MdOut {
    Md    *m;
    Buf   *b;
    Arena  temp;  // per text block
};

// The text of a header's children (gfm.rb update_raw_text)
static void headertext(Buf *b, MdSpan *v, iz n)
{
    for (iz i = 0; i < n; i++) {
        switch (v[i].type) {
        case S_TEXT:
        case S_CODE:  print(b, v[i].text); break;
        case S_CDATA: printhtml(b, v[i].text); break;
        case S_CHAR:  putcp(b, v[i].aux); break;
        case S_ENTITY:
            if (v[i].aux > 0) {
                putcp(b, v[i].aux);
            } else {
                print(b, v[i].text);
            }
            break;
        case S_SYM:
            putcp(b, v[i].aux==LAQUO ? LAQUO : NBSP);
            putcp(b, v[i].aux==LAQUO ? NBSP : RAQUO);
            break;
        }
    }
}

static void headerattr(MdOut *o, MdBlock *h, MdSpan *v, iz n, Arena scratch)
{
    Buf *b = o->b;
    if (h->id.len) {
        print(b, " id=\"");
        printesc(b, h->id, 1);
        putbyte(b, '"');
        return;
    }

    Buf raw(&scratch, 256);
    headertext(&raw, v, n);
    Str text = finish(&raw);
    Buf slug(&scratch, text.len+16);
    for (iz i = 0; i < text.len;) {
        iz  k;
        i32 c = downcase(cpat(text, i, &k));
        i  += k;
        if (c==' ' || c=='\t') {
            putbyte(&slug, '-');
        } else if (c == 0x130) {
            putcp(&slug, 'i');
            putcp(&slug, 0x307);
        } else if (c=='-' || uniword(c)) {
            putcp(&slug, c);
        }
    }
    Str id = finish(&slug);
    i32 *count = upsert(&o->m->ids, id, 0);
    if (!count) {
        count = upsert(&o->m->ids, clone(o->m->a, id), o->m->a);
    }
    i32 dup = (*count)++;
    if (!id.len && !dup) return;
    print(b, " id=\"");
    printesc(b, id, 1);
    if (dup) {
        putbyte(b, '-');
        print(b, (i64)dup);
    }
    putbyte(b, '"');
}

static void spanwarnings(MdOut *o, MdBlock *blk, Str text, MdSpan *v, iz n)
{
    iz  line    = -1;
    u8 *scanned = text.data;  // newlines counted up to here, usually in order
    iz  nls     = 0;
    for (iz i = 0; i < n; i++) {
        Str where = {};
        Str msg   = {};
        if (v[i].type == S_WARN) {
            where = v[i].attr;
            msg   = v[i].text;
        } else if (v[i].type == S_TEXT) {
            Str t = v[i].text;
            iz  k = 0;
            for (; k+1<t.len; k++) {
                if (t.data[k]=='{' && (t.data[k+1]=='{' || t.data[k+1]=='%')) break;
            }
            if (k+1 >= t.len) continue;
            where = cuthead(t, k);
            msg   = "Liquid syntax is not supported";
        } else {
            continue;
        }
        if (line < 0) line = srcline(blk->src, blk->off);
        iz nl = 0;
        if (where.data>=text.data && where.data<=text.data+text.len) {
            if (where.data < scanned) {
                scanned = text.data;
                nls     = 0;
            }
            for (; scanned < where.data; scanned++) nls += *scanned=='\n';
            nl = nls;
        }
        mdwarn(o->m, line+nl, msg);
    }
}

// Render a block's raw text as spans. Text directly inside the block
// turns "\\\n" into a line break (gfm.rb update_elements), but not inside
// headers or HTML elements. With lastbr, a "\\\n" ending the final text
// is dropped, as for the last child of a block element.
static void renderspans(MdOut *o, MdBlock *blk, b32 header, b32 lastbr)
{
    Arena   t = o->temp;
    Arena   misc = t;
    misc.beg = t.beg + (t.end - t.beg)/2;
    t.end    = misc.beg;

    MdSpans sp = {};
    sp.m    = o->m;
    sp.a    = &t;
    sp.misc = &misc;
    sp.s    = blk->text;
    sp.fuel = 64*blk->text.len + 65536;  // real text needs about 1x
    MdFrame root = {};
    root.type = -1;
    parsespans(&sp, &root);
    if (sp.fuel < 0) {
        Str msg = "pathological nesting, rendered as text";
        mdwarn(o->m, srcline(blk->src, blk->off), msg);
    }

    Buf    *b = o->b;
    MdSpan *v = sp.nodes.data;
    iz      n = sp.nodes.len;
    if (header) {
        print(b, "<h");
        print(b, (i64)blk->level);
        headerattr(o, blk, v, n, misc);
        putbyte(b, '>');
    }
    spanwarnings(o, blk, blk->text, v, n);

    i32 html  = 0;  // inside HTML elements
    i32 depth = 0;  // inside any element
    for (iz i = 0; i < n; i++) {
        MdSpan *s = v + i;
        switch (s->type) {
        case S_TEXT: {
            Str text = s->text;
            if (header || html) {
                printesc(b, text, 0);
                break;
            }
            iz last = 0;
            for (iz k = 0; k < text.len; k++) {
                if (text.data[k] != '\\') continue;
                b32 nl = k+1<text.len ? text.data[k+1]=='\n' :
                         i+1<n && v[i+1].type==S_TEXT && at(v[i+1].text, 0)=='\n';
                if (!nl) continue;
                printesc(b, slice(text, last, k), 0);
                last = k + 1;
                // No break for a trailing "\\\n" ending the block
                b32 end = lastbr && !depth && (k+2==text.len ? i+1==n :
                          k+1==text.len && i+2==n && v[i+1].text.len==1);
                if (!end) print(b, "<br />");
            }
            printesc(b, cuthead(text, last), 0);
        } break;
        case S_CODE:
            print(b, "<code>");
            printhtml(b, s->text);
            print(b, "</code>");
            break;
        case S_BR:      print(b, "<br />"); break;
        case S_CHAR:    putcp(b, s->aux); break;
        case S_SYM:
            putcp(b, s->aux==LAQUO ? LAQUO : NBSP);
            putcp(b, s->aux==LAQUO ? NBSP : RAQUO);
            break;
        case S_ENTITY:
            if (s->text.len) {
                print(b, s->text);
            } else {
                putcp(b, s->aux);
            }
            break;
        case S_COMMENT: print(b, s->text); break;
        case S_CDATA:   printhtml(b, s->text); break;
        case S_WARN:    break;
        case S_EM:      depth++; print(b, "<em>"); break;
        case S_EMEND:   depth--; print(b, "</em>"); break;
        case S_STRONG:  depth++; print(b, "<strong>"); break;
        case S_STRONGEND: depth--; print(b, "</strong>"); break;
        case S_A:
            depth++;
            print(b, "<a href=\"");
            if (s->aux & MAILTO) print(b, "mailto:");
            printesc(b, s->text, 1);
            if (s->attr.data) {
                print(b, "\" title=\"");
                printesc(b, s->attr, 1);
            }
            print(b, "\">");
            break;
        case S_AEND:    depth--; print(b, "</a>"); break;
        case S_IMG:
            print(b, "<img src=\"");
            printesc(b, s->text, 1);
            print(b, "\" alt=\"");
            printesc(b, s->alt, 1);
            if (s->attr.data) {
                print(b, "\" title=\"");
                printesc(b, s->attr, 1);
            }
            print(b, "\" />");
            break;
        case S_DEL:     depth++; html++; print(b, "<del>"); break;
        case S_DELEND:  depth--; html--; print(b, "</del>"); break;
        case S_TAG:
            putbyte(b, '<');
            printname(b, s->text, s->aux & TAG_KNOWN);
            printattrs(b, s->attr, s->aux & TAG_KNOWN, 1);
            if (s->aux & TAG_VOID) {
                print(b, " />");
            } else {
                depth++;
                html++;
                putbyte(b, '>');
            }
            break;
        case S_TAGEND:
            depth--;
            html--;
            print(b, "</");
            printname(b, s->text, s->aux & TAG_KNOWN);
            putbyte(b, '>');
            break;
        }
    }

    if (header) {
        print(b, "</h");
        print(b, (i64)blk->level);
        print(b, ">\n");
    }
}

static void renderblock(MdOut *o, MdBlock *blk, iz indent, b32 last);

static b32 lastblock(MdBlock *blk)
{
    MdBlock *c = blk->next;
    for (; c && c->type==B_EOB; c = c->next) {}
    return !c;
}

static void renderchildren(MdOut *o, MdBlock *parent, iz indent)
{
    b32 lastblank = 0;
    for (MdBlock *c = parent->child; c; c = c->next) {
        if (c->type == B_EOB) continue;
        if (c->type==B_BLANK && lastblank) continue;
        lastblank = c->type == B_BLANK;
        renderblock(o, c, indent, lastblock(c));
    }
}

static MdBlock *firstchild(MdBlock *blk)
{
    MdBlock *c = blk->child;
    for (; c && c->type==B_EOB; c = c->next) {}
    return c;
}

// Raw text is spliced into its parent, so whether a trailing hard break
// is dropped depends on its position there: last, and not in the root.
static void renderblock(MdOut *o, MdBlock *blk, iz indent, b32 last)
{
    Buf *b = o->b;
    switch (blk->type) {
    case B_BLANK:
        putbyte(b, '\n');
        break;
    case B_P:
        if (blk->transparent) {
            renderspans(o, blk, 0, 1);
        } else {
            putspaces(b, indent);
            print(b, "<p>");
            renderspans(o, blk, 0, 1);
            print(b, "</p>\n");
        }
        break;
    case B_TEXT:
        renderspans(o, blk, 0, last);
        break;
    case B_HEADER:
        putspaces(b, indent);
        renderspans(o, blk, 1, 0);
        break;
    case B_CODE:
        putspaces(b, indent);
        print(b, "<pre class=\"highlight\"><code>");
        if (blk->lang.len) {
            if (!highlight(b, blk->lang, blk->text, o->temp)) {
                mdwarn(o->m, srcline(blk->src, blk->off), "unknown code language");
            }
        } else {
            printhtml(b, blk->text);
        }
        if (!endswith(blk->text, "\n")) putbyte(b, '\n');
        print(b, "</code></pre>\n");
        break;
    case B_QUOTE:
    case B_UL:
    case B_OL: {
        Str name = blk->type==B_QUOTE ? Str("blockquote") :
                   blk->type==B_UL    ? Str("ul") : Str("ol");
        putspaces(b, indent);
        putbyte(b, '<');
        print(b, name);
        print(b, ">\n");
        renderchildren(o, blk, indent+2);
        putspaces(b, indent);
        print(b, "</");
        print(b, name);
        print(b, ">\n");
    } break;
    case B_LI: {
        MdBlock *first = firstchild(blk);
        b32 tight = !first || (first->type==B_P && first->transparent);
        putspaces(b, indent);
        print(b, "<li>");
        if (!tight) putbyte(b, '\n');
        iz start = b->len;
        renderchildren(o, blk, indent+2);
        if (!tight || (b->len>start && b->data[b->len-1]=='\n')) {
            putspaces(b, indent);
        }
        print(b, "</li>\n");
    } break;
    case B_HR:
        putspaces(b, indent);
        print(b, "<hr />\n");
        break;
    case B_HTML:
    case B_COMMENT:
        putspaces(b, indent);
        print(b, blk->text);
        putbyte(b, '\n');
        break;
    }
}

// Render Markdown source into perm. The name and first line number are
// used for diagnostics, which are appended to log.
static Markdown markdown(Str src, Str name, iz line, Arena *perm, Arena scratch, Log *log)
{
    Markdown r = {};
    Md       m = {};
    m.a    = &scratch;
    m.log  = log;
    m.name = name;

    // Like kramdown: "\r\n" and a lone "\r" are newlines, and the text
    // ends with a newline.
    b32 cr = 0;
    for (iz i = 0; i < src.len; i++) {
        cr |= src.data[i] == '\r';  // no early exit, so it vectorizes
    }
    if (cr || !endswith(src, "\n")) {
        Str t = {allocbytes(&scratch, src.len+1), 0};
        for (iz i = 0; i < src.len; i++) {
            u8 c = src.data[i];
            if (c == '\r') {
                c  = '\n';
                i += at(src, i+1) == '\n';
            }
            t.data[t.len++] = c;
        }
        if (!endswith(t, "\n")) {
            t.data[t.len++] = '\n';
        }
        src = t;
    }
    MdSrc   *root = newsrc(&m, src, 0, 0);
    MdBlock *doc  = alloc<MdBlock>(&scratch);
    root->line0 = line;
    doc->type   = B_ROOT;
    parseblocks(&m, doc, root);

    // Jekyll's excerpt ends at the separator, a top-level <!--more-->
    // or else the first blank line. Take the blocks that start before it.
    iz split = -1;
    for (MdBlock *c = doc->child; c && split<0; c = c->next) {
        split = c->type==B_COMMENT && c->text=="<!--more-->" ? c->off : -1;
    }
    split = split>=0 ? split : search(src, 0, "\n\n");
    split = split>=0 ? split : src.len;

    // Persistent allocations (header ids) below, per-block scratch above
    MdOut o = {};
    o.m    = &m;
    o.temp = scratch;
    o.temp.beg = scratch.beg + (scratch.end - scratch.beg)/2;
    scratch.end = o.temp.beg;

    Buf b(perm, src.len + src.len/2 + 256);
    o.b = &b;
    b32 lastblank = 0;
    iz  mark      = 0;
    for (MdBlock *c = doc->child; c; c = c->next) {
        if (c->type == B_EOB) continue;
        if (c->type==B_BLANK && lastblank) continue;
        lastblank = c->type == B_BLANK;
        renderblock(&o, c, 0, 0);
        if (c->off < split) mark = b.len;
    }
    r.html    = finish(&b);
    r.excerpt = mark;
    if ((byte *)(b.data+b.cap) == perm->beg) {
        perm->beg = (byte *)(b.data+b.len);  // return the unused capacity
    }
    return r;
}
