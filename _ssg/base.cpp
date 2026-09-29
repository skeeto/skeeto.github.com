// Foundation: types, arenas, strings, buffers, maps, sorting
// This is free and unencumbered software released into the public domain.

using u8   = unsigned char;
using b32  = decltype(0);
using i32  = decltype(0);
using u32  = decltype(0u);
using i64  = decltype(0ll);
using u64  = decltype(0ull);
using iz   = decltype((u8 *)0 - (u8 *)0);
using uz   = decltype(sizeof(0));
using byte = decltype('0');

#define assert(c)   while (!(c)) __builtin_unreachable()
#define countof(a)  (iz)(sizeof(a) / sizeof(*(a)))

template<typename T>
void *operator new(uz, T *p) noexcept { return p; }

struct Arena {
    byte *beg = {};
    byte *end = {};
};

// Provided by the platform layer: report out-of-memory and exit.
[[noreturn]] static void os_oom();

template<typename T>
static T *alloc(Arena *a, iz count = 1)
{
    iz size = sizeof(T);
    iz pad  = -(uz)a->beg & (alignof(T) - 1);
    if (count >= (a->end - a->beg - pad)/size) {
        os_oom();
    }
    T *r = (T *)(a->beg + pad);
    a->beg += pad + count*size;
    for (iz i = 0; i < count; i++) {
        new (r+i) T();
    }
    return r;
}

// Like alloc<u8>, but does not zero the memory.
static u8 *allocbytes(Arena *a, iz count)
{
    if (count > a->end - a->beg) {
        os_oom();
    }
    u8 *r = (u8 *)a->beg;
    a->beg += count;
    return r;
}

static void copybytes(void *dst, void *src, iz len)
{
    if (len) __builtin_memcpy(dst, src, (uz)len);
}


// Strings

struct Str {
    union {
        u8         *data = 0;
        char const *cdata;
    };
    iz len = 0;

    Str() = default;

    constexpr Str(u8 *data, iz len) : data{data}, len{len} {}

    template<iz N>
    constexpr Str(char const (&s)[N]) : cdata{s}, len{N-1} {}

    u8 &operator[](iz i)
    {
        assert(i >= 0 && i < len);
        return data[i];
    }

    bool operator==(Str s)
    {
        return len==s.len && (!len || !__builtin_memcmp(data, s.data, (uz)len));
    }
};

static Str span(u8 *beg, u8 *end)
{
    return {beg, end - beg};
}

static Str takehead(Str s, iz n)
{
    assert(n >= 0 && n <= s.len);
    s.len = n;
    return s;
}

static Str cuthead(Str s, iz n)
{
    assert(n >= 0 && n <= s.len);
    s.data += n;
    s.len  -= n;
    return s;
}

static Str taketail(Str s, iz n)
{
    return cuthead(s, s.len-n);
}

static Str cuttail(Str s, iz n)
{
    return takehead(s, s.len-n);
}

static b32 startswith(Str s, Str prefix)
{
    return s.len>=prefix.len && takehead(s, prefix.len)==prefix;
}

static b32 endswith(Str s, Str suffix)
{
    return s.len>=suffix.len && taketail(s, suffix.len)==suffix;
}

static i32 compare(Str a, Str b)  // like memcmp, byte-wise
{
    iz len = a.len<b.len ? a.len : b.len;
    for (iz i = 0; i < len; i++) {
        i32 d = a[i] - b[i];
        if (d) return d;
    }
    return a.len<b.len ? -1 : a.len>b.len;
}

struct Cut {
    Str head;
    Str tail;
    b32 ok;
};

static Cut cut(Str s, u8 c)
{
    Cut r = {};
    iz  i = 0;
    for (; i < s.len && s[i] != c; i++) {}
    r.ok   = i < s.len;
    r.head = takehead(s, i);
    r.tail = cuthead(s, i+r.ok);
    return r;
}

static iz find(Str s, Str needle)  // -1 if not found
{
    if (!needle.len) {
        return 0;
    }
    u8 *last = s.data + s.len - needle.len;
    for (u8 *p = s.data; p <= last; p++) {
        if (*p==needle.data[0] && !__builtin_memcmp(p, needle.data, (uz)needle.len)) {
            return p - s.data;
        }
    }
    return -1;
}

static b32 whitespace(u8 c)
{
    return c==' ' || c=='\t' || c=='\n' || c=='\r' || c=='\f' || c=='\v';
}

static b32 digit(u8 c)
{
    return c>='0' && c<='9';
}

static b32 letter(u8 c)
{
    return (c>='a' && c<='z') || (c>='A' && c<='Z');
}

static b32 alnum(u8 c)
{
    return letter(c) || digit(c);
}

static u8 lowercase(u8 c)
{
    return c>='A' && c<='Z' ? (u8)(c + 32) : c;
}

static Str trimleft(Str s)
{
    for (; s.len && whitespace(*s.data); s.data++, s.len--) {}
    return s;
}

static Str trimright(Str s)
{
    for (; s.len && whitespace(s.data[s.len-1]); s.len--) {}
    return s;
}

static Str trim(Str s)
{
    return trimleft(trimright(s));
}

static Str clone(Arena *a, Str s)
{
    Str r = s;
    r.data = allocbytes(a, s.len);
    copybytes(r.data, s.data, s.len);
    return r;
}

// Concatenate, extending head in place when it sits at the arena's end.
static Str concat(Arena *a, Str head, Str tail)
{
    if (!head.data || (byte *)(head.data+head.len) != a->beg) {
        head = clone(a, head);
    }
    head.len += clone(a, tail).len;
    return head;
}

static u64 hash(Str s)
{
    u64 h = 0x100;
    for (iz i = 0; i < s.len; i++) {
        h ^= s[i];
        h *= 1111111111111111111u;
    }
    return h;
}

// Decode one UTF-8 code point at s[*i], advancing *i. Invalid bytes
// decode as themselves (Latin-1), one byte at a time.
static i32 utf8decode(Str s, iz *i)
{
    u8  c = s[(*i)++];
    i32 n = c>=0xf0 ? 3 : c>=0xe0 ? 2 : c>=0xc0 ? 1 : 0;
    if (!n || *i+n > s.len) {
        return c;
    }
    i32 r = c & (0x3f >> n);
    for (i32 k = 0; k < n; k++) {
        u8 b = s[*i+k];
        if ((b & 0xc0) != 0x80) {
            return c;
        }
        r = r<<6 | (b & 0x3f);
    }
    *i += n;
    return r;
}

// Code point ending just before s[i], or -1 at the start.
static i32 utf8prev(Str s, iz i, iz *start)
{
    if (i <= 0) {
        *start = 0;
        return -1;
    }
    iz j = i - 1;
    for (i32 k = 0; k < 3 && j > 0 && (s[j]&0xc0) == 0x80; k++, j--) {}
    iz end = j;
    i32 r = utf8decode(s, &end);
    if (end != i) {
        j = i - 1;
        r = s[j];
    }
    *start = j;
    return r;
}


// Dynamic arrays

template<typename T>
struct Slice {
    T *data = {};
    iz len  = {};
    iz cap  = {};

    T &operator[](iz i)
    {
        assert(i >= 0 && i < len);
        return data[i];
    }
};

template<typename T>
static Slice<T> push(Arena *a, Slice<T> s, T v)
{
    if (s.len == s.cap) {
        if (!s.data || (byte *)(s.data+s.cap) != a->beg) {
            T *data = alloc<T>(a, s.cap);
            for (iz i = 0; i < s.len; i++) {
                data[i] = s.data[i];
            }
            s.data = data;
        }
        iz extend = s.cap ? s.cap : 8;
        alloc<T>(a, extend);
        s.cap += extend;
    }
    s.data[s.len++] = v;
    return s;
}


// Hash trie keyed by strings. With a null arena, lookup only.

template<typename V>
struct Map {
    Map *child[4];
    Str  key;
    V    val;
};

template<typename V>
static V *upsert(Map<V> **m, Str key, Arena *a)
{
    for (u64 h = hash(key); *m; h <<= 2) {
        if ((*m)->key == key) {
            return &(*m)->val;
        }
        m = &(*m)->child[h>>62];
    }
    if (!a) {
        return 0;
    }
    *m = alloc<Map<V>>(a);
    (*m)->key = key;
    return &(*m)->val;
}


// Output buffers: appending grows in the arena, in place when possible.

struct Buf {
    u8    *data = {};
    iz     len  = {};
    iz     cap  = {};
    Arena *a    = {};

    Buf() = default;
    Buf(Arena *a, iz cap = 1<<12) : data{allocbytes(a, cap)}, cap{cap}, a{a} {}
};

static void reserve(Buf *b, iz need)
{
    if (b->cap - b->len >= need) {
        return;
    }
    Arena *a = b->a;
    iz extend = b->cap > need ? b->cap : need;
    if ((byte *)(b->data+b->cap) == a->beg) {
        allocbytes(a, extend);
    } else {
        u8 *data = allocbytes(a, b->cap+extend);
        copybytes(data, b->data, b->len);
        b->data = data;
    }
    b->cap += extend;
}

static void print(Buf *b, Str s)
{
    reserve(b, s.len);
    copybytes(b->data+b->len, s.data, s.len);
    b->len += s.len;
}

static void putbyte(Buf *b, u8 c)
{
    reserve(b, 1);
    b->data[b->len++] = c;
}

static void print(Buf *b, i64 v)
{
    u8  tmp[32];
    u8 *end = tmp + countof(tmp);
    u8 *beg = end;
    i64 t   = v<0 ? v : -v;
    do {
        *--beg = (u8)('0' - t%10);
    } while (t /= 10);
    if (v < 0) {
        *--beg = '-';
    }
    print(b, span(beg, end));
}

// Zero-padded decimal of at least width digits.
static void printpad(Buf *b, i64 v, i32 width)
{
    i64 limit = 1;
    for (i32 i = 1; i < width; i++) {
        limit *= 10;
        if (v < limit) putbyte(b, '0');
    }
    print(b, v);
}

static void printhex(Buf *b, u64 v, i32 digits)
{
    for (i32 i = digits-1; i >= 0; i--) {
        putbyte(b, "0123456789abcdef"[v>>(4*i) & 15]);
    }
}

// Escape & < > for HTML text content.
static void printhtml(Buf *b, Str s)
{
    iz last = 0;
    for (iz i = 0; i < s.len; i++) {
        Str rep = {};
        switch (s[i]) {
        case '&': rep = "&amp;"; break;
        case '<': rep = "&lt;";  break;
        case '>': rep = "&gt;";  break;
        default: continue;
        }
        print(b, span(s.data+last, s.data+i));
        print(b, rep);
        last = i + 1;
    }
    print(b, cuthead(s, last));
}

// Escape & < > " for double-quoted HTML attributes.
static void printattr(Buf *b, Str s)
{
    iz last = 0;
    for (iz i = 0; i < s.len; i++) {
        Str rep = {};
        switch (s[i]) {
        case '&': rep = "&amp;";  break;
        case '<': rep = "&lt;";   break;
        case '>': rep = "&gt;";   break;
        case '"': rep = "&quot;"; break;
        default: continue;
        }
        print(b, span(s.data+last, s.data+i));
        print(b, rep);
        last = i + 1;
    }
    print(b, cuthead(s, last));
}

static Str finish(Buf *b)
{
    return {b->data, b->len};
}


// Stable merge sort

template<typename T>
static void sort(T *a, iz n, b32 (*less)(T *, T *), Arena scratch)
{
    if (n < 2) {
        return;
    }
    T *tmp = alloc<T>(&scratch, n);
    for (iz width = 1; width < n; width *= 2) {
        for (iz lo = 0; lo < n; lo += 2*width) {
            iz mid = lo+width < n ? lo+width : n;
            iz hi  = lo+2*width < n ? lo+2*width : n;
            iz i = lo, j = mid, k = lo;
            while (i < mid && j < hi) {
                tmp[k++] = less(a+j, a+i) ? a[j++] : a[i++];
            }
            while (i < mid) tmp[k++] = a[i++];
            while (j < hi)  tmp[k++] = a[j++];
        }
        for (iz i = 0; i < n; i++) {
            a[i] = tmp[i];
        }
    }
}
