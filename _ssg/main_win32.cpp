// Win32 platform layer: no CRT, no headers, and kernel32.dll the only import
// $ cmake -S _ssg -B _ssg/build-w32 -DCMAKE_TOOLCHAIN_FILE=cmake/mingw-w64.cmake
//
// Paths are UTF-8 with either separator. Each is made absolute and
// extended-length (\\?\) before use, so MAX_PATH never applies.
//
// This is free and unencumbered software released into the public domain.
#include "ssg.cpp"
#include "wide.cpp"

enum : i32 {
    CREATE_ALWAYS                 = 2,
    ERROR_ALREADY_EXISTS          = 183,
    FILE_ATTRIBUTE_DEVICE         = 0x40,
    FILE_ATTRIBUTE_DIRECTORY      = 0x10,
    FILE_ATTRIBUTE_NORMAL         = 0x80,
    FILE_ATTRIBUTE_REPARSE_POINT  = 0x400,
    FILE_SHARE_ALL                = 7,
    FILE_TYPE_DISK                = 1,
    FIND_FIRST_EX_LARGE_FETCH     = 2,
    FindExInfoBasic               = 1,
    FindExSearchNameMatch         = 0,
    GENERIC_READ                  = (i32)0x80000000,
    GENERIC_WRITE                 = 0x40000000,
    GetFileExInfoStandard         = 0,
    INVALID_FILE_ATTRIBUTES       = -1,
    IO_REPARSE_TAG_NAME_SURROGATE = 0x20000000,  // symlinks, junctions
    MEM_COMMIT                    = 0x1000,
    MEM_RESERVE                   = 0x2000,
    OPEN_EXISTING                 = 3,
    PAGE_READWRITE                = 4,
    STD_ERROR_HANDLE              = -12,
};

enum : uz {
    INVALID_HANDLE_VALUE = (uz)-1,
};

struct Filetime {
    u32 lo;
    u32 hi;
};

struct FileAttributes {  // WIN32_FILE_ATTRIBUTE_DATA
    u32      attr;
    Filetime ctime;
    Filetime atime;
    Filetime mtime;
    u32      sizehi;
    u32      sizelo;
};
static_assert(sizeof(FileAttributes) == 36);

struct FindData {  // WIN32_FIND_DATAW
    u32      attr;
    Filetime ctime;
    Filetime atime;
    Filetime mtime;
    u32      sizehi;
    u32      sizelo;
    u32      reparsetag;
    u32      reserved;
    c16      name[260];
    c16      altname[14];
};
static_assert(sizeof(FindData) == 592);

#define W32(r, p) extern "C" __declspec(dllimport) r __stdcall p noexcept
W32(b32,    CloseHandle(uz));
W32(b32,    CopyFileW(c16 *, c16 *, b32));
W32(b32,    CreateDirectoryW(c16 *, uz));
W32(uz,     CreateFileW(c16 *, i32, i32, uz, i32, i32, uz));
W32(void,   ExitProcess[[noreturn]](i32));
W32(b32,    FindClose(uz));
W32(uz,     FindFirstFileExW(c16 *, i32, FindData *, i32, uz, i32));
W32(b32,    FindNextFileW(uz, FindData *));
W32(c16 *,  GetCommandLineW());
W32(b32,    GetConsoleMode(uz, i32 *));
W32(b32,    GetFileAttributesExW(c16 *, i32, FileAttributes *));
W32(i32,    GetFileAttributesW(c16 *));
W32(b32,    GetFileSizeEx(uz, i64 *));
W32(i32,    GetFileType(uz));
W32(i32,    GetFullPathNameW(c16 *, i32, c16 *, c16 **));
W32(i32,    GetLastError());
W32(uz,     GetStdHandle(i32));
W32(void,   GetSystemTimeAsFileTime(Filetime *));
W32(b32,    ReadFile(uz, u8 *, i32, i32 *, uz));
W32(void,   Sleep(i32));
W32(byte *, VirtualAlloc(uz, iz, i32, i32));
W32(b32,    WriteConsoleW(uz, c16 *, i32, i32 *, uz));
W32(b32,    WriteFile(uz, u8 *, i32, i32 *, uz));

// GCC emits calls to these even without a CRT. Defining them keeps
// msvcrt.dll out of the imports.
#if defined(__x86_64__) || defined(__i386__)
extern "C" void *memcpy(void *dst, void const *src, uz len)
{
    void *r = dst;
    __asm volatile ("rep movsb" : "+D"(dst), "+S"(src), "+c"(len) : : "memory");
    return r;
}

extern "C" void *memset(void *dst, i32 c, uz len)
{
    void *r = dst;
    __asm volatile ("rep stosb" : "+D"(dst), "+c"(len) : "a"(c) : "memory");
    return r;
}

extern "C" void *memchr(void const *s, i32 c, uz len)
{
    u8 *p = (u8 *)s;
    for (uz i = 0; i < len; i++) {
        if (p[i] == (u8)c) {
            return p + i;
        }
    }
    return 0;
}

extern "C" i32 memcmp(void const *a, void const *b, uz len)
{
    u8 *x = (u8 *)a;
    u8 *y = (u8 *)b;
    for (uz i = 0; i < len; i++) {
        if (x[i] != y[i]) {
            return x[i] - y[i];
        }
    }
    return 0;
}
#endif

struct Os {};

static i32 chunk(iz len)  // bound a single ReadFile/WriteFile
{
    i32 max = 1<<21;
    return len<max ? (i32)len : max;
}

static u64 ticks(Filetime t)
{
    return (u64)t.hi<<32 | t.lo;
}

// Absolute, extended-length, null-terminated form of a path. Falls back
// to the plain conversion if the system rejects the path, leaving the
// error for the caller's system call to report.
static Str16 widepath(Arena *a, Str path)
{
    Str16 rel = towide(a, path);
    c16  *buf = alloc<c16>(a, 0);
    iz    max = (a->end - (byte *)buf)/(iz)sizeof(c16) - 8;
    i32   cap = max<0x8000 ? (i32)max : 0x8000;  // longest possible path
    i32   len = cap>0 ? GetFullPathNameW(rel.data, cap, buf+6, 0) : 0;
    if (!len || len>=cap) {
        return rel;
    }
    Str16 r = extendpath(buf, len);
    a->beg = (byte *)(r.data + r.len + 1);  // keep the terminator
    return r;
}

static b32 writeall(uz h, Str s)
{
    for (iz off = 0; off < s.len;) {
        i32 len = chunk(s.len - off);
        if (!WriteFile(h, s.data+off, len, &len, 0) || !len) {
            return 0;
        }
        off += len;
    }
    return 1;
}

static Str os_read(Os *, Arena *a, Str path)
{
    Arena tmp = *a;
    uz h = CreateFileW(
        widepath(&tmp, path).data, GENERIC_READ, FILE_SHARE_ALL, 0,
        OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0
    );
    if (h == INVALID_HANDLE_VALUE) {
        return {};
    }
    i64 size = 0;
    if (GetFileType(h)!=FILE_TYPE_DISK || !GetFileSizeEx(h, &size)) {
        CloseHandle(h);
        return {};
    }
    if (size > a->end - a->beg) {
        os_oom();
    }

    Str r  = {};
    r.data = allocbytes(a, (iz)size);
    while (r.len < size) {
        i32 len = chunk((iz)size - r.len);
        if (!ReadFile(h, r.data+r.len, len, &len, 0) || !len) {
            CloseHandle(h);
            return {};
        }
        r.len += len;
    }
    CloseHandle(h);
    return r;
}

static b32 os_write(Os *, Arena scratch, Str path, Str data)
{
    uz h = CreateFileW(
        widepath(&scratch, path).data, GENERIC_WRITE, FILE_SHARE_ALL, 0,
        CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0
    );
    if (h == INVALID_HANDLE_VALUE) {
        return 0;
    }
    b32 ok = writeall(h, data);
    ok &= !!CloseHandle(h);
    return ok;
}

static b32 os_mkdirs(Os *, Arena scratch, Str path)
{
    Str16 p    = widepath(&scratch, path);
    c16  *s    = p.data;
    iz    root = rootlen(p);

    // Walk up to the nearest existing ancestor, usually the path itself...
    iz end = p.len;
    while (end > root) {
        c16 save = s[end];
        s[end] = 0;
        b32 exists = GetFileAttributesW(s) != INVALID_FILE_ATTRIBUTES;
        s[end] = save;
        if (exists) {
            break;
        }
        for (end--; end>root && s[end]!='\\' && s[end]!='/'; end--) {}
    }

    // ...then create the missing directories on the way back down. Like
    // mkdir -p, an existing non-directory is only an error as a parent.
    while (end < p.len) {
        iz next = end + 1;
        for (; next<p.len && s[next]!='\\' && s[next]!='/'; next++) {}
        c16 save = s[next];
        s[next] = 0;
        b32 ok = CreateDirectoryW(s, 0) || GetLastError()==ERROR_ALREADY_EXISTS;
        s[next] = save;
        if (!ok) {
            return 0;
        }
        end = next;
    }
    return 1;
}

static Dirent *os_list(Os *, Arena *a, Str path)
{
    if (!path.len) {
        return 0;  // fail like opendir(""), not list \* (the drive root)
    }

    Arena tmp = *a;
    Str16 dir = widepath(&tmp, path);
    c16  *pat = alloc<c16>(&tmp, dir.len+3);
    iz    len = dir.len;
    copybytes(pat, dir.data, len*(iz)sizeof(c16));
    if (!len || (pat[len-1]!='\\' && pat[len-1]!='/')) {
        pat[len++] = '\\';
    }
    pat[len] = '*';

    FindData fd = {};  // not in the arena: entries are allocated there
    uz h = FindFirstFileExW(
        pat, FindExInfoBasic, &fd, FindExSearchNameMatch, 0,
        FIND_FIRST_EX_LARGE_FETCH
    );
    if (h == INVALID_HANDLE_VALUE) {
        return 0;
    }

    Dirent *head = 0;
    do {
        c16 *name = fd.name;
        if (name[0]=='.' && (!name[1] || (name[1]=='.' && !name[2]))) {
            continue;
        }
        if (fd.attr & FILE_ATTRIBUTE_DEVICE) {
            continue;
        }
        if (fd.attr&FILE_ATTRIBUTE_REPARSE_POINT &&
                fd.reparsetag&IO_REPARSE_TAG_NAME_SURROGATE) {
            continue;
        }
        iz namelen = 0;
        for (; namelen<countof(fd.name) && name[namelen]; namelen++) {}
        Dirent *d = alloc<Dirent>(a);
        d->name  = fromwide(a, name, namelen);
        d->size  = (i64)fd.sizehi<<32 | fd.sizelo;
        d->mtime = (i64)ticks(fd.mtime);
        d->isdir = !!(fd.attr & FILE_ATTRIBUTE_DIRECTORY);
        d->next  = head;
        head = d;
    } while (FindNextFileW(h, &fd));
    FindClose(h);
    return head;
}

static i32 os_copy(Os *, Arena scratch, Str src, Str dst)
{
    c16 *wsrc = widepath(&scratch, src).data;
    c16 *wdst = widepath(&scratch, dst).data;

    FileAttributes s = {};
    FileAttributes d = {};
    if (!GetFileAttributesExW(wsrc, GetFileExInfoStandard, &s)) {
        return COPY_FAILED;
    }
    if (GetFileAttributesExW(wdst, GetFileExInfoStandard, &d) &&
            !(d.attr & FILE_ATTRIBUTE_DIRECTORY) &&
            d.sizehi==s.sizehi && d.sizelo==s.sizelo &&
            ticks(d.mtime)>=ticks(s.mtime)) {
        return COPY_SKIPPED;
    }
    // Unlike the POSIX layer, the copy keeps the source's timestamp.
    return CopyFileW(wsrc, wdst, 0) ? COPY_DONE : COPY_FAILED;
}

[[noreturn]] static void os_oom()
{
    writeall(GetStdHandle(STD_ERROR_HANDLE), "ssg: out of memory\n");
    ExitProcess(2);
}

static i64 os_now(Os *)
{
    Filetime ft = {};
    GetSystemTimeAsFileTime(&ft);
    return (i64)(ticks(ft)/10000000) - 11644473600;  // 1601 -> 1970
}

static void os_sleep(Os *, i32 ms)
{
    Sleep(ms);
}

static b32 os_print(Os *, i32 fd, Str s)
{
    uz  h    = GetStdHandle(-10 - fd);
    i32 mode = 0;
    if (!GetConsoleMode(h, &mode)) {
        return writeall(h, s);
    }

    // Consoles display UTF-16 correctly regardless of code page.
    c16 buf[512];
    i32 len = 0;
    for (iz i = 0; i < s.len;) {
        len += (i32)utf16encode(buf+len, utf8decode(s, &i));
        if (len>countof(buf)-2 || i==s.len) {
            i32 dummy;
            if (!WriteConsoleW(h, buf, len, &dummy, 0)) {
                return 0;
            }
            len = 0;
        }
    }
    return 1;
}

extern "C" __attribute((force_align_arg_pointer))
void mainCRTStartup()
{
    iz    cap = (iz)1<<28;
    byte *mem = VirtualAlloc(0, cap, MEM_COMMIT|MEM_RESERVE, PAGE_READWRITE);
    if (!mem) {
        os_oom();
    }
    Arena a = {mem, mem+cap};

    c16 *cmd = GetCommandLineW();
    iz   len = 0;
    for (; cmd[len]; len++) {}
    Slice<Str> args = splitcmdline(&a, fromwide(&a, cmd, len));

    Os os;
    ExitProcess(ssg_main(&os, a, args.data, (i32)args.len));
}
