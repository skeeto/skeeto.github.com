// POSIX platform layer (macOS, Linux)
// This is free and unencumbered software released into the public domain.
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>
#include "ssg.cpp"

struct Os {};

static char *cstr(Arena *a, Str s)
{
    char *r = alloc<char>(a, s.len+1);
    copybytes(r, s.data, s.len);
    return r;
}

static Str os_read(Os *, Arena *a, Str path)
{
    Arena tmp = *a;
    i32 fd = open(cstr(&tmp, path), O_RDONLY);
    if (fd < 0) {
        return {};
    }
    struct stat st;
    if (fstat(fd, &st) || !S_ISREG(st.st_mode)) {
        close(fd);
        return {};
    }
    Str r  = {};
    r.data = allocbytes(a, (iz)st.st_size);
    while (r.len < st.st_size) {
        iz n = read(fd, r.data+r.len, (uz)(st.st_size - r.len));
        if (n <= 0) {
            close(fd);
            return {};
        }
        r.len += n;
    }
    close(fd);
    return r;
}

static b32 writeall(i32 fd, Str s)
{
    for (iz off = 0; off < s.len;) {
        iz n = write(fd, s.data+off, (uz)(s.len-off));
        if (n < 0) {
            if (errno == EINTR) continue;
            return 0;
        }
        off += n;
    }
    return 1;
}

static b32 os_write(Os *, Arena scratch, Str path, Str data)
{
    i32 fd = open(cstr(&scratch, path), O_WRONLY|O_CREAT|O_TRUNC, 0666);
    if (fd < 0) {
        return 0;
    }
    b32 ok = writeall(fd, data);
    ok &= !close(fd);
    return ok;
}

static b32 os_mkdirs(Os *, Arena scratch, Str path)
{
    char *p = cstr(&scratch, path);
    for (iz i = 1; i <= path.len; i++) {
        if (i<path.len && p[i]!='/') {
            continue;
        }
        char save = p[i];
        p[i] = 0;
        if (mkdir(p, 0777) && errno != EEXIST) {
            return 0;
        }
        p[i] = save;
    }
    return 1;
}

static Dirent *os_list(Os *, Arena *a, Str path)
{
    Arena tmp = *a;
    char *cpath = cstr(&tmp, path);
    DIR  *dir   = opendir(cpath);
    if (!dir) {
        return 0;
    }
    Dirent *head = 0;
    for (struct dirent *e; (e = readdir(dir));) {
        Str name = {(u8 *)e->d_name, 0};
        while (name.data[name.len]) name.len++;
        if (name == "." || name == "..") {
            continue;
        }
        Arena t = *a;  // past the entries allocated so far
        struct stat st;
        Str full = concat(&t, concat(&t, path, "/"), name);
        if (lstat(cstr(&t, full), &st)) {
            continue;
        }
        if (!S_ISDIR(st.st_mode) && !S_ISREG(st.st_mode)) {
            continue;
        }
        #ifdef __APPLE__
        struct timespec mtim = st.st_mtimespec;
        #else
        struct timespec mtim = st.st_mtim;
        #endif
        Dirent *d = alloc<Dirent>(a);
        d->name  = clone(a, name);
        d->size  = (i64)st.st_size;
        d->mtime = (i64)mtim.tv_sec*1000000000 + mtim.tv_nsec;
        d->isdir = S_ISDIR(st.st_mode);
        d->next  = head;
        head = d;
    }
    closedir(dir);
    return head;
}

static i32 os_copy(Os *, Arena scratch, Str src, Str dst)
{
    char *csrc = cstr(&scratch, src);
    char *cdst = cstr(&scratch, dst);

    struct stat ss, ds;
    if (stat(csrc, &ss)) {
        return COPY_FAILED;
    }
    if (!stat(cdst, &ds) && ds.st_size == ss.st_size &&
            ds.st_mtime >= ss.st_mtime) {
        return COPY_SKIPPED;
    }

    i32 in = open(csrc, O_RDONLY);
    if (in < 0) {
        return COPY_FAILED;
    }
    i32 out = open(cdst, O_WRONLY|O_CREAT|O_TRUNC, 0666);
    if (out < 0) {
        close(in);
        return COPY_FAILED;
    }
    iz  cap = 1<<16;
    u8 *buf = allocbytes(&scratch, cap);
    b32 ok  = 1;
    for (;;) {
        iz n = read(in, buf, (uz)cap);
        if (n < 0 && errno == EINTR) {
            continue;
        } else if (n <= 0) {
            ok = !n;
            break;
        }
        if (!writeall(out, {buf, n})) {
            ok = 0;
            break;
        }
    }
    close(in);
    ok &= !close(out);
    return ok ? COPY_DONE : COPY_FAILED;
}

[[noreturn]] static void os_oom()
{
    Str msg = "ssg: out of memory\n";
    writeall(2, msg);
    _exit(2);
}

static i64 os_now(Os *)
{
    return (i64)time(0);
}

static i64 os_clock(Os *)
{
    struct timespec ts = {};
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (i64)ts.tv_sec*1000 + ts.tv_nsec/1000000;
}

static b32 os_print(Os *, i32 fd, Str s)
{
    return writeall(fd, s);
}

static void os_sleep(Os *, i32 ms)
{
    struct timespec ts = {ms/1000, (long)(ms%1000)*1000000};
    while (nanosleep(&ts, &ts) && errno == EINTR) {}
}

int main(int argc, char **argv)
{
    iz    cap = (iz)1<<28;
    byte *mem = (byte *)mmap(0, (uz)cap, PROT_READ|PROT_WRITE,
                             MAP_PRIVATE|MAP_ANON, -1, 0);
    if (mem == MAP_FAILED) {
        return 2;
    }
    Arena a = {mem, mem+cap};

    Str *args = alloc<Str>(&a, argc);
    for (i32 i = 0; i < argc; i++) {
        args[i].data = (u8 *)argv[i];
        while (args[i].data[args[i].len]) args[i].len++;
    }

    Os os;
    return ssg_main(&os, a, args, argc);
}
