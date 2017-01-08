// cc -std=c99 -D_BSD_SOURCE -Os closure-demo.c -lm

#if (!defined(__unix__) && !defined(__APPLE__)) || !defined(__x86_64__)
#  error Requires x86_64 and System V ABI
#endif

/* Closure functions. */
#include <string.h>
#include <unistd.h>
#include <sys/mman.h>

static void
closure_set_data(void *closure, void *data)
{
    void **p = closure;
    p[-2] = data;
}

static void
closure_set_function(void *closure, void *f)
{
    void **p = closure;
    p[-1] = f;
}

static unsigned char thunk[6][13] = {
    {
        0x48, 0x8b, 0x3d, 0xe9, 0xff, 0xff, 0xff,
        0xff, 0x25, 0xeb, 0xff, 0xff, 0xff
    }, {
        0x48, 0x8b, 0x35, 0xe9, 0xff, 0xff, 0xff,
        0xff, 0x25, 0xeb, 0xff, 0xff, 0xff
    }, {
        0x48, 0x8b, 0x15, 0xe9, 0xff, 0xff, 0xff,
        0xff, 0x25, 0xeb, 0xff, 0xff, 0xff
    }, {
        0x48, 0x8b, 0x0d, 0xe9, 0xff, 0xff, 0xff,
        0xff, 0x25, 0xeb, 0xff, 0xff, 0xff
    }, {
        0x4C, 0x8b, 0x05, 0xe9, 0xff, 0xff, 0xff,
        0xff, 0x25, 0xeb, 0xff, 0xff, 0xff
    }, {
        0x4C, 0x8b, 0x0d, 0xe9, 0xff, 0xff, 0xff,
        0xff, 0x25, 0xeb, 0xff, 0xff, 0xff
    },
};


static void *
closure_create(void *f, int nargs, void *userdata)
{
    long page_size = sysconf(_SC_PAGESIZE);
    int prot = PROT_READ | PROT_WRITE;
    int flags = MAP_ANONYMOUS | MAP_PRIVATE;
    char *p = mmap(0, page_size * 2, prot, flags, -1, 0);
    if (p == MAP_FAILED)
        return 0;
    void *closure = p + page_size;
    memcpy(closure, thunk[nargs - 1], sizeof(thunk[0]));
    mprotect(closure, page_size, PROT_READ | PROT_EXEC);
    closure_set_function(closure, f);
    closure_set_data(closure, userdata);
    return closure;
}

static void
closure_destroy(void *closure)
{
    long page_size = sysconf(_SC_PAGESIZE);
    munmap((char *)closure - page_size, page_size * 2);
}

/* Coordinates */
#include <math.h>

struct coord {
    float x;
    float y;
};

static inline float
distance(const struct coord *a, const struct coord *b)
{
    float dx = a->x - b->x;
    float dy = a->y - b->y;
    return sqrtf(dx * dx + dy * dy);
}

int
coord_cmp_r(const void *a, const void *b, void *target)
{
    float dist_a = distance(a, target);
    float dist_b = distance(b, target);
    if (dist_a < dist_b)
        return -1;
    else if (dist_a > dist_b)
        return 1;
    else
        return 0;
}

/* Demo */
#include <stdio.h>
#include <stdlib.h>

#define N 64

int
main(void)
{
    /* Create random coordinates. */
    size_t ncoords = N;
    struct coord coords[N];
    struct coord target = {
        1000.0f * rand() / (float)RAND_MAX,
        1000.0f * rand() / (float)RAND_MAX
    };
    for (int i = 0; i < N; i++) {
        coords[i].x = 1000.0f * rand() / (float)RAND_MAX;
        coords[i].y = 1000.0f * rand() / (float)RAND_MAX;
    }

    /* Sort using a closure. */
    int (*closure)(const void *, const void *);
    closure = closure_create(coord_cmp_r, 3, &target);
    qsort(coords, ncoords, sizeof(coords[0]), closure);
    closure_destroy(closure);

    /* Print results. */
    printf("target =% 7.5g,% 7.5g\n", target.x, target.y);
    for (int i = 0; i < N; i++)
        printf("% 7.5g,% 7.5g -> %.9g\n",
               coords[i].x, coords[i].y,
               distance(coords + i, &target));
}
