/* cc -std=c99 -Os -Wl,-z,execstack trampoline.c */
#include <stdio.h>
#include <stdlib.h>

int cmp(const void *a, const void *b, _Bool invert)
{
    int r = *(int *)a - *(int *)b;
    return invert ? -r : r;
}

void intsort(int *base, size_t nmemb, _Bool invert)
{
    unsigned long fp = (unsigned long)cmp;
    volatile unsigned char buf[] = {
        // mov  edx, invert
        0xba, invert, 0x00, 0x00, 0x00,
        // mov  rax, cmp
        0x48, 0xb8, fp >>  0, fp >>  8, fp >> 16, fp >> 24,
                    fp >> 32, fp >> 40, fp >> 48, fp >> 56,
        // jmp  rax
        0xff, 0xe0
    };
    int (*trampoline)(const void *, const void *) = (void *)buf;
    qsort(base, nmemb, sizeof(*base), trampoline);
}

int
main(void)
{
    int a[] = {2, 4, 3, 1, 5};
    intsort(a, sizeof(a)/sizeof(a[0]), 1);
    printf("%d %d %d %d %d\n", a[0], a[1], a[2], a[3], a[4]);
    intsort(a, sizeof(a)/sizeof(a[0]), 0);
    printf("%d %d %d %d %d\n", a[0], a[1], a[2], a[3], a[4]);
}
