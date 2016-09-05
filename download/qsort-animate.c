#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#define ARRAY_SIZE      64
#define ELEMENT_WIDTH   8
#define ELEMENT_HEIGHT  100
#define MARKER_HEIGHT   8
#define SORT_FUNCTION   qsort

static int array[ARRAY_SIZE];

static uint64_t
xorshift64star(void)
{
    static uint64_t x = UINT64_C(0xaf7f68915f410bde);
    x ^= x >> 12;
    x ^= x << 25;
    x ^= x >> 27;
    return x * UINT64_C(0x2545f4914f6cdd1d);
}

static void
draw_array(int a, int b)
{
    printf("P6\n%d %d\n255\n", ELEMENT_WIDTH * ARRAY_SIZE, ELEMENT_HEIGHT);
    for (int y = 0; y < ELEMENT_HEIGHT; y++) {
        int edge = y < MARKER_HEIGHT || y > ELEMENT_HEIGHT - MARKER_HEIGHT;
        for (int x = 0; x < ELEMENT_WIDTH * ARRAY_SIZE; x++) {
            int xx = x / ELEMENT_WIDTH;
            int selected = xx == a || xx == b;
            int r = 0, g = 0, b = 0;
            if (edge && selected)
                r = 255;
            else
                r = g = b = (array[xx] * 255) / ARRAY_SIZE;
            printf("%c%c%c", r, g, b);
        }
    }
}

static int
intcmp(const void *a, const void *b)
{
    const int *ia = (int *) a, *ib = (int *) b;
    draw_array(ia - array, ib - array);
    return *ia - *ib;
}

static void
array_init(void)
{
    for (int i = 0; i < ARRAY_SIZE; i++)
        array[i] = i;
    for (int i = ARRAY_SIZE - 1; i > 0; i--) {
        int swap = xorshift64star() % (i + 1);
        int tmp = array[i];
        array[i] = array[swap];
        array[swap] = tmp;
    }
}

int
main(void)
{
    array_init();
    draw_array(-1, -1);
    SORT_FUNCTION(array, ARRAY_SIZE, sizeof(array[0]), intcmp);
    draw_array(-1, -1);
    return 0;
}
