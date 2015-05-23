/**
 * Reads concatenated PPMs (netpbm) on stdin and writes the montage as
 * a PPM to stdout. Puts 60 images per row and only takes a few
 * milliseconds.
 *
 * Compile program:
 *   $ gcc -std=c99 -O3 -o fastmontage fastmontage.c
 *
 * Video montage:
 *   $ avconv -i video.mp4 -s:v 32x18 -r 1 -c:v ppm -f image2pipe - | \
 *         ./fastmontage | convert - montage.png
 *
 * Warning: The PPM reader is incomplete as it doesn't tolerate PPM
 *          header comments, so don't try to input images with them!
 *
 * This is free and unencumbered software released into the public domain.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#include <io.h>
#include <fcntl.h>
#endif

#define WIDTH 60

typedef struct image {
    long width;
    long height;
    char colors[];
} image;

static image *
image_create(long width, long height)
{
    image *im = calloc(sizeof(*im) + width * height * 3, 1);
    im->width = width;
    im->height = height;
    return im;
}

static image *
image_grow(image *im, long new_height)
{
    im = realloc(im, sizeof(*im) + im->width * new_height * 3);
    if (new_height > im->height) {
        void *old_end = im->colors + im->width * im->height * 3;
        size_t size = im->width * (new_height - im->height) * 3;
        memset(old_end, 0, size);
    }
    im->height = new_height;
    return im;
}

static image *
image_read(image *im)
{
    long width, height, depth;
    if (scanf("P6 %ld %ld %ld", &width, &height, &depth) != 3)
        return NULL;
    getchar();
    if (im != NULL) {
        if (im->width != width || im->height != height || depth != 255)
            return NULL;
    } else {
        im = image_create(width, height);
    }
    fread(im->colors, width * height, 3, stdin);
    return im;
}

static void
image_copy(image *dst, long x, long y, const image *src)
{
    for (long h = 0; h < src->height; h++)
        memcpy(dst->colors + (y + h) * dst->width * 3 + (x * 3),
               src->colors + (0 + h) * src->width * 3 + (0 * 3),
               src->width * 3);
}

static void
image_write(const image *im)
{
    printf("P6\n%ld %ld\n255\n", im->width, im->height);
    fwrite(im->colors, im->width * im->height, 3, stdout);
}

int
main(void)
{
    #ifdef _WIN32
    _setmode(fileno(stdout), _O_BINARY);
    _setmode(fileno(stdin), _O_BINARY);
    #endif

    image *frame = image_read(NULL);
    image *final = image_create(frame->width * WIDTH, frame->height);
    image_copy(final, 0, 0, frame);
    long count = 1;
    while (image_read(frame)) {
        long x = count % WIDTH;
        long y = count / WIDTH;
        if ((y + 1) * frame->height > final->height)
            final = image_grow(final, final->height * 2);
        image_copy(final, x * frame->width, y * frame->height, frame);
        count++;
    }
    final = image_grow(final, ((count + WIDTH - 1) / WIDTH) * frame->height);
    image_write(final);
    free(final);
    free(frame);
    return 0;
}
