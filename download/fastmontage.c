/**
 * Reads concatenated PPMs (netpbm) on stdin and writes the montage as
 * a PPM to stdout. Places 60 images per row (default); see -w option.
 *
 * Compile program:
 *   $ cc -std=c99 -O3 -o fastmontage fastmontage.c
 *
 * Video montage (with avconv + ImageMagick):
 *   $ avconv -i video.mp4 -s:v 32x18 -r 1 -c:v ppm -f image2pipe - | \
 *         ./fastmontage | convert - montage.png
 *
 * Image montage (with ImageMagick):
 *   $ find frames/ -iname '*.jpg' -print0 | sort -z | \
 *         xargs -0 -I {} convert {} ppm:- | \
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
#include <getopt.h>

#ifdef _WIN32
#include <io.h>
#include <fcntl.h>
#endif

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

static void
print_usage(char *program)
{
    printf("Usage: %s [-w <num_frames>] [-h]\n", program);
    puts("  -w <num_frames>   Input frames per row (60)");
    puts("  -h                Print usage information\n");
    puts("Reads concatenated PPMs on stdin, writes montage PPM to stdout.");
}

int
main(int argc, char **argv)
{
    #ifdef _WIN32
    _setmode(fileno(stdout), _O_BINARY);
    _setmode(fileno(stdin), _O_BINARY);
    #endif

    /* Options */
    int frames_wide = 60;
    int option;
    while ((option = getopt(argc, argv, "w:h")) != -1) {
        switch (option) {
            case 'w':
                frames_wide = atoi(optarg);
                break;
            case 'h':
                print_usage(argv[0]);
                return EXIT_SUCCESS;
            default:
                print_usage(argv[0]);
                return EXIT_FAILURE;
        }
    }

    image *frame = image_read(NULL);
    image *montage = image_create(frame->width * frames_wide, frame->height);
    image_copy(montage, 0, 0, frame);
    long count = 1;
    while (image_read(frame)) {
        long x = count % frames_wide;
        long y = count / frames_wide;
        if ((y + 1) * frame->height > montage->height)
            montage = image_grow(montage, montage->height * 2);
        image_copy(montage, x * frame->width, y * frame->height, frame);
        count++;
    }
    long frames_high = (count + frames_wide - 1) / frames_wide;
    montage = image_grow(montage, frames_high * frame->height);
    image_write(montage);
    free(montage);
    free(frame);
    return 0;
}
