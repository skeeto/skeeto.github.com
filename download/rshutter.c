#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct frame {
    size_t width;
    size_t height;
    unsigned char data[];
};

static struct frame *
frame_create(size_t width, size_t height)
{
    struct frame *f = malloc(sizeof(*f) + width * height * 3);
    f->width = width;
    f->height = height;
    return f;
}

static void
frame_write(struct frame *f)
{
    printf("P6\n%zu %zu\n255\n", f->width, f->height);
    fwrite(f->data, f->width * f->height, 3, stdout);
}

static struct frame *
frame_read(struct frame *f)
{
    size_t width, height;
    if (scanf("P6 %zu%zu%*d%*c", &width, &height) < 2) {
        free(f);
        return 0;
    }
    if (!f || f->width != width || f->height != height) {
        free(f);
        f = frame_create(width, height);
    }
    fread(f->data, width * height, 3, stdin);
    return f;
}

int
main(int argc, char **argv)
{
    int shutter_step = argc > 1 ? atoi(argv[1]) : 5;
    struct frame *f = frame_read(0);
    struct frame *out = frame_create(f->width, f->height);
    size_t shutter = 0;
    while (shutter < f->height && (f = frame_read(f))) {
        size_t offset = shutter * f->width * 3;
        size_t length = f->height * f->width * 3 - offset;
        memcpy(out->data + offset, f->data + offset, length);
        frame_write(out);
        shutter += shutter_step;
    }
    free(out);
    free(f);
}
