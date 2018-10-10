---
title: Integer Overflow into Information Disclosure
layout: post
date: 2017-07-19T01:57:36Z
tags: [netsec, c]
uuid: c85545c5-23a4-3147-b654-6dc7a62ee426
---

Last week I was discussing [CVE-2017-7529][cve] with [my intern][intern].
Specially crafted input to Nginx causes an integer overflow which has the
potential to leak sensitive information. But how could an integer overflow
be abused to trick a program into leaking information? To answer this
question, I put together the simplest practical example I could imagine.

* <https://github.com/skeeto/integer-overflow-demo>

This small C program converts a vector image from a custom format
(described below) into a [Netpbm image][netpbm], a [conveniently simple
format][fan]. The program defensively and carefully parses its input, but
still makes a subtle, fatal mistake. This mistake not only leads to
sensitive information disclosure, but, with a more sophisticated attack,
could be used to execute arbitrary code.

After getting the hang of the interface for the program, I encourage you
to take some time to work out an exploit yourself. Regardless, I'll reveal
a functioning exploit and explain how it works.

### A new vector format

The input format is line-oriented and very similar to Netpbm itself. The
first line is the header, starting with the magic number `V2` (ASCII)
followed by the image dimensions. The target output format is Netpbm's
"P2" (text gray scale) format, so the "V2" parallels it. The file must end
with a newline.

    V2 <width> <height>

What follows is drawing commands, one per line. For example, the `s`
command sets the value of a particular pixel.

    s <x> <y> <00–ff>

Since it's not important for the demonstration, this is the only command I
implemented. It's easy to imagine additional commands to draw lines,
circles, Bezier curves, etc.

Here's an example (`example.txt`) that draws a single white point in the
middle of the image:

    V2 256 256
    s 127 127 ff

The rendering tool reads standard input to standard output:

    $ render < example.txt > example.pgm

Here's what it looks like rendered:

![](/img/int-overflow/example.png)

However, you will notice that when you run the rendering tool, it prompts
you for username and password. This is silly, of course, but it's an
excuse to get "sensitive" information into memory. It will accept any
username/password combination where the username and password don't match
each other. The key is this: **It's possible to craft a valid image that
leaks the the entered password.**

### Tour of the implementation

Without spoiling anything yet, let's look at how this program works. The
first thing to notice is that I'm using a custom "[obstack][obstack]"
allocator instead of `malloc()` and `free()`. Real-world allocators have
some defenses against this particular vulnerability. Plus a specific
exploit would have to target a specific libc. By using my own allocator,
the exploit will mostly be portable, making for a better and easier
demonstration.

The allocator interface should be pretty self-explanatory, except for two
details. This is an *obstack* allocator, so freeing an object also frees
every object allocated after it. Also, it doesn't call `malloc()` in the
background. At initialization you give it a buffer from which to allocate
all memory.

~~~c
struct mstack {
    char *top;
    char *max;
    char buf[];
};

struct mstack *mstack_init(void *, size_t);
void          *mstack_alloc(struct mstack *, size_t);
void           mstack_free(struct mstack *, void *);
~~~

There are no vulnerabilities in these functions (I hope!). It's just
here for predictability.

Next here's the "authentication" function. It reads a username and
password combination from `/dev/tty`. It's only an excuse to get a flag in
memory for this capture-the-flag game. The username and password must be
less than 32 characters each.

~~~c
int
authenticate(struct mstack *m)
{
    FILE *tty = fopen("/dev/tty", "r+");
    if (!tty) {
        perror("/dev/tty");
        return 0;
    }

    char *user = mstack_alloc(m, 32);
    if (!user) {
        fclose(tty);
        return 0;
    }
    fputs("User: ", tty);
    fflush(tty);
    if (!fgets(user, 32, tty))
        user[0] = 0;

    char *pass = mstack_alloc(m, 32);
    int result = 0;
    if (pass) {
        fputs("Password: ", tty);
        fflush(tty);
        if (fgets(pass, 32, tty))
            result = strcmp(user, pass) != 0;
    }

    fclose(tty);
    mstack_free(m, user);
    return result;
}
~~~

Next here's a little version of `calloc()` for the custom allocator. Hmm,
I wonder why this is called "naive"…

~~~c
void *
naive_calloc(struct mstack *m, unsigned long nmemb, unsigned long size)
{
    void *p = mstack_alloc(m, nmemb * size);
    if (p)
        memset(p, 0, nmemb * size);
    return p;
}
~~~

Next up is a paranoid wrapper for `strtoul()` that defensively checks its
inputs. If it's out of range of an `unsigned long`, it bails out. If
there's trailing garbage, it bails out. If there's no number at all, it
bails out. If you make prolonged eye contact, it bails out.

~~~c
unsigned long
safe_strtoul(char *nptr, char **endptr, int base)
{
    errno = 0;
    unsigned long n = strtoul(nptr, endptr, base);
    if (errno) {
        perror(nptr);
        exit(EXIT_FAILURE);
    } else if (nptr == *endptr) {
        fprintf(stderr, "Expected an integer\n");
        exit(EXIT_FAILURE);
    } else if (!isspace(**endptr)) {
        fprintf(stderr, "Invalid character '%c'\n", **endptr);
        exit(EXIT_FAILURE);
    }
    return n;
}
~~~

The `main()` function parses the header using this wrapper and allocates
some zeroed memory:

~~~c
    unsigned long width = safe_strtoul(p, &p, 10);
    unsigned long height = safe_strtoul(p, &p, 10);
    unsigned char *pixels = naive_calloc(m, width, height);
    if (!pixels) {
        fputs("Not enough memory\n", stderr);
        exit(EXIT_FAILURE);
    }
~~~

Then there's a command processing loop, also using `safe_strtoul()`. It
carefully checks bounds against `width` and `height`. Finally it writes
out a Netpbm, P2 (.pgm) format.

~~~c
    printf("P2\n%ld %ld 255\n", width, height);
    for (unsigned long y = 0; y < height; y++) {
        for (unsigned long x = 0; x < width; x++)
            printf("%d ", pixels[y * width + x]);
        putchar('\n');
    }
~~~

The vulnerability is in something I've shown above. Can you find it?

### Exploiting the renderer

Did you find it? If you're on a platform with 64-bit `long`, here's your
exploit:

    V2 16 1152921504606846977

And here's an exploit for 32-bit `long`:

    V2 16 268435457

Here's how it looks in action. The most obvious result is that the program
crashes:

    $ echo V2 16 1152921504606846977 | ./mstack > capture.txt
    User: coolguy
    Password: mysecret
    Segmentation fault

Here are the initial contents of `capture.txt`:

    P2
    16 1152921504606846977 255
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    109 121 115 101 99 114 101 116 10 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0

Where did those junk numbers come from in the image data? Plug them into
an ASCII table and you'll get "mysecret". Despite allocating the image
with `naive_calloc()`, the password has found its way into the image! How
could this be?

What happened is that `width * height` overflows an `unsigned long`.
(Well, technically speaking, unsigned integers are defined *not* to
overflow in C, wrapping around instead, but it's really the same thing.)
In `naive_calloc()`, the overflow results in a value of 16, so it only
allocates and clears 16 bytes. The requested allocation "succeeds" despite
*far* exceeding the available memory. The caller has been given a lot less
memory than expected, and the memory believed to have been allocated
contains a password.

The final part that writes the output doesn't multiply the integers and
doesn't need to test for overflow. It uses a nested loop instead,
continuing along with the original, impossible image size.

How do we fix this? Add an overflow check at the beginning of the
`naive_calloc()` function (making it no longer naive). This is what the
real `calloc()` does.

~~~c
    if (nmemb && size > -1UL / nmemb)
        return 0;
~~~

The frightening takeaway is that this check is *very* easy to forget. It's
a subtle bug with potentially disastrous consequences.

In practice, this sort of program wouldn't have sensitive data resident in
memory. Instead an attacker would target the program's stack with those
`s` commands — specifically the [return pointers][cfg] — and perform a ROP
attack against the application. With the exploit header above and a
platform where `long` the same size as a `size_t`, the program will behave
as if all available memory has been allocated to the image, so the `s`
command could be used to poke custom values *anywhere* in memory. This is
a much more complicated exploit, and it has to contend with ASLR and
random stack gap, but it's feasible.


[cve]: https://security-tracker.debian.org/tracker/CVE-2017-7529
[intern]: /blog/2016/09/02/
[fan]: /blog/2017/07/02/
[netpbm]: https://en.wikipedia.org/wiki/Netpbm_format
[cfg]: /blog/2017/01/21/
[obstack]: http://www.gnu.org/software/libc/manual/html_node/Obstacks.html
