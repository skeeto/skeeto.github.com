---
title: C Object Oriented Programming
layout: post
date: 2014-10-21T03:52:43Z
tags: [c, cpp, tutorial]
uuid: 3851ee30-1f9d-35af-e59f-e4be5023b2d5
---

~~Object oriented programming, polymorphism in particular, is
essential to nearly any large, complex software system. Without it,
decoupling different system components is difficult.~~ (*Update in
2017*: I no longer agree with this statement.) C doesn't come with
object oriented capabilities, so large C programs tend to grow their
own out of C's primitives. This includes huge C projects like the
Linux kernel, BSD kernels, and SQLite.

### Starting Simple

Suppose you're writing a function `pass_match()` that takes an input
stream, an output stream, and a pattern. It works sort of like grep.
It passes to the output each line of input that matches the pattern.
The pattern string contains a shell glob pattern to be handled by
[POSIX `fnmatch()`][fnmatch]. Here's what the interface looks like.

~~~c
void pass_match(FILE *in, FILE *out, const char *pattern);
~~~

Glob patterns are simple enough that pre-compilation, as would be done
for a regular expression, is unnecessary. The bare string is enough.

Some time later the customer wants the program to support regular
expressions in addition to shell-style glob patterns. For efficiency's
sake, regular expressions need to be pre-compiled and so will not be
passed to the function as a string. It will instead be a [POSIX
`regex_t`][regex] object. A quick-and-dirty approach might be to
accept both and match whichever one isn't NULL.

~~~c
void pass_match(FILE *in, FILE *out, const char *pattern, regex_t *re);
~~~

Bleh. This is ugly and won't scale well. What happens when more kinds
of filters are needed? It would be much better to accept a single
object that covers both cases, and possibly even another kind of
filter in the future.

### A Generalized Filter

One of the most common ways to customize the the behavior of a
function in C is to pass a function pointer. For example, the final
argument to [`qsort()`][qsort] is a comparator that determines how
objects get sorted.

For `pass_match()`, this function would accept a string and return a
boolean value deciding if the string should be passed to the output
stream. It gets called once on each line of input.

~~~c
void pass_match(FILE *in, FILE *out, bool (*match)(const char *));
~~~

However, this has one of the [same problems as `qsort()`][closure]:
the passed function lacks context. It needs a pattern string or
`regex_t` object to operate on. In other languages these would be
attached to the function as a closure, but C doesn't have closures. It
would need to be smuggled in via a global variable, [which is not
good][global].

~~~c
static regex_t regex;  // BAD!!!

bool regex_match(const char *string)
{
    return regexec(&regex, string, 0, NULL, 0) == 0;
}
~~~

Because of the global variable, in practice `pass_match()` would be
neither reentrant nor thread-safe. We could take a lesson from GNU's
`qsort_r()` and accept a context to be passed to the filter function.
This simulates a closure.

~~~c
void pass_match(FILE *in, FILE *out,
                bool (*match)(const char *, void *), void *context);
~~~

The provided context pointer would be passed to the filter function as
the second argument, and no global variables are needed. This would
probably be good enough for most purposes and it's about as simple as
possible. The interface to `pass_match()` would cover any kind of
filter.

But wouldn't it be nice to package the function and context together
as one object?

### More Abstraction

How about putting the context on a struct and making an interface out
of that? Here's a tagged union that behaves as one or the other.

~~~c
enum filter_type { GLOB, REGEX };

struct filter {
    enum filter_type type;
    union {
        const char *pattern;
        regex_t regex;
    } context;
};
~~~

There's one function for interacting with this struct:
`filter_match()`. It checks the `type` member and calls the correct
function with the correct context.

~~~c
bool filter_match(struct filter *filter, const char *string)
{
    switch (filter->type) {
    case GLOB:
        return fnmatch(filter->context.pattern, string, 0) == 0;
    case REGEX:
        return regexec(&filter->context.regex, string, 0, NULL, 0) == 0;
    }
    abort(); // programmer error
}
~~~

And the `pass_match()` API now looks like this. This will be the final
change to `pass_match()`, both in implementation and interface.

~~~c
void pass_match(FILE *input, FILE *output, struct filter *filter);
~~~

It still doesn't care how the filter works, so it's good enough to
cover all future cases. It just calls `filter_match()` on the pointer
it was given. However, the `switch` and tagged union aren't friendly
to extension. Really, it's outright hostile. We finally have some
degree of polymorphism, but it's crude. It's like building duct tape
into a design. Adding new behavior means adding another `switch` case.
This is a step backwards. We can do better.

#### Methods

With the `switch` we're no longer taking advantage of function
pointers. So what about putting a function pointer on the struct?

~~~c
struct filter {
    bool (*match)(struct filter *, const char *);
};
~~~

The filter itself is passed as the first argument, providing context.
In object oriented languages, that's the implicit `this` argument. To
avoid requiring the caller to worry about this detail, we'll hide it
in a new `switch`-free version of `filter_match()`.

~~~c
bool filter_match(struct filter *filter, const char *string)
{
    return filter->match(filter, string);
}
~~~

Notice we're still lacking the actual context, the pattern string or
the regex object. Those will be different structs that embed the
filter struct.

~~~c
struct filter_regex {
    struct filter filter;
    regex_t regex;
};

struct filter_glob {
    struct filter filter;
    const char *pattern;
};
~~~

For both the original filter struct is the first member. This is
critical. We're going to be using a trick called *type punning*. The
first member is guaranteed to be positioned at the beginning of the
struct, so a pointer to a `struct filter_glob` is also a pointer to a
`struct filter`. Notice any resemblance to inheritance?

Each type, glob and regex, needs its own match method.

~~~c
static bool
method_match_regex(struct filter *filter, const char *string)
{
    struct filter_regex *regex = (struct filter_regex *) filter;
    return regexec(&regex->regex, string, 0, NULL, 0) == 0;
}

static bool
method_match_glob(struct filter *filter, const char *string)
{
    struct filter_glob *glob = (struct filter_glob *) filter;
    return fnmatch(glob->pattern, string, 0) == 0;
}
~~~

I've prefixed them with `method_` to indicate their intended usage. I
declared these `static` because they're completely private. Other
parts of the program will only be accessing them through a function
pointer on the struct. This means we need some constructors in order
to set up those function pointers. (For simplicity, I'm not error
checking.)

~~~c
struct filter *filter_regex_create(const char *pattern)
{
    struct filter_regex *regex = malloc(sizeof(*regex));
    regcomp(&regex->regex, pattern, REG_EXTENDED);
    regex->filter.match = method_match_regex;
    return &regex->filter;
}

struct filter *filter_glob_create(const char *pattern)
{
    struct filter_glob *glob = malloc(sizeof(*glob));
    glob->pattern = pattern;
    glob->filter.match = method_match_glob;
    return &glob->filter;
}
~~~

Now this is real polymorphism. It's really simple from the user's
perspective. They call the correct constructor and get a filter object
that has the desired behavior. This object can be passed around
trivially, and no other part of the program worries about how it's
implemented. Best of all, since each method is a separate function
rather than a `switch` case, new kinds of filter subtypes can be
defined independently. Users can create their own filter types that
work just as well as the two "built-in" filters.

#### Cleaning Up

Oops, the regex filter needs to be cleaned up when it's done, but the
user, by design, won't know how to do it. Let's add a `free()` method.

~~~c
struct filter {
    bool (*match)(struct filter *, const char *);
    void (*free)(struct filter *);
};

void filter_free(struct filter *filter)
{
    return filter->free(filter);
}
~~~

And the methods for each. These would also be assigned in the
constructor.

~~~c
static void
method_free_regex(struct filter *f)
{
    struct filter_regex *regex = (struct filter_regex *) f;
    regfree(&regex->regex);
    free(f);
}

static void
method_free_glob(struct filter *f)
{
    free(f);
}
~~~

The glob constructor should perhaps `strdup()` its pattern as a
private copy, in which case it would be freed here.

### Object Composition

A good rule of thumb is to prefer composition over inheritance. Having
tidy filter objects opens up some interesting possibilities for
composition. Here's an AND filter that composes two arbitrary filter
objects. It only matches when both its subfilters match. It supports
short circuiting, so put the faster, or most discriminating, filter
first in the constructor (user's responsibility).

~~~c
struct filter_and {
    struct filter filter;
    struct filter *sub[2];
};

static bool
method_match_and(struct filter *f, const char *s)
{
    struct filter_and *and = (struct filter_and *) f;
    return filter_match(and->sub[0], s) && filter_match(and->sub[1], s);
}

static void
method_free_and(struct filter *f)
{
    struct filter_and *and = (struct filter_and *) f;
    filter_free(and->sub[0]);
    filter_free(and->sub[1]);
    free(f);
}

struct filter *filter_and(struct filter *a, struct filter *b)
{
    struct filter_and *and = malloc(sizeof(*and));
    and->sub[0] = a;
    and->sub[1] = b;
    and->filter.match = method_match_and;
    and->filter.free = method_free_and;
    return &and->filter;
}
~~~

It can combine a regex filter and a glob filter, or two regex filters,
or two glob filters, or even other AND filters. It doesn't care what
the subfilters are. Also, the `free()` method here frees its
subfilters. This means that the user doesn't need to keep hold of
every filter created, just the "top" one in the composition.

To make composition filters easier to use, here are two "constant"
filters. These are statically allocated, shared, and are never
actually freed.

~~~c
static bool
method_match_any(struct filter *f, const char *string)
{
    return true;
}

static bool
method_match_none(struct filter *f, const char *string)
{
    return false;
}

static void
method_free_noop(struct filter *f)
{
}

struct filter FILTER_ANY  = { method_match_any,  method_free_noop };
struct filter FILTER_NONE = { method_match_none, method_free_noop };
~~~

The `FILTER_NONE` filter will generally be used with a (theoretical)
`filter_or()` and `FILTER_ANY` will generally be used with the
previously defined `filter_and()`.

Here's a simple program that composes multiple glob filters into a
single filter, one for each program argument.

~~~c
int main(int argc, char **argv)
{
    struct filter *filter = &FILTER_ANY;
    for (char **p = argv + 1; *p; p++)
        filter = filter_and(filter_glob_create(*p), filter);
    pass_match(stdin, stdout, filter);
    filter_free(filter);
    return 0;
}
~~~

Notice only one call to `filter_free()` is needed to clean up the
entire filter.

### Multiple Inheritance

As I mentioned before, the filter struct must be the first member of
filter subtype structs in order for type punning to work. If we want
to "inherit" from two different types like this, they would both need
to be in this position: a contradiction.

Fortunately type punning can be generalized such that it the
first-member constraint isn't necessary. This is commonly done through
a `container_of()` macro. Here's a C99-conforming definition.

~~~c
#include <stddef.h>

#define container_of(ptr, type, member) \
    ((type *)((char *)(ptr) - offsetof(type, member)))
~~~

Given a pointer to a member of a struct, the `container_of()` macro
allows us to back out to the containing struct. Suppose the regex
struct was defined differently, so that the `regex_t` member came
first.

~~~c
struct filter_regex {
    regex_t regex;
    struct filter filter;
};
~~~

The constructor remains unchanged. The casts in the methods change to
the macro.

~~~c
static bool
method_match_regex(struct filter *f, const char *string)
{
    struct filter_regex *regex = container_of(f, struct filter_regex, filter);
    return regexec(&regex->regex, string, 0, NULL, 0) == 0;
}

static void
method_free_regex(struct filter *f)
{
    struct filter_regex *regex = container_of(f, struct filter_regex, filter);
    regfree(&regex->regex);
    free(f);

}
~~~

It's a constant, compile-time computed offset, so there should be no
practical performance impact. The filter can now participate freely in
other *intrusive* data structures, like linked lists and such. It's
analogous to multiple inheritance.

### Vtables

Say we want to add a third method, `clone()`, to the filter API, to
make an independent copy of a filter, one that will need to be
separately freed. It will be like the copy assignment operator in C++.
Each kind of filter will need to define an appropriate "method" for
it. As long as new methods like this are added at the end, this
doesn't break the API, but it does break the ABI regardless.

~~~c
struct filter {
    bool (*match)(struct filter *, const char *);
    void (*free)(struct filter *);
    struct filter *(*clone)(struct filter *);
};
~~~

The filter object is starting to get big. It's got three pointers —
24 bytes on modern systems — and these pointers are the same between
all instances of the same type. That's a lot of redundancy. Instead,
these pointers could be shared between instances in a common table
called a *virtual method table*, commonly known as a *vtable*.

Here's a vtable version of the filter API. The overhead is now only
one pointer regardless of the number of methods in the interface.

~~~c
struct filter {
    struct filter_vtable *vtable;
};

struct filter_vtable {
    bool (*match)(struct filter *, const char *);
    void (*free)(struct filter *);
    struct filter *(*clone)(struct filter *);
};
~~~

Each type creates its own vtable and links to it in the constructor.
Here's the regex filter re-written for the new vtable API and clone
method. This is all the tricks in one basket for a big object oriented
C finale!

~~~c
struct filter *filter_regex_create(const char *pattern);

struct filter_regex {
    regex_t regex;
    const char *pattern;
    struct filter filter;
};

static bool
method_match_regex(struct filter *f, const char *string)
{
    struct filter_regex *regex = container_of(f, struct filter_regex, filter);
    return regexec(&regex->regex, string, 0, NULL, 0) == 0;
}

static void
method_free_regex(struct filter *f)
{
    struct filter_regex *regex = container_of(f, struct filter_regex, filter);
    regfree(&regex->regex);
    free(f);
}

static struct filter *
method_clone_regex(struct filter *f)
{
    struct filter_regex *regex = container_of(f, struct filter_regex, filter);
    return filter_regex_create(regex->pattern);
}

/* vtable */
struct filter_vtable filter_regex_vtable = {
    method_match_regex, method_free_regex, method_clone_regex
};

/* constructor */
struct filter *filter_regex_create(const char *pattern)
{
    struct filter_regex *regex = malloc(sizeof(*regex));
    regex->pattern = pattern;
    regcomp(&regex->regex, pattern, REG_EXTENDED);
    regex->filter.vtable = &filter_regex_vtable;
    return &regex->filter;
}
~~~

This is almost exactly what's going on behind the scenes in C++. When
a method/function is declared `virtual`, and therefore dispatches
based on the run-time type of its left-most argument, it's listed in
the vtables for classes that implement it. Otherwise it's just a
normal function. This is why functions need to be declared `virtual`
ahead of time in C++.

In conclusion, it's relatively easy to get the core benefits of object
oriented programming in plain old C. It doesn't require heavy use of
macros, nor do users of these systems need to know that underneath
it's an object system, unless they want to extend it for themselves.

Here's the whole example program once if you're interested in poking:

* [https://gist.github.com/skeeto/5faa131b19673549d8ca](https://gist.github.com/skeeto/5faa131b19673549d8ca)


[fnmatch]: http://man7.org/linux/man-pages/man3/fnmatch.3.html
[regex]: http://man7.org/linux/man-pages/man3/regexec.3.html
[qsort]: http://man7.org/linux/man-pages/man3/qsort.3.html
[global]: /blog/2014/10/12/
[closure]: /blog/2014/08/29/
