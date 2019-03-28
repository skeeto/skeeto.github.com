---
title: "Global State: a Tale of Two Bad C APIs"
layout: post
date: 2014-10-12T22:48:00Z
tags: [c, posix]
uuid: 8a1c5135-e669-308b-6605-58c86be3003b
---

Mutable global variables are evil. You've almost certainly heard that
before, but it's worth repeating. It makes programs, and libraries
especially, harder to understand, harder to optimize, more fragile,
more error prone, and less useful. If you're using global state in a
way that's visible to users of your API, and it's not essential to the
domain, you're almost certainly doing something wrong.

In this article I'm going to use two well-established C APIs to
demonstrate why global state is bad for APIs: BSD regular expressions
and POSIX Getopt.

### BSD Regular Expressions

The [BSD regular expression API][bsdre] dates back to 4.3BSD, released
in 1986. It's just a pair of functions: one compiles the regex, the
other executes it on a string.

~~~c
char *re_comp(const char *regex);
int   re_exec(const char *string);
~~~

It's immediately obvious that there's hidden internal state. Where
else would the resulting compiled regex object be? Also notice there's
no `re_free()`, or similar, for releasing resources held by the
compiled result. That's because, due to its limited design, it doesn't
hold any. It's entirely in static memory, which means there's some
upper limit on the complexity of the regex given to this API. Suppose
an implementation *does* use dynamically allocated memory. It seems
this might not matter when only one compiled regex is allowed.
However, this would create warnings in Valgrind and make it harder to
use for bug testing.

This API is not thread-safe. Only one thread can use it at a time.
It's not reentrant. While using a regex, calling another function that
might use a regex means you have to recompile when it returns, just in
case. The global state being entirely hidden, there's no way to tell
if another part of the program used it.


#### Fixing BSD Regular Expressions

This API has been deprecated for some time now, so hopefully no one's
using it anymore. 15 years after the BSD regex API came out, POSIX
standardized [a much better API][posixre]. It operates on an opaque
`regex_t` object, on which all state is stored. There's no global
state.

~~~c
int    regcomp(regex_t *preg, const char *regex, int cflags);
int    regexec(const regex_t *preg, const char *string, ...);
size_t regerror(int errcode, const regex_t *preg, ...);
void   regfree(regex_t *preg);
~~~

This is what a good API looks like.

### Getopt

POSIX defines a C API called Getopt for parsing command line
arguments. It's a single function that operates on the `argc` and
`argv` values provided to `main()`. An option string specifies which
options are valid and whether or not they require an argument. Typical
use looks like this,

~~~c
int main(int argc, char **argv)
{
    int option;
    while ((option = getopt(argc, argv, "ab:c:d")) != -1) {
        switch (option) {
            case 'a':
            /* ... */
        }
    }
    /* ... */
    return 0;
}
~~~

The `b` and `c` options require an argument, indicated by the colons.
When encountered, this argument is passed through a global variable
`optarg`. There are four external global variables in total.

~~~c
extern char *optarg;
extern int optind, opterr, optopt;
~~~

If an invalid option is found, `getopt()` will automatically print a
locale-specific error message and return `?`. The `opterr` variable
can be used to disable this message and the `optopt` variable is used
to get the actual invalid option character.

The `optind` variable keeps track of Getopt's progress. It slides
along `argv` as each option is processed. In a minimal, strictly
POSIX-compliant Getopt, this is all the global state required.

The `argc` value in `main()`, and therefore the same parameter in
`getopt()`, is completely redundant and serves no real purpose. Just
like the C strings it points to, the `argv` vector is guaranteed to be
NULL-terminated. At best it's a premature optimization.

#### Threading an Reentrancy

The most immediate problem is that the entire program can only parse
one argument vector at a time. It's not thread-safe. This leaves out
the possibility of parsing argument vectors in other threads. For
example, if the program is a server that exposes a shell-like
interface to remote users, and multiple threads are used to handle
those requests, it won't be able to take advantage of Getopt.

The second problem is that, even in a single-threaded application, the
program can't pause to parse a different argument vector before
returning. It's not reentrant. For example, suppose one of the
arguments to the program is a string containing more arguments to be
parsed for some subsystem.

    #  -s    Provide a set of sub-options to pass to XXX.
    $ myprogram -s "-a -b -c foo"

In theory, the value of `optind` could be saved and restored. However,
this isn't portable. POSIX doesn't explicitly declare that the entire
state is captured by `optind`, nor is it required to be.
Implementations are allowed to have internal, hidden global state.
This has implications in resetting Getopt.

#### Resetting Getopt

In a minimal, strict Getopt, resetting Getopt for parsing another
argument vector is just a matter of setting `optind` to back to its
original value of 1. However, this idiom isn't portable, and POSIX
provides no portable method for resetting the global parser state.

Real implementations of Getopt go beyond POSIX. Probably the most
popular extra feature is option grouping. Typically, multiple options
can be grouped into a single argument, so long as only the final
option requires an argument.

    $ myprogram -adb foo

After processing `a`, `optind` cannot be incremented, because it's
still working on the first argument. This means there's another
internal counter for stepping across the group. In glibc this is
called `nextchar`. Setting `optind` to 1 will not reset this internal
counter, nor would it be detectable by Getopt if it was already set
to 1. The glibc way to reset Getopt is to set `optind` to 0, which is
otherwise an invalid value. Some other Getopt implementations follow
this idiom, but it's not entirely portable.

Not only does Getopt have nasty global state, the user has no way to
reliably control it!

#### Error Printing

I mentioned that Getopt will automatically print an error message
unless disabled with `opterr`. There's no way to get at this error
message, should you want to redirect it somewhere else. It's more
hidden, internal state. You could write your own message, but you'd
lose out on the automatic locale support.

#### Fixing Getopt

The way Getopt *should* have been designed was to accept a context
argument and store all state on that context. Following other POSIX
APIs (pthreads, regex), the context itself would be an opaque object.
In typical use it would have automatic (i.e. stack) duration. The
context would either be zero initialized or a function would be
provided to initialize it. It might look something like this (in the
zero-initialized case).

~~~c
int getopt(getopt_t *ctx, char **argv, const chat *optstring);
~~~

Instead of `optarg` and `optopt` global variables, these values would
be obtained by interrogating the context. The same applies for
`optind` and the diagnostic message.

~~~c
const char *getopt_optarg(getopt_t *ctx);
int         getopt_optopt(getopt_t *ctx);
int         getopt_optind(getopt_t *ctx);
const char *getopt_opterr(getopt_t *ctx);
~~~

Alternatively, instead of `getopt_optind()` the API could have a
function that continues processing, but returns non-option arguments
instead of options. It would return NULL when no more arguments are
left. This is the API I'd prefer, because it would allow for argument
permutation (allow options to come after non-options, per GNU Getopt)
without actually modifying the argument vector. This common extension
to Getopt could be added cleanly. The real Getopt isn't designed well
for extension.

~~~c
const char *getopt_next_arg(getopt_t *ctx);
~~~

This API eliminates the global state and, as a result, solves *all* of
the problems listed above. It's essentially the same API defined by
[Popt][popt] and my own embeddable [Optparse][optparse]. They're much
better options if the limitations of POSIX-style Getopt are an issue.


[optparse]: https://github.com/skeeto/optparse
[popt]: http://linux.die.net/man/3/popt
[bsdre]: http://man7.org/linux/man-pages/man3/re_comp.3.html
[posixre]: http://man7.org/linux/man-pages/man3/regcomp.3.html
