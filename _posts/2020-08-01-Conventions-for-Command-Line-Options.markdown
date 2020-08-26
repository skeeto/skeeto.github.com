---
title: Conventions for Command Line Options
layout: post
date: 2020-08-01T00:34:23Z
tags: [tutorial, posix, c, python, go]
uuid: 9be2ce0e-298e-4085-8789-49674aecfeeb
excerpt_separator: <!--more-->
---

*This article was discussed [on Hacker News][hn] and critiqued [on
Wandering Thoughts][wt] ([2][wt2], [3][wt3]).*

Command line interfaces have varied throughout their brief history but
have largely converged to some common, sound conventions. The core
[originates from unix][util], and the Linux ecosystem extended it,
particularly via the GNU project. Unfortunately some tools initially
*appear* to follow the conventions, but subtly get them wrong, usually
for no practical benefit. I believe in many cases the authors simply
didn't know any better, so I'd like to review the conventions.

<!--more-->

### Short Options

The simplest case is the *short option* flag. An option is a hyphen —
specifically HYPHEN-MINUS U+002D — followed by one alphanumeric
character. Capital letters are acceptable. The letters themselves [have
conventional meanings][catb] and are worth following if possible.

```
program -a -b -c
```

Flags can be grouped together into one program argument. This is both
convenient and unambiguous. It's also one of those often missed details
when programs use hand-coded argument parsers, and the lack of support
irritates me.

```
program -abc
program -acb
```

The next simplest case are short options that take arguments. The
argument follows the option.

```
program -i input.txt -o output.txt
```

The space is optional, so the option and argument can be packed together
into one program argument. Since the argument is required, this is still
unambiguous. This is another often-missed feature in hand-coded parsers.

```
program -iinput.txt -ooutput.txt
```

This does not prohibit grouping. When grouped, the option accepting an
argument must be last.

```
program -abco output.txt
program -abcooutput.txt
```

This technique is used to create another category, *optional option
arguments*. The option's argument can be optional but still unambiguous
so long as the space is always omitted when the argument is present.

```
program -c       # omitted
program -cblue   # provided
program -c blue  # omitted (blue is a new argument)

program -c -x   # two separate flags
program -c-x    # -c with argument "-x"
```

Optional option arguments should be used judiciously since they can be
surprising, but they have their uses.

Options can typically appear in any order — something parsers often
achieve via *permutation* — but non-options typically follow options.

```
program -a -b foo bar
program -b -a foo bar
```

GNU-style programs usually allow options and non-options to be mixed,
though I don't consider this to be essential.

```
program -a foo -b bar
program foo -a -b bar
program foo bar -a -b
```

If a non-option looks like an option because it starts with a hyphen,
use `--` to demarcate options from non-options.

```
program -a -b -- -x foo bar
```

An advantage of requiring that non-options follow options is that the
first non-option demarcates the two groups, so `--` is less often
needed.

```
# note: without argument permutation
program -a -b foo -x bar  # 2 options, 3 non-options
```

### Long options

Since short options can be cryptic, and there are such a limited number
of them, more complex programs support long options. A long option
starts with two hyphens followed by one or more alphanumeric, lowercase
words. Hyphens separate words. Using two hyphens prevents long options
from being confused for grouped short options.

```
program --reverse --ignore-backups
```

Occasionally flags are paired with a mutually exclusive inverse flag
that begins with `--no-`. This avoids a future *flag day* where the
default is changed in the release that also adds the flag implementing
the original behavior.

```
program --sort
program --no-sort
```

Long options can similarly accept arguments.

```
program --output output.txt --block-size 1024
```

These may optionally be connected to the argument with an equals sign
`=`, much like omitting the space for a short option argument.

```
program --output=output.txt --block-size=1024
```

Like before, this opens up the doors for optional option arguments. Due
to the required `=` this is still unambiguous.

```
program --color --reverse
program --color=never --reverse
```

The `--` retains its original behavior of disambiguating option-like
non-option arguments:

```
program --reverse -- --foo bar
```

### Subcommands

Some programs, such as Git, have subcommands each with their own
options. The main program itself may still have its own options distinct
from subcommand options. The program's options come before the
subcommand and subcommand options follow the subcommand. Options are
never permuted around the subcommand.

```
program -a -b -c subcommand -x -y -z
program -abc subcommand -xyz
```

Above, the `-a`, `-b`, and `-c` options are for `program`, and the
others are for `subcommand`. So, really, the subcommand is another
command line of its own.

### Option parsing libraries

There's little excuse for not getting these conventions right assuming
you're interested in following the conventions. Short options can be
parsed correctly in [just ~60 lines of C code][getopt]. Long options are
[just slightly more complex][long].

GNU's `getopt_long()` supports long option abbreviation — with no way to
disable it (!) — but [this should be avoided][cw].

Go's [flag package][flag] intentionally deviates from the conventions.
It only supports long option semantics, via a single hyphen. This makes
it impossible to support grouping even if all options are only one
letter. Also, the only way to combine option and argument into a single
command line argument is with `=`. It's sound, but I miss both features
every time I write programs in Go. That's why I [wrote my own argument
parser][go]. Not only does it have a nicer feature set, I like the API a
lot more, too.

Python's primary option parsing library is `argparse`, and I just can't
stand it. Despite appearing to follow convention, it actually breaks
convention *and* its behavior is unsound. For instance, the following
program has two options, `--foo` and `--bar`. The `--foo` option accepts
an optional argument, and the `--bar` option is a simple flag.

```py
import argparse
import sys

parser = argparse.ArgumentParser()
parser.add_argument('--foo', type=str, nargs='?', default='X')
parser.add_argument('--bar', action='store_true')
print(parser.parse_args(sys.argv[1:]))
```

Here are some example runs:

```
$ python parse.py
Namespace(bar=False, foo='X')

$ python parse.py --foo
Namespace(bar=False, foo=None)

$ python parse.py --foo=arg
Namespace(bar=False, foo='arg')

$ python parse.py --bar --foo
Namespace(bar=True, foo=None)

$ python parse.py --foo arg
Namespace(bar=False, foo='arg')
```

Everything looks good except the last. If the `--foo` argument is
optional then why did it consume `arg`? What happens if I follow it with
`--bar`? Will it consume it as the argument?

```
$ python parse.py --foo --bar
Namespace(bar=True, foo=None)
```

Nope! Unlike `arg`, it left `--bar` alone, so instead of following the
unambiguous conventions, it has its own ambiguous semantics and attempts
to remedy them with a "smart" heuristic: "If an optional argument *looks
like* an option, then it must be an option!" Non-option arguments can
never follow an option with an optional argument, which makes that
feature pretty useless. Since `argparse` does not properly support `--`,
that does not help.

```
$ python parse.py --foo -- arg
usage: parse.py [-h] [--foo [FOO]] [--bar]
parse.py: error: unrecognized arguments: -- arg
```

Please, stick to the conventions unless you have *really* good reasons
to break them!


[catb]: http://www.catb.org/~esr/writings/taoup/html/ch10s05.html
[cw]: https://utcc.utoronto.ca/~cks/space/blog/python/ArgparseAbbreviatedOptions
[flag]: https://golang.org/pkg/flag/
[getopt]: https://github.com/skeeto/getopt
[go]: https://github.com/skeeto/optparse-go
[hn]: https://news.ycombinator.com/item?id=24020952
[long]: https://github.com/skeeto/optparse
[util]: https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap12.html
[wt]: https://utcc.utoronto.ca/~cks/space/blog/unix/MyOptionsConventions
[wt2]: https://utcc.utoronto.ca/~cks/space/blog/unix/UnixOptionsConventions
[wt3]: https://utcc.utoronto.ca/~cks/space/blog/python/ArgparseSomeUnixNotes
