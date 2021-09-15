---
title: "OpenBSD's pledge and unveil from Python"
layout: post
date: 2021-09-15T02:46:56Z
tags: [bsd, c, python]
uuid: cd3857dd-270c-430e-824d-6512688687a3
excerpt_separator: <!--more-->
---

*This article was discussed [on Hacker News][hn].*

Years ago, OpenBSD gained two new security system calls, [`pledge(2)`][p]
(originally [`tame(2)`][t]) and [`unveil`][u]. In both, an application
surrenders capabilities at run-time. The idea is to perform initialization
like usual, then drop capabilities before handling untrusted input,
limiting unwanted side effects. This feature is applicable even where type
safety isn't an issue, such as Python, where a program might still get
tricked into accessing sensitive files or making network connections when
it shouldn't. So how can a Python program access these system calls?

As [discussed previously][dll], it's quite easy to access C APIs from
Python through its [`ctypes`][ctypes] package, and this is no exception.
In this article I show how to do it. Here's the full source if you want to
dive in: [**`openbsd.py`**][src].

<!--more-->

I've chosen these extra constraints:

* As extra safety features, unnecessary for correctness, attempts to call
  these functions on systems where they don't exist will silently do
  nothing, as though they succeeded. They're provided as a best effort.

* Systems other than OpenBSD may support these functions, now or in the
  future, and it would be nice to automatically make use of them when
  available. This means no checking for OpenBSD specifically but instead
  *feature sniffing* for their presence.

* The interfaces should be Pythonic as though they were implemented in
  Python itself. Raise exceptions for errors, and accept strings since
  they're more convenient than bytes.

For reference, here are the function prototypes:

```c
int pledge(const char *promises, const char *execpromises);
int unveil(const char *path, const char *permissions);
```

The [string-oriented interface of `pledge`][str] will make this a whole
lot easier to implement.

### Finding the functions

The first step is to grab functions through `ctypes`. Like a lot of Python
documentation, this area is frustratingly imprecise and under-documented.
I want to grab a handle to the already-linked libc and search for either
function. However, getting that handle is a little different on each
platform, and in the process I saw four different exceptions, only one of
which is documented.

I came up with passing None to `ctypes.CDLL`, which ultimately just passes
`NULL` to [`dlopen(3)`][dlopen]. That's really all I wanted. Currently on
Windows this is a TypeError. Once the handle is in hand, try to access the
`pledge` attribute, which will fail with AttributeError if it doesn't
exist. In the event of any exception, just assume the behavior isn't
available. If found, I also define the function prototype for `ctypes`.

```py
_pledge = None
try:
    _pledge = ctypes.CDLL(None, use_errno=True).pledge
    _pledge.restype = ctypes.c_int
    _pledge.argtypes = ctypes.c_char_p, ctypes.c_char_p
except Exception:
    _pledge = None
```

Catching a broad Exception isn't great, but it's the best we can do since
the documentation is incomplete. From this block I've seen TypeError,
AttributeError, FileNotFoundError, and OSError. I wouldn't be surprised if
there are more possibilities, and I don't want to risk missing them.

Note that I'm catching Exception rather than using a bare `except`. My
code will not catch KeyboardInterrupt nor SystemExit. This is deliberate,
and I never want to catch these.

The same story for `unveil`:

```py
_unveil = None
try:
    _unveil = ctypes.CDLL(None, use_errno=True).unveil
    _unveil.restype = ctypes.c_int
    _unveil.argtypes = ctypes.c_char_p, ctypes.c_char_p
except Exception:
    _unveil = None
```

### Pythonic wrappers

The next and final step is to wrap the low-level call in an interface that
hides their C and `ctypes` nature.

Python strings must be encoded to bytes before they can be passed to C
functions. Rather than make the caller worry about this, we'll let them
pass friendly strings and have the wrapper do the conversion. Either may
also be `NULL`, so None is allowed.

```py
def pledge(promises: Optional[str], execpromises: Optional[str]):
    if not _pledge:
        return  # unimplemented

    r = _pledge(None if promises is None else promises.encode(),
                None if execpromises is None else execpromises.encode())
    if r == -1:
        errno = ctypes.get_errno()
        raise OSError(errno, os.strerror(errno))
```

As usual, a return of -1 means there was an error, in which case we fetch
`errno` and raise the appropriate OSError.

`unveil` works a little differently since the first argument is a path.
Python functions that accept paths, such as `open`, generally accept
either strings or bytes. On unix-like systems, [paths are fundamentally
bytestrings][wtf] and not necessarily Unicode, so it's necessary to accept
bytes. Since strings are nearly always more convenient, they take both.
The `unveil` wrapper here will do the same. If it's a string, encode it,
otherwise pass it straight through.

```py
def unveil(path: Union[str, bytes, None], permissions: Optional[str]):
    if not _unveil:
        return  # unimplemented

    r = _unveil(path.encode() if isinstance(path, str) else path,
                None if permissions is None else permissions.encode())
    if r == -1:
        errno = ctypes.get_errno()
        raise OSError(errno, os.strerror(errno))
```

That's it!

### Trying it out

Let's start with `unveil`. Initially a process has access to the whole
file system with the usual restrictions. On the first call to `unveil`
it's immediately restricted to some subset of the tree. Each call reveals
a little more until a final `NULL` which locks it in place for the rest of
the process's existence.

Suppose a program has been tricked into accessing your shell history,
perhaps by mishandling a path:

```py
def hackme():
    try:
        with open(pathlib.Path.home() / ".bash_history"):
            print("You've been hacked!")
    except FileNotFoundError:
        print("Blocked by unveil.")

hackme()
```

If you're a Bash user, this prints:

    You've been hacked!

Using our new feature to restrict the program's access first:

```py
# restrict access to static program data
unveil("/usr/share", "r")
unveil(None, None)

hackme()
```

On OpenBSD this now prints:

    Blocked by unveil.

Working just as it should!

With `pledge` we declare what abilities we'd like to keep by supplying a
list of promises, *pledging* to use only those abilities afterward. A
common case is the `stdio` promise which allows reading and writing of
open files, but not *opening* files. A program might open its log file,
then drop the ability to open files while retaining the ability to write
to its log.

An invalid or unknown promise is an error. Does that work?

    >>> pledge("doesntexist", None)
    OSError: [Errno 22] Invalid argument

So far so good. How about the functionality itself?

```py
pledge("stdio", None)
hackme()
```

The program is instantly killed when making the disallowed system call:

    Abort trap (core dumped)

If you want something a little softer, include the `error` promise:

```py
pledge("stdio error", None)
hackme()
```

Instead it's an exception, which will be a lot easier to debug when it
comes to Python, so you probably always want to use it.

    OSError: [Errno 78] Function not implemented

The core dump isn't going to be much help to a Python program, so you
probably always want to use this promise. In general you need to be extra
careful about `pledge` in complex runtimes like Python's which may
reasonably need to do many arbitrary, undocumented things at any time.


[ctypes]: https://docs.python.org/3/library/ctypes.html
[dll]: /blog/2021/06/29/
[dlopen]: https://man.openbsd.org/dlopen.3
[hn]: https://news.ycombinator.com/item?id=28535255
[p]: https://man.openbsd.org/pledge.2
[src]: https://github.com/skeeto/scratch/tree/master/misc/openbsd.py
[str]: https://flak.tedunangst.com/post/string-interfaces
[t]: https://www.openbsd.org/papers/tame-fsec2015/mgp00001.html
[u]: https://man.openbsd.org/unveil.2
[wtf]: https://simonsapin.github.io/wtf-8/
