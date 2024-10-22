---
title: Everything you never wanted to know about Win32 environment blocks
layout: post
date: 2023-08-23T21:51:10Z
tags: [c, win32]
uuid: 3e73a0bb-fc27-4da2-9ae9-fab773a759d0
---

In an effort to avoid [programming by superstition][ss], I did a deep dive
into the Win32 "environment block," the data structure holding a process's
environment variables, in order to better understand it. Along the way I
discovered implied and undocumented behaviors. (The *environment block*
must not to be confused with the [Process Environment Block][peb] (PEB)
which is different.) Because I cannot possibly retain all the quirky
details in my head for long, I'm writing them down for future reference. I
ran my tests on different Windows versions as far back as Windows XP SP3
in order to fill in gaps where documentation is ambiguous, incomplete, or
wrong. Overall conclusion: Correct, direct manipulation of an environment
block is impossible *in the general case* due to under-specified and
incorrect documentation. This has important consequences mainly for
programming language runtimes.

Win32 has two interfaces for interacting with environment variables:

1. [GetEnvironmentVariable][get] and [SetEnvironmentVariable][set]
2. [GetEnvironmentStrings][str] and [FreeEnvironmentStrings][free]

The first, which I'll call get/set, is the easy interface, with Windows
doing all the searching and sorting on your behalf. It's also the only
supported interface through which a process can manipulate its own
variables. It has no function for enumerating variables.

The second, which I'll call get/free, allocates a *copy of* the
environment block. Calls to get/set does not modify existing copies.
Similarly, manipulating this block has no effect on the environment as
viewed through get/set. In other words, it's *read only*. We can enumerate
our environment variables by walking the environment block. As I will
discuss below, enumeration is it's only consistently useful purpose!

Technically it's possible to access the actual environment block through
undocumented fields in the PEB. It's the same content as returned by
get/free except that it's not a copy. It cannot be accessed safely, so I'm
ignoring this route.

The environment block format is a null-terminated block of null-terminated
strings:

    keyA=a\0keyBB=bb\0keyCCC=ccc\0\0

Each string ~~begins with a character other than `=` and~~ contains at
least one `=`. In my tests this rule was strictly enforced by Windows, and
I could not construct an environment block that broke this rule. This list
is usually, but not always, sorted. It may contain repeated variables, but
they're always assigned the same value, which is also strictly enforced by
Windows.

~~The get/free interface has no "set" function, and a process cannot set
its own environment block to a custom buffer.~~ (Update: Stefan Kanthak
points out [SetEnvironmentStringsW][sesw]. I missed it because it was only
officially documented a few months before this article was written.) There
*is* one interface where a process gets to provide a raw environment
block: [CreateProcess][cp]. That is, a parent can construct one for its
children.

```c
    wchar_t env[] = L"HOME=C:\\Users\\me\0PATH=C:\\bin;C:\\Windows\0";
    CreateProcessW(L"example.exe", ..., env, ...);
```

Windows imposes some rules upon this environment block:

* ~~If an element begins with `=` or does not contain `=`, CreateProcess
  fails.~~

* Repeated variables are modified to match the first instance. If you're
  potentially overriding using a duplicate, put the override first.

* Some cases of bad formatting become memory access violations.

As usual for Win32, there are [no rules against ill-formed UTF-16][wtf8],
and I could [always pass][wild] such "UTF-16" through into the child
environment block. Keep that in mind even when using the get/set
interface.

The SetEnvironmentVariable documentation gives a maximum variable size:

> The maximum size of a user-defined environment variable is 32,767
> characters. There is no technical limitation on the size of the
> environment block.

At least on more recent versions of Windows, my experiments proved exactly
the opposite. There is no limit on a user-defined environment variables,
but environment blocks are limited to 2GiB, for both 32-bit and 64-bit
processes. I could even create such huge environments in [large address
aware][laa] 32-bit processes, though the interfaces are prone to error due
to allocations problems.

There's one special case where CreateProcess is illogical, and it's
certainly a case of confusion within its implementation. **An environment
block is not allowed to be empty.** An empty environment is represented as
a block containing one empty (zero length) element. That is, two null
terminators in a row. It's the one case where an environment block may
contain an element without a `=`. The *logical* empty environment block
would be just one null terminator, to terminate the block itself, because
it contains no variables. You can safely pretend that's the case when
parsing an environment block, as this special case is superfluous.

However, CreateProcess partially enforces this silly, unnecessary special
case! If an environment block begins with a null terminator, the next
character *must be in a mapped memory region* because it will read this
character. If it's not mapped, the result is a memory access violation.
Its actual value doesn't matter, and CreateProcess will treat it as though
it was another null terminator. Surely someone at Microsoft would have
noticed by now that this behavior makes no sense, but I guess it's kept
for backwards compatibility?

The CreateProcess documentation says that "the system uses a sorted
environment" but this made no difference in my tests. The word "must"
appears in this sentence, but it's unclear if it applies to sorting, or
even outside the special case being discussed. GetEnvironmentVariable
works fine on an unsorted environment block. SetEnvironmentVariable
maintains sorting, but given an unsorted block it goes somewhere in the
middle, probably wherever a bisection happens to land. Perhaps look-ups in
sorted blocks are faster, but environment blocks are so small — ~~a
maximum of 32K characters~~ (Update: only true for ANSI) — that, in
practice, it really does not matter.

Suppose you're meticulous and want to sort your environment block before
spawning a process. How do you go about it? There's the rub: The official
documentation is incomplete! The [Changing Environment Variables][env]
page says:

> All strings in the environment block must be sorted alphabetically by
> name. The sort is case-insensitive, Unicode order, without regard to
> locale.

What do they mean by "case-insensitive" sort? Does "Unicode order" mean
[case folding][fold]? A reasonable guess, but no, that's not how get/set
works. Besides, how does "Unicode order" apply to ill-formed UTF-16?
Worse, get/set sorting is certainly not "Unicode order" even outside of
case-insensitivity! For example, `U+1F31E` (SUN WITH FACE) sorts ahead of
`U+FF01` (FULLWIDTH EXCLAMATION MARK) because the former encodes in UTF-16
as `U+D83C U+DF1E`. Maybe it's case-insensitive only in ASCII? Nope, π
(`U+03C0`) and Π (`U+03A0`) are considered identical. Windows uses some
kind of case-insensitive, but not case-*folded*, undocumented early 1990s
UCS-2 sorting logic for environment variables.

**Update**: John Doty [suspects][doty] the [RtlCompareUnicodeString][rtl]
function for sorting. It [lines up perfectly with get/set][experiment] for
all possible inputs.

Without better guidance, the only reliable way to "correctly" sort an
environment block is to build it with get/set, then retrieve the result
with get/free. The algorithm looks like:

1. Get a copy of the environment with GetEnvironmentStrings.
2. Walk the environment and call SetEnvironmentVariable on each name with
   a null pointer as the value. This clears out the environment.
3. Call SetEnvironmentVariable for each variable in the new environment.
4. Get a sorted copy of the new environment with GetEnvironmentStrings.

Unfortunately that's all global state, so you can only construct one new
environment block at a time.

If you know all your variable names ahead of time, then none of this is a
problem. Determine what Windows thinks the order should be, then use that
in your program when constructing the environment block. It's the *general
case* where this is a challenge, such as a language runtime designed to
operate on arbitrary environment variables with behavior congruent to the
rest of the system.

There are similar issues with looking up variables in an environment
block. How does case-insensitivity work? Sorting is "without regard to
locale" but what about when comparing variable names? The documentation
doesn't say. When enumerating variables using get/free, you might read
what get/set considers to be duplicates, though at least values will
always agree with get/set, i.e. they're aliases of one variables. Windows
maintains that invariant in my tests. The above algorithm would also
delete these duplicates.

For example, if someone passed you a "dirty" environment with duplicates,
or that was unsorted, this would clean it up in a way that allows get/free
to be traversed in order without duplicates.

```c
    wchar_t *env = GetEnvironmentStringsW();

    // Clear out the environment
    for (wchar_t *var = env; *var;) {
        size_t len = wcslen(var);
        size_t split = wcscspn(var, L"=");
        var[split] = 0;
        SetEnvironmentVariableW(var, 0);
        var[split] = '=';
        var += len + 1;
    }

    // Restore the original variables
    for (wchar_t *var = env; *var;) {
        size_t len = wcslen(var);
        size_t split = wcscspn(var, L"=");
        var[split] = 0;
        SetEnvironmentVariableW(var, var+split+1);
        var += len + 1;
    }

    FreeEnvironmentStringsW(env);
```

On the second pass, SetEnvironmentVariableW will gobble up all the
duplicates.

As a final note, the CreateProcess page had said this [up until February
2023][feb] about the environment block parameter:

> If this parameter is `NULL` and the environment block of the parent
> process contains Unicode characters, you must also ensure that
> `dwCreationFlags` includes `CREATE_UNICODE_ENVIRONMENT`.

That seems to indicate it's virtually always wrong to call CreateProcess
without that flag — that is, Windows will trash the child's environment
unless this flag is passed — which is a bonkers default. Fortunately this
appears to be wrong, which is probably why the documentation was finally
corrected (after several decades). Omitting this flag was fine under all
my tests, and I was unable to produce surprising behavior on any system.

In summary:

* Prefer get/set for all operations except enumeration
* Environment blocks are not necessarily sorted
* Repeat variables are forced to the value of the first instance
* Variables may contain ill-formed UTF-16
* Empty environment blocks have a superfluous special case
* ~~Entries cannot begin with `=`~~
* Entries must contain at least one `=`
* Sort order is ambiguous, so you cannot reliably do it yourself
* Case-insensitivity of names is ambiguous, so rely on get/set
* `CREATE_UNICODE_ENVIRONMENT` necessary only for non-null environment

**Update September 2024**: Correction from Kasper Brandt [regarding
variables beginning with `=`][eq]. I misunderstood how it was parsed and
came to the wrong conclusion.


[cp]: https://learn.microsoft.com/en-us/windows/win32/api/processthreadsapi/nf-processthreadsapi-createprocessw
[doty]: https://lists.sr.ht/~skeeto/public-inbox/%3Cc2a4c4d7-95cc-48a4-8047-c79b55eba261%40app.fastmail.com%3E
[env]: https://learn.microsoft.com/en-us/windows/win32/procthread/changing-environment-variables
[eq]: https://lists.sr.ht/~skeeto/public-inbox/%3C098b0421-af0e-46fb-8921-2a4e76f5a361@app.fastmail.com%3E
[experiment]: https://github.com/skeeto/scratch/blob/master/misc/envsort.c
[feb]: https://web.archive.org/web/20180110151515/http://msdn.microsoft.com/en-us/library/ms682425(VS.85).aspx
[fold]: https://www.unicode.org/Public/15.0.0/ucd/CaseFolding.txt
[free]: https://learn.microsoft.com/en-us/windows/win32/api/processenv/nf-processenv-freeenvironmentstringsw
[get]: https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-getenvironmentvariable
[laa]: https://learn.microsoft.com/en-us/cpp/build/reference/largeaddressaware-handle-large-addresses
[peb]: https://www.geoffchappell.com/studies/windows/km/ntoskrnl/inc/api/pebteb/peb/index.htm
[rtl]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdm/nf-wdm-rtlcompareunicodestring
[sesw]: https://learn.microsoft.com/en-us/windows/win32/api/processenv/nf-processenv-setenvironmentstringsw
[set]: https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-setenvironmentvariable
[ss]: https://utcc.utoronto.ca/~cks/space/blog/programming/ProgrammingViaSuperstition
[str]: https://learn.microsoft.com/en-us/windows/win32/api/processenv/nf-processenv-getenvironmentstringsw
[wild]: /blog/2022/02/18/
[wtf8]: https://simonsapin.github.io/wtf-8/
