---
title: SDL2 common mistakes and how to avoid them
layout: post
date: 2023-01-08T02:09:26Z
tags: [c, win32]
uuid: 5b345c81-80d1-4459-981f-b5826a2bb8e7
excerpt_separator: <!--more-->
---

*This article was discussed [on reddit][r].*

[SDL][] has grown on me over the past year. I didn't understand its value
until viewing it in the right lens: as a complete platform and runtime
replacing the host's runtime, possibly including libc. Ideally an SDL
application links exclusively against SDL and otherwise not directly
against host libraries, though in practice it's somewhat porous. With care
— particularly in avoiding mistakes covered in this article — that ideal
is quite achievable for C applications that fit within SDL's feature set.

<!--more-->

SDL applications are always interesting one way or another, so I like to
dig in when I come across them. The items in this article are mistakes
I've either made myself or observed across many such passion projects in
the wild.

### Mistake 1: Not using `sdl2-config`

This shell script comes with SDL2 and smooths over differences between
platforms, even when cross compiling. It informs your compiler where to
find and how to link SDL2. The script even works on Windows if you have a
unix shell, such as via [w64devkit][]. Use it as a command substitution at
the end of the build command, particularly when using `--libs`. A one-shot
or [unity build][] (my preference) looks like so:

    $ cc app.c $(sdl2-config --cflags --libs)

Or under separate compilation:

    $ cc -c app.c $(sdl2-config --cflags)
    $ cc app.o $(sdl2-config --libs)

Alternatively, static link by replacing `--libs` with `--static-libs`,
though this is discouraged by the SDL project. When dynamically linked,
users can, and do, trivially substitute a different SDL2 binary, such as
one patched for their system. In my experience, static linking works
reliably on Windows but poorly on Linux.

Alternatively, use the general purpose `pkg-config`. Don't forget `eval`!

    $ eval cc app.c $(pkg-config sdl2 --cflags --libs)

I wrote [a pkg-config for Windows][u-config] specifically for this case.

Caveats:

* Some circumstances require special treatment, and `sdl2-config` may be
  too blunt a tool. That's fine, but generally prefer `sdl2-config` as the
  default approach.

* `sdl2-config` does not support extensions such as `SDL2_image`, so you
  will need to use `pkg-config`. Personally I don't think they're worth
  the trouble when there's [stb][], or [QOI instead of PNG][qoi].

* There's an alternative build option using CMake, without any use of
  `sdl2-config`, but I won't discuss it here.

### Mistake 2: Including `SDL2/SDL.h`

A lot of examples, including tutorials linked from the official SDL
website, have `SDL2/` in their include paths. That's because they're
making mistake 1, not using `sdl2-config`, and are instead relying on
Linux distributions having installed SDL2 in a place *coincidentally*
accessible through that include path.

This is annoying when SDL2 *not* installed there, or if I don't want it
using the system's SDL2. Worse, it can result in subtly broken builds as
it mixes and matches different SDL installations. The correct SDL2 include
is the following:

```c
#include "SDL.h"
```

Note the quotes, which helps prevent picking up an arbitrary system header
by accident. When carefully and narrowly targeting SDL-the-platform, this
will be the only "system" include anywhere in your application.

### Mistake 3: Not surrendering `main`

A conventional SDL application has a `main` function defined in its
source, but despite the name, this is distinct from C `main`. To smooth
over [platform differences][wild], SDL may rename the application's `main`
to `SDL_main` and substitute its own C `main`. Because of this, `main`
must have the conventional `argc`/`argv` prototype and must return a
value. (As a special case, C permits `main` to implicitly `return 0`, so
it's an easy mistake to make.)

With this in mind, the bare minimum SDL2 application:

```c
#include "SDL.h"

int main(int argc, char **argv)
{
    return 0;
}
```

Caveat: Like with `sdl2-config`, some special circumstances require
control over the application entry point — see `SDL_MAIN_HANDLED` and
`SDL_SetMainReady` — but that should be reserved until there's a need.

One such special case is avoiding linking a CRT on Windows. In principle
it's this simple:

```c
#include "SDL.h"

int WinMainCRTStartup(void)
{
    SDL_SetMainReady();
    // ...
    return 0;
}
```

Then it's [the usual compiler and linker flags][fs]:

    $ cc -nostdlib -o app.exe app.c $(sdl2-config --cflags --libs)

This will create a tiny `.exe` that doesn't link any system DLL, just
`SDL2.dll`. Quite platform agnostic indeed!

    $ objdump -p app.exe | grep -Fi .dll
            DLL Name: SDL2.dll

Alas, as of this writing, this does not work reliably. SDL2's accelerated
renderers on Windows do not clean up properly in `SDL_QuitSubSystem` nor
`SDL_Quit`, so the process cannot exit without calling ExitProcess in
`kernel32.dll` (or similar). This is still an open experiment.

### Mistake 4: Using the SDL wiki for API documentation

The [SDL wiki][] is not authoritative documentation, merely a *convenient*
web-linkable — and downloadable (see "offline html") — information source.
However, anyone who's spent time on it can tell you it's incomplete. The
authoritative API documentation is *the SDL headers*, which fortunately
are already on hand for building SDL applications. The SDL maintainers
[themselves use the headers, not the wiki][sdlamp].

If, like me, you're using [ctags][], this is actually good news! With a
bit of configuration, you can jump to any bit of SDL documentation at any
time in your editor, treating the SDL headers like a hyperlinked wiki
built into your editor. Just like building, `sdl2-config` can tell ctags
where find those headers:

    $ ctags -a -R --kinds-c=dept $(sdl2-config --prefix)/include/SDL2

I'm using `-a` (`--append`) to append to the tags file I've already
generated for my own program, `-R` (`--recurse`) to automatically find all
the headers, and `--kinds-c=dept` capture exactly the kinds of symbols I
care about — `#define`, `enum`, prototypes, `typedef` — no more no less.

In Vim I `CTRL-]` over any SDL symbol to jump to its documentation, and
then I can use it again within its documentation comment to jump further
still to any symbols it mentions, then finally use the jump or tag stack
to return. As long as I have `t` in [`'complete'`][cpt] (`'cpt'`), which
is the default, I can also "tab"-complete any SDL symbol using the tags
table. There are a few rough edges here and there, but overall it's a
solid editing paradigm.

By the way, with `sdl2-config` in your `$PATH`, all the above works out of
the box in w64devkit! That's where I've mostly been working with SDL.

### Mistake 5: Using stdio streams

A common bit of code in real SDL programs and virtually every tutorial:

```c
if (SDL_Init(...)) {
    fprintf(stderr, "SDL_Init(): %s\n", SDL_GetError());
    return 1;
}
```

This is not ideal:

* `fprintf` is not part of the SDL platform. This is going behind SDL's
  back, reaching around the abstraction to a different platform. Strictly
  speaking, this API may not even be available to an SDL application.

* SDL applications are graphical, so `stderr` is likely disconnected from
  anything useful. Few would ever see this message.

Fortunately SDL provides two alternatives:

* `SDL_Log`: like C `printf`, but SDL will strive to connect it to
  somewhere useful. If the application was launched from a terminal or
  console, SDL will find it and hook it up to the logger. On Windows, if
  there's a debugger attached, SDL will use [OutputDebugString][ods] to
  send logs to the debugger.

* `SDL_ShowSimpleMessageBox`: using any means possible, attempt to display
  a message to the user. Like `SDL_Log`, it's safe to use before/without
  initializing SDL subsystems.

If you're paranoid, you could even use both:

```c
if (SDL_Init(...)) {
    SDL_ShowSimpleMessageBox(
        SDL_MESSAGEBOX_ERROR, "SDL_Init()", SDL_GetError(), 0
    );
    SDL_Log("SDL_Init(): %s", SDL_GetError());
    return 1;
}
```

Though note that `SDL_ShowSimpleMessageBox` can fail, which will set a
new, different error message for `SDL_Log`!

There's a similar story again with `fopen` and loading assets. SDL has an
I/O API, `SDL_RWops`. It's probably better than the host's C equivalent,
particularly with regards to paths. If you're not already embedding your
assets, use the SDL API instead.

### Mistake 6: Using `SDL_RENDERER_ACCELERATED`

This flag — and its surrounding bit set, `SDL_RendererFlags` — are a
subtle design flaw in the SDL2 API. Its existence is misleading, causing
to widespread misuse. It does not help that the documentation, both header
and wiki, is incomplete and unclear. The `SDL_CreateRenderer` function
accepts a bit set as its third argument, and it serves two simultaneous
purposes:

* Indicates *mandatory* properties of the renderer. Examples: "must use
  accelerated rendering," "must use software rendering," "must support
  vertical synchronization (vsync)." Drivers without the chosen properties
  are skipped.

* If `SDL_RENDERER_PRESENTVSYNC` is set, also enables vsync in the created
  render.

The common mistake is thinking that this bit indicates preference: "prefer
an accelerated renderer if possible". But it really means "accelerated
renderer or bust."

Given a zero for renderer flags, SDL will first attempt to create an
accelerated renderer. Failing that, it will then attempt to create a
software renderer. A software renderer fallback is exactly the behavior
you want! After all, this fallback is one of the primary features of the
SDL renderer API. This is so straightforward there are no caveats.

### Mistake 7: Not accounting for vsync

For a game, you probably ought to enable vsync in your renderer. The hint:
You're using `SDL_PollEvent` in your main event loop. Otherwise you will
waste lots of resources rendering thousands of frames per second. If my
laptop fan spins up running your SDL application, it's probably because
you didn't do this. The following should be the most conventional SDL
renderer configuration:

```c
r = SDL_CreateRenderer(window, -1, SDL_RENDERER_PRESENTVSYNC);
```

The software renderer supports vsync, so it will not be excluded from the
driver search when vsync is requested.

That's only for SDL renderers. If you're using OpenGL, set a non-zero
`SDL_GL_SetSwapInterval` so that `SDL_GL_SwapWindow` synchronizes. For the
other rendering APIs, consult their documentation. (I can only speak to
SDL and OpenGL from experience.)

Caveat: Beware accidentally relying on vsync for timing in your game. You
don't want your game's physics to depend on the host's display speed. Even
the pros make this mistake from time to time.

However, if you're *not* making a game – perhaps instead an [IMGUI][]
application *without active animations* — there's a good chance you don't
need or want vsync. The hint: You're using `SDL_WaitEvent` in your main
event loop.

In summary, graphical SDL applications fall into one of two cases:

* `SDL_PollEvent` with vsync
* `SDL_WaitEvent` without vsync

### Mistake 8: Using `assert.h` instead of `SDL_assert`

Alright, this one isn't so common, but I'd like to highlight it. **The
`SDL_assert` macro is fantastic**, easily beating `assert.h` which
[doesn't even break in the right place][assert]. It uses SDL to present a
user interface to the assertion, with support for retrying and ignoring.
It also works great under debuggers, breaking exactly as it should. I have
nothing but praise for it, so don't pass up the chance to use it when you
can.

While I'm at it: during developing and testing, *always always always* run
your application under a debugger. Don't close the debugger, just launch
through it again after rebuilding. Also, enable UBSan and ASan when
available for the extra assertions.

### SDL wishlist

For months I had wondered why SDL provides no memory allocation API. I'm
fine if it doesn't have a general purpose allocator since I just want to
grab a chunk of host memory [for an arena][arena]. However, SDL *does*
have allocations functions — `SDL_malloc`, etc. I didn't know about them
until I stopped making mistake 4.

It was the same story again with math functions: I'd like not to stray
from SDL as a platform, but what if I need transcendental functions? I
could whip up crude implementations myself, but I'd prefer not. SDL has
those too: `SDL_sin`, etc. Caveat: The `math.h` functions are built-ins,
and compilers use that information to better optimize programs, e.g. cool
stuff like `-mrecip`, or SIMD vectorization. That cannot be done with
SDL's equivalents.

I'm surprised SDL has no random number generator considering how important
it is to games. Since I [prefer to handle this myself][prng], I don't mind
that so much, but it does leave a lot of toy programs out there calling C
`rand`. I *would* like SDL if provided [a single, good seed early during
startup][seed]. There isn't even a wall clock function for the classic
`srand(time(0))` seeding event! My solution has been to mix event
timestamps into the random state:

```c
static Uint32 rand32(Uint64 *);

Uint64 rng = 0;
for (SDL_Event e; SDL_PollEvent(&e);) {
    rng ^= e.common.timestamp;
    rand32(&rng);  // stir
    switch (e.type) { /* ... */ }
}
```

As I learn more in the future, I may come back and add to this list. At
the very least I expect to use SDL increasingly in my own projects.


[IMGUI]: https://caseymuratori.com/blog_0001
[SDL wiki]: https://wiki.libsdl.org/SDL2/FrontPage
[arena]: https://www.rfleury.com/p/untangling-lifetimes-the-arena-allocator
[assert]: /blog/2022/06/26/
[cpt]: https://vimdoc.sourceforge.net/htmldoc/options.html#'cpt'
[ctags]: https://github.com/universal-ctags/ctags
[fs]: /blog/2016/01/31/
[ods]: https://learn.microsoft.com/en-us/windows/win32/api/debugapi/nf-debugapi-outputdebugstringw
[prng]: /blog/2017/09/21/
[qoi]: /blog/2022/12/18/
[r]: https://old.reddit.com/r/C_Programming/comments/106djd0/sdl2_common_mistakes_and_how_to_avoid_them/
[sdl]: https://www.libsdl.org/
[sdlamp]: https://www.youtube.com/playlist?list=PL6m6sxLnXksbqdsAcpTh4znV9j70WkmqG
[seed]: /blog/2019/04/30/
[stb]: https://github.com/nothings/stb
[u-config]: /blog/2023/01/18/
[unity build]: https://en.wikipedia.org/wiki/Unity_build
[w64devkit]: https://github.com/skeeto/w64devkit
[wild]: /blog/2022/02/18/
