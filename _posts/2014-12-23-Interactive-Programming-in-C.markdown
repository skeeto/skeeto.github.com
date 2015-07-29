---
title: Interactive Programming in C
layout: post
date: 2014-12-23T05:43:41Z
tags: [c, tutorial]
uuid: 203e981d-b086-393e-27c0-db18dacfc4bf
---

I'm a huge fan of interactive programming (see: [JavaScript][skewer],
[Java][java], [Lisp][slime], [Clojure][cider]). That is, modifying and
extending a program while it's running. For certain kinds of non-batch
applications, it takes much of the tedium out of testing and tweaking
during development. Until last week I didn't know how to apply
interactive programming to C. How does one go about redefining
functions in a running C program?

Last week in [Handmade Hero][hh] (days 21-25), Casey Muratori added
interactive programming to the game engine. This is especially useful
in game development, where the developer might want to tweak, say, a
boss fight without having to restart the entire game after each tweak.
Now that I've seen it done, it seems so obvious. **The secret is to
build almost the entire application as a shared library.**

This puts a serious constraint on the design of the program: **it
cannot keep any state in global or static variables**, though this
[should be avoided anyway][global]. Global state will be lost each
time the shared library is reloaded. In some situations, this can also
restrict use of the C standard library, including functions like
`malloc()`, depending on how these functions are implemented or
linked. For example, if the C standard library is statically linked,
functions with global state may introduce global state into the shared
library. It's difficult to know what's safe to use. This works fine in
Handmade Hero because the core game, the part loaded as a shared
library, makes no use of external libraries, including the standard
library.

Additionally, the shared library must be careful with its use of
function pointers. The functions being pointed at will no longer exist
after a reload. This is a real issue when combining interactive
programming with [object oriented C][oop].

### An example with the Game of Life

To demonstrate how this works, let's go through an example. I wrote a
simple ncurses Game of Life demo that's easy to modify. You can get
the entire source here if you'd like to play around with it yourself
on a Unix-like system.

* [https://github.com/skeeto/interactive-c-demo](https://github.com/skeeto/interactive-c-demo)

**Quick start**:

1. In a terminal run `make` then `./main`. Press `r` randomize and `q`
   to quit.
2. Edit `game.c` to change the Game of Life rules, add colors, etc.
3. In a second terminal run `make`. Your changes will be reflected
   immediately in the original program!

![](/img/screenshot/live-c.gif)

As of this writing, Handmade Hero is being written on Windows, so
Casey is using a DLL and the Win32 API, but the same technique can be
applied on Linux, or any other Unix-like system, using libdl. That's
what I'll be using here.

The program will be broken into two parts: the Game of Life shared
library ("game") and a wrapper ("main") whose job is only to load the
shared library, reload it when it updates, and call it at a regular
interval. The wrapper is agnostic about the operation of the "game"
portion, so it could be re-used almost untouched in another project.

To avoid maintaining a whole bunch of function pointer assignments in
several places, the API to the "game" is enclosed in a struct. This
also eliminates warnings from the C compiler about [mixing data and
function pointers][fp]. The layout and contents of the `game_state`
struct is private to the game itself. The wrapper will only handle a
pointer to this struct.

~~~c
struct game_state;

struct game_api {
    struct game_state *(*init)();
    void (*finalize)(struct game_state *state);
    void (*reload)(struct game_state *state);
    void (*unload)(struct game_state *state);
    bool (*step)(struct game_state *state);
};
~~~

In the demo the API is made of 5 functions. The first 4 are primarily
concerned with loading and unloading.

* `init()`: Allocate and return a state to be passed to every other
  API call. This will be called once when the program starts and never
  again, even after reloading. If we were concerned about using
  `malloc()` in the shared library, the wrapper would be responsible
  for performing the actual memory allocation.

* `finalize()`: The opposite of `init()`, to free all resources held
  by the game state.

* `reload()`: Called immediately after the library is reloaded. This
  is the chance to sneak in some additional initialization in the
  running program. Normally this function will be empty. It's only
  used temporarily during development.

* `unload()`: Called just before the library is unloaded, before a new
  version is loaded. This is a chance to prepare the state for use by
  the next version of the library. This can be used to update structs
  and such, if you wanted to be really careful. This would also
  normally be empty.

* `step()`: Called at a regular interval to run the game. A real game
  will likely have a few more functions like this.

The library will provide a filled out API struct as a global variable,
`GAME_API`. **This is the only exported symbol in the entire shared
library!** All functions will be declared static, including the ones
referenced by the structure.

~~~c
const struct game_api GAME_API = {
    .init     = game_init,
    .finalize = game_finalize,
    .reload   = game_reload,
    .unload   = game_unload,
    .step     = game_step
};
~~~

#### dlopen, dlsym, and dlclose

The wrapper is focused on calling `dlopen()`, `dlsym()`, and
`dlclose()` in the right order at the right time. The game will be
compiled to the file `libgame.so`, so that's what will be loaded. It's
written in the source with a `./` to force the name to be used as a
filename. The wrapper keeps track of everything in a `game` struct.

~~~c
const char *GAME_LIBRARY = "./libgame.so";

struct game {
    void *handle;
    ino_t id;
    struct game_api api;
    struct game_state *state;
};
~~~

The `handle` is the value returned by `dlopen()`. The `id` is the
inode of the shared library, as returned by `stat()`. The rest is
defined above. Why the inode? We could use a timestamp instead, but
that's indirect. What we really care about is if the shared object
file is actually a different file than the one that was loaded. The
file will never be updated in place, it will be replaced by the
compiler/linker, so the timestamp isn't what's important.

Using the inode is a much simpler situation than in Handmade Hero. Due
to Windows' broken file locking behavior, the game DLL can't be
replaced while it's being used. To work around this limitation, the
build system and the loader have to rely on randomly-generated
filenames.

~~~c
void game_load(struct game *game)
~~~

The purpose of the `game_load()` function is to load the game API into
a `game` struct, but only if either it hasn't been loaded yet or if
it's been updated. Since it has several independent failure
conditions, let's examine it in parts.

~~~c
struct stat attr;
if ((stat(GAME_LIBRARY, &attr) == 0) && (game->id != attr.st_ino)) {
~~~

First, use `stat()` to determine if the library's inode is different
than the one that's already loaded. The `id` field will be 0
initially, so as long as `stat()` succeeds, this will load the library
the first time.

~~~c
    if (game->handle) {
        game->api.unload(game->state);
        dlclose(game->handle);
    }
~~~

If a library is already loaded, unload it first, being sure to call
`unload()` to inform the library that it's being updated. **It's
critically important that `dlclose()` happens before `dlopen()`.** On
my system, `dlopen()` looks only at the string it's given, not the
file behind it. Even though the file has been replaced on the
filesystem, `dlopen()` will see that the string matches a library
already opened and return a pointer to the old library. (Is this a
bug?) The handles are reference counted internally by libdl.

~~~c
    void *handle = dlopen(GAME_LIBRARY, RTLD_NOW);
~~~

Finally load the game library. There's a race condition here that
cannot be helped due to limitations of `dlopen()`. The library may
have been updated *again* since the call to `stat()`. Since we can't
ask `dlopen()` about the inode of the library it opened, we can't
know. But as this is only used during development, not in production,
it's not a big deal.

~~~c
    if (handle) {
        game->handle = handle;
        game->id = attr.st_ino;
        /* ... more below ... */
    } else {
        game->handle = NULL;
        game->id = 0;
    }
~~~

If `dlopen()` fails, it will return `NULL`. In the case of ELF, this
will happen if the compiler/linker is still in the process of writing
out the shared library. Since the unload was already done, this means
no game will be loaded when `game_load` returns. The user of the
struct needs to be prepared for this eventuality. It will need to try
loading again later (i.e. a few milliseconds). It may be worth filling
the API with stub functions when no library is loaded.

~~~c
    const struct game_api *api = dlsym(game->handle, "GAME_API");
    if (api != NULL) {
        game->api = *api;
        if (game->state == NULL)
            game->state = game->api.init();
        game->api.reload(game->state);
    } else {
        dlclose(game->handle);
        game->handle = NULL;
        game->id = 0;
    }
~~~

When the library loads without error, look up the `GAME_API` struct
that was mentioned before and copy it into the local struct. Copying
rather than using the pointer avoids one more layer of redirection
when making function calls. The game state is initialized if it hasn't
been already, and the `reload()` function is called to inform the game
it's just been reloaded.

If looking up the `GAME_API` fails, close the handle and consider it
a failure.

The main loop calls `game_load()` each time around. And that's it!

~~~c
int main(void)
{
    struct game game = {0};
    for (;;) {
        game_load(&game);
        if (game.handle)
            if (!game.api.step(game.state))
                break;
        usleep(100000);
    }
    game_unload(&game);
    return 0;
}
~~~

Now that I have this technique in by toolbelt, it has me itching to
develop a proper, full game in C with OpenGL and all, perhaps in
[another Ludum Dare][ld]. The ability to develop interactively is very
appealing.


[java]: /blog/2011/08/30/
[skewer]: /blog/2012/10/31/
[slime]: http://common-lisp.net/project/slime/
[cider]: https://github.com/clojure-emacs/cider
[hh]: http://handmadehero.org/
[global]: /blog/2014/10/12/
[oop]: /blog/2014/10/21/
[fp]: /blog/2010/02/18/
[ld]: /blog/2014/12/09/
