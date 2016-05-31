---
title: An Emacs Foreign Function Interface
layout: post
date: 2014-04-26T16:25:51Z
tags: [elisp, emacs]
uuid: ba31fe59-f5b0-3603-b243-4bcae00aebcf
---

For many years Richard Stallman (RMS) prohibited a foreign function
interface (FFI) in GNU Emacs. An FFI is an API for dynamically calling
native libraries at run-time, like the Java Native Interface (JNI). He
was concerned that people might use it to make proprietary extensions
to the popular editor. This was the same (paranoid) justification for
rejecting a package manager in Emacs for many years, that someone
might use it to distribute proprietary packages.

Fortunately, times have changed. RMS reevaluated his
[stances on FFI][rms] and on package managers. Today Emacs comes with
a package manager (package.el), and there are multiple package
repositories with no proprietary packages in sight. Though, outside of
[some unaccepted patches][patch], no significant progress has been
made to add an FFI.

A few weeks ago I did something about that by writing a package that
adds an FFI. It requires no patches or any other changes to Emacs
itself. Instead, it drives a subprocess running [libffi][libffi],
passing arguments and return values back and forth through a pipe, in
the spirit of [EmacSQL][emacsql]. It's not as efficient as a built-in
API, but it could potentially be distributed through an ELPA
repository.

* [Emacs Lisp Foreign Function Interface](https://github.com/skeeto/elisp-ffi)

The API is modeled loosely after [Julia's elegant FFI][julia]. A call
interface (CIF) doesn't need to be prepared ahead of time. Provide all
the necessary information at the call site and the library takes care
of building and caching CIFs and handles for you.

### API Examples

The core function for the FFI is `ffi-call`. Here's an example that
calls the system's `srand()` and then `rand()`.

~~~cl
;; seed with 0
(ffi-call nil "srand" [:void :uint32] 0)
;; => :void

(ffi-call nil "rand" [:sint32])
;; => 1102520059
~~~

The first two arguments are similar to the first two arguments of
`dlsym()`. For `ffi-call`, the first argument is the library shared
object name. The back-end automatically takes care of obtaining a
handle on the library with `dlopen()`. In this case we're accessing a
function that's already in the main program, so we pass nil. This is
identical to passing NULL to `dlsym()`. In this FFI, nil always
corresponds to NULL.

The second argument is the function name, just like `dlsym()`'s second
argument.

The third argument is the function signature. It's a vector of
keywords declaring the return value type followed by the types of each
argument. In this example, `srand()` returns nothing (void) and
accepts a single 32-bit unsigned argument, so the signature is
`[:void :uint32]`.

The remaining arguments are the native function arguments. I can keep
making the second FFI call ("rand") to retrieve different numbers,
using the first FFI call ("srand") to reset the sequence.

#### Using a Library

Here's another example, loading `libm` and calling `cos`.

~~~cl
;; cos(1.2)
(ffi-call "libm.so" "cos" [:double :double] 1.2)
;; => 0.362357754476674
~~~

The first time a library is used, the back-end creates a handle for it
with `dlopen()`. Further calls will reuse the handle, trying to be as
efficient as possible. Handles are never closed.

#### Pointers

Here are a couple of examples that use pointers. As stated before, nil
is used to pass a NULL pointer. Like the underlying libffi, the FFI
doesn't care what *kind* of pointer you're passing, just that it's a
pointer, so it's declared with `:pointer`.

~~~cl
;; time(NULL);
(ffi-call nil "time" [:uint64 :pointer] nil)
;; => 1396496875
~~~

Strings are automatically copied to the subprocess, their lifetime
tied to the lifetime of the Elisp string (note: this detail is still
unimplemented). When used as arguments, they become pointers.

~~~cl
;; getenv("DISPLAY")
(ffi-call nil "getenv" [:pointer :pointer] "DISPLAY")
;; => 0x7fffc13ceb29

(ffi-get-string '0x7fffc13ceb29)
;; => ":0"
~~~

Pointers can be handled as values on the Elisp side. They're
represented as symbols whose name is an address. In the above example,
`0x7fffc13ceb29` is one of these symbols. I would have preferred to
use a plain integer to represent pointers, but, because Elisp integers
are *tagged*, they're guaranteed not to be wide enough for this. I
plan to add pointer operators to do pointer arithmetic on these
special pointer values.

The function `ffi-get-string` is used to retrieve the null-terminated
string referenced by a pointer. If the string returned by `getenv()`
needed to be freed (it doesn't and shouldn't), the FFI caller would
need to be careful to call `free()` as another FFI call.

### How It Works: The Stack Machine

My goal is to keep the back-end as simple as possible. All resource
management is handled by Emacs, [tied to garbage collection][final].
For example, the pointer returned by `dlopen()` isn't stored anywhere
in the subprocess. It's passed to Emacs and managed there. To call a
function using the handle, the pointer is transmitted back to the
subprocess.

To keep it simple, the back-end is just a stack machine with a simple
human-readable bytecode. You can see the instruction set by looking at
the big switch statement in `ffi-glue.cc`. For example, to push a
signed 2-byte integer 237 onto the stack, send a `j` followed by an
ASCII representation of the number (terminated by a space if needed):
`j237`.

As usual, my assumption is that the Elisp printer and reader is faster
than any possible serialization I could implement within Elisp itself.
This also nicely sidesteps the byte-order issue.

The function signature is declared by pushing zeros of the
return/argument types onto the stack, with a special void "value" used
to communicate `void`. Once it's all set up, the `C` instruction is
called, collapsing the signature into a CIF handle: a pointer for the
Elisp side to manage.

Pointers to raw strings of bytes are pushed onto the stack with the
`M` instruction. It pops the top integer on the stack to get the byte
count, reads that number of bytes from input into a buffer,
null-terminates the buffer in case it's used as a string, and finally
puts a pointer to that buffer on the stack.

Calling functions is just a matter of pushing all the needed
information onto the stack, invoking libffi to magically call the
function, then popping the result off the stack. Popping a value
transmits it to Elisp.

#### Stack Machine Example

Here's a concise example that calls `cos(1.2)` (assuming libm.so is
already linked). The actual Elisp-generated FFI bytecode doesn't plan
things quite this way — particularly because it needs to keep track
of the various pointers involved — but this example keeps it simple.

    d1.2d0d0w1Cp0w3McosSco

You can run this example manually by executing the `ffi-glue` program
and pasting in that line as standard input. The result will be
printed.

1. `d1.2` : Push a double, 1.2, onto the stack. This will be the
   function argument.
2. `d0d0` : Push a couple of zero doubles onto the stack. This is our
   function signature. It takes a double and returns a double.
3. `w1` : Push an unsigned 32-bit 1 onto the stack. Instructions that
   use integers accept unsigned 32-bit integers. This 1 indicates that
   our function accepts one argument.
4. `C` : create a CIF. The integer 1 and the two 0 doubles are
   consumed and a pointer to a CIF is put on the stack. Elisp would
   normally pop this off and save it for future use, but we're going
   to leave it there (and ultimately leak it in the example).
5. `p0` : Push a NULL onto the stack. `p` means push a pointer and 0
   is a NULL pointer. This is our library handle. We're assuming `cos`
   will be in the main program.
6. `w3Mcos` : Put a pointer to the string "cos" into the stack. First
   push on the number 3 (string length), then `M` to read from input,
   then pass three bytes: "cos". In our example, this buffer will be
   leaked because we lose the buffer pointer.
7. `S` : Call `dlsym()` on the string and handle on top of the stack.
   This consumes the top two values (NULL and "cos"), and pushes a
   function handle on top of the stack. At this point the stack has
   three values: 1.2, the CIF, and the function handle.
8. `c` : Call the function pointed to by the top of the stack. This
   consumes the top pointer, the CIF below it, and the CIF indicates
   how many more values to consume: just one in this case, since the
   function takes one argument. The function's return value is pushed
   on the stack. If the function is `void`, the special void "value"
   is pushed on the stack.
9. `o` : Pop the top stack value, sending it to Emacs. This is
   what would be returned by `ffi-call`.

Before I got the Elisp side of things going, I was testing out the
back-end by writing lots of little programs like this by hand.

### A Safe FFI

While using an FFI through a pipe is slow compared to a built-in FFI,
there is a distinct advantage. The FFI can never crash Emacs! Normally,
making calls to an FFI is *unsafe*. It allows the programmer to
violate normal language constraints. If the programmer misuses the
FFI, the whole process may crash or become corrupt. This will lose any
state held behind foreign interface, but Emacs will be safe.

In my package, the handle for the FFI Emacs subprocess is called the
*context*. A context is automatically established and bound to the
`ffi-context` global variable as needed. This context keeps track of
CIFs, string buffers, handles, and any other resources held by the
subprocess. If the subprocess dies, the context becomes meaningless
since the pointers it holds are dead.

### Limitations

This FFI package is about 80% complete. It occasionally leaks memory
in the subprocess, it's overly-sensitive to mis-typing, it doesn't
manage stdin/stdout, it can't inspect/modify structs, and it can't set
up closures.

The last point, closures, would require some changes to the
interprocess communication. The purpose here would be to allow foreign
functions to call Elisp functions. The subprocess would need to be
able to initiate activity with Elisp.

Manipulating structs is complex, and even libffi has limited support
for working with them. It allows structs to be declared, but leaves
alignment and access up to the user to sort out. That's where the
previously-mentioned pointer arithmetic comes into play.

Currently stdin, stdout, and stderr are problems, especially when I
was trying to write a test GTK application with Elisp. Any command
line junkie knows that GTK (and Qt) applications are ridiculously
noisy. It spews hundreds of lines of warnings and notifications as
part of its normal operation. This noise interferes with FFI
communication with Emacs. I need to figure out how to separate this
and get standard input/output/error to/from Emacs through separate
channels.

Like libffi, there are no guarantees about variadic function calls. It
should generally Just Work, but you can't rely on it.

The whole thing will not work as well in 32-bit Emacs, where integers
are limited to a tiny 29 bits. For example, those `rand()` return
values will simply not fit. In the long run, this is probably the
single largest barrier to making the FFI work smoothly. It's too easy
to run into large integer values.

Right now I consider it a proof of concept; an FFI *really can* be
done this way. I don't have any particular uses in mind, and, outside
of the "cool factor," I can't actually think of any useful
applications. If a solid FFI already existed, I may have tried to use
it for EmacSQL rather than use this subprocess trick. My FFI *is*
probably mature enough to drive SQLite, so maybe this is the future of
EmacSQL.

If you can think of a good use for an Emacs FFI, please share it. I
need good test ideas.


[rms]: http://lists.gnu.org/archive/html/emacs-devel/2010-03/msg00240.html
[patch]: http://www.loveshack.ukfsn.org/emacs/dynamic-loading/
[libffi]: http://sourceware.org/libffi/
[emacsql]: /blog/2014/02/06/
[julia]: http://julia.readthedocs.org/en/latest/manual/calling-c-and-fortran-code/
[final]: /blog/2014/01/27/
