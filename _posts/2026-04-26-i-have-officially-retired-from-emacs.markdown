---
title: I have officially retired from Emacs
layout: post
date: 2026-04-26T00:00:00Z
tags: [ai, cpp, emacs, elfeed]
uuid: 9db72e48-92f3-4426-a18f-9a317354e2c8
---

This past Tuesday I typed `C-x C-c` in Emacs for the last time after 20
years of daily use. Though [nearly half that time][vim] was gradually
retiring it, switching to modal editing, then to Vim. Emacs is a platform,
and I'd grown accustomed to its applications, especially those I built
myself. There was no particular hurry, so replacements came slowly. With
my [newly-acquired superpowers][sp] I could knock out the last two pieces
in a few days' work, namely [`M-x calc`][calc] with **[stackcalc][]** and
[Elfeed][init] with **[Elfeed2][]**. I'm especially excited about the
latter because it already exceeds the original. Both are multi-platform,
native C++ GUI applications using native UI components.

![](/img/elfeed2.png)

These [actively-in-use][melpa] packages require new maintainers (apply on
the project's issues/discussion):

* [@](https://github.com/skeeto/at-el/) ([about](/blog/2013/04/07/))
* [aio](https://github.com/skeeto/emacs-aio/) (probably covered; [about](/blog/2019/03/10/))
* [bitpack](https://github.com/skeeto/bitpack/)
* [Elfeed](https://github.com/skeeto/elfeed/) ([apply here](https://github.com/skeeto/elfeed/discussions/563))
* [Impatient](https://github.com/skeeto/impatient-mode/) ([about](/blog/2012/08/20/))
* [javadoc-lookup](https://github.com/skeeto/javadoc-lookup/) ([about](/blog/2013/01/30/))
* [json-rpc](https://github.com/skeeto/elisp-json-rpc/)
* [memoize](https://github.com/skeeto/emacs-memoize/) ([about](/blog/2010/07/26/))
* [nasm-mode](https://github.com/skeeto/nasm-mode/) ([about](/blog/2015/04/19/))
* [simple-httpd](https://github.com/skeeto/emacs-web-server/) ([about](/blog/2012/08/20/))
* [Skewer](https://github.com/skeeto/skewer-mode/) ([about](/blog/2012/10/31/))
* [weak-ref](https://github.com/skeeto/elisp-weak-ref/) ([about](/blog/2012/12/17/))
* [x86-lookup](https://github.com/skeeto/x86-lookup/) ([about](/blog/2015/11/21/))

No wonder it took so long for me to move on! I'm not handing these off to
just anyone, and you'll need to establish your reputation. Having already
made contributions is a good sign, even if never merged. I'm willing to
transfer them off my namespace, though you'll need to manage the Melpa
hand-off (on which I'll sign-off). If there are no takers, these projects
will be archived but not deleted.

### Trying out wxWidgets

The Emacs Calculator is amazing and the best calculator I've ever used,
which is why nothing I could find was going to replace it. My clone uses
GMP and MPFR for multi-precision, so it's far faster, as to be expected,
but it's not nearly at feature parity. It's missing esoteric features
including symbolic processing. Though it's enough to cover all of my own
usage. I can add more features later. The Emacs Calculator manual served
as a specification when building stackcalc.

Elfeed has been a cornerstone of my daily routines for the past 13 years.
Nothing else I've found scratches that itch for me, so I've always known
it would require a rewrite someday. Knowing it would take a few weeks of
work, and that I *already had the feed reader I wanted*, made motivation
difficult to find. Though now that I can accomplish ~3 weeks of old-way
work in a new-way day, this sort of project becomes that much easier to
start and finish. Though it's not yet at a 1.0 release, after a couple
days Elfeed2 was working well enough to replace the original Elfeed.

While [Dear ImGui][] was the right choice for [dcmake][], it would not be
so for these two applications. Active rendering doesn't suit a feed reader
left running all day, and I needed a richer toolkit. Professionally I work
in Qt, but I wanted something lighter-weight for my projects, accessible
via CMake `FetchContent`. That naturally led to [wxWidgets][]. While it
has issues — mitigatable character encoding problems, accidental quadratic
time in many places — it's worked better than I anticipated, letting me
rapidly produce native-looking applications on Windows, macOS, and Linux.

Unlike Dear ImGui, wxWidgets is a platform, including [sane][] I/O and
path handling. I *mostly* don't need platform layers when building
applications like these. I can simply rely on wxWidgets' utilities.

Both of these projects build out-of-the-box on [w64devkit][] thanks to the
dependencies being `FetchContent`-compatible. On all platforms you just
need a C++ toolchain and CMake:

    $ cmake -B build
    $ cmake --build build

Now that I have experience with wxWidgets, learning its limitations and
capabilities, it's likely to be a foundation of most of my GUI projects to
come, except where something like Dear ImGui is a better git.


[Dear ImGui]: https://github.com/ocornut/imgui
[Elfeed2]: https://github.com/skeeto/Elfeed2
[calc]: /blog/2009/06/23/
[curl]: /blog/2016/06/16/
[dcmake]: https://github.com/skeeto/dcmake
[dtrace]: /blog/2018/01/17/
[help]: https://github.com/skeeto/elfeed/discussions/563
[init]: /blog/2013/09/04/
[melpa]: https://melpa.org/#/?q=skeeto
[sp]: /blog/2026/03/29/
[stackcalc]: https://github.com/skeeto/stackcalc
[vim]: /blog/2017/04/01/
[w64devkit]: https://github.com/skeeto/w64devkit
[wxWidgets]: https://wxwidgets.org/
[sane]: /blog/2021/12/30/
