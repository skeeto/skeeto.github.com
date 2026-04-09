---
title: "2026 has been the most pivotal year in my career… and it's only March"
layout: post
date: 2026-03-29T21:38:22Z
tags: [ai, c, cpp]
uuid: 91d679b3-4f07-4b61-b359-5890695ad621
---

In February I left my employer after nearly two decades of service. In the
moment I was optimistic, yet unsure I made the right choice. Dust settled,
I'm now absolutely sure I chose correctly. I'm happier and better for it.
There were multiple factors, but it's not mere chance it coincides with
these early months of [the automation of software engineering][big]. I
left an employer that is *years behind* adopting AI to one actively
supporting and encouraging it. As of March, in my professional capacity
**I no longer write code myself**. My current situation was unimaginable
to me only a year ago. Like it or not, this is the future of software
engineering. Turns out I like it, and having tasted the future I don't
want to go back to the old ways.

In case you're worried, this is still me. These are my own words. [Writing
is thinking][writes], and it would defeat the purpose for an AI to write
in my place on my personal blog. That's not going to change.

I still spend much time reading and understanding code, and using most of
the same development tools. It's more like being a manager, orchestrating
a nebulous team of inhumanly-fast, nameless assistants. Instead of dicing
the vegetables, I conjure a helper to do it while I continue to run the
kitchen. I haven't managed people in some 20 years now, but I can feel
those old muscles being put to use again as I improve at this new role.
Will these kitchens still need human chefs like me by the end of the
decade? Unclear, and it's something we all need to prepare for.

My situation gave me an experience onboarding with AI assistance — a fast
process given a near-instant, infinitely-patient helper answering any
question about the code. By second week I was making substantial, wide
contributions to the large C++ code base. It's difficult to attach a
quantifiable factor like 2x, 5x, 10x, etc. faster, but I can say for
certain this wouldn't have been possible without AI. The bottlenecks have
shifted from producing code, which now takes relatively no time at all, to
other points, and we're all still trying to figure it out.

My personal programming has transformed as well. Everything [I said about
AI in late 2024][llm] is, as I predicted, utterly obsolete. There's a
huge, growing gap between open weight models and the frontier. Models you
can run yourself are toys. In general, almost any AI product or service
worth your attention costs money. The free stuff is, at minimum, months
behind. Most people only use limited, free services, so there's a broad
unawareness of just how far AI has advanced. AI is *now highly skilled at
programming*, and better than me at almost every programming task, with
inhumanly-low defect rates. The remaining issues are mainly steering
problems: If AI code doesn't do what I need, likely the AI writing it
didn't understand what I needed.

I'll still write code myself from time to time for fun — [minimalist][],
with my [style][] and [techniques][] — the same way I play [shogi][] on
the weekends for fun. However, artisan production is uneconomical in the
presence of industrialization. AI makes programming so cheap that only the
rich will write code by hand.

A small part of me is sad at what is lost. A bigger part is excited about
the possibilities of the future. I've always had more ideas than time or
energy to pursue them. With AI at my command, the problem changes shape. I
can comfortably take on complexity from which I previously shied away, and
I can take a shot at any idea sufficiently formed in my mind to prompt an
AI — a whole skill of its own that I'm actively developing.

For instance, a couple weeks ago I [put AI to work on a problem][curses],
and it produced a working solution for me after ~12 hours of continuous,
autonomous work, literally while I slept. The past month [w64devkit][] has
burst with activity, almost entirely AI-driven. Some of it architectural
changes I've wanted for years, but would require hours of tedious work,
and so I never got around to it. AI knocked it out in minutes, with the
new architecture opening new opportunities. It's also taken on most of the
cognitive load of maintenance.

### Quilt.cpp

So far the my biggest, successful undertaking is **[Quilt.cpp][]**, a C++
clone of [Quilt][], an early, actively-used source control system for
patch management. Git is a glaring omission from the [almost][] complete
w64devkit, due platform and build issues. I've thought Quilt could fill
*some* of that source control hole, except the original is written in
Bash, Perl, and GNU Coreutils — even more of a challenge than Git. Since
Quilt is conceptually simple, and I could lean on [busybox-w32][] `diff`
and `patch`, I've considered writing my own implementation, just [as I did
pkg-config][u-config], but I never found the energy to do it.

Then I got good enough with AI to knock out a near feature-complete clone
in about four days, including a built-in `diff` and `patch` so it doesn't
actually depend on external tools (except invoking `$EDITOR`). On Windows
it's a ~1.6MB standalone EXE, to be included in future w64devkit releases.
The source is distributed as an amalgamation, a single file `quilt.cpp`
per its namesake:

    $ c++ -std=c++20 -O2 -s -o quilt.exe quilt.cpp
    $ ./quilt.exe --help
    Usage: quilt [--quiltrc file] <command> [options] [args]

    Commands:
      new        Create a new empty patch
      add        Add files to the topmost patch
      push       Apply patches to the source tree
      pop        Remove applied patches from the stack
      refresh    Regenerate a patch from working tree changes
      diff       Show the diff of the topmost or a specified patch
      series     List all patches in the series
      applied    List applied patches
      unapplied  List patches not yet applied
      top        Show the topmost applied patch
      next       Show the next patch after the top or a given patch
      previous   Show the patch before the top or a given patch
      delete     Remove a patch from the series
      rename     Rename a patch
      import     Import an external patch into the series
      header     Print or modify a patch header
      files      List files modified by a patch
      patches    List patches that modify a given file
      edit       Add files to the topmost patch and open an editor
      revert     Discard working tree changes to files in a patch
      remove     Remove files from the topmost patch
      fold       Fold a diff from stdin into the topmost patch
      fork       Create a copy of the topmost patch under a new name
      annotate   Show which patch modified each line of a file
      graph      Print a dot dependency graph of applied patches
      mail       Generate an mbox file from a range of patches
      grep       Search source files (not implemented)
      setup      Set up a source tree from a series file (not implemented)
      shell      Open a subshell (not implemented)
      snapshot   Save a snapshot of the working tree for later diff
      upgrade    Upgrade quilt metadata to the current format
      init       Initialize quilt metadata in the current directory

    Use "quilt <command> --help" for details on a specific command.

It supports Windows and POSIX, and runs ~5x faster than the original. AI
developed it on Windows, Linux, and macOS: It's best when the AI can close
the debug loop and tackle problems autonomously without involving a human
slowpoke. The handful of "not implemented" parts aren't because they're
too hard — each would probably take an AI ~10 minutes — but deliberate
decisions of taste.

There's an irony that the reason I could produce Quilt.cpp with such ease
is also a reason I don't really need it anymore.

I changed the output of `quilt mail` to be more Git-compatible. The mbox
produced by Quilt.cpp can be imported into Git with a plain `git am`:

    $ quilt mail --mbox feature-branch.mbox
    $ git am feature-branch.mbox

The idea being that I could work on a machine without Git (e.g. Windows
XP), and copy/mail the mbox to another machine where Git can absorb it as
though it were in Git the whole time. `git format-patch` to `quilt import`
sends commits in the opposite direction, useful for manually testing
Quilt.cpp on real change sets.

To be clear, I could not have done this if the original Quilt did not
exist as a working program. I began with an AI generating a [conformance
suite][test] based on the original, its documentation, and other online
documentation, validating that suite against the original implementation
(see `-DQUILT_TEST_EXECUTABLE`). Then had another AI code to the tests, on
architectural guidance from me, with `-D_GLIBCXX_DEBUG` and sanitizers as
guardrails. That was day one. The next three days were lots of refining
and iteration as I discover the gaps in the test suite. I'd prompt AI to
compare Quilt.cpp to the original Quilt man page, add tests for missing
features, validate the new tests against the original Quilt, then run
several agents to fix the tests. While they worked I'd try the latest
build and note any bugs. As of this writing, the result is about equal
parts test and non-test, ~9KLoC each.

I'm likely to use this technique to clone other tools with implementations
unsuitable for my purposes. I learned quite a bit from this first attempt.

Why C++ instead of my usual choice of C? As we know, [conventional C is
highly error-prone][libc]. Even AI has trouble with it. In the ~9k lines
of C++ that is Quilt.cpp, I am only aware of three memory safety errors by
the AI. Two were null-terminated string issues with `strtol`, where the AI
was essentially writing C instead of C++, after which I directed the AI to
use `std::from_chars` and drop as much direct libc use as possible. (The
other was an unlikely branch with `std::vector::back` on an empty vector.)
We can rescue C with better techniques like arena allocation, counted
strings, and slices, but while (current) state of the art AI understands
these things, it cannot work effectively with them in C. I've tried. So I
picked C++, and from my professional work I know AI is better at C++ than
me.

Also like a manager, I have not read most of the code, and instead focused
on results, so you might say this was "vibe-coded." It *is* thoroughly
tested, though I'm sure there are still bugs to be ironed out, especially
on the more esoteric features I haven't tried by hand yet.

### Let's discuss tools

After opposing CMake for years, you may have noticed the latest w64devkit
now includes CMake and Ninja. What happened? Preparing for my anticipated
employment change, this past December I read [*Professional CMake*][book].
I realized that my practical problems with CMake were that nearly everyone
uses it incorrectly. Most CMake builds are a disaster, but my new-found
knowledge allows me to navigate the common mistakes. Only high profile
open source projects manage to put together proper CMake builds. Otherwise
the internet is loaded with CMake misinformation. Similar to AI, if you're
not paying for CMake knowledge then it's likely wrong or misleading. So I
highly recommend that book!

Frontier AI is *very good* with CMake. When a project has a CMake build
that isn't *too* badly broken, just tell AI to fix it, *without any
specifics*, and build problems disappear in mere minutes without having to
think about it. It's awesome. Combine it with the previous discussion
about tests making AI so much more effective, and that it *also* knows
CTest well, and you've got a killer formula. I'm more effective with CTest
myself merely from observing how AI uses it. AI (currently) cannot use
debuggers, so putting powerful, familiar testing tools in its hands helps
a lot, versus the usual bespoke, debugger-friendly solutions I prefer.

Similar to solving CMake problems: Have a hairy merge conflict? Just ask
AI resolve it. It's like magic. I no longer fear merge conflicts.

So part of my motivation for adding CMake to w64devkit was anticipation of
projects like Quilt.cpp, where they'd be available to AI, or at least so I
could use the tools the AI used to build/test myself. It's already paid
for itself, and there's more to come.

For agent software, on personal projects I'm using Claude Code. It's a
great value, cheaper than paying API rates but requires working around
5-hour limit windows. I started with Pro (US$20/mo), but I'm getting so
much out of it that as of this writing I'm on 5x Max (US$100/mo) simply to
have enough to explore all my ideas. Be warned: **Anthropic software is
quite buggy, more so than industry average**, and it's obvious that they
never even *start*, let alone test, some of their released software on
disfavored platforms (Windows, Android). Don't expect to use Claude Code
effectively for native Windows platform development, which sadly includes
w64devkit. Hopefully that's fixed someday. I suspect Anthropic hit a
bottleneck on QA, and unable to fit AI in that role they don't bother. You
can theoretically report bugs on GitHub, but they're just ignored and
closed. (Why don't they have AI agents jumping on this wealth of bug
reports?)

At work I'm using Cursor where I get a choice of models. My favorite for
March has been GPT-5.4, which in my experience beats Opus 4.6 on Claude
Code by a small margin. It's immediately obvious that Cursor is better
agent software than Claude Code. It's more robust, more featureful, and
with a clearer UI than Claude Code. It has no trouble on Windows and can
drive w64devkit flawlessly. It's also more expensive than Claude Code. My
employer currently spends ~US$250/mo on my AI tokens, dirt cheap
considering what they're getting out of it. I have bottlenecks elsewhere
that keep me from spending even more.

As a general rule, for software engineering always use the smartest model
available. The cheaper, dumber models cost more in the long run. It takes
more tokens to achieve worse results, which costs more human time to sort
out.

Neither Cursor nor Claude Code are open source, so what are the purists to
do, even if they're willing to pay API rates for tokens? Sadly I have no
answers for you. I haven't gotten any open source agent software actually
working, and it seems they may lack the necessary secret sauce.

Update: Several folks suggested I give [OpenCode][] another shot, and this
time I got over the configuration hurdle. Single executable, slick
interface, and unlike Claude Code, I observed no bugs in my brief trial.
Give that a shot if you're looking for an open source client.

The future is going to be weird. My experience is only a peek at what's to
come, and my head is still spinning. However, the more I adapt to the
changes, the better I feel. If you're feeling anxious like I was, don't
flinch from improving your own AI knowledge and experience.


[OpenCode]: https://opencode.ai/
[Quilt.cpp]: https://github.com/skeeto/quilt.cpp
[Quilt]: https://savannah.nongnu.org/projects/quilt
[almost]: /blog/2020/09/25/
[big]: https://shumer.dev/something-big-is-happening
[book]: https://crascit.com/professional-cmake/
[busybox-w32]: https://frippery.org/busybox/
[curses]: https://github.com/skeeto/w64devkit/pull/357
[libc]: /blog/2023/02/11/
[llm]: /blog/2024/11/10/
[minimalist]: /blog/2018/06/10/
[shogi]: https://en.wikipedia.org/wiki/Shogi
[style]: /blog/2023/10/08/
[techniques]: /blog/2025/01/19/
[test]: https://eli.thegreenplace.net/2026/rewriting-pycparser-with-the-help-of-an-llm/
[u-config]: /blog/2023/01/18/
[w64devkit]: https://github.com/skeeto/w64devkit
[writes]: https://paulgraham.com/writes.html
