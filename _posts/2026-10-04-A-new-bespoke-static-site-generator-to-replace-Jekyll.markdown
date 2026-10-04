---
title: A new, bespoke static site generator to replace Jekyll
date: 2026-10-04T16:35:49Z
tags: [c, cpp, meta, ai]
uuid: bee8d4c0-5185-8d4b-b14a-4aba71276e87
---

My blog began as a [blosxom][] (Perl) site running on a VPS. In 2011 I
[moved to the new GitHub Pages][move], with the site generated statically
by [Jekyll][] (Ruby) on a GitHub server. It was a no-brainer: easier,
faster, and cheaper, better in every way. After 15 years of Jekyll, this
week I replaced it with [a new, custom-built static site generator][ssg],
dubbed *ssg*, in "C with templates" C++20. The ~8KLoC source [closely
follows my personal coding style][style] including [templated arenas and
slices][c++], zero dependencies, and a libc-free core. It's wicked fast,
and a complete, cold generation of my blog takes 150ms on my MacBook. That
is, *it's done before Ruby would even reach Jekyll's entry point*. It's a
been a great, real-world demonstration of the effectiveness of my coding
philosophy.

Look around and you'll find almost nothing visually changed. Outside of
syntax highlighting, the HTML is semantically identical. I did not try to
match Rouge's syntax highlighting, just its CSS selectors so that I could
use my style sheet unmodified. With that in my own hands, I now get syntax
highlighting that better suits my needs, some Rouge bugs gone and support
for new languages, particularly assembly ([WAT][], [AT&T][], [Aarch64][])
and [QBasic][].

Because ssg only supports my site, and its source lives along side it, I
don't need a template system, i.e. Liquid. I can modify the C++ source as
needed. So the upgrade was three steps: (1) fix 19 years of accumulated
site bugs that Jekyll quietly tolerated, (2) add the new generator, (3)
delete Liquid templates and pre-generated tag pages (now dynamically
generated, with preservation of original feed UUIDs). Jekyll and ssg both
work simultaneously at step #2, allowing side-by-side comparisons from the
same site source. This is where most of the time was spent. Dropping
templates means performance comparisons with Jekyll are unfair, as Jekyll
is solving a different problem. (But I think it's fair to speculate that
ssg with Liquid templates would still smoke Jekyll!)

Historically, Jekyll was a dreadfully slow site generator until [the 4.0.0
release in August 2019][4]. While performance is mostly resolved, I still
have ingrained habits to work around it. The final Jekyll version of my
site took ~2 seconds on the same MacBook. Not too bad, and it has a fast
`--incremental` mode, though it's always been a little buggy, not quite
matching a full generation.

**No, the pain point for Jekyll is not performance but *deployment***.
Especially when trying to match the GitHub Pages configuration. The Ruby
deployment situation remains just awful. I never once generated my blog on
Windows, in part to avoid jumping through hoops to set up Jekyll. Last
year I switched to macOS (from Linux) as my primary, personal development
environment. This meant setting up Jekyll on macOS, and [the situation is
farcical][macOS]. The Ruby community ought to feel embarrassed about this.
The `source` lines delay shell startup by 60ms, and I'm unwilling to bear
that cost in nearly every shell just to occasionally run Jekyll, plus
environment litter that may interfere with other tools. So I needed to
remember to enter an isolated Jekyll environment, and to tell AIs to use
if they needed Jekyll.

With ssg you just need a C++ compiler. As native application, you don't
need any development tools once it's built of course, and the compiled
program is a single file that could be copied to another system and run
as-is. It's a unity build — I did say it closely follows my personal style
— so you don't even need a build system. On [w64devkit][] that looks like:

    $ cc -nostartfiles -o _ssg/ssg.exe _ssg/main_windows.cpp

`cc` (or `gcc`) works because the compiler driver knows to invoke the C++
front-end for this input. It doesn't use the C++ standard library, so the
linker doesn't need `-lstdc++`. `-O0` builds like this run at about half
speed compared to optimized builds, and `-O1` is sufficient to get all the
compiler optimization benefits. Normally debug builds would be around 10x
slower, but fast debug builds is par for the course for my style. While
you don't need it, there *is* a CMake build, but that's really just for
driving the test suite.

Both build and tool work on Windows XP, too, so for the first time ever I
could [fully compose a post on my old XP laptop][xp] if I wished.

On the MacBook this `-O0` build takes ~150ms — quite good for 8,372 lines
of code. A cold site generation takes this build ~260ms. That's so fast I
could very well re-compile the generator each time I regenerate the site.
Indeed that's exactly how it works in the GitHub Actions pipeline. Overall
it's faster to use a debug build than a release build. The cache is too
slow and unreliable for release builds to make sense in Actions.

    $ cc -std=c++20 -o _ssg/ssg _ssg/main_posix.cpp  # macOS

For the first decade of GitHub Pages, Jekyll was the only option. It ran
opaquely in the somewhere at GitHub with a pass/fail result, requiring a
local matching configuration for debugging. When they introduced GitHub
Actions in 2018, Jekyll was a pre-configured, transparent pipeline, and it
became possible to run whatever generator you wanted in its place. I'm
finally taking advantage of that, and I still get automatic page builds on
push like I had with Jekyll. It's now slightly faster — Actions overhead
dominates either way — despite compiling an entire C++ program each run.

I do not plan to divorce my generator from my blog, so ssg will not become
a general-purpose static site generator. Its whole purpose is to serve
this one particular need. You're free to fork it and use it as a basis for
your own needs, of course. This sort of bespoke, written-to-order software
is likely the future of software. Why bother with one-site-fits-all when
an exactly-sized solution has the same cost?

### A first for generative AI

I've been thinking about this project for years. Had I written it in the
2023–2025 time frame, I estimate ~2–4 weeks, using [u-config][] (3KLoC,
lower complexity) as a measuring stick. Here in [2026][], it took about
~20 minutes to write my detailed prompt, then ~2 hours for Opus 5.5 in
Claude Desktop to produce ssg in essentially its current form, bug-free as
far as I can tell, including an *untested* Win32 platform layer (written
on the MacBook, no Wine). I've spent more time on this article than I did
on the new generator.

I spent a couple more hours looking it over, shocked at Opus 5.5 perfectly
matching my personal style. It really does look like I wrote ssg. Reading
it is uncanny, like seeing someone reproduce my own handwriting such that
I couldn't distinguish it. During this review I realized we ought to use
debug builds in the pipeline, so we had [one follow-up on the hot spot in
debug builds][debug]. Then I noticed the Win32 platform layer generated an
empty site on Windows XP, [another small followup][fix-xp]. (XP wasn't in
my original prompt, so I don't count this as a bug.) That was it.

Two years ago [I said][llm] that AI-generated code was too poor to be
practical. That changed by the end of 2025. In earlier 2026 projects I
tried to get AI to write C following my style, using my writing on the
subject as a guide, but they'd just recreate the [no-good standard
library][libc] from scratch then flub arena allocation. Ask those models
to write conventional C and you get the usual, error-prone C you'll find
anywhere. So I settled on conventional C++ as Good Enough. I could
complete projects ~20x faster, but with results not quite as good as I'd
have done myself. A reasonable trade-off.

Opus 5.5 released on September 22nd. It is the first model I've used that
actually understands my coding philosophy and **writes C at least as well
as me**. This was the ideal project for this test, as my core writing on
this subject was naturally in context, but it generalizes when referencing
my writing and prior work. As of two weeks ago I can now have my cake and
eat it, too. No more trade-offs.

If you'd like to get similar C-with-templates results on your own project,
cite these four articles in your prompt:

* <https://nullprogram.com/blog/2025/01/19/>
* <https://nullprogram.com/blog/2024/04/14/>
* <https://nullprogram.com/blog/2025/09/30/>
* <https://nullprogram.com/blog/2025/10/16/>

As well as perhaps my relevant, similar projects. All the frontier models
know about me personally and are familiar with my work, so invoking my
name may be enough. If the model is as good as Opus 5.5, you might get an
accurate impression of how I would have done your project.


[2026]: /blog/2026/03/29/
[4]: https://jekyllrb.com/news/2019/08/20/jekyll-4-0-0-released/
[AT&T]: /blog/2024/02/05/
[Aarch64]: /blog/2022/02/18/
[Jekyll]: https://jekyllrb.com/
[QBasic]: /blog/2020/11/17/
[WAT]: /blog/2025/04/04/
[blosxom]: https://www.blosxom.com/
[c++]: /blog/2024/04/14/
[debug]: https://github.com/skeeto/skeeto.github.com/commit/2c9aa4e9
[fix-xp]: https://github.com/skeeto/skeeto.github.com/commit/35228acf
[libc]: /blog/2023/02/11/
[llm]: /blog/2024/11/10/
[macOS]: https://jekyllrb.com/docs/installation/macos/
[move]: /blog/2011/08/05/
[ssg]: https://github.com/skeeto/skeeto.github.com/tree/master/_ssg
[ssg]: https://github.com/skeeto/skeeto.github.com/tree/master/_ssg
[style]: /blog/2025/01/19/
[u-config]: /blog/2023/01/18/
[w64devkit]: https://github.com/skeeto/w64devkit
[xp]: /blog/2025/03/02/
