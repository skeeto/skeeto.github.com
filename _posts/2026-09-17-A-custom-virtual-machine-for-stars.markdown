---
title: A custom virtual machine for the Stars! 4X game
layout: post
date: 2026-06-17T02:00:00Z
tags: [c, game]
uuid: 0b80f9d8-07a1-4d9f-a26d-f119b08364da
---

[Stars!][] is a 1995 [4X][] game (explore, expand, exploit, exterminate)
for 16-bit Windows 3.1 that I first played ~28 years ago. While Windows is
famously backwards compatible, it's notoriously difficult to play Stars!
today. Windows x64 cannot run 16-bit applications, and playing requires
either retro hardware or emulation ([otvdm][], [DOSBox][]), sometimes
paired with [Wine][]. My new, exciting solution, **[Stars!VM][]**, or
*Stars! Virtual Machine*, embeds a custom [80286][] emulator and a Win16
to Win32 bridge. As native Win32, the game looks and feels exactly as it
did originally, except sporting a modern file chooser and 4k scaling. It's
indistinguishable from a genuine 32-bit or 64-bit port of the game,
especially with the original 16-bit game embedded inside the VM
executable.

![](/img/stars.png)

The [signed][] releases on GitHub embed a compressed copy of the original
16-bit game, so that single EXE is ready to play out-of-the-box with no
further setup or downloads. I'm distributing 32-bit builds (but requires
SSE2) because there's no advantage to 64-bit here, and these builds work
(almost) everywhere _except_ 16-bit Windows. 32-bit Windows could run the
original 16-bit game, but the VM-encapsulated version is better behaved.
It doesn't dump a `Stars.ini` under `C:\WINDOWS`, it interacts properly
with the task bar, and copy protection is neutralized via the OS bridge.

If you ever been curious about Stars!, now's the time to try it. The game
has a thorough, built-in tutorial, but also check out the [wiki][], the
[official strategy guide][ssg], and [AutoHost][] (play-by-email service).
The game predates the modern search engine concept, otherwise they might
have chosen a better name. I suggest using "stars 4x" in your searches.

If you want to build from source and hack on the VM yourself, the best
tool for the job is [w64devkit][], of course, because it [comes with
everything you'll need][]. Plus the game itself: `stars.exe` from
[`stars27jrc3.zip`][dl].

### Implementation details

The emulator itself requires x86 or x86-64 because it does not implement
x87 (80-bit floating point) in software, but instead runs these operation
directly on the host's x87 hardware. This is simple, fast, and precise.
The project validates the emulation as a whole with a differential fuzzer
against the host. The fuzzer randomly generates a 16-bit instruction,
emulates it, then runs it with [JIT][] on the host and compares the
results.

Handles on Windows are pointer-sized, and so the Win16-to-Win32 bridge
maps 16-bit handles to host handles. It marshals between different struct
layouts when translating these calls, services the DOS interrupts the game
requires, an copies data in and out of guest memory. It's rather like
running a [Wasm][] instance, which of course makes sense in retrospect.

The Win32 bridge is also monitorable and manipulatable using the Model
Context Protocol (MCP). AI agents can "see" the UI "DOM" as it's built,
and can drive it by injecting synthetic events into the event pump, all
without going through the usual desktop control. The MCP can also read and
write guest memory. Opus 5 played a complete game through MCP — which is
quite fun to watch — requesting my assistance at just two points when it
got stuck in the UI. A foundation for a new Stars!Bench?

Targeting old 16-bit computers, the authors couldn't afford to build a
sloppy, wasteful UI, and so by modern standards the game UI is remarkably
fast and responsive. They don't make 'em like they used to. Computing the
next turn, or "turn generation," is the computational bottleneck, and so
that's where I focused my optimization efforts. The emulator can trace
executed instructions, so I gathered traces of turn generation, then had
Fable 5.1 identify and reverse engineer the hottest common routines (e.g.
the game's L'Ecuyer MCG PRNG) and basic blocks. Each was re-written in C
and mapped into the instruction decoder as new 80286 instructions. On load
the emulator identifies these routines and patches them with the new
instruction. This resulted in a nearly ~2x speedup of turn generation. By
exploiting local conditions, emulating a particular known program, I get
JIT performance without JIT complexity.

The original game doesn't use [buffered I/O][buf], and instead issues many
small reads and writes. Passing these small reads/writes straight to Win32
made I/O take ~5% turn generation time, probably worse today than it was
back then. Plus it's just rude. The emulator buffers the game's I/O calls,
further speeding up turn generation.

The game has some sound effects in the "battle VCR" and the final version
of the game shipped with Microsoft's `WaveMix.dll`. Rather than load and
link this DLL, the emulator implements the DLL's interfaces natively, and
these routines are dynamically linked into the 16-bit process. You will
not need this DLL with the emulator, nor is it embedded in releases.

The game also has art assets embedded uncompressed in the original EXE,
forming the bulk of its ~3MB. Stars!VM uses a custom LZ-based compression
algorithm tailored to compressing the original game. It's compressed when
embedded in releases. So the 32-bit version of the game is half the size
of the original, at ~1.5MB.

The original game requires a serial code, serving as its copy protection.
A code is 8 alpha-numeric characters that must pass two checks. Failing
the first is loud, but failing the second will sabotage your game with
penalties. You'll know because it will announce that your people suspect
you are a usurper. Most codes you'll find online are such "usurper" codes.
Play-by-email (PBEM) saves embed a hardware signature derived from C and D
drive configuration. People playing on different machines using the same
serial code recieve the usurper penalty.

I bought a serial code back in the day, but it hasn't been possible to
purchase one a for at least decade now. So the emulator injects a fixed
serial code on first run (disable with `--prompt-serial`), and you won't
need to worry about it. The VM also produces a fixed hardware signature
(same as any other emulator), so it looks like everyone running Stars!VM
is sharing the a machine, meaning no penalty for key reuse. I cracked the
serial code checks anyway, allowing me discover interesting ones. My
favorites: CLONEMUM, CROSSNUT, EGGSWAIN, GHOSTKIN, GONKSHOW, GRIPMIME,
SEEKCAPS, SIFTBOLD, SIRBUOYS, SLIMFAZE, SLOTMOPS, SPAWNELK, SUNBLOND,
WANTNEAR, and WRONGPOX. These look like some of [my passwords][poker].

### Endless possibilities

I'm quite pleased and excited with the results, especially for a weekend
project. It's breathed life back into the game for me, not only having a
better experience running it, but also that I can trivially bend the game
to my will in the ways I dreamed about. A few hooks in the right places
should open the game to easy modding, but I'm more engineer than modder.


[4X]: https://en.wikipedia.org/wiki/4X
[80286]: /blog/2014/12/09/
[AutoHost]: https://starsautohost.org/stars.htm
[DOSBox]: https://www.dosbox.com/
[JIT]: /blog/2015/03/19/
[Stars!VM]: https://github.com/skeeto/StarsVM
[Stars!]: https://en.wikipedia.org/wiki/Stars!
[Wasm]: /blog/2026/01/01/
[Wine]: https://www.winehq.org/
[buf]: /blog/2023/02/13/
[dl]: https://wiki.starsautohost.org/wiki/Downloads
[everything]: /blog/2020/09/25/
[otvdm]: https://github.com/otya128/winevdm
[poker]: /blog/2017/07/27/
[signed]: /blog/2026/04/25/
[ssg]: https://starsautohost.org/strategy/guidef/SSG.htm
[w64devkit]: https://github.com/skeeto/w64devkit
[wiki]: https://wiki.starsautohost.org/
