---
title: Why Aren't There C Conferences?
layout: post
date: 2018-11-21T17:25:45Z
tags: [c]
uuid: 78211c4b-f553-313d-659f-15cda1339893
---

*This article was [discussed on Hacker News][hn].*

Most widely-used programming languages have at least one regular
conference dedicated to discussing it. Heck, even [Lisp has
one][lisp]. It's a place to talk about the latest developments of the
language, recent and upcoming standards, and so on. However, C is a
notable exception. Despite [its role as the foundation][essay] of the
entire software ecosystem, there aren't any regular conferences about
C. I have a couple of theories about why.

First, C is so fundamental and ubiquitous that a conference about C
would be too general. There are so many different uses ranging across
embedded development, operating system kernels, systems programming,
application development, and, most recently, web development
(WebAssembly). It's just not a cohesive enough topic. Any conference
that might be about C is instead focused on some particular subset of
its application. It's not a C conference, it's a database conference,
or an embedded conference, or a Linux conference, or a BSD conference,
etc.

Second, C has a tendency to be conservative, changing and growing very
slowly. This is a feature, and one that is often undervalued by
developers. (In fact, I'd personally like to see a future revision
that makes the C language specification *smaller* and *simpler*,
rather than accumulate more features.) The last major revision to C
happened in 1999 (C99). There was a minor revision in 2011 (C11), and
an even smaller revision in 2018 (C17). If there was a C conference,
recent changes to the language wouldn't be a very fruitful topic.

However, the *tooling* has advanced significantly in recent years,
especially with the advent of LLVM and Clang. This is largely driven
by the C++ community, and C has significantly benefited as a side
effect due to its overlap. Those are topics worthy of conferences, but
these are really C++ conferences.

The closest thing we have to a C conference every year is CppCon. A
lot of CppCon isn't *really* just about C++, and the subjects of many
of the talks are easily applied to C, since C++ builds so much upon C.
In a sense, **a subset of CppCon could be considered a C conference**.
That's what I'm looking for when I watch the CppCon presentations each
year on YouTube.

Starting last year, I began a list of all the talks that I thought
would be useful to C programmers. Some are entirely relevant to C,
others just have significant portions that are relevant to C. When
someone asks about where they can find a C conference, I send them my
list.

I'm sharing them here so you can bookmark this page and never return
again.

### 2017

Here's the list for CppCon 2017. These are *roughly* ordered from
highest to lowest recommendation:

* [Matt Godbolt “What Has My Compiler Done for Me Lately? Unbolting the Compiler's Lid”](https://www.youtube.com/watch?v=bSkpMdDe4g4)
* [Chandler Carruth “Going Nowhere Faster”](https://www.youtube.com/watch?v=2EWejmkKlxs)
* [James McNellis “Everything You Ever Wanted to Know about DLLs”](https://www.youtube.com/watch?v=JPQWQfDhICA)
* [John Regehr “Undefined Behavior in 2017 (part 1 of 2)”](https://www.youtube.com/watch?v=v1COuU2vU_w)
* [John Regehr “Undefined Behavior in 2017 (part 2 of 2)”](https://www.youtube.com/watch?v=TPyLrJED0zQ)
* [Piotr Padlewski “Undefined Behaviour is awesome!” ](https://www.youtube.com/watch?v=ehyHyAIa5so)
* [Tobias Fuchs “Multidimensional Index Sets for Data Locality in HPC Applications”](https://www.youtube.com/watch?v=1HqY9dPccMI)
* [John D. Woolverton “C Pointers”](https://www.youtube.com/watch?v=iJ1rwgCI1Xc)
* [Charles Bailey “Enough x86 Assembly to Be Dangerous”](https://www.youtube.com/watch?v=IfUPkUAEwrk)
* [Tony Van Eerd “An Interesting Lock-free Queue - Part 2 of N”](https://www.youtube.com/watch?v=HP2InVqgBFM)
* [Matt Kulukundis “Designing a Fast, Efficient, Cache-friendly Hash Table, Step by Step”](https://www.youtube.com/watch?v=ncHmEUmJZf4)
* [Fedor Pikus “Read, Copy, Update, then what? RCU for non-kernel programmers”](https://www.youtube.com/watch?v=rxQ5K9lo034)
* [Ansel Sermersheim “Multithreading is the answer. What is the question? (part 1 of 2)”](https://www.youtube.com/watch?v=GNw3RXr-VJk)
* [Ansel Sermersheim “Multithreading is the answer. What is the question? (part 2 of 2)”](https://www.youtube.com/watch?v=sDLQWivf1-I)
* [Carl Cook “When a Microsecond Is an Eternity: High Performance Trading Systems in C++”](https://www.youtube.com/watch?v=NH1Tta7purM)
* [Alfred Bratterud “Deconstructing the OS: The devil’s In the side effects”](https://www.youtube.com/watch?v=h7D88U-5pKc) ([counterpoint](https://www.joyent.com/blog/unikernels-are-unfit-for-production))
* [P. McKenney, M. Michael & M. Wong “Is Parallel Programming still hard? PART 1 of 2”](https://www.youtube.com/watch?v=YM8Xy6oKVQg)
* [P. McKenney, M. Michael & M. Wong “Is Parallel Programming still hard? PART 2 of 2”](https://www.youtube.com/watch?v=74QjNwYAJ7M)
* [Adrien Devresse “Nix: A functional package manager for your C++ software stack”](https://www.youtube.com/watch?v=6wJ4-wP-nnA)
* [Mathieu Ropert “API & ABI Versioning...”](https://www.youtube.com/watch?v=Ia3IDPjA-d0)
* [Paul Blinzer “Heterogeneous System Architecture - Why Should You Care?”](https://www.youtube.com/watch?v=CVAVKIe7CnY)
* [Mathieu Ropert “Using Modern CMake Patterns to Enforce a Good Modular Design”](https://www.youtube.com/watch?v=eC9-iRN2b04)
* [Allan Deutsch “Esoteric Data Structures and Where to Find Them”](https://www.youtube.com/watch?v=-8UZhDjgeZU)
* [D. Rodriguez-Losada Gonzalez “Faster Delivery of Large C/C++ Projects with...”](https://www.youtube.com/watch?v=xA9yRX4Mdz0)

### 2018

The final CppCon 2018 videos were uploaded this week, so my 2018
listing can be complete:

* [Robert Schumacher “Don't package your libraries, write packagable libraries!”](https://www.youtube.com/watch?v=sBP17HQAQjk)
* [Stoyan Nikolov “OOP Is Dead, Long Live Data-oriented Design”](https://www.youtube.com/watch?v=yy8jQgmhbAU)
* [Fedor Pikus “Design for Performance”](https://www.youtube.com/watch?v=m25p3EtBua4)
* [JF Bastien “Signed integers are two's complement”](https://www.youtube.com/watch?v=JhUxIVf1qok)
* [Alan Talbot “Moving Faster: Everyday efficiency in modern C++”](https://www.youtube.com/watch?v=EovBkh9wDnM)
* [Geoffrey Romer “What do you mean "thread-safe"?”](https://www.youtube.com/watch?v=s5PCh_FaMfM)
* [Chandler Carruth “Spectre: Secrets, Side-Channels, Sandboxes, and Security”](https://www.youtube.com/watch?v=_f7O3IfIR2k)
* [Bob Steagall “Fast Conversion From UTF-8 with C++, DFAs, and SSE Intrinsics”](https://www.youtube.com/watch?v=5FQ87-Ecb-A)
* [Nir Friedman “Understanding Optimizers: Helping the Compiler Help You”](https://www.youtube.com/watch?v=8nyq8SNUTSc)
* [Barbara Geller & Ansel Sermersheim “Undefined Behavior is Not an Error” ](https://www.youtube.com/watch?v=XEXpwis_deQ)
* [Matt Godbolt “The Bits Between the Bits: How We Get to main()”](https://www.youtube.com/watch?v=dOfucXtyEsU)
* [Mike Shah “Let's Intercept OpenGL Function Calls...for Logging!”](https://www.youtube.com/watch?v=DMNFb5ycpNY)
* [Kostya Serebryany “Memory Tagging and how it improves C/C++ memory safety”](https://www.youtube.com/watch?v=lLEcbXidK2o)
* [Patricia Aas “Software Vulnerabilities in C and C++” ](https://www.youtube.com/watch?v=0S0QgQd75Sw)
* [Patricia Aas “Make It Fixable: Preparing for Security Vulnerability Reports”](https://www.youtube.com/watch?v=IupP8AFrOJk)
* [Greg Law “Debugging Linux C++”](https://www.youtube.com/watch?v=V1t6faOKjuQ)
* [Simon Brand “How C++ Debuggers Work”](https://www.youtube.com/watch?v=0DDrseUomfU)
* [Odin Holmes “Concurrency Challenges of Interrupt Service Routines”](https://www.youtube.com/watch?v=gcRdG7dGMOw)

There were three talks strictly about C++ that I thought were
interesting from a language design perspective. So I think they're
worth recommending, too. (In fact, they're a sort of ammo *against*
using C++ due to its insane complexity.)

* [Nicolai Josuttis “The Nightmare of Initialization in C++”](https://www.youtube.com/watch?v=7DTlWPgX6zs)
* [Timur Doumler “Can I has grammar?”](https://www.youtube.com/watch?v=tsG95Y-C14k)
* [Richard Powell “How to Argue(ment)"](https://www.youtube.com/watch?v=ZbVCGCy3mGQ)

### 2019

Only three this year. The last is about C++, but I thought it was
interesting.

* [JF Bastien "Deprecating volatile"](https://www.youtube.com/watch?v=KJW_DLaVXIY)
* [J. Bialek, S. Block “Killing Uninitialized Memory: Protecting the OS Without Destroying Performance”](https://www.youtube.com/watch?v=rQWjF8NvqAU)
* [Matt Godbolt “Path Tracing Three Ways: A Study of C++ Style”](https://www.youtube.com/watch?v=HG6c4Kwbv4I)

### 2020

Four more worthwhile talks in 2020. The first is about the C++ abstract
machine, but is nearly identical to the C abstract machine. The second is
a proverbial warning about builds. The rest are about performance, and
while the context is C++ the concepts are entirely applicable to C.

* [Bob Steagall "Back to Basics - The Abstract Machine"](https://www.youtube.com/watch?v=ZAji7PkXaKY)
* [Dave Steffen "Build Everything From Source - A Case Study in Fear"](https://www.youtube.com/watch?v=54uVTkhinDE)
* [Bob Steagall "Adventures in SIMD-Thinking"](https://www.youtube.com/watch?v=qejTqnxQRcw) ([part 2](https://www.youtube.com/watch?v=qXleSwCCEvY))
* [Alex Reinking "Halide - A Language for Fast, Portable Computation on Images and Tensors"](https://www.youtube.com/watch?v=1ir_nEfKQ7A)

### 2021 and 2022

CppCon's current sponsor interferes with scheduling and video releases,
deliberately reducing accessibility to the outside (unlisted videos,
uploading talks multiple times, etc.). Since it's too time consuming to
track it all myself, I've given up on following CppCon, at least until
they get better-behaved sponsor.

### Bonus

Finally, here are a few more good presentations from other C++
conferences which you can just pretend are about C:

* [CppCon 2016: Chandler Carruth “Garbage In, Garbage Out: Arguing about Undefined Behavior..."](https://www.youtube.com/watch?v=yG1OZ69H_-o)
* [CppCon 2014: Mike Acton "Data-Oriented Design and C++"](https://www.youtube.com/watch?v=rX0ItVEVjHc) (this is a personal favorite of mine)
* [CppCon 2014: Chandler Carruth "Efficiency with Algorithms, Performance with Data Structures"](https://www.youtube.com/watch?v=fHNmRkzxHWs)
* [CppCon 2015: Chandler Carruth "Tuning C++: Benchmarks, and CPUs, and Compilers! Oh My!"](https://www.youtube.com/watch?v=nXaxk27zwlk)
* [CppCon 2016: Chandler Carruth “High Performance Code 201: Hybrid Data Structures"](https://www.youtube.com/watch?v=vElZc6zSIXM)
* [Meeting Cpp 2015: Chandler Carruth: Understanding Compiler Optimization](https://www.youtube.com/watch?v=FnGCDLhaxKU)
* [BoostCon 2013: Chandler Carruth: Optimizing the Emergent Structures of C++ ](https://www.youtube.com/watch?v=eR34r7HOU14)
* [CppCon 2016: Nicholas Ormrod “The strange details of std::string at Facebook" ](https://www.youtube.com/watch?v=kPR8h4-qZdk)
* [Handmade Seattle 2021: Context is Everything: Andreas Fredriksson](https://vimeo.com/644068002)


[essay]: https://skeeto.s3.amazonaws.com/share/onward17-essays2.pdf
[hn]: https://news.ycombinator.com/item?id=18504879
[lisp]: https://www.european-lisp-symposium.org/
