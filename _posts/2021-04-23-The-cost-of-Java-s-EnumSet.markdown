---
title: The cost of Java's EnumSet
layout: post
date: 2021-04-23T21:20:53Z
tags: [java]
uuid: ae95c24d-ab40-4f5f-a375-3e4c1ee58c27
---

It's been about a decade since I last worked in Java and much has changed.
I thought I'd brush up by re-reading *Effective Java* by Joshua Bloch
which has since received a new edition. It was once my guiding star for
Java. However, after an additional decade of experience in a variety of
languages and platforms, this book frequently made me shake my head. I
strongly disagreed with 20% of [its items][nutshell]. One conflicting
topic was enumerations, particularly item 36: "Use EnumSet instead of bit
fields."

This is not to say Bloch is necessarily wrong to make this recommendation,
or any of the others I don't like. The book is conservative, playing it
safe by always erring on the side of Java idiom whether or not it makes
sense. The problem lies in Java idiom more than the book. To write your
Java like everyone else, do exactly as the book says. When in Rome, do as
the Romans do.

On the other hand, EnumSet is perhaps the most pointless class in the Java
standard library. It has two goals, but fails at both.

### Background

Back in the old days Java programmers would build enumerations much like C
programmers. For example, here's a C-style bitfield enumeration (these
Romans *love* their keywords):

```java
public final class Color {
    public static final int R = 1 << 0;
    public static final int G = 1 << 1;
    public static final int B = 1 << 2;
}
```

To build a *set* of these items, use the OR (`|`) operator, just like C:

```java
int purple = Color.R | Color.B;
```

The downside is lack of type safety. These are plain old integers, not a
dedicated type, and getting your integers and bitfields crossed won't be
checked by the compiler. To solve this, early Java gained an enumeration
reference type:

```java
public enum Color { R, G, B }
```

Thankfully this is only slightly more verbose than the same syntax for a C
enumeration. While it would have been useful, these types don't support
the OR operator. Instead you're supposed to build a Set. Romans also love
ceremony:

```java
Set<Color> purple = new HashSet<>(Arrays.asList(Color.R, Color.B));
```

As you might guess, compared to the original integer operation, this
HashSet is incredibly slow and inefficient. The type safety comes at a
serious cost. Attempting to mitigate this, Java provides a special Set
implementation for enumerations:

```java
Set<Color> purple = EnumSet.of(Color.R, Color.B);
```

Not as syntactically lean as the OR operator, but less ceremony and more
efficient than a HashSet. The efficiency comes from using a bitfield
internally just like the original pre-enumeration example. *But how much
more efficient is it?*

### Benchmarks

The original C-style bitfield was a primitive `int`: fast, efficient, no
allocations, and easy to optimize. Except for the lack of type safety it's
essentially the best possible case. Since an EnumSet uses a bitfield
internally, isn't it basically the same? Unfortunately not.

An EnumSet is a reference type, not a primitive, so creating an EnumSet
requires:

* Memory allocation
* Running a constructor
* Run-time construction
* Reflection

It's the essence of [**individual element thinking**][cm]. There's little
reason to think an EnumSet is going to be efficient.

Wanting to get a feel for the relative costs, I put together some crude
benchmarks. In the benchmark I construct a set of values, then construct
the same set many more times and compare it to the original set. Here's
the EnumSet benchmark:

```java
enum Flag { A, B, C, D, E, F, G }

// ...

static void benchmarkEnumSet() {
    System.gc();
    long beg = System.nanoTime();
    Set<Flag> a = EnumSet.of(Flag.A, Flag.B, Flag.G);
    for (int i = 0; i < 1_000_000_000; i++) {
        Set<Flag> b = EnumSet.of(Flag.A, Flag.B, Flag.G);
        assert a.equals(b);
    }
    long end = System.nanoTime();
    System.out.println("EnumSet\t" + (end - beg)/1e9);
}
```

A benchmark for a classical bitfield:

```java
static final int A = 1 << 0;
static final int B = 1 << 1;
static final int C = 1 << 2;
static final int D = 1 << 3;
static final int E = 1 << 4;
static final int F = 1 << 5;
static final int G = 1 << 6;

// ...

static void benchmarkBitfield() {
    System.gc();
    long beg = System.nanoTime();
    int a = A | B | G;
    for (int i = 0; i < 1_000_000_000; i++) {
        int b = A | B | G;
        assert a == b;
    }
    long end = System.nanoTime();
    System.out.println("bitfield\t" + (end - beg)/1e9);
}
```

There's also a HashSet benchmark, but it's just a slight variation of the
EnumSet benchmark so I won't show it here. Due to JIT warm-up costs, the
benchmark runs three times in a row in the same process (`for` loop). The
`-ea` option enables the assertions in the test:

    java -ea Benchmark
    
Full source: [Benchmark.java][gist]

The results on x86-64 Debian Buster with its OpenJDK 11 (the most recent
long-term support release of OpenJDK):

    HashSet    104.486260884
    EnumSet      3.900099588
    bitfield     0.003371834

    HashSet    109.827488593
    EnumSet      3.484818891
    bitfield     0.003430366

    HashSet    107.106317379
    EnumSet      3.742689517
    bitield      0.000000057

An EnumSet is two orders of magnitude faster than a HashSet. This sounds
pretty good until the next result: At their worst, bitfields are *three
orders of magnitude* faster than an EnumSet.

What's even more interesting is that third run. It looks like a benchmark
failure, and in another context I might agree. Obviously the JIT compiler
wised up and optimized away the entire benchmark. Normally this is a
useless result, but it's telling in contrast with the other two
benchmarks. The compiler failed to realize this same optimization with
both the HashSet and EnumSet benchmarks. **Once warmed up, bitfields are
*more than* 1000x faster** because they won't inhibit optimizations.

### Two goals

So what's the point of an EnumSet? I mentioned that it doesn't accomplish
either of its two goals.

1. A bitfield lacks type safety. A Set (via generics) does not.
2. The usual Set implementation, HashSet, is far less efficient than a
   bitfield. An EnumSet attempts to bring this back in line with a
   bitfield.

An EnumSet is unnecessary for type safety since that's already a property
of Set. We already have a more general Set implementation: HashSet.
Relative to a bitfield, an EnumSet isn't meaningfully faster than a
HashSet. A hare isn't meaningfully faster than a turtle in the context of
rockets.

The usual argument against bitfields is that "speed isn't important" or
that it's "premature optimization." If that's true, then what was wrong
with a HashSet? If speed *is* important, then you use a bitfield. Where's
the need for an EnumSet? It's extra API surface area for a non-existent
use case.

It's also at odds with "Item 6: Avoid creating unnecessary objects."
Unlike a Set, a bitfield is a primitive and doesn't create an object. Per
the benchmark, the type safety of a Set comes at a high cost. The C
programmer within me cringes at that cost even where it truly doesn't
matter.

This isn't to say you should change the way you write Java. This is a
criticism of the ecosystem — its design and idioms — and one of the (many)
reasons why I haven't missed it.


[cm]: https://www.youtube.com/watch?v=f4ioc8-lDc0&t=4407s
[gist]: https://gist.github.com/skeeto/db52f44f99f94b222f35e2a771da3a71
[nutshell]: https://github.com/nicolasmanic/effective-java-in-a-nutshell
