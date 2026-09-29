---
title: Java is Death By A Thousand Paper Cuts
layout: post
tags: [java, rant]
uuid: beca38a0-bd9d-3304-02c9-1941ced67121
---

<!-- 13 August 2010 -->

At least it is for me. This past week at work I've been furiously
rushing work on a project written in Java. It's completely my fault
that I'm using Java since I'm the one who picked the language. I
wanted to use Java the platform, but I don't know any other JVM
languages well enough to use them in place of Java the language for a
project at work.

It's all sorts of little things that make writing Java so exhausting
for me. At the end of the day it I just feel irritated. I hate the
absolute lack of functional programming and that I have to specify
everything at such a low level. The whole reason we program in
high-level languages is so we can express algorithms more concisely,
but Java fails at this.

Here's an example of what I'm talking about, something I basically did
a few times today. Let's say you have an array of
floats, `nums` and you want to sum them and return the
result (or maybe use it in another expression). In Lisp it's very
straightforward.

```cl
(reduce '+ nums)
```

"Reduce the sequence `nums` by addition." Notice that it's
more about saying *what* I want to do rather than *how* to
do it. I don't have to introduce any temporary storage or
iterators. To do the same thing in Java it will look something like
this.

```java
int sum = 0;
for (double num : nums) {
    sum += num;
}
return sum;
```

If you're using an older Java without the enhanced looping construct
it gets even uglier. I had to introduce a variable for accumulation
and a second variable for iteration. This sort of thing has to be
done *all over the place* in Java, and it greatly increases the
cognitive overload when reading Java code.

This instruction is more about telling the computer the *how*
rather than my overall intention. One problem with telling it
the *how* is that I've unnecessarily locked in a specific
algorithm and ordering. The literal instruction says that the
numbers *must* be added in order, sequentially. My Lisp
instruction doesn't do that.

It gets even worse when you complicate it slightly by adding a second
array and, say, multiplying it pairwise with the first.

```java
for (int i = 0; i < nums1.length; i++) {
    sum += nums1[i] * nums2[i];
}
return sum;
```

Now the loop gets more complex. I have to tell it *how* to
increment the iterator. I have to tell it to check the bounds of the
array. The iterator is a misdirection because the actual number stored
in it isn't what's important. Again, the Lisp method is much more
concise.

```cl
(reduce '+ (map 'list '* nums1 nums2))
```

"Map the two sequences by multiplication into a list, then reduce it
by addition." Unfortunately we start to leak a little bit into
the *how* here. I am telling it that the intermediate structure
should be a list, because `map` forces me to pick a
representation. Besides that, I am *only* describing my overall
intention and not the obvious details.

So with Java my days become filled with the tedious low-level
algorithm descriptions that I have to hammer out over and over and
over. Death by a thousand paper cuts.

Lisp isn't the only language that has a (generally) much better
approach; it's just my favorite. :-) Most languages with at least some
decent functional facilities will also do the above concisely.
