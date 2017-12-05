---
title: Introducing the Pokerware Secure Passphrase Generator
layout: post
date: 2017-07-27T17:49:10Z
tags: [crypto, meatspace, netsec]
uuid: c2d33d1a-d2a2-3863-04ae-68d2b48eecd5
---

I recently developed [**Pokerware**][pokerware], an offline passphrase
generator that operates in the same spirit as [Diceware][diceware].
The primary difference is that it uses a shuffled deck of playing
cards as its entropy source rather than dice. Draw some cards and use
them to select a uniformly random word from a list. Unless you're some
sort of [tabletop gaming nerd][dd], a deck of cards is more readily
available than five 6-sided dice, which would typically need to be
borrowed from the Monopoly board collecting dust on the shelf, then
rolled two at a time.

There are various flavors of two different word lists here:

* <https://github.com/skeeto/pokerware/releases/tag/1.0>

Hardware random number generators are [difficult to verify][verify]
and may not actually be as random as they promise, either
intentionally or unintentionally. For the particularly paranoid,
Diceware and Pokerware are an easily verifiable alternative for
generating secure passphrases for [cryptographic purposes][enchive].
At any time, a deck of 52 playing cards is in one of 52! possible
arrangements. That's more than 225 bits of entropy. If you give your
deck [a thorough shuffle][shuffle], it will be in an arrangement that
has never been seen before and will never be seen again. Pokerware
draws on some of these bits to generate passphrases.

The Pokerware list has 5,304 words (12.4 bits per word), compared to
Diceware's 7,776 words (12.9 bits per word). My goal was to invent a
card-drawing scheme that would uniformly select from a list in the same
sized ballpark as Diceware. Much smaller and you'd have to memorize more
words for the same passphrase strength. Much larger and the words on the
list would be more difficult to memorize, since the list would contain
longer and less frequently used words. Diceware strikes a nice balance
at five dice.

<!-- Photo credit: Kelsey Wellons -->
![](/img/pokerware/deck.jpg)

One important difference for me is that *I like my Pokerware word
lists a lot more* than the two official Diceware lists. My lists only
have simple, easy-to-remember words (for American English speakers, at
least), without any numbers or other short non-words. Pokerware has
two official lists, "formal" and "slang," since my early testers
couldn't agree on which was better. Rather than make a difficult
decision, I took the usual route of making no decision at all.

The "formal" list is derived in part from [Google's Ngram
Viewer][ngram], with my own additional filters and tweaking. It's called
"formal" because the ngrams come from formal publications and represent
more formal kinds of speech.

The "slang" list is derived from [*every* reddit comment][db] between
December 2005 and May 2017, tamed by the same additional filters. I
[have this data on hand][reddit], so I may as well put it to use. I
figured more casually-used words would be easier to remember. Due to
my extra filtering, there's actually a lot of overlap between these
lists, so the differences aren't too significant.

If you have your own word list, perhaps in a different language, you
can use the Makefile in the repository to build your own Pokerware
lookup table, both plain text and PDF. The PDF is generated using
Groff macros.

### Passphrase generation instructions

1. Thoroughly shuffle the deck.

2. Draw two cards. Sort them by value, then suit. Suits are in
   alphabetical order: Clubs, Diamonds, Hearts, Spades.

3. Draw additional cards until you get a card that doesn't match the
   face value of either of your initial two cards. Observe its suit.

4. Using your two cards and observed suit, look up a word in the table.

5. Place all cards back in the deck, shuffle, and repeat from step 2
   until you have the desired number of words. Each word is worth 12.4
   bits of entropy.

A word of warning about step 4: If you use software to do the word list
lookup, beware that it might save your search/command history — and
therefore your passphrase — to a file. For example, the `less` pager
will store search history in `~/.lesshst`. It's easy to prevent that
one:

    $ LESSHISTFILE=- less pokerware-slang.txt

#### Example word generation

Suppose in step 2 you draw King of Hearts (KH/K♥) and Queen of Clubs
(QC/Q♣).

![](/img/pokerware/kh.png){: .card}
![](/img/pokerware/qc.png){: .card}
{: .grid}

In step 3 you first draw King of Diamonds (KD/K♦), discarding it because
it matches the face value of one of your cards from step 2.

![](/img/pokerware/kd.png){: .card}
{: .grid}

Next you draw Four of Spades (4S/4♠), taking spades as your extra suit.

![](/img/pokerware/4s.png){: .card}
{: .grid}

In order, this gives you Queen of Clubs, King of Hearts, and Spades:
QCKHS or Q♣K♥♠. This corresponds to "wizard" in the formal word list and
would be the first word in your passphrase.

#### A deck of cards as an office tool

I now have an excuse to keep a deck of cards out on my desk at work.
I've been using Diceware — or something approximating it since I'm not
so paranoid about hardware RNGs — for passwords [for over 8 years
now][old]. From now I'll deal new passwords from an in-reach deck of
cards. Though typically I need to tweak the results to meet [outdated
character-composition requirements][passwords].


[dd]: /blog/2011/01/10/
[diceware]: http://world.std.com/~reinhold/diceware.html
[enchive]: /blog/2017/03/12/
[ngram]: https://books.google.com/ngrams
[old]: /blog/2009/02/07/
[passwords]: https://www.troyhunt.com/passwords-evolved-authentication-guidance-for-the-modern-era/
[pokerware]: https://github.com/skeeto/pokerware
[reddit]: /blog/2016/12/01/
[shuffle]: https://possiblywrong.wordpress.com/2011/03/27/card-shuffling-youre-not-done-yet/
[verify]: https://lwn.net/Articles/629714/
[db]: http://files.pushshift.io/reddit/
