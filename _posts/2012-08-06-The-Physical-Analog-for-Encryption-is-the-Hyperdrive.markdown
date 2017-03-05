---
title: The Physical Analog for Encryption is the Hyperdrive
layout: post
tags: [crypto, compsci]
uuid: 89cd4041-90bf-3536-3bda-c1fe56e26383
---

I was recently
[watching GetDaved play](http://www.youtube.com/playlist?list=PL80F8C1F2AE9B29DD)
through
[X-Wing Alliance](http://en.wikipedia.org/wiki/Star_Wars:_X-Wing_Alliance),
a game I myself played in college. I have a lot of nostalgia for it,
especially because
[TIE Fighter](http://en.wikipedia.org/wiki/Star_Wars:_TIE_Fighter) was
the first games I ever invested a lot of time into playing. Just
hearing the sounds and music brings back relaxing memories.

In one of the early missions the player travels through hyperspace
(which ain't like dusting crops)
[to a storage area](http://youtu.be/SeB1sn_6Zhk) located in deep
space. It's a family business and the player is out there to take
inventory of storage containers. Like when I
[saw the wormhole minefield in Deep Space 9](/blog/2008/12/16/), it
got me thinking, "*Why?*" Why keep all these storage containers in
deep space? There's no defense or security out there to stop someone
from stealing containers. It seems like it would be better to store
those at the home base where they can be protected.

![](/img/misc/deep-space.jpg)

Storing items at random locations in deep space is actually *very*
secure — more so than any lock! Space is *huge*. Even with
faster-than-light travel searching a galaxy for a storage location
would be impractical. It would be as impractical as using brute-force
to find an encryption key — another huge search space. Also, if the
storage location as been in use for *X* years, you'd need to come
within *X* light-years of it, at least, in order to find it, since
even gravity itself is limited by the speed of light.

Physical locks are usually described as the physical analogy of
cryptography. Honestly, it's not a very good analogy. The brute-force
method for bypassing a lock isn't to keep trying different keys or
combinations until it works. No, it's to just smash something (a
window, the lock) or pick the lock. When translated back into the
crypto world that's like breaking a cipher, which isn't a practical
attack in modern cryptography.

No, the physical analogy for cryptography is deep space storage. The
only practical way to access deep space items is to learn the
coordinates of the storage location, which is the equivalent of the
encryption key. If the coordinates are lost or forgotten, the items
are as good as destroyed, just like data.

There are actually some advantages of physical "encryption."
Ciphertext can be decrypted offline without being detected. It's not
possible to visit deep space storage without having a physical
presence, which is certainly more detectable than offline
decryption. There's also the advantage that it's somewhat easier to
tell when the key (location) generation algorithm is busted or you're
just bad at picking passphrases: someone else's stuff will already be
there. A *literal* collision.
