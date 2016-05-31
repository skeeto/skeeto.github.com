---
title: Live Systems are Handy
layout: post
tags: [debian]
uuid: e56188a1-1152-358f-882f-690cae8cf966
---

The [personal live system](/blog/2013/06/17/) I configured a couple
months ago has turned out to be *quite* handy. I've used it almost
every day, especially as I've learned more about the features of
live-boot and live-config, the runtime components of
[live-build](http://live.debian.net/).

At work I use a number of "scratch" computers whose only real purpose
is to temporarily sit at particular points on an experimental,
isolated network. It doesn't matter what operating system is installed
on them; they really just need to be able to communicate on the
network and, occasionally, run Wireshark.

For awhile these systems just had a base Ubuntu install from a couple
of years ago. This works fine, of course, but it was missing
[all of my dotfiles](/blog/2012/06/23/). It didn't feel like home. I
could put my dotfiles on these systems, but there are a number of
these machines and I'd have to keep my dotfiles up to date on each,
without any Internet access, as they changed.

More so, Unity, Ubuntu's default desktop environment, isn't all that
friendly to a keyboard-only setup. I don't have any mice attached to
these systems so I have to navigate without one. Sure, It has keyboard
shortcuts for the basic things, but that's not enough. On the other
hand, my personal configuration is entirely built around avoiding the
mouse. Except for a few specific, reasonable cases (graphics programs
like Gimp, Inkscape), I can do anything just as comfortably without as
I can with a mouse.

Here's where my live system comes to the rescue. **The generated ISO
is a "hybrid" image.** This means it can be both burned into a CD as a
standard ISO 9660 filesystem *and* written directly to a hard disk as
a read-only disk image. After `dd`ing the ISO onto a thumb drive, I
could plug the drive into these machines and boot directly into a
comfortable development environment. When I update my dotfiles
significantly I just build a new ISO (takes about 5 minutes) and copy
the new version onto the thumb drive.

### Running a live system from RAM

So the next problem is that there are a number of these systems but I
have only one thumb drive. Once I boot a machine from the thumb drive
I can't remove the drive and continue to use the system. I could burn
a bunch of CDs, one for each system, but, fortunately, live-boot has a
wonderful solution for this: `toram`.

That's right, just add `toram` to the system boot arguments and during
boot the entire system is lifted into memory. **Editing the system
arguments before booting can be done by hitting <kbd>tab</kbd> at the
isolinux/syslinux boot menu.** Once the system is up I'm free to
remove the thumb drive and use it to boot another system.
Additionally, the system will also be *much* faster — faster than
even a normal system — since it's running entirely in RAM.

The disadvantage is that there's now less memory available to the
system for other things. This can be mitigated by making use of a swap
partition on a local disk.

### Persistence

These live systems use an interesting filesystem called
[SquashFS][squashfs]. It's like a compressed tar archive, but it has
two significant advantages:

 * The contents are available for efficient random access.
 * It can be mounted as a first-class filesystem.

SquashFS is read-only. This presents a big problem if you want to use
it as your root filesystem. Luckily there's another cool trick to fix
this: a *union* filesystem, `aufs`. A writable filesystem can be
mounted over top the read-only SquashFS filesystem, unioning the
contents of the two filesystems, providing the illusion of a writable
SquashFS. It's effectively copy-on-write.

Typically for a live system the writable filesystem is `tmpfs`, a
filesystem backed by RAM. This means all changes are lost when the
operating system halts. This is useful for a throwaway operating
system, but sometimes data persistence between boots is required. This
time live-config has a solution: `persistence`. Add this word to the
system boot arguments and, rather than a RAM filesystem, a partition
on real media will be used to back changes to the SquashFS filesystem.

The next question would be, "Which partition?" That's part of a
discovery phase. During boot, the various storage media are scanned
looking for a filesystem labeled "persistence". If one is found, it
looks for a file in the root of that filesystem called
`persistence.conf`, which explains how to map this partition with the
live system. Full persistence can be achieved with this one-liner
configuration,

    / union

This means the root of this filesystem should be union mounted with
the live system. However, this will capture some extraneous
directories like `/tmp`, which I don't want to write to storage media.
I prefer a more nuanced configuration,

    /etc union
    /home union
    /opt union
    /sbin union
    /usr union
    /var union

This captures all the important stuff while leaving `/tmp` to a RAM
filesystem (where it belongs!). Not only is my home directory
persistent, but so is any additional software I install. Effectively,
it is now a normal, non-live system.

If the live system is installed on a thumb drive, the space *after*
the image can be used for this persistence. After `dd`ing the ISO, use
fdisk/parted to add a second partition, format a fresh partition
labeled "persistence" in it, then drop in config file.

    mkfs.ext4 -L persistence /dev/sdb2
    mount /dev/sdb2 /mnt/sdb2
    cp peristence.conf /mnt/sdb2

Furthermore, the persistence partition can be encrypted with LUKS,
providing a portable, persistent, encrypted operating system. The
live-config will automatically find it, prompt for the passphrase on
boot, and handle all the rest on its own like normal. (Well, once
[this bug fix is released][bug].)

#### Persistence as an installation

Remember how I had said the exact operating system for these scratch
computers wasn't important? While using one of these systems a
lightbulb went off in my head. After booting the live system, I could
just `dd` the contents of the thumb drive directly onto the hard disk,
*including the persistence partition*, make "persistence" a default
boot argument, and I'll have installed a fully configured operating
system with all my goodies within a few minutes! It's the ultimate
anti-thin client. This is what I'm using now.

Since it's possible to build a live system image within a live system
("Yo dawg ..."), I can use the `toram` boot option to update and/or
reinstall the entire system from scratch, in-place, without the help
of external storage.

Live systems are a powerful tool, at least for my line of work.


[bug]: http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=700902
[squashfs]: http://en.wikipedia.org/wiki/SquashFS
