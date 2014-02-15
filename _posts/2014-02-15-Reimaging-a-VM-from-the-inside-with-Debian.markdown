---
title: Reimaging a VM from the inside with Debian
layout: post
date: 2014-02-15T03:26:45Z
tags: [debian]
uuid: b8708360-8720-3757-142d-ada37953cf15
---

Recently I was in a situation where I wanted to spin up a virtual
machine in a cloud computing service but the provided operating system
selection was meager. The two options were Ubuntu 11.10 (i.e. October
2011) or RHEL 6.3 (June 2012). Worse, these were the desktop versions
of these platforms, so they had X.Org installed along with a bunch of
desktop applications. These images were not really intended as
servers.

As always, I strongly prefer [Debian][deb]. Ubuntu is derived from
Debian so it isn't far from ideal, but I didn't want to deal with such
an old version, plus all the desktop cruft. Also, I *really just want
to use Debian Wheezy* (currently stable). It's extremely solid and I
know it through and through.

Since there's no way for me to provide my own image, my only option is
to install Debian from within the virtual machine. A significant
difficulty in this is that I also have no way to provide alternate
boot media. I would need to install Debian from within Ubuntu,
*over top* of Ubuntu *while* it's running. Within these restrictions
this may sound like an impossible task.

Fortunately, Debian, being *the* universal operating system and living
up to its name, has a way to do this, and do it cleanly. When I'm done
there will be absolutely *no* trace of Ubuntu left. I could do the
same from within the Red Hat system, but working from Ubuntu takes one
fewer step. The magic tool for solving this problem is
[**debootstrap**][debootstrap]. It installs a Debian base system into
an arbitrary subdirectory. This is what the official Debian installer
uses and it's used to [build my live CD][live]. Ubuntu offers this as
a normal package, so I don't need to download anything special.

### Creating a Pivot

So I've got a way to install Debian from within another Linux
installation, but now how can I install Debian over top of Ubuntu? I
will ultimately need to use debootstrap on the root of a fresh
filesystem. I can't wipe Ubuntu's root partition while it's running. I
can't even resize it since that's where `/` is mounted.

Fortunately for me the person who set up this VM just went through
Ubuntu's defaults. This means there's a single primary partition
holding the entire Ubuntu installation (sda1), a second extended
partition (sda2), and within the extended partition there's a swap
partition (sda5).

![](/img/screenshot/ubuntu-gparted.png)

The swap partition is 1GB, which, while cramped, is plenty of room for
a Debian base system. I have no use for an extended partition so I can
just blow the whole thing away and install Debian to sda2.

    ubuntu# swapoff /dev/sda5
    ubuntu# fdisk /dev/sda  # (fix up partitions)
    ubuntu# mkfs.ext4 /dev/sda2
    ubuntu# mkdir /mnt/debian
    ubuntu# mount /dev/sda2 /mnt/debian
    ubuntu# debootstrap wheezy /mnt/debian <local-deb-mirror>

Now I have a proper Debian base system installed on a second
partition. At this point I could configure Grub to boot into it by
default rather than Ubuntu. However, as it stands so far, I would have
no way to access it remotely (or at all) since I haven't set anything
up. From here I just follow the guide in [appendix D][d] and configure
it from a chroot,

    ubuntu# LANG=C.UTF-8 chroot /mnt/debian /bin/bash
    chroot# mount -t proc proc /proc
    chroot# dpkg-reconfigure tzdata
    chroot# apt-get install linux-image-amd64 locales openssh-server
    chroot# passwd

I just copy Ubuntu's `/etc/network/interfaces` so that the new system
uses the same network configuration (DHCP in this case).

    ubuntu# cp /etc/network/interfaces /mnt/debian/etc/network/

A create a one-line `/etc/fstab`, mounting `/dev/sda2` as `/`.

    /dev/sda2  /  ext4  defaults  0  1

Finally I overwrite the Ubuntu-installed Grub. I need to set up the
devices in order to actually install Grub.

    chroot# apt-get install makedev
    chroot# cd /dev
    chroot# MAKEDEV generic
    chroot# apt-get install grub-pc
    chroot# update-grub

The Ubuntu system isn't visible to the Grub installer, so the VM will
now boot into my tiny Debian system by default.

    ubuntu# reboot

### Taking Over the Host

After about a minute I can SSH into the VM again. This time I'm in a
proper Debian Wheezy system running from sda2! Now the problem is
making use of that large partition that still houses Ubuntu. I *could*
try slicing it up and mounting parts of it for Debian, but there's
something simpler I can do: repeat the same exact process on the first
partition. The Debian system I just set up becomes a *pivot* for the
real installation.

    pivot# mkfs.ext4 /dev/sda1
    pivot# mkdir /mnt/debian
    pivot# mount /dev/sda1 /mnt/debian
    pivot# debootstrap wheezy /mnt/debian
    (... etc ...)

After installing Grub again, reboot. There may be some way to simply
copy the pivot system directly but I'm not confident enough to trust a
straight `cp -r`.

### Final Touches

Now I'm running Debian on the large partition. The pivot can be
converted back into a swap partition.

    debian# mkswap /dev/sda2
    debian# swapon /dev/sda2
    debian# echo /dev/sda2 swap swap defaults 0 0 >> /etc/fstab

This is now as good as a fresh installation of a Debian base system
(i.e. what the cloud provider *should* have offered in the first
place). If it was possible on this cloud, this is where I'd make a
snapshot for cloning future VMs. Since that's not an option, should I
need more Debian VMs in the future I'll write a pair of shell scripts
to do all this for me. An automated conversion process would probably
take about 5 minutes.

I love Debian.


[deb]: http://www.debian.org/
[live]: /blog/2013/06/17/
[debootstrap]: https://wiki.debian.org/Debootstrap
[d]: http://www.debian.org/releases/stable/amd64/apds03.html.en
