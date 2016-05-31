---
title: Versioning Personal Configuration Dotfiles
layout: post
tags: [crypto, git]
uuid: d2428806-5a27-3996-39aa-ae0c411da126
---

For almost two months now I've been
[versioning all my personal dotfiles](https://github.com/skeeto/dotfiles)
in Git. Just as [when I did the same with Emacs](/blog/2011/10/19/),
it's been extremely liberating and I wish I had been doing this for
years. Currently it covers 11 different applications including my web
browser, shell, window manager, and cryptographic keys, giving me a
unified experience across all of my machines — which, between home,
work, and virtual computers is about half a dozen.

Like anything, the biggest problem with *not* versioning these files
is introducing changes. If I add
[an interesting tweak to a dotfile](/blog/2012/06/08/), I won't see
that change on my other machines until I either copy it over or I
enter it manually again. Because I'd worry about clobbering other
unpropagated changes, it was usually the latter. Only changes I could
commit to memory would propagate. Any tweak that wasn't easy to
duplicate manually I couldn't rely on, so I was discouraged from
customizing too much and relied mostly on defaults. This is bad!

Source control solves almost all of this trivially. If I notice a
pattern in my habits or devise an interesting configuration, I can
immediately make the change, commit it, and push it. Later, when I'm
on another computer and I notice it missing, I just do a pull without
needing to worry about clobbering any local changes. When moving onto
a new computer/install, all I need to do is clone the repository and
I've got *every* configuration I have without having the snoop around
the last computer I used figuring out what to copy over.

Most of [the applications I prefer](/blog/2012/04/29/) have tidy,
manually-editable dotfiles that version well, so I would be able to
capture almost my entire environment. One near-exception was
Firefox. By itself, it doesn't play well, but
[since I use Pentadactyl](/blog/2009/04/03/) I'm able to configure it
cleanly like a proper application.

The last straw that triggered my dotfiles repository was
[managing my Bash aliases](/blog/2011/11/03/). It had gotten *just*
long enough that I was tired of manually synchronizing them. It was
finally time to invest some time into nipping this in the bud once and
for all. Unsure what approach to take, I looked around to see what
other people were doing. There are two basic approaches: version your
entire home directory or symbolically link your dotfiles into place
from a stand-alone repository.

The first approach is straightforward but has a number of issues that
make it a poor choice. You don't need an install script or anything
special, you just use your home directory.

    cd
    git init
    git add .bashrc .gitconfig ...

The first problem is that most the files Git sees you *do not* want to
version. These are all going to show up in the status listing and,
because there's no pattern to them, there's really no way to filter
them out with a `.gitconfig`. Any other clones you have in your home
directory may also confuse Git, looking like submodules. You'll have
to dodge this extra stuff all the time when working in the repository.

The second problem is that Git has only only one `.git` directory, in
the repository root. If there's no `.git` in the current directory, it
will keep searching upwards until it finds one ... which will
inevitably be your dotfiles repository. This will eventually lead to
annoying mistakes where you accidentally commit work to your dotfiles
repository for awhile until you notice you forgot a `git init`. A
possible workaround is to keep the `.git` directory out of your home
directory and use the environment variable `GIT_DIR` to tell Git where
it is when you're working on it. That sounds like a pain to me.

The other approach is to have your dotfiles repository cloned on its
own, then use symlinks to put the configuration files into place. You
need to write an install script to do this. However, not all
configuration files are sitting directly in your home directory. Some
have their own directory. Modern applications have moved into a
directory under `~/.config/`. Your script needs to handle these.

Why symlinks rather than just copying the file into place? Well, if
you make any changes to the installed files, Git won't see them and
you risk losing those changes.

Why symlinks rather than hard links? Symlinks deal with the atomic
replacement issue better. Conscientious applications are very careful
about how they write your data to disk. Unless it's some kind of
database, files are never edited in-place. The application rewrites
the entire file at once. If the application is stupid and overwrites
the file directly, there's a brief instant where you data is not on
disk at all! First, it truncates the original file, deleting your
data, then it rewrites the data, and, if it's not *too* stupid, calls
`fsync()` to force the write to the hardware. It's stupid, but it will
work with symlinks.

The conscientious application will write the data to a temporary file,
call `fsync()` (well,
[sort of](http://stackoverflow.com/questions/7433057/is-rename-without-fsync-safe)),
then atomically `rename()` the new file over top the original file. If
there's any failure along the way, *some* intact version of the data
will be on the disk. The problem is that this will replace your
symlink and changes won't be captured by the repository. Such an
incident will be obvious with symlinks, since the file will no longer
be a symlink. Hard links are much less obvious.

Smart applications, like Emacs, also know not to clobber your symlinks
and will handle these writes properly, leaving the symlink
intact. With hard links, there is no way for the application to know
it needs to treat a file specially.

I figured that I could use someone else's install script, so I
wouldn't have to worry about getting this right. Since Ruby is so
popular with Git, many people are using Rake for this task. However, I
want to be able to maintain the install script myself and I don't know
Rake. I also don't want to depend on anything unusual to install my
dotfiles. So that was out.

Second, I don't want to have to specifically list the files to
install, or not install, in the script. Don't put the same information
in two places when one will do. This script should be able to tell on
its own what files to install.

Third, I didn't want my dotfiles to actually *be* dotfiles in my
repository. It makes them hard to see and manage, since they're
hidden. They're much easier to handle when the dot is replaced with an
underscore.

So I wrote my own install script which installs any file beginning
with an underscore. I've since added support for "private" dotfiles
along the way. These are dotfiles that contain sensitive information
and are encrypted in the repository, allowing me to continue
publishing it safely. I'll elaborate
[in a future post](/blog/2012/06/24/).

If you'd like to create your own dotfiles repository, my dotfile
repository may not be useful beyond standing as an example but my
install script may be directly reusable for you.

There's a lot to talk about, so I'll be making a few more posts about
this.
