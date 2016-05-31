---
title: Some Cool Shell Aliases
layout: post
tags: [trick]
uuid: 98001174-3eda-3ae3-9131-943831f4d5fa
---

Over the last couple of years I've worked out some cool shell tricks,
which I use as aliases. Like any good software developer, if I notice
a pattern I take steps to generalize and reduce it. For shells, it
might be as simple as replacing a regularly typed long command with a
short alias, but the coolest tricks are the ones that reduce an entire
habit.

The first one is the singleton pattern. Say you have a terminal
program that should only have a single process instance but should
only start on demand. Some programs may enforce that rule, if it makes
sense to, but some do not.

In my case, that program was
[rtorrent](http://libtorrent.rakshasa.no/). I only want a single
instance of this program running at a time, but I also don't want to
have to think about whether or not I've started it already. I always
run it in [`screen`](/blog/2009/03/05/) so that I can detach it and
let it run in the background. My shell habits looked like this.

    # Assume it's there already
    $ screen -r rtorrent
    # If not, fire it up
    $ screen -S rtorrent rtorrent

If I needed to start rtorrent for the first time I was often typing in
that first command just to see it fail. Fortunately, it really does
fail: the exit code is non-zero. This allows me to make this cool
alias,

~~~bash
alias rt='screen -r rtorrent || screen -S rtorrent rtorrent'
~~~

Either it attaches to the existing instance or fires a new one up for
me and attaches me to that one. Now, there *is* a race condition
here. That "or" operator isn't atomic, so something else might spawn
an rtorrent instance in between check and creation. Since I'm only ever
running this by hand, and there is only one of me, that's not a problem.

The next trick has to do with
[my habit of throwing up a temporary web server](/blog/2010/09/21/)
when I need to share files. I noticed that I would launch it, run it
for a minute, kill it, run one or two commands, and launch it
again. For example, if I'm working on a program and I want to share
the build with someone else. I might drop out of it just to do
something with git and rebuild. Once again, my alias fix involves
`screen`,

~~~bash
alias httpd='screen -S http python -m SimpleHTTPServer 8080'
~~~

Rather than kill the server only to restart it again, I always run it
in `screen`. So instead I detach, but I don't even need to bother
reattaching.

This next one is my Emacs alias. Emacs has the really, really cool
ability to become a daemon. You can launch a daemon instance, then
connect to it as needed with clients to do your editing (`emacsclient
--create-frame` or just `-c`). This allows your Emacs session to live
for a long time, preserving all your buffers. Long-living sessions are
[an old Lisp tradition](/blog/2011/01/30/). Also, being a daemon
eliminates any lengthy startup penalty, because it only happens once
after reboot.

    $ emacs --daemon
    $ emacsclient -c
    # Close it and sometime later start another client
    $ emacsclient -c

This is another case of the single-instance problem. However, Emacs is
really smart about managing this by itself. It has an argument,
`--alternate-editor` (`-a`), which allows you to specify another
editor to use in case the daemon isn't started.

    emacsclient -ca nano

The most important part of this option is its hidden feature. When the
argument is empty it defaults to launching a daemon. No need to launch
it manually, it's just one command.

~~~bash
alias e='emacsclient -cna ""'
~~~

Naturally, Emacs gets to be in one of the coveted, single-letter
slots. I also set one up for terminal mode Emacs (`-t` instead of
`-c`),

~~~bash
alias et='emacsclient -ta ""'
~~~

And just to teach the editor heathens a lesson or two, this command
has a second alias,

~~~bash
alias vi='emacsclient -ta ""'
~~~

The final trick is one I just figured out this week, and it involves
passphrase agents. Just in case you are not familiar, both `ssh` and
`gpg` have daemons which will securely store your passphrases for
you.

*Update June 2012:* I [have a better solution](/blog/2012/06/08/) for
this problem.

OpenSSH is loaded with extremely useful functionality. One of them is
key authentication. Rather than use a password to log into a system,
you can prove your identity cryptographically — you solve a math
problem that *only* you have the information to solve. This is
invaluable to Git, because it allows for passwordless access to remote
repositories. You can host a repository for a bunch of users without
the awkward password step ("Pst ... your password is `passwordABC`.
Change it after you first log in."). Instead, they all send you their
public keys.

To use this feature, you first you generate a public/private keypair
for yourself, which gets stored in `~/.ssh/id_rsa` and
`~/.ssh/id_rsa.pub`.

    $ ssh-keygen

At one point in this process you will be asked for a passphrase, which
is used to encrypt your private key. At first you might wonder why you
bother with the key at all if you're going to encrypt it. Rather than
enter a password to log into a system, you have to enter a passphrase,
which is even worse because it's longer. So you don't bother with a
passphrase. This is dangerous because it's practically the same as
storing your login password in a file! If someone got a hold of your
private key file, they have full access to your systems.

This is where `ssh-agent` comes in. It runs as a service in the
background. You register your private key with it with `ssh-add` and
it queries for your passphrase, storing it in memory. It's very
careful about this. It's stored on a memory page that has been
registered with the operating system such that it's never written to
permanent storage (swap). When the process dies, or zeros out that
memory, the passphrase is completely lost.

GnuPG's daemon, `gpg-agent`, works very similarly. It holds onto your
PGP passphrase so you can perform a number of actions with it without
needing to enter it a bunch of times.

`gpg` and `ssh` know how communicate with their agents through
information stored in environmental variables. However, this creates a
problem when launching the agents. They can't change the environment
of their parent process, your shell. The easiest way to do it is to
reverse the relationship, with the agent becoming the parent of your
shell.

    $ exec ssh-agent bash

This tells `ssh-agent` to launch a new shell with the proper
environment. In case you're not familiar, the `exec` causes the new
shell replace the current one. It's the same `exec()` as the POSIX
function. You could leave it off, but you'll be left with nested
shells, which can be very confusing.

GnuPG's agent is launched like this,

    $ exec gpg-agent --daemon bash

My problem was that I often want both of these at the same time. I
could run them back to back, but making them into a single, alias-able
command is tricky. This naive expression will not work,

    $ exec ssh-agent bash && exec gpg-agent bash

The first `exec` causes the current shell to end, so the second part
is never evaluated. I can't simply remove the `exec`s and live with
nested shells because the next agent isn't launched until the first
agent's shell dies. The very cool solution is to chain them together!

~~~bash
alias agent='exec ssh-agent gpg-agent --daemon bash'
~~~

The current shell turns into the `ssh-agent`, which spawns `gpg-agent`
with the proper environment for `ssh`, which forwards its environment
along to spawn a new shell with the proper environment for both.

If I ever need another agent I just add it to the chain. That command
should probably be at the end of my `.bashrc` or something, but it's
just an alias for now. Sometimes I log into X first, sometimes `ssh`
first, so I'm not sure what the correct place would be.
