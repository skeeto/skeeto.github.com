---
title: SSH and GPG Agents
layout: post
tags: [crypto, netsec]
uuid: 7a12226e-073a-3902-4fe8-842afdfdb951
---

If you're using SSH or GPG with any sort of frequency, you should
definitely be using their accompanying `*-agent` programs. The agents
allow you to gain a whole lot of convenience without compromising your
security. Many people seem to be unaware these tools exist, so here's
an overview along with some tips on how to use them effectively.

Let's start from the top.

Both SSH and GPG involve the use of asymmetric encryption, and the
private key is protected by a user-entered passphrase. The private key
is generally never written in to the filesystem in plaintext. In the
case of GPG, these keys are the primary focus of the application. For
SSH, they're a useful tool to make accessing remote machines less
tedious. (The SSH server is authenticated by a public key, too, but
this is unrelated to agents.)

For those who are unaware, rather than enter a password when logging
into a remove machine, you can identify yourself by a public
key. Generating a key is simple.

    ssh-keygen

You'll almost certainly want to accept the default location for the
key (`~/.ssh/id_rsa`) because this is where SSH will look for it. Make
sure you enter a passphrase, which will encrypt the private key. The
reason this is important is because, without it, anyone who gains
access to your `id_rsa` file will be able to access any remote systems
that have been told to trust your public key. By having a passphrase,
this person needs not only the `id_rsa` file, but also the passphrase
(two-factor authentication), so you probably want to pick a long,
strong one. This may sound inconvenient, but `ssh-agent` will help
you.

The key generation process will create two files: `id_rsa` (private
key) and `id_rsa.pub` (public key). The latter is what you give to
remote systems.

Telling a remote system about your key is simple,

    ssh-copy-id <host>

This will copy your `id_rsa.pub` to the remote system, prompting you
for the *password* on the *remote* system (not the passphrase you just
entered), adding it to the file `~/.ssh/authorized_keys`. From this
point on, all logins will use your new keypair rather than prompt you
for a password. Since you put a passphrase on your key, this may seem
pointless — it seems you still need to type in a password for every
connection. Bear with me here!

As a side note, you should have a unique SSH keypair for each
<i>site</i>, so you'll have several of them. This way you can revoke
access to a particular site without affecting the others.

For GPG — the GNU Privacy Guard, <i>the</i> free software PGP
implementation — your keys are stored under `~/.gnupg/` in a
database. Generating a key is also a simple command,

    gpg --gen-key

This is a slightly more complicated process, which I won't get into
here. In contrast to SSH, you'll generally have only one keypair per
<i>identity</i> (i.e. you only have one).

So you've got these keys are encrypted by passphrases. If they're
going to be any use then they'll be long, annoying things that are a
pain to type in. If that was the end of the story this would be really
inconvenient, enough to make the use of passphrases too costly for
many people to bother. Fortunately, we have agents to help.

An agent is a daemon process that can hold onto your passphrase
(`gpg-agent`) or your private key (`ssh-agent`) so that you only need
to enter your passphrase once within in some period of time (possibly
for the entire life of the agent process), rather than type it many
times over and over again as it's needed. The agents are very careful
about how they hold on to this sensitive information, such as avoiding
having it written to swap. You can also configure how long you want
them to hold onto your passphrase/key before purging it from memory.

The `ssh` and `gpg` programs need to know where to find the
agents. This is done through environmental variables. For `ssh-agent`,
the process ID is stored in `SSH_AGENT_PID` and the location of the
Unix socket for communication is in `SSH_AUTH_SOCK`. `gpg-agent`
stuffs everything into one variable, `GPG_AGENT_INFO` (which is a pain
if you want to use this information in a script). When the main
program is invoked and it needs to use the private key, it will use
these variables and get in touch with the agent to see if it can
supply the needed information without bothering the user.

Remember, a process can't change the environment of their parent
process so you need to set this information in the agent's parent
shell somehow. There are two methods to set these up: eval and exec.

When you start the agent, it forks off its daemon process and prints
the variable information to stdout. This can be `eval`ed directly into
the current environment. You could drop these lines directly in your
`.bashrc` so that the agents are always there. (Though they won't exit
with your shell, lingering around uselessly! More on this ahead.)

    eval $(ssh-agent)
    eval $(gpg-agent --daemon)

For the exec method, you *replace* your current shell with a new one
with a modified environment. To do this, you ask the agent to exec
into a shell, with the variables set, rather than return control.

    exec ssh-agent bash
    exec gpg-agent --daemon bash

As cool trick, you can chain these together. `ssh-agent` becomes
`gpg-agent` which then becomes `bash`.

    exec ssh-agent gpg-agent --daemon bash

Note that `gpg-agent` is capable of being an `ssh-agent` as well by
using the `--enable-ssh-support` option, so you don't need to launch
an `ssh-agent`. Unfortunately, I don't like to use this because
`gpg-agent` gets a little too personal with the SSH key, storing its
own copy with its own passphrase again.

On the other hand, `gpg-agent` is *much* more advanced than OpenSSH's
`ssh-agent`. When you want to have `ssh-agent` manage a key, you need
to first tell it about the key with `ssh-add`. With no arguments, it
will use `~/.ssh/id_rsa`. If you forget to do this, `ssh` will ask for
your passphrase directly, in your terminal, not allowing `ssh-agent`
to hold onto it. By comparison, `gpg` will always ask `gpg-agent` to
retrieve your passphrase when it's needed (if the agent is available),
so it will cache your passphrase on demand. No need to explicitly
register with the agent. Even better, it will try its best to use a
"PIN entry" program to read your key, which helps protect against some
kinds of keyloggers — preventing other processes from seeing your
keystrokes.

Well, this is all fine and dandy except when you've already got an
agent running. Say you're launching a new terminal emulator window
from an existing one, creating a new shell. Unfortunately, even though
you have agents running *and* they're listed in your environment (from
the origin shell), *they'll still spawn new agents*! This is really
lousy behavior, in my opinion. There's no `--inherit` option to tell
them to silently pass along the information of the existing agent if
it appears to be valid. This causes two problems. One, you'll need to
enter your passphrases *again* for the new agent. Second, these new
agents will linger around after the spawning shell has exited —
hogging important non-swappable memory.

The direct workaround is to, in your shell init script, check for
these variables yourself and check that they're valid (the agent
process is still running) before trying to spawn any agents. This is
tedious, error-prone, and makes each user do a lot of work that could
have been done in one place by one person instead.

There's still the problem of when you launch a new shell that doesn't
inherit the variables (i.e. a remote login), so there's no way for it
to be aware of the existing agents. To fix this, you'd need to write
the agent information to a file. The shell init script checks this
file for an existing agent before spawning one. This is even more
complicated, more error-prone, and subject to race-conditions. Why
make every use go through this process?!

Fortunately someone's done all this work so you don't have to! There's
an awesome little tool called
[Keychain](http://www.funtoo.org/wiki/Keychain) which can be used to
launch the agents for you. It stores the agent information in a file
so that you only ever launch one instance of the agent, and the agents
will be shared across every shell. It *does* have an `--inherit`
option — the default behavior, so you don't even need to ask
nicely. Instead of running the `*-agent`s directly, you just put this
in your `.bashrc`,

    eval $(keychain --eval --quiet)

So simple and it *just works*! I was so happy when I found this. This
is the magic word that makes using agents a breeze, so I can't
recommend it enough.
