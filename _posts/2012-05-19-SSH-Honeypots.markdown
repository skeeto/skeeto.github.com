---
title: SSH Honeypots
layout: post
tags: [netsec]
uuid: f7897af2-ca8e-3a80-f951-de22639bf7c7
---

Three years ago I was experimenting with high-interaction SSH
honeypots. I failed to document the effort as a blog post
afterwards. Fortunately, I've been experimenting with honeypots again,
so I'm taking the time to document it this time.

A *honeypot* is a fake service or computer on a network used in
detecting and deflecting attacks on the network. Ideally, an attacker
is unable to tell honeypots apart from real systems, attacking the
honeypots instead. In general, honeypots fall into two categories:
*high-interaction* and *low-interaction*. The former will imitate a
real system with high fidelity while the latter may just listen for
connections on common ports, without actually accepting or sending
data.

What triggered my curiosity was that I wanted to put OpenBSD's
[`securelevel(7)`](http://www.openbsd.org/cgi-bin/man.cgi?query=securelevel)
feature to the test. In short, it's a runtime system value that ranges
from -1 (least secure) to 2 (most secure), and it's not possible to
decrease the level without gaining physical access to the system. Each
increase makes the system more read-only, and less flexible, so it's a
trade-off. A system running at level 2 should not carry over any state
between boots — like a LiveCD on a system with no disks.

I set up a fresh OpenBSD install in a [QEMU](http://qemu.org/) virtual
machine, locked the system down with `securelevel` at 1, forwarded the
SSH port all the way out to the Internet, and than gave
[Gavin](http://devrand.org/) the root password. I told him to go nuts,
with the ultimate goal that when he was done I should be unable to
tell he had even logged into the system. All of the system logs were
set to append-only, enforced through the kernel by `securelevel`, so
this should have been a very difficult task indeed.

It turned out he was much more successful than I expected. When he
told me he was done, I SSHed into the system to check the logs finding
that there were no entries indicating he logged in at all. The only
proof I could find that he was actually in was a message he
intentionally left behind for me. Did he just subvert `securelevel`?!

Turns out not quite. Whew! I was just putting too much trust into a
system I knew was compromised. He mounted a loopback filesystem
over top of the `/var/log`, then filled it with fake logs. He also
sabotaged the mount programs so that they'd hide the loopback mount
from me. Since the mount programs were on a read-only system, he had
to do a loopback mount there, too. After restarting OpenSSH, it was no
longer writing to the append-only log, but to the doctored log.

So, the proper way to check your security logs is by mounting the
compromised filesystem in a known trusted system — or, in this case,
just rebooting would have fixed it. Even with `securelevel`, you can't
check the compromised system in-place. Let this be a lesson to all
those amateur sysadmins out there (including me)!

We did a second round and he managed to trick me again by taking me
further into the rabbit hole. Instead of loopback mounts, since I was
expecting that, he had root log into a chroot environment, filled with
a full copy of the system including fake logs. This version survived
reboots and really required inspection from an external system.

After all this, I wanted to crank things up a notch by letting some
real attackers into my test system. I was already accustomed to seeing
many password-guesses on my SSH server in the logs, so getting someone
into my honeypot wouldn't take long at all. While I didn't care of
they trashed my VM — restoring from snapshot was an automatic process
— I really didn't want them to take advantage of my Internet
connection, using it for DDoS attacks or pivoting to attack other SSH
servers. So I needed a way to allow them *in* though SSH, but not allow
any other traffic *out*.

If I was doing this today, I'd probably use `iptables` to only allow
SSH in, and then bridge the VM to the Internet with a TUN/TAP,
replacing my real SSH server on port 22. However, three years ago I
didn't know how to do this. Instead I found a really simple hack to
get this done: [`tsocks`](http://tsocks.sourceforge.net/). `tsocks`
adds SOCKS proxying to any application by replacing the sockets API
with its own. In my case, I wrapped the VM in `tsocks` configured to
use a non-existent SOCKS proxy (127.0.0.1). It could accept any
incoming connection (though limited to SSH because of NAT) but unable
to make any outgoing connections. Perfect!

I hadn't realized it yet, but this was a high-interaction SSH honeypot
I created.

I set the root password to "password" and let it go for awhile,
tailing the OpenSSH logs to watch for activity. The brute-force bots
would eventually make their way inside but immediately log out and
keep guessing passwords for root. Either they were really poorly
programmed or they were specifically testing for honeypots that allow
different passwords. They must have logged the address for a human to
investigate some time in the future, because I never witnessed any
shell activity. On the other hand, this was all very difficult to
observe, for the same reasons Gavin was able to cover his tracks. My
honeypot was useful for catching and detecting attackers, but it
wasn't good for observing them in action.

While I was investigating this I came across
[Kojoney](http://kojoney.sourceforge.net/), which is a low-interaction
SSH honeypot mainly for seeing what sorts of passwords attackers were
guessing. Unfortunately, I could never get it to work, so I never used
it.

Several years passed and I recently came across a project that didn't
exist last time: [kippo](http://code.google.com/p/kippo/), a
"medium"-interaction SSH honeypot. This is everything I was looking
for before. It doesn't require a full-blown VM, it's has high fidelity
interaction, it's safe, and it allows me to fully observe all activity
— it even records the tty session for replay. Cool!

kippo is written in pure Python, so there shouldn't be any buffer
overflows, and doesn't execute any external programs. It *should* be
safe, but I'm not aware of any real security reviews, so it's a
use-at-your-own risk thing. They warn about this on their website.

I've run this off and on on the weekends. Since I haven't run my real
SSH server on port 22 since 2009 (no recorded attacks since!), my IP
address atracts much less attention than before, so it hasn't seen too
much activity. I have had two humans connect and log in. Both
downloaded a well-known script kiddie tool called `go.sh`. Here's an
analysis of the tool by someone who was actually attacked with it:
[SSH Bruteforce](http://www.shellperson.net/hacked-ssh-bruteforce/).

In fact, `go.sh` is so well known that it gave me a little scare. In
my tty recording it looked like the tool was actually executed! The
skull banner printed out and it had an interface. I was really nervous
until I found kippo's
[malware.py](http://code.google.com/p/kippo/source/browse/trunk/kippo/commands/malware.py?r=204). Kippo
actually recognizes some script kiddie tools and imitates their
interfaces to further confuse attackers. I *do* run kippo as an
unprivileged user so it wouldn't be the end of the world if something
did happen, but
[I'd still](http://it.slashdot.org/story/08/02/10/2011257/linux-kernel-26-local-root-exploit)
[be uncomfortable](http://blog.zx2c4.com/749).

There's neat feature of kippo, which hilariously caught Gavin
off-guard when I had him poke at it. kippo will never disconnect a
session on its own. If an `exit` or `C-d` is given, it drops into
*another* fake shell with the hostname "localhost", merely pretending
to log out. That way you get a chance to see some commands the
attackers are meaning to run on their own system, before they realize
their mistake. The only way to disconnected is to either close your
terminal emulator or use SSH's `~.` escape sequence.

I've been considering running kippo all the time with no password set
— using it as a true honeypot. This would help keep anyone from
finding my real SSH server, since they would find the honeypot and
stop searching other ports. It would also waste time that could be
spent attacking other people's real SSH servers, helping to protect
other servers out there. My real SSH server (on my router) doesn't
allow password logins, only key logins, so I already feel pretty good
about its security. I've never seen a brute-force attempt on the
current port anyway. But if I do, I now have kippo as another tool in
my security toolbelt.
