---
title: 'Endlessh: an SSH Tarpit'
layout: post
date: 2019-03-22T17:26:45Z
tags: [netsec, python, c, posix, asyncio]
uuid: 5429ee15-3d42-4af2-8690-f7f402870dd0
excerpt_separator: <!--more-->
---

*This article was discussed [on Hacker News][hn] ([later][hn2]), [on
reddit][reddit] ([also][reddit2]), featured in [BSD Now 294][bsdnow].
Also check out [this Endlessh analysis][analysis].*

I'm a big fan of tarpits: a network service that intentionally inserts
delays in its protocol, slowing down clients by forcing them to wait.
This arrests the speed at which a bad actor can attack or probe the
host system, and it ties up some of the attacker's resources that
might otherwise be spent attacking another host. When done well, a
tarpit imposes more cost on the attacker than the defender.

<!--more-->

The Internet is a very hostile place, and anyone who's ever stood up
an Internet-facing IPv4 host has witnessed the immediate and
continuous attacks against their server. I've maintained [such a
server][mail] for nearly six years now, and more than 99% of my
incoming traffic has ill intent. One part of my defenses has been
tarpits in various forms. The latest addition is an SSH tarpit I wrote
a couple of months ago:

[**Endlessh: an SSH tarpit**][endlessh]

This program opens a socket and pretends to be an SSH server. However,
it actually just ties up SSH clients with false promises indefinitely
— or at least until the client eventually gives up. After cloning the
repository, here's how you can try it out for yourself (default port
2222):

    $ make
    $ ./endlessh &
    $ ssh -p2222 localhost

Your SSH client will hang there and wait for at least several days
before finally giving up. Like a mammoth in the La Brea Tar Pits, it
got itself stuck and can't get itself out. As I write, my
Internet-facing SSH tarpit currently has 27 clients trapped in it. A
few of these have been connected for weeks. In one particular spike it
had 1,378 clients trapped at once, lasting about 20 hours.

My Internet-facing Endlessh server listens on port 22, which is the
standard SSH port. I long ago moved my real SSH server off to another
port where it sees a whole lot less SSH traffic — essentially none.
This makes the logs a whole lot more manageable. And (hopefully)
Endlessh convinces attackers not to look around for an SSH server on
another port.

How does it work? Endlessh exploits [a little paragraph in RFC
4253][rfc], the SSH protocol specification. Immediately after the TCP
connection is established, and before negotiating the cryptography,
both ends send an identification string:

    SSH-protoversion-softwareversion SP comments CR LF

The RFC also notes:

> The server MAY send other lines of data before sending the version
> string.

There is no limit on the number of lines, just that these lines must
not begin with "SSH-" since that would be ambiguous with the
identification string, and lines must not be longer than 255
characters including CRLF. So **Endlessh sends and *endless* stream of
randomly-generated "other lines of data"** without ever intending to
send a version string. By default it waits 10 seconds between each
line. This slows down the protocol, but prevents it from actually
timing out.

This means Endlessh need not know anything about cryptography or the
vast majority of the SSH protocol. It's dead simple.

### Implementation strategies

Ideally the tarpit's resource footprint should be as small as
possible. It's just a security tool, and the server does have an
actual purpose that doesn't include being a tarpit. It should tie up
the attacker's resources, not the server's, and should generally be
unnoticeable. (Take note all those who write the awful "security"
products I have to tolerate at my day job.)

Even when many clients have been trapped, Endlessh spends more than
99.999% of its time waiting around, doing nothing. It wouldn't even be
accurate to call it I/O-bound. If anything, it's *timer-bound*,
waiting around before sending off the next line of data. **The most
precious resource to conserve is *memory*.**

#### Processes

The most straightforward way to implement something like Endlessh is a
fork server: accept a connection, fork, and the child simply alternates
between `sleep(3)` and `write(2)`:

```c
for (;;) {
    ssize_t r;
    char line[256];

    sleep(DELAY);
    generate_line(line);
    r = write(fd, line, strlen(line));
    if (r == -1 && errno != EINTR) {
        exit(0);
    }
}
```

A process per connection is a lot of overhead when connections are
expected to be up hours or even weeks at a time. An attacker who knows
about this could exhaust the server's resources with little effort by
opening up lots of connections.

#### Threads

A better option is, instead of processes, to create a thread per
connection. On Linux [this is practically the same thing][raw], but it's
still better. However, you still have to allocate a stack for the thread
and the kernel will have to spend some resources managing the thread.

#### Poll

For Endlessh I went for an even more lightweight version: a
single-threaded `poll(2)` server, analogous to stackless green threads.
The overhead per connection is about as low as it gets.

Clients that are being delayed are not registered in `poll(2)`. Their
only overhead is the socket object in the kernel, and another 78 bytes
to track them in Endlessh. Most of those bytes are used only for
accurate logging. Only those clients that are overdue for a new line
are registered for `poll(2)`.

When clients are waiting, but no clients are overdue, `poll(2)` is
essentially used in place of `sleep(3)`. Though since it still needs
to manage the *accept* server socket, it (almost) never actually waits
on *nothing*.

There's an option to limit the total number of client connections so
that it doesn't get out of hand. In this case it will stop polling the
accept socket until a client disconnects. I probably shouldn't have
bothered with this option and instead relied on `ulimit`, a feature
already provided by the operating system.

I could have used epoll (Linux) or kqueue (BSD), which would be much
more efficient than `poll(2)`. The problem with `poll(2)` is that it's
constantly registering and unregistering Endlessh on each of the
overdue sockets each time around the main loop. This is by far the
most CPU-intensive part of Endlessh, and it's all inflicted on the
kernel. Most of the time, even with thousands of clients trapped in
the tarpit, only a small number of them at polled at once, so I opted
for better portability instead.

One consequence of not polling connections that are waiting is that
disconnections aren't noticed in a timely fashion. This makes the logs
less accurate than I like, but otherwise it's pretty harmless.
Unforunately even if I wanted to fix this, the `poll(2)` interface
isn't quite equipped for it anyway.

#### Raw sockets

With a `poll(2)` server, the biggest overhead remaining is in the
kernel, where it allocates send and receive buffers for each client
and manages the proper TCP state. The next step to reducing this
overhead is Endlessh opening a *raw socket* and speaking TCP itself,
bypassing most of the operating system's TCP/IP stack.

Much of the TCP connection state doesn't matter to Endlessh and doesn't
need to be tracked. For example, it doesn't care about any data sent by
the client, so no receive buffer is needed, and any data that arrives
could be dropped on the floor.

Even more, raw sockets would allow for some even nastier tarpit tricks.
Despite the long delays between data lines, the kernel itself responds
very quickly on the TCP layer and below. ACKs are sent back quickly and
so on. An astute attacker could detect that the delay is artificial,
imposed above the TCP layer by an application.

If Endlessh worked at the TCP layer, it could [tarpit the TCP protocol
itself][tcp]. It could introduce artificial "noise" to the connection
that requires packet retransmissions, delay ACKs, etc. It would look a
lot more like network problems than a tarpit.

I haven't taken Endlessh this far, nor do I plan to do so. At the
moment attackers either have a hard timeout, so this wouldn't matter,
or they're pretty dumb and Endlessh already works well enough.

### asyncio and other tarpits

Since writing Endless [I've learned about Python's `asyncio`][aio], and
it's actually a near perfect fit for this problem. I should have just
used it in the first place. The hard part is already implemented within
`asyncio`, and the problem isn't CPU-bound, so being written in Python
[doesn't matter][dumb].

Here's a simplified (no logging, no configuration, etc.) version of
Endlessh implemented in about 20 lines of Python 3.7:

```py
import asyncio
import random

async def handler(_reader, writer):
    try:
        while True:
            await asyncio.sleep(10)
            writer.write(b'%x\r\n' % random.randint(0, 2**32))
            await writer.drain()
    except ConnectionResetError:
        pass

async def main():
    server = await asyncio.start_server(handler, '0.0.0.0', 2222)
    async with server:
        await server.serve_forever()

asyncio.run(main())
```

Since Python coroutines are stackless, the per-connection memory
overhead is comparable to the C version. So it seems asyncio is
perfectly suited for writing tarpits! Here's an HTTP tarpit to trip up
attackers trying to exploit HTTP servers. It slowly sends a random,
endless HTTP header:

```py
import asyncio
import random

async def handler(_reader, writer):
    writer.write(b'HTTP/1.1 200 OK\r\n')
    try:
        while True:
            await asyncio.sleep(5)
            header = random.randint(0, 2**32)
            value = random.randint(0, 2**32)
            writer.write(b'X-%x: %x\r\n' % (header, value))
            await writer.drain()
    except ConnectionResetError:
        pass

async def main():
    server = await asyncio.start_server(handler, '0.0.0.0', 8080)
    async with server:
        await server.serve_forever()

asyncio.run(main())
```

Try it out for yourself. Firefox and Chrome will spin on that server
for hours before giving up. I have yet to see curl actually timeout on
its own in the default settings (`--max-time`/`-m` does work
correctly, though).

Parting exercise for the reader: Using the examples above as a starting
point, implement an SMTP tarpit using asyncio. Bonus points for using
TLS connections and testing it against real spammers.


[aio]: /blog/2019/03/10/
[analysis]: https://github.com/bediger4000/ssh-tarpit-behavior
[bsdnow]: https://www.youtube.com/watch?v=bM65iyRRW0A&t=3m52s
[dumb]: /blog/2019/02/24/
[endlessh]: https://github.com/skeeto/endlessh
[hn]: https://news.ycombinator.com/item?id=19465967
[hn2]: https://news.ycombinator.com/item?id=24491453
[mail]: /blog/2017/06/15/
[raw]: /blog/2015/05/15/
[reddit]: https://old.reddit.com/r/programming/comments/b4iq00/endlessh_an_ssh_tarpit/
[reddit2]: https://old.reddit.com/r/netsec/comments/b4dwjl/endlessh_an_ssh_tarpit/
[rfc]: https://tools.ietf.org/html/rfc4253#section-4.2
[tarpit]: https://en.wikipedia.org/wiki/Tarpit_(networking)
[tcp]: https://nyman.re/super-simple-ssh-tarpit/
