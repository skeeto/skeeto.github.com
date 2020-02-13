---
title: A Go Module Testbed
layout: post
date: 2020-02-13T01:03:24Z
tags: [go]
uuid: 838f3d56-f5d0-4422-be45-277a175e5daf
excerpt_separator: <!--more-->
---

I had [recently lamented][tool] that due to Go's strict module security
policy it was unreasonably difficult to experiment and practice with
modules. Modules can only be fetched from servers with valid TLS
certificates, including both the module path and repository servers.
Setting up a small, local experiment meant creating a certificate
authority, generating and signing certificates, and installing these all
in the right places. I'd much rather relax Go's security policy for the
experiment.

As a result of that complaint, [I learned][bcm] that the upcoming Go
1.14 has as a new feature: [`GOINSECURE`][doc]. It's like the old
`-insecure` option, but safer due to being finer grained: a whitelist of
exceptions. It's exactly what I needed. Since then I've been using it to
run small module experiments. It started as some scripts, but I
eventually formalized it into its own little project.

**<https://github.com/skeeto/go-module-testbed>** [requires Go 1.14]

<!--more-->

It's first and foremost a shell script, and the Go source is only there
as a server. The interface is like a [Python virtual environment][venv]
where "activating" the environment in a shell allows Go run from that
shell to interact with the testbed servers. The script establishes the
testbed environment and starts both servers in that environment. It
optionally accepts a testbed directory as an argument, defaulting to the
working directory as the testbed.

    $ ./go-module-testbed

In addition to running the servers in the foreground, the script
populates the testbed directory with an `activate` script, `src/`
containing module Git repositories, and `www/` containing the static web
server contents. These are initialized with a module named
`127.0.0.1/example` at v1.0.0. Why not `localhost` as the domain? The
domain part of a module path must contain at least one dot, and IP
addresses are acceptable.

The server logs requests to standard output so you can see each request
Go makes to the server. This is has been an important part of learning
what exactly Go is requesting from the web server hosting the module
path.

There's one giant caveat: Modules *must* be hosted on a privileged port.
Normally that's 443 (HTTPS), though in this case it's 80 (HTTP). Since
it's a privileged port, you'll need to do some system configuration. On
Linux it's easy enough just to temporarily forward the testbed port 8001
to port 80.

    # iptables -t nat -I OUTPUT -p tcp -d 127.0.0.1 \
               --dport 80 -j REDIRECT --to-ports 8001

Unfortunately this means, outside of doing something with namespace or
containers, there can only be one testbed per host at at time. My goal
is just to run small, local, temporary experiments, so this isn't a big
deal for me, but I wish it could be better.

### Activating the environment

With the server running and the port forwarding configured, source the
`activate` script from a shell:

    $ source activate

This sets up an isolated, disposable `GOPATH` so that the testbed is
completely isolated from your normal development. It also updates
`PATH`, unconditionally enables modules (`GO111MODULE=on`), whitelists
the testbed servers in `GOINSECURE`, and sets `GOPRIVATE` so that the
testbed modules don't leak anywhere outside the testbed environment.

The ensure that it's all working, try installing the `hello` command
from the example module:

    $ go get 127.0.0.1/example/cmd/demo
    go: downloading 127.0.0.1/example v1.0.0
    go: found 127.0.0.1/example/cmd/demo in 127.0.0.1/example v1.0.0
    $ demo
    Example v1.0.0

Non-testbed modules are still accessible like normal, though all fetched
and built artifacts are isolated in the testbed environment:

    $ go get nullprogram.com/x/passphrase2pgp
    $ go get golang.org/x/tools/cmd/goimports

So you can mix your experiments and practice with real modules.

### Running experiments

From here you could practice creating a new minor version of the example
module, and see how it appears to the module's users.

    $ sed -i s/v1.0.0/v1.1.0/ src/example/example.go 
    $ git -C src/example/ commit -a -m 'Bump to v1.1.0'
    [master 7a3cf82] Bump to v1.1.0
     1 file changed, 1 insertion(+), 1 deletion(-)
    $ git -C src/example/ tag -a v1.1.0 -m v1.1.0
    $ go get 127.0.0.1/example/cmd/demo
    go: downloading 127.0.0.1/example v1.1.0
    go: found 127.0.0.1/example/cmd/demo in 127.0.0.1/example v1.1.0
    $ demo
    Example v1.1.0

Or try more challenging: Release a v2.0.0, which requires changing the
module path.

    $ cd src/example/
    $ go mod edit -module 127.0.0.1/example/v2 go.mod
    $ sed -i s/v1.0.0/v2.0.0/ example.go 
    $ git commit -a -m 'Bump to v2.0.0'
    [master bf5c4cf] Bump to v2.0.0
     2 files changed, 2 insertions(+), 2 deletions(-)
    $ git tag -a v2.0.0 -m v2.0.0
    $ cd ../../www/example/
    $ mkdir v2
    $ sed 's#e git#e/v2 git#' index.html >v2/index.html
    $ cd ../../
    $ go get 127.0.0.1/example/v2/cmd/demo
    go: downloading 127.0.0.1/example/v2 v2.0.0
    go: downloading 127.0.0.1/example v1.0.0
    go: found 127.0.0.1/example/v2/cmd/demo in 127.0.0.1/example/v2 v2.0.0
    go: finding module for package 127.0.0.1/example
    go: found 127.0.0.1/example in 127.0.0.1/example v1.0.0

I was able to figure this all out specifically because of my testbed.
Adding a `/v2` module path on the web server was not obvious, and it's
glossed over in the tutorials.

### Nested modules

One of the under-documented corners of Go modules is *nested modules*.
That is, repositories that contain more than one module. (Note: These
are not called *submodules* since that would be confusing in the context
of Git.) The Go module testbed is great place to try them out — and to
learn why they should never be used. Even if I never plan to use them, I
still want to understand them since I might need to debug them someday.

There are two tricky parts to nested modules: the version tag and the
module path. Neither are documented as far as I've seen, so I had to
figure them out from official examples.

    $ mkdir src/example/nested
    $ cd src/example/nested/
    $ go mod init 127.0.0.1/example/nested
    go: creating new go.mod: module 127.0.0.1/example/nested
    $ echo package nested >nested.go
    $ git add .
    $ git commit -m 'Add a nested module'
    [master c5b1a29] Add a nested module
     2 files changed, 4 insertions(+)
     create mode 100644 nested/go.mod
     create mode 100644 nested/nested.go
    $ git tag -a nested/v1.2.3 -m v1.2.3
    $ cd ../../../
    $ mkdir www/example/nested
    $ cp www/example/index.html www/example/nested/
    $ go get 127.0.0.1/example/nested
    go: downloading 127.0.0.1/example v1.0.0
    go: downloading 127.0.0.1/example/nested v1.2.3
    go: 127.0.0.1/example/nested upgrade => v1.2.3

Module versions are derived from the Git tag, which is global to the
repository. So how are nested modules versions indicated? They get
namespaced tags, as shown above with `nested/v1.2.3`. If I didn't create
this tag, it would be as if I didn't tag any version of that module.

The second unintuitive part is the web server's response to `?go-get=1`.
At the nested module path, the response *must* indicate the containing
module and where to get it. In other words, it's the same response as
the containing module, which is why I merely copied `index.html`.
Returning a 404 for the module path is no good — another thing I've
learned from the module testbed.

There are still many things I have yet to try or practice in my module
testbed. It's great that now when I have a niggling question about
modules or `go get` behavior, I can get an answer within a minute or so
without needing to dig through useless online search results.


[bcm]: https://github.com/golang/go/issues/36746
[doc]: https://golang.org/doc/go1.14#go-env-vars
[tool]: /blog/2020/01/21/
[venv]: https://docs.python.org/3/tutorial/venv.html
