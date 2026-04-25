---
title: "My brave new code-signing world"
layout: post
date: 2026-04-25T18:12:29Z
tags: [ai, c, cpp]
uuid: 9db72e48-92f3-4426-a18f-9a317354e2c8
---

The new [w64devkit][] release two weeks ago is the first to be code-signed
with my identity, verified by Microsoft's certificate chain. Currently
only the release packaging is signed — the self-extracting archive *and*
its payload — but I will soon code-sign individual EXEs and DLLs within
the distribution. In fact, *all* Windows builds of my project releases
have been code-signed the past two weeks, including [dcmake][], and so
should everything going forward. My signing identity builds reputation
with each download, so users will have an easier time with SmartScreen,
and security software generally. [Azure Artifact Signing][aas] creates the
actual signature, but the rest is done with new infrastructure I built
myself, **[aas-sign][]**. As is often the case, the existing options were
deficient for my needs, so I had to build it myself.

**This code-signing is not free**, and simply having `aas-sign` on hand,
or using the GitHub Actions action, is insufficient. You must be serious
enough to spend US$10/month for the Azure subscription. After that you are
subjected to the labyrinth that is the Azure portal, the most confusing UI
I've ever used. Luckily we live in [an age of wonders][ai], and I could
describe to Claude in Chrome what I wanted and it would happen (Sonnet
works better than Opus for this). It took as much time to figure out Azure
as I spent creating a fully-functional, native debugger front-end. Clear
your schedule if you're going to try it yourself. If it weren't for AI
assistance I would have given up.

The one-time setup process is **only open to North America**, and involves
sharing identify documents (i.e. driver's license) with Microsoft. Unlike
the rest of Azure, that part was streamlined and fairly painless. Between
the cost and this requirement, *this is a niche space*.

However, if this is your niche, aas-sign is currently the best software
available. *It's the tool Microsoft should have written*, but didn't due
to ongoing institutional failures. The alternatives are a pair of tools:
[Azure CLI][az] (Python) combined with either [Jsign][] (Java) or
[SignTool.exe][] (Windows only). All impose artificial runtime constraints
hostile to build pipeline composablility. Poor engineering. In contrast,
aas-sign is a native, multi-platform, single-file application.

If you know this space, [osslsigncode][] probably comes to mind, but it
produces signatures itself. It doesn't interface with Azure and so has no
role here aside from semi-reliable validation. The most popular use case
is code-signing with self-signed certificates, but [that actually makes
everything worse][ss].

There are two modes for aas-sign: Laptop and Action. Laptop mode is the
most compelling, so we'll start with that, but Action mode is the most
useful in practice.

### Laptop/desktop mode

Suppose you built an EXE or DLL, and would like to code-sign and publish
it. Typically that looks like this:

    $ aas-sign sign myapp.exe myapp.dll

It computes an Authenticode for each (concurrently), sends it off to
Azure, gets back a signature, then a countersignature, and embeds the
signatures in the images. If you have multiple signing identities then you
might use `--as` ("sign as"):

    $ aas-sign sign --as eus:contoso:jdoe myapp.exe myapp.dll

The colon-delimited triple is my own invention to combine region (East
US), tenant (Contoso), and profile (J. Doe) into one string. The first
time you use it, and every ~90 days thereafter, you'll need to
authenticate with Azure first:

    $ aas-sign login

This will open a browser (just like `az login`) to log in, from which it
will obtain a token than can be used to obtain signing tokens. (Yes, a
token to get tokens; I'm concealing as much complexity as possible.) You
might also want to establish a default identity, as typically you'd only
have one:

    $ aas-sign config eus:contoso:jdoe

Or all at once:

    $ aas-sign login eus:contoso:jdoe

My goal was, after enduring the Azure portal sign-up, to maximally
streamline code-signing.

### Action mode

Manually building, signing, and publishing releases is easy and might be
fine if you're not releasing too frequently — or too *in*infrequently that
you forget how to do it — but likely you'd want to automate this process.
I was stubborn about it myself, until [Peter0x44][] pushed me hard enough
to take it seriously, for which I'm grateful. There's an official GitHub
Action to code-sign with Azure, but it requires a Windows runner, fatally
limiting for my own needs. So aas-sign also defines a code-signing action.
The previous example would have this in its own action:

{% raw %}
```yaml
  - name: Sign
    uses: skeeto/aas-sign@v1.0.0
    with:
      endpoint:  ${{ secrets.TRUSTED_SIGNING_ENDPOINT }}
      account:   ${{ secrets.TRUSTED_SIGNING_ACCOUNT }}
      profile:   ${{ secrets.CERTIFICATE_PROFILE }}
      client-id: ${{ secrets.AZURE_CLIENT_ID }}
      tenant-id: ${{ secrets.AZURE_TENANT_ID }}
      files: |
        myapp.exe
        myapp.dll
```
{% endraw %}

The secrets are bunch of strings you (or your AI agent) retrieve from the
Azure portal. You also need to create Federated Identity Credential (FIC)
for each repository, which I suggest triggering on an environment. (This
all may [sound like a joke][joke] but it's real.) Again, just ask an AI to
do all this stuff. The mandatory Azure interfacing limits how much I can
streamline this process. Then aas-sign combines these with per-job tokens
GitHub injects into the runner to authenticate (via the FIC) and sign.

I've gone through this a number of times, and the AI breezes through the
GitHub UI, but struggles through the Azure portal — objective evidence of
how awful it is. Idea for a UI benchmark: How many AI tokens does it take
to accomplish typical activities?

For w64devkit, my plan is to run aas-sign inside the Docker build and sign
executables in the container before it's SFX-packaged. This is impossible
with SignTool.exe and needlessly frictional with Jsign (requires at least
a JRE if not a JDK). The easiest path forward was to literally build my
own tool from scratch.

I'm considering `aas-sign` as a new w64devkit command, but it's so niche
that I'm likely to be its sole user. On the other hand, those already
running w64devkit in GitHub Actions could use it in Action mode to
code-sign their builds without any additional tools.


[Jsign]: https://github.com/ebourg/jsign
[Peter0x44]: https://peter0x44.github.io/
[SignTool.exe]: https://learn.microsoft.com/en-us/dotnet/framework/tools/signtool-exe
[aas-sign]: https://github.com/skeeto/aas-sign
[aas]: https://learn.microsoft.com/en-us/azure/artifact-signing/overview
[ai]: /blog/2026/03/29/
[az]: https://learn.microsoft.com/en-us/cli/azure/?view=azure-cli-latest
[dcmake]: /blog/2026/04/07/
[joke]: https://www.youtube.com/watch?v=y8OnoxKotPQ
[osslsigncode]: https://github.com/mtrojnar/osslsigncode
[ss]: https://www.bcs.org/articles-opinion-and-research/what-happens-when-microsoft-defender-flags-your-software/
[w64devkit]: https://github.com/skeeto/w64devkit
