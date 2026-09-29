---
title: Don't Write Your Own E-mail Validator
layout: post
tags: [rant, perl]
uuid: 0e9536bd-6f02-332b-e974-442f31779080
---

<!-- 24 December 2008 -->

Gmail has a nice feature: when delivering e-mail, everything including
and after a `+` in a Gmail address is ignored. For example,
mail arriving at all of these addresses would go to the same place if
they were Gmail addresses,

```
account@example.com
account+nullprogram@example.com
account+slashdot@example.com
```

Thanks to this feature, when a user acquires a Gmail account, Google
is actually providing about a googol (as in the number
10<sup>100</sup>) different e-mail addresses to that user! Quite
appropriate, really.

I have seen other mailers do similar things, like ignoring everything
after dashes. A nice advantage to this is when registering at a new
website I can customize my e-mail address for them by, say, throwing
the website name in it. Because I have a google of e-mail addresses
available, it is impossible to run out, so I can give every person I
meet their own version of my address. The custom address can come in
handy for sorting and filtering, and it will also tell me who is
selling out my e-mail address. This, of course, assumes that someone
isn't stripping out the extra text in my address to counter the Gmail
feature.

However, in my personal experience, most websites will not permit
`+`'s in addresses. This is completely ridiculous, because
it means that **virtually every website will incorrectly invalidate
perfectly valid e-mail addresses**. Even major websites, like
*coca-cola.com*, screw this up. They see the `+` in
the address and give up.

In fact, if I do a Google search for "email validation regex" right
now, 9 of the first 10 results return websites with regular
expressions that are complete garbage and will toss out many common,
valid addresses. The only useful result was at the fifth spot (linked
below).

For the love of Stallman's beard, **stop writing your own e-mail
address validators!**

Why shouldn't you even bother writing your own? Because the proper
Perl regular expression for [RFC822](http://www.ietf.org/rfc/rfc0822.txt?number=822) is [over 6
kilobytes in length](http://ex-parrot.com/~pdw/Mail-RFC822-Address.html)! Follow that link and look at that. This is
the *smallest* regular expression you would need to get it right.

If you *really* insist on having a nice short one and don't want
to use a validation library, which, again, is a stupid idea and you
*should* be using a library, then use the dumbest, most liberal
expression you can. (Just don't forget the security issues.) Like
this,

```
.+@.+
```

Seriously, if you add anything else you most almost surely make it
incorrectly reject valid addresses. Note that e-mail addresses can
contain spaces, and even more than one `@`! These are
valid addresses,

```
"John Doe"@example.com
"@TheBeach"@example.com
```

I have not yet found a website that will accept either of these, even
though both are completely valid addresses. Even MS Outlook, which I
use at work (allowing me to verify this), will refuse to send e-mail
to these addresses (Gmail accepts it just fine). Hmmm... maybe having
an address like these is a good anti-spam measure!

So if your e-mail address is `"John Doe"@example.com` no
one using Outlook can send you e-mail, which sounds like a feature to
me, really.

So, everyone, please stop writing e-mail validation regular
expressions. The work has been done, and you will only get it wrong,
guaranteed.

This is a similar rant I came across while writing mine: [ Stop Doing Email Validation the Wrong Way](http://www.santosj.name/general/stop-doing-email-validation-the-wrong-way/).
