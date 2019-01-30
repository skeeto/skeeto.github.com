---
title: Emacs Lisp Reddit API Wrapper
layout: post
date: 2013-12-16T23:27:23Z
tags: [emacs, elisp, reddit, web]
uuid: 3362934d-9762-3f58-e05c-4d8b28175367
---

A couple of months ago I wrote an Emacs Lisp wrapper for the
[reddit API](http://old.reddit.com/dev/api). I didn't put it in MELPA,
not yet anyway. If anyone is finding it useful I'll see about getting
that done. My intention was give it some exercise and testing before
putting it out there for people to use, locking down the API. You can
find it here,

 * [https://github.com/skeeto/emacs-reddit-api](https://github.com/skeeto/emacs-reddit-api)

Except for logging in, the library is agnostic about the actual API
endpoints themselves. It just knows how to translate between Elisp and
the reddit API protocol. This makes the library dead simple to use. I
had considered supporting [OAuth2 authentication][oath2] rather than
password authentication, but reddit's OAuth2 support is pretty rough
around the edges.

### Library Usage

The reddit API has two kinds of endpoints, GET and POST, so there are
really only three functions to concern yourself with.

 * `reddit-login`
 * `reddit-get`
 * `reddit-post`

And one variable,

 * `reddit-session`

The `reddit-login` function is really just a special case of
`reddit-post`. It returns a session value (cookie/modhash tuple) that
is used by the other two functions for authenticating the user. Just
as you get automatically with almost all Elisp data structures —
probably more so than *any* other popular programming language — it
can be serialized with the printer and reader, allowing a reddit
session to be maintained across Emacs sessions.

The return value of `reddit-login` generally doesn't need to be
captured. It automatically sets the dynamic variable `reddit-session`,
which is what the other functions access for authentication. This can
be bound with `let` to other session values in order to switch between
different users.

Both `reddit-get` and `reddit-post` take an endpoint name and a list
of key-value pairs in the form of a property list (plist). (The
`api-type` key is automatically supplied.) They each return the JSON
response from the server in association list (alist) form. The actual
shape of this data matches the response from reddit, which,
unfortunately, is inconsistent and unspecified, so writing any sort of
program to operate on the API requires lots of trial and error. If the
API responded with an error, these functions signal a `reddit-error`.

Typical usage looks like so. Notice that values need not be only
strings; they just need to print to something reasonable.

~~~cl
;; Login first
(reddit-login "your-username" "your-password")

;; Subscribe to a subreddit
(reddit-post "/api/subscribe" '(:sr "t5_2s49f" :action sub))

;; Post a comment
(reddit-post "/api/comment/" '(:text "Hello world." :thing_id "t1_cd3ar7y"))
~~~

For plists keys I considered automatically converting between dashes
and underscores so that the keywords could have Lisp-style names. But
the reddit API is inconsistent, using both, so there's no correct way
to do this.

To further refine the API it might be worth defining a function for
each of the reddit endpoints, forming a facade for the wrapper
library, hiding way the plist arguments and complicated responses.
That would eliminate the trial and error of using the API.

~~~cl
(defun reddit-api-comment (parent comment)
  (if (null reddit-session)
      (error "Not logged in.")
    ;; TODO: reduce the return value into a thing/struct
    (reddit-post "/api/comment/" '(:thing_id parent :text comment))))
~~~

Furthermore there could be defstructs for comments, posts, subreddits,
etc. so that the "thing" ID stuff is hidden away. This is basically
what was already done for sessions out of necessity. I might add these
structs and functions someday but I don't currently have a need for
it.

It would be neat to use this API to create an interface to reddit from
within Emacs. I imagine it might look like one of the Emacs mail
clients, or [like Elfeed][elfeed]. Almost everything, including
viewing image posts within Emacs, should be possible.

### Background

For the last 3.5 years I've been a moderator of [/r/civ][civ],
[starting back when it had about 100 subscribers][mod]. As of this
writing it's just short of 60k subscribers and we're now up to 9
moderators.

A few months ago we decided to institute a self-post-only Sunday. All
day Sunday, midnight to midnight Eastern time, only self-posts are
allowed in the subreddit. One of the other moderators was turning this
on and off manually, so I offered to write a bot to do the job. There
[weren't any Lisp wrappers yet][wrappers] (though raw4j could be used
with Clojure), so I decided to write one.

As mentioned before, the reddit API leaves *a lot* to be desired. It
randomly returns errors, so a correct program needs to be prepared to
retry requests after a short delay, depending on the error. My
particular annoyance is that the `/api/site_admin` endpoint requires
that most of its keys are supplied, and it's not documented which ones
are required. Even worse, there's no single endpoint to get all of the
required values, the key names between endpoints are inconsistent, and
even the values themselves can't be returned as-is, requiring
[massaging/fixing before returning them back to the API][bug].

I hope other people find this library useful!


[oath2]: http://blog.jenkster.com/2013/10/an-oauth2-in-emacs-example.html
[civ]: http://old.reddit.com/r/civ
[mod]: http://old.reddit.com/r/civ/comments/clxj4/lets_tidy_rciv_up_a_bit/
[wrappers]: https://github.com/reddit/reddit/wiki/API-Wrappers
[bug]: http://old.reddit.com/r/bugs/comments/1t162o/
[elfeed]: /blog/2013/09/04/
