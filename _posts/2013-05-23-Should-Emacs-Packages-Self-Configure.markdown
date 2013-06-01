---
title: Should Emacs Packages Self Configure?
layout: post
tags: [emacs]
uuid: 3421bb8a-23e9-3f5f-3329-f3ec256a91af
---

_Update 2013-06-01_: I ultimately decided that Skewer should *not*
modify any mode hooks automatically. Instead the major mode hooks can
be configured by putting `(skewer-setup)` in your initialization file.
This function is designed to play well with autoloading, so using it
won't increase your startup time.

There's a discussion happening on a Skewer issue on GitHub:
[Problems with skewer-css autoload][issue]. The issue was opened by
Steve Purcell. Right now Skewer's CSS minor mode is enabled by default
in less-css-mode, which is, of course, incompatible with the minor
mode. It's enabled because less-css-mode is derived from css-mode, so
it runs all of css-mode's hooks.

There are actually two separate problems here.

 * The hook to activate the minor mode needs to check what major mode
   is activating it, because css-mode-hook is run by other modes. This
   is easy to fix, though it's not very elegant. I would need to do
   the same for skewer-html-mode.

 * Skewer is eagerly configuring itself once it's installed. This is
   intentional: I want Skewer to be *really* easy to use right
   out-of-the-box. There's no "install the package, then add these
   lines to your startup configuration." If I remove this behavior,
   the previous problem becomes the user's problem, since it's up to
   them to activate the minor mode.

Steve is telling me that this auto-configuration is a bad idea; it so
often causes these sorts of messes. The gist of his argument is that
*installing* a package is separate from *enabling* a package. It's up
to the user to decide how and when they want to use a package. Steve
maintains a number of popular Emacs packages and, even more
importantly, he's one of the MELPA maintainers. He knows this stuff
much better than I do. He even used to share my current opinion
[up until two months ago][older] when someone changed his mind.

On the other hand, I really dislike when software has such awful
defaults that it's unusable without first configuring it. Skewer's
case wouldn't be too bad since it can be enabled manually without
editing any configuration, but practical use would require that users
configure Skewer in their startup files. It would also make it harder
for users to discover Skewer's features. They might not otherwise even
be aware there are CSS and HTML minor modes! If the concern is
separating installation and activation, package.el *does* have a
variable, `package-load-list`, for this purpose, though using it isn't
very convenient.

Right now I'm stuck in this dilemma like a deer caught in the
headlights. Formal packages are a very new thing for Emacs, so there
doesn't seem to be a community consensus on this issue yet. I really
like when the packages I install behave like Skewer does now (i.e.
[nrepl.el][nrepl]) so I don't need to configure them. But I would also
be easily frustrated if that configuration magic was getting in my
way. This particular annoyance happens to me outside of Emacs often
enough (i.e. Chromium), though it's worse because it generally can't
easily be fixed like in Emacs.

I'm still trying to make up my mind about this. If you have an opinion
on the matter I'd like to hear it. You can leave a comment here, or
much better, leave your comment on the issue on GitHub. It's not going
to come down to a vote or anything like that. I just want to get a
feel for how people expect Emacs packages to work.


[issue]: https://github.com/skeeto/skewer-mode/issues/22
[older]: https://github.com/purcell/elisp-slime-nav/pull/6
[nrepl]: https://github.com/kingtim/nrepl.el
