---
title: Live CSS Interaction with Skewer
layout: post
tags: [emacs, web]
uuid: 92c8a519-1e4c-374b-7f90-37b1dadfc862
---

This evening [Skewer](/blog/2012/10/31/) gained support for live CSS.
When editing CSS code, you can send your rules and declarations from
the editing buffer to be applied in the open page in the browser. It
makes experimenting with CSS really, really easy. The functionality is
exposed through the familiar interaction keybindings, so if you're
already familiar with other Emacs interaction modes
([SLIME](/blog/2010/01/15/), [nREPL](/blog/2013/01/07/), Skewer,
[Geiser][geiser], Emacs Lisp), this should feel right at home.

To provide the keybindings in css-mode there's a new minor mode,
skewer-css-mode. CSS "expressions" are sent to the browser through the
communication channel already provided by Skewer. It's essentially an
extension to Skewer: it could have been created without making any
changes to Skewer itself.

Unfortunately Emacs' css-mode is nowhere near as sophisticated as
js2-mode — which reads in and exposes a full JavaScript AST. I had to
write my own very primitive CSS parsing routines to tease things
apart. It should generally be able to parse declarations and rules
reasonably no matter how it's indented, but it's not very good at
navigating *around* comments, especially when they contain CSS
syntax. If I find a way to parse CSS more easily sometime I'll see
about fixing it, but it's plenty good enough for now.

To "evaluate" the CSS, the code is simply dropped into the page as a
new `<style>` tag. I had considered other approaches, but this seemed
to be by far the simplest way to support arbitrary selectors and
shorthand properties. The more programmatic approaches would require
re-writing something that browser already does.

The consequence of this is that every "evaluation" adds a new
`<style>` tag to the page, which adds more and more load to style
computation, most of which completely mask each other. Since there's
no way to tell when a particular `<style>` tag has been completely
masked I can't remove any of them from the page. That might revert a
declaration that's still in usde. I haven't seen it happen yet but I
wonder if it's possible to run into browser problems during extended
CSS interaction, when thousands of stylesheets have built up on a
single page. Time will tell.

Just before doing all this, I added full support for Cross-resource
Resource Sharing (CORS), which means *any* page from any server can be
skewered, not just pages hosted by Emacs itself ... as long as you can
get skewer.js in the page as a script. To help with that, I wrote a
[Greasemonkey userscript][gm] that can automatically skewer any
visited page. I can now manipulate from Emacs the JavaScript and CSS
of *any* page I visit in my browser. It feels really powerful. I
already have a good use for this at work right now.


[gm]: https://github.com/skeeto/skewer-mode/blob/master/skewer-everything.user.js
[geiser]: http://www.nongnu.org/geiser/
