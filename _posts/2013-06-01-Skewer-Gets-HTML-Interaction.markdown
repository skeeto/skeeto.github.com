---
title: Skewer Gets HTML Interaction
layout: post
tags: [javascript, emacs, web]
uuid: f8c13ac6-2da6-3851-497c-8785db8a203e
---

A month ago Zane Ashby made a pull request that [added another minor
mode to Skewer][issue]: skewer-html-mode. It's analogous to the
skewer-css minor mode in that it evaluates HTML "expressions" in the
context of the current page. The original pull request was mostly a
proof of concept, with evaluated HTML snippets being appended to the
end of the page (`body`) unless a target selector is manually
specified.

This mode is still a bit rough around this edges, but since I think
it's useful enough for productive work I've merged it in.

### Replacing HTML

Unsatisfied with just appending content, I ran with the idea and
updated it to automatically *replace* structurally-matching content on
the page when possible. Zane's fundamental idea remained intact: a CSS
selector is sent to the browser along with the HTML. Skewer running in
the browser uses `querySelector()` to find the relevant part of the
document and replaces it with the provided HTML. This is done with the
command `skewer-html-eval-tag` (default: `C-M-x`), which selects the
innermost tag enclosing the point.

To accomplish this, an important piece of skewer-html exists to
compute this CSS selector. It's a purely structural selector, ignoring
classes, IDs, and so on, instead relying on the pseudo-selector
[:nth-of-type][nth-of-type]. For example, say this is the content of
the buffer and the point is somewhere inside the second heading (Bar).

~~~html
<html>
  <head></head>
  <body>
    <div id="main">
      <h1>Foo</h1>
      <p>I am foo.</p>
      <h1>Bar</h1>
      <p>I am bar.</p>
    </div>
  </body>
</html>
~~~

The function `skewer-html-compute-selector` will generate this
selector. Note that :nth-of-type is 1-indexed.

    body:nth-of-type(1) > div:nth-of-type(1) > h1:nth-of-type(2)

The `>` syntax requires that these all be direct descendants and
\:nth-of-type allows it to ignore all those paragraph elements. This
means other types of elements can be added around these headers, like
additional paragraphs, without changing the selector. The :nth-of-type
on `body` is obviously unnecessary, but this is just to keep
skewer-html dead simple. It doesn't need to know the semantics of
HTML, just the surface syntax. There will only ever be one `body` tag,
but to skewer-html it's just another HTML tag.

Side note: this is why I *strongly* prefer to use `/>` self-closing
syntax in HTML5 even though it's unnecessary. Unlike XML, that closing
slash is treated as whitespace and it's impossible to self-close tags.
The schema specifies which tags are "void" (always self-closing:
`img`, `br`) and which tags are "normal" (explicitly closed: `script`,
`canvas`). This means if you *don't* use `/>` syntax, your editor
would need to know the HTML5 schema in order to properly understand
the syntax. I prefer not to require this of a text editor — or
anything else doing dumb manipulations of HTML text — especially with
the HTML5 specification constantly changing.

When I was writing this I originally included `html` in the selector.
Selector computation would just walk up to the root of the document
regardless of what the tags were. Curiously, including this causes the
selector to fail to match even though this is literally the page
structure. So, out of necessity, skewer-html knows enough to leave it
off.

For replacement, rather than a simple `innerHTML` assignment on the
selected element, Skewer is parsing the HTML into an node object,
removing the selected node object, and putting the new one in its
place. The reason for this is that I want to include all of the
replacement element's attributes.

Another HTML oddity is that the `body` and `head` elements cannot be
replaced. It's a limitation of the DOM. This means these tags cannot
be "evaluated" directly, only their descendants. Brian and I also ran
into this issue in [impatient-mode][imp] while trying to work around a
strange HTML encoding corner case: scripts loaded with a `script` tag
created by `document.write()` are parsed with a different encoding
than when loaded directly by adding a `script` element to the page.

This last part is actually a small saving grace for skewer-css, which
works by appending new stylesheets to the end of `body`. Why `body`
and not `head`? Because some documents out there have stylesheets
linked from `body`, and properly overriding these requires appending
stylesheets *after* them. If `body` is replaced by skewer-html, all of
the dynamic stylesheets appended by skewer-css would be lost,
reverting the style of the page. Since we can't do that, this isn't an
issue!

### Appending HTML

So what happens when the selector doesn't match anything in the
current document? Skewer fills in the missing part of the structure
and sticks the content in the right place. Next time the tag is
evaluated, the structure exists and it becomes a replacement
operation. This means the document in the browser can start completely
empty (like the `run-skewer` page) and you can fill in content as you
write it.

But what if the page already has content? There's an interactive
command `skewer-html-fetch-selector-into-buffer`. You select a part of
the page and it gets inserted into the current buffer (probably a
scratch buffer). The idea is that you can then modify and then
evaluate it to update the page. This is the roughest part of
skewer-html right now since I'm still figuring out a good workflow
around it.

If you have Skewer installed and updated, you already have
skewer-html. It was merged into `master` about a month ago. If you
have any ideas or opinions for how you think this minor mode should
work, please share it. The intended workflow is still not a
fully-formed idea.


[issue]: https://github.com/skeeto/skewer-mode/pull/19
[nth-of-type]: https://developer.mozilla.org/en-US/docs/Web/CSS/:nth-of-type
[imp]: http://www.50ply.com/blog/2012/08/13/introducing-impatient-mode/
