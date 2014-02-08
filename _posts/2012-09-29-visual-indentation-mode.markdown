---
title: Emacs visual-indentation-mode
layout: post
tags: [emacs]
uuid: 6171fe76-7d01-3230-66b1-a9fa03078001
---

I watched this presentation last night about introducing Clojure in
the workplace, [Bootstrapping Clojure][video]. Most of the video is
Tyler presenting code he wrote, so there's a lot of Clojure code
displayed in the presentation. The way he presented the code itself
was interesting: indentation was highlighted in alternating shades of
gray.

Such emphasis on indentation could be useful specifically for reading
Lisp, because, for humans, what makes s-expressions readable isn't the
parenthesis but the indentation. That's why us Lispers shake our heads
when non-Lispers complain that there are too many parenthesis. We
don't notice them!

He's a Vim user so I assume this is some Vim extension, but maybe it's
just the output from a particular pretty printer. I thought it would
be interesting to have this feature in Emacs, so I created a minor
mode for it.

 * [https://github.com/skeeto/visual-indentation-mode][repo]

Here's what it looks like with the default Emacs theme.

![](/img/emacs/visual-indentation-mode-lisp.png)

And some Java with `visual-indentation-width` set to 4,

![](/img/emacs/visual-indentation-mode-java.png)

Dark themes (like the Wombat theme I personally use) will work as
well, with the indentation highlighting appearing darker.

It can be enabled by default in all programming modes easily,

~~~cl
(add-hook 'prog-mode-hook 'visual-indentation-mode)
~~~

It completely falls apart when tabs are used for indentation, which
Emacs will use by default. [My configuration](/blog/2011/10/19/)
forbids tabs (tabs are stupid, people!) but I still need to edit other
people's code containing tabs. I don't think there's a way to apply
highlighting to *part* of a tab, so I'm not sure if there's a way to
fix that. Because I don't intend to actually use the mode regularly,
it's just a proof of concept, and fixing it would be non-trivial, I
don't intend to fix it.

### See Also

 * [Highlighting indentation for Emacs](https://github.com/antonj/Highlight-Indentation-for-Emacs/)
 * [Show White Space](http://emacswiki.org/emacs/ShowWhiteSpace)

[video]: http://www.infoq.com/presentations/Bootstrapping-Clojure
[repo]: https://github.com/skeeto/visual-indentation-mode
