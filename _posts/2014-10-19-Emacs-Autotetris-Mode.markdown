---
title: Emacs Autotetris Mode
layout: post
date: 2014-10-19T21:45:53Z
tags: [emacs, elisp, interactive]
uuid: e76556be-ebeb-3f65-7041-bffbe2e19952
---

For more than a decade now, Emacs has come with a built-in Tetris
clone, originally written by XEmacs' Glynn Clements. Just run `M-x
tetris` any time you want to play. For anyone too busy to waste time
playing Tetris, earlier this year I wrote an autotetris-mode that will
play the Emacs game automatically.

* [https://github.com/skeeto/autotetris-mode](https://github.com/skeeto/autotetris-mode)

Load the source, `autotetris-mode.el` and `M-x autotetris`. It will
start the built-in Tetris but make all the moves itself. It works best
when byte compiled.

![](/img/diagram/tetris/screenshot.png)

At the time I had read [an article][orig] and was interested in trying
my hand at my own Tetris AI. Like most things Emacs, the built-in
Tetris game is very hackable. It's also pretty simple and easy to
understand. Rather than write my own I chose to build upon this one.

### Heuristics

It's not a particularly strong AI. It doesn't pay attention to the
next piece in queue, it doesn't know the game's basic shapes, and it
doesn't try to maximize the score (clearing multiple rows at once).
The goal is to continue running for as long as possible. But since
it's able to get to the point where the game is so fast that the AI is
unable to move pieces fast enough (it's rate limited like a human
player), that means it's good enough.

When a new piece appears at the top of the screen, the AI, in memory,
tries placing it in all possible positions and all possible
orientations. For each of these positions it runs a heuristic on the
resulting game state, summing five metrics. Each metric is scaled by a
hand-tuned weight to adjust its relative priority. Smaller is better,
so the position with the lowest score is selected.

#### Number of Holes

![](/img/diagram/tetris/holes.png)

A hole is any open space that has a solid block above it, even if that
hole is accessible without passing through a solid block. Count these
holes.

#### Maximum Height

![](/img/diagram/tetris/height.png)

Add the height of the tallest column. Column height includes any holes
in the column. The game ends when a column touches the top of the
screen (or something like that), so this should be kept in check.

#### Mean Height

![](/img/diagram/tetris/mean.png)

Add the mean height of all columns. The higher this is, the closer we
are to losing the game. Since each row will have at least one hole,
this will be a similar measure to the hole count.

#### Height Disparity

![](/img/diagram/tetris/disparity.png)

Add the difference between the shortest column height and the tallest
column height. If this number is large it means we're not making
effective use of the playing area. It also discourages the AI from
getting into that annoying situation we all remember: when you
*really* need a 4x1 piece that never seems to come. Those are the
brief moments when I truly believe the version I'm playing has to be
rigged.

#### Surface Roughness

![](/img/diagram/tetris/surface.png)

Take the root mean square of the column heights. A rougher surface
leaves fewer options when placing pieces. This measure will be similar
to the disparity measurement.

### Emacs-specific Details

With a position selected, the AI sends player inputs at a limited rate
to the game itself, moving the piece into place. This is done by
calling `tetris-move-right`, `tetris-move-left`, and
`tetris-rotate-next`, which, in the normal game, are bound to the
arrow keys.

The built-in tetris-mode isn't quite designed for this kind of
extension, so it needs a little bit of help. I defined two pieces of
advice to create hooks. These hooks alert my AI to two specific events
in the game: the game start and a fresh, new piece.

~~~cl
(defadvice tetris-new-shape (after autotetris-new-shape-hook activate)
  (run-hooks 'autotetris-new-shape-hook))

(defadvice tetris-start-game (after autotetris-start-game-hook activate)
  (run-hooks 'autotetris-start-game-hook))
~~~

I talked before about [the problems with global state][global].
Fortunately, tetris-mode doesn't store any game state in global
variables. It stores everything in buffer-local variables, which can
be exploited for use in the AI. To perform the "in memory" heuristic
checks, it creates a copy of the game state and manipulates the copy.
The copy is made by way of `clone-buffer` on the `*Tetris*` buffer.
The tetris-mode functions all work equally as well on the clone, so I
can use the existing game rules to properly place the next piece in
each available position. The game's own rules take care of clearing
rows and checking for collisions for me. I wrote an
`autotetris-save-excursion` function to handle the messy details.

~~~cl
(defmacro autotetris-save-excursion (&rest body)
  "Restore tetris game state after BODY completes."
  (declare (indent defun))
  `(with-current-buffer tetris-buffer-name
     (let ((autotetris-saved (clone-buffer "*Tetris-saved*")))
       (unwind-protect
           (with-current-buffer autotetris-saved
             (kill-local-variable 'kill-buffer-hook)
             ,@body)
         (kill-buffer autotetris-saved)))))
~~~

The `kill-buffer-hook` variable is also cloned, but I don't want
tetris-mode to respond to the clone being killed, so I clear out the
hook.

That's basically all there is to it! While watching it feels like it's
making dumb mistakes, not placing pieces in optimal positions, but it
recovers well from these situations almost every time, so it must know
what it's doing. Currently it's a better player than me, which is [my
rule-of-thumb][chess] for calling an AI successful.


[orig]: http://www.cs.cornell.edu/boom/1999sp/projects/tetris/
[global]: /blog/2014/10/12/
[chess]: /blog/2011/08/24/
