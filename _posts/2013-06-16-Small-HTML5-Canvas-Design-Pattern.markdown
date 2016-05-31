---
title: An HTML5 Canvas Design Pattern
layout: post
tags: [javascript, java, lisp]
uuid: 841e3d35-6d30-380d-ffdc-96b37f991ce7
---

I've been falling into a particular design pattern when using the
HTML5 Canvas element. By "design pattern" I don't mean some pretty
arrangement but rather a [software design pattern][dp]. This one's a
very Lisp-like pattern, and I wonder if I would have come up with it
if I hadn't first seen it in Lisp. It can also be applied to the Java
2D API, though less elegantly.

First, a review.

### Drawing Basics

A canvas is just another element in the page.

~~~html
<canvas id="display" width="200" height="200"></canvas>
~~~

To draw onto it, get a context and call drawing methods on it.

~~~javascript
var ctx = document.getElementById('display').getContext('2d');
ctx.fillStyle = 'blue';
ctx.beginPath();
ctx.arc(100, 100, 75, 0, Math.PI * 3 / 2);
ctx.fill();
~~~

This will result in a canvas that looks like this,

![](/img/screenshot/canvas-arc.png)

Here's how to do the same thing with Java 2D. Very similar, except the
canvas is called a JComponent and the context is called a Graphics2D.
As you could imagine from this example, Java 2D API is much richer,
and more object-oriented than the Canvas API. The cast from Graphics
to Graphics2D is required due to legacy.

~~~java
public class Example extends JComponent {
    public void paintComponent(Graphics graphics) {
        Graphics2D g = (Graphics2D) graphics;
        g.setColor(Color.BLUE);
        g.fill(new Arc2D.Float(25, 25, 150, 150, 0, 360, Arc2D.CHORD));
    }
}
~~~

An important feature of both is the ability to globally apply
transforms — translate, scale, shear, and rotate — to all drawing
commands. For example, drawings on the canvas can be vertically scaled
using the `scale()` method. Graphics2D also has a `scale()` method.

~~~javascript
// ...
ctx.scale(1, 0.5);
// ...
~~~

![](/img/screenshot/canvas-arc-scaled.png)

For both JavaScript and Java the rendered image isn't being
*stretched*. Instead, the input vertices are being transformed before
rendering to pixels. This is what makes it possible to decouple the
*screen* coordinate system from the program's internal coordinate
system. Outside of rare performance concerns, the program's internal
logic shouldn't be written in terms of pixels. It should rely on these
transforms to convert between coordinate systems at rendering time,
allowing for a moving camera.

### The Transform Stack

Both cases also allow the current transform to be captured and
restored. Not only does this make it easier for a function to clean up
after itself and properly share the canvas with other functions, but
also multiple different coordinate transforms can be *stacked* on top
of each other. For example, the bottom transform might convert between
internal coordinates and screen coordinates. When it comes time to
draw a minimap, another transform can be pushed on top and the same
exact drawing methods applied to the canvas.

This is where Canvas and Java 2D start to differ. Both got some aspect
right and some aspect wrong, and I wish I could easily have the best
of both.

In canvas, this is literally a stack, and there are a pair of methods,
`save()` and `restore()` for pushing and popping the transform matrix
on an internal stack. The above JavaScript example may be in a
function that is called more than once, so it should restore the
transform matrix before returning.

~~~javascript
ctx.save();
ctx.scale(1, 0.5);
// ... draw ...
ctx.restore();
~~~

In Java this stack is managed manually, and it (typically) sits inside
the call stack itself as a variable.

~~~java
AffineTransform tf = g.getTransform();
g.scale(1, 0.5);
// ... draw ...
g.setTransform(tf);
~~~

I think Canvas's built-in stack is more elegant than managing an
extraneous variable and object. However, what's significant about Java
2D is that **we actually have access to the transform matrix**. It's
that [AffineTransform][at] object. The Canvas transform matrix is an
internal, inaccessible data structure. It has an established external
representation, [SVGMatrix][svg], but it won't provide a copy. If one
of these is needed, a separate matrix must to be maintained in
parallel. What a pain!

Why would we need the transform matrix? **So that we can transform
coordinates in reverse!** When a user interacts with the display, the
program receives *screen* coordinates. To be useful, these need to be
converted into internal coordinates so that the program can determine
where in the world the user clicked. The Java AffineTransform class
has a `createInverse()` method for computing this inverse transform.
This is something I really miss having when using Canvas. It's such an
odd omission.

### The Design Pattern

So, back to the design pattern. When it comes time draw something, a
transform is established on the context, something is drawn to the
context, then finally the transform is removed. The word "finally"
should stand out here. If we're being careful, we should put the
teardown step inside a `finally` block. If something goes wrong, the
context will be left in a clean state. This has personally helped me
in debugging.

~~~javascript
ctx.save();
ctx.scale(1, 0.5);
try {
    // ... draw ...
} finally {
    ctx.restore();
}
~~~

In Lisp, this pattern is typically captured as a `with-` macro.

 1. Perform setup
 2. Run body
 3. Teardown
 4. Return the body's return value

Instead of `finally`, the special form `unwind-protect` is used to
clean up regardless of any error condition. Here's a simplified
version of Emacs' `with-temp-buffer` macro, which itself is built on
another `with-` macro, `with-current-buffer`.

~~~cl
(defmacro with-temp-buffer (&rest body)
  `(let ((temp-buffer (generate-new-buffer " *temp*")))
     (with-current-buffer temp-buffer
       (unwind-protect
           (progn ,@body)
         (kill-buffer temp-buffer)))))
~~~

The setup is to create a new buffer and switch to it. The teardown
destroys the buffer, regardless of what happens in the body. An
example from Common Lisp would be `with-open-file`.

~~~cl
(with-open-file (stream "/etc/passwd")
  (loop while (listen stream)
     collect (read-line stream)))
~~~

This macro ensures that the stream is closed when the body exits, no
matter what. (Side note: this can be very surprising when combined
with Clojure's laziness!)

There are no macros in JavaScript, let alone Lisp's powerful macro
system, but the pattern can still be captured using closures. Replace
the body with a callback.

~~~javascript
function Transform() {
    // ...
}

// ...

Transform.prototype.withDraw = function(ctx, callback) {
    ctx.save();
    this.applyTransform(ctx);
    try {
        callback();
    } finally {
        ctx.restore();
    }
};
~~~

The callback is called once the context is in the proper state. Here's
how it would be used.

~~~javascript
var transform = new Transform().scale(1, 0.5);  // (fluent API)

function render(step) {
    transform.withDraw(ctx, function() {
        // ... draw ...
    });
}
~~~

Since JavaScript has proper closures, that `step` variable is
completely available to the callback. This function-as-body pattern
comes up a lot (e.g. [AMD][amd]), and seeing it work so well makes me
think of JavaScript as a "suitable Lisp."

Java can just barely pull off the pattern using anonymous classes, but
it's very clunky.

~~~java
class Transform {
    // ...

    AffineTransform transform;

    public void withDraw(Graphics2D g, Runnable callback) {
        AffineTransform original = g.getTransform();
        g.transform(transform);
        try {
            callback.run();
        } finally {
            g.setTransform(original);
        }
    }
}

class Foo {
    // ...

    Transform transform;

    public void render(Graphics2D g, double step) {
        transform.withDraw(g, new Runnable() {
            public void run() {
                // ... draw ...
            }
        });
    }
}
~~~

Java's anonymous classes are closures, but, unlike Lisp and
JavaScript, they close over *values* rather than *bindings*. Purely in
attempt to hide this complexity, Java requires that variables accessed
from the anonymous class be declared as `final`. It's awkward and
confusing enough that I probably wouldn't try to apply it in Java.

I think this pattern works very well with JavaScript, and if you dig
around in some of my graphical JavaScript you'll see that I've already
put it to use. JavaScript functions work pretty well as a stand in for
some kinds of Lisp macros.


[canvas]: https://developer.mozilla.org/en-US/docs/Canvas_tutorial
[dp]: http://en.wikipedia.org/wiki/Software_design_pattern
[java]: http://en.wikipedia.org/wiki/Java_2D
[amd]: http://requirejs.org/docs/whyamd.html
[svg]: http://www.w3.org/TR/SVGTiny12/svgudom.html#svg__SVGMatrix
[at]: http://docs.oracle.com/javase/7/docs/api/java/awt/geom/AffineTransform.html
