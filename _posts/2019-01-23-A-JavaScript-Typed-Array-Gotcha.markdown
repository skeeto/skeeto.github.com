---
title: A JavaScript Typed Array Gotcha
layout: post
date: 2019-01-23T02:50:30Z
tags: [c, javascript, lang]
uuid: da8bd8c0-6003-3d77-5409-16db63420368
---

JavaScript's prefix increment and decrement operators can be
surprising when applied to typed arrays. It caught be by surprise when
I was [porting some C code][ulidc] over [to JavaScript][ulidjs] Just
using your brain to execute this code, what do you believe is the
value of `r`?

```js
let array = new Uint8Array([255]);
let r = ++array[0];
```

The increment and decrement operators originated in the B programming
language. Its closest living relative today is C, and, as far as these
operators are concered, C can be considered an ancestor of JavaScript.
So what is the value of `r` in this similar C code?

```c
uint8_t array[] = {255};
int r = ++array[0];
```

Of course, if they were the same then there would be nothing to write
about, so that should make it easier to guess if you aren't sure. The
answer: In JavaScript, `r` is 256. In C, `r` is 0.

What happened to me was that I wrote an 80-bit integer increment
routine in C like this:

```c
uint8_t array[10];
/* ... */
for (int i = 9; i >= 0; i--)
    if (++array[i])
        break;
```

But I was getting the wrong result over in JavaScript from essentially
the same code:

```js
let array = new Uint8Array(10);
/* ... */
for (let i = 9; i >= 0; i--)
    if (++array[i])
        break;
```

So what's going on here?

### JavaScript specification

The ES5 specification says this about [the prefix increment
operator][es5pre]:

> Let *expr* be the result of evaluating UnaryExpression.
>
> 1. Throw a SyntaxError exception if the following conditions are all
>    true: [omitted]
>
> 2. Let *oldValue* be ToNumber(GetValue(*expr*)).
>
> 3. Let *newValue* be the result of adding the value 1 to *oldValue*,
>    using the same rules as for the + operator (see 11.6.3).
>
> 4. Call PutValue(*expr*, *newValue*).
>
> Return *newValue*.

So, *oldValue* is 255. This is a double precision float because all
numbers in JavaScript (outside of the bitwise operations) are double
precision floating point. Add 1 to this value to get 256, which is
*newValue*. When *newValue* is stored in the array via PutValue(), it's
converted to an unsigned 8-bit integer, which truncates it to 0.

However, *newValue* is returned, not the value that was actually stored
in the array!

Since JavaScript is dynamically typed, this difference did not
actually matter until typed arrays are involved. I suspect if typed
arrays were in JavaScript from the beginning, the specified behavior
would be more in line with C.

This behavior isn't limited to the prefix operators. Consider
assignment:

```js
let array = new Uint8Array([255]);
let r = (array[0] = array[0] + 1);
let s = (array[0] += 1);
```

Both `r` and `s` will still be 256. The result of the assignment
operators is a similar story:

>  LeftHandSideExpression = AssignmentExpression is evaluated as
>  follows:
>
> 1. Let *lref* be the result of evaluating LeftHandSideExpression.
>
> 2. Let *rref* be the result of evaluating AssignmentExpression.
>
> 3. Let *rval* be GetValue(*rref*).
>
> 4. Throw a SyntaxError exception if the following conditions are all
>    true: [omitted]
>
> 5. Call PutValue(*lref*, *rval*).
>
> 7. Return *rval*.

Again, the result of the expression is independent of how it was stored
with PutValue().

### C specification

I'll be referencing the original C89/C90 standard. The C specification
requires a little more work to get to the bottom of the issue. Starting
with 3.3.3.1 (Prefix increment and decrement operators):

> The value of the operand of the prefix ++ operator is incremented. The
> result is the new value of the operand after incrementation. The
> expression ++E is equivalent to (E+=1).

Later in 3.3.16.2 (Compound assignment):

> A compound assignment of the form E1 op = E2 differs from the simple
> assignment expression E1 = E1 op (E2) only in that the lvalue E1 is
> evaluated only once.

Then finally in 3.3.16 (Assignment operators):

> An assignment operator stores a value in the object designated by
> the left operand. An assignment expression has the value of the left
> operand **after the assignment**, but is not an lvalue.

So the result is explicitly the value after assignment. Let's look at
this step by step after rewriting the expression.

```c
int r = (array[0] = array[0] + 1);
```

In C, all integer operations are performed with *at least* `int`
precision. Smaller integers are implicitly promoted to `int` before the
operation. The value of array[0] is 255, and, since `uint8_t` is smaller
than `int`, it gets promoted to `int`. Additionally, the literal
constant 1 is also an `int`, so there are actually two reasons for this
promotion.

So since these are `int` values, the result of the addition is 256, like
in JavaScript. To store the result, this value is then demoted to
`uint8_t` and truncated to 0. Finally, this post-assignment 0 is the
result of the expression, not the right-hand result as in JavaScript.

### Specifications are useful

These situations are why I prefer programming languages that have a
formal and approachable specification. If there's no specification and
I'm observing [undocumented, idiosyncratic behavior][matlab], is this
just some subtle quirk of the current implementation — e.g. something
that might change without notice in the future — or is it intended
behavior that I can rely upon for correctness?


[es5ass]: https://es5.github.io/#x11.13.1
[es5pre]: https://es5.github.io/#x11.4.4
[matlab]: https://old.reddit.com/r/matlab/comments/ae68bh/_/edmysxr/
[ulidc]: https://github.com/skeeto/ulid-c
[ulidjs]: https://github.com/skeeto/ulid-js
