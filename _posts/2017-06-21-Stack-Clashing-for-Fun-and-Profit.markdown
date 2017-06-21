---
title: Stack Clashing for Fun and Profit
layout: post
date: 2017-06-21T05:28:56Z
tags: [c, posix, netsec]
uuid: 43402771-3340-3dff-c18f-7110caeedb7d
---

*Stack clashing* has been in the news lately due to [some recently
discovered vulnerablities][qualys] along with proof-of-concept
exploits. As the announcement itself notes, this is not a new issue,
though this appears to be the first time it's been given this
particular name. I do know of one "good" use of stack clashing, where
it's used for something productive than as part of an attack. In this
article I'll explain how it works.

You can find the complete code for this article here, ready to run:

* <https://github.com/skeeto/stack-clash-coroutine>

But first, what is a stack clash? Here's a rough picture of the
typical way process memory is laid out. The stack starts at a high
memory address and grows downwards. Code and static data sit at low
memory, with a `brk` pointer growing upward to make small allocations.
In the middle is the heap, where large allocations and memory mappings
take place.

![](/img/diagram/process-memory.svg)

Below the stack is a slim *guard page* that divides the stack and the
region of memory reserved for the heap. Reading or writing to that
memory will trap, causing the program to crash or some special action
to be taken. The goal is to prevent the stack from growing into the
heap, which could cause all sorts of trouble, like security issues.

The problem is that this thin guard page isn't enough. It's possible to
put a large allocation on the stack, never read or write to it, and
completely skip over the guard page, such that the heap and stack
overlap without detection.

Once this happens, writes into the heap will change memory on the
stack and vice versa. If an attacker can cause the program to make
such a large allocation on the stack, then legitimate writes into
memory on the heap can manipulate local variables or [return pointers,
changing the program's control flow][rop]. This can bypass buffer
overflow protections, such as stack canaries.

### Binary trees and coroutines

![](/img/diagram/binary-search-tree.svg)

Now, I'm going to abruptly change topics to discuss binary search
trees. We'll get back to stack clash in a bit. Suppose we have a
binary tree which we would like to iterate depth-first. For this
demonstration, here's the C interface to the binary tree.

~~~c
struct tree {
    struct tree *left;
    struct tree *right;
    char *key;
    char *value;
};

void  tree_insert(struct tree **, char *k, char *v);
char *tree_find(struct tree *, char *k);
void  tree_visit(struct tree *, void (*f)(char *, char *));
void  tree_destroy(struct tree *);
~~~

An empty tree is the NULL pointer, hence the double-pointer for
insert. In the demonstration it's an unbalanced search tree, but this
could very well be a balanced search tree with the addition of another
field on the structure.

For the traversal, first visit the root node, then traverse its left
tree, and finally traverse its right tree. It makes for a simple,
recursive definition — the sort of thing you'd teach a beginner.
Here's a definition that accepts a callback, which the caller will use
to *visit* each key/value in the tree. This really is as simple as it
gets.

~~~c
void
tree_visit(struct tree *t, void (*f)(char *, char *))
{
    if (t) {
        f(t->key, t->value);
        tree_visit(t->left, f);
        tree_visit(t->right, f);
    }
}
~~~

Unfortunately this isn't so convenient for the caller, who has to
split off a callback function that [lacks context][ctx], then hand
over control to the traversal function.

~~~c
void
printer(char *k, char *v)
{
    printf("%s = %s\n", k, v);
}

void
print_tree(struct tree *tree)
{
    tree_visit(tree, printer);
}
~~~

Usually it's much nicer for the caller if instead it's provided an
*iterator*, which the caller can invoke at will. Here's an interface
for it, just two functions.

~~~c
struct tree_it *tree_iterator(struct tree *);
int             tree_next(struct tree_it *, char **k, char **v);
~~~

The first constructs an iterator object, and the second one visits a
key/value pair each time it's called. It returns 0 when traversal is
complete, automatically freeing any resources associated with the
iterator.

The caller now looks like this:

~~~c
    char *k, *v;
    struct tree_it *it = tree_iterator(tree);
    while (tree_next(it, &k, &v))
        printf("%s = %s\n", k, v);
~~~

Notice I haven't defined `struct tree_it`. That's because I've got
four different implementations, each taking a different approach. The
last one will use stack clashing.

#### Manual State Tracking

With just the standard facilities provided by C, there's a some manual
bookkeeping that has to take place in order to convert the recursive
definition into an iterator. Depth-first traversal is a stack-oriented
process, and with recursion the stack is implicit in the call stack.
As an iterator, the traversal stack needs to be [managed
explicitly][trie]. The iterator needs to keep track of the path it
took so that it can backtrack, which means keeping track of parent
nodes as well as which branch was taken.

Here's my little implementation, which, to keep things simple, has a
hard depth limit of 32. It's structure definition includes a stack of
node pointers, and 2 bits of information per visited node, stored
across a 64-bit integer.

~~~c
struct tree_it {
    struct tree *stack[32];
    unsigned long long state;
    int nstack;
};

struct tree_it *
tree_iterator(struct tree *t)
{
    struct tree_it *it = malloc(sizeof(*it));
    it->stack[0] = t;
    it->state = 0;
    it->nstack = 1;
    return it;
}
~~~

The 2 bits track three different states for each visited node:

1. Visit the current node
2. Traverse the left tree
3. Traverse the right tree

It works out to the following. Don't worry too much about trying to
understand how this works. My point is to demonstrate that converting
the recursive definition into an iterator complicates the
implementation.

~~~c
int
tree_next(struct tree_it *it, char **k, char **v)
{
    while (it->nstack) {
        int shift = (it->nstack - 1) * 2;
        int state = 3u & (it->state >> shift);
        struct tree *t = it->stack[it->nstack - 1];
        it->state += 1ull << shift;
        switch (state) {
            case 0:
                *k = t->key;
                *v = t->value;
                if (t->left) {
                    it->stack[it->nstack++] = t->left;
                    it->state &= ~(3ull << (shift + 2));
                }
                return 1;
            case 1:
                if (t->right) {
                    it->stack[it->nstack++] = t->right;
                    it->state &= ~(3ull << (shift + 2));
                }
                break;
            case 2:
                it->nstack--;
                break;
        }
    }
    free(it);
    return 0;
}
~~~

Wouldn't it be nice to keep both the recursive definition while also
getting an iterator? There's an exact solution to that: coroutines.

#### Coroutines

C doesn't come with coroutines, but there are a number of libraries
available. We can also build our own coroutines. One way to do that is
with *user contexts* (`<ucontext.h>`) provided by the X/Open System
Interfaces Extension (XSI), an extension to POSIX. This set of
functions allow programs to create their own call stacks and switch
between them. That's the key ingredient for coroutines. Caveat: These
functions aren't widely available, and probably shouldn't be used in
new code.

Here's my iterator structure definition.

~~~c
#define _XOPEN_SOURCE 600
#include <ucontext.h>

struct tree_it {
    char *k;
    char *v;
    ucontext_t coroutine;
    ucontext_t yield;
};
~~~

It needs one context for the original stack and one context for the
iterator's stack. Each time the iterator is invoked, it the program
will switch to the other stack, find the next value, then switch back.
This process is called *yielding*. Values are passed between context
using the `k` (key) and `v` (value) fields on the iterator.

Before I get into initialization, here's the actual traversal
coroutine. It's nearly the same as the original recursive definition
except for the `swapcontext()`. This is the *yield*, pausing execution
and sending control back to the caller. The current context is saved
in the first argument, and the second argument becomes the current
context.

~~~c
static void
coroutine(struct tree *t, struct tree_it *it)
{
    if (t) {
        it->k = t->key;
        it->v = t->value;
        swapcontext(&it->coroutine, &it->yield);
        coroutine(t->left, it);
        coroutine(t->right, it);
    }
}
~~~

While the actual traversal is simple again, initialization is more
complicated. The first problem is that there's no way to pass pointer
arguments to the coroutine. Technically only `int` arguments are
permitted. (All the online tutorials get this wrong.) To work around
this problem, I smuggle the arguments in as global variables. This
would cause problems should two different threads try to create
iterators at the same time, even on different trees.

~~~c
static struct tree *tree_arg;
static struct tree_it *tree_it_arg;

static void
coroutine_init(void)
{
    coroutine(tree_arg, tree_it_arg);
}
~~~

The stack has to be allocated manually, which I do with a call to
`malloc()`. Nothing [fancy is needed][raw], though this means the new
stack won't have a guard page. For the stack size, I use the suggested
value of `SIGSTKSZ`. The `makecontext()` function is what creates the
new context from scratch, but the new context must first be
initialized with `getcontext()`, even though that particular snapshot
won't actually be used.

~~~c
struct tree_it *
tree_iterator(struct tree *t)
{
    struct tree_it *it = malloc(sizeof(*it));
    it->coroutine.uc_stack.ss_sp = malloc(SIGSTKSZ);
    it->coroutine.uc_stack.ss_size = SIGSTKSZ;
    it->coroutine.uc_link = &it->yield;
    getcontext(&it->coroutine);
    makecontext(&it->coroutine, coroutine_init, 0);
    tree_arg = t;
    tree_it_arg = it;
    return it;
}
~~~

Notice I gave it a function pointer, a lot like I'm starting a new
thread. This is no coincidence. There's a lot of similarity between
coroutines and multiple threads, as you'll soon see.

Finally the iterator function itself. Since NULL isn't a valid key, it
initializes the key to NULL before yielding to the iterator context.
If the iterator has no more nodes to visit, it doesn't set the key,
which can be detected when control returns.

~~~c
int
tree_next(struct tree_it *it, char **k, char **v)
{
    it->k = 0;
    swapcontext(&it->yield, &it->coroutine);
    if (it->k) {
        *k = it->k;
        *v = it->v;
        return 1;
    } else {
        free(it->coroutine.uc_stack.ss_sp);
        free(it);
        return 0;
    }
}
~~~

That's all it takes to create and operate a coroutine in C, provided
you're on a system with these XSI extensions.

#### Semaphores

Instead of a coroutine, we could just use actual threads and a couple
of semaphores to synchronize them. This is a heavy implementation and
also probably shouldn't be used in practice, but at least it's fully
portable.

Here's the structure definition:

~~~c
struct tree_it {
    struct tree *t;
    char *k;
    char *v;
    sem_t visitor;
    sem_t main;
    pthread_t thread;
};
~~~

The main thread will wait on one semaphore and the iterator thread
will wait on the other. This [should sound very familiar][emacs].

The actual traversal function looks the same, but with `sem_post()`
and `sem_wait()` as the yield.

~~~c
static void
visit(struct tree *t, struct tree_it *it)
{
    if (t) {
        it->k = t->key;
        it->v = t->value;
        sem_post(&it->main);
        sem_wait(&it->visitor);
        visit(t->left, it);
        visit(t->right, it);
    }
}
~~~

There's a separate function to initialize the iterator context again.

~~~c
static void *
thread_entrance(void *arg)
{
    struct tree_it *it = arg;
    sem_wait(&it->visitor);
    visit(it->t, it);
    sem_post(&it->main);
    return 0;
}
~~~

Creating the iterator only requires initializing the semaphores and
creating the thread:

~~~c
struct tree_it *
tree_iterator(struct tree *t)
{
    struct tree_it *it = malloc(sizeof(*it));
    it->t = t;
    sem_init(&it->visitor, 0, 0);
    sem_init(&it->main, 0, 0);
    pthread_create(&it->thread, 0, thread_entrance, it);
    return it;
}
~~~

The iterator function looks just like the coroutine version.

~~~c
int
tree_next(struct tree_it *it, char **k, char **v)
{
    it->k = 0;
    sem_post(&it->visitor);
    sem_wait(&it->main);
    if (it->k) {
        *k = it->k;
        *v = it->v;
        return 1;
    } else {
        pthread_join(it->thread, 0);
        sem_destroy(&it->main);
        sem_destroy(&it->visitor);
        free(it);
        return 0;
    }
}
~~~

Overall, this is almost identical to the coroutine version.

#### Coroutines using stack clashing

Finally I can tie this back into the topic at hand. Without either XSI
extensions or Pthreads, we can (usually) create coroutines by abusing
`setjmp()` and `longjmp()`. Technically this violates two of the C's
rules and relies on undefined behavior, but it generally works. This
[is not my own invention][co], and it dates back to at least 2010.

From the very beginning, C has provided a crude "exception" mechanism
that allows the stack to be abruptly unwound back to a previous state.
It's a sort of non-local goto. Call `setjmp()` to capture an opaque
`jmp_buf` object to be used in the future. This function returns 0
this first time. Hand that value to `longjmp()` later, even in a
different function, and `setjmp()` will return again, this time with a
non-zero value.

It's technically unsuitable for coroutines because the jump is a
one-way trip. The unwound stack invalidates any `jmp_buf` that was
created after the target of the jump. In practice, though, you can
still use these jumps, which is one rule being broken.

That's where stack clashing comes into play. In order for it to be a
proper coroutine, it needs to have its own stack. But how can we do
that with these primitive C utilities? **Extend the stack to overlap
the heap, call `setjmp()` to capture a coroutine on it, then return.**
Generally we can get away with using `longjmp()` to return to this
heap-allocated stack.

Here's my iterator definition for this one. Like the XSI context
struct, this has two `jmp_buf` "contexts." The `stack` holds the
iterator's stack buffer so that it can be freed, and the `gap` field
will be used to prevent the optimizer from spoiling our plans.

~~~c
struct tree_it {
    char *k;
    char *v;
    char *stack;
    volatile char *gap;
    jmp_buf coroutine;
    jmp_buf yield;
};
~~~

The coroutine looks familiar again. This time the yield is performed
with `setjmmp()` and `longjmp()`, just like `swapcontext()`. Remember
that `setjmp()` returns twice, hence the branch. The `longjmp()` never
returns.

~~~c
static void
coroutine(struct tree *t, struct tree_it *it)
{
    if (t) {
        it->k = t->key;
        it->v = t->value;
        if (!setjmp(it->coroutine))
            longjmp(it->yield, 1);
        coroutine(t->left, it);
        coroutine(t->right, it);
    }
}
~~~

Next is the tricky part to cause the stack clash. First, allocate the
new stack with `malloc()` so that we can get its address. Then use a
local variable on the stack to determine how much the stack needs to
grow in order to overlap with the allocation. Taking the difference
between these pointers is illegal as far as the language is concerned,
making this the second rule I'm breaking. I can [imagine an
implementation][adv] where the stack and heap are in two separate
kinds of memory, and it would be meaningless to take the difference. I
don't actually have to imagine very hard, because this is actually how
it used to work on the 8086 with its [segmented memory
architecture][seg].

~~~c
struct tree_it *
tree_iterator(struct tree *t)
{
    struct tree_it *it = malloc(sizeof(*it));
    it->stack = malloc(STACK_SIZE);
    char marker;
    char gap[&marker - it->stack - STACK_SIZE];
    it->gap = gap; // prevent optimization
    if (!setjmp(it->yield))
        coroutine_init(t, it);
    return it;
}
~~~

I'm using a variable-length array (VLA) named `gap` to indirectly
control the stack pointer, moving it over the heap. I'm assuming the
stack grows downward, since otherwise the sign would be wrong.

The compiler is smart and will notice I'm not actually using `gap`,
and it's happy to throw it away. In fact, it's vitally important that
I *don't* touch it since the guard page, along with a bunch of
unmapped memory, is actually somewhere in the middle of that array. I
only want the array for its side effect, but that side effect isn't
officially supported, which means the optimizer doesn't need to
consider it in its decisions. To inhibit the optimizer, I store the
array's address where someone might potentially look at it, meaning
the array has to exist.

Finally, the iterator function looks just like the others, again.

~~~c
int
tree_next(struct tree_it *it, char **k, char **v)
{
    it->k = 0;
    if (!setjmp(it->yield))
        longjmp(it->coroutine, 1);
    if (it->k) {
        *k = it->k;
        *v = it->v;
        return 1;
    } else {
        free(it->stack);
        free(it);
        return 0;
    }
}
~~~

And that's it: a nasty hack using a stack clash to create a context
for a `setjmp()`+`longjmp()` coroutine.


[qualys]: https://blog.qualys.com/securitylabs/2017/06/19/the-stack-clash
[co]: http://fanf.livejournal.com/105413.html
[rop]: /blog/2017/01/21/
[ctx]: /blog/2017/01/08/
[trie]: /blog/2016/11/13/
[raw]: /blog/2015/05/15/
[emacs]: /blog/2017/02/14/
[adv]: /blog/2017/05/03/
[seg]: https://en.wikipedia.org/wiki/X86_memory_segmentation
