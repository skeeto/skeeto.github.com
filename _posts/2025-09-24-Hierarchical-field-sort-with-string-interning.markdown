---
title: Hierarchical field sort with string interning
layout: post
date: 2025-09-24T17:11:32Z
tags: [c]
uuid: 30d4b889-d14b-4b32-b389-858fb3dde34b
---

In a recent, real world problem I needed to load a heterogeneous sequence
of records from a buffer. Record layout is defined in a header before the
sequence. Each field is numeric, with a unique name composed of non-empty
alphanumeric period-delimited segments, where segments signify nested
structure. Field names are a comma-delimited list, in order of the record
layout. The catch motivating this article is that nested structures are
not necessarily contiguous. In my transformed representation I needed
nested structures to be contiguous. For illustrative purposes here, it
will be for JSON output. I came up with what I think is an interesting
solution, which I've implemented in C using [techniques previously
discussed][prev].

The above description is probably confusing on its own, and an example is
worth a thousand words, so here's a listing naming 7 fields:

    timestamp,point.x,point.y,foo.bar.z,point.z,foo.bar.y,foo.bar.x

Where `point` is a substructure, as is `foo` and `bar`, but note they're
interleaved in the record. So if a record contains these values:

    {1758158348, 1.23, 4.56, -100, 7.89, -200, -300}

The JSON representation would look like:

```json
{
  "timestamp": 1758158348,
  "point": {
    "x": 1.23,
    "y": 4.56,
    "z": 7.89
  },
  "foo": {
    "bar": {
      "z": -100,
      "y": -200,
      "x": -300
    }
  }
}
```

Notice `point.z` moved up and `foo.bar.z` down, so that substructures are
contiguous in this representation as required for JSON. Sorting the field
names lexicographically would group them together as a simple solution.
However, as an additional constraint I want to retain the original field
order as much as possible. For example, `timestamp` is first in both the
original and JSON representations, but sorting would put it last. If all
substructures are already contiguous, nothing should change.

### Solution with string interning

My solution is to intern the segment strings, assigning each a unique,
monotonic integral token in the order they're observed. In my program,
zero is reserved as a special "root" token, and so the first string has
the value 1. The concrete values aren't important, only that they're
assigned monotonically.

The trick is that a string is always interned in the "namespace" of a
previous token. That is, we're building a `(token, string) -> token` map.
For our segments that namespace is the token for the parent structure, and
the top-level fields are interned in the reserved "root" namespace. When
applied to the example, we get the token sequences:

    timestamp  -> 1
    point.x    -> 2 3
    point.y    -> 2 4
    foo.bar.z  -> 5 6 7
    point.z    -> 2 8
    foo.bar.y  -> 5 6 9
    foo.bar.x  -> 5 6 10

And our map looks like:

    {0, "timestamp"} -> 1
    {0, "point"}     -> 2
    {2, "x"}         -> 3
    {2, "y"}         -> 4
    {0, "foo"}       -> 5
    {5, "bar"}       -> 6
    {6, "z"}         -> 7
    {2, "z"}         -> 8
    {6, "y"}         -> 9
    {6, "x"}         -> 10

Notice how `"x"` is assigned 3 and 10 due to different namespaces. That's
important because otherwise the fields of `foo.bar` would sort in the same
order as `point`. Namespace gives these fields unique identities.

Once we have the token representation, sort lexicographically *by token*.
That pulls `point.z` up to its siblings.

    timestamp  -> 1
    point.x    -> 2 3
    point.y    -> 2 4
    point.z    -> 2 8
    foo.bar.z  -> 5 6 7
    foo.bar.y  -> 5 6 9
    foo.bar.x  -> 5 6 10

Now we have the "output" order with minimal re-ordering. If substructures
were already contiguous, nothing changes. Assuming a reasonable map, this
is `O(n log n)`, primarily due to sorting.

#### Alternatives

Before I thought of namespaces, my initial idea was to intern the whole
prefix of a segment. The sequence of look-ups would be:

    "timestamp"    -> 1  -> {1}
    "point"        -> 2
    "point.x"      -> 3  -> {2, 3}
    "point"        -> 2
    "point.y"      -> 4  -> {2, 4}
    "foo"          -> 5
    "foo.bar"      -> 6
    "foo.bar.z"    -> 7  -> {5, 6, 7}
    "point"        -> 2
    "point.z"      -> 8  -> {2, 8}
    "foo"          -> 5
    "foo.bar"      -> 6
    "foo.bar.y"    -> 9  -> {5, 6, 9}
    "foo"          -> 5
    "foo.bar"      -> 6
    "foo.bar.x"    -> 10 -> {5, 6, 10}

Ultimately it produces the same tokens, and this is a more straightforward
`string -> string` map. The prefixes are acting as namespaces. However, I
wrote it this way as a kind of visual proof: Notice the right triangle
shape formed by the strings for each field. From the area we can see that
processing prefixes as strings is `O(n^2)` quadratic time on the number of
segments! In my real problem the inputs were never large enough for this
to matter, but I hate [leaving behind avoidable quadratic algorithms][q].
Using a token as a namespace flattens the prefix to a constant size.

Another option is a different map for each namespace. So for `foo.bar.z`
lookup the `"foo"` map `(string -> map)` in the root `(string -> map)`,
then within that lookup the `"bar"` table `(string -> token)` (since this
is the penultimate segment), then intern `"z"` within that to get its
token. That wouldn't have quadratic time complexity, but it seems quite a
bit more complicated than a single, flat `(token, string) -> token` map.

### Implementation in C

Because [the standard library has little useful for us][libc], I am
building on [**previously-established definitions**][prev], so refer to
that article for basic definitions like `Str`. To start off, tokens will
be a size-typed integer so we never need to worry about overflowing the
token counter. We'd run out of memory first:

```c
typedef ptrdiff Token;
```

We're building a `(token, string) -> token)` map, so we'll need a hash
function for such keys:

```c
uint64_t hash(Token t, Str s)
{
    uint64_t r = (uint64_t)t << 8;
    for (ptrdiff i = 0; i < s.len; i++) {
        r ^= s.data[i];
        r *= 1111111111111111111u;
    }
    return r;
}
```

The map itself is a forever-useful [hash trie][ht].

```c
typedef struct Map Map;
struct Map {
    Map  *child[4];
    Token namespace;
    Str   segment;
    Token token;
};

Token *upsert(Map **m, Token namespace, Str segment, Arena *a)
{
    for (uint64_t h = hash(ns, segment); *m; h <<= 2) {
        if (namespace==(*m)->namespace && equals(segment, (*m)->segment)) {
            return &(*m)->token;
        }
        m = &(*m)->child[h>>62];
    }
    *m = new(a, 1, Map);
    (*m)->namespace = namespace;
    (*m)->segment = segment;
    return &(*m)->token;  // caller will assign
}
```

We'll use this map to convert a string naming a field into a sequence of
tokens, so we'll [need a slice][slice]. Fields also have an offset within
the record and a type, which we'll track via its original ordering, which
I'll do with an `index` field (e.g. into the original header). Also track
the original name.

```c
typedef struct {
    Str          name;
    ptrdiff_t    index;
    Slice(Token) tokens;
} Field;
```

To sort fields we'll need a comparator:

```c
ptrdiff_t field_compare(Field a, Field b)
{
    ptrdiff_t len = min(a.tokens.len, b.tokens.len);
    for (ptrdiff_t i = 0; i < len; i++) {
        Token d = a.tokens.data[i] - b.tokens.data[i];
        if (d) {
            return d;
        }
    }
    return a.tokens.len - b.tokens.len;
}
```

Because field names are unique, each token sequence is unique, and so we
need not use `index` in the comparator.

Finally down to business: [cut up the list][cut] and build the token
sequences with the established `push` macro. The sort function isn't
interesting, and could be as simple as libc `qsort` with the above
comparator (and adapter), so I'm only listing the prototype.

```c
void field_sort(Slice(Field), Arena scratch);

Slice(Field) parse_fields(Str fieldlist, Arena *a)
{
    Slice(Field) fields  = {};
    Map         *strtab  = 0;
    ptrdiff_t    ntokens = 0;

    for (Cut c = {.tail=fieldlist, .ok=true}; c.ok;) {
        c = cut(c.tail, ',');
        Field field = {};
        field.name  = c.head;
        field.index = fields.len;

        Token prev = 0;
        for (Cut f = {.tail=field.name, .ok=true}; f.ok;) {
            f = cut(f.tail, '.');
            Token *token = upsert(&strtab, prev, f.head, a);
            if (!*token) {
                *token = ++ntokens;
            }
            *push(a, &field.tokens) = *token;
            prev = *token;
        }

        *push(a, &fields) = field;
    }

    field_sort(fields, *a);
    return fields;
}
```

Usage here suggests `Cut::ok` should be inverted to `Cut::done` so that it
better zero-initializes. Something I'll need to consider. Because it's all
allocated from an arena, no need for destructors or anything like that, so
this is the complete implementation. Back to the example:

```c
    Str fieldlist = S(
        "timestamp,"
        "point.x,"
        "point.y,"
        "foo.bar.z,"
        "point.z,"
        "foo.bar.y,"
        "foo.bar.x"
    );
    Slice(Field) fields = parse_fields(fieldlist, &scratch);
    for (ptrdiff_t i = 0; i < fields.len; i++) {
        Str name = fields.data[i].name;
        fwrite(name.data, 1, name.len, stdout);
        putchar('\n');
    }
```

This program will print the proper output field order. In a real program
we'd hold onto the string table, define an inverse lookup to translate
tokens back into strings, and use it when in producing output. I do just
that in my exploratory program, [**`rec2json.c`**][src], written a little
differently than presented above. It uses the sorted tokens to compile a
simple bytecode program that, when run against a record, produces its JSON
representation. It compiles the example to:

    OPEN          # print '{'
    KEY     1     # print token 1 as a key, i.e. "timestamp:"
    READ    0     # print double at record offset 0
    COMMA         # print ','
    KEY     2     # print token 2 as a key, i.e. "point:"
    OPEN
    KEY     3
    READ    8     # print double at record offset 8
    COMMA
    KEY     4
    READ    16
    COMMA
    KEY     8
    READ    32
    CLOSE         # print '}'
    COMMA
    KEY     5
    OPEN
    KEY     6
    OPEN
    KEY     7
    READ    24
    COMMA
    KEY     9
    READ    40
    COMMA
    KEY     10
    READ    48
    CLOSE
    CLOSE
    CLOSE

Seeing it written out, I notice more room for improvement. An optimization
pass could coalesce instructions so that, for instance, `OPEN` then `KEY`
[concatenate][] to a single string at compile time so that it only needs
one instruction. This program could be 15 instructions instead of 31. In
my real case I didn't need anything quite this sophisticated, but it was
fun to explore.


[concatenate]: /blog/2024/05/25/
[cut]: /blog/2025/03/02/
[ht]: /blog/2023/09/30/
[libc]: /blog/2023/02/11/
[prev]: /blog/2025/01/19/
[q]: https://randomascii.wordpress.com/category/quadratic/
[slice]: /blog/2025/06/26/
[src]: https://github.com/skeeto/scratch/blob/master/rec2json/rec2json.c
