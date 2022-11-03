---
title: Compressing and embedding a Wordle word list
layout: post
date: 2022-03-07T03:22:41Z
tags: [c, python, compression]
uuid: 95e1a2c2-c1b6-4472-9954-7bc76b4bab10
---

[Wordle][] is all the rage, resulting in an explosion of hobbyist clones,
with new ones appearing every day. At the current rate I estimate by the
end of 2022 that 99% of all new software releases will be Wordle clones.
That's no surprise since the rules are simple, it's more fun to implement
and study than to actually play, and the hard part is building a decent
user interface. Such implementations go back [at least 30 years][sd].
Implementers get to decide on a platform, language, and the particular
subject of this article: how to handle the word list. Is it a separate
file/database or [embedded in the program][embed]? If embedded, is it
worth compressing? In this article I'll present a simple, tailored Wordle
list compression strategy that beats general purpose compressors.

Last week one particular [QuickBASIC][qb] clone, [WorDOSle][], caught my
eye. It embeds its word list despite the dire constraints of its 16-bit
platform. The original Wordle list ([1][], [2][]) has 12,972 words which,
naively stored, would consume 77,832 bytes (5 letters, plus newline).
Sadly this exceeds a 16-bit address space. Eliminating the redundant
newline delimiter brings it down to 64,860 bytes — just small enough to
fit in an 8086 segment, but probably still difficult to manage from
QuickBASIC.

The author made a trade-off, reducing the word list to a more manageable,
if meager, 2,318 words, wisely excluding delimiters. Otherwise no further
effort made towards reducing the size. The list is sorted, and the program
cleverly tests words against the list in place using a binary search.

### Compaction baseline

Before getting into any real compression technologies, there's low hanging
fruit to investigate. Words are exactly five, case-insensitive, English
language letters: a–z. To illustrate, here are the first 100 5-letter
words from a short Wordle word list.

    abbey acute agile album alloy ample apron array attic awful
    abide adapt aging alert alone angel arbor arrow audio babes
    about added agree algae along anger areas ashes audit backs
    above admit ahead alias aloud angle arena aside autos bacon
    abuse adobe aided alien alpha angry argue asked avail badge
    acids adopt aides align altar ankle arise aspen avoid badly
    acorn adult aimed alike alter annex armed asses await baked
    acres after aired alive amber apart armor asset awake baker
    acted again aisle alley amend apple aroma atlas award balls
    actor agent alarm allow among apply arose atoms aware bands

In ASCII/UTF-8 form it's 8 bits per letter, 5 bytes per word, but I only
need 5 bits per letter, or more specifically, ~4.7 bits (`log2(26)`) per
letter. If I instead treat each word as a base-26 number, I can pack each
word into 3 bytes (`26**5` is ~23.5 bits). A 40% savings just by using a
smarter representation.

With 12,972 words, that's **38,916 bytes** for the whole list. Any
compression I apply must at least beat this size in order to be worth
using.

### Letter frequency

Not all letters occur at the same frequency. Here's the letter frequency
for the original Wordle word list:

    a:5990  e:6662  i:3759  m:1976  q: 112  u:2511  y:2074
    b:1627  f:1115  j: 291  n:2952  r:4158  v: 694  z: 434
    c:2028  g:1644  k:1505  o:4438  s:6665  w:1039
    d:2453  h:1760  l:3371  p:2019  t:3295  x: 288

When encoding a word, I can save space by spending fewer bits on frequent
letters like `e` at the cost of spending more bits on infrequent letters
like `q`. There are multiple approaches, but the simplest is [Huffman
coding][huffman]. It's not the most efficient, but it's so easy I can
almost code it in my sleep.

While my ultimate target is C, I did the frequency analysis, explored the
problem space, and implemented my compressors in Python. I don't normally
like to use Python, but it *is* good for one-shot, disposable data
science-y stuff like this. The decompressor will be implemented in C,
partially via meta-programming: Python code generating my C code. Here's
my letter histogram code:

```py
words = [line[:5] for line in sys.stdin]
hist = collections.defaultdict(int)
for c in itertools.chain(*words):
    hist[c] += 1
```

To build a Huffman coding tree, I'll need a min-heap (priority queue)
initially filled with nodes representing each letter and its frequency.
While the heap has more than one element, I pop off the two lowest
frequency nodes, create a new parent node with the sum of their
frequencies, and push it into the heap. When the heap has one element, the
remaining element is the root of the Huffman coding tree.

```py
def huffman(hist):
    heap = [(n, c) for c, n in hist.items()]
    heapq.heapify(heap)
    while len(heap) > 1:
        a, b = heapq.heappop(heap), heapq.heappop(heap)
        node = a[0]+b[0], (a[1], b[1])
        heapq.heappush(heap, node)
    return heap[0][1]

tree = huffman(hist)
```

(By the way, I love that `heapq` operates directly on a plain `list`
rather than being its own data structure.) This produces the following
Huffman coding tree (via `pprint`):

    ((('e', 's'),
      (('t', 'l'), (('g', ('v', 'w')), ('h', 'm')))),
     ((('i', ('p', 'c')),
       ('r', ('y', ('f', ('z', ('j', ('q', 'x'))))))),
      (('o', ('d', 'u')), ('a', ('n', ('k', 'b'))))))

It would be more useful to actually see the encodings.

```py
def flatten(tree, prefix=""):
    if isinstance(tree, tuple):
        return flatten(tree[0], prefix+"0") + \
               flatten(tree[1], prefix+"1")
    else:
        return [(tree, prefix)]
```

I used `isinstance` to distinguish leaves (`str`) from internal nodes
(`tuple`). With `sorted(flatten(tree))`, I get something like Morse Code:

    [('a', '1110'),       ('j', '10111110'),   ('s', '001'),
     ('b', '111111'),     ('k', '111110'),     ('t', '0100'),
     ('c', '10011'),      ('l', '0101'),       ('u', '11011'),
     ('d', '11010'),      ('m', '01111'),      ('v', '011010'),
     ('e', '000'),        ('n', '11110'),      ('w', '011011'),
     ('f', '101110'),     ('o', '1100'),       ('x', '101111111'),
     ('g', '01100'),      ('p', '10010'),      ('y', '10110'),
     ('h', '01110'),      ('q', '101111110'),  ('z', '1011110')]
     ('i', '1000'),       ('r', '1010'),

In terms of encoded bit length, what is the shortest and longest?

```py
codes = dict(flatten(tree))
lengths = [(sum(len(codes[c]) for c in w), w) for w in words]
```

`min(lengths)` is "esses" at 15 bits, and `max(lengths)` is "qajaq" at 34
bits. In other words, the worst case is worse than the compact, 24-bit
representation! However, the total is better: `sum(w[0] for w in lengths)`
reports 281,956 bits, or 35,245 bytes. Packed appropriately, that shaves
off ~3.5kB, though it comes at the cost of losing random access, and
therefore binary search.

Speaking of bit packing, I'm ready to compress the entire word list into a
bit stream:

```py
bits = "".join("".join(codes[c] for c in w) for w in words)
```

Where `bits` begins with:

    11101110011100001101011101110010110001000111011101...

On the C side I'll pack these into 32-bit integers, least significant bit
first. I abused `textwrap` to dice it up, and I also need to reverse each
set of bits before converting to an integer.

```py
u32 = [int(b[::-1], 2) for b in textwrap.wrap(bits, width=32)]
```

I now have my compressed data as a sequence of 32-bit integers. Next, some
meta-programming:

```py
print(f"static const uint32_t words[{len(u32)}] =", "{", end="")
for i, u in enumerate(u32):
    if i%6 == 0:
        print("\n    ", end="")
    print(f"0x{u:08x},", end="")
print("\n};")
```

That produces a C table, the beginnings of my decompressor. The array
length isn't necessary since the C compiler can figure it out, but being
explicit allows human readers to know the size at a glance, too. Observe
how the final 32-bit integer isn't entirely filled.

```c
static const uint32_t words[8812] = {
    0x4eeb0e77,0xb8caee23,0xffb892bb,0x397fddf2,0xddfcbfee,0x5ff7997f,
    // ...
    0x7b4e66bd,0x35ebcccd,0x8f9af60f,0x0000000c,
};
```

Now, how to go about building the rest of the decompressor? I have a
Huffman coding tree, which is *an awful lot* [like a state machine][sm],
eh? I can even have Python generate a state transition table from the
Huffman tree:

```py
def transitions(tree, states, state):
    if isinstance(tree, tuple):
        child = len(states)
        states[state] = -child
        states.extend((None, None))
        transitions(tree[0], states, child+0)
        transitions(tree[1], states, child+1)
    else:
        states[state] = ord(tree)
    return states

states = transitions(tree, [None], 0)
```

The central idea: positive entries are leaves, and negative entries are
internal nodes. The negated value is the index of the left child, with the
right child immediately following. In `transitions`, the caller reserves
space in the state table for callees, hence starting with `[None]`. I'll
show the actual table in C form after some more meta-programming:

```py
print(f"static const int8_t states[{len(states)}] =", "{", end="")
for i, s in enumerate(states):
    if i%12 == 0:
        print("\n    ", end="")
    print(f"{s:4},", end="")
print("\n};")
```

I chose `int8_t` since I know these values will all fit in an octet, and
it must be signed because of the negatives. The result:

```c
static const int8_t states[51] = {
      -1,  -3, -19,  -5,  -7, 101, 115,  -9, -11, 116, 108, -13,
     -17, 103, -15, 118, 119, 104, 109, -21, -39, -23, -27, 105,
     -25, 112,  99, 114, -29, 121, -31, 102, -33, 122, -35, 106,
     -37, 113, 120, -41, -45, 111, -43, 100, 117,  97, -47, 110,
     -49, 107,  98,
};
```

The first node is -1, meaning if you read a 0 bit then transition to state
1, else state 2 (e.g. immediately following 1). The decompressor reads one
bit at a time, walking the state table until it hits a positive value,
which is an ASCII code. I've decided on this function prototype:

```c
int32_t next(char word[5], int32_t n);
```

The `n` is the bit index, which starts at zero. The function decodes the
word at the given index, then returns the bit index for the next word.
Callers can iterate the entire word list without decompressing the whole
list at once. Finally the decompressor code:

```c
int32_t next(char word[5], int32_t n)
{
    for (int i = 0; i < 5; i++) {
        int state = 0;
        for (; states[state] < 0; n++) {
            int b = words[n>>5]>>(n&31) & 1;  // next bit
            state = b - states[state];
        }
        word[i] = states[state];
    }
    return n;
}
```

When compiled, this is about 80 bytes of instructions, both x86-64 and
ARM64. This, along with the 51 bytes for the state table, should be
counted against the compression size. That's 35,579 bytes total.

Trying it out, this program indeed reproduces the original word list:

```c
int main(void)
{
    int32_t state = 0;
    char word[] = ".....\n";
    for (int i = 0; i < 12972; i++) {
        state = next(word, state);
        fwrite(word, 6, 1, stdout);
    }
}
```

Searching 12,972 words linearly isn't too bad, even for an old 16-bit
machine. However, if you really need to speed it up, you could build a
little run time index to track various bit positions in the list. For
example, the first word starting with `b` is at bit offset 15,743. If the
word I'm looking up begins with `b` then I can start there and stop at the
first `c`, decompressing just 909 words.

### Taking it to the next level: run-length encoding

Here's the 100-word word list sample again. The sorting is deliberate:

    abbey acute agile album alloy ample apron array attic awful
    abide adapt aging alert alone angel arbor arrow audio babes
    about added agree algae along anger areas ashes audit backs
    above admit ahead alias aloud angle arena aside autos bacon
    abuse adobe aided alien alpha angry argue asked avail badge
    acids adopt aides align altar ankle arise aspen avoid badly
    acorn adult aimed alike alter annex armed asses await baked
    acres after aired alive amber apart armor asset awake baker
    acted again aisle alley amend apple aroma atlas award balls
    actor agent alarm allow among apply arose atoms aware bands

If I look at words column-wise, I see a long run of `a`, then a long run
of `b`, etc. Even the second column has long runs. I should really exploit
this somehow. The first scheme would have worked equally as well on a
shuffled list as a sorted list, which is an indication that it's storing
unnecessary information, namely the word list order. (Rule of thumb:
Compression should work better on sorted inputs.)

For this second scheme, I'll pivot the whole list so that I can encode it
in column-order. (This is roughly how one part of bzip2 works, by the
way.) I'll use run-length encoding (RLE) to communicate "91 'a', 135 'b',
etc.", then I'll encode these RLE tokens using Huffman coding, per the
first scheme, since there will be lots of repeated tokens.

First, pivot the word list:

```py
pivot = "".join("".join(w[i] for w in words) for i in range(5))
```

Next compute the RLE token stream. The stream works in pairs, first
indicating a letter (1–26), then the run length.

```py
tokens = []
offset = 0
while offset < len(pivot):
    c = pivot[offset]
    start = offset
    while offset < len(pivot) and pivot[offset] == c:
        offset += 1
    tokens.append(ord(c) - ord('a') + 1)
    tokens.append(offset - start)
```

I've biased the letter representation by 1 — i.e. 1–26 instead of 0–25 —
since I'm going to encode all the tokens using the same Huffman tree.
(Exercise for the reader: Does compression improve with two distinct
Huffman trees, one for letters and the other for runs?) There are no
zero-length runs, and I want there to be as few unique tokens as possible.

`tokens` looks like so (e.g. 737 'a', 909 'b', …):

    [1, 737, 2, 909, 3, 922, 4, 685, 5, 303, 6, 598, ...]

The original Wordle list results in 139 unique tokens. A few tokens appear
many times, but most of appear only once. Reusing my Huffman coding tree
builder from before:

```py
tree = huffman(collections.Counter(tokens))
```

This makes for a more complex and interesting tree:

    (1,
     ((((18, 20), (25, (((10, 24), (26, 22)), 8))),
       (5,
        ((11,
          ((23,
            ((17,
              (((35, (46, 76)), ((82, 93), (104, 111))),
               (((165, 168), 27), (28, (((30, 39), 31), 38))))),
             ((((((40, 41), ((44, 48), 45)),
                 ((53, (54, 56)), 55)),
                ((((57, 59), 58), ((60, 61), (62, 63))),
                 ((64, (65, 66)), ((67, 70), 68)))),
               (((((71, 75), 74), (77, (78, 79))),
                 (((80, 85), 87), 81)),
                ((((90, 91), (92, 97)), (96, (99, 100))),
                 (((101, 103), 102),
                  ((105, 106), (109, 110)))))),
              ((((((113, 114), 117), ((120, 121), (125, 129))),
                 (((130, 133), (137, 139)), (138, (140, 142)))),
                ((((144, 145), (147, 153)), (148, (166, 175))),
                 (((181, 183), (187, 189)),
                  ((193, 202), (220, 242))))),
               (((((262, 303), (325, 376)),
                  ((413, 489), (577, 598))),
                 (((628, 638), (685, 693)),
                  ((737, 815), (859, 909)))),
                ((((922, 1565), 29), 32), (34, (33, 43)))))))),
           6)),
         3))),
      ((19, 2),
       ((4, (15, (21, 16))), ((14, 9), (12, (13, 7)))))))

Peeking at the first 21 elements of `sorted(flatten(tree))`, which chops
off the long tail of large-valued, single-occurrence tokens:

    [(1, '0'),            (8, '100111'),       (15, '111010'),
     (2, '1101'),         (9, '111101'),       (16, '1110111'),
     (3, '10111'),        (10, '10011000'),    (17, '1011010100'),
     (4, '11100'),        (11, '101100'),      (18, '10000'),
     (5, '1010'),         (12, '111110'),      (19, '1100'),
     (6, '1011011'),      (13, '1111110'),     (20, '10001'),
     (7, '1111111'),      (14, '111100'),      (21, '1110110')]

Huffman-encoding the RLE stream is more straightforward:

```py
codes = dict(flatten(tree))
bits = "".join(codes[token] for token in tokens)
```

This time `len(bits)` is 164,958, or 20,620 bytes! A huge difference,
around 40% additional savings!

Slicing and dicing 32-bit integers and printing the table works the same
as before. However, this time the state table has larger values (e.g. that
run of 909), and so the state table will be `int16_t`. I copy-pasted the
original meta-programming code and make the appropriate adjustments:

```c
static const int16_t states[277] = {
      -1,   1,  -3,  -5,-257,  -7, -21,  -9, -11,  18,  20,  25,
     -13, -15,   8, -17, -19,  10,  24,  26,  22,   5, -23, -25,
       3,  11, -27, -29,   6,  23, -31, -33, -63,  17, -35, -37,
     -49, -39, -43,  35, -41,  46,  76, -45, -47,  82,  93, 104,
     111, -51, -55, -53,  27, 165, 168,  28, -57, -59,  38, -61,
      31,  30,  39, -65,-155, -67,-109, -69, -85, -71, -79, -73,
     -75,  40,  41, -77,  45,  44,  48, -81,  55,  53, -83,  54,
      56, -87, -99, -89, -93, -91,  58,  57,  59, -95, -97,  60,
      61,  62,  63,-101,-105,  64,-103,  65,  66,-107,  68,  67,
      70,-111,-129,-113,-123,-115,-119,-117,  74,  71,  75,  77,
    -121,  78,  79,-125,  81,-127,  87,  80,  85,-131,-143,-133,
    -139,-135,-137,  90,  91,  92,  97,  96,-141,  99, 100,-145,
    -149,-147, 102, 101, 103,-151,-153, 105, 106, 109, 110,-157,
    -213,-159,-185,-161,-173,-163,-167,-165, 117, 113, 114,-169,
    -171, 120, 121, 125, 129,-175,-181,-177,-179, 130, 133, 137,
     139, 138,-183, 140, 142,-187,-199,-189,-195,-191,-193, 144,
     145, 147, 153, 148,-197, 166, 175,-201,-207,-203,-205, 181,
     183, 187, 189,-209,-211, 193, 202, 220, 242,-215,-245,-217,
    -231,-219,-225,-221,-223, 262, 303, 325, 376,-227,-229, 413,
     489, 577, 598,-233,-239,-235,-237, 628, 638, 685, 693,-241,
    -243, 737, 815, 859, 909,-247,-253,-249,  32,-251,  29, 922,
    1565,  34,-255,  33,  43,-259,-261,  19,   2,-263,-269,   4,
    -265,  15,-267,  21,  16,-271,-273,  14,   9,  12,-275,  13,
       7,
};
```

(Since 277 is prime it will never wrap to a nice rectangle no matter what
width I plug in. Ugh.)

With column-wise compression it's not possible to iterate a word at a
time. The entire list must be decompressed at once. The interface now
looks like so, where the caller supplies a `12972*5`-byte buffer to be
filled:

```c
void decompress(char *);
```

Exercise for the reader: Modify this to decompress into the 24-bit compact
form, so the caller only needs a `12972*3`-byte buffer.

Here's my decoder, much like before:

```c
void decompress(char *buf)
{
    for (int32_t x = 0, y = 0, i = 0; i < 164958;) {
        // Decode letter
        int state = 0;
        for (; states[state] < 0; i++) {
            int b = words[i>>5]>>(i&31) & 1;
            state = b - states[state];
        }
        int c = states[state] + 96;

        // Decode run-length
        state = 0;
        for (; states[state] < 0; i++) {
            int b = words[i>>5]>>(i&31) & 1;
            state = b - states[state];
        }
        int len = states[state];

        // Fill columns
        for (int n = 0; n < len; n++, y++) {
            buf[y*5+x] = c;
        }
        if (y == 12972) {
            y = 0;
            x++;
        }
    }
}
```

And my new test exactly reproduces the original list:

```c
int main(void)
{
    char buf[12972*5L];
    decompress(buf);

    char word[] = ".....\n";
    for (int i = 0; i < 12972; i++) {
        memcpy(word, buf+i*5, 5);
        fwrite(word, 6, 1, stdout);
    }
}
```

Totalling it up:

* Compressed data is 20,620 bytes
* State table is 554 bytes
* Decompressor is about 200 bytes

That's a total of 21,374 bytes. Surprisingly this beats general purpose
compressors!

    PROGRAM     VERSION   SIZE
    bzip2 -9    1.0.8     33,752
    gzip -9     1.10      30,338
    zstd -19    1.4.8     27,098
    brotli -9   1.0.9     26,031
    xz -9e      5.2.5     16,656
    lzip -9     1.22      16,608

Only `xz` and `lzip` come out ahead on the raw compressed data, but lose
if accounting for an embedded decompressor (on the order of 10kB). Clearly
there's an advantage to customizing compression to a particular dataset.

*Update*: [Johannes Rudolph has pointed out][jr] a compression scheme for
a Game Boy Wordle clone last month that gets it [down to 17,871 bytes,
*and* supports iteration][gb]. I improved on this scheme to [further
reduce it to 16,659 bytes][misc].


[1]: https://gist.github.com/cfreshman/cdcdf777450c5b5301e439061d29694c
[2]: https://gist.github.com/cfreshman/a03ef2cba789d8cf00c08f767e0fad7b
[WorDOSle]: http://grahamdowney.com/software/WorDOSle/WorDOSle.htm
[Wordle]: https://en.wikipedia.org/wiki/Wordle
[embed]: /blog/2016/11/15/
[gb]: http://alexanderpruss.blogspot.com/2022/02/game-boy-wordle-how-to-compress-12972.html
[huffman]: https://en.wikipedia.org/wiki/Huffman_coding
[jr]: https://lists.sr.ht/~skeeto/public-inbox/%3CCAKF7Hnc4nVKS%3D2adUjyiRb5yBZUdw5z0K_Fb9kFbaW5S6i7POw%40mail.gmail.com%3E
[misc]: https://github.com/skeeto/scratch/blob/master/misc/wordle.c
[qb]: /blog/2020/11/17/
[sd]: https://www.youtube.com/watch?v=Yi2mTMWC4BM&t=1270s
[sm]: /blog/2020/12/31/
