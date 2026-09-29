# encoding: utf-8
# Generate test/vectors.inc: Markdown inputs and kramdown's HTML output,
# using the options Jekyll gives kramdown for this site.
#
#   $ source ~/.ruby && ruby test/vectors.rb >test/vectors.inc
#
# Code blocks are rewritten to the generator's lean markup. Ruby is only
# needed to regenerate the vectors; the output is committed.
require 'kramdown'
require 'kramdown-parser-gfm'

OPTIONS = {
  input: 'GFM',
  hard_wrap: false,
  auto_ids: true,
  entity_output: :as_char,
  smart_quotes: 'lsquo,rsquo,ldquo,rdquo',
  gfm_quirks: [:paragraph_end],
  syntax_highlighter: nil,
}

def render(md)
  html = Kramdown::Document.new(md, OPTIONS).to_html
  return html unless html.valid_encoding?
  html.gsub(%r{<pre><code(?: class="[^"]*")?>}, '<pre class="highlight"><code>')
end

def cstr(s)
  out = +'"'
  s.b.each_byte do |c|
    case c
    when 0x5c then out << '\\\\'
    when 0x22 then out << '\\"'
    when 0x0a then out << '\\n'
    when 0x09 then out << '\\t'
    when 0x20..0x7e then out << c.chr
    else out << format('\\%03o', c)
    end
  end
  out << '"'
end

# Hand-written vectors, by topic
vectors = []

# Smart quotes: every rule, and the quirks the blog depends on
vectors += [
  "`x`'s", "`x`'ll", "'80s were fun", "He said, \"'Quoted' words.\"",
  "x \"*foo*\" y", "café's", "'...'", "(\"Yo dawg ...\")", "—\"a—\"",
  "x&'y", "a\n'b'", "`x`'' y", "don't", "'em", "rock 'n' roll",
  "\"portable executable 'ports.\"", "\"meet.\"", "...\\\"", "'tis",
  "the '90s", "the 1990's", "\"'hi'\"", "'\"hi\"'", "a \"'b'\" c",
  "a '\"b\"' c", "say \"hi\"", "say 'hi'", "\"hi,\" she said",
  "'hi,' she said", "5' 11\"", "\"", "'", "''", "\"\"", "'''", "\"\"\"",
  "a\"", "a'", "\"a", "'a", "a'b", "a\"b", "a '", "a \"", "' a", "\" a",
  "(\"a\")", "('a')", "[\"a\"]", "{'a'}", "-'a'", "\\'a'", "\\\"a\"",
  "'*a*'", "\"**a**\"", "\"_a_\"", "'_ a'", "'* a'", "'.", "\"!", "'?x",
  "'..", "'...", "\"'.", "x'.", "x'a", "x's", "x'st", "x's.", "x'sé",
  "é'a", "1'2", "1\"2", "a\t'b'", "a\n\"b\"", "'1", "'12", "'12s",
  "'12st", " '12s", "a '12s", "\"'12s", "'\"12", "\"'a", "'\"a",
]

# Quotes next to every span element
spans = ["`c`", "*e*", "**s**", "_u_", "[l](/u)", "![i](/u)", "<kbd>k</kbd>",
         "<code>c</code>", "<span>s</span>", "<del>d</del>", "<s>s</s>",
         "&amp;", "&#8212;", "&quot;", "\\*", "...", "--", "~~d~~",
         "<http://x.y>", "<!-- c -->", "<br/>", "x  \ny", "[r]", "[r][]"]
spans.each do |sp|
  ["'", '"'].each do |q|
    vectors << "#{sp}#{q}s" << "#{q}#{sp}#{q}" << "a #{q}#{sp}#{q} b"
    vectors << "#{sp}#{q} a" << "#{sp}#{q}ll" << "#{q}#{sp}"
  end
end

# Emphasis
vectors += [
  "*a*", "**a**", "***a***", "*a **b** c*", "**a *b* c**", "*a*b", "a*b*c",
  "*i*th", "*in*infrequently", "*BSD 4 * 5 *a*b", "***Update**:",
  "**foo*", "*foo**", "* a *", "*a *", "* a*", "**a", "a**", "*a\nb*",
  "_a_", "__a__", "a_b_c", "GL_RGBA", "pthread_join()", "_a_b", "a_b_",
  "__a", "_ a_", "*a _b_ c*", "_a *b* c_", "**a\n**", "*a**b*", "*a***",
  "***a*", "***a**", "****", "** **", "*", "**", "a * b * c", "4 *\"x",
  "*\"x\"*", "*'a'*", "**\"a\"**", "*a*'s", "**a**'s", "*`c`*", "*[l](/u)*",
  "[*l*](/u)", "*a [b*](/u)", "*<kbd>*</kbd>*", "*a ~~b *c* d~~ e*",
  "a-_b_", "a_-b_", "é_a_", "1_a_", "_é_", "*é*", "*a\\*b*",
]

# Code spans
vectors += [
  "`a`", "``a``", "`` a ``", "``  a  ``", "`` ` ``", "`` C-x ` ``",
  "a ` b", "` a", "`a", "a`", "`a``b`", "``a`b``", "`a\nb`", "`<&>`",
  "`'\"`", "x `a` y", "`montage.jpg `", "``` a ```", "`` a` ``",
  "\\`a`", "`\\`", "`*a*`", "*`a`*", "`[a](b)`", "a `` b",
]

# Links, references, and images
vectors += [
  "[a](/b)", "[a](</b c>)", "[a](/b \"t\")", "[a](/b 't')",
  "[a](http://x.y/(z))", "[a](http://x.y/(z)", "[a](/b%29)",
  "[a](/b?c=1&d=2)", "[a](/b?c=1&amp;d=2)", "[a]( /b )", "[a](/b\n)",
  "[a][b]\n\n[b]: /u", "[a][]\n\n[a]: /u", "[a]\n\n[a]: /u",
  "[A]\n\n[a]: /u", "[a b][]\n\n[A  B]: /u", "[a\nb][]\n\n[a b]: /u",
  "[a] [b]\n\n[b]: /u", "[a]\n[b]\n\n[b]: /u", "[a][x]\n\n[a]: /u",
  "[a][]", "[a]", "[a][b]", "[a]: /u\n[a]: /v\n\n[a]", "[a]: <>\n\n[a]",
  "[a]: <b c>\n\n[a]", "[a]: /u \"T\"\n\n[a]", "[a]: /u 'T'\n\n[a]",
  "[a]: /u\n\"T\"\n\n[a]", "[a]: /u \"T\" x\n\n[a]", "[a]: (/u)\n\n[a]",
  "para\n[a]: /u\n\n[a]", "  [a]: /u\n\n[a]", "[a]:/u\n\n[a]",
  "![a](/i.png)", "![](/i.png)", "![a b](/i.png \"t\")", "![a *b*](/i.png)",
  "![a\\*b](/i.png)", "[![](/t.png)](/f.png)", "[a ![b](/i) c](/u)",
  "[a ![b] c](/u)", "![a][b]\n\n[b]: /i", "![a]\n\n[a]: /i",
  "[a [b] c](/u)", "[a [b](/c) d](/e)", "[a](/b) [c](/d)", "[a]()",
  "[a](<b>)", "[a](<b> \"t\")", "[a](b \"t)", "[a](b 't' x)",
  "[a\\]](/u)", "[a\\[](/u)", "\\[a](/u)", "[a](/u)'s", "[a](/u)\"",
  "[^a]", "a]b", "a[b", "![", "[", "[a", "[a]b", "[a]]", "[[a]]",
  "[*a*][b]\n\n[b]: /u", "[`a`][]\n\n[`a`]: /u",
  "<http://a.b>", "<https://a.b/c?d=e&f=g>", "<mailto:a@b.c>", "<a@b.c>",
  "<http:>", "<http:>a>", "<ftp://a>", "<http://a b>", "<a.b@c.d>",
  "<a@b>", "<@b>", "<a@>", "<http://a\nb>",
]

# Escapes
vectors += [
  "\\\\", "\\.", "\\*a\\*", "\\_", "\\+", "\\`", "\\<", "\\>", "\\(",
  "\\)", "\\[", "\\]", "\\{", "\\}", "\\#", "\\!", "\\:", "\\|", "\\\"",
  "\\'", "\\$", "\\=", "\\-", "\\~", "\\^", "\\a", "2\\^128", "a\\",
  "a\\\nb", "a\\\\\nb", "*a\\\nb*", "~~a\\\nb~~", "<span>a\\\nb</span>",
  "a \\<< b", "a \\>> b",
]

# Typographic symbols, entities, line breaks
vectors += [
  "a...b", "a....b", "a.. b", "a---b", "a--b", "a -- b", "<<a>>", "a << b",
  "a >> b", "<< a >>", "&amp;", "&lt;", "&gt;", "&quot;", "&#39;",
  "&#123;", "&#x7b;", "&#x7B;", "&#60;", "&#38;", "&#34;", "&amp",
  "a & b", "a &b; c", "AT&T", "&#0;", "&#1114112;", "a  \nb", "a   \nb",
  "a \nb", "a  \n  b", "a  ", "a\\\\", "*a*  \nb", "a  \n",
]

# Strikethrough
vectors += [
  "~~a~~", "~~a b~~", "~~ a~~", "~~a ~~", "~~a~~b~~", "~a~", "~~~a~~~",
  "~~a\nb~~", "~~'a'~~", "'~~a~~'", "~~*a*~~", "*~~a~~*", "~~a ~~b~~",
  "~~[a](/b)~~", "[~~a~~](/b)", "~~`a`~~", "a~~b~~c",
]

# Inline HTML
vectors += [
  "<kbd>it's</kbd>", "<code>it's</code>", "<del>it's</del>", "<s>it's</s>",
  "<span>it's</span>", "<b>it's</b>", "<i>*a*</i>", "<kbd>*a*</kbd>",
  "<var>'a'</var>", "<samp>a</samp>", "<u>'a'</u>", "<mark>'a'</mark>",
  "a </b> c", "a <p>b</p> c", "a <div>b</div> c", "<span>a", "a <span",
  "<span\nstyle=\"a\">b</span>", "<span title=\"a\nb\">c</span>",
  "<img src=\"a.png\">", "<img src=\"a.png\"/>", "<img src=a.png>",
  "<br>", "<br/>", "<br />", "<span/>", "<b></b>", "<B>a</b>",
  "<Foo>a</Foo>", "<foo>a</foo>", "<a href=\"x\" href=\"y\">z</a>",
  "<a href=\"x\" id=\"\">z</a>", "<a id=\"\">z</a>", "<abbr title='a\"b'>c</abbr>",
  "<span>a <kbd>'b'</kbd> c</span>", "<kbd><b>'a'</b></kbd>",
  "<sup>1</sup>", "a<!-- b -->c", "a <!-- b\nc --> d", "<![CDATA[a<b]]>",
  "<span>a</SPAN>", "<x:y>a</x:y>", "<a href=\"a&b\">c</a>",
  "<a href=\"a&amp;b\">c</a>", "<a href=\"<\">c</a>", "a <3 b", "a < b",
  "<a>[b](/c)</a>", "[<a>b</a>](/c)", "<span>`a`</span>", "`<span>`",
  "<del>a\nb</del>", "<em>'a'</em>", "<strong>a</strong>'s",
]

# Headers and ids
vectors += [
  "# a", "## a", "### What's wrong with swank-js?", "### Async / await with fibers",
  "### (11–9) Henry VI", "### Glauber’s dynamics", "### Built-in functions... ugh",
  "#### Build (`-B`)", "### Mistake 6: Using `SDL_RENDERER_ACCELERATED`",
  "### Bonus: `*_s` isn't helping you", "#### xorshift128+ and xorshift64\\*",
  "#### [Feedly](http://www.feedly.com/)", "### Alternatives ", "### a ###",
  "### a #", "### a#", "### #", "### ##", "###", "####### a", "#a",
  "### a\n### a\n### a", "### a {#b}", "### a {#b}\n### a",
  "### ![i](/i.png) x", "### <span>a</span> b", "### a <!-- c -->",
  "### a  \n", "### É", "### a_b-c d\te", "### &amp; &lt;", "### ---",
  "### 'a' \"b\"", "### «a»", "### a--b", "### a <<b>>", "### 1. a",
  "para\n# h\npara", "# h\npara", "### a\\\nb", "### *a* __b__",
  "### [a][b]\n\n[b]: /c", "### [a][b]", "### ~~a~~", "### a &#8217; b",
]

# Paragraphs and interruptions
vectors += [
  "a\nb", "a\n\nb", " a", "   a", "a  ", "a\n* b", "a\n1. b", "a\n> b",
  "a\n```\nb\n```", "a\n~~~\nb", "a\n    b", "a\n<div>b</div>",
  "a\n</div>\nb", "a\n<span>b</span>", "a\n<script>b</script>", "a\n# b",
  "a\n#b", "a\n: b", "a\n[b]: /c", "a\n* * *", "a\n---", "a\n===",
  "a\n<!-- b -->", "a\n<s>b</s> c", "a\n<p>b</p>", "a\n<video>b</video>",
  "a\n\n\n\nb", "  \n\na", "a\n  \nb", "a\n\tb", "\ta", " \ta",
]

# Setext headers (a warning, but rendered faithfully)
vectors += ["a\n---", "a\n===", "a\n-", "\na\n---\nb", "a\nb\n---"]

# Code blocks
vectors += [
  "    a", "    a\n    b", "    a\n\n    b", "    a\n\n\n    b\n\n",
  "\ta\n\tb", "    a\n        b", "    a\n  \n    b", "    a\n      \n    b",
  "    a\nb", "    a\n   b", "    <a&b>", "    a\n    <div>", "    a\n<div>\n",
  "```\na\n```", "```c\na\n```", "``` c\na\n```", "```c++\na\n```",
  "~~~\na\n~~~", "~~~\na\n~~~~", "````\na\n```", "````c\na\n```",
  "```\na\n````", "```\n```", "```\n\n```", "```\na\n\n```",
  "```\na", "```\na\n```b\n```", "  ```\n  a\n  ```", "   ```\na\n   ```",
  "```\n<&>\n```", "```c x\na\n```", "```js?x\na\n```", "~~~`\na\n~~~`",
  "```\n'a' \"b\" ...\n```", "a\n\n```\nb\n```\nc", "```\na\n```  \nb",
]

# Lists
vectors += [
  "* a\n* b", "* a\n\n* b", "* a\n* b\n\n* c", "* a\n\n* b\n* c",
  "1. a\n2. b", "1. a\n\n2. b", "5. a\n7. b", "1) a", "* a\n1. b",
  "1. a\n* b", "* a\n  * b\n* c", "* a\n    * b", "* a\n\n  * b",
  "* July 2017: [x](/y)\n  * a\n  * b\n* c", "* a\nb", "* a\n\nb",
  "* a\n  b", "* a\n\n  b", "* a\n\n    b", "* a\n\n  b\n\n* c",
  "*\ta", "* \ta", "*  a\n   b", "*    a", "*     a", "* a\n* * *",
  "* a\n\n* * *", "  * a\n  * b", "   * a", "    * a", "* \n  a",
  "* a\n  > b", "* a\n\n  > b", "* a\n  ```\n  b\n  ```", "* a\n\n      b",
  "* a\n# b", "* a\n<div>b</div>", "* a\n[b]: /c\n\n[b]", "* [b]: /c\n* d",
  "* a\n\n\n* b", "* a\n* b\n\nc", "- a", "+ a", "- a\n+ b", "*|a",
  "* a\n  * b\n\n  c", "1. a\n   * b\n2. c", "10. a\n11. b",
  "* a\n 1. b", "* 'a'", "* \"a\" b", "* a'\n  * b", "* *a*", "* a\n\n  ```\n  b\n  ```",
  "* a\n\n  para\n\n* b", "* [ ] a", "* [x] a", "* a\n* ", "* \n* a",
  "1. a\n\n   b\n2. c", "* a\n  b\n  * c\n  d", "* a\n\t* b",
]

# Blockquotes
vectors += [
  "> a", "> a\n> b", "> a\nb", "> a\n\nb", ">a", ">  a", "> > a",
  "> a\n> * b\n> * c", "> 1. a\n>\n> 2. b", "> # a", "> ```\n> a\n> ```",
  "   > a", "> a\n<div>b</div>", "> a\n\n> b", "> 'a'", "> a\n---",
  "> a\n    b", "> [a]: /b\n\n[a]",
]

# Block HTML
vectors += [
  "<div>a</div>", "<div>\na\n</div>", "<div>*a* 'b'</div>",
  "<div><p>a</p></div>", "<div>a</div> b", "<div>a</div>\nb",
  "<div>a</div>\n\nb", "  <div>a</div>", "<DIV>a</div>", "<Div>a</DIV>",
  "<video src=\"a.mp4\" width=\"10\"\n  controls>\n</video>",
  "<video src=\"a.mp4\" loop muted autoplay controls></video>",
  "<video><source src=\"a\" type=\"b\"></video>", "<hr>", "<hr/>\na",
  "<p class=\"abstract\">a 'b'</p>", "<pre>a <i>b</i> &#123; c</pre>",
  "<script>a < b && 'c'</script>", "<script>\na\n</SCRIPT>\nb",
  "<style>a{}</style>", "<svg><path d=\"a\"/></svg>", "<s>a *b* 'c'</s> d",
  "<table><tr><td>a</td></tr></table>", "<div>a\n\nb</div>",
  "<div><div>a</div></div>", "<div>a</p></div>", "<div>a < b</div>",
  "<div>a &amp; b &c</div>", "<div a=\"<\" b='\"'>c</div>",
  "<div><!-- a --></div>", "<div><?a?></div>", "<div><![CDATA[a<b]]></div>",
  "<!-- a -->", "<!-- a -->\nb", "<!-- a --> b", "<!--more-->\n\na",
  "<!--\na\n-->", "  <!-- a -->", "<!-- a", "</div>", "</div>\na",
  "<span>a</span>\nb", "<a href=\"/b\">c</a>", "<img src=\"a\"/>\n<!-- c -->",
  "<div id=\"\">a</div>", "<div id=\"a\" id=\"b\">c</div>",
  "<div\nclass=\"a\">b</div>", "<video src=a.mp4></video>", "<p>a</p>\n<p>b</p>",
  "<div>a</div><div>b</div>", "<div/>", "<div />\na", "<br>\na",
  "<details><summary>a</summary>b</details>", "<x-y>a</x-y>",
]

# Hand-written documents mixing blocks
vectors += [
  "# T\n\nA 'para' with [a link][x] and `code`.\n\n* one\n* two\n\n[x]: /x\n",
  "*This article was discussed on [HN][hn].*\n\n<!--more-->\n\nText.\n\n[hn]: https://news.ycombinator.com/\n",
  "Intro:\n\n    int main(void)\n    {\n        return 0;\n    }\n\nOutro.\n",
  "1. First\n\n   More first.\n\n2. Second\n\n   * nested\n   * list\n",
  "> Quote 'one'\n> and \"two\"\n\n-- attribution\n",
  "### Header\nText right after.\n\n```c\nint x = 1;\n```\nText after fence.\n",
]

# Found by differential review against kramdown
vectors += [
  # Raw text lines (no block parser applies) keep a trailing hard break
  # unless they end a block element
  " \t\\", " \ta\\\n \tb\\", " \ta\\\n\nx", ">  \t\\", ">  \ta\\\n>\n> b",
  "* a\n\n   \tb\\", "* a\n\n   \tb\\\n* c",
  # [^ without a footnote name starts a link definition
  "[^]:/", "[^]: /u\n\n[^]", "[^-a]: /u\n\n[^-a]", "x [^] y",
  # The markdown attribute is dropped everywhere
  "<div><br markdown=\"1\"></div>", "<div><img src=\"a\" markdown=\"x\"></div>",
  "<video><foo:bar markdown=\"\"></foo:bar></video>", "<span markdown=\"x\">a</span>",
  "<div a=\"b\" markdown=\"0\"><br></div>", "<DIV><P MARKDOWN=\"x\">a</P></DIV>",
  # Carriage returns
  "a\rb", "a\r\nb", "a\r", "\r\r", "> a\r> b", "a  \rb", "\r'",
  # A lazy line in a code block starts with a non-space
  "    #\n\f]", "\t[\r\v~", "    a\n\vb",
  # Case folding and word characters beyond ASCII
  "### Łukasiewicz", "### Straße Ärger", "### Ωmega Σ", "### Škoda Čech",
  "### İstanbul", "### Șerban Țara", "### Ǆ ǅ ǆ", "### Ⅻ Ⓐ ＡＢＣ", "### x → ℝ",
  "### ℓ₁ norm", "### Ơn Ưu", "[École][]\n\n[école]: /u", "[ΑΒΓ][]\n\n[αβγ]: /u",
  "[Łódź][]\n\n[ŁÓDŹ]: /u", "[İ][]\n\n[i̇]: /u", "‿_a_", "<a@‿.c>", "a‿'b'",
  "ℓ'a'",
  # Fence lengths
  "`````\na\n```\n", "~~~~~c\na\n~~~\n", "~`~\na\n~`~~\n", "```` c\n```\n````\n",
  "````\n```\n````", "``` a\n````\n", "<pre>`|`</pre>",
]

# Random documents built from span and block tokens, with a fixed seed.
# Set SEED and FUZZ (a multiplier) in the environment for a larger search.
rng  = Random.new((ENV['SEED'] || 20260928).to_i)
fuzz = (ENV['FUZZ'] || 1).to_i
span_tokens = [
  "*", "**", "_", "__", "`", "``", "'", "\"", "[", "]", "(", ")", "![", "<",
  ">", "&", "&amp;", "\\", "~~", "...", "--", " ", " ", " ", "\n", "  \n",
  "a", "b", "s", "é", "—", "1", ".", ",", "-", "<kbd>", "</kbd>", "<span>",
  "</span>", "<b>", "</b>", "</p>", "<p>", "http://x", "<http://x>",
  "[x]", "[y][x]", "](/u)", "![i](/u)", "<!-- c -->", "&#39;", "&quot;",
  "_a_", "*a*", "'s", "'ll", "a'", "\"a\"", "<br>", "~",
]
(600*fuzz).times do
  n = 2 + rng.rand(10)
  vectors << Array.new(n) { span_tokens[rng.rand(span_tokens.size)] }.join
end

line_tokens = [
  "* a", "* b 'c'", "  * c", "    * d", "1. x", "2. y", "", "", "", "> q",
  "> 'q'", "para *x*", "text [x]", "    code", "\tcode", "```", "```c",
  "~~~", "# h", "### h 'x'", "<div>", "</div>", "<!-- c -->", "[x]: /u",
  "  more", "* * *", "---", "<p>p</p>", "a  ", "  b", "<video>", "</video>",
  "*a", "b*", "`c", "d`", "[x][]", "  > r", "   1. z",
]
(400*fuzz).times do
  n = 2 + rng.rand(7)
  vectors << Array.new(n) { line_tokens[rng.rand(line_tokens.size)] }.join("\n")
end

# Mixed block and span tokens, joined without separators
mixed_tokens = [
  "\n", "\n", "\n\n", "  \n", "\\\n", "    ", "\t", "* ", "1. ", "> ", "# ",
  "### ", "```", "```c\n", "~~~\n", "<div>", "</div>", "<p>", "</p>", "<!--",
  "-->", "<!-- x -->", "<span>", "</span>", "<kbd>", "</kbd>", "<a href=\"u\">",
  "</a>", "<img src=\"i\">", "<br>", "[a]: /u\n", "[a]", "[a][]", "[b][a]",
  "](/v)", "[", "]", "(", ")", "![", "*", "**", "***", "_", "__", "`", "``",
  "'", "\"", "''", "&", "&amp;", "&#39;", "...", "--", "~~", "\\", "\\*",
  "a", "b", "word", "x y", "é", "—", "1", "s", ".", ",", ":", "-", "=",
  "<http://x.y>", "<a@b.c>", "}", "<", ">", "<<", ">>",
]
(300*fuzz).times do
  n = 3 + rng.rand(14)
  vectors << Array.new(n) { mixed_tokens[rng.rand(mixed_tokens.size)] }.join
end

# Quote contexts, exhaustively
pre  = ["", "a", " ", "é", "—", "(", "[", "{", "-", "`c`", "*e*", "[l](/u)",
        "<kbd>k</kbd>", "&amp;", "\\*", "...", "~~d~~", "1", "s", "a\n", ".",
        "!", "_", "<!-- c -->", "<b>b</b>"]
post = ["", "a", "s", "s.", "st", "ll", " b", "\nb", "80s", "*e*", "**s**",
        "`c`", "...", "..", ".", ")", "]", "é", "—", "[l](/u)", "&amp;", "-"]
pre.each do |a|
  ["'", "\"", "''", "'\""].each do |q|
    post.each { |b| vectors << "#{a}#{q}#{b}" }
  end
end

# The same span contexts inside other blocks
(vectors.select { |v| !v.include?("\n") }.first(200)).each do |v|
  vectors << "* #{v}" << "### #{v}" << "> #{v}"
end

# Unsupported syntax, which warns instead: definition lists, tables, task
# lists. Also unknown named entities (kramdown writes &amp;name; while
# this port passes every name through) and &#0;.
unsupported = ["a\n: b", "*|a", "* [ ] a", "* [x] a", "a &b; c", "&#0;"]
vectors.uniq!
vectors -= unsupported

puts "// Generated by test/vectors.rb from kramdown #{Kramdown::VERSION} and " \
     "kramdown-parser-gfm #{Kramdown::Parser::GFM::VERSION}. Do not edit."
vectors.each do |md|
  html = render(md)
  if !html.valid_encoding?
    warn "skipping (invalid output): #{md.inspect}"
  elsif html =~ /<dl>|task-list|class="footnotes"|kdmath/ || md =~ /\{:.*\}/m ||
        (html.include?('<abbr title') && !md.include?('<abbr')) ||
        (html.include?('<table>') && !md.include?('<table>'))
    warn "skipping (unsupported syntax): #{md.inspect}"
  else
    puts "{#{cstr(md)}, #{cstr(html)}},"
  end
end
