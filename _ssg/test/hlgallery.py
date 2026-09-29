#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.11"
# dependencies = []
# ///
"""Highlight every fenced code block in the posts and write a gallery.

Usage: hlgallery.py [--posts DIR] [--hlcat EXE] [--css FILE] [--only TAG]
                    [-o OUT.html]

Each block is run through hlcat, and its output is checked to round-trip:
stripping the spans and unescaping must give back the block exactly. The
gallery shows every block in light and dark, grouped by language. Exits
nonzero if any block fails to round-trip or has an unknown language.
"""

import argparse
import collections
import concurrent.futures
import html
import os
import re
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
FENCE = re.compile(r"^ {0,3}(([~`])\2{2,})[ \t]*([^\s`]+)?[ \t]*$")
SPAN = re.compile(r'<span class="[a-z]+">|</span>')


def blocks(posts):
    """Yield (name, line, lang, code) for each fenced block."""
    for name in sorted(os.listdir(posts)):
        if not name.endswith((".markdown", ".md")):
            continue
        with open(os.path.join(posts, name), encoding="utf-8", newline="") as f:
            lines = f.read().replace("\r\n", "\n").split("\n")
        i = 0
        while i < len(lines):
            m = FENCE.match(lines[i])
            if not m:
                i += 1
                continue
            fence, ch, lang = m.group(1), m.group(2), m.group(3) or ""
            close = re.compile(r"^ {0,3}" + re.escape(fence) + re.escape(ch) + r"*[ \t]*$")
            j = i + 1
            while j < len(lines) and not close.match(lines[j]):
                j += 1
            if j == len(lines):
                i += 1
                continue
            yield name, i + 1, lang, "".join(l + "\n" for l in lines[i + 1:j])
            i = j + 1


def unhighlight(markup):
    text = SPAN.sub("", markup)
    if "<" in text or ">" in text:
        return None
    return text.replace("&lt;", "<").replace("&gt;", ">").replace("&amp;", "&")


def run(hlcat, block):
    name, line, lang, code = block
    p = subprocess.run([hlcat, lang], input=code.encode(), capture_output=True)
    out = p.stdout.decode("utf-8", errors="surrogateescape")
    return block, p.returncode, out


def splitcss(css):
    """Return the light rules and the dark rules rescoped under .dark."""
    m = re.search(r"@media\s*\(prefers-color-scheme:\s*dark\)\s*\{", css)
    if not m:
        return css, ""
    depth, i = 1, m.end()
    while depth:
        depth += {"{": 1, "}": -1}.get(css[i], 0)
        i += 1
    inner = css[m.end():i - 1]
    dark = re.sub(r"(^|\})(\s*)([^{}]+?)\s*\{",
                  lambda r: r[1] + r[2] + ", ".join(".dark " + s.strip() for s in r[3].split(",")) + " {",
                  inner)
    return css[:m.start()] + css[i:], dark


PAGE = """<!DOCTYPE html>
<meta charset="utf-8">
<title>Highlighting gallery</title>
<style>
body {{ font-family: sans-serif; margin: 1em 2em; color: black; background: white; }}
nav a {{ margin-right: 0.8em; white-space: nowrap; }}
.src {{ font-size: 12px; color: #666; margin-top: 1em; }}
.bad {{ color: red; font-weight: bold; }}
.pair {{ display: grid; grid-template-columns: 1fr 1fr; gap: 6px; }}
.pair > div {{ min-width: 0; padding: 4px; }}
.dark {{ background: #333; }}
pre {{ margin: 0; padding: 0.5em; line-height: 1.1; overflow-x: auto; }}
pre code {{ font-size: 13px; font-family: monospace; }}
.light pre {{ background: FloralWhite; border: 1px dotted LightGray; }}
.dark pre {{ background: #222; border: 1px dotted #777; color: #eee; }}
{light}
{dark}
</style>
<h1>Highlighting gallery</h1>
<p>{summary}</p>
<nav>{nav}</nav>
{body}
"""


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--posts", default=os.path.join(ROOT, "_posts"))
    ap.add_argument("--hlcat", default=os.path.join(ROOT, "_ssg", "build", "hlcat"))
    ap.add_argument("--css", default=os.path.join(ROOT, "css", "syntax.css"))
    ap.add_argument("--only", help="only this language tag")
    ap.add_argument("-o", "--output", default="hl-gallery.html")
    args = ap.parse_args()

    todo = [b for b in blocks(args.posts) if b[2] and (not args.only or b[2] == args.only)]
    plain = sum(1 for b in blocks(args.posts) if not b[2])
    with concurrent.futures.ThreadPoolExecutor(os.cpu_count()) as ex:
        results = list(ex.map(lambda b: run(args.hlcat, b), todo))

    bylang = collections.defaultdict(list)
    failures = unknown = 0
    for block, status, out in results:
        name, line, lang, code = block
        problem = None
        if status == 1:
            problem = "unknown language"
            unknown += 1
        elif status != 0:
            problem = f"hlcat exited with status {status}"
        elif unhighlight(out) != code:
            problem = "does not round-trip"
        if problem and status != 1:
            failures += 1
        if problem:
            print(f"{name}:{line}: {lang}: {problem}", file=sys.stderr)
        bylang[lang].append((name, line, out, problem))

    langs = sorted(bylang, key=lambda l: (-len(bylang[l]), l))
    nav = " ".join(f'<a href="#{html.escape(l)}">{html.escape(l)}&nbsp;({len(bylang[l])})</a>' for l in langs)
    body = []
    for lang in langs:
        body.append(f'<h2 id="{html.escape(lang)}">{html.escape(lang)}</h2>')
        for name, line, out, problem in bylang[lang]:
            note = f' <span class="bad">{html.escape(problem)}</span>' if problem else ""
            body.append(f'<div class="src">{html.escape(name)}:{line}{note}</div>')
            pre = f'<pre class="highlight"><code>{out}</code></pre>'
            body.append(f'<div class="pair"><div class="light">{pre}</div><div class="dark">{pre}</div></div>')

    light, dark = splitcss(open(args.css).read())
    summary = (f"{len(results)} blocks in {len(langs)} languages "
               f"({plain} blocks without a language not shown); "
               f"{failures} failures, {unknown} unknown")
    with open(args.output, "w") as f:
        f.write(PAGE.format(light=light, dark=dark, summary=summary, nav=nav, body="\n".join(body)))
    print(summary, file=sys.stderr)
    return 1 if failures or unknown else 0


if __name__ == "__main__":
    sys.exit(main())
