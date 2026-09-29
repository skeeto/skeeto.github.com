#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.11"
# dependencies = []
# ///
"""Semantic comparison of two generated site trees.

Usage: compare.py OLD NEW [options]

HTML is compared after canonicalization (attribute order, whitespace,
comments, entity spelling, code-block wrapper markup, highlighting spans).
Atom/RSS feeds are compared field by field, with entry content canonicalized
as HTML. Everything else is compared byte for byte.
"""

import argparse
import concurrent.futures
import difflib
import fnmatch
import hashlib
import html.parser
import os
import re
import sys
import xml.etree.ElementTree as ET

VOID = {
    "area", "base", "br", "col", "command", "embed", "hr", "img", "input",
    "keygen", "link", "meta", "param", "source", "track", "wbr",
}
INLINE = {
    "a", "abbr", "acronym", "b", "bdo", "big", "br", "cite", "code", "del",
    "dfn", "em", "i", "img", "ins", "kbd", "mark", "q", "s", "samp", "small",
    "span", "strong", "sub", "sup", "time", "tt", "u", "var",
}
PRESERVE = {"pre", "script", "style", "textarea"}
WS = re.compile(r"[ \t\n\r\f]+")
TOLERANT_TEXT = str.maketrans({
    "‘": "'", "’": "'", "“": '"', "”": '"',
    "…": "...", "–": "--",
})
SITE_URL = "https://nullprogram.com"


class Canon(html.parser.HTMLParser):
    """Turn HTML into a flat list of canonical tokens."""

    def __init__(self, tolerant):
        super().__init__(convert_charrefs=True)
        self.tolerant = tolerant
        self.toks = []        # (kind, name, payload)
        self.preserve = 0     # depth inside pre/script/style/textarea
        self.pre = 0          # depth inside pre
        self.dropstack = {}   # tag -> stack of bools: was this start dropped?

    def _dropped(self, tag, attrs):
        cls = (dict(attrs).get("class") or "").split()
        if tag == "div" and ("highlighter-rouge" in cls or "highlight" in cls):
            return True
        if tag == "figure" and "highlight" in cls:
            return True
        if self.pre and tag == "span":
            return True
        if self.tolerant:
            if tag in ("p", "i", "b", "em", "strong"):
                return True
            if tag == "code" and self.pre:
                return True
        return False

    def _start(self, tag, attrs, selfclose):
        drop = self._dropped(tag, attrs)
        if tag not in VOID:
            self.dropstack.setdefault(tag, []).append(drop)
            if tag in PRESERVE:
                self.preserve += 1
            if tag == "pre":
                self.pre += 1
        if drop:
            return
        a = {}
        for k, v in attrs:
            v = "" if v is None else WS.sub(" ", v).strip()
            a[k.lower()] = v
        if tag in ("pre", "code"):
            a = {}
        if self.tolerant:
            if tag in ("h1", "h2", "h3", "h4", "h5", "h6"):
                a.pop("id", None)
            if "class" in a:
                c = [x for x in a["class"].split() if x != "center"]
                if c:
                    a["class"] = " ".join(c)
                else:
                    del a["class"]
        payload = " ".join(f'{k}="{a[k]}"' for k in sorted(a))
        self.toks.append(("S", tag, payload))
        if selfclose and tag not in VOID:
            self._end(tag)

    def handle_starttag(self, tag, attrs):
        self._start(tag, attrs, False)

    def handle_startendtag(self, tag, attrs):
        self._start(tag, attrs, True)

    def _end(self, tag):
        if tag in VOID:
            return
        stack = self.dropstack.get(tag)
        if self.tolerant and not stack:
            # A stray end tag with no open element of that name. Browsers
            # ignore it, and the converted posts had these removed.
            return
        drop = stack.pop() if stack else False
        if stack is not None and tag in PRESERVE and self.preserve:
            self.preserve -= 1
        if tag == "pre" and self.pre:
            self.pre -= 1
        if not drop:
            self.toks.append(("E", tag, ""))

    def handle_endtag(self, tag):
        self._end(tag)

    def handle_data(self, data):
        if self.tolerant:
            data = data.translate(TOLERANT_TEXT)
        kind = "P" if self.preserve else "T"
        self.toks.append((kind, "", data))

    def handle_comment(self, data):
        pass

    def handle_decl(self, decl):
        self.toks.append(("D", "", WS.sub(" ", decl.lower())))

    def handle_pi(self, data):
        self.toks.append(("D", "", data))


def canon_html(text, tolerant=False):
    p = Canon(tolerant)
    p.feed(text)
    p.close()
    toks = p.toks

    # Merge adjacent text tokens of the same kind.
    merged = []
    for t in toks:
        if merged and t[0] in "TP" and merged[-1][0] == t[0]:
            merged[-1] = (t[0], "", merged[-1][2] + t[2])
        else:
            merged.append(t)
    toks = merged

    # Preserved text: drop the newline right after <pre>, and trailing
    # newlines at the end of the block (fences keep one, Liquid's
    # highlight tag does not).
    for i, t in enumerate(toks):
        if t[0] != "P":
            continue
        s = t[2]
        if i and toks[i-1][0] == "S" and toks[i-1][1] == "pre":
            if s.startswith("\n"):
                s = s[1:]
        j = i + 1
        while j < len(toks) and toks[j][0] == "E" and toks[j][1] == "code":
            j += 1
        if j < len(toks) and toks[j][0] == "E" and toks[j][1] == "pre":
            s = s.rstrip("\n")
        if toks[i-1][1] in ("script", "style") if i else False:
            s = s.strip()
        toks[i] = ("P", "", s)

    # Collapse whitespace in ordinary text, trim at block boundaries.
    def isblock(t):
        return t[0] in "SED" and t[1] not in INLINE

    out = []
    for i, t in enumerate(toks):
        if t[0] == "T":
            s = WS.sub(" ", t[2])
            if i == 0 or isblock(toks[i-1]):
                s = s.lstrip(" ")
            if i == len(toks) - 1 or isblock(toks[i+1]):
                s = s.rstrip(" ")
            if not s:
                continue
            out.append(("T", "", s))
        elif t[0] == "P":
            if t[2]:
                out.append(t)
        else:
            out.append(t)

    lines = []
    for kind, name, payload in out:
        if kind == "S":
            lines.append(f"<{name} {payload}>" if payload else f"<{name}>")
        elif kind == "E":
            lines.append(f"</{name}>")
        elif kind == "D":
            lines.append(f"<!{payload}>")
        elif kind == "T":
            lines.append("T " + payload)
        else:
            for ln in payload.split("\n"):
                lines.append("| " + ln)
    return lines


def sort_tag_index(lines):
    """The tag index is compared as a sorted set of <li> groups."""
    try:
        beg = lines.index('<ul class="post-list">') + 1
        end = len(lines) - 1 - lines[::-1].index("</ul>")
    except ValueError:
        return lines
    groups, cur = [], []
    for ln in lines[beg:end]:
        cur.append(ln)
        if ln == "</li>":
            groups.append(cur)
            cur = []
    groups.sort()
    return lines[:beg] + [x for g in groups for x in g] + cur + lines[end:]


ATOM = "{http://www.w3.org/2005/Atom}"


def urlpath(u):
    u = (u or "").strip()
    if u.startswith(SITE_URL):
        u = u[len(SITE_URL):]
    return u


def canon_feed(data, opts):
    root = ET.fromstring(data)
    lines = []

    def field(name, val):
        lines.append(f"{name}: {WS.sub(' ', (val or '')).strip()}")

    def content(text, link):
        tol = urlpath(link) in opts.tolerant
        lines.extend("  " + x for x in canon_html(text or "", tol))

    if root.tag == ATOM + "feed":
        field("title", root.findtext(ATOM + "title"))
        for ln in root.findall(ATOM + "link"):
            field("link", " ".join(f"{k}={ln.get(k)}" for k in sorted(ln.keys())))
        upd = root.findtext(ATOM + "updated")
        field("updated", "MASKED" if opts.mask_time else upd)
        field("id", root.findtext(ATOM + "id"))
        for a in root.findall(ATOM + "author/*"):
            field("author." + a.tag.replace(ATOM, ""), a.text)
        for e in root.findall(ATOM + "entry"):
            lines.append("--- entry")
            field("title", e.findtext(ATOM + "title"))
            link = e.find(ATOM + "link")
            href = link.get("href") if link is not None else ""
            field("link", href)
            field("id", e.findtext(ATOM + "id"))
            field("updated", e.findtext(ATOM + "updated"))
            field("categories", " ".join(
                c.get("term") for c in e.findall(ATOM + "category")))
            c = e.find(ATOM + "content")
            field("content.type", c.get("type") if c is not None else "")
            content(c.text if c is not None else "", href)
    elif root.tag == "rss":
        ch = root.find("channel")
        for name in ("title", "description", "link", "language"):
            field(name, ch.findtext(name))
        al = ch.find(ATOM + "link")
        if al is not None:
            field("atom:link", " ".join(f"{k}={al.get(k)}" for k in sorted(al.keys())))
        for name in ("pubDate", "lastBuildDate"):
            field(name, "MASKED" if opts.mask_time else ch.findtext(name))
        for it in ch.findall("item"):
            lines.append("--- item")
            for name in ("title", "link", "guid", "pubDate", "comments"):
                field(name, it.findtext(name))
            content(it.findtext("description"), it.findtext("link"))
    else:
        raise ValueError("unknown feed type " + root.tag)
    return lines


def pagepath(rel):
    """Output path -> URL path, e.g. blog/2008/01/29/index.html -> /blog/2008/01/29/."""
    u = "/" + rel
    if u.endswith("/index.html"):
        u = u[:-len("index.html")]
    return u


def compare_file(rel, opts):
    """Return None when equivalent, else the diff text."""
    a = os.path.join(opts.old, rel)
    b = os.path.join(opts.new, rel)
    with open(a, "rb") as f:
        da = f.read()
    with open(b, "rb") as f:
        db = f.read()
    if da == db:
        return None
    ext = os.path.splitext(rel)[1]
    try:
        if ext == ".html":
            tol = pagepath(rel) in opts.tolerant
            la = canon_html(da.decode(), tol)
            lb = canon_html(db.decode(), tol)
            if rel == "tags/index.html":
                la, lb = sort_tag_index(la), sort_tag_index(lb)
        elif ext in (".xml", ".rss"):
            la = canon_feed(da, opts)
            lb = canon_feed(db, opts)
        else:
            return "binary files differ\n"
    except (UnicodeDecodeError, ET.ParseError, ValueError) as e:
        return f"cannot canonicalize: {e}\n"
    if la == lb:
        return None
    d = difflib.unified_diff(la, lb, "old/" + rel, "new/" + rel,
                             n=opts.context, lineterm="")
    return "\n".join(d) + "\n"


def walk(root):
    out = set()
    for dirpath, dirnames, filenames in os.walk(root):
        for f in filenames:
            out.add(os.path.relpath(os.path.join(dirpath, f), root))
    return out


def load_accept(path):
    acc = {}
    if path:
        with open(path) as f:
            for ln in f:
                ln = ln.split("#", 1)[0].strip()
                if ln:
                    rel, digest = ln.split()
                    acc[rel] = digest
    return acc


def load_tolerant(path):
    s = set()
    if path:
        with open(path) as f:
            for ln in f:
                ln = ln.split("#", 1)[0].strip()
                if ln:
                    if not ln.startswith("/"):
                        ln = pagepath(ln)
                    s.add(ln)
    return s


def worker(args):
    rel, opts = args
    return rel, compare_file(rel, opts)


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("old")
    ap.add_argument("new")
    ap.add_argument("--mask-time", action="store_true",
                    help="ignore the site build time in feeds")
    ap.add_argument("--tolerant-posts", metavar="FILE",
                    help="URL paths of converted posts compared tolerantly")
    ap.add_argument("--accept", metavar="FILE",
                    help="lines of 'PATH SHA256' naming accepted diffs")
    ap.add_argument("--only", metavar="GLOB", action="append",
                    help="only compare paths matching GLOB")
    ap.add_argument("--ignore", metavar="GLOB", action="append", default=[],
                    help="ignore paths matching GLOB (either side)")
    ap.add_argument("--context", type=int, default=3)
    ap.add_argument("--max-lines", type=int, default=60,
                    help="diff lines shown per file (0 = unlimited)")
    ap.add_argument("--list", action="store_true",
                    help="print only the names of differing files with digests")
    opts = ap.parse_args()
    opts.tolerant = load_tolerant(opts.tolerant_posts)
    accept = load_accept(opts.accept)

    def keep(rel):
        if any(fnmatch.fnmatch(rel, g) for g in opts.ignore):
            return False
        if opts.only:
            return any(fnmatch.fnmatch(rel, g) for g in opts.only)
        return True

    old = {r for r in walk(opts.old) if keep(r)}
    new = {r for r in walk(opts.new) if keep(r)}
    bad = 0
    for rel in sorted(old - new):
        print(f"only in old: {rel}")
        bad += 1
    for rel in sorted(new - old):
        print(f"only in new: {rel}")
        bad += 1

    common = sorted(old & new)
    same = accepted = differ = 0
    with concurrent.futures.ProcessPoolExecutor() as ex:
        results = ex.map(worker, ((r, opts) for r in common), chunksize=8)
        for rel, diff in results:
            if diff is None:
                same += 1
                continue
            digest = hashlib.sha256(diff.encode()).hexdigest()
            if accept.get(rel) == digest:
                accepted += 1
                continue
            differ += 1
            if opts.list:
                print(f"{rel} {digest}")
                continue
            print(f"=== {rel}  (sha256 {digest})")
            lines = diff.splitlines()
            if opts.max_lines and len(lines) > opts.max_lines:
                extra = len(lines) - opts.max_lines
                lines = lines[:opts.max_lines] + [f"... {extra} more lines"]
            print("\n".join(lines))

    print(f"\n{same} equivalent, {accepted} accepted, {differ} differ, "
          f"{bad} missing on one side", file=sys.stderr)
    return 1 if differ or bad else 0


if __name__ == "__main__":
    sys.exit(main())
