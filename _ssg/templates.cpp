// Page templates: layouts and generated pages, printed into buffers
// This is free and unencumbered software released into the public domain.

static Str site_url   = "https://nullprogram.com";
static Str site_title = "null program";
static Str site_desc  = "Hobby computing blog";
static Str feed_uuid  = "f8b65823-4ec5-3a70-efc8-2b713aa63091";

// The title is the concatenation of two parts.
static void layouthead(Buf *b, Str title, Str title2 = {})
{
    print(b, "<!DOCTYPE html>\n<title>");
    printhtml(b, title);
    printhtml(b, title2);
    print(b, "</title>\n"
R"(<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0"/>
<link rel="alternate" type="application/atom+xml" href="/feed/" title="Atom Feed"/>
<link rel="pgpkey" type="application/pgp-keys" href="/0xAFD1503A8C8FF42A.pgp"/>
<link rel="stylesheet" href="/css/full.css"/>

<main lang="en">
)");
}

static void layouttail(Buf *b)
{
    print(b,
R"(</main>

<header>
  <div class="container">
    <div class="portrait identity"></div>
    <h1 class="site-title identity"><a href="/">null program</a></h1>
    <h2 class="full-name identity">Chris Wellons</h2>
    <address class="identity">
      <div><a id="email" href="mailto&#x3a;wellons&#x40;nullprogram&#x2e;com">wellons&#x40;nullprogram&#x2e;com</a> (<a rel="publickey" type="application/pgp-keys" href="/0xAFD1503A8C8FF42A.pgp">PGP</a>)</div>
      <div><a id="public-inbox" href="mailto:~skeeto/public-inbox@lists.sr.ht">~skeeto/public-inbox@lists.sr.ht</a> (<a href="https://lists.sr.ht/~skeeto/public-inbox">view</a>)</div>
    </address>
    <nav>
      <ul>
        <li class="nav index"><a href="/index/">Index</a></li>
        <li class="nav tags"><a href="/tags/">Tags</a></li>
        <li class="nav feed"><a href="/feed/">Feed</a></li>
        <li class="nav about"><a href="/about/">About</a></li>
        <li class="nav tools"><a href="/tools/">Tools</a></li>
        <li class="nav toys"><a href="/toys/">Toys</a></li>
        <li class="nav github"><a href="https://github.com/skeeto">GitHub</a></li>
      </ul>
    </nav>
  </div>
</header>

<footer>
  <p>
    All information on this blog, unless otherwise noted, is
    hereby released into the public domain, with no rights
    reserved.
  </p>
</footer>
)");
}

// The date header shared by post pages and the front page.
static void postheader(Buf *b, Post *p)
{
    print(b, "  <h2><a href=\"");
    printattr(b, p->url);
    print(b, "\">");
    printhtml(b, p->title);
    print(b, "</a></h2>\n  <time datetime=\"");
    printymd(b, p->time);
    print(b, "\">\n    ");
    printlongdate(b, p->time);
    print(b, "\n  </time>\n  <div class=\"print-only url\">\n    nullprogram.com");
    printhtml(b, p->url);
    print(b, "\n  </div>\n\n");
}

static void posttags(Buf *b, Post *p)
{
    print(b, "  <ul class=\"tags\">\n");
    for (iz i = 0; i < p->tags.len; i++) {
        print(b, "    <li><a href=\"/tags/");
        printattr(b, p->tags[i]);
        print(b, "/\">");
        printhtml(b, p->tags[i]);
        print(b, "</a></li>\n");
    }
    print(b, "  </ul>\n  <ol class=\"references print-only\"></ol>\n");
}

static void postpage(Buf *b, Post *p)
{
    layouthead(b, p->title);
    print(b, "<article class=\"single\">\n");
    postheader(b, p);
    print(b, p->html);
    print(b, "\n\n");
    posttags(b, p);

    print(b,
R"(
  <div class="no-print comments">
    <p>Have a comment on this article? Start a discussion in my
    <a href="https://lists.sr.ht/~skeeto/public-inbox">public inbox</a>
    by sending an email to
    <a href="mailto:~skeeto/public-inbox@lists.sr.ht?Subject=Re%3A%20)");
    printuriescape(b, p->title);
    print(b,
R"(">
        ~skeeto/public-inbox@lists.sr.ht
    </a>
    <span class="etiquette">
    [<a href="https://man.sr.ht/lists.sr.ht/etiquette.md">mailing list etiquette</a>]
    </span>,
    or see
    <a href="https://lists.sr.ht/~skeeto/public-inbox?search=)");
    printurlencode(b, p->title);
    print(b,
R"(">existing discussions</a>.
    </p>
  </div>

  <nav class="no-print">
)");
    if (p->older) {
        print(b, "    <div class=\"prev\">\n      <span class=\"marker\">«</span>\n      <a href=\"");
        printattr(b, p->older->url);
        print(b, "\">\n        ");
        printhtml(b, p->older->title);
        print(b, "\n      </a>\n    </div>\n");
    }
    if (p->newer) {
        print(b, "    <div class=\"next\">\n      <span class=\"marker\">»</span>\n      <a href=\"");
        printattr(b, p->newer->url);
        print(b, "\">\n        ");
        printhtml(b, p->newer->title);
        print(b, "\n      </a>\n    </div>\n");
    }
    print(b, "  </nav>\n</article>\n");
    layouttail(b);
}

static void homepage(Buf *b, Site *site, iz limit)
{
    layouthead(b, site_title);
    for (iz i = 0; i < site->posts.len && i < limit; i++) {
        Post *p = site->posts[i];
        print(b, "<article class=\"many\">\n");
        postheader(b, p);
        print(b, "  <blockquote class=\"excerpt\" cite=\"");
        printattr(b, p->url);
        print(b, "\">\n");
        print(b, takehead(p->html, p->excerpt));
        print(b, "\n  </blockquote>\n  <div class=\"read-more\">[<a href=\"");
        printattr(b, p->url);
        print(b, "\">…</a>]</div>\n\n");
        posttags(b, p);
        print(b, "</article>\n");
    }
    layouttail(b);
}

static void postitem(Buf *b, Post *p)
{
    print(b, "    <li>\n      <time datetime=\"");
    printymd(b, p->time);
    print(b, "\">\n        ");
    printymd(b, p->time);
    print(b, "\n      </time>\n      <a href=\"");
    printattr(b, p->url);
    print(b, "\">");
    printhtml(b, p->title);
    print(b, "</a>\n    </li>\n");
}

static void archivepage(Buf *b, Site *site)
{
    layouthead(b, "Archives");
    print(b, "<article class=\"index\">\n  <h2>Archives</h2>\n  <p>\n"
             "    There are <span class=\"post-count\">");
    print(b, (i64)site->posts.len);
    print(b, "</span> articles.\n  </p>\n  <ul class=\"post-list\">\n");
    for (iz i = 0; i < site->posts.len; i++) {
        postitem(b, site->posts[i]);
    }
    print(b, "  </ul>\n</article>\n");
    layouttail(b);
}

static void tagindexpage(Buf *b, Site *site)
{
    layouthead(b, "Tags");
    print(b, "<article class=\"tag\">\n  <h2>Tags</h2>\n  <ul class=\"post-list\">\n");
    for (iz i = 0; i < site->tags.len; i++) {
        Str name = site->tags[i]->name;
        print(b, "    <li>\n      <a href=\"/tags/");
        printattr(b, name);
        print(b, "/feed/\" class=\"feed\"></a>\n      <a href=\"/tags/");
        printattr(b, name);
        print(b, "/\" class=\"tag-entry\">");
        printhtml(b, name);
        print(b, "</a>\n    </li>\n");
    }
    print(b, "  </ul>\n</article>\n");
    layouttail(b);
}

static void tagpage(Buf *b, Tag *tag)
{
    layouthead(b, "Posts tagged ", tag->name);
    print(b, "<article class=\"tag\">\n  <h2>\n    <a class=\"feed\" href=\"/tags/");
    printattr(b, tag->name);
    print(b, "/feed/\"></a>\n    Articles tagged \"");
    printhtml(b, tag->name);
    print(b, "\"\n  </h2>\n  <ul class=\"post-list\">\n");
    for (iz i = 0; i < tag->posts.len; i++) {
        postitem(b, tag->posts[i]);
    }
    print(b, "  </ul>\n</article>\n");
    layouttail(b);
}

static void printcdata(Buf *b, Str s)
{
    print(b, "<![CDATA[");
    for (;;) {
        iz i = find(s, "]]>");
        if (i < 0) {
            break;
        }
        print(b, takehead(s, i+2));
        print(b, "]]><![CDATA[");
        s = cuthead(s, i+2);
    }
    print(b, s);
    print(b, "]]>");
}

static void atomentry(Buf *b, Post *p)
{
    print(b, "  <entry>\n    <title>");
    printhtml(b, p->title);
    print(b, "</title>\n    <link rel=\"alternate\" type=\"text/html\" href=\"");
    printattr(b, site_url);
    printattr(b, p->url);
    print(b, "\"/>\n    <id>urn:uuid:");
    print(b, p->uuid);
    print(b, "</id>\n    <updated>");
    printiso(b, p->time);
    print(b, "</updated>\n    ");
    for (iz i = 0; i < p->tags.len; i++) {
        print(b, "<category term=\"");
        printattr(b, p->tags[i]);
        print(b, "\"/>");
    }
    print(b, "\n    <content type=\"html\">\n      ");
    printcdata(b, p->html);
    print(b, "\n    </content>\n  </entry>\n");
}

static void atomauthor(Buf *b, Str uri, Str suffix)
{
    print(b, "  <author>\n    <name>Christopher Wellons</name>\n    <uri>");
    print(b, uri);
    print(b, suffix);
    print(b, "</uri>\n    <email>wellons@nullprogram.com</email>\n  </author>\n\n");
}

static void atomfeed(Buf *b, Site *site)
{
    print(b, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
             "<feed xmlns=\"http://www.w3.org/2005/Atom\">\n\n  <title>");
    printhtml(b, site_title);
    print(b, "</title>\n  <link rel=\"alternate\" type=\"text/html\" href=\"");
    print(b, site_url);
    print(b, "\"/>\n  <link rel=\"self\" type=\"application/atom+xml\" href=\"");
    print(b, site_url);
    print(b, "/feed/\"/>\n  <updated>");
    printiso(b, site->now);
    print(b, "</updated>\n  <id>urn:uuid:");
    print(b, feed_uuid);
    print(b, "</id>\n\n");
    atomauthor(b, site_url, "/");
    for (iz i = 0; i < site->posts.len && i < 8; i++) {
        atomentry(b, site->posts[i]);
    }
    print(b, "\n</feed>\n");
}

static void tagfeed(Buf *b, Site *site, Tag *tag)
{
    print(b, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
             "<feed xmlns=\"http://www.w3.org/2005/Atom\">\n\n  <title>Articles tagged ");
    printhtml(b, tag->name);
    print(b, " at ");
    printhtml(b, site_title);
    print(b, "</title>\n  <link rel=\"alternate\" type=\"text/html\"\n        href=\"");
    print(b, site_url);
    print(b, "/tags/");
    printattr(b, tag->name);
    print(b, "/\"/>\n  <link rel=\"self\" type=\"application/atom+xml\"\n        href=\"");
    print(b, site_url);
    print(b, "/tags/");
    printattr(b, tag->name);
    print(b, "/feed/\"/>\n  <updated>");
    printiso(b, site->now);
    print(b, "</updated>\n  <id>urn:uuid:");
    print(b, tag->uuid);
    print(b, "</id>\n\n");
    atomauthor(b, site_url, "");
    for (iz i = 0; i < tag->posts.len; i++) {
        atomentry(b, tag->posts[i]);
    }
    print(b, "\n</feed>\n");
}

static void rssfeed(Buf *b, Site *site)
{
    print(b, "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n"
             "<rss version=\"2.0\" xmlns:atom=\"http://www.w3.org/2005/Atom\">\n"
             "  <channel>\n    <title>");
    printhtml(b, site_title);
    print(b, "</title>\n    <description>");
    printhtml(b, site_desc);
    print(b, "</description>\n    <link>");
    print(b, site_url);
    print(b, "</link>\n    <atom:link href=\"");
    print(b, site_url);
    print(b, "/blog/index.rss\" rel=\"self\" type=\"application/rss+xml\"/>\n"
             "    <language>en-us</language>\n    <pubDate>");
    printrfc822(b, site->now);
    print(b, "</pubDate>\n    <lastBuildDate>");
    printrfc822(b, site->now);
    print(b, "</lastBuildDate>\n\n");
    for (iz i = 0; i < site->posts.len && i < 8; i++) {
        Post *p = site->posts[i];
        print(b, "    <item>\n      <title>");
        printhtml(b, p->title);
        print(b, "</title>\n      <link>");
        print(b, site_url);
        printhtml(b, p->url);
        print(b, "</link>\n      <guid>");
        print(b, site_url);
        printhtml(b, p->url);
        print(b, "</guid>\n      <pubDate>");
        printrfc822(b, p->time);
        print(b, "</pubDate>\n      <comments>");
        print(b, site_url);
        printhtml(b, p->url);
        print(b, "#disqus_thread</comments>\n      <description>\n        ");
        printcdata(b, p->html);
        print(b, "\n      </description>\n    </item>\n");
    }
    print(b, "\n  </channel>\n</rss>\n");
}

static void aboutpage(Buf *b, Page *p)
{
    layouthead(b, p->title);
    print(b, "<article class=\"single\">\n<h2>");
    printhtml(b, p->title);
    print(b, "</h2>\n");
    print(b, p->html);
    print(b, "\n</article>\n");
    layouttail(b);
}
