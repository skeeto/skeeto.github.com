---
title: Web Scraping into an E-book with BeautifulSoup and Pandoc
layout: post
date: 2017-05-15T02:39:20Z
tags: [python, web]
uuid: 8e05a4a5-4601-3717-d1ef-c03ea2413025
---

I recently learned how to use [BeautifulSoup][bs4], a Python library for
manipulating HTML and XML parse trees, and it's been a fantastic
addition to my virtual toolbelt. In the past when I've needed to process
raw HTML, I've tried nasty hacks with Unix pipes, or [routing the
content through a web browser][skewer] so that I could manipulate it via
the DOM API. None of that worked very well, but now I finally have
BeautifulSoup to fill that gap. It's got a selector interface and,
except for rendering, it's basically as comfortable with HTML as
JavaScript.

Today's problem was that I wanted to read [a recommended][df] online
book called [*Interviewing Leather*][il], a story set "in a world where
caped heroes fight dastardly villains on an everyday basis." I say
"online book" because the 39,403 word story is distributed as a series
of 14 blog posts. I'd rather not read it on the website in a browser,
instead preferring it in e-book form where it's more comfortable. The
[last time I did this][ts], I manually scraped the entire book into
Markdown, spent a couple of weeks editing it for mistakes, and finally
sent the Markdown to [Pandoc][pd] to convert into an e-book.

For this book, I just want a quick-and-dirty scrape in order to shift
formats. I've never read it and I may not even like it (*update*: I
enjoyed it), so I definitely don't want to spend much time on the
conversion. Despite [having fun with typing lately][type], I'd also
prefer to keep all the formating — italics, etc. — without re-entering
it all manually.

Fortunately Pandoc can consume HTML as input, so, in theory, I can feed
it the original HTML and preserve all of the original markup. The
challenge is that the HTML is spread across 14 pages surrounded by all
the expected blog cruft. I need some way to extract the book content
from each page, concatenate it together along with chapter headings, and
send the result to Pandoc. Enter BeautifulSoup.

First, I need to construct the skeleton HTML document. Rather than code
my own HTML, I'm going to build it with BeautifulSoup. I start by
creating a completely empty document and adding a doctype to it.

~~~py
from bs4 import BeautifulSoup, Doctype

doc = BeautifulSoup()
doc.append(Doctype('html'))
~~~

Next I create the `html` root element, then add the `head` and `body`
elements. I also add a `title` element. The original content has fancy
Unicode markup — left and right quotation marks, em dash, etc. — so it's
important to declare the page as UTF-8, since otherwise these characters
are likely to be interpreted incorrectly. It always feels odd declaring
the encoding within the content being encoded, but that's just the way
things are.

~~~py
html = doc.new_tag('html', lang='en-US')
doc.append(html)
head = doc.new_tag('head')
html.append(head)
meta = doc.new_tag('meta', charset='utf-8')
head.append(meta)
title = doc.new_tag('title')
title.string = 'Interviewing Leather'
head.append(title)
body = doc.new_tag('body')
html.append(body)
~~~

If I `print(doc.prettify())` then I see the skeleton I want:

~~~html
<!DOCTYPE html>
<html lang="en-US">
 <head>
  <meta charset="utf-8"/>
  <title>
   Interviewing Leather
  </title>
 </head>
 <body>
 </body>
</html>
~~~

Next, I assemble a list of the individual blog posts. When I was
actually writing the script, I first downloaded them locally with [my
favorite download tool][curl], curl, and ran the script against local
copies. I didn't want to hit the web server each time I tested. (Note:
I've truncated these URLs to fit in this article.)

~~~py
chapters = [
    "https://banter-latte.com/2007/06/26/...",
    "https://banter-latte.com/2007/07/03/...",
    "https://banter-latte.com/2007/07/10/...",
    "https://banter-latte.com/2007/07/17/...",
    "https://banter-latte.com/2007/07/24/...",
    "https://banter-latte.com/2007/07/31/...",
    "https://banter-latte.com/2007/08/07/...",
    "https://banter-latte.com/2007/08/14/...",
    "https://banter-latte.com/2007/08/21/...",
    "https://banter-latte.com/2007/08/28/...",
    "https://banter-latte.com/2007/09/04/...",
    "https://banter-latte.com/2007/09/20/...",
    "https://banter-latte.com/2007/09/25/...",
    "https://banter-latte.com/2007/10/02/..."
]
~~~

I visit a few of these pages in my browser to determine which part of
the page I want to extract. I want to look closely enough to see what
I'm doing, but not *too* closely as to not spoil myself! Right clicking
the content in the browser and selecting "Inspect Element" (Firefox) or
"Inspect" (Chrome) pops up a pane to structurally navigate the page.
"View Page Source" would work, too, especially since this is static
content, but I find the developer pane easier to read. Plus it hides
most of the content, revealing only the structure.

The content is contained in a `div` with the class `entry-content`. I
can use a selector to isolate this element and extract its child `p`
elements. However, it's not quite so simple. Each chapter starts with a
bit of commentary that's not part of the book, and I don't want to
include in my extract. It's separated from the real content by an `hr`
element. There's also a footer below another `hr` element, likely put
there by someone who wasn't paying attention to the page structure. It's
not quite the shining example of semantic markup, but it's regular
enough I can manage.

~~~html
<body>
  <main class="site-main">
    <div class="entry-body">
      <div class="entry-content">
        <p>A little intro.</p>
        <p>Some more intro.</p>
        <hr/>
        <p>Actual book content.</p>
        <p>More content.</p>
        <hr/>
        <p>Footer navigation junk.</p>
      </div>
    </div>
  </main>
</body>
~~~

The next step is visiting each of these pages. I use `enumerate` since I
want the chapter numbers when inserting `h1` chapter elements. Pandoc
will use these to build the table of contents.

~~~py
for i, chapter in enumerate(chapters):
    # Construct h1 for the chapter
    header = doc.new_tag('h1')
    header.string = 'Chapter %d' % (i + 1,)
    body.append(header)
~~~

Next grab the page content using `urllib` and parse it with
BeautifulSoup. I'm using a selector to locate the `div` with the
book content.

~~~py
    # Load chapter content
    with urllib.request.urlopen(chapter) as url:
        page = BeautifulSoup(url)
    content = page.select('.entry-content')[0]
~~~

Finally I iterate over the child elements of the `div.entry-content`
element. I keep a running count of the `hr` element and only extract
content when we've seen exactly one `hr` element.

~~~py
    # Append content between hr elements
    hr_count = 0
    for child in content.children:
        if child.name == 'hr':
            hr_count += 1
        elif child.name == 'p' and hr_count == 1:
            child.attrs = {}
            if child.string == '#':
                body.append(doc.new_tag('hr'))
            else:
                body.append(child)
~~~

If it's a `p` element, I copy it into the output document, taking a
moment to strip away any attributes present on the `p` tag, since, for
some reason, some of these elements have old-fashioned alignment
attributes in the original content.

The original content also uses the text "`#`" by itself in a `p` to
separate sections rather than using the appropriate markup. Despite
being semantically incorrect, I'm thankful for this since more `hr`
elements would have complicated matters further. I convert these to the
correct markup for the final document.

Finally I pretty print the result:

~~~py
print(doc.prettify())
~~~

Alternatively I could pipe it through [tidy][tidy].

    $ python3 extract.py | tidy -indent -utf8 > output.html

A brief inspection with a browser indicates that everything seems to
have come out correctly. I won't know for sure, though, until I actually
read through the whole book. Finally I have Pandoc perform the
conversion.

    $ pandoc -t epub3 -o output.epub output.html

And that's it! It's ready to read offline in my e-book reader of
choice. The crude version of my script took around 15–20 minutes to
write and test, so I had an e-book conversion in under 30 minutes.
That's about as long as I was willing to spend to get it. Tidying the
script up for this article took a lot longer.

I don't have permission to share the resulting e-book, but I can share
my script so that you can generate your own, at least as long as it's
hosted at the same place with the same structure.

* [extract.py](/download/leather/extract.py){: .download}


[bs4]: https://www.crummy.com/software/BeautifulSoup/
[skewer]: /blog/2013/01/24/
[ts]: /blog/2015/09/03/
[df]: http://daviddfriedman.blogspot.com/2017/05/something-different-or-maybe-not.html
[il]: https://banter-latte.com/portfolio/interviewing-leather/
[pd]: http://pandoc.org/
[curl]: /blog/2016/06/16/
[tidy]: http://tidy.sourceforge.net/
[type]: /blog/2017/04/01/
