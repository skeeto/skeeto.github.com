#!/usr/bin/env python3

# Interviewing Leather e-book creation script
#
# * https://banter-latte.com/portfolio/interviewing-leather/
# * https://nullprogram.com/blog/2017/05/15/

import urllib
from bs4 import BeautifulSoup, Doctype

chapters = [
    "https://banter-latte.com/2007/06/26/interviewing-leather-part-one/",
    "https://banter-latte.com/2007/07/03/interviewing-leather-part-two/",
    "https://banter-latte.com/2007/07/10/interviewing-leather-part-three/",
    "https://banter-latte.com/2007/07/17/interviewing-leather-part-four/",
    "https://banter-latte.com/2007/07/24/interviewing-leather-part-five/",
    "https://banter-latte.com/2007/07/31/interviewing-leather-part-six/",
    "https://banter-latte.com/2007/08/07/interviewing-leather-part-seven/",
    "https://banter-latte.com/2007/08/14/interviewing-leather-part-eight/",
    "https://banter-latte.com/2007/08/21/interviewing-leather-part-nine/",
    "https://banter-latte.com/2007/08/28/interviewing-leather-part-ten/",
    "https://banter-latte.com/2007/09/04/interviewing-leather-part-eleven/",
    "https://banter-latte.com/2007/09/20/interviewing-leather-part-twelve/",
    "https://banter-latte.com/2007/09/25/interviewing-leather-part-thirteen/",
    "https://banter-latte.com/2007/10/02/interviewing-leather-part-fourteen/"
]

# Construct HTML skeleton
doc = BeautifulSoup()
doc.append(Doctype('html'))
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

# Gather each chapter's content
for i, chapter in enumerate(chapters):
    # Construct h1 for the chapter
    header = doc.new_tag('h1')
    header.string = 'Chapter %d' % (i + 1,)
    body.append(header)

    # Load chapter content
    with urllib.request.urlopen(chapter) as url:
        page = BeautifulSoup(url)
    content = page.select('.entry-content')[0]

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

# Output final document
print(doc.prettify())
