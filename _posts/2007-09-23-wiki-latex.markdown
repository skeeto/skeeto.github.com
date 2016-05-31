---
title: Converting MediaWiki Markup to LaTeX
layout: post
tags: [perl]
uuid: 6c418044-7097-3eb2-3910-9724a375c0e6
---

*Update: Don't use this. Use [Pandoc][pandoc] instead!*

Today I had a large document written in [MediaWiki][mediawiki] markup.
The document was a simple one consisting only of paragraphs, all
possible heading levels, and flat lists. There were no mathematical
expressions or images — nothing fancy whatsoever. I wanted this
document available in LaTeX markup so that I could have a nice,
professional looking printout.

After some brief searching I couldn't find a practical, simple script
to convert the document to LaTeX markup (I didn't look very
hard). Everything I found wanted to do the conversion the opposite way
that was needed. It ended up that writing and debugging my own Perl
script to do the job only took me about 30 minutes.

 * [/download/wiki2latex.perl](/download/wiki2latex.perl)

Here are the main guts to give you the gist of it,

~~~pl
while (<>) {
    # Sections
    s/======([^=]+)======/\\subparagraph{$1}/g;
    s/=====([^=]+)=====/\\paragraph{$1}/g;
    s/====([^=]+)====/\\subsubsection{$1}/g;
    s/===([^=]+)===/\\subsection{$1}/g;
    s/==([^=]+)==/\\section{$1}/g;

    # Special characters
    s/([&amp;#_])/\\$1/g;

    # Lists
    if (m/^\* / &amp;&amp; !$listmode) {
        print "\\begin{itemize}\n";
        $listmode = 1;
    }
    if (!m/^\* / &amp;&amp; $listmode) {
        print "\\end{itemize}\n";
        $listmode = 0;
    }
    s/^\* /\\item{} /;

    print;
}
~~~

Here is some sample input markup and output,

* [/download/wiki-sample.wiki](/download/wiki-sample.wiki)
* [/download/wiki-sample.tex](/download/wiki-sample.tex)
* [/download/wiki-sample.pdf](/download/wiki-sample.pdf)

The script will write out all the header and footer markup (including
title page and table of contents, which is what I needed) so that the
output can go right into LaTeX for processing. The script is *very*
far from being complete in terms of a "MediaWiki to LaTeX converter",
and I have no intention on making it any more complete either. It does
only what I needed it to do: handle a few special characters, convert
headings, and create lists. I am providing it here in case someone
finds it useful or interesting. Perhaps it may serve as a stepping
stone for creating something more complete.


[pandoc]: http://johnmacfarlane.net/pandoc/
[mediawiki]: http://www.mediawiki.org/
