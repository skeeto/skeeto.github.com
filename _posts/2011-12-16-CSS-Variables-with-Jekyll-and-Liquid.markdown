---
title: CSS Variables with Jekyll and Liquid
layout: post
tags: [web]
uuid: e05bdf1a-f3d9-30b9-01f3-5dc7730aa678
---

CSS variables have been proposed
[a number](http://oocss.org/spec/css-variables.html)
[of times](http://disruptive-innovations.com/zoo/cssvariables/)
already, but, as far as I know, it has never been taken
seriously. Variables — *constants*, really, depending on the proposal
— would be useful in eliminating redundancy, because the same value
often appears multiple times across a consistent theme. The cascading
part of cascading stylesheets can deal with some of this, but not
all. For example,

    @variables color {
        border: #7fa;
    }

    article {
        box-shadow: var(color.border);
    }

    header {
        border: 1px solid var(color.border);
    }

Because the color has been defined in one place, adjusting the color
theme requires only one change. That's a big help to maintenance.

I recently investigated CSS variables, not so much to reduce
maintenance issues, but mainly because I wanted to have
user-selectable color themes. I wanted to use JavaScript to change the
variable values dynamically so I could modify the page style on the
fly. Since CSS variables are merely an idea at the moment, I went for
the next tool already available to me:
[Liquid](http://liquidmarkup.org/), the templating system used by
Jekyll. Jekyll essentially *is* Liquid, which is what makes it so
powerful. It continues to be my ideal blogging platform.

If you look in my site's source repository (not the build code hosted
here), you'll see my core stylesheet is an `_include` and looks like
this,

<pre>
code {
    border: 1px solid &#123;{ page.border }};
    background-color: &#123;{ page.backA }};
}

pre {
    border: 1px solid &#123;{ page.border }};
    background-color: &#123;{ page.backA }};
    padding: 3px;
    margin-left: 1em;
}

pre code {
    border: none;
    background-color: &#123;{ page.backA }};
}

blockquote {
    border: 1px dashed &#123;{ page.border }};
    background-color: &#123;{ page.backC }};
    padding: 0 0 0 0.5em;
}
</pre>

Those are Liquid variables. Each theme source file looks like this,

~~~yaml
---
backA: "#ecffdc"
backB: White
backC: WhiteSmoke
foreA: Black
foreB: SlateGray
border: Silver
links: Blue
---
~~~

That's just some YAML front-matter defining the theme's variables. For
my themes, I define three background colors, two foreground colors,
and the link color. For each theme, a full stylesheet is generated
from the stylesheet template above. To allow the user to select a
theme, I just use some JavaScript to select the proper stylesheet. You
can try this out with the theme-selector on the sidebar.

*Update December 2012*: I feel like themes weren't really adding much
to the blog so I removed them. However, the Liquid CSS variables do
remain because it makes maintenance simpler.
