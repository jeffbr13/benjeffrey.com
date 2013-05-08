-----
title: Please people, let's use IDs more
description: HyperText was built to enable deep linking between documents. So why not act like it?
date: 2013-05-05
-----

You remember how books or papers used to refer to specific parts of other
texts? For example, you might have to review the sections §3-4,
or be told to read pages pp.168-178. If even more specificity was
required, then perhaps you had to refer to paragraphs ¶¶4-5 on p.98.

But, for example, [this interview with Mailbox's Gentry
Underwood][interview] is a pretty standard document on the World Wide
Web. I wanted to link to one of Gentry's responses, where he talked about
the lack of folders in Mailbox:

> One of the things that we saw when we watched people using mail was
> that they would often file things in deeply nested folders, but when
> they went looking for something they would just search.
> <cite>Gentry Underwood, <a href="http://www.fastcolabs.com/3008886/open-company/mailboxs-gentry-underwood-what-hackers-should-know-about-design-thinking">interview with Fast Company</a> </cite>

But I can't link to that specific element. I have to tell you that it's
about 2/3 of the way down the page, or that it's the 15th question (I may
have miscounted).

**This is the web, in 2013 -- why can I not link to sections on most
webpages**? [Hyperlinks](http://en.wikipedia.org/wiki/Hyperlink) between
resources are a core feature of the <abbr title="HyperText">HT</abbr> in
<abbr title="HyperText Markup Language">HTML</abbr>. Not only are you
able to link to documents, but HTML anchors can also link to specific
items on webpages using `id` and `name` attributes.


Good Examples
-------------

* MDN
* ReadTheDocs, Haddock, or pretty much any generated code documentation

These often have the advantage of being arranged according to specific
entities, such as methods.




http://www.w3.org/TR/html401/struct/global.html#adef-id
https://developer.mozilla.org/en-US/docs/HTML/Global_attributes#attr-id
http://stackoverflow.com/questions/484719/html-anchors-with-name-or-id#answer-484781


* IDs for `section` or `h*` elements
* IDs for each paragraph `p` and other semantic elements at the top-level
    of a section
* Pretty links for each element, which don't encroach upon the reading
    experience. A good example is the Read the Docs output, with a
    TOC for each page, and a link appearing next to each section header
    on hover, although they try to be optimised for code documentation
    in particular.


Possible Solutions
------------------

### Implement IDs for `section` and `h` elements

The very least that you can do is implement IDs on your `<section>`
elements, or `h1`-`h5` if you're not using HTML5 yet. Then we can link
to the interes


You should also try to make these IDs human readable. I'm looking at
Wikipedia when I say this, along with all the other Mediawiki sites that
somehow seem to generate some of the least readable IDs ever.
Pandoc has a good set of rules for generating IDs for headers, and they
basically boil down to

* strip preceding numbers
* lowercase it
* replace whitespace with hyphens



<!-- links -->

[interview]: http://www.fastcolabs.com/3008886/open-company/mailboxs-gentry-underwood-what-hackers-should-know-about-design-thinking
