-----
title: Pandoc Syntax Highlighting with CSS
description: Improving Pandoc's syntax-highlighting with Solarized and some CSS.
date: 2013-04-24
-----

[John Macfarlane's Pandoc][pandoc] is an awesome tool for writing
documents. One of my favourite features is its extended Markdown syntax;
especially the [GitHub][gh-syntax]-style
[fenced code-blocks][code-blocks],
which let you write code inside a Markdown file like this:

    ```bash
    cd some_dir
    grep "some text"
    # etc...
    ```

and get out this:

```bash
cd some_dir
grep "some text"
# etc...
```

However, the [stylesheet provided with Pandoc][pandoc css] isn't winning
any design awards, so I decided to write one which would decorate
Pandoc's output with something [more well-known][solarized]!


The `highlighting-kate` Package
-------------------------------

Unless you dig into Pandoc's [Haddock documentation][pandoc-docs], you
won't find much information on the internet telling you what the
generated markup classes (`.kw`, `.co`, `.ot`, etc.) mean, or how you can
customize them.

It turns out that Pandoc relies on another one of John Macfarlane's
creations in order to mark up code syntax, the [`highlighting-kate`
package][hk],

> a syntax highlighting library with support for nearly one hundred
> languages. The syntax parsers are automatically generated from Kate
> syntax descriptions (http://kate-editor.org/), so any syntax supported
> by Kate can be added. <cite>[`highlighting-kate` package description][hk]</cite>

So it turns out that the syntax tokens are based on definitions provided
by the KDE text editor, [Kate][].

According to the documentation for `highlighting-kate`'s
[HTML Formatters][hk-html], the classes used to mark-up
syntax tokens are as follows:

span class          code token
---------------     -----------------------
`.kw`               Keyword
`.dt`               DataType
`.dv`               DecVal (decimal values)
`.bn`               BaseN
`.fl`               Float
`.ch`               Char
`.st`               String
`.co`               Comment
`.ot`               OtherToken
`.at`               AlertToken
`.fu`               Function
`.re`               RegionMarker
`.er`               ErrorTok

These classifications are (sort of) explained in an old HOWTO on the
Kate site for [writing syntax-highlighting files][kate-syntax].

It also turns out that a few extra styles are [included][hk-styles] with
the `kate-highlighting` package, if you want to extract and use them!


a Solarized Light colour-scheme for Pandoc
------------------------------------------

Since I wanted something a little more distinctive, I decided to
reimplement one of [Ethan Schoover's Solarized colour-schemes][solarized]
for the Pandoc/`highlighting-kate`'s markup, as I could only find
versions for [Google Code and Codemirror][css-solarized]. It's not
perfect, as Kate's syntax-highlighting, er, syntax isn't very expansive,
but it's more than adequate for web viewing!

This is the CSS for putting Pandoc's code-blocks into Solarized Light:

```css
pre {
    background-color: #FDF6E3
}

// KeyWordTok
.sourceCode .kw { color: #268BD2; }
// DataTypeTok
.sourceCode .dt { color: #268BD2; }

// DecValTok (decimal value), BaseNTok, FloatTok
.sourceCode .dv, .sourceCode .bn, .sourceCode .fl { color: #D33682; }
// CharTok
.sourceCode .ch { color: #DC322F; }
// StringTok
.sourceCode .st { color: #2AA198; }
// CommentTok
.sourceCode .co { color: #93A1A1; }
// OtherTok
.sourceCode .ot { color: #A57800; }
// AlertTok
.sourceCode .al { color: #CB4B16; font-weight: bold; }
// FunctionTok
.sourceCode .fu { color: #268BD2; }
// RegionMarkerTok
.sourceCode .re { }
// ErrorTok
.sourceCode .er { color: #D30102; font-weight: bold; }
```

The actual code in use on [benjeffrey.com](/) is written [as a Sass mixin
][sass-mixin].



<!-- links -->

[pandoc]: http://www.johnmacfarlane.net/pandoc/index.html
[code-blocks]: http://www.johnmacfarlane.net/pandoc/README.html#fenced-code-blocks
[gh-syntax]: https://help.github.com/articles/github-flavored-markdown#syntax-highlighting
[hk]: http://hackage.haskell.org/package/highlighting-kate
[pandoc-docs]: http://hackage.haskell.org/package/pandoc
[Kate]: http://kate-editor.org/
[hk-html]: http://hackage.haskell.org/packages/archive/highlighting-kate/0.5.3.8/doc/html/Text-Highlighting-Kate-Format-HTML.html
[hk-styles]: http://hackage.haskell.org/packages/archive/highlighting-kate/0.5.3.8/doc/html/Text-Highlighting-Kate-Styles.html
[kate-syntax]: http://kate-editor.org/2005/03/24/writing-a-syntax-highlighting-file/
[solarized]: http://ethanschoonover.com/solarized
[css-solarized]: http://css-tricks.com/snippets/css/solarized-theme-for-codemirror-and-prettify/
[sass-mixin]: https://github.com/jeffbr13/benjeffrey.com/blob/master/scss/_syntax-highlighting-solarized-light.scss
[pandoc css]: http://jaspervdj.be/hakyll/tutorials/faq.html#does-hakyll-support-syntax-highlighting
