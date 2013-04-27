-----
title: Styling Pandoc's Syntax Highlighting with CSS
description: Pandoc's syntax-highlighting is pretty boring out of the box, so I wrote some CSS using the Solarized colour-scheme.
date: 2013-04-24
-----

[John Macfarlane's Pandoc][pandoc] is an amazingly useful tool for
creating and converting documents. One of the best things about it is the
extended Markdown syntax, especially the [GitHub style][gh-syntax]
[code- block syntax-highlighting][code-blocks], which allows you to mark
up code inside a Markdown file like thus

    ```bash
    cd some_dir
    grep "some text"
    # etc...
    ```

and get out:

```bash
cd some_dir
grep "some text"
# etc...
```

However, the default syntax-highlighting styles leave much to be desired,
and unless you dig deep into [Pandoc's Haddock docs][pandoc-docs] you
won't find any information on the internet telling you what the generated
markup classes (`.kw`, `.co`, `.ot`, etc.) mean, or how you can customize them.


The `highlighting-kate` Package
-------------------------------

It turns out that Pandoc relies on another one of John Macfarlane's
creations to mark up code syntax, the [`highlighting-kate` package][hk],
which is

> a syntax highlighting library with support for nearly one hundred
> languages. The syntax parsers are automatically generated from Kate
> syntax descriptions (http://kate-editor.org/), so any syntax supported
> by Kate can be added. <cite>[`highlighting-kate` package description][hk]</cite>

So it turns out that the syntax tokens are based on definitions provided
by the KDE text editor, [Kate][]! According to `highlighting-kate`'s
[HTML formatter documentation][hk-html], the classes used to mark-up
syntax map to the code syntax tokens as tabulated below:

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

These classifications are (sort of)explained in an old article on the
Kate site about [writing syntax-highlighting files][kate-syntax].

It also turns out that a few extra styles are [included][hk-styles] with
the package, if you want to extract and use them!


a Solarized Light colour-scheme
-------------------------------

Since I wanted something a little more personal, I thought I might
reimplement one of [Ethan Schoover's Solarized colour-schemes][solarized]
for Pandoc/`highlighting-kate`'s syntax markup, as the closest versions
only exist for [Google Code and Codemirror][css-solarized]. It's not
perfect, as the Kate syntax-highlighting, er, syntax leaves much to be
desired. Nevertheless, it's more than adequate for web viewing!

A standalone version of my Solarized Light colour-scheme CSS is below:

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

The actual code in use here is written as a Sass mixin -- it's available
[on GitHub][sass-mixin] if you want to see/use it.



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
