---
title: Building a static blog with Hakyll & Zurb Foundation
description: How I rebuilt my website using Hakyll (the static site generator written in Haskell), and Zurb's Foundation framework.
tags: web-development
date: 2013-04-06
---

**Note:** Sequence shortened and some steps removed.

Prelude
-------

A few weeks ago, I found that [benjeffrey.com](http://benjeffrey.com) was
finally back up for sale, so I snatched it up before another registrar
decided to sit on it again.

But (I mused...) if I was going to move to this fancy new domain name
then maybe I ought to consider giving my old hand-written HTML a bit of
spit-and-polish? After all, it was the first webpage I'd ever really put
online, back when I was paying £10/annum for [a domain name and shared
hosting][fasthostingdirect]!) and now my web presence was looking
rather... meh. Certainly not the standard you'd expect from a student of
<abbr title="Computer Science">Informatics</abbr>!

I quite liked the fact that my old site could run from
[any old webserver](/posts/nginx), so I decided to find a trendy
static-site generator to run everything.

Unfortunately the [most popular option][Jekyll] is written in Ruby, a
language that I'm not too keen on, personally. And the set-up is hellish
if you don't know what you're doing (I don't). I ended up too exhausted
getting the basic [Jekyll][]/[Octopress][] site running on [GitHub
Pages][] to then learn how to configure or customize the damn thing.

However, I came across a cool little project by the name of [Hakyll][].
Shamelessly riffing off Jekyll's fame, Hakyll is instead written in
Haskell! Having spent last semester getting to grips with this language,
Hakyll seemed like a pretty good choice, if only as a learning
experience!

On the style side, I wanted the benefits of using a large framework, but thought
I'd experiment (i.e. *not* use [Bootstrap][]) for once.
[Foundation 4][Foundation] had just been released, and I found out that it used
Sass, which I thought would be fun.

So I settled on [a colour-scheme on ColourLovers][intellectual owl],
and [a font from Google Web Fonts][Vollkorn], and got to work!


Hakyll static site generator
----------------------------

### Install

The [Hakyll tutorials][] are plenty to get you started, and
the [Hakyll Haddock documentation][Hakyll reference] is invaluable if you
want to *really* customize your site.

**Requirements:** GHC and [Cabal][], Haskell's package-management system.

After you've [installed Hakyll][][^hakyll-init], initialise it in a bare
folder (since it generates a whole example site for you to play with).
Delete it before you commit to a repo...

```bash
cabal install hakyll [website directory]
cd [website directory]
# Give the Hakyll script a more descriptive name
mv site.hs hakyll.hs
ghc --make hakyll.hs

# Launch a preview server, and view the site (from another terminal)
./hakyll preview
sensible-browser "http://localhost:8000"
```

### Configure Hakyll

The `hakyll.hs`/`site.hs` source file compiles an executable which
is itself the static site generator.





<!-- footnotes -->
[^hakyll-init]: If you can't run the `hakyll-init` command, you may
    need to add the local Cabal binary folder to your shell's `$PATH`
    variable with the following lines in your shell config:

    ```bash
    ## Cabal Haskell package management scripts
    PATH=$PATH:$HOME/.cabal/bin
    ```

<!-- links -->
[fasthostingdirect]: http://www.fasthostingdirect.co.uk/
[Jekyll]: http://jekyllrb.com/
[Octopress]: http://octopress.org/
[GitHub Pages]: http://pages.github.com/
[Hakyll]: http://jaspervdj.be/hakyll/
[Hakyll tutorials]: http://jaspervdj.be/hakyll/tutorials.html
[Hakyll reference]: http://jaspervdj.be/hakyll/reference/index.html
[Bootstrap]: http://twitter.github.io/bootstrap/
[Foundation]: http://foundation.zurb.com/
[intellectual owl]: http://www.colourlovers.com/palette/1464688/Intellectual_Owl
[Vollkorn]: http://www.google.com/fonts/specimen/Vollkorn
[Cabal]: http://www.haskell.org/cabal/
[installed Hakyll]: http://jaspervdj.be/hakyll/tutorials/01-installation.html
