---
title: Building a static blog with Hakyll & Zurb Foundation
description: Rebuilding my website using the Hakyll static site generator, and Zurb's Foundation framework.
tags: web-development
date: 2013-04-06
---

**Note:** Sequence shortened and some steps removed.

Prelude
-------

A few weeks ago, I found that [benjeffrey.com][] was
finally back up for sale, so I snatched it up before another registrar
decided to sit on it again.

But (I mused...) if I was going to move to this fancy new domain name
then maybe I ought to consider giving my old hand-written HTML a bit of
spit-and-polish? After all, it was the first webpage I'd ever really put
online (back when I was paying £10/annum for [a domain name and shared
hosting][fasthostingdirect]) and now my web presence was looking
rather... meh. Certainly not the standard you'd expect from a student of
<abbr title="Computer Science">Informatics</abbr>!

So my new website for [benjeffrey.com][] is built and deployed (as of
2013-04) through a combination of:

* Hakyll
* Foundation 4
* Compass & Sass
* Nginx
* Google Web Fonts
* Git
* SSH


Static Site Generation with Hakyll
----------------------------------

I quite liked the fact that my old site could run from
[any old webserver](/posts/nginx), so I decided to find a trendy
static-site generator to run everything.

Unfortunately [Jekyll][], the most popular static-site generator, is
written in Ruby, a language that I'm not too keen on. And the Ruby
toolchain's set-up is hellish if you don't know what you're doing -- and
I don't. I had too little patience left after getting the basic
[Jekyll][]/[Octopress][] site running on [GitHub Pages][] to then learn
how to configure or customize the damned thing!

Eventually though, I came across a cool little project by the name of
[Hakyll][]. Shamelessly riffing off Jekyll's fame, Hakyll performs the same
function, only it's written in Haskell! Having spent last semester getting
to grips with this language, Hakyll seemed like a pretty good choice, if
only as a learning experience.


### Installing Hakyll

The [Hakyll tutorials][] are plenty to get you started, and
the [Hakyll Haddock documentation][Hakyll reference] is invaluable if you
want to *really* customize it.

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

### Custom Hakyll rules




Site Design
-----------

On the style side, I wanted the benefits of using a large framework, but
thought I'd experiment a bit -- i.e. *not* use [Bootstrap][]. A new
version of the [Zurb Foundation framework][Foundation] had just been
released, and since it uses Sass, I thought it might be fun to try out.
At least I wouldn't end up with too many presentational classes stuck in
my HTML.

Finally, I settled on [a colour-scheme from ColourLovers][intellectual owl],
and [a font from Google Web Fonts][Vollkorn], and got to work!

### Layout

After playing about with a single-column layout for a while, I just
couldn't get happy with the vertical space that a navigation flagstaff
took up before you even got to the oversized page header.

These days, when many laptops are *still* shipping with displays only
768px tall, you can't afford the vertical space needed for a true
single-column layout.

I had an inkling that I might repeat what I did on the
[INF-YT site][INF-YT], where the page title took up the entire
left-hand-side of the page.
However, I really liked [Dayle Rees's site][daylerees] (after I came
across it while looking for his excellent [Sublime Text colour
schemes][ST colour schemes]). And so you can probably see that I pretty-
much ripped off his whole logo/face/nav sidebar-thing here! Great artists
steal, and all that jazz...


### Colour-Scheme


### Typography

* font-combinator
* http://hellohappy.org/beautiful-web-type/
* http://bueltge.de/free-web-font-combinations/


### Finishing Touches

* background
* logo
* getting `pre` to work properly
* `humans.txt` -- this really comes along with Foundation, but it's good
    to update it to what you actually used
*


Summary
-------

A tentative project like this only really comes together at the end,
when putting on all the finishing "touches" to it. These are what make
the






<!-- footnotes -->

[^hakyll-init]: If you can't run the `hakyll-init` command, you may
    need to add the local Cabal binary folder to your shell's `$PATH`
    variable with the following lines in your shell config:

    ```bash
    ### ~/.profile

    # Cabal executables
    PATH=$PATH:$HOME/.cabal/bin
    ```


<!-- links -->

[benjeffrey.com]: http://benjeffrey.com
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
[INF-YT]: http://inf-yt.org.uk/
[daylerees]: http://daylerees.com/
[ST colour schemes]: https://github.com/daylerees/colour-schemes
