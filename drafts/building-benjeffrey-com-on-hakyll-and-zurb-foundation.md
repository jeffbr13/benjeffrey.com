---
title: Building benjeffrey.com on Hakyll & Zurb Foundation
description: I now use Hakyll, a static-site generator, to build and deploy my website, while Zurb Foundation is behind the front-end.
tags: web-development
date: 2013-04-06
---

A few weeks ago I found [benjeffrey.com][] up for sale again,
after many years of it being sat on by a registrar. So, with all due haste, I
snatched it up, set up a redirect to [my old domain][benjeffrey.net], and
began to consider all the wonderful things I wanted on my shiny-new dot-com,
if I was going to rebuild my website from scratch.


After all, [benjeffrey.net][] was the first webpage I'd ever really put
online (back when I was paying £10/annum for [a domain name and shared
hosting][fasthostingdirect]) and now my web presence was looking
rather... meh. Certainly not the standard you'd expect from a student of
<abbr title="Computer Science">Informatics</abbr>!


I settled on the following requirements for my new website:

* A static-site generator would build all the content, because <del>that's what all
    the cool kids are doing</del> I like being able to host my content from
    any old webserver
* I wanted to use Zurb's [Foundation][] framework for the front-end,
    as I didn't want to litter my pages with Bootstrap classes.
* A funky colour-scheme from [COLOURlovers][], rather than using the
    framework defaults
* Unintrusive but better-than-default font selection


Static Site Generation with Hakyll
----------------------------------

I came across a cool little project by the name of
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


tl;dr
-----

benjeffrey.com][] is built and deployed (as of
2013-04) through a combination of:

* Hakyll
* Foundation 4
* Compass & Sass
* Nginx
* Google Web Fonts
* Git
* SSH




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
[benjeffrey.net]: http://benjeffrey.net
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
[COLOURlovers]: http://www.colourlovers.com/
