---
title: Building a static blog with Hakyll and Compass
description: Rebuilding my website from scratch, using Hakyll and Foundation
date: 2013-04-06
---

Hakyll is a static site generator (like [Jekyll][]) written in Haskell.

Install and set up Hakyll in the root of your website folder:

```bash
cabal install hakyll
hakyll-init [site folder]
```

Next, give the Hakyll script a more descriptive name:

```bash
mv site.hs generate-static-site.hs
```


* move `site.hs` to `hakyll.hs`
* importing old content and notes



<!-- footnotes -->
[^hakyll-init]: If you can't run `hakyll-init`, add Cabal's binaries
    to your shell's `$PATH` variable by adding the following lines to your
    shell config:

    ```bash
    ## Cabal Haskell package management scripts
    PATH=$PATH:$HOME/.cabal/bin
    ```

<!-- links -->
[install Hakyll]:
