benjeffrey.com
==============

## Requirements

- GHC
- Hakyll
- [Compass](http://compass-style.org/) for Zurb Foundation

### On Mac OS X:

```sh
brew install ghc rbenv ruby-build
cabal update
cabal install hakyll
gem install compass zurb-foundation
ghc --make site.hs
```

## Commands

```sh
ghc --make src/site.hs && src/site rebuild      # recompile executable and rebuild site
src/site build          # build changed parts of the site
src/site rebuild        # delete site and rebuild
src/site deploy         # run src/deploy.sh
```
