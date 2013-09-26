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
./site build
./site rebuild
./site deploy
```
