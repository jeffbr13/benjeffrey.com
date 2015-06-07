benjeffrey.com
==============

```sh
cabal update
cabal sandbox init
cabal install --only-dependencies
ghc --make site.hs
```

## Commands

```sh
cabal build                                       # compile executable
dist/build/benjeffrey-com/benjeffrey-com build    # build site
dist/build/benjeffrey-com/benjeffrey-com rebuild  # delete site and rebuild
dist/build/benjeffrey-com/benjeffrey-com deploy   # run src/deploy.sh
```
