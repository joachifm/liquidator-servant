#! /bin/sh -eux
./cabal new-haddock all
./cabal new-build all
./cabal new-test all
