#! /bin/sh

set -eux

./cabal new-haddock all
./cabal new-build all
./cabal new-test all
