#! /bin/sh -eu
NPROC=$(nproc --all)
nix-shell --run "make -j$NPROC"
set -x
./cabal new-haddock all
./cabal new-build all
./cabal new-test all
