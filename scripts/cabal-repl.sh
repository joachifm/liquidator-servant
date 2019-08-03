#! /bin/sh -eux
nix-shell --run 'make -B'
ulimit -v 1073741824
./cabal new-repl ${1:-'liquidator-redux'}
