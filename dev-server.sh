#! /bin/sh

exec nix-shell ./shell.nix --run "ghcid -T main --restart=*.hs serve.hs"
