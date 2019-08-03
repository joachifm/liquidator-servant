#! /bin/sh
set -x
exec nix run nixpkgs.busybox -c httpd -f -p 3000 "${@}"
