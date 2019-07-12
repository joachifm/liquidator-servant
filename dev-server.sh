#! /bin/sh

exec ghcid -c 'ghci -fobject-code' -T main --restart='*.hs' app/serve.hs
