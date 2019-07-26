#! /bin/sh

umask 022

set -x
exec openssl req \
     -x509 \
     -newkey rsa:2048 \
     -sha256 \
     -days 3650 \
     -nodes \
     -keyout site.key \
     -out site.crt -subj /CN=Liquidator \
     -addext subjectAltName=DNS:example.com,DNS:example.net,IP:10.0.0.1
