#! /bin/sh

url=:3000
httpopts="--json"
call="http $httpopts"

$call $url/secret
