#! /bin/sh

url=localhost:3000
curlopts="-L --insecure -i"
httpopts="--all --follow --verify false"

set -x

http $httpopts --json POST $url/token <<< '
{
  "user": {
    "user_name": "Mary420",
    "password": "hunter2"
  }
}
'

http $httpopts --json GET $url/secret

exit 0

# Authorization: Basic <credentials>
#
# where credentials is the base64 encoding of id and password joined by a
# single colon (:).

curl $curlopts \
     -X POST \
     -H 'Authorization: Basic dGVzdGNsaWVudDpzZWNyZXQ=' \
     -d 'grant_type=password' \
     $url/token
