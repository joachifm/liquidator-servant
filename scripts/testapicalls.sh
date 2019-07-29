#! /bin/sh

api=:3000/api/v1
httpopts="--all --follow --verify false"
curlopts="-L --insecure"

set -x

http $httpopts --json POST $api/transaction <<< '
{
  "transactionMoney": {
    "moneyAmount": 100
  },
  "transactionDate": "1970-01-01",
  "transactionCleared": false,
  "transactionClearedTime": null,
  "transactionFlagged": false,
  "transactionNotes": []
}
'

http $httpopts --json GET $api/transaction/1

http $httpopts --json PATCH $api/transaction/1 <<< '
{
  "transactionDiffSetCleared": true,
  "transactionDiffSetFlagged": false,
  "transactionDiffAddNote": "@income"
}
'

http $httpopts --json GET $api/transaction/1

curl $curlopts -X POST -H 'Authorization: Basic dGVzdGNsaWVudDpzZWNyZXQ=' -d 'grant_type=password&username=test&password=test' localhost:3000/oauth/token

curl $curlopts --insecure 'localhost:3000/secret?access_token=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJ1c2VyIjoiVlx1MDAxNcKbwoNUwoonbFPCu8KhwrYiLCJpYXQiOjE0NDQyNjI1NDMsImV4cCI6MTQ0NDI2MjU2M30.MldruS1PvZaRZIJR4legQaauQ3_DYKxxP2rFnD37Ip4'
