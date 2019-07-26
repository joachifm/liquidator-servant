#! /bin/sh
exec httperf --server=localhost --port=3000 --num-conns=10000
