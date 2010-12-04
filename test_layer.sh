#!/bin/sh

curl -s -i http://localhost:8000/test

curl -s -i http://localhost:8000/data/cardA/

curl -s -i http://localhost:8000/data/cardA/channel2

curl -vvv -s -X POST -H "Content-Type: application/json" \
    -d '{"cardA":{"mode":"differential", "channel1":{"gain":2}, "channel2":{"gain":8}}, "cardB":{"mode":"single-ended", "channel4":{"gain":4}}}' http://localhost:8000/config