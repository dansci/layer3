#!/bin/sh

# check that simplest of webmachine resources is accessible
curl -s -i http://localhost:8000/test

# get all data from cardA
curl -s -i http://localhost:8000/data/cardA/

# get channel 2 from cardA
curl -s -i http://localhost:8000/data/cardA/channel2


curl -vvv -s -X POST -H "Content-Type: application/json" \
    -d '{"cardA":{"gain": 2, "mode":"single_ended", "channel1":{"gain":4}, "channel2":{"gain":8}}, "cardB":{"mode":"single_ended", "channel4":{"gain":4}}}' http://localhost:8000/config