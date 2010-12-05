#!/bin/sh

# check that simplest of webmachine resources is accessible
curl -s -i http://localhost:8000/test

# get all data from cardA
curl -s -i http://localhost:8000/data/cardA/

# and from cardB
curl -s -i http://localhost:8000/data/cardB/

curl -s -i http://localhost:8000/data/cardC/

# get channel 2 from cardA
curl -s -i http://localhost:8000/data/cardA/channel2

# configure some legit stuff on cards A and B
curl -vvv -s -X POST -H "Content-Type: application/json" \
    -d '{"cardA":{"gain": 2, "mode":"differential", "channel1":{"gain":4}, "channel2":{"gain":8}}, "cardB":{"mode":"differential", "channel4":{"gain":4}}}' http://localhost:8000/config


# do a legit write request on card C
curl -vvv -s -X POST -H "Content-Type: application/json" \
    -d '{"cardC":{"channel1":1, "channel2":0, "channel19":1}}' http://localhost:8000/write

