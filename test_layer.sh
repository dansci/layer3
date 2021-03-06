#!/bin/sh

# check that simplest of webmachine resources is accessible
curl -s -i http://localhost:8000/test

# get all data from cardA
curl -s -i http://localhost:8000/data/cardA/

# and from cardB
curl -s -i http://localhost:8000/data/cardB/

# and from cardC
curl -s -i http://localhost:8000/data/cardC/

# and from a nonexistent card
curl -s -i http://localhost:8000/data/cardE/

# get config data
curl -s -i http://localhost:8000/config/cardA/

# configure some legit stuff on cards A and B
curl -vvv -s -X POST -H "Content-Type: application/json" \
    -d '{"cardA":{"gain": 4, "mode":"differential", "channel1":{"gain":4}, "channel2":{"gain":8}}}' http://localhost:8000/config

# configure illegit stuff...
curl -vvv -s -X POST -H "Content-Type: application/json" \
    -d '{"cardA":{"gain": 3, "mode":"differential", "channel1":{"gain":4}, "channel2":{"gain":8}, "channel3":{"gain":3}}}' http://localhost:8000/config


# malformed json request...
curl -vvv -s -X POST -H "Content-Type: application/json" \
    -d '{{"cardA":{"gain": 2, "mode":"differential", "channel1":{"gain":4}, "channel2":{"gain":8}}, "cardB":{"mode":"differential", "channel4":{"gain":4}}}' http://localhost:8000/config

# do a legit write request on card C
curl -vvv -s -X POST -H "Content-Type: application/json" \
    -d '{"cardC":{"channel1":1, "channel2":0, "channel19":1}}' http://localhost:8000/write

# illegit write request
# FIXME get writes to return errors too...
curl -vvv -s -X POST -H "Content-Type: application/json" \
    -d '{"cardC":{"channel1":2, "channel2":0, "channel19":1}}' http://localhost:8000/write
