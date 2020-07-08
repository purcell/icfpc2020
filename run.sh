#!/bin/bash

set -e

SERVER_URL=$1
PLAYER_KEY=$2

echo "ServerUrl: $SERVER_URL; PlayerKey: $PLAYER_KEY"
curl -s "$SERVER_URL?playerKey=$PLAYER_KEY"
