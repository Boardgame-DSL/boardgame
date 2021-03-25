#!/usr/bin/env bash

docker run -id -v $(pwd):/workspace -w /workspace --name "$1" terrorjack/asterius
docker exec $1 ahc-cabal new-update
