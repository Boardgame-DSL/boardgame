#!/usr/bin/env bash

docker start $1
if (( $? != 0 ))
then
	echo "Could not start the docker container \"$1\"."
	exit 1
fi

exec 5>&1
cabalOut=$(docker exec $1 ahc-cabal new-build --flags="wasm" | tee >(cat - >&5))
regex="Linking ([^ ]*) \\.\\.\\."
if [[ "$cabalOut" =~ $regex ]]
then
	mkdir "$(pwd)/$2"
	docker exec $1 ahc-dist --browser --bundle --input-exe ${BASH_REMATCH[1]} --output-directory "/workspace/$2"
else
	echo "Failed while building. Could not find the output."
	exit 1
fi
