#!/usr/bin/env pwsh

param (
	[Parameter(Position = 0, Mandatory = $true)]
	[String]
	$containerName
)

docker run -id -v "$($pwd):/workspace" -w /workspace --name "$containerName" terrorjack/asterius
docker exec $containerName ahc-cabal new-update
