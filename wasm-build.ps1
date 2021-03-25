#!/usr/bin/env pwsh

param (
	[Parameter(Position = 0, Mandatory = $true)]
	[String]
	$containerName,
	[Parameter(Position = 1, Mandatory = $true)]
	[String]
	$outDirectory
)

docker start $containerName
if (-not $?) {
	Write-Output "Could not start the docker container `"$containerName`""
	exit 1
}

docker exec $containerName ahc-cabal new-build --flags="wasm" | Tee-Object -Variable cabalOut
if ("$cabalOut" -match "Linking ([^ ]*) \.\.\.") {
	New-Item -Path $outDirectory -ItemType Directory -ErrorAction SilentlyContinue > $null
	docker exec $containerName ahc-dist --browser --bundle --input-exe $matches[1] --output-directory "/workspace/$outDirectory"
	exit 0
}
else {
	Write-Output "Failed while building. Could not find the output."
	exit 1
}
