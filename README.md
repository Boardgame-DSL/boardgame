# Boardgames

Kandidatarbete at [CTH](https://www.chalmers.se/) by [Jennifer Krogh](https://github.com/jenniferkrogh),
[Mattias Mattsson](https://github.com/matmat), [Emma Pettersson](https://github.com/emmouto),
[Simon Sundqvist](https://github.com/Zinfour), [Carl Wiede](https://github.com/carlwiede),
and [Mårten Åsberg](https://github.com/89netraM).

[Project description/proposal](https://www.chalmers.se/SiteCollectionDocuments/CSE/Kandidatprojekt2021/Datx02-21-06_Spr%C3%A5k_br%C3%A4dspel.pdf)
(in Swedish)

[Group page](https://chalmers.instructure.com/groups/69903/wiki)
(access restricted)

## Description

A library with the basis for modeling and playing boardgames. Comes with
built-in functions for playing games through a web interface (requires WASM
compilation).

## Development

We use `cabal` for development.

The library is located in `./src`. During development
`cabal repl <path/to/file>` can be useful to test new features of the library.

A example executable is located in `./executable`. Use `cabal run boardgame` to
test it.

A test suite is located in `./tests`. Use `cabal test` to run it.

## UI Development

The UI side of this project is targeting the web by compiling the Haskell code
to WASM with [Asterius](https://github.com/tweag/asterius/).

The easiest way to use Asterius is with their prebuilt Docker container. We have
two shell scripts that interact with it.

First install docker on your machine by following [their tutorial](https://docs.docker.com/get-started/).

Then run the `setup-ahc` script (choose appropriate version for your machine).
This script takes one argument, and creates a new docker container with the
provided name.

Then, to build the code to WASM use the second script `wasm-build`. It takes two
arguments, the name of the docker container you created previously and a
relative path where to place the output.

If you want to learn more about how `ahc` and the docker containers work, read
about it in [Asterius documentation](https://asterius.netlify.app/).
