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

### Setup

The easies way to use Asterius is with their prebuilt Docker container, here is
how you do it:

1. Firstly you need to install Docker, just follow the instructions from
   [Docker docs](https://docs.docker.com/get-docker/).
2. Once Docker is up and running, run the Asterius container with the following
   command
   ```sh
   docker run -it -v $(pwd):/workspace -w /workspace terrorjack/asterius
   ```
   Where `$(pwd)` is the absolute path to this directory. Docker will mount this
   directory to the "workspace", where you can use all the Asterius commands.
3. The first time you use the container you must ["boot"](https://asterius.netlify.app/architecture.html#about-booting)
   Asterius, it is also a good idea to update cabal. Do that with these
   commands:
   ```sh
   ahc-boot && ahc-cabal new-update
   ```
   Together they take about 20 minutes, and then you ready.

### Building

Well up and running, you should use `ahc-cabal` instead of `cabal` to compile
the project, we've also have a flag for activating the WASM features.
Building the executable should look like this now:
```sh
ahc-cabal new-build exe:boardgame --flags="wasm"
```

But building targeting WASM doesn't actually get us anything useful. To
transform the output to `.wasm` and `.js` you can use `ahc-dist`. Run the
following command:
```sh
ahc-dist --browser --bundle \
	--input-exe path/to/boardgame \
	--output-directory /absolute/path/to/existing/out-directory
```
The `--input-exe` is the path to the output of building the project, it's
probably `dist-newstyle/build/x86_64-linux/ghc-8.8.4/boardgame-0.1.0.0/x/boardgame/opt/build/boardgame/boardgame`
from the root of the project.

In your output directory you'll now find `boardgame.wasm` and `boardgame.js`,
and an example HTML file `boardgame.html` showing how to use them. Open
`boardgame.html` in a web server (can't load WASM from a local file), open the
console in your web browser, and play around with the `window.boardgame` object.

If you wish to have a UI to play around with, you can mode the `boardgame.wasm`
and `boardgame.js` files over to the example of the [boardgame.js](https://github.com/Boardgame-DSL/boardgame.js)
repo.
