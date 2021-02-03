# Boardgames

Kandidatarbete at [CTH](https://www.chalmers.se/) by [Jennifer Krogh](https://github.com/jenniferkrogh),
[Mattias Mattsson](https://github.com/matmat), [Emma Pettersson](https://github.com/emmouto),
[Simon Sundqvist](https://github.com/Zinfour), [Carl Wiede](https://github.com/carlwiede),
and [Mårten Åsberg](https://github.com/89netraM).

[Project description/proposal](https://www.chalmers.se/SiteCollectionDocuments/CSE/Kandidatprojekt2021/Datx02-21-06_Spr%C3%A5k_br%C3%A4dspel.pdf)
(in Swedish)

[Group page](https://chalmers.instructure.com/groups/69903/wiki)
(access restricted)

## Development

We use `cabal` for development.

The library is located in `./src`. During development
`cabal repl <path/to/file>` can be useful to test new features of the library.

A example executable is located in `./executable`. Use `cabal run boardgame` to
test it.

A test suite is located in `./tests`. Use `cabal test` to run it.
