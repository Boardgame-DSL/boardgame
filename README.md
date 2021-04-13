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

This branch does not run. It is only meant to be used for building the
documentation.

Haddock can not build the documentation without first compiling the source it
describes. And the regular Cabal can not compile the WASM related functions.
Therefore, all WASM related functions are set to `undefined` in this branch.
This is so that we can compile them, and build their documentation.

```sh
cabal new-haddock
```
