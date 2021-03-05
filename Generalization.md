Sketch for furhter generalization

A positional game consists of:

- Elements of a finite set X ("The board")
- A family of subsets F ("The winning sets") which is a subset of Pow(X).
- Two players, `FP` (First Player, Maker) and `SP` (Second Player, Breaker)
- A criterion for winning the game

The players take turns, claiming previously unoccupied elements of X until all elements of X are claimed.

There are different winning criteria depending on the type of postitional game:

- Strong positional game (Maker-Maker): The winner is the first player to fully claim some element in F. If neither
  player has claimed some element in F when all elements in X are claimed the game is a draw.

- Weak positional game (Maker-Breaker): `FP` ("Maker") wins the game as soon as he fully claims a
  set in F (not necessarily first). If Maker has not won by the time all elemnets of X are claimed, 
  `SP` ("Breaker") wins (i.e. `SP` has claimed at least one element in every winning set). Draws are not possible. Every
  Strong positional game can be reformulated as a Maker-Breaker variant?

- Avoider-Enforcer positional game: This game has the exact opposite (misère) winning criteria of the
  corresponding Maker-Breaker game for the same X and F. I.e. `SP` ("Enforcer") wins if `FP` ("Avoider") fully
  claims an element of F ("losing sets"). Avoider wins if by the time all elements of X are claimed, he has not
  claimed an element of F.


We can codify a positional game by the following properties:

- The "Board" (the set X) is represented by a graph

- Elements of X ("positions") are either the set of edges, or the set of vertices in X.

- The way in which elements are related (interconnected) in X. E.g. Hexagon board, square board, complete graph, ...

- The size of X. The meaning may depend on how elements are related. E.g. size is the side for a square board, but the number of vertices for a board consisting of a complete graph.

- Valid next moves (element in X to be claimed) for either player.

- Winning criteria. Maybe specify the type of game from above and use this as help when deciding who the winner is. It is desireable to
  deduce a winner as soon as soon as only one outcome is possible (assuming non-perfect players). E.g. not let the game continue until
  all elements of X are claimed.


Classifications of some games:

## Strong

Tic-Tac-Toe
Arithmetic progression game (The van der Waerden game)
Connect6
Gomuko
mnk-game


## Maker-Breaker

Gale
Hex
Havannah (but draws are possible?)
The Shannon Switching Game


## Avoider-Enforcer

Sim
Yavalath(?)




Alternative classification

## Connection games

Havannah
Hex
