# Sketch for further generalization

A positional game consists of:

- Elements of a finite set X ("The board")
- A family of subsets F ("The winning sets") which is a subset of Pow(X).
- Two players, `FP` (First Player, Maker) and `SP` (Second Player, Breaker)
- A criterion for winning the game

The players take turns, claiming previously unoccupied elements of X until all elements of X are claimed.

There are different winning criteria depending on the type of postitional game:

- **Strong positional** game (Maker-Maker): The winner is the first player to fully claim some element in F. If neither
  player has claimed some element in F when all elements in X are claimed the game is a draw.

- Weak positional game (**Maker-Breaker**): `FP` ("Maker") wins the game as soon as he fully claims a
  set in F (not necessarily first). If Maker has not won by the time all elemnets of X are claimed, 
  `SP` ("Breaker") wins (i.e. `SP` has claimed at least one element in every winning set). Draws are not possible. Every
  Strong positional game can be reformulated as a Maker-Breaker variant?

- **Avoider-Enforcer** positional game: This game has the exact opposite (misère) winning criteria of the
  corresponding Maker-Breaker game for the same X and F. I.e. `SP` ("Enforcer") wins if `FP` ("Avoider") fully
  claims an element of F ("losing sets"). Avoider wins if by the time all elements of X are claimed, he has not
  claimed an element of F.

- **Discrepancy game**: A hybrid between Maker-Breaker and Avoider-Enforcer. `FP`'s ("Balancer") aims to end up with the correct proportion of elements in element of F.

- **[Orientation games](https://www-sciencedirect-com.ezproxy.ub.gu.se/science/article/pii/S0012365X97002240)**: 

- **Waiter-Client game** (Picker-Chooser):


We can codify a positional game by the following properties:

- The "Board" (the set X) is represented by a graph

- Represent the elements of X ("positions") as either the set of edges, or the set of vertices in X.

- Define in what way elements in X are related (interconnected). E.g. as a Hexagon board, square board, complete graph, ...

- Define the "size" of X. The meaning of size may depend on how elements are related. E.g. size can be the side for a square square board, the number of vertices for a complete graph, ...

- Valid next moves (element in X to be potentially claimed) for either player.

- Winning criteria. Maybe the type of positional game can be used as a help when deciding who the winner is. It is desireable to
  deduce a winner as soon as soon as only one outcome is possible (assuming non-perfect players). E.g. to not let the game continue until
  all elements of X are claimed if it does not change the winner assuming non-perfect players.
  
  Ideally a set of general criteria (or small functions) should be figured out that could be reused when specifying a game. Some ideas: neigbouring positions of length n making up a geometric line (for Tic-Tac-Toe, Gomuko, ...), Some property on spanning trees (Gale, Shannon Switching Game, ...), Path existence (Hex), ..., Existence of Hamiltonian cycle, ...


## Classification of some games:

### Strong Positional

- [Arithmetic progression game](https://en.wikipedia.org/wiki/Arithmetic_progression_game) (The van der Waerden game)
- [Tic-Tac-Toe](https://en.wikipedia.org/wiki/Tic-tac-toe)
- [Connect6](https://en.wikipedia.org/wiki/Connect6)
- [Gomuko](https://en.wikipedia.org/wiki/Gomoku)
- [m,n,k-game](https://en.wikipedia.org/wiki/M,n,k-game)


### Maker-Breaker

- [Gale](https://en.wikipedia.org/wiki/Shannon_switching_game#Gale)
- [Hex](https://en.wikipedia.org/wiki/Hex_(board_game))
- [Havannah](https://en.wikipedia.org/wiki/Havannah) (but draws are possible?)
- [Shannon Switching Game](https://en.wikipedia.org/wiki/Shannon_switching_game)


### Avoider-Enforcer

- [Sim](https://en.wikipedia.org/wiki/Sim_(pencil_game))
- [Yavalath](https://de.wikipedia.org/wiki/Yavalath)(?)


## Alternative classification

### Connection games

- [Havannah](https://en.wikipedia.org/wiki/Havannah)
- [Hex](https://en.wikipedia.org/wiki/Hex_(board_game))
