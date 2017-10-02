# crib-square

This is an implementation of crib square solitaire. There is currently only a
command-line interface, but adding other frontends would be very simple.

## Dependencies

* [CHICKEN Scheme](https://www.call-cc.org/) - tested on version 4.12.0

## Building and Running

To build the main executable, run `make`, this will create an executable `cs`
that can be run to play the game. `make test` will run the test suite.

## Goal

As you draw each card, place it somewhere in one of the 16 spaces in the 4x4
tableau. You cannot move a card once it has been placed. Once all 16 cards
have been played, one additional card is drawn, and the game is scored. Each
row and column of the tableau is scored as a hand along with the turn up to
make 8 5-card hands. Hands are scored according to crib rules:

* **Pairs** - Each pair is worth two points. A set of 3 is treated as 3 pairs,
  and so is worth 6 points, and similarly a set of 4 is treated as 6 pairs,
  and so is worth 12 points.

* **Runs** - A run of 3 or more cards is worth the number of cards in the run.

* **Flushes** - All 4 cards in a hand sharing the same suit is worth 4 points,
  or 5 if the turn-up matches as well. Note that having 3 of one suit in a
  hand and the turn-up matching is worth 0 points, not 4.

* **Jack** - The jack of the same suit as the turn-up is worth 1 point.

* **Fifteens** - Each unique set of cards in a hand that add up to 15 is worth
  2 points.

After the game is finished, the program will offer to explain why it scored
how it did, in case something is unclear.
