# ContinuGo rules

1. ContinuGo is played on a square board of side `L = 1000`, by two players called Black and White.
2. A **stone** is a disc of integer radius `r` and diameter `d = 2r`.
3. A **spot** is a point with integer coordinates, each between `r` and `L-r`, inclusive, whose distance from every stone's centre is at least `d`.
4. Two points are **adjacent** if their Euclidean distance is strictly less than `d√2`. Two stones are adjacent if their centres are.
5. A **group** consists of all same-colour stones reachable from one another through adjacency.
6. A **liberty** of a group is a spot adjacent to the centre of at least one stone in that group.
7. **Capturing** a group removes its stones from the board.
8. Starting with an empty board, the players alternate turns, starting with Black.
9. A turn is either a pass or a legal placement.
10. A placement consists of placing a stone of one's colour at a spot, capturing the opponent's groups without liberties simultaneously, then doing the same for one's own groups. It is legal if the new stone survives and ko permits it.
11. **Ko:** a single-stone capture is forbidden if the captured stone was played on the preceding turn and captured exactly one stone. (Note: This might not be necessary. TBD!)
12. The game ends after two consecutive passes.
13. Every board point belongs to its nearest stone's colour.
14. Whoever owns more area wins.

## Comments

1. For an `N×N` equivalent, `r` is `L / (2N)` rounded down: `55` for `9×9`, `38` for `13×13`, and `26` for `19×19`.
2. For handicap games, the weaker player, taking black, may be given an _n stone handicap_; these are n consecutive moves played before the first white move.
3. By prior agreement, for games between equals, a fixed amount can be added to white's final score. This is called _komi_.
