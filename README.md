# Continuous go

Long enough have we suffered under the constraints imposed upon us by the grid of intersections. Down with the grid of intersections!

## Reasoning behind this

None. This is a terrible idea.

### Inspiration

The idea might be bad, but it is not even original.

- [AnalogChess](https://github.com/ehulinsky/AnalogChess), [play online](https://replit.com/@EricMiller8/AnalogChess-fixed-icons)
- [Not Tetris 2](https://stabyourself.net/nottetris2/)

## Rules

1. You know [how to play go](https://online-go.com/learn-to-play-go/). Exceptions below.
2. Go is played on an empty square, called a board. It may have lines drawn on it who cares about those.
3. **Stones may not overlap.**
4. A point on the board where a stone would fit is called a spot.
5. Spots whose centres are closer than **the stone diameter times the square root of two** are *considered adjacent* and, when occupied by friendly stones, are shown connected by a line.
6. A **liberty is an adjacent spot where a stone would fit**.
7. When a stone or a group of connected stones has no liberties, it is captured.
7. Ko: one may not capture just one stone if that stone was played on the previous move and captured just one stone. No positional superko fuck off.

## TODO

- [ ] Ask for confirmation before placing a move, esp on mobile
- [ ] Drag from stone to place a stone as far as possible and still connected
- [ ] Scoring
- [ ] Enforce ko
