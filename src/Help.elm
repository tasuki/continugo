module Help exposing (..)

import Html
import Markdown


help : Html.Html msg
help =
    Markdown.toHtml [] """
## ContinuGo

Long have we suffered under the tyranny of the grid of intersections. Down with the grid!

### Rules

1. You know [how to play go](https://online-go.com/learn-to-play-go/). Here are the exceptions and clarifications.
2. Go is played on an empty square, called a board. Lines may be drawn on it who cares about those.
3. Stones may not overlap.
4. A point on the board where a stone can fit is called a spot.
5. Spots whose centres are **closer than the stone diameter times the square root of two are considered adjacent**. Adjacent spots occupied by friendly stones are shown connected by a line.
6. A **liberty is an adjacent spot where a stone can fit**.
7. When a stone or a group of connected stones loses all liberties, it is captured.
8. Ko: one may not capture just one stone if that stone was played on the previous turn and captured just one stone. No positional superko fuck off.
9. Scoring: after two consecutive passes, whoever has more stones on the board wins. (Yes, area scoring with group tax!)

### How to actually play

- Currently, only taking turns on a single device is implemented.
- The url holds the game record - you can bookmark the current position, share it to play by email, use the browser back button to undo, etc.
- Hover over (or touch) stones to see their liberties.
- Mouse down and move (or touch-move) to stage a move, then click the staged move to confirm it.
- Pull from your own stone to place a stone as far as possible while it stays connected.
- If a spot clearly has enough space for a stone but the stone is not appearing, that means playing there would be a suicide.
- While suicide itself is not allowed, you can **capture another group of your own stones** by removing their last liberty. It feels this could be useful at times.

### Code/bugs/feedback

How else could go work if not played on the grid?
It feels like I discovered this rather than invented it...

[Code available on GitHub](https://github.com/tasuki/continugo), [bug reports and feature requests also go there please](https://github.com/tasuki/continugo/issues).
"""
