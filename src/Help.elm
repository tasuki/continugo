module Help exposing (..)

import Html
import Markdown


help : Html.Html msg
help =
    Markdown.toHtml [] """
## ContinuGo

Long enough have we suffered under the constraints imposed upon us by the grid of intersections. Down with the grid!

### Rules

1. You know [how to play go](https://online-go.com/learn-to-play-go/). Exceptions and clarifications below.
2. Go is played on an empty square, called a board. It may have lines drawn on it who cares about those.
3. **Stones may not overlap.**
4. A point on the board where a stone would fit is called a spot.
5. Spots whose centres are closer than **the stone diameter times the square root of two** are *considered adjacent* and, when occupied by friendly stones, are shown connected by a line.
6. A **liberty is an adjacent spot where a stone would fit**.
7. When a stone or a group of connected stones has no liberties, it is captured.
8. Ko: one may not capture just one stone if that stone was played on the previous move and captured just one stone. No positional superko fuck off.
9. Scoring: after two consecutive passes, whoever has more stones on the board wins (yes we have group tax!).

### How to actually play

So far only taking turns on a single-device is implemented. The url holds the game record - you can bookmark the current position, share it, use the browser back button to undo, etc.

Some possible gotchas, I think these are - and should be - a part of the game, but some of are weird:

- Sometimes there's a spot where there's clearly enough space to put a stone but the stone is not appearing: that's because playing there would be a suicide.
- While suicide by itself is not allowed (it wouldn't be a good move ever I think?), it's perfectly possible to capture *another* group of your stones by removing their last liberty (ie by preventing placement of any and all stones which would be connected to it). I don't want to disallow playing a perfectly valid move just because it captures.

I don't think the rules can be much different, but if you have any ideas I'd love to hear them!

### Code/bugs

[Code is on GitHub](https://github.com/tasuki/continugo), [bug reports also go there](https://github.com/tasuki/continugo/issues).
"""
