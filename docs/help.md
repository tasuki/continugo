## ContinuGo

Long have we suffered under the tyranny of the grid of intersections. Down with the grid!

### Rules

1. You know [how to play go](https://online-go.com/learn-to-play-go/). The following are the differences.
2. ContinuGo is played on a square board. Lines may be drawn on it who cares about those.
3. Stones may not overlap.
4. Stones whose centres are **closer than the stone diameter times the square root of two are considered adjacent**. Adjacent stones of the same colour are connected, and shown as such by a connecting line.
6. A **liberty is an adjacent empty point where a stone can fit**.
7. Scoring: After two consecutive passes, every point belongs to the nearest stone's colour. Whoever controls more board area wins.

The [full rules are also available](rules.md), though I doubt you want to read 'em.

### How to actually play

- Currently, only taking turns on a single device is implemented.
- The url holds the game record - you can bookmark the current position, share it to play by email, use the browser back button to undo, etc.
- Tap groups/stones to highlight their liberties.
- Drag to stage a move, then tap the staged move to confirm it.
- Drag from your own stone to place a stone as far as possible while remaining connected.
- If a spot clearly has enough space for a stone but the stone is not appearing, playing there would be a suicide.
- While suicide is not allowed, you can **capture another group of your own stones** by removing their last liberty.
- **Capture dead groups before passing**: scoring uses the stones still on the board, with no automatic dead-stone removal.
- Other than artificially created positions, I've not seen a ko. If you see an actual ko, do let me know!

### Analysis mode

- Toggle **o** to enter/leave analysis mode. Analysis does not change the turn or game record.
- Tap to add hollow circles. Circles must fit without overlapping stones or other circles. Dashed lines show potential connections to stones of either colour and other circles; circles do not capture or belong to either player.
- Drag from any stone or circle to create another circle as far away as possible while still connected – just as when dragging from your own stone during play.
- Circles cannot be moved: remove one by tapping it and draw a new one instead.
- Double-tap a circle to leave analysis and stage a stone at that spot.

### Code/bugs/feedback

How else could go work if not played on the grid?
It feels like I discovered this rather than invented it...

[Code available on GitHub](https://github.com/tasuki/continugo), [bug reports and feature requests also go there please](https://github.com/tasuki/continugo/issues).
