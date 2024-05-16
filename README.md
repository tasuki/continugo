# Continuous go

Long enough have we suffered under the constraints imposed upon us by the grid of intersections. Down with the grid of intersections!

## Reasoning behind this

None. This is a terrible idea.

### Inspiration

The idea might be bad, but it is not even original.

- [AnalogChess](https://github.com/ehulinsky/AnalogChess), [play online](https://replit.com/@EricMiller8/AnalogChess-fixed-icons)
- [Not Tetris 2](https://stabyourself.net/nottetris2/)

## Setup

On Debian 12 Bookworm:

```
sudo apt install npm uglifyjs
sudo npm install -g elm@latest-0.19.1 elm-test elm-format serve
```

To develop, run `make develop`, see http://localhost:9071/

## TODO

- [ ] Drag from stone to place a stone as far as possible and still connected
- [ ] Test whether the "adjacent distance" of 1.4142 isn't too high
- [ ] Enforce ko: low priority, likely won't happen much...
