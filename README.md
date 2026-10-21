# ContinuGo

Long have we suffered under the tyranny of the grid of intersections. Down with the grid!

**See [the help page](docs/help.md)!**


## Reasoning behind this

None. This is a terrible idea.

### Inspiration

The idea might be bad, but it is not even original.

- [AnalogChess](https://github.com/ehulinsky/AnalogChess), [play online](https://replit.com/@EricMiller8/AnalogChess-fixed-icons)
- [Not Tetris 2](https://stabyourself.net/nottetris2/), [source](https://github.com/Stabyourself/nottetris2/tree/master)


## Develop

[![CI](https://github.com/tasuki/continugo/actions/workflows/ci.yml/badge.svg?branch=master)](https://github.com/tasuki/continugo/actions/workflows/ci.yml)

On Debian 12 Bookworm:

```
sudo apt install npm uglifyjs
sudo npm install -g elm@latest-0.19.1 elm-test elm-format serve
```

To develop, run `make develop`, see http://localhost:9071/

For hover/capture profiling results and reproducible benchmarks, see [benchmarks](benchmarks/README.md).
