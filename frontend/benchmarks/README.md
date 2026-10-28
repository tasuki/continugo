# Hover/capture performance

## Findings and changes

- `Rules.Liberties.hasLiberties` exhausted roughly 18,000 integer candidates for every
  trapped stone. It now tests four cheap probes, then searches integer row
  intervals with the occupied discs subtracted. Strict overlap/adjacency and
  inclusive board boundaries are preserved, including single-point openings.
  The production build no longer constructs/sorts the exhaustive candidate ring.
- `Rules.Play.captureGroupsWithoutLiberties` revisited connected groups through multiple nearby stones and
  repeatedly removed the same stones. It now remembers discovered group members;
  group traversal uses coordinate sets instead of comparing entire stone records.
- Hover/drag legality was calculated once in update and replayed again during
  rendering. Previews now retain the successful result, group and captures.
  The entire SVG is lazy on position/display inputs, not pointer bookkeeping;
  identical snapped previews retain their original references.
- Elm evaluates `Maybe.withDefault`'s fallback eagerly. A staged move that was
  being retained still ran `handleHover`. An explicit `case` removes that work.
- Liberty-area geometry inspected the entire board. Only discs within one
  adjacency radius plus one diameter of the group can affect it. Intersection
  validation also reuses the circle's already-filtered nearby blockers.
- Browser profiling then found that repainting the unchanged Gaussian-blurred
  background dominated the remaining work. The background and changing position
  now have separate, fixed-size SVG viewports. Chrome can retain the background
  without repainting the blur alongside every moving mask.
- Mobile pinch zoom magnifies mask textures as well as the board. Liberty masks
  and their painted rectangle are now bounded to the group's reach plus one
  stone radius. Deadzones paint the inverse sweep directly rather than nesting
  two full-board masks. Compositing hints apply only to the whole SVG viewports:
  promoting individual masked groups left rectangular stale tiles after zoom
  and repaint. The changing masks now share one stable viewport with the stones.
- Service-worker cache bumped so deployed clients can receive the new JS/CSS.

## Measurements

One local run, optimized Elm, before = `006370a`. Times vary with hardware/JIT.
The benchmark uses the existing four-stone capture record from `Fixtures.Play`.

| Pure operation | Before | After |
| --- | ---: | ---: |
| Blocked stone's liberty check | 2.64 ms | 0.084 ms |
| Capture legality | 79.0 ms | 2.47 ms |
| Open-space hover legality | 0.039 ms | 0.026 ms |
| Full-board coverage geometry | 1.84 ms | 1.29 ms |
| Local liberty-area geometry | 2.00 ms | 0.265 ms |

Headless Chrome, 900×900, software rendering, alternating 20 times between the
capture preview and an empty location, after the initial fade animation:

| Browser work, per pointer event | Before | After |
| --- | ---: | ---: |
| Capture/empty hover: scripting | 183 ms | 8.9 ms |
| Capture/empty hover: total main-thread task time | 189 ms | 13.2 ms |
| Unchanged hover: scripting | 27.2 ms | 1.14 ms |

A later mobile-emulation trace (390×844, DPR 3, 3× pinch zoom, 90 moving
previews) reduced summed raster-worker time from about 31 to 4.9 ms/event with
the bounded masks, direct inverse sweep, and separate SVG viewports.

These are CPU-work measurements, not guaranteed frame latency or device-wide
speedups. The browser after-build includes release minification. Chrome's
compositing benefit may differ in other browsers. Changing previews still
requires computing/rendering their SVG masks; nearest-playable snapping still
uses the original exhaustive search. Plain open-space move legality was already
cheap, so it was not the priority.

## Reproduce

Run these commands from the repository root after `mise install`.

Pure benchmark (median of five measured batches after one warmup):

```sh
(cd frontend && mise exec -- elm make benchmarks/Benchmark.elm --optimize --output=/tmp/continugo-benchmark.js)
mise exec -- node frontend/benchmarks/run.js /tmp/continugo-benchmark.js
```

For old sources, adapt the copied harness before compiling: replace
`Rules.Position` with `Go`, `Rules.Play as Play` with `Play`, and
`Rules.Liberties as Liberties` with `Liberties`, and `Coverage` with `DeadZones`.
Remove `import Placement` and
replace `Placement.playNearby` with `Play.playNearby` and
`Play.replayPlacements` with `Play.playStones`.

```sh
mkdir -p /tmp/continugo-before
git archive 006370a src elm.json | tar -x -C /tmp/continugo-before
cp -r frontend/benchmarks /tmp/continugo-before/
# Adapt /tmp/continugo-before/benchmarks/Benchmark.elm as described above.
mise exec -- sh -c 'cd /tmp/continugo-before && elm make benchmarks/Benchmark.elm --optimize --output=/tmp/continugo-before.js'
mise exec -- node frontend/benchmarks/run.js /tmp/continugo-before.js
```

Browser benchmark and interaction checks (requires a separately installed Chrome;
no additional npm dependencies):

```sh
mise -C frontend release
mise -C frontend serve
# In another terminal:
google-chrome --headless --disable-gpu --remote-debugging-port=19272 \
  --user-data-dir=/tmp/continugo-benchmark-chrome about:blank
# In another terminal:
mise exec -- node frontend/benchmarks/browser.js
```

Defaults: app at `http://localhost:9071/`, CDP at `http://localhost:19272`.
Override with `AFTER_URL` and `CDP_URL`. Set `BEFORE_URL` to an independently
served old build (including its old stylesheet) for the comparison. The script
bypasses browser/service-worker caches and checks hover/capture cancellation,
unchanged stones and record URL, staging, scoring and committing a move. Touch
checks also cover preview tracking and staging at 3× zoom/DPR 3, and verify that
a lone preview no longer allocates full-board liberty masks. Real two-finger zoom
and pixel comparisons after SVG invalidation at 1.5×, 3×, and 5× check for stale
mask tiles, rather than just checking that the DOM contains the right circles.

Correctness: `mise -C frontend test --fuzz 1000` covers interval-search comparisons
against exhaustive search for randomized blockers and board corners, strict distance
boundaries, isolated touching liberties, captures, suicide, game progression,
scoring, placement adjustment, previews, and deadzones.
