# ContinuGo viewer

Read-only, embeddable ContinuGo boards with the application's rendering and hover
previews. No iframe, framework, external styles, fonts, network requests, storage,
or page-level keyboard handlers. Multiple instances are independent.

## Build

From the repository root:

```sh
mise run //frontend:build-viewer
```

The generated, gitignored `frontend/viewer/dist/continugo-viewer.js` is the only
file consumers need. Copy it wherever your site serves JavaScript. To try the
two-board example, open `frontend/viewer/example.html` after building.

`mise run //frontend:test-viewer` builds and smoke-tests the bundle in headless
Chrome. Set `CHROME` if its executable is not named `google-chrome`. Viewer model
and hover tests are also included in the normal frontend test suite.

## Embed

```html
<div id="position" style="width: 400px; max-width: 100%"></div>
<script src="continugo-viewer.js"></script>
<script>
  const viewer = ContinugoViewer.mount(document.querySelector('#position'), {
    sgf: '(;FF[9071]GM[9071]SZ[13];B[fOfO];W[nyny])',
    move: 1,
    showPolicy: true,
  });

  viewer.update({ move: 2 });
  // When removing the viewer:
  // viewer.destroy();
</script>
```

The container controls width; the board stays square and resizes automatically.
The bundle creates HTML/SVG and includes its CSS in a Shadow DOM, keeping host
styles and board IDs isolated. It adds one child without changing existing
container content. Modern browsers with Shadow DOM and Pointer Events are required.

## API

`ContinugoViewer.mount(element, options?)` returns `{ update, destroy }`.
Only one viewer may be mounted per container; different containers are independent.

| Option | Default | Meaning |
| --- | --- | --- |
| `sgf` | `""` | ContinuGo SGF text; empty means an empty default board. |
| `move` | `null` | Number of moves to display, including passes. `0` is the start; `null` is the end. Values beyond the end are clamped. |
| `showPolicy` | `false` | Show recorded policy alternatives for the displayed position. |
| `hover` | `true` | Mouse/pen group highlighting and hypothetical placement/capture previews. |

`update(patch)` merges any of these options with their current values. To load a
new record at its end, use `update({ sgf: newSgf, move: null })`. Position updates
clear hover previews. Rendering happens on the browser's next animation frame.
Replay buttons, sliders, and other controls belong to the host page.

`destroy()` removes the viewer and its listeners. Repeated calls are harmless;
`update()` after destruction throws. The container can then be mounted again.
Call `destroy()` rather than just removing the container.

Invalid option types and unknown keys throw synchronously. Malformed SGF or an
unsupported board size displays an error inside the viewer; a valid update
recovers. SGF replay follows the application's permissive rules: illegal recorded
placements are skipped on the board, not treated as validation errors. This is a
ContinuGo viewer, not an ordinary intersection-Go SGF viewer; SGF variations are
not supported.

Policy alternatives come from existing JSON annotations in SGF move comments.
As in the application, the next move's annotation describes the displayed
position. Nothing is calculated by an AI or fetched from a server. Pass
alternatives have no board location and are not drawn. See `example.html` for an
annotated record.

Clicks never play or modify the record. Touch gestures retain normal browser
scrolling/zooming rather than producing hover previews.

## Sources

`src/BoardScene.elm`, `src/BoardHover.elm`, and `public/media/board.css` are shared
with the application. `src/Viewer.elm` supplies the read-only Elm entry point;
`viewer/index.js` supplies mounting and container-relative pointer coordinates.
`viewer/style.css` only supplies viewer-specific layout. `build.mjs` compiles Elm
and packages the runtime, wrapper, and styles without exposing a global `Elm`.
