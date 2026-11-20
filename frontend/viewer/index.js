// Wrapped with the Elm runtime and styles by build.mjs; no globals except the public API.
const mounted = new WeakMap();

function options(value) {
  if (!value || typeof value !== 'object' || Array.isArray(value)) {
    throw new TypeError('Viewer options must be an object.');
  }
  for (const key of Object.keys(value)) {
    if (!['sgf', 'move', 'showPolicy', 'hover', 'backgroundColor', 'blackColor', 'whiteColor'].includes(key)) {
      throw new TypeError(`Unknown viewer option: ${key}`);
    }
  }
  if (typeof value.sgf !== 'string') throw new TypeError('sgf must be a string.');
  if (value.move !== null && (!Number.isSafeInteger(value.move) || value.move < 0)) {
    throw new TypeError('move must be a nonnegative integer or null.');
  }
  if (typeof value.showPolicy !== 'boolean' || typeof value.hover !== 'boolean') {
    throw new TypeError('showPolicy and hover must be booleans.');
  }
  for (const key of ['backgroundColor', 'blackColor', 'whiteColor']) {
    if (typeof value[key] !== 'string' || !globalThis.CSS.supports('color', value[key])) {
      throw new TypeError(`${key} must be a CSS color.`);
    }
  }
  return { ...value };
}

function mount(element, initial = {}) {
  if (!element || element.nodeType !== 1 || typeof element.attachShadow !== 'function') {
    throw new TypeError('mount requires an HTML element.');
  }
  if (mounted.has(element)) throw new Error('This element already has a viewer.');
  if (!initial || typeof initial !== 'object' || Array.isArray(initial)) {
    throw new TypeError('Viewer options must be an object.');
  }
  let config = options({
    sgf: '', move: null, showPolicy: true, hover: true,
    backgroundColor: '#CB5', blackColor: '#222', whiteColor: '#FFF', ...initial,
  });
  const document = element.ownerDocument;
  const host = document.createElement('div');
  const shadow = host.attachShadow({ mode: 'open' });
  const style = document.createElement('style');
  style.textContent = CSS + `
    #board { background-color: var(--viewer-background); }
    #hide-lines circle { fill: var(--viewer-background); }
    #stones, #links, #ghost-stone, #ghost-links, #removed-stones { stroke: var(--viewer-black); }
    .black { fill: var(--viewer-black); }
    .white { fill: var(--viewer-white); }
  `;
  function colors() {
    host.style.setProperty('--viewer-background', config.backgroundColor);
    host.style.setProperty('--viewer-black', config.blackColor);
    host.style.setProperty('--viewer-white', config.whiteColor);
  }
  colors();
  const node = document.createElement('div');
  shadow.append(style, node);
  element.append(host);
  let app = Elm.Viewer.init({ node, flags: config });

  function pointer(event) {
    if (!config.hover || event.pointerType === 'touch') return;
    const board = shadow.getElementById('board');
    if (!board) return;
    const rect = board.getBoundingClientRect();
    if (!rect.width || !rect.height) return;
    app.ports.pointer.send({
      x: (event.clientX - rect.left) / rect.width,
      y: (event.clientY - rect.top) / rect.height,
    });
  }
  function leave() {
    app.ports.pointer.send(null);
  }
  host.addEventListener('pointermove', pointer);
  host.addEventListener('pointerleave', leave);
  host.addEventListener('pointercancel', leave);

  const viewer = {
    update(patch) {
      if (!app) throw new Error('Viewer has been destroyed.');
      if (!patch || typeof patch !== 'object' || Array.isArray(patch)) {
        throw new TypeError('Viewer options must be an object.');
      }
      config = options({ ...config, ...patch });
      colors();
      app.ports.configure.send(config);
    },
    destroy() {
      if (!app) return;
      host.removeEventListener('pointermove', pointer);
      host.removeEventListener('pointerleave', leave);
      host.removeEventListener('pointercancel', leave);
      host.remove();
      mounted.delete(element);
      // No timers, document listeners, or outbound subscriptions are installed.
      app = null;
    },
  };
  mounted.set(element, viewer);
  return viewer;
}

globalThis.ContinugoViewer = Object.freeze({ mount });
