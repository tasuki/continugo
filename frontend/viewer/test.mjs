// Real-browser smoke test; no browser automation dependency required.
import assert from 'node:assert/strict';
import { spawn } from 'node:child_process';
import { mkdtempSync, readFileSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { setTimeout as sleep } from 'node:timers/promises';

const profile = mkdtempSync(join(tmpdir(), 'continugo-viewer-chrome-'));
const chrome = spawn(process.env.CHROME || 'google-chrome', [
  '--headless', '--no-sandbox', '--disable-gpu', '--remote-debugging-port=0',
  `--user-data-dir=${profile}`, 'about:blank',
], { stdio: 'ignore' });
let launchError;
chrome.on('error', error => { launchError = error; });
let ws;
try {
  let port;
  for (let i = 0; i < 100; i++) {
    if (launchError) throw launchError;
    try { port = readFileSync(join(profile, 'DevToolsActivePort'), 'utf8').split('\n')[0]; } catch {}
    if (port) break;
    await sleep(50);
  }
  assert.ok(port, 'Chrome did not start; set CHROME to its executable path.');
  const targets = await fetch(`http://localhost:${port}/json/list`).then(r => r.json());
  ws = new WebSocket(targets.find(target => target.type === 'page').webSocketDebuggerUrl);
  await new Promise((resolve, reject) => { ws.onopen = resolve; ws.onerror = reject; });
  let id = 0;
  const pending = new Map();
  const errors = [];
  ws.onmessage = ({ data }) => {
    const message = JSON.parse(data);
    if (message.id) {
      const { resolve, reject } = pending.get(message.id);
      pending.delete(message.id);
      message.error ? reject(Error(JSON.stringify(message.error))) : resolve(message.result);
    } else if (message.method === 'Runtime.exceptionThrown') {
      errors.push(message.params.exceptionDetails);
    }
  };
  const call = (method, params = {}) => new Promise((resolve, reject) => {
    pending.set(++id, { resolve, reject });
    ws.send(JSON.stringify({ id, method, params }));
  });
  const evaluate = async expression => {
    const result = await call('Runtime.evaluate', { expression, awaitPromise: true, returnByValue: true });
    if (result.exceptionDetails) throw Error(JSON.stringify(result.exceptionDetails));
    return result.result.value;
  };
  await call('Runtime.enable');
  await call('Page.enable');
  await call('Page.navigate', { url: new URL('./example.html', import.meta.url).href });
  for (let i = 0; i < 100; i++) {
    if (await evaluate("!!document.querySelector('#first > div')?.shadowRoot?.querySelector('#stones')")) break;
    await sleep(50);
  }
  const result = await evaluate(`(async () => {
    const check = (condition, message) => { if (!condition) throw Error(message); };
    const frame = () => new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve)));
    const container = document.querySelector('#first');
    const host = container.firstElementChild;
    const root = host.shadowRoot;
    const other = document.querySelector('#second').firstElementChild.shadowRoot;
    const count = (root, selector) => root.querySelectorAll(selector).length;
    check(count(root, '#stones circle') === 2, 'initial replay position');
    check(count(other, '#stones circle') === 4, 'independent final position');
    check(count(root, '#policy-alternatives circle') === 2, 'next-node policy');
    check(typeof globalThis.Elm === 'undefined', 'Elm runtime must remain private');
    check(getComputedStyle(root.querySelector('svg')).display !== 'none', 'host SVG style leaked');
    check(getComputedStyle(root.querySelector('#stones .black')).fill === 'rgb(34, 34, 34)', 'host stone style leaked');

    first.update({ move: 0 }); await frame();
    check(count(root, '#stones circle') === 0 && count(other, '#stones circle') === 4, 'isolated replay update');
    first.update({ move: 2, showPolicy: false }); await frame();
    check(count(root, '#policy-alternatives circle') === 0, 'policy disabled');
    first.update({ showPolicy: true }); await frame();
    check(count(root, '#policy-alternatives circle') === 2, 'policy enabled');

    // Offset and resize the board: pointer coordinates must use this instance's rect.
    container.style.width = '300px';
    container.style.transform = 'translate(25px, 15px)';
    const point = async (x, y) => {
      const board = root.getElementById('board');
      const r = board.getBoundingClientRect();
      check(Math.abs(r.width - r.height) < 1 && Math.abs(r.width - 300) < 1, 'responsive square sizing');
      board.dispatchEvent(new PointerEvent('pointermove', {
        bubbles: true, composed: true, pointerType: 'mouse',
        clientX: r.left + x * r.width, clientY: r.top + y * r.height,
      }));
      await frame();
    };
    await point(0.5, 0.5);
    check(root.querySelector('#ghost-stone circle')?.getAttribute('cx') === '500', 'container-relative hover');
    check(count(other, '#ghost-stone circle') === 0, 'hover isolated');
    root.getElementById('board').click(); await frame();
    check(count(root, '#stones circle') === 2, 'click must not play');
    await point(0.3, 0.3);
    check(count(root, '#highlights circle') === 1, 'existing group highlight');
    host.dispatchEvent(new PointerEvent('pointerleave')); await frame();
    check(count(root, '#highlights circle, #ghost-stone circle') === 0, 'leave clears hover');
    first.update({ hover: false }); await frame();
    await point(0.5, 0.5);
    check(count(root, '#ghost-stone circle') === 0, 'hover disabled');

    let threw = false;
    try { first.update({ move: -1 }); } catch { threw = true; }
    check(threw, 'invalid options must throw');
    first.update({ sgf: 'bad input' }); await frame();
    check(!!root.querySelector('[role=alert]'), 'SGF error displayed');
    first.update({ sgf: '(;FF[9071]GM[9071]SZ[9];B[fOfO])', move: null }); await frame();
    check(count(root, '#stones circle') === 1 && !root.querySelector('[role=alert]'), 'recover from bad SGF');

    first.destroy(); first.destroy();
    check(container.children.length === 0, 'destroy removes only its host');
    threw = false;
    try { first.update({ move: 0 }); } catch { threw = true; }
    check(threw, 'update after destroy must throw');
    const replacement = ContinugoViewer.mount(container);
    replacement.destroy();
    check(count(other, '#stones circle') === 4, 'destroy does not affect other viewers');
    return 'Viewer browser tests passed';
  })()`);
  assert.deepEqual(errors, [], 'No browser runtime errors');
  console.log(result);
} finally {
  ws?.close();
  if (chrome.exitCode === null && !launchError) {
    const exited = new Promise(resolve => chrome.once('exit', resolve));
    chrome.kill();
    await exited;
  }
  rmSync(profile, { recursive: true, force: true, maxRetries: 5, retryDelay: 100 });
}
