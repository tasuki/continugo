// Node >= 22. Start Chrome with --remote-debugging-port=19272 first.
const assert = require('node:assert/strict');
const sleep = ms => new Promise(resolve => setTimeout(resolve, ms));

(async () => {
  const version = await fetch(`${process.env.CDP_URL || 'http://localhost:19272'}/json/version`).then(r => r.json());
  const ws = new WebSocket(version.webSocketDebuggerUrl);
  await new Promise(resolve => { ws.onopen = resolve; });
  let id = 0;
  const pending = new Map(), errors = [];
  ws.onmessage = ({ data }) => {
    const message = JSON.parse(data);
    if (message.id) {
      const { resolve, reject } = pending.get(message.id);
      pending.delete(message.id);
      message.error ? reject(message.error) : resolve(message.result);
    } else if (message.method === 'Runtime.exceptionThrown') {
      errors.push(message.params.exceptionDetails);
    }
  };
  const call = (method, params = {}, sessionId) => new Promise((resolve, reject) => {
    const requestId = ++id;
    pending.set(requestId, { resolve, reject });
    ws.send(JSON.stringify({ id: requestId, method, params, sessionId }));
  });
  const { targetId } = await call('Target.createTarget', { url: 'about:blank' });
  const { sessionId } = await call('Target.attachToTarget', { targetId, flatten: true });
  const c = (method, params = {}) => call(method, params, sessionId);
  const evaluate = async expression => {
    const result = await c('Runtime.evaluate', { expression, returnByValue: true, awaitPromise: true });
    if (result.exceptionDetails) throw Error(JSON.stringify(result.exceptionDetails));
    return result.result.value;
  };
  const wait = async expression => {
    for (let i = 0; i < 100; i++) {
      if (await evaluate(expression)) return;
      await sleep(50);
    }
    throw Error('Timed out: ' + expression);
  };
  await c('Runtime.enable');
  await c('Page.enable');
  await c('Performance.enable');
  await c('Network.enable');
  await c('Network.setCacheDisabled', { cacheDisabled: true });
  await c('Network.setBypassServiceWorker', { bypass: true });
  await c('Emulation.setDeviceMetricsOverride', { width: 900, height: 900, deviceScaleFactor: 1, mobile: false });
  const urls = { before: process.env.BEFORE_URL, after: process.env.AFTER_URL || 'http://localhost:9071/' };
  const navigate = async (build, record = '') => {
    await c('Page.navigate', { url: urls[build] + (record ? '?record=' + record : '') });
    await wait("!!document.querySelector('#board svg')");
    await evaluate(`
      window.frame = () => new Promise(r => requestAnimationFrame(() => requestAnimationFrame(() => requestAnimationFrame(r))));
      window.pointerPressed = false;
      window.move = async (x, y, type = 'mousemove') => {
        const b = document.querySelector('#board').getBoundingClientRect();
        if (type === 'mousedown') pointerPressed = true;
        if (type === 'mouseup') pointerPressed = false;
        // Keep the same scenarios usable against older mouse-event builds.
        const EventClass = ${build === 'before' ? 'MouseEvent' : 'PointerEvent'};
        const eventType = ${build === 'before' ? 'type' : "type.replace('mouse', 'pointer')"};
        document.querySelector('#board-container').dispatchEvent(new EventClass(eventType, {
          bubbles: true, clientX: b.x + x*b.width/1000, clientY: b.y + y*b.height/1000,
          pointerId: 1, pointerType: 'mouse', isPrimary: true,
          button: type === 'mousemove' ? -1 : 0, buttons: pointerPressed ? 1 : 0
        }));
        await frame();
      };
    `);
    await evaluate('frame()');
  };
  const record = 'B[oifh];W[fgoc];B[odpz];W[fhfm];B[ihdL];W[lfem];B[ekpj];W[fsqe];B[dWnk];W[eSmd];B[dGll];W[dsqt];B[fHkR];W[iogz];B[jAfl];W[kPgM];B[gMfX];W[gUez];B[jnhQ];W[hIit];B[lLfD];W[mRgQ];B[mLdR];W[jPcR];B[iHbT];W[hqcz];B[kJbI];W[mbcC];B[nrbT];W[hLaM];B[lZaM];W[jKjx];B[lfiC];W[lCko];B[mVjo];W[nAkG];B[oQjC];W[pCkQ];B[cQim];W[qrmH];B[pYii];W[qNju];B[regx];W[oshO];B[codi];W[dhfM];B[hNlD];W[pvgk]';
  const metrics = async () => Object.fromEntries((await c('Performance.getMetrics')).metrics.map(m => [m.name, m.value]));
  try {
    // Scoring dims the whole board, with a centered result in either orientation.
    for (const [width, height] of [[1200, 800], [390, 844]]) {
      await c('Emulation.setDeviceMetricsOverride', { width, height, deviceScaleFactor: 1, mobile: false });
      await navigate('after', record);
      // Every tooltip stays on one line and left-aligned, including long labels.
      const menuItems = await evaluate("document.querySelectorAll('#menu .icon').length");
      for (let i = 0; i < menuItems; i++) {
        const point = await evaluate(`(() => {
          const r = document.querySelectorAll('#menu .icon')[${i}].getBoundingClientRect();
          return { x: r.x + 15, y: r.y + 15 };
        })()`);
        await c('Input.dispatchMouseEvent', { type: 'mouseMoved', ...point });
        await evaluate('frame()');
        assert.equal(await evaluate(`(() => {
          const tooltip = document.querySelectorAll('#menu .tooltip')[${i}];
          const style = getComputedStyle(tooltip);
          return style.display !== 'none' && style.textAlign === 'left'
            && tooltip.getBoundingClientRect().height <= parseFloat(style.lineHeight);
        })()`), true);
      }
      await c('Input.dispatchMouseEvent', { type: 'mouseMoved', x: width - 1, y: height - 1 });
      await evaluate("document.querySelector('#score-toggle').click();frame()");
      const score = await evaluate(`(() => {
        const board = document.querySelector('#board').getBoundingClientRect();
        const overlay = document.querySelector('#score');
        const rect = overlay.getBoundingClientRect();
        const result = overlay.querySelector('.score-result').getBoundingClientRect();
        return {
          coversBoard: ['x','y','width','height'].every(k => Math.abs(board[k]-rect[k]) < 1),
          centered: Math.abs(result.x+result.width/2-board.x-board.width/2) < 1
            && Math.abs(result.y+result.height/2-board.y-board.height/2) < board.height*0.1,
          background: getComputedStyle(overlay).backgroundColor,
          color: getComputedStyle(overlay).color,
          black: getComputedStyle(document.querySelector('.territory-black')).fill,
          white: getComputedStyle(document.querySelector('.territory-white')).fill
        };
      })()`);
      assert.equal(score.coversBoard, true);
      assert.equal(score.centered, true);
      assert.equal(score.background, 'rgba(0, 0, 0, 0.533)');
      assert.equal(score.color, 'rgb(255, 255, 255)');
      assert.notEqual(score.black, score.white);
      assert.equal(await evaluate('location.search'), '?record=' + record);
      await evaluate("document.querySelector('#score-toggle').click();frame()");
      assert.equal(await evaluate("document.querySelectorAll('#score, #territory').length"), 0);
      await evaluate(`(async () => {
        const pass = [...document.querySelectorAll('#menu .icon')].find(e => e.textContent.startsWith('*'));
        pass.click(); await frame(); pass.click(); await frame();
      })()`);
      assert.equal(await evaluate("document.querySelectorAll('#score .score-result, #territory').length"), 2);
    }
    await c('Emulation.setDeviceMetricsOverride', { width: 900, height: 900, deviceScaleFactor: 1, mobile: false });
    for (const build of (urls.before ? ['before', 'after'] : ['after'])) {
      await navigate(build, record);
      await evaluate('move(839,240)');
      assert.equal(await evaluate("document.querySelectorAll('#stones .pending-capture').length"), 4);
      await evaluate('move(850,850)');
      await sleep(5500); // Let the existing blurred-stone fade animation finish.
      const start = await metrics();
      await evaluate('(async()=>{for(let i=0;i<20;i++){await move(839,240);await move(850,850);}})()');
      const end = await metrics();
      console.log(`${build}: capture-hover script ${((end.ScriptDuration-start.ScriptDuration)*1000/40).toFixed(2)} ms/event; total main-thread ${((end.TaskDuration-start.TaskDuration)*1000/40).toFixed(2)} ms/event`);
      await evaluate(`window.mutations=0; new MutationObserver(ms=>{mutations+=ms.length}).observe(document.querySelector('#board-position'),{attributes:true,childList:true,subtree:true});`);
      const steadyStart = await metrics();
      await evaluate('(async()=>{for(let i=0;i<20;i++)await move(850,850);})()');
      const steadyEnd = await metrics();
      console.log(`${build}: unchanged-hover script ${((steadyEnd.ScriptDuration-steadyStart.ScriptDuration)*1000/20).toFixed(2)} ms/event; SVG mutations ${await evaluate('mutations')}`);
      assert.equal(await evaluate("document.querySelectorAll('#stones circle').length"), 45);
      assert.equal(await evaluate('location.search'), '?record=' + record);
    }
    await navigate('after');
    await evaluate('move(500,500)');
    assert.equal(await evaluate("document.querySelectorAll('#ghost-stone circle').length"), 1);
    assert.equal(await evaluate("document.querySelectorAll('#liberty-area-fill').length"), 1);
    // A lone preview must not allocate full-board liberty mask textures.
    assert.equal(await evaluate(`(() => {
      const fill = document.querySelector('#liberty-area-fill');
      const mask = document.querySelector('#liberty-swept-area');
      return +fill.getAttribute('width') < 400 && +fill.getAttribute('height') < 400
        && ['x','y','width','height'].every(k => fill.getAttribute(k) === mask.getAttribute(k));
    })()`), true);
    await evaluate("move(500,500,'mousedown')");
    await evaluate('move(650,500)');
    await evaluate("move(650,500,'mouseup')");
    assert.equal(await evaluate("document.querySelectorAll('#staged-stone circle').length"), 1);
    assert.equal(await evaluate("document.querySelector('#stone-swept-area g circle').getAttribute('cx')"), '650');
    await evaluate("document.querySelector('#score-toggle').click();frame()");
    assert.equal(await evaluate("document.querySelectorAll('#ghost-stone circle,#staged-stone circle,#liberty-area-fill,#capture-preview line').length"), 0);
    await evaluate("document.querySelector('#score-toggle').click();frame()");
    await evaluate('move(800,800)');
    await evaluate("move(800,800,'mousedown')");
    await evaluate("move(800,800,'mouseup')");
    await wait("document.querySelectorAll('#stones circle').length===1");
    // Analysis is a separate, persistent scratch layer: no moves or turns.
    await evaluate(`
      window.tap = async (x,y) => { await move(x,y,'mousedown'); await move(x,y,'mouseup'); };
      window.pull = async (x,y,toX,toY) => { await move(x,y,'mousedown'); await move(toX,toY); await move(toX,toY,'mouseup'); };
    `);
    await evaluate('tap(200,800)'); // Add a white stone, so both colours can be pulled.
    const positionUrl = await evaluate('location.search');
    const checkAnalysisDeadzones = async () => {
      const centers = selector => evaluate(`Array.from(document.querySelectorAll(${JSON.stringify(selector)}),c=>[+c.getAttribute('cx'),+c.getAttribute('cy')]).sort()`);
      assert.deepEqual(await centers('#stone-swept-area > g > g > circle'), await centers('#stones circle, #analysis-circles circle'));
    };
    await evaluate("document.querySelector('#analysis-toggle').click();frame()");
    const realStoneDeadzones = await evaluate("document.querySelector('#dead-zones').innerHTML");
    await evaluate('move(500,500)');
    assert.equal(await evaluate("document.querySelectorAll('#analysis-circles .preview').length"), 1);
    assert.equal(await evaluate("document.querySelectorAll('#analysis-circles circle:not(.preview)').length"), 0);
    await checkAnalysisDeadzones();
    assert.equal(await evaluate('location.search'), positionUrl);
    await evaluate("document.querySelector('#board').dispatchEvent(new PointerEvent('pointerleave'));frame()");
    assert.equal(await evaluate("document.querySelectorAll('#analysis-circles .preview').length"), 0);
    assert.equal(await evaluate("document.querySelector('#dead-zones').innerHTML"), realStoneDeadzones);
    await evaluate('(async()=>{await tap(500,500);await tap(600,500);})()');
    assert.equal(await evaluate("document.querySelectorAll('#analysis-circles circle').length"), 2);
    const proposedX = await evaluate("document.querySelector('#analysis-circles circle').getAttribute('cx')");
    assert.equal(await evaluate("document.querySelectorAll('#analysis-links line').length"), 1);
    await evaluate('(async()=>{await pull(600,500,900,500);await pull(800,800,500,800);await pull(200,800,500,800);})()');
    assert.equal(await evaluate("document.querySelectorAll('#analysis-circles circle').length"), 5);
    await checkAnalysisDeadzones();
    assert.equal(await evaluate("document.querySelectorAll('#stones circle').length"), 2);
    assert.equal(await evaluate('location.search'), positionUrl);
    await evaluate("[...document.querySelectorAll('#menu .icon')].find(e=>e.textContent.startsWith('*')).click();frame()");
    assert.equal(await evaluate('location.search'), positionUrl); // Passing is inert in analysis.
    await evaluate('tap(707,500)');
    await sleep(400);
    assert.equal(await evaluate("document.querySelectorAll('#analysis-circles circle').length"), 4);
    await checkAnalysisDeadzones();
    await evaluate('(async()=>{await tap(500,500);await tap(500,500);})()');
    assert.equal(await evaluate("document.querySelector('#analysis-toggle').getAttribute('aria-pressed')"), 'false');
    assert.equal(await evaluate("document.querySelectorAll('#analysis-layer').length"), 0);
    assert.equal(await evaluate("document.querySelector('#staged-stone circle').getAttribute('cx')"), proposedX);
    assert.equal(await evaluate("document.querySelectorAll('#stone-swept-area > g > g > circle').length"), 3);
    assert.equal(await evaluate('location.search'), positionUrl);
    await sleep(400); // The first tap's removal timer must not play or delete anything.
    assert.equal(await evaluate("document.querySelectorAll('#stones circle').length"), 2);
    await evaluate("document.querySelector('#analysis-toggle').click();frame()");
    assert.equal(await evaluate("document.querySelectorAll('#analysis-circles circle').length"), 4);
    await evaluate('(async()=>{await tap(500,500);await tap(500,500);await tap(500,500);})()');
    await wait("document.querySelectorAll('#stones circle').length===3");
    assert.notEqual(await evaluate('location.search'), positionUrl);
    await evaluate("document.querySelector('#analysis-toggle').click();frame()");
    assert.equal(await evaluate("document.querySelectorAll('#analysis-circles circle').length"), 3);
    await evaluate("document.querySelector('#score-toggle').click();frame()");
    assert.equal(await evaluate("document.querySelectorAll('#analysis-layer').length"), 0);
    await evaluate("document.querySelector('#analysis-toggle').click();frame()");
    assert.equal(await evaluate("document.querySelectorAll('#territory').length"), 0);
    assert.equal(await evaluate("document.querySelectorAll('#analysis-circles circle').length"), 3);

    // Ordinary placement snapping applies to hover previews and circle placement.
    await evaluate('move(450,500)');
    await checkAnalysisDeadzones();
    assert.equal(await evaluate("+document.querySelector('#analysis-circles .preview').getAttribute('cx')"), Number(proposedX) - 76);
    assert.equal(await evaluate("document.querySelectorAll('#analysis-links line').length > 0"), true);
    await evaluate('move(500,500)');
    assert.equal(await evaluate("document.querySelectorAll('#analysis-circles .preview').length"), 0);
    await evaluate('tap(450,500)');
    assert.equal(await evaluate("document.querySelectorAll('#analysis-circles circle').length"), 4);
    assert.equal(await evaluate("+document.querySelector('#analysis-circles circle:last-child').getAttribute('cx')"), Number(proposedX) - 76);
    assert.notEqual(await evaluate("getComputedStyle(document.querySelector('#board')).cursor"), 'crosshair');

    // A circle can fit physically even when proposing it would be suicide.
    const suicide = 'B[aMcm];W[aMdK];B[cmco];W[ckdM];B[dKcm];W[eddI];B[eCaO];W[fmcj];W[gaaM];W[cIaS];B[aMaM];W[]';
    await navigate('after', suicide);
    await evaluate(`
      window.tap = async (x,y) => { await move(x,y,'mousedown'); await move(x,y,'mouseup'); };
      document.querySelector('#analysis-toggle').click();
    `);
    await evaluate('(async()=>{await frame();await tap(138,44);await tap(138,44);await tap(138,44);})()');
    assert.equal(await evaluate("document.querySelectorAll('#analysis-circles .invalid').length"), 1);
    assert.equal(await evaluate("document.querySelectorAll('#staged-stone circle').length"), 0);
    assert.equal(await evaluate('location.search'), '?record=' + suicide);

    // Real touch events must not double-handle taps through synthetic mouse events.
    await c('Emulation.setDeviceMetricsOverride', { width: 900, height: 900, deviceScaleFactor: 1, mobile: true });
    await c('Emulation.setTouchEmulationEnabled', { enabled: true });
    await navigate('after');
    await evaluate("document.querySelector('#analysis-toggle').click();frame()");
    const touch = async (type, x = 500, y = 500) => {
      const point = await evaluate(`(()=>{const b=document.querySelector('#board').getBoundingClientRect();return {x:b.x+${x}*b.width/1000,y:b.y+${y}*b.height/1000};})()`);
      await c('Input.dispatchTouchEvent', { type, touchPoints: type === 'touchEnd' || type === 'touchCancel' ? [] : [point] });
      await evaluate('frame()');
    };
    const touchTap = async () => { await touch('touchStart'); await touch('touchEnd'); };
    await touchTap();
    await sleep(400);
    assert.equal(await evaluate("document.querySelectorAll('#analysis-circles circle').length"), 1);
    await touchTap();
    await sleep(400);
    assert.equal(await evaluate("document.querySelectorAll('#analysis-circles circle').length"), 0);
    await touchTap();
    await touch('touchStart'); await touch('touchMove', 900, 500); await touch('touchEnd', 900, 500);
    assert.equal(await evaluate("document.querySelectorAll('#analysis-circles circle').length"), 2);
    await touch('touchStart'); await touch('touchMove', 500, 900); await touch('touchCancel');
    assert.equal(await evaluate("document.querySelectorAll('#analysis-circles circle').length"), 2);
    await touchTap(); await touchTap();
    assert.equal(await evaluate("document.querySelectorAll('#staged-stone circle').length"), 1);
    assert.equal(await evaluate("document.querySelectorAll('#stones circle').length"), 0);
    await touchTap();
    await wait("document.querySelectorAll('#stones circle').length===1");
    assert.equal(await evaluate('visualViewport.scale'), 1);
    // Pinch zoom changes raster scale, not board coordinates or touch staging.
    await c('Emulation.setDeviceMetricsOverride', { width: 390, height: 390, deviceScaleFactor: 3, mobile: true });
    await c('Emulation.setPageScaleFactor', { pageScaleFactor: 3 });
    await touch('touchStart', 180, 180);
    for (const x of [200, 220, 240]) {
      await touch('touchMove', x, 240);
      assert.equal(await evaluate("+document.querySelector('#ghost-stone circle').getAttribute('cx')"), x);
      assert.equal(await evaluate("+document.querySelector('#ghost-stone circle').getAttribute('cy')"), 240);
    }
    await touch('touchEnd', 240, 240);
    assert.equal(await evaluate("+document.querySelector('#staged-stone circle').getAttribute('cx')"), 240);
    assert.equal(await evaluate("document.querySelectorAll('#stones circle').length"), 1);
    assert.equal(await evaluate('visualViewport.scale'), 3);

    // Exercise real two-finger zoom, including its visual-viewport offset.
    await c('Emulation.setPageScaleFactor', { pageScaleFactor: 1 });
    await navigate('after', record);
    await sleep(5500);
    const fingers = distance => [{ x: 195 - distance, y: 195, id: 1 }, { x: 195 + distance, y: 195, id: 2 }];
    await c('Input.dispatchTouchEvent', { type: 'touchStart', touchPoints: fingers(30) });
    for (let distance = 35; distance <= 90; distance += 5) {
      await c('Input.dispatchTouchEvent', { type: 'touchMove', touchPoints: fingers(distance) });
      await sleep(20);
    }
    await c('Input.dispatchTouchEvent', { type: 'touchEnd', touchPoints: [] });
    assert.ok(await evaluate('visualViewport.scale > 1'));

    // Invalidating a zoomed SVG must not leave rectangular stale mask tiles.
    for (const scale of [1.5, 3, 5]) {
      await c('Emulation.setPageScaleFactor', { pageScaleFactor: scale });
      await evaluate('move(500,500)');
      const before = (await c('Page.captureScreenshot', { format: 'png' })).data;
      await evaluate("document.querySelector('#board-position').style.display='none';frame()");
      await evaluate("document.querySelector('#board-position').style.display='';frame()");
      await sleep(150);
      const after = (await c('Page.captureScreenshot', { format: 'png' })).data;
      const changed = await evaluate(`(async () => {
        const pixels = async data => {
          const image = new Image();
          image.src = 'data:image/png;base64,' + data;
          await image.decode();
          const canvas = document.createElement('canvas');
          canvas.width = image.width; canvas.height = image.height;
          const context = canvas.getContext('2d', { willReadFrequently: true });
          context.drawImage(image, 0, 0);
          return context.getImageData(0, 0, canvas.width, canvas.height).data;
        };
        const a = await pixels(${JSON.stringify(before)}), b = await pixels(${JSON.stringify(after)});
        let changed = 0;
        for (let i = 0; i < a.length; i += 4) {
          if ([0,1,2].some(c => Math.abs(a[i+c]-b[i+c]) > 16)) changed++;
        }
        return changed / (a.length / 4);
      })()`);
      assert.ok(changed < 0.001, `${scale}× zoom: ${(100*changed).toFixed(2)}% of pixels changed after repaint`);
    }
    assert.equal(errors.length, 0, JSON.stringify(errors));
    console.log('Browser checks passed: play previews/staging/scoring, analysis geometry and both-colour dragging, delayed removal, exact proposals, suicide rejection, persistence, touch and cancellation.');
  } finally {
    await call('Target.closeTarget', { targetId });
    ws.close();
  }
})().catch(error => { console.error(error); process.exitCode = 1; });
