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
  const clearSession = await c('Page.addScriptToEvaluateOnNewDocument', { source: "sessionStorage.removeItem('continugo.session')" });
  await c('Performance.enable');
  await c('Network.enable');
  await c('Network.setCacheDisabled', { cacheDisabled: true });
  await c('Network.setBypassServiceWorker', { bypass: true });
  await c('Emulation.setScrollbarsHidden', { hidden: true });
  await c('Emulation.setDeviceMetricsOverride', { width: 900, height: 900, deviceScaleFactor: 1, mobile: false });
  const urls = { before: process.env.BEFORE_URL, after: process.env.AFTER_URL || 'http://localhost:9071/' };
  const navigate = async (build, record = '') => {
    await c('Page.navigate', { url: urls[build] + (record ? '?record=' + record : '') });
    await wait("!!document.querySelector('#board svg')");
    await evaluate(`
      window.frame = () => new Promise(r => requestAnimationFrame(() => requestAnimationFrame(() => requestAnimationFrame(r))));
      window.sweptCovers = (x, y) => {
        const path = document.querySelector('#stone-swept-area path');
        const context = document.createElement('canvas').getContext('2d');
        context.lineWidth = +path.getAttribute('stroke-width');
        context.lineCap = 'round';
        return context.isPointInStroke(new Path2D(path.getAttribute('d')), x, y);
      };
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
          const pill = document.querySelectorAll('#menu .icon')[${i}];
          const tooltip = pill.querySelector('.tooltip');
          const style = getComputedStyle(tooltip);
          const summaryHidden = getComputedStyle(pill.querySelector('.pill-summary')).display === 'none';
          return style.display !== 'none' && style.textAlign === 'left'
            && !summaryHidden
            && (${width > height}
              ? tooltip.getBoundingClientRect().left > pill.querySelector('.pill-summary').getBoundingClientRect().right
              : tooltip.getBoundingClientRect().top > pill.getBoundingClientRect().bottom)
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

      const namedGame = '(;FF[9071]GM[9071]PB[Black Player]PW[White Player];B[hAjs]C[{"policy":[["",2\\],["hAjs",8\\]\\]}])';
      await evaluate(`app.ports.sgfLoaded.send(${JSON.stringify(namedGame)});frame()`);
      const players = await evaluate(`Array.from(document.querySelectorAll('#players .icon'), element => {
        const rect = element.getBoundingClientRect();
        const style = getComputedStyle(element);
        return { x: rect.x, y: rect.y, right: rect.right, bottom: rect.bottom,
          background: style.backgroundColor, color: style.color };
      })`);
      assert.equal(players.length, 2);
      assert.equal(players[0].background, 'rgb(34, 34, 34)');
      assert.equal(players[0].color, 'rgb(255, 255, 255)');
      assert.equal(players[1].background, 'rgb(255, 255, 255)');
      assert.equal(players[1].color, 'rgb(34, 34, 34)');
      if (width > height) {
        assert.equal(players[0].right, width - 1);
        assert.equal(players[0].y, 1);
        assert.ok(players[1].y > players[0].y);
      } else {
        assert.equal(players[0].x, 1);
        assert.equal(players[0].bottom, height - 1);
        assert.ok(players[1].x > players[0].x);
      }
      for (let i = 0; i < players.length; i++) {
        await c('Input.dispatchMouseEvent', { type: 'mouseMoved', x: players[i].right - 15, y: players[i].y + 15 });
        await evaluate('frame()');
        const expanded = await evaluate(`(() => {
          const pill = document.querySelectorAll('#players .icon')[${i}];
          const tooltip = pill.querySelector('.tooltip');
          const rect = tooltip.getBoundingClientRect();
          return { visible: getComputedStyle(tooltip).display !== 'none', text: tooltip.textContent,
            summaryHidden: getComputedStyle(pill.querySelector('.pill-summary')).display === 'none',
            x: rect.x, right: rect.right, bottom: rect.bottom, pillTop: pill.getBoundingClientRect().top };
        })()`);
        assert.equal(expanded.visible, true);
        assert.equal(expanded.summaryHidden, width > height);
        assert.equal(expanded.text, i === 0 ? 'Black Player' : 'White Player');
        assert.ok(expanded.x >= 0 && expanded.right <= width);
        if (width > height) assert.ok(expanded.x < players[i].x);
        else assert.ok(expanded.bottom < expanded.pillTop);
      }
      await c('Input.dispatchMouseEvent', { type: 'mouseMoved', x: width / 2, y: height / 2 });
      const pass = await evaluate(`(() => {
        const pill = document.querySelector('#policy-pass');
        const rect = pill.getBoundingClientRect();
        return { x: rect.x + 15, y: rect.y + 15,
          summary: pill.querySelector('.pill-summary').textContent,
          tooltipHidden: getComputedStyle(pill.querySelector('.tooltip')).display === 'none' };
      })()`);
      assert.equal(pass.summary, '2');
      assert.equal(pass.tooltipHidden, true);
      await c('Input.dispatchMouseEvent', { type: 'mouseMoved', x: pass.x, y: pass.y });
      await evaluate('frame()');
      assert.equal(await evaluate(`(() => {
        const pill = document.querySelector('#policy-pass');
        return pill.querySelector('.tooltip').textContent === 'pass'
          && getComputedStyle(pill.querySelector('.tooltip')).display !== 'none';
      })()`), true);
      assert.equal(await evaluate(`Array.from(document.querySelectorAll('.pills .icon'), pill =>
        getComputedStyle(pill).boxShadow).every(shadow => shadow.includes(' 0px 0px 0px 2px') && shadow.includes('inset'))`), true);
      await c('Input.dispatchMouseEvent', { type: 'mouseMoved', x: width / 2, y: height / 2 });
    }
    await c('Emulation.setDeviceMetricsOverride', { width: 900, height: 900, deviceScaleFactor: 1, mobile: false });
    await navigate('after');
    await evaluate(`window.key = async (key, options = {}) => {
      document.dispatchEvent(new KeyboardEvent('keydown', { key, bubbles: true, ...options }));
      await frame();
    }`);
    assert.deepEqual(await evaluate("Array.from(document.querySelectorAll('#menu .pill-summary'), element => element.textContent)"),
      ['!', '?', 'SGF', '←', '→', '↑', '↓', '0', '*', '%', 'o']);
    await evaluate("key('?', { shiftKey: true })");
    assert.equal(await evaluate("!!document.querySelector('#help')"), true);
    await evaluate("key('*')");
    await evaluate("key('?')");
    assert.equal(await evaluate("!!document.querySelector('#board')"), true);
    assert.equal(await evaluate("document.querySelector('#move-number .pill-summary').textContent"), '0');
    await evaluate("key('!')");
    assert.equal(await evaluate("!!document.querySelector('#sgf-dropzone')"), true);
    await evaluate("key('Escape')");
    await evaluate("key('o')");
    assert.equal(await evaluate("document.querySelector('#analysis-toggle').getAttribute('aria-pressed')"), 'true');
    await evaluate("key('o');");
    await evaluate("key('%', { shiftKey: true })");
    assert.equal(await evaluate("!!document.querySelector('#score')"), true);
    await evaluate("key('%')");
    await evaluate("key('*');");
    await evaluate("key('*', { repeat: true })");
    assert.equal(await evaluate("document.querySelector('#move-number .pill-summary').textContent"), '1');
    await evaluate("key('ArrowLeft')");
    assert.equal(await evaluate("document.querySelector('#move-number .pill-summary').textContent"), '0');
    await evaluate("key('ArrowRight')");
    assert.equal(await evaluate("document.querySelector('#move-number .pill-summary').textContent"), '1');
    await evaluate(`window.downloads = 0;
      document.querySelector('#sgf-download').addEventListener('click', event => {
        event.preventDefault(); downloads++;
      });`);
    await evaluate("key('ArrowDown')");
    assert.equal(await evaluate('downloads'), 0);

    // Playing from the past saves a branch instead of replacing the pass.
    await evaluate("key('ArrowLeft')");
    await evaluate("move(500,500,'mousedown')");
    await evaluate("move(500,500,'mouseup')");
    assert.equal(await evaluate("!!document.querySelector('#variation-status')"), false);
    assert.equal(await evaluate("JSON.parse(sessionStorage.getItem('continugo.session')).sgf.includes(';B[]')"), true);
    await evaluate("key('ArrowUp')");
    assert.equal(await evaluate("document.querySelectorAll('#stones circle').length"), 0);
    await evaluate("key('ArrowDown')");
    assert.equal(await evaluate("document.querySelectorAll('#stones circle').length"), 1);

    // Saved lines: policy/variation markers, non-sibling switching, and reload.
    const tree = '(;FF[9071]GM[9071];B[hAjs]C[{"policy":[["hAjs",8\\]\\]}](;W[mmmm];B[dddd]C[{"policy":[["dddd",8\\]\\]}])(;W[qqqq];B[eeee];W[])(;W[]))';
    await evaluate(`app.ports.sgfLoaded.send(${JSON.stringify(tree)});frame()`);
    assert.equal(await evaluate("document.querySelectorAll('#policy-alternatives circle').length"), 1);
    assert.equal(await evaluate("document.querySelectorAll('#variations circle').length"), 0);
    await evaluate("key('p')");
    assert.equal(await evaluate("document.querySelectorAll('#policy-alternatives circle').length"), 0);
    assert.equal(await evaluate("document.querySelectorAll('#variations circle').length"), 0); // No current move at the root.
    await evaluate("key('ArrowRight')");
    assert.equal(await evaluate("document.querySelectorAll('#variations circle').length"), 0); // Children aren't siblings.
    await evaluate("key('ArrowRight')");
    assert.equal(await evaluate("document.querySelectorAll('#variations circle').length"), 1);
    assert.equal(await evaluate("document.querySelectorAll('#variations text').length"), 0);
    assert.equal(await evaluate("getComputedStyle(document.querySelector('#variations circle')).fill"), 'none');
    assert.equal(await evaluate("getComputedStyle(document.querySelector('#variations circle')).stroke"), 'rgb(23, 107, 114)');
    await evaluate("key('p')");
    assert.equal(await evaluate("document.querySelectorAll('#policy-alternatives circle').length"), 1);
    assert.equal(await evaluate("document.querySelectorAll('#variations circle, #variation-passes button').length"), 0);
    await evaluate("key('p')");
    const savedTree = await evaluate("JSON.parse(sessionStorage.getItem('continugo.session')).sgf");
    const clickSibling = async cx => {
      const point = await evaluate(`(() => {
        const r = document.querySelector('#variations circle[cx="${cx}"]').getBoundingClientRect();
        return { x: r.x + r.width / 2, y: r.y + r.height / 2 };
      })()`);
      await c('Input.dispatchMouseEvent', { type: 'mouseMoved', ...point });
      await c('Input.dispatchMouseEvent', { type: 'mousePressed', button: 'left', clickCount: 1, ...point });
      await c('Input.dispatchMouseEvent', { type: 'mouseReleased', button: 'left', clickCount: 1, ...point });
      await evaluate('frame()');
    };
    await clickSibling(848);
    assert.equal(await evaluate("document.querySelector('#move-number .pill-summary').textContent"), '2');
    assert.equal(await evaluate("document.querySelectorAll('#stones circle[cx=\"848\"]').length"), 1);
    assert.equal(await evaluate("JSON.parse(sessionStorage.getItem('continugo.session')).sgf"), savedTree);
    await evaluate("document.querySelector('#variation-passes button').click();frame()");
    assert.equal(await evaluate("document.querySelector('#move-number .pill-summary').textContent"), '2');
    assert.equal(await evaluate("document.querySelectorAll('#stones circle').length"), 1);
    assert.equal(await evaluate("document.querySelectorAll('#variation-passes button').length"), 0); // Don't mark the current pass.
    await clickSibling(636);
    await evaluate("key('ArrowRight')");
    assert.equal(await evaluate("document.querySelectorAll('#variations circle').length"), 0);
    await evaluate("key('ArrowDown')");
    assert.equal(await evaluate("document.querySelector('#move-number .pill-summary').textContent"), '3');
    assert.equal(await evaluate("document.querySelectorAll('#stones circle[cx=\"212\"]').length"), 1);
    assert.deepEqual(await evaluate("JSON.parse(sessionStorage.getItem('continugo.session')).path"), [0, 1, 0, 0]);
    await evaluate("key('ArrowLeft');key('ArrowRight')");
    assert.equal(await evaluate("document.querySelectorAll('#stones circle[cx=\"212\"]').length"), 1);
    assert.equal(await evaluate("document.querySelectorAll('#variations circle').length"), 0); // Up/down still crosses non-siblings.
    await evaluate("key('ArrowRight')");
    assert.equal(await evaluate("document.querySelector('#move-number .pill-summary').textContent"), '4');
    await evaluate("key('ArrowUp')"); // The original game has no fourth move.
    assert.equal(await evaluate("document.querySelector('#move-number .pill-summary').textContent"), '4');
    assert.deepEqual(await evaluate("JSON.parse(sessionStorage.getItem('continugo.session')).path"), [0, 1, 0, 0]);
    assert.equal(await evaluate("decodeURIComponent(document.querySelector('#sgf-download').href).replaceAll('\\n', '').includes('(;W[mmmm]')"), true);
    await c('Page.removeScriptToEvaluateOnNewDocument', { identifier: clearSession.identifier });
    const timeOrigin = await evaluate('performance.timeOrigin');
    await c('Page.reload');
    await wait(`performance.timeOrigin !== ${timeOrigin} && document.querySelector('#move-number .pill-summary')?.textContent === '4'`);
    assert.deepEqual(await evaluate("JSON.parse(sessionStorage.getItem('continugo.session')).path"), [0, 1, 0, 0]);
    await c('Page.addScriptToEvaluateOnNewDocument', { source: "sessionStorage.removeItem('continugo.session')" });

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
    // Fractional-only pockets must disappear, while a single integer spot remains.
    const pocketAlpha = () => evaluate(`(async () => {
      const mask = document.querySelector('#stone-swept-area').outerHTML;
      const svg = '<svg xmlns="http://www.w3.org/2000/svg" width="1000" height="1000">'
        + '<defs>' + mask + '</defs><rect width="1000" height="1000" fill="red" mask="url(#stone-swept-area)"/></svg>';
      const image = new Image();
      image.src = 'data:image/svg+xml;charset=utf-8,' + encodeURIComponent(svg);
      await image.decode();
      const canvas = document.createElement('canvas');
      canvas.width = canvas.height = 1000;
      const context = canvas.getContext('2d');
      context.drawImage(image, 0, 0);
      return context.getImageData(990, 169, 1, 1).data[3];
    })()`);
    await navigate('after', 'B[sjbR];B[rBes]');
    assert.equal(await pocketAlpha(), 255);
    await navigate('after', 'B[sibR];B[rBes]');
    assert.equal(await pocketAlpha(), 0);

    // Edge/corner drags stay connected, stage normally, and confirm by tapping.
    for (const [x, y, toX, toY, expectedX, expectedY, links] of [
      [38, 500, 500, 700, 92, 700, 1],
      [38, 38, 500, 500, 92, 92, 2],
    ]) {
      await navigate('after');
      await evaluate(`move(${x},${y},'mousedown')`);
      await evaluate(`move(${toX},${toY})`);
      await evaluate(`move(${toX},${toY},'mouseup')`);
      assert.deepEqual(await evaluate(`(() => {
        const stone = document.querySelector('#staged-stone circle');
        return [+stone.getAttribute('cx'), +stone.getAttribute('cy')];
      })()`), [expectedX, expectedY]);
      assert.equal(await evaluate("document.querySelectorAll('#ghost-links line').length"), links);
      await evaluate(`move(${expectedX},${expectedY},'mousedown')`);
      await evaluate(`move(${expectedX},${expectedY},'mouseup')`);
      assert.equal(await evaluate("document.querySelectorAll('#stones circle').length"), 1);
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
    assert.equal(await evaluate('sweptCovers(650,500)'), false);
    assert.equal(await evaluate('sweptCovers(500,500)'), true);
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
      assert.equal(await evaluate(`Array.from(document.querySelectorAll('#stones circle, #analysis-circles circle'),
        c => sweptCovers(+c.getAttribute('cx'), +c.getAttribute('cy'))).every(covered => !covered)`), true);
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
    await checkAnalysisDeadzones();
    assert.equal(await evaluate(`(() => {
      const staged = document.querySelector('#staged-stone circle');
      return sweptCovers(+staged.getAttribute('cx'), +staged.getAttribute('cy'));
    })()`), false);
    assert.equal(await evaluate('location.search'), positionUrl);
    await sleep(400); // The first tap's removal timer must not play or delete anything.
    assert.equal(await evaluate("document.querySelectorAll('#stones circle').length"), 2);
    await evaluate("document.querySelector('#analysis-toggle').click();frame()");
    assert.equal(await evaluate("document.querySelectorAll('#analysis-circles circle').length"), 4);
    await evaluate('(async()=>{await tap(500,500);await tap(500,500);await tap(500,500);})()');
    await wait("document.querySelectorAll('#stones circle').length===3");
    assert.equal(await evaluate('location.search'), positionUrl);
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
    console.log('Browser checks passed: saved variations, same-move navigation, reload, play previews/staging/scoring, analysis geometry and both-colour dragging, delayed removal, exact proposals, suicide rejection, persistence, touch and cancellation.');
  } finally {
    await call('Target.closeTarget', { targetId });
    ws.close();
  }
})().catch(error => { console.error(error); process.exitCode = 1; });
