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
      window.move = async (x, y, type = 'mousemove') => {
        const b = document.querySelector('#board').getBoundingClientRect();
        document.querySelector('#board-container').dispatchEvent(new MouseEvent(type, {
          bubbles: true, clientX: b.x + x*b.width/1000, clientY: b.y + y*b.height/1000
        }));
        await frame();
      };
    `);
    await evaluate('frame()');
  };
  const record = 'B[oifh];W[fgoc];B[odpz];W[fhfm];B[ihdL];W[lfem];B[ekpj];W[fsqe];B[dWnk];W[eSmd];B[dGll];W[dsqt];B[fHkR];W[iogz];B[jAfl];W[kPgM];B[gMfX];W[gUez];B[jnhQ];W[hIit];B[lLfD];W[mRgQ];B[mLdR];W[jPcR];B[iHbT];W[hqcz];B[kJbI];W[mbcC];B[nrbT];W[hLaM];B[lZaM];W[jKjx];B[lfiC];W[lCko];B[mVjo];W[nAkG];B[oQjC];W[pCkQ];B[cQim];W[qrmH];B[pYii];W[qNju];B[regx];W[oshO];B[codi];W[dhfM];B[hNlD];W[pvgk]';
  const metrics = async () => Object.fromEntries((await c('Performance.getMetrics')).metrics.map(m => [m.name, m.value]));
  try {
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
      await evaluate(`window.mutations=0; new MutationObserver(ms=>{mutations+=ms.length}).observe(document.querySelector('#board svg'),{attributes:true,childList:true,subtree:true});`);
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
    assert.equal(errors.length, 0, JSON.stringify(errors));
    console.log('Browser checks passed: hover, capture cancellation, unchanged position/URL, staging, scoring, committing.');
  } finally {
    await call('Target.closeTarget', { targetId });
    ws.close();
  }
})().catch(error => { console.error(error); process.exitCode = 1; });
