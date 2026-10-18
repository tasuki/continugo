// Compile first: elm make benchmarks/Benchmark.elm --optimize --output=/tmp/continugo-benchmark.js
const { performance } = require('node:perf_hooks');
const { Elm } = require(process.argv[2] || '/tmp/continugo-benchmark.js');
const app = Elm.Benchmark.init({ flags: null });
let checksum, finished;
app.ports.done.subscribe(value => { checksum = value; finished(); });

(async () => {
  for (const [scenario, iterations] of [
    ['blocked-liberties', 1000], ['capture', 100], ['hover', 100],
    ['deadzones', 100], ['liberty-geometry', 100],
  ]) {
    const times = [];
    for (let trial = 0; trial < 6; trial++) {
      const result = new Promise(resolve => { finished = resolve; });
      const start = performance.now();
      app.ports.run.send({ scenario, iterations });
      const elapsed = performance.now() - start;
      await result;
      if (trial) times.push(elapsed / iterations);
    }
    times.sort((a, b) => a - b);
    console.log(`${scenario}: ${times[2].toFixed(3)} ms/op (checksum ${checksum})`);
  }
})().catch(error => { console.error(error); process.exitCode = 1; });
