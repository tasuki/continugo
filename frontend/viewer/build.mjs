import { execFileSync } from 'node:child_process';
import { mkdtempSync, mkdirSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';

const frontend = fileURLToPath(new URL('../', import.meta.url));
const temporary = mkdtempSync(join(tmpdir(), 'continugo-viewer-'));
try {
  const elm = join(temporary, 'elm.js');
  execFileSync('elm', ['make', 'src/Viewer.elm', '--optimize', `--output=${elm}`], {
    cwd: frontend, stdio: 'inherit',
  });
  const read = path => readFileSync(join(frontend, path), 'utf8');
  const css = read('public/media/board.css') + '\n' + read('viewer/style.css');
  const bundle = `(function () {
const runtime = {};
(function () { ${readFileSync(elm, 'utf8')} }).call(runtime);
const Elm = runtime.Elm;
const CSS = ${JSON.stringify(css)};
${read('viewer/index.js')}
})();\n`;
  const output = join(frontend, 'viewer/dist');
  mkdirSync(output, { recursive: true });
  const minified = execFileSync('uglifyjs', [
    '--compress', 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe',
    '--mangle',
  ], { input: bundle, maxBuffer: 10 * 1024 * 1024 });
  writeFileSync(join(output, 'continugo-viewer.js'), minified);
  console.log('Built viewer/dist/continugo-viewer.js');
} finally {
  rmSync(temporary, { recursive: true, force: true });
}
