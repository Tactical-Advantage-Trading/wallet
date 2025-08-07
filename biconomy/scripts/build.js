const { build } = require('esbuild');

build({
  entryPoints: ['src/server.ts'],
  platform: 'node',
  target: 'node18',
  sourcemap: false,
  outdir: 'dist',
  bundle: true,
});

// npm install --save-dev esbuild
// npm run build
// npm start