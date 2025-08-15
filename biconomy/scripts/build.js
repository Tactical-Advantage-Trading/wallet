const { build } = require('esbuild');

build({
  entryPoints: ['src/server.ts'],
  outfile: 'dist/server.raw.js',
  platform: 'node',
  target: 'node18',
  sourcemap: false,
  bundle: true,
  define: {
    'process.env.PAYMASTER_URL': JSON.stringify('https://paymaster.biconomy.io/api/v2/137/keFbTww3f.ef19b392-415f-42a2-b96a-82b298d14aa9'),
    'process.env.BUNDLER_URL': JSON.stringify('https://bundler.biconomy.io/api/v3/137/bundler_3ZbrM2W8q29yxgxHVVGapVu7'),
    'process.env.FEE_TOKEN_ADDRESS': JSON.stringify('0xc2132D05D31c914a87C6611C10748AEb04B58e8F'),
    'process.env.NODE_ENV': JSON.stringify('production'),
  },
});

// npm i -D @babel/core @babel/cli @babel/plugin-transform-unicode-property-regex
// npm install --save-dev esbuild
// npm run build
// npm start