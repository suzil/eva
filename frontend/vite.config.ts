import { defineConfig } from 'vitest/config'
import react from '@vitejs/plugin-react'

export default defineConfig({
  plugins: [react()],
  optimizeDeps: {
    // Monaco is a large ESM package with circular-looking internals. Excluding
    // it from Vite's pre-bundler avoids spurious circular-dependency warnings
    // and keeps each worker chunk cleanly code-split from main.js.
    exclude: ['monaco-editor'],
  },
  build: {
    rollupOptions: {
      output: {
        // Explicitly split Monaco editor and the YAML worker into named chunks
        // so neither is inlined into the main bundle. The main bundle must stay
        // under 500 KB gzipped (EVA-106 performance audit).
        //
        // Measured chunk sizes after `npm run build` (EVA-106):
        //   monaco-editor.[hash].js   3,773 KB unminified / 974 KB gzipped
        //   monaco-yaml.[hash].js        21 KB unminified /   8 KB gzipped
        //   editor.worker.[hash].js     252 KB unminified (dynamic worker)
        //   yaml.worker.[hash].js       723 KB unminified (dynamic worker)
        //   index.[hash].js           1,022 KB unminified / 310 KB gzipped ✓
        manualChunks: {
          'monaco-editor': ['monaco-editor'],
          'monaco-yaml': ['monaco-yaml'],
        },
      },
    },
  },
  server: {
    proxy: {
      '/api': {
        target: 'http://localhost:8080',
        ws: true,
      },
    },
  },
  test: {
    globals: true,
    environment: 'jsdom',
    setupFiles: ['./src/test/setup.ts'],
  },
})
