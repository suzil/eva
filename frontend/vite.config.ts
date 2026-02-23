import { defineConfig } from 'vitest/config'
import react from '@vitejs/plugin-react'

export default defineConfig({
  plugins: [react()],
  optimizeDeps: {
    // Monaco is a large ESM package with circular-looking internals. Excluding
    // it from Vite's pre-bundler avoids spurious circular-dependency warnings
    // and keeps each worker chunk cleanly code-split from main.js.
    //
    // Monaco bundle chunks (EVA-58, unminified sizes):
    //   editor.worker.[hash].js   252 KB  (editor core)
    //   yaml.worker.[hash].js     723 KB  (monaco-yaml + yaml-language-server)
    // Both are code-split by Vite; not inlined into main.js.
    exclude: ['monaco-editor'],
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
