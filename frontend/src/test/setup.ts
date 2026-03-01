import '@testing-library/jest-dom'
import { toHaveNoViolations } from 'jest-axe'

expect.extend(toHaveNoViolations)

// Extend vitest's expect with the jest-axe custom matcher
declare module 'vitest' {
  interface Matchers<R = void> {
    toHaveNoViolations(): R
  }
}

// react-flow (and other layout-dependent libs) use ResizeObserver which jsdom lacks.
class ResizeObserverMock {
  observe() {}
  unobserve() {}
  disconnect() {}
}
;(window as Window & typeof globalThis & { ResizeObserver: typeof ResizeObserverMock }).ResizeObserver = ResizeObserverMock

// Make requestAnimationFrame synchronous so that RAF-debounced state updates
// are visible within the same act() call in tests. Production keeps the real
// browser RAF which batches renders to ~60fps.
window.requestAnimationFrame = (callback: FrameRequestCallback): number => {
  callback(performance.now())
  return 0
}
window.cancelAnimationFrame = () => {}
