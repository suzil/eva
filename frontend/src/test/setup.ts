import '@testing-library/jest-dom'

// react-flow (and other layout-dependent libs) use ResizeObserver which jsdom lacks.
class ResizeObserverMock {
  observe() {}
  unobserve() {}
  disconnect() {}
}
;(window as Window & typeof globalThis & { ResizeObserver: typeof ResizeObserverMock }).ResizeObserver = ResizeObserverMock
