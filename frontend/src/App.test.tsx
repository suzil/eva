import { render, screen } from '@testing-library/react'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { describe, it, expect } from 'vitest'
import App from './App.tsx'

function wrapper({ children }: { children: React.ReactNode }) {
  const client = new QueryClient({ defaultOptions: { queries: { retry: false } } })
  return <QueryClientProvider client={client}>{children}</QueryClientProvider>
}

describe('App', () => {
  it('renders Eva heading', () => {
    render(<App />, { wrapper })
    expect(screen.getByRole('heading', { name: 'Eva' })).toBeInTheDocument()
  })
})
