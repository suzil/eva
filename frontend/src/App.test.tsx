import { render, screen, within } from '@testing-library/react'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { describe, it, expect } from 'vitest'
import App from './App.tsx'

function wrapper({ children }: { children: React.ReactNode }) {
  const client = new QueryClient({ defaultOptions: { queries: { retry: false } } })
  return <QueryClientProvider client={client}>{children}</QueryClientProvider>
}

describe('App', () => {
  it('renders the app shell with activity bar', () => {
    render(<App />, { wrapper })
    expect(screen.getByRole('complementary')).toBeInTheDocument() // ActivityBar aside
    expect(screen.getByRole('banner')).toBeInTheDocument()        // Toolbar header
  })

  it('renders all activity bar navigation items', () => {
    render(<App />, { wrapper })
    // Scope to the ActivityBar <aside> to avoid matching the Toolbar breadcrumb 'Programs' button
    const aside = screen.getByRole('complementary')
    expect(within(aside).getByRole('button', { name: 'Programs' })).toBeInTheDocument()
    expect(within(aside).getByRole('button', { name: 'Node Palette' })).toBeInTheDocument()
    expect(within(aside).getByRole('button', { name: 'Knowledge' })).toBeInTheDocument()
    expect(within(aside).getByRole('button', { name: 'Runs' })).toBeInTheDocument()
    expect(within(aside).getByRole('button', { name: 'Settings' })).toBeInTheDocument()
  })

  it('renders the Author/Operate mode toggle', () => {
    render(<App />, { wrapper })
    expect(screen.getByRole('button', { name: 'author' })).toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'operate' })).toBeInTheDocument()
  })

  it('renders the bottom panel tab bar', () => {
    render(<App />, { wrapper })
    expect(screen.getByRole('button', { name: 'Logs' })).toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'Output' })).toBeInTheDocument()
  })
})
