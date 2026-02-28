import { render, screen, fireEvent } from '@testing-library/react'
import { describe, it, expect, vi, beforeEach } from 'vitest'
import { KnowledgeForm } from './KnowledgeForm'
import type { KnowledgeConfig, SearchResult } from '../../../types'

// ---------------------------------------------------------------------------
// Mock useKnowledgeSearch
// ---------------------------------------------------------------------------

vi.mock('../../../api/hooks', () => ({
  useKnowledgeSearch: vi.fn(),
}))

// Also mock the Monaco editor — it isn't needed for these tests
vi.mock('@monaco-editor/react', () => ({
  default: ({ value, onChange }: { value: string; onChange: (v: string) => void }) => (
    <textarea data-testid="monaco-editor" value={value} onChange={(e) => onChange(e.target.value)} />
  ),
}))

import { useKnowledgeSearch } from '../../../api/hooks'

// ---------------------------------------------------------------------------
// Fixtures
// ---------------------------------------------------------------------------

const ENTRY_1: SearchResult['entry'] = {
  id: 'entry-1',
  sourceType: 'manual',
  category: 'summary',
  title: 'Sprint Goals',
  content: 'Focus on shipping EVA-82',
  confidence: 1.0,
  isEdited: true,
  createdAt: '2026-02-01T00:00:00Z',
  updatedAt: '2026-02-01T00:00:00Z',
  scannedAt: '2026-02-01T00:00:00Z',
}

const ENTRY_2: SearchResult['entry'] = {
  id: 'entry-2',
  sourceType: 'codebase',
  category: 'pattern',
  title: 'Handler pattern',
  content: 'Use AppM monad for all handlers',
  confidence: 0.87,
  isEdited: false,
  createdAt: '2026-02-01T00:00:00Z',
  updatedAt: '2026-02-01T00:00:00Z',
  scannedAt: '2026-02-01T00:00:00Z',
}

const RESULTS: SearchResult[] = [
  { entry: ENTRY_1, score: 1.0 },
  { entry: ENTRY_2, score: 0.87 },
]

const BASE_CONFIG: KnowledgeConfig = {
  source: { type: '_inline_text', value: 'Some inline content' },
  format: 'text',
  refreshPolicy: { type: 'static' },
}

const LIBRARY_CONFIG: KnowledgeConfig = {
  source: { type: '_library_ref', value: 'entry-1' },
  format: 'text',
  refreshPolicy: { type: 'static' },
}

function renderForm(
  config: KnowledgeConfig = BASE_CONFIG,
  onChange = vi.fn(),
  programId = 'prog-1',
) {
  return { onChange, ...render(<KnowledgeForm config={config} onChange={onChange} programId={programId} />) }
}

beforeEach(() => {
  vi.mocked(useKnowledgeSearch).mockReturnValue({
    data: RESULTS,
    isLoading: false,
  } as unknown as ReturnType<typeof useKnowledgeSearch>)
})

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

describe('KnowledgeForm — source tabs', () => {
  it('renders all 4 source tabs', () => {
    renderForm()
    expect(screen.getByRole('button', { name: 'Inline' })).toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'File' })).toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'URL' })).toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'Library' })).toBeInTheDocument()
  })

  it('File and URL tabs are disabled', () => {
    renderForm()
    expect(screen.getByRole('button', { name: 'File' })).toBeDisabled()
    expect(screen.getByRole('button', { name: 'URL' })).toBeDisabled()
  })

  it('Library tab is enabled', () => {
    renderForm()
    expect(screen.getByRole('button', { name: 'Library' })).not.toBeDisabled()
  })
})

describe('KnowledgeForm — Library tab', () => {
  it('clicking Library tab calls onChange with _library_ref source', () => {
    const { onChange } = renderForm()
    fireEvent.click(screen.getByRole('button', { name: 'Library' }))
    expect(onChange).toHaveBeenCalledWith(
      expect.objectContaining({ source: { type: '_library_ref', value: '' } }),
    )
  })

  it('shows library picker when source is _library_ref', () => {
    renderForm(LIBRARY_CONFIG)
    expect(screen.getByTestId('library-search')).toBeInTheDocument()
  })

  it('does not show inline editor when source is _library_ref', () => {
    renderForm(LIBRARY_CONFIG)
    expect(screen.queryByTestId('monaco-editor')).not.toBeInTheDocument()
  })

  it('renders knowledge entries in the picker', () => {
    renderForm(LIBRARY_CONFIG)
    expect(screen.getAllByText('Sprint Goals').length).toBeGreaterThanOrEqual(1)
    expect(screen.getByText('Handler pattern')).toBeInTheDocument()
  })

  it('clicking an entry calls onChange with that entry id', () => {
    const { onChange } = renderForm(LIBRARY_CONFIG)
    fireEvent.click(screen.getByTestId('entry-entry-2'))
    expect(onChange).toHaveBeenCalledWith(
      expect.objectContaining({ source: { type: '_library_ref', value: 'entry-2' } }),
    )
  })

  it('shows selected entry badge when a valid entry is selected', () => {
    renderForm(LIBRARY_CONFIG)
    expect(screen.getByText(/Selected:/)).toBeInTheDocument()
    expect(screen.getAllByText('Sprint Goals').length).toBeGreaterThanOrEqual(1)
  })

  it('typing in search input updates the search text (passes to hook)', () => {
    renderForm(LIBRARY_CONFIG)
    const searchInput = screen.getByTestId('library-search')
    fireEvent.change(searchInput, { target: { value: 'handler' } })
    expect(useKnowledgeSearch).toHaveBeenCalledWith('prog-1', 'handler')
  })

  it('shows empty state when no entries exist', () => {
    vi.mocked(useKnowledgeSearch).mockReturnValue({
      data: [],
      isLoading: false,
    } as unknown as ReturnType<typeof useKnowledgeSearch>)
    renderForm(LIBRARY_CONFIG)
    expect(screen.getByText(/No knowledge entries for this program/)).toBeInTheDocument()
  })

  it('shows loading state', () => {
    vi.mocked(useKnowledgeSearch).mockReturnValue({
      data: undefined,
      isLoading: true,
    } as unknown as ReturnType<typeof useKnowledgeSearch>)
    renderForm(LIBRARY_CONFIG)
    expect(screen.getByText('Loading…')).toBeInTheDocument()
  })

  it('shows warning when programId is empty', () => {
    renderForm(LIBRARY_CONFIG, vi.fn(), '')
    expect(screen.getByText(/Save the program first/)).toBeInTheDocument()
  })
})
