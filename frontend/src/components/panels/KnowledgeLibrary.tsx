import { useState, useEffect, useRef } from 'react'
import { Search, ChevronDown, ChevronRight, Loader2 } from 'lucide-react'
import { useUiStore } from '../../store/uiStore'
import { useCanvasStore } from '../../store/canvasStore'
import { useKnowledgeEntries, useKnowledgeSearch } from '../../api/hooks'
import type { KnowledgeEntry } from '../../types'

// ---------------------------------------------------------------------------
// Source badge config
// ---------------------------------------------------------------------------

const SOURCE_BADGE: Record<string, { label: string; className: string }> = {
  codebase: { label: 'codebase', className: 'bg-at-field-900 text-at-field-300' },
  linear:   { label: 'linear',   className: 'bg-eva-green-900 text-eva-green-400' },
  manual:   { label: 'manual',   className: 'bg-terminal-700 text-terminal-300' },
  github:   { label: 'github',   className: 'bg-terminal-700 text-terminal-400' },
  http:     { label: 'http',     className: 'bg-terminal-700 text-terminal-400' },
}

function sourceBadge(sourceType: string) {
  return SOURCE_BADGE[sourceType] ?? { label: sourceType, className: 'bg-terminal-700 text-terminal-400' }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

function confidenceDotClass(confidence: number): string {
  if (confidence >= 0.9) return 'bg-eva-green-400'
  if (confidence >= 0.7) return 'bg-warn-amber-400'
  return 'bg-nerv-red-400'
}

function freshnessLabel(scannedAt: string): string {
  const diffMs = Date.now() - new Date(scannedAt).getTime()
  const diffMins = Math.floor(diffMs / 60_000)
  if (diffMins < 60) return `${diffMins}m ago`
  const diffHours = Math.floor(diffMins / 60)
  if (diffHours < 24) return `${diffHours}h ago`
  const diffDays = Math.floor(diffHours / 24)
  return `${diffDays}d ago`
}

function groupBySource(entries: KnowledgeEntry[]): Record<string, KnowledgeEntry[]> {
  return entries.reduce<Record<string, KnowledgeEntry[]>>((acc, entry) => {
    const key = entry.sourceType
    if (!acc[key]) acc[key] = []
    acc[key].push(entry)
    return acc
  }, {})
}

// ---------------------------------------------------------------------------
// KnowledgeEntryRow
// ---------------------------------------------------------------------------

interface KnowledgeEntryRowProps {
  entry: KnowledgeEntry
  isSelected: boolean
  onSelect: () => void
}

function KnowledgeEntryRow({ entry, isSelected, onSelect }: KnowledgeEntryRowProps) {
  const badge = sourceBadge(entry.sourceType)

  return (
    <button
      onClick={onSelect}
      className={[
        'w-full px-3 py-2 text-left transition-colors duration-[150ms]',
        isSelected
          ? 'bg-at-field-900/60'
          : 'hover:bg-terminal-700/60',
      ].join(' ')}
      data-testid={`knowledge-entry-${entry.id}`}
    >
      <div className="flex items-center gap-1.5 min-w-0">
        {/* Confidence dot */}
        <span
          className={`h-1.5 w-1.5 flex-shrink-0 rounded-full ${confidenceDotClass(entry.confidence)}`}
          title={`Confidence: ${Math.round(entry.confidence * 100)}%`}
        />

        {/* Title */}
        <span className={[
          'flex-1 truncate text-[11px] font-medium',
          isSelected ? 'text-at-field-200' : 'text-terminal-100',
        ].join(' ')}>
          {entry.title}
        </span>
      </div>

      <div className="mt-0.5 flex items-center gap-1.5 pl-3">
        {/* Category */}
        <span className="truncate text-[10px] text-terminal-400">{entry.category}</span>

        {/* Freshness */}
        <span className="ml-auto flex-shrink-0 text-[10px] text-terminal-500">
          {freshnessLabel(entry.scannedAt)}
        </span>

        {/* Source badge */}
        <span className={`flex-shrink-0 rounded px-1.5 py-0.5 font-display text-[9px] uppercase tracking-widest ${badge.className}`}>
          {badge.label}
        </span>
      </div>
    </button>
  )
}

// ---------------------------------------------------------------------------
// KnowledgeSourceGroup
// ---------------------------------------------------------------------------

interface KnowledgeSourceGroupProps {
  sourceType: string
  entries: KnowledgeEntry[]
  selectedId: string | null
  onSelect: (id: string) => void
}

function KnowledgeSourceGroup({ sourceType, entries, selectedId, onSelect }: KnowledgeSourceGroupProps) {
  const [expanded, setExpanded] = useState(true)
  const badge = sourceBadge(sourceType)

  return (
    <div className="border-b border-terminal-700">
      {/* Group header */}
      <button
        onClick={() => setExpanded((v) => !v)}
        className="flex w-full items-center gap-2 px-3 py-1.5 hover:bg-terminal-700/40 transition-colors"
        aria-expanded={expanded}
      >
        {expanded
          ? <ChevronDown size={12} className="flex-shrink-0 text-terminal-400" />
          : <ChevronRight size={12} className="flex-shrink-0 text-terminal-400" />
        }
        <span className={`rounded px-1.5 py-0.5 font-display text-[9px] uppercase tracking-widest ${badge.className}`}>
          {badge.label}
        </span>
        <span className="flex-shrink-0 rounded bg-terminal-700 px-1.5 py-0.5 font-mono text-[10px] text-terminal-300">
          {entries.length}
        </span>
      </button>

      {/* Entry rows */}
      {expanded && (
        <div className="flex flex-col gap-px">
          {entries.map((entry) => (
            <KnowledgeEntryRow
              key={entry.id}
              entry={entry}
              isSelected={selectedId === entry.id}
              onSelect={() => onSelect(entry.id)}
            />
          ))}
        </div>
      )}
    </div>
  )
}

// ---------------------------------------------------------------------------
// KnowledgeSearchBar
// ---------------------------------------------------------------------------

interface KnowledgeSearchBarProps {
  value: string
  onChange: (v: string) => void
}

function KnowledgeSearchBar({ value, onChange }: KnowledgeSearchBarProps) {
  return (
    <div className="relative flex-shrink-0 border-b border-terminal-600 px-2 py-1.5">
      <Search size={11} className="absolute left-4 top-1/2 -translate-y-1/2 text-terminal-400" />
      <input
        type="text"
        placeholder="Search knowledge…"
        value={value}
        onChange={(e) => onChange(e.target.value)}
        className="w-full rounded border border-terminal-600 bg-terminal-800 py-1 pl-6 pr-2 text-[11px] text-terminal-100 placeholder:text-terminal-500 outline-none focus:border-at-field-500 focus:ring-1 focus:ring-at-field-500/30 transition-colors duration-[150ms]"
        data-testid="knowledge-search"
      />
    </div>
  )
}

// ---------------------------------------------------------------------------
// KnowledgeSegment
// ---------------------------------------------------------------------------

function KnowledgeSegment({ programId }: { programId: string }) {
  const selectedKnowledgeEntryId = useUiStore((s) => s.selectedKnowledgeEntryId)
  const setSelectedKnowledgeEntryId = useUiStore((s) => s.setSelectedKnowledgeEntryId)
  const clearSelection = useCanvasStore((s) => s.clearSelection)

  const [inputValue, setInputValue] = useState('')
  const [debouncedQuery, setDebouncedQuery] = useState('')
  const timerRef = useRef<ReturnType<typeof setTimeout> | null>(null)

  useEffect(() => {
    if (timerRef.current) clearTimeout(timerRef.current)
    timerRef.current = setTimeout(() => setDebouncedQuery(inputValue.trim()), 300)
    return () => {
      if (timerRef.current) clearTimeout(timerRef.current)
    }
  }, [inputValue])

  const isSearching = debouncedQuery !== ''

  const { data: allEntries, isLoading: isLoadingAll } = useKnowledgeEntries(
    isSearching ? null : programId,
  )
  const { data: searchResults, isLoading: isLoadingSearch } = useKnowledgeSearch(
    programId,
    debouncedQuery,
  )

  const isLoading = isSearching ? isLoadingSearch : isLoadingAll

  const handleSelect = (entryId: string) => {
    clearSelection()
    setSelectedKnowledgeEntryId(entryId === selectedKnowledgeEntryId ? null : entryId)
  }

  return (
    <div className="flex flex-1 flex-col overflow-hidden">
      <KnowledgeSearchBar value={inputValue} onChange={setInputValue} />

      {isLoading && (
        <div className="flex flex-1 items-center justify-center gap-2">
          <Loader2 size={13} className="animate-spin text-terminal-400" />
          <span className="text-[11px] text-terminal-400">Loading…</span>
        </div>
      )}

      {!isLoading && (
        <div className="flex-1 overflow-y-auto">
          {/* Search results — flat list */}
          {isSearching && (
            <>
              {searchResults && searchResults.length === 0 && (
                <p className="px-3 py-4 text-center text-[11px] text-terminal-500">
                  No entries match your search
                </p>
              )}
              {searchResults?.map(({ entry }) => (
                <KnowledgeEntryRow
                  key={entry.id}
                  entry={entry}
                  isSelected={selectedKnowledgeEntryId === entry.id}
                  onSelect={() => handleSelect(entry.id)}
                />
              ))}
            </>
          )}

          {/* Grouped list */}
          {!isSearching && (
            <>
              {(!allEntries || allEntries.length === 0) && (
                <p className="px-3 py-4 text-center text-[11px] text-terminal-500">
                  No knowledge entries for this program
                </p>
              )}
              {allEntries && allEntries.length > 0 && (
                <>
                  {Object.entries(groupBySource(allEntries)).map(([sourceType, entries]) => (
                    <KnowledgeSourceGroup
                      key={sourceType}
                      sourceType={sourceType}
                      entries={entries}
                      selectedId={selectedKnowledgeEntryId}
                      onSelect={handleSelect}
                    />
                  ))}
                </>
              )}
            </>
          )}
        </div>
      )}
    </div>
  )
}

// ---------------------------------------------------------------------------
// KnowledgeLibrary (top-level, segmented header)
// ---------------------------------------------------------------------------

export function KnowledgeLibrary() {
  const [activeSegment, setActiveSegment] = useState<'knowledge' | 'templates'>('knowledge')
  const selectedProgramId = useUiStore((s) => s.selectedProgramId)

  return (
    <div className="flex flex-1 flex-col overflow-hidden">
      {/* Segment tabs */}
      <div className="flex flex-shrink-0 gap-1 border-b border-terminal-600 bg-terminal-900 px-2 py-1.5">
        {(['knowledge', 'templates'] as const).map((seg) => (
          <button
            key={seg}
            onClick={() => setActiveSegment(seg)}
            className={[
              'flex-1 rounded px-2 py-1 font-display text-[10px] uppercase tracking-widest transition-colors duration-[150ms]',
              activeSegment === seg
                ? 'bg-terminal-600 text-terminal-50'
                : 'text-terminal-400 hover:text-terminal-100',
            ].join(' ')}
          >
            {seg}
          </button>
        ))}
      </div>

      {activeSegment === 'templates' && (
        <div className="flex flex-1 flex-col items-center justify-center gap-1 p-4">
          <p className="text-xs font-medium text-terminal-400">Templates</p>
          <p className="text-center text-[10px] text-terminal-500">Coming in P2-M7</p>
        </div>
      )}

      {activeSegment === 'knowledge' && !selectedProgramId && (
        <div className="flex flex-1 flex-col items-center justify-center gap-1 p-4">
          <p className="text-center text-[11px] text-terminal-500">Select a program to browse its knowledge library</p>
        </div>
      )}

      {activeSegment === 'knowledge' && selectedProgramId && (
        <KnowledgeSegment programId={selectedProgramId} />
      )}
    </div>
  )
}
