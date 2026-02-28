import { useState } from 'react'
import { ChevronDown, ChevronRight, Loader2 } from 'lucide-react'
import { DiffEditor } from '@monaco-editor/react'
import type { FileChange } from '../../types'
import { useAcceptFile, useRejectFile } from '../../api/hooks'

const EXT_TO_LANGUAGE: Record<string, string> = {
  ts: 'typescript', tsx: 'typescript',
  js: 'javascript', jsx: 'javascript',
  hs: 'haskell',
  py: 'python',
  rs: 'rust',
  go: 'go',
  java: 'java',
  cs: 'csharp',
  cpp: 'cpp', cc: 'cpp', cxx: 'cpp',
  c: 'c',
  css: 'css', scss: 'scss',
  html: 'html', htm: 'html',
  yaml: 'yaml', yml: 'yaml',
  json: 'json',
  md: 'markdown', mdx: 'markdown',
  sh: 'shell', bash: 'shell',
  sql: 'sql',
  toml: 'ini',
  xml: 'xml',
}

function languageFromPath(path: string): string {
  const ext = path.split('.').pop()?.toLowerCase() ?? ''
  return EXT_TO_LANGUAGE[ext] ?? 'plaintext'
}

function dirAndBase(path: string): { dir: string; base: string } {
  const idx = path.lastIndexOf('/')
  if (idx === -1) return { dir: '', base: path }
  return { dir: path.slice(0, idx + 1), base: path.slice(idx + 1) }
}

const ACTION_BADGE: Record<FileChange['action'], { symbol: string; className: string }> = {
  add:    { symbol: '+', className: 'bg-eva-green-900 text-eva-green-300' },
  modify: { symbol: '~', className: 'bg-warn-amber-900 text-warn-amber-300' },
  delete: { symbol: '-', className: 'bg-nerv-red-900 text-nerv-red-300' },
}

const STATUS_BADGE: Record<FileChange['status'], { label: string; className: string }> = {
  pending:  { label: 'pending',  className: 'bg-terminal-700 text-terminal-300' },
  accepted: { label: 'accepted', className: 'bg-eva-green-900 text-eva-green-400' },
  rejected: { label: 'rejected', className: 'bg-nerv-red-900 text-nerv-red-400' },
}

const DIFF_OPTIONS = {
  readOnly: true,
  renderSideBySide: true,
  minimap: { enabled: false },
  fontSize: 12,
  fontFamily: '"JetBrains Mono", "Fira Code", monospace',
  scrollBeyondLastLine: false,
} as const

interface DiffViewerProps {
  file: FileChange
  changesetId: string
  defaultExpanded?: boolean
}

export function DiffViewer({ file, changesetId, defaultExpanded = true }: DiffViewerProps) {
  const [expanded, setExpanded] = useState(defaultExpanded)

  const acceptFile = useAcceptFile()
  const rejectFile = useRejectFile()

  const isPending = file.status === 'pending'
  const isAccepting = acceptFile.isPending
  const isRejecting = rejectFile.isPending

  const handleAccept = () => {
    if (!isPending) return
    void acceptFile.mutate({ changesetId, fileId: file.id })
  }

  const handleReject = () => {
    if (!isPending) return
    void rejectFile.mutate({ changesetId, fileId: file.id })
  }

  const action = ACTION_BADGE[file.action]
  const status = STATUS_BADGE[file.status]
  const { dir, base } = dirAndBase(file.path)

  const original = file.action === 'add' ? '' : (file.originalContent ?? '')
  const modified = file.action === 'delete' ? '' : file.proposedContent

  return (
    <div className="flex flex-col border border-terminal-600 bg-terminal-850">
      {/* Header */}
      <div
        role="button"
        tabIndex={0}
        onClick={() => setExpanded((v) => !v)}
        onKeyDown={(e) => { if (e.key === 'Enter' || e.key === ' ') setExpanded((v) => !v) }}
        className="flex h-9 flex-shrink-0 cursor-pointer items-center gap-2 px-3 hover:bg-terminal-800 transition-colors"
        aria-expanded={expanded}
      >
        {expanded
          ? <ChevronDown size={14} className="flex-shrink-0 text-terminal-400" />
          : <ChevronRight size={14} className="flex-shrink-0 text-terminal-400" />
        }

        {/* Action badge */}
        <span className={`flex-shrink-0 rounded px-1.5 font-mono text-xs font-bold ${action.className}`}>
          {action.symbol}
        </span>

        {/* File path */}
        <span className="min-w-0 flex-1 truncate font-mono text-xs">
          {dir && <span className="text-terminal-400">{dir}</span>}
          <span className="text-terminal-100">{base}</span>
        </span>

        {/* Status badge */}
        <span className={`flex-shrink-0 rounded px-2 py-0.5 font-display text-[10px] uppercase tracking-widest ${status.className}`}>
          {status.label}
        </span>

        {/* Accept / Reject buttons — stop propagation so click doesn't collapse */}
        <div className="flex flex-shrink-0 items-center gap-1.5" onClick={(e) => e.stopPropagation()}>
          <button
            onClick={handleAccept}
            disabled={!isPending || isAccepting || isRejecting}
            className={[
              'flex items-center gap-1 rounded px-2.5 py-1 font-display text-[10px] uppercase tracking-widest transition-colors',
              isPending && !isAccepting && !isRejecting
                ? 'bg-eva-green-800 text-eva-green-300 hover:bg-eva-green-700'
                : 'cursor-not-allowed bg-terminal-700 text-terminal-500',
            ].join(' ')}
          >
            {isAccepting && <Loader2 size={10} className="animate-spin" />}
            Accept
          </button>

          <button
            onClick={handleReject}
            disabled={!isPending || isAccepting || isRejecting}
            className={[
              'flex items-center gap-1 rounded px-2.5 py-1 font-display text-[10px] uppercase tracking-widest transition-colors',
              isPending && !isAccepting && !isRejecting
                ? 'bg-nerv-red-900 text-nerv-red-300 hover:bg-nerv-red-800'
                : 'cursor-not-allowed bg-terminal-700 text-terminal-500',
            ].join(' ')}
          >
            {isRejecting && <Loader2 size={10} className="animate-spin" />}
            Reject
          </button>
        </div>
      </div>

      {/* Diff editor body */}
      {expanded && (
        <div className="h-60 w-full border-t border-terminal-600">
          <DiffEditor
            original={original}
            modified={modified}
            language={languageFromPath(file.path)}
            theme="eva-dark"
            options={DIFF_OPTIONS}
            className="h-full w-full"
          />
        </div>
      )}
    </div>
  )
}
