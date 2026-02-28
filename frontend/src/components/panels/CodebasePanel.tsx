import { useState, useEffect } from 'react'
import {
  FolderCode,
  FolderOpen,
  Folder,
  FileCode,
  FileJson,
  FileText,
  File,
  RefreshCw,
  Unplug,
  Loader2,
  ChevronRight,
  ChevronDown,
} from 'lucide-react'
import {
  useCodebases,
  useConnectCodebase,
  useDisconnectCodebase,
  useRefreshCodebase,
  useFileTree,
} from '../../api/hooks'
import { useUiStore } from '../../store/uiStore'
import type { FileNode, CodebaseMetadata } from '../../types'

// ---------------------------------------------------------------------------
// Language icon map — extension → Lucide icon component
// ---------------------------------------------------------------------------
const EXT_ICON: Record<string, React.ComponentType<{ className?: string }>> = {
  ts: FileCode,
  tsx: FileCode,
  js: FileCode,
  jsx: FileCode,
  hs: FileCode,
  cabal: FileCode,
  py: FileCode,
  rb: FileCode,
  rs: FileCode,
  go: FileCode,
  json: FileJson,
  yaml: FileJson,
  yml: FileJson,
  toml: FileJson,
  md: FileText,
  txt: FileText,
  css: FileText,
  html: FileText,
}

function fileIcon(name: string): React.ComponentType<{ className?: string }> {
  const ext = name.split('.').pop()?.toLowerCase() ?? ''
  return EXT_ICON[ext] ?? File
}

// ---------------------------------------------------------------------------
// FileTreeNode — recursive
// ---------------------------------------------------------------------------
interface FileTreeNodeProps {
  node: FileNode
  codebaseId: string
  depth: number
}

function FileTreeNode({ node, codebaseId, depth }: FileTreeNodeProps) {
  const [expanded, setExpanded] = useState(depth === 0)
  const openFile = useUiStore((s) => s.openFile)

  const indent = depth * 10

  if (node.isDir) {
    return (
      <div>
        <button
          onClick={() => setExpanded((v) => !v)}
          className="flex w-full items-center gap-1 px-2 py-0.5 text-left text-xs text-terminal-300 hover:bg-terminal-700 transition-colors"
          style={{ paddingLeft: `${8 + indent}px` }}
        >
          {expanded ? (
            <ChevronDown className="h-3 w-3 shrink-0 text-terminal-400" />
          ) : (
            <ChevronRight className="h-3 w-3 shrink-0 text-terminal-400" />
          )}
          {expanded ? (
            <FolderOpen className="h-3.5 w-3.5 shrink-0 text-at-field-400" />
          ) : (
            <Folder className="h-3.5 w-3.5 shrink-0 text-terminal-400" />
          )}
          <span className="truncate">{node.name}</span>
        </button>
        {expanded &&
          node.children.map((child) => (
            <FileTreeNode
              key={child.path}
              node={child}
              codebaseId={codebaseId}
              depth={depth + 1}
            />
          ))}
      </div>
    )
  }

  const Icon = fileIcon(node.name)

  return (
    <button
      onClick={() => openFile({ codebaseId, path: node.path })}
      className="flex w-full items-center gap-1.5 px-2 py-0.5 text-left text-xs text-terminal-300 hover:bg-terminal-700 hover:text-terminal-100 transition-colors"
      style={{ paddingLeft: `${8 + indent + 12}px` }}
      title={node.path}
    >
      <Icon className="h-3.5 w-3.5 shrink-0 text-terminal-500" />
      <span className="truncate">{node.name}</span>
    </button>
  )
}

// ---------------------------------------------------------------------------
// ConnectedView — metadata + file tree
// ---------------------------------------------------------------------------
interface ConnectedViewProps {
  codebase: CodebaseMetadata
  programId: string
}

function ConnectedView({ codebase, programId }: ConnectedViewProps) {
  const disconnect = useDisconnectCodebase(programId)
  const refresh = useRefreshCodebase()
  const setActiveCodebaseId = useUiStore((s) => s.setActiveCodebaseId)

  const { data: tree, isLoading: treeLoading } = useFileTree(codebase.id)

  const handleDisconnect = () => {
    disconnect.mutate(codebase.id, {
      onSuccess: () => setActiveCodebaseId(null),
    })
  }

  const handleRefresh = () => {
    refresh.mutate(codebase.id)
  }

  const topLangs = Object.entries(codebase.languageStats)
    .sort(([, a], [, b]) => b - a)
    .slice(0, 5)

  const lastScanned = new Date(codebase.lastScannedAt).toLocaleString(undefined, {
    month: 'short',
    day: 'numeric',
    hour: '2-digit',
    minute: '2-digit',
  })

  return (
    <div className="flex flex-1 flex-col overflow-hidden">
      {/* Metadata section */}
      <div className="border-b border-terminal-600 px-3 py-2.5 space-y-2">
        <div className="flex items-center gap-1.5 min-w-0">
          <FolderCode className="h-3.5 w-3.5 shrink-0 text-at-field-400" />
          <span className="truncate text-[11px] text-terminal-200 font-mono" title={codebase.path}>
            {codebase.path}
          </span>
        </div>

        {codebase.gitBranch && (
          <p className="text-[10px] text-terminal-500">
            branch:{' '}
            <span className="text-terminal-300 font-mono">{codebase.gitBranch}</span>
            {codebase.gitDirty && (
              <span className="ml-1 text-warn-amber-400">dirty</span>
            )}
          </p>
        )}

        {topLangs.length > 0 && (
          <div className="flex flex-wrap gap-1">
            {topLangs.map(([ext, count]) => (
              <span
                key={ext}
                className="rounded border border-terminal-600 px-1.5 py-0.5 text-[10px] text-terminal-400"
              >
                .{ext} <span className="text-terminal-300">{count}</span>
              </span>
            ))}
          </div>
        )}

        <p className="text-[10px] text-terminal-500">scanned {lastScanned}</p>

        <div className="flex gap-1.5 pt-0.5">
          <button
            onClick={handleRefresh}
            disabled={refresh.isPending}
            className="flex items-center gap-1 rounded border border-terminal-600 px-2 py-1 text-[10px] text-terminal-300 hover:border-terminal-500 hover:text-terminal-100 disabled:opacity-50 transition-colors"
          >
            <RefreshCw
              className={['h-3 w-3', refresh.isPending ? 'animate-spin' : ''].join(' ')}
            />
            Refresh
          </button>
          <button
            onClick={handleDisconnect}
            disabled={disconnect.isPending}
            className="flex items-center gap-1 rounded border border-terminal-600 px-2 py-1 text-[10px] text-terminal-300 hover:border-red-500/60 hover:text-red-400 disabled:opacity-50 transition-colors"
          >
            <Unplug className="h-3 w-3" />
            Disconnect
          </button>
        </div>
      </div>

      {/* File tree */}
      <div className="flex-1 overflow-y-auto py-1">
        {treeLoading && (
          <div className="flex items-center justify-center py-6">
            <Loader2 className="h-4 w-4 animate-spin text-terminal-500" />
          </div>
        )}
        {tree && (
          <FileTreeNode node={tree} codebaseId={codebase.id} depth={0} />
        )}
      </div>
    </div>
  )
}

// ---------------------------------------------------------------------------
// EmptyView — path input + connect button
// ---------------------------------------------------------------------------
interface EmptyViewProps {
  programId: string
}

function EmptyView({ programId }: EmptyViewProps) {
  const [path, setPath] = useState('')
  const connect = useConnectCodebase(programId)
  const setActiveCodebaseId = useUiStore((s) => s.setActiveCodebaseId)

  const handleConnect = () => {
    if (!path.trim()) return
    connect.mutate(
      { path: path.trim() },
      {
        onSuccess: (codebase) => {
          setActiveCodebaseId(codebase.id)
          setPath('')
        },
      },
    )
  }

  return (
    <div className="flex flex-1 flex-col items-center justify-center gap-4 px-4">
      <FolderCode className="h-8 w-8 text-terminal-600" />
      <p className="text-center text-xs text-terminal-400">
        Connect a local codebase to browse files and run agent changesets.
      </p>
      <div className="w-full space-y-2">
        <input
          type="text"
          value={path}
          onChange={(e) => setPath(e.target.value)}
          onKeyDown={(e) => e.key === 'Enter' && handleConnect()}
          placeholder="/absolute/path/to/repo"
          className="w-full rounded border border-terminal-600 bg-terminal-900 px-2.5 py-1.5 text-xs text-terminal-100 placeholder-terminal-500 focus:border-at-field-500 focus:outline-none transition-colors"
        />
        <button
          onClick={handleConnect}
          disabled={!path.trim() || connect.isPending}
          className="flex w-full items-center justify-center gap-1.5 rounded border border-at-field-600 bg-at-field-900/30 px-3 py-1.5 text-xs text-at-field-300 hover:bg-at-field-800/40 hover:text-at-field-100 disabled:opacity-50 transition-colors"
        >
          {connect.isPending ? (
            <Loader2 className="h-3.5 w-3.5 animate-spin" />
          ) : (
            <FolderCode className="h-3.5 w-3.5" />
          )}
          Connect
        </button>
        {connect.isError && (
          <p className="text-[10px] text-red-400">
            {(connect.error as Error).message ?? 'Failed to connect codebase.'}
          </p>
        )}
      </div>
    </div>
  )
}

// ---------------------------------------------------------------------------
// CodebasePanel — top-level
// ---------------------------------------------------------------------------
export function CodebasePanel() {
  const selectedProgramId = useUiStore((s) => s.selectedProgramId)
  const activeCodebaseId = useUiStore((s) => s.activeCodebaseId)
  const setActiveCodebaseId = useUiStore((s) => s.setActiveCodebaseId)

  const { data: codebases, isLoading } = useCodebases(selectedProgramId)

  // Sync first codebase into store when data first loads
  useEffect(() => {
    if (!codebases) return
    const first = codebases[0]
    if (first && !activeCodebaseId) {
      setActiveCodebaseId(first.id)
    } else if (!first && activeCodebaseId) {
      setActiveCodebaseId(null)
    }
  }, [codebases, activeCodebaseId, setActiveCodebaseId])

  if (!selectedProgramId) {
    return (
      <div className="flex flex-1 flex-col items-center justify-center gap-2 p-4">
        <FolderCode className="h-7 w-7 text-terminal-600" />
        <p className="text-center text-xs text-terminal-500">
          Select a program to manage its codebase.
        </p>
      </div>
    )
  }

  if (isLoading) {
    return (
      <div className="flex flex-1 items-center justify-center">
        <Loader2 className="h-4 w-4 animate-spin text-terminal-500" />
      </div>
    )
  }

  const codebase = codebases?.[0] ?? null

  if (!codebase) {
    return <EmptyView programId={selectedProgramId} />
  }

  return <ConnectedView codebase={codebase} programId={selectedProgramId} />
}
