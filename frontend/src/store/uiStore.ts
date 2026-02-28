import { create } from 'zustand'
import type { AssistantMessage, ConversationThread, LogEntry, ProgramId, RunId, FileTab } from '../types'

export type { FileTab }

export type ActivityKey = 'programs' | 'nodes' | 'knowledge' | 'runs' | 'codebase' | 'settings'
export type AppMode = 'author' | 'operate'
export type BottomTab = 'logs' | 'output' | 'timeline' | 'changes'
export type EditorTab = 'graph' | 'code' | 'spec'
export type SpecSyncState = 'graph_source' | 'yaml_source' | 'conflict'
export type DetailPanelTab = 'inspector' | 'magi'

const CONV_LS_KEY = 'eva:assistant:conversations'

function loadConversations(): Record<ProgramId, ConversationThread> {
  try {
    const raw = localStorage.getItem(CONV_LS_KEY)
    return raw ? (JSON.parse(raw) as Record<ProgramId, ConversationThread>) : {}
  } catch {
    return {}
  }
}

function persistConversations(conversations: Record<ProgramId, ConversationThread>): void {
  try {
    localStorage.setItem(CONV_LS_KEY, JSON.stringify(conversations))
  } catch {
    // storage quota exceeded — ignore
  }
}

interface UiState {
  activeActivity: ActivityKey
  mode: AppMode
  bottomPanelOpen: boolean
  activeBottomTab: BottomTab
  sidePanelWidth: number
  detailPanelWidth: number
  bottomPanelHeight: number
  selectedProgramId: string | null
  /** The run currently being streamed, or null when idle. */
  activeRunId: RunId | null
  /** The run whose step states are overlaid on the canvas in Operate mode. */
  inspectedRunId: RunId | null
  /** Accumulated LLM tokens from the active (or most recent) run. */
  llmOutput: string
  /** Accumulated log entries from the active (or most recent) run. */
  logEntries: LogEntry[]
  /** Error message from a failed run, shown in the output panel when no LLM tokens were produced. */
  runError: string | null
  activeEditorTab: EditorTab
  specSyncState: SpecSyncState
  specDirty: boolean
  /** Files currently open in the CODE tab editor. */
  openFiles: FileTab[]
  /** Path of the file currently visible in the CODE tab editor. */
  activeFilePath: string | null
  /** The codebase currently selected in the CodebasePanel. */
  activeCodebaseId: string | null
  /** The knowledge entry currently selected in the KnowledgeLibrary (opens KnowledgeEntryView). */
  selectedKnowledgeEntryId: string | null
  /** Which tab is active in the right DetailPanel — Inspector or MAGI assistant. */
  detailPanelTab: DetailPanelTab
  /** Whether the Cmd+K CommandBar overlay is open. */
  commandBarOpen: boolean
  /** Per-program assistant conversation threads, persisted to localStorage. */
  assistantConversations: Record<ProgramId, ConversationThread>

  setActiveActivity: (activity: ActivityKey) => void
  setMode: (mode: AppMode) => void
  setBottomPanelOpen: (open: boolean) => void
  toggleBottomPanel: () => void
  setActiveBottomTab: (tab: BottomTab) => void
  setSidePanelWidth: (width: number) => void
  setDetailPanelWidth: (width: number) => void
  setBottomPanelHeight: (height: number) => void
  setSelectedProgramId: (id: string | null) => void
  setActiveRunId: (id: RunId | null) => void
  setInspectedRunId: (id: RunId | null) => void
  setRunError: (msg: string | null) => void
  appendLlmToken: (token: string) => void
  /** Replace the full LLM output (used when loading a completed run's output). */
  setLlmOutput: (output: string) => void
  appendLogEntry: (entry: LogEntry) => void
  /** Reset output, logs, and error — called when a new run starts. */
  clearRunOutput: () => void
  setActiveEditorTab: (tab: EditorTab) => void
  setSpecSyncState: (state: SpecSyncState) => void
  setSpecDirty: (dirty: boolean) => void
  /** Open a file in the CODE editor; brings it to front if already open. */
  openFile: (tab: FileTab) => void
  /** Close a file tab; adjusts activeFilePath to the last remaining tab or null. */
  closeFile: (path: string) => void
  setActiveFilePath: (path: string | null) => void
  setActiveCodebaseId: (id: string | null) => void
  setSelectedKnowledgeEntryId: (id: string | null) => void
  setDetailPanelTab: (tab: DetailPanelTab) => void
  setCommandBarOpen: (open: boolean) => void
  toggleCommandBar: () => void
  appendAssistantMessage: (programId: ProgramId, message: AssistantMessage) => void
  setAssistantStreaming: (programId: ProgramId, streaming: boolean) => void
  clearAssistantConversation: (programId: ProgramId) => void
}

export const useUiStore = create<UiState>((set) => ({
  activeActivity: 'programs',
  mode: 'author',
  bottomPanelOpen: false,
  activeBottomTab: 'logs',
  sidePanelWidth: 240,
  detailPanelWidth: 360,
  bottomPanelHeight: 200,
  selectedProgramId: null,
  activeRunId: null,
  inspectedRunId: null,
  llmOutput: '',
  logEntries: [],
  runError: null,
  activeEditorTab: 'graph',
  specSyncState: 'graph_source',
  specDirty: false,
  openFiles: [],
  activeFilePath: null,
  activeCodebaseId: null,
  selectedKnowledgeEntryId: null,
  detailPanelTab: 'inspector',
  commandBarOpen: false,
  assistantConversations: loadConversations(),

  setActiveActivity: (activity) => set({ activeActivity: activity }),
  setMode: (mode) => set({ mode }),
  setBottomPanelOpen: (open) => set({ bottomPanelOpen: open }),
  toggleBottomPanel: () => set((s) => ({ bottomPanelOpen: !s.bottomPanelOpen })),
  setActiveBottomTab: (tab) => set({ activeBottomTab: tab }),
  setSidePanelWidth: (width) => set({ sidePanelWidth: Math.min(500, Math.max(180, width)) }),
  setDetailPanelWidth: (width) => set({ detailPanelWidth: Math.min(600, Math.max(280, width)) }),
  setBottomPanelHeight: (height) => set({ bottomPanelHeight: Math.min(600, Math.max(100, height)) }),
  setSelectedProgramId: (id) => set({ selectedProgramId: id }),
  setActiveRunId: (id) => set({ activeRunId: id }),
  setInspectedRunId: (id) => set({ inspectedRunId: id }),
  setRunError: (msg) => set({ runError: msg }),
  appendLlmToken: (token) => set((s) => ({ llmOutput: s.llmOutput + token })),
  setLlmOutput: (output) => set({ llmOutput: output }),
  appendLogEntry: (entry) => set((s) => ({ logEntries: [...s.logEntries, entry] })),
  clearRunOutput: () => set({ llmOutput: '', logEntries: [], runError: null }),
  setActiveEditorTab: (tab) => set({ activeEditorTab: tab }),
  setSpecSyncState: (state) => set({ specSyncState: state }),
  setSpecDirty: (dirty) => set({ specDirty: dirty }),
  openFile: (tab) =>
    set((s) => {
      const existingIdx = s.openFiles.findIndex((f) => f.path === tab.path)
      if (existingIdx !== -1) {
        // Move to end (most-recently-used) without duplicating
        const reordered = [
          ...s.openFiles.slice(0, existingIdx),
          ...s.openFiles.slice(existingIdx + 1),
          s.openFiles[existingIdx],
        ]
        return { openFiles: reordered, activeFilePath: tab.path, activeEditorTab: 'code' }
      }
      const withNew = [...s.openFiles, tab]
      // Evict LRU (first entry) when exceeding 8 open files
      const trimmed = withNew.length > 8 ? withNew.slice(1) : withNew
      return { openFiles: trimmed, activeFilePath: tab.path, activeEditorTab: 'code' }
    }),
  closeFile: (path) =>
    set((s) => {
      const remaining = s.openFiles.filter((f) => f.path !== path)
      const activeFilePath =
        s.activeFilePath === path
          ? (remaining[remaining.length - 1]?.path ?? null)
          : s.activeFilePath
      return { openFiles: remaining, activeFilePath }
    }),
  setActiveFilePath: (path) => set({ activeFilePath: path }),
  setActiveCodebaseId: (id) => set({ activeCodebaseId: id }),
  setSelectedKnowledgeEntryId: (id) => set({ selectedKnowledgeEntryId: id }),
  setDetailPanelTab: (tab) => set({ detailPanelTab: tab }),
  setCommandBarOpen: (open) => set({ commandBarOpen: open }),
  toggleCommandBar: () => set((s) => ({ commandBarOpen: !s.commandBarOpen })),
  appendAssistantMessage: (programId, message) =>
    set((s) => {
      const existing = s.assistantConversations[programId] ?? {
        id: programId,
        programId,
        messages: [],
        isStreaming: false,
      }
      const updated: ConversationThread = {
        ...existing,
        messages: [...existing.messages, message],
      }
      const next = { ...s.assistantConversations, [programId]: updated }
      persistConversations(next)
      return { assistantConversations: next }
    }),
  setAssistantStreaming: (programId, streaming) =>
    set((s) => {
      const existing = s.assistantConversations[programId] ?? {
        id: programId,
        programId,
        messages: [],
        isStreaming: false,
      }
      const updated: ConversationThread = { ...existing, isStreaming: streaming }
      const next = { ...s.assistantConversations, [programId]: updated }
      persistConversations(next)
      return { assistantConversations: next }
    }),
  clearAssistantConversation: (programId) =>
    set((s) => {
      const next = { ...s.assistantConversations }
      delete next[programId]
      persistConversations(next)
      return { assistantConversations: next }
    }),
}))
