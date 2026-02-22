import { create } from 'zustand'
import type { LogEntry, RunId } from '../types'

export type ActivityKey = 'programs' | 'nodes' | 'knowledge' | 'runs' | 'settings'
export type AppMode = 'author' | 'operate'
export type BottomTab = 'logs' | 'output' | 'timeline'

interface UiState {
  activeActivity: ActivityKey
  mode: AppMode
  bottomPanelOpen: boolean
  activeBottomTab: BottomTab
  sidePanelWidth: number
  detailPanelWidth: number
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

  setActiveActivity: (activity: ActivityKey) => void
  setMode: (mode: AppMode) => void
  setBottomPanelOpen: (open: boolean) => void
  toggleBottomPanel: () => void
  setActiveBottomTab: (tab: BottomTab) => void
  setSidePanelWidth: (width: number) => void
  setDetailPanelWidth: (width: number) => void
  setSelectedProgramId: (id: string | null) => void
  setActiveRunId: (id: RunId | null) => void
  setInspectedRunId: (id: RunId | null) => void
  setRunError: (msg: string | null) => void
  appendLlmToken: (token: string) => void
  appendLogEntry: (entry: LogEntry) => void
  /** Reset output, logs, and error — called when a new run starts. */
  clearRunOutput: () => void
}

export const useUiStore = create<UiState>((set) => ({
  activeActivity: 'programs',
  mode: 'author',
  bottomPanelOpen: false,
  activeBottomTab: 'logs',
  sidePanelWidth: 240,
  detailPanelWidth: 360,
  selectedProgramId: null,
  activeRunId: null,
  inspectedRunId: null,
  llmOutput: '',
  logEntries: [],
  runError: null,

  setActiveActivity: (activity) => set({ activeActivity: activity }),
  setMode: (mode) => set({ mode }),
  setBottomPanelOpen: (open) => set({ bottomPanelOpen: open }),
  toggleBottomPanel: () => set((s) => ({ bottomPanelOpen: !s.bottomPanelOpen })),
  setActiveBottomTab: (tab) => set({ activeBottomTab: tab }),
  setSidePanelWidth: (width) => set({ sidePanelWidth: Math.min(500, Math.max(180, width)) }),
  setDetailPanelWidth: (width) => set({ detailPanelWidth: Math.min(600, Math.max(280, width)) }),
  setSelectedProgramId: (id) => set({ selectedProgramId: id }),
  setActiveRunId: (id) => set({ activeRunId: id }),
  setInspectedRunId: (id) => set({ inspectedRunId: id }),
  setRunError: (msg) => set({ runError: msg }),
  appendLlmToken: (token) => set((s) => ({ llmOutput: s.llmOutput + token })),
  appendLogEntry: (entry) => set((s) => ({ logEntries: [...s.logEntries, entry] })),
  clearRunOutput: () => set({ llmOutput: '', logEntries: [], runError: null }),
}))
