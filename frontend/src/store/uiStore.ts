import { create } from 'zustand'

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

  setActiveActivity: (activity: ActivityKey) => void
  setMode: (mode: AppMode) => void
  setBottomPanelOpen: (open: boolean) => void
  toggleBottomPanel: () => void
  setActiveBottomTab: (tab: BottomTab) => void
  setSidePanelWidth: (width: number) => void
  setDetailPanelWidth: (width: number) => void
}

export const useUiStore = create<UiState>((set) => ({
  activeActivity: 'programs',
  mode: 'author',
  bottomPanelOpen: false,
  activeBottomTab: 'logs',
  sidePanelWidth: 240,
  detailPanelWidth: 360,

  setActiveActivity: (activity) => set({ activeActivity: activity }),
  setMode: (mode) => set({ mode }),
  setBottomPanelOpen: (open) => set({ bottomPanelOpen: open }),
  toggleBottomPanel: () => set((s) => ({ bottomPanelOpen: !s.bottomPanelOpen })),
  setActiveBottomTab: (tab) => set({ activeBottomTab: tab }),
  setSidePanelWidth: (width) => set({ sidePanelWidth: Math.min(500, Math.max(180, width)) }),
  setDetailPanelWidth: (width) => set({ detailPanelWidth: Math.min(600, Math.max(280, width)) }),
}))
