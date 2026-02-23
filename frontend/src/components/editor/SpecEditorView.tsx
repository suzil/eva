import { useCallback, useEffect, useRef, useState } from 'react'
import { AlertTriangle, RotateCcw, Save } from 'lucide-react'
import Editor, { type Monaco, type OnMount } from '@monaco-editor/react'
import { parseDocument } from 'yaml'
import { useUiStore } from '../../store/uiStore'
import { useCanvasStore } from '../../store/canvasStore'
import { useSpec, useSaveSpec } from '../../api/hooks'
import { SpecSaveError } from '../../api/client'
import { SyncWarningModal } from './SyncWarningModal'

type IStandaloneCodeEditor = Parameters<OnMount>[0]

const EDITOR_OPTIONS = {
  minimap: { enabled: false },
  lineNumbers: 'on' as const,
  wordWrap: 'on' as const,
  fontSize: 12,
  fontFamily: '"JetBrains Mono", "Fira Code", monospace',
  scrollBeyondLastLine: false,
  renderWhitespace: 'boundary' as const,
  tabSize: 2,
  insertSpaces: true,
}

export function SpecEditorView() {
  const programId = useUiStore((s) => s.selectedProgramId)
  const specSyncState = useUiStore((s) => s.specSyncState)
  const setSpecSyncState = useUiStore((s) => s.setSpecSyncState)
  const setSpecDirty = useUiStore((s) => s.setSpecDirty)
  const loadGraph = useCanvasStore((s) => s.loadGraph)
  const markClean = useCanvasStore((s) => s.markClean)

  const { data: specData, isLoading, isError, refetch } = useSpec(programId)
  const saveSpec = useSaveSpec(programId ?? '')

  const editorRef = useRef<IStandaloneCodeEditor | null>(null)
  const monacoRef = useRef<Monaco | null>(null)
  const loadedForProgramRef = useRef<string | null>(null)
  // Kept as refs so handleMount and Monaco commands always see current values.
  const specDataRef = useRef(specData)
  const programIdRef = useRef(programId)
  const localYamlRef = useRef('')
  const handleSaveRef = useRef<() => Promise<void>>()

  const [localYaml, setLocalYaml] = useState('')
  const [saveErrors, setSaveErrors] = useState<string[]>([])
  const [isSaving, setIsSaving] = useState(false)
  const [replacePending, setReplacePending] = useState(false)

  // Keep refs up-to-date with current render values.
  specDataRef.current = specData
  programIdRef.current = programId
  localYamlRef.current = localYaml

  // Seed the editor when spec data arrives (initial fetch, program switch, or Replace Graph).
  useEffect(() => {
    if (!specData || !editorRef.current) return
    const isNewProgram = programId !== loadedForProgramRef.current
    if (isNewProgram || replacePending) {
      editorRef.current.setValue(specData.yaml)
      loadedForProgramRef.current = programId
      setLocalYaml(specData.yaml)
      setSpecDirty(false)
      setSaveErrors([])
      if (replacePending) setReplacePending(false)
    }
  }, [specData, programId, replacePending, setSpecDirty])

  const applyYamlMarkers = useCallback((value: string) => {
    if (!editorRef.current || !monacoRef.current) return
    const model = editorRef.current.getModel()
    if (!model) return
    const doc = parseDocument(value)
    const markers = doc.errors.map((err) => {
      const lp = err.linePos
      const startLine = lp?.[0].line ?? 1
      const startCol = lp?.[0].col ?? 1
      const endLine = lp?.[1]?.line ?? startLine
      const endCol = (lp?.[1]?.col ?? startCol) + 1
      return {
        startLineNumber: startLine,
        startColumn: startCol,
        endLineNumber: endLine,
        endColumn: endCol,
        message: err.message,
        severity: monacoRef.current!.MarkerSeverity.Error,
      }
    })
    monacoRef.current.editor.setModelMarkers(model, 'yaml-spec', markers)
  }, [])

  const handleEditorChange = useCallback(
    (value: string | undefined) => {
      const v = value ?? ''
      setLocalYaml(v)
      setSpecDirty(v !== (specDataRef.current?.yaml ?? ''))
      setSaveErrors([])
      applyYamlMarkers(v)
    },
    [setSpecDirty, applyYamlMarkers],
  )

  const handleSave = useCallback(async () => {
    if (!programIdRef.current) return
    setIsSaving(true)
    setSaveErrors([])
    try {
      const program = await saveSpec.mutateAsync(localYamlRef.current)
      loadGraph(program.graph, programIdRef.current)
      markClean()
      setSpecDirty(false)
      setSpecSyncState('yaml_source')
    } catch (err) {
      if (err instanceof SpecSaveError) {
        setSaveErrors(err.errors.map((e) => e.message))
      } else {
        setSaveErrors([err instanceof Error ? err.message : 'Save failed'])
      }
    } finally {
      setIsSaving(false)
    }
  }, [saveSpec, loadGraph, markClean, setSpecDirty, setSpecSyncState])

  // Keep the save ref current so the Monaco keybinding always calls the latest version.
  handleSaveRef.current = handleSave

  const handleMount: OnMount = useCallback(
    (editorInstance, monacoInstance) => {
      editorRef.current = editorInstance
      monacoRef.current = monacoInstance

      editorInstance.addCommand(
        monacoInstance.KeyMod.CtrlCmd | monacoInstance.KeyCode.KeyS,
        () => { void handleSaveRef.current?.() },
      )

      // If specData is already cached (e.g., React Query hit), seed immediately.
      const data = specDataRef.current
      const pid = programIdRef.current
      if (data && loadedForProgramRef.current !== pid) {
        editorInstance.setValue(data.yaml)
        loadedForProgramRef.current = pid
        setLocalYaml(data.yaml)
        setSpecDirty(false)
      }
    },
    // eslint-disable-next-line react-hooks/exhaustive-deps
    [setLocalYaml, setSpecDirty],
  )

  const handleReplace = useCallback(() => {
    setReplacePending(true)
    setSpecSyncState('graph_source')
    void refetch()
  }, [setSpecSyncState, refetch])

  const handleKeep = useCallback(() => {
    setSpecSyncState('yaml_source')
  }, [setSpecSyncState])

  // --- Empty state ---
  if (!programId) {
    return (
      <div className="flex h-full w-full items-center justify-center">
        <p className="font-display text-xs uppercase tracking-widest text-terminal-500">
          No program selected
        </p>
      </div>
    )
  }

  // --- Loading state ---
  if (isLoading) {
    return (
      <div className="flex h-full w-full items-center justify-center">
        <p className="font-display text-xs uppercase tracking-widest text-terminal-400">
          Loading spec…
        </p>
      </div>
    )
  }

  // --- Fetch error state ---
  if (isError) {
    return (
      <div className="flex h-full w-full flex-col items-center justify-center gap-3">
        <AlertTriangle className="h-5 w-5 text-warn-amber-400" />
        <p className="font-display text-xs uppercase tracking-widest text-warn-amber-400">
          Failed to load spec
        </p>
        <button
          onClick={() => void refetch()}
          className="flex items-center gap-1.5 rounded border border-terminal-500 bg-terminal-700 px-3 py-1.5 font-display text-xs uppercase tracking-widest text-terminal-200 transition-colors hover:bg-terminal-600"
        >
          <RotateCcw className="h-3 w-3" />
          Retry
        </button>
      </div>
    )
  }

  return (
    <div className="relative flex h-full w-full flex-col overflow-hidden">
      {/* Toolbar */}
      <div className="flex flex-shrink-0 items-center justify-between border-b border-terminal-500 bg-terminal-800 px-3 py-1">
        <p className="font-display text-xs uppercase tracking-widest text-terminal-400">
          YAML Spec
        </p>
        <button
          onClick={() => void handleSave()}
          disabled={isSaving}
          title="Save (⌘S)"
          className="flex items-center gap-1.5 rounded px-2 py-1 font-display text-xs uppercase tracking-widest text-terminal-300 transition-colors hover:bg-terminal-700 hover:text-terminal-100 disabled:opacity-50"
        >
          <Save className="h-3 w-3" />
          {isSaving ? 'Saving…' : 'Save'}
        </button>
      </div>

      {/* Monaco editor */}
      <div className="flex-1 overflow-hidden">
        <Editor
          language="yaml"
          theme="eva-dark"
          path="eva-program.yaml"
          defaultValue=""
          options={EDITOR_OPTIONS}
          onMount={handleMount}
          onChange={handleEditorChange}
        />
      </div>

      {/* Save error banner */}
      {saveErrors.length > 0 && (
        <div className="flex flex-shrink-0 flex-col gap-1 border-t border-error-red-500/30 bg-error-red-500/10 px-4 py-2">
          <div className="flex items-center gap-2">
            <AlertTriangle className="h-3 w-3 flex-shrink-0 text-error-red-400" />
            <p className="font-display text-xs uppercase tracking-widest text-error-red-400">
              Spec errors
            </p>
          </div>
          <ul className="ml-5 space-y-0.5">
            {saveErrors.map((msg, i) => (
              <li key={i} className="text-xs text-error-red-300">
                {msg}
              </li>
            ))}
          </ul>
        </div>
      )}

      {/* Conflict modal — shown when graph was modified while SPEC tab had edits */}
      {specSyncState === 'conflict' && (
        <SyncWarningModal onReplace={handleReplace} onKeep={handleKeep} />
      )}
    </div>
  )
}
