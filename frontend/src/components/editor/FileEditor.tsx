import { useCallback, useEffect, useRef } from 'react'
import Editor, { type OnMount } from '@monaco-editor/react'

interface FileEditorProps {
  language: string
  content: string
  readOnly: boolean
  onChange: (value: string) => void
  onSave: () => void
}

const EDITOR_OPTIONS = {
  minimap: { enabled: false },
  lineNumbers: 'on' as const,
  wordWrap: 'off' as const,
  fontSize: 12,
  fontFamily: '"JetBrains Mono", "Fira Code", monospace',
  scrollBeyondLastLine: false,
  renderWhitespace: 'none' as const,
}

export function FileEditor({ language, content, readOnly, onChange, onSave }: FileEditorProps) {
  type IStandaloneCodeEditor = Parameters<OnMount>[0]
  const editorRef = useRef<IStandaloneCodeEditor | null>(null)
  // Keep a stable ref so the Cmd+S keybinding always calls the latest onSave.
  const onSaveRef = useRef(onSave)
  onSaveRef.current = onSave

  // When the content prop changes (file switch), push the new value into Monaco.
  useEffect(() => {
    const editor = editorRef.current
    if (!editor) return
    if (editor.getValue() !== content) {
      editor.setValue(content)
    }
  }, [content])

  const handleMount: OnMount = useCallback((editorInstance, monacoInstance) => {
    editorRef.current = editorInstance

    editorInstance.addCommand(
      monacoInstance.KeyMod.CtrlCmd | monacoInstance.KeyCode.KeyS,
      () => { onSaveRef.current() },
    )

    // Seed initial content
    if (editorInstance.getValue() !== content) {
      editorInstance.setValue(content)
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [])

  return (
    <Editor
      language={language}
      theme="eva-dark"
      options={{ ...EDITOR_OPTIONS, readOnly }}
      onChange={(value) => onChange(value ?? '')}
      onMount={handleMount}
      className="h-full w-full"
    />
  )
}
