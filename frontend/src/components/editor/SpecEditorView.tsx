import { AlertTriangle } from 'lucide-react'
import Editor from '@monaco-editor/react'

const SPEC_EDITOR_OPTIONS = {
  minimap: { enabled: false },
  lineNumbers: 'on',
  wordWrap: 'on',
  fontSize: 12,
  fontFamily: '"JetBrains Mono", "Fira Code", monospace',
  scrollBeyondLastLine: false,
  renderWhitespace: 'boundary',
  tabSize: 2,
  insertSpaces: true,
  readOnly: true,
} as const

export function SpecEditorView() {
  return (
    <div className="flex h-full w-full flex-col overflow-hidden">
      {/* Sync placeholder banner — removed in EVA-64 when sync state machine ships */}
      <div className="flex flex-shrink-0 items-center gap-2 border-b border-warn-amber-500/30 bg-warn-amber-500/10 px-4 py-2">
        <AlertTriangle className="h-3 w-3 flex-shrink-0 text-warn-amber-400" />
        <p className="font-display text-xs uppercase tracking-widest text-warn-amber-400">
          Spec sync not yet available — coming in P2-M3
        </p>
      </div>

      {/* Monaco YAML editor
          path="eva-program.yaml" sets the model URI to file:///eva-program.yaml,
          which matches the fileMatch registered in monacoSetup.ts, enabling
          schema validation and autocomplete stubs from the placeholder schema. */}
      <div className="flex-1 overflow-hidden">
        <Editor
          language="yaml"
          theme="eva-dark"
          value=""
          path="eva-program.yaml"
          options={SPEC_EDITOR_OPTIONS}
        />
      </div>
    </div>
  )
}
