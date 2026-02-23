import * as monaco from 'monaco-editor'
import { loader } from '@monaco-editor/react'
import { configureMonacoYaml } from 'monaco-yaml'
import EditorWorker from 'monaco-editor/esm/vs/editor/editor.worker?worker'
import YamlWorker from './workers/yaml.worker?worker'

// Bind @monaco-editor/react to the same ESM Monaco instance so that
// defineTheme calls below are visible to all <Editor> components.
loader.config({ monaco })

// ─── EVA dark theme ────────────────────────────────────────────────────────
// Colors sourced from eva-evangelion-theme.mdc and EVA-57 ticket spec.
// Foreground hex values must NOT include the '#' prefix (Monaco requirement).
const EVA_DARK_THEME: monaco.editor.IStandaloneThemeData = {
  base: 'vs-dark',
  inherit: true,
  rules: [
    { token: '',                        foreground: 'C4C5D6' }, // terminal-100  — default text
    { token: 'keyword',                 foreground: 'FF6A00' }, // at-field-500
    { token: 'keyword.operator',        foreground: 'FF6A00' },
    { token: 'string',                  foreground: '00DD44' }, // eva-green-500
    { token: 'string.escape',           foreground: '33FF5C' }, // eva-green-400
    { token: 'comment',                 foreground: '4F5070' }, // terminal-400
    { token: 'comment.line',            foreground: '4F5070' },
    { token: 'comment.block',           foreground: '4F5070' },
    { token: 'number',                  foreground: '3A9EFF' }, // magi-blue (ticket spec)
    { token: 'number.float',            foreground: '3A9EFF' },
    { token: 'type',                    foreground: '9B7CFF' }, // violet (ticket spec)
    { token: 'type.identifier',         foreground: '9B7CFF' },
    { token: 'identifier',              foreground: 'C4C5D6' }, // terminal-100
    { token: 'variable',                foreground: 'C4C5D6' },
    { token: 'constant',                foreground: 'FF8533' }, // at-field-400
    { token: 'regexp',                  foreground: '00DD44' },
    // Markdown tokens
    { token: 'markup.heading',          foreground: 'FF6A00', fontStyle: 'bold' },
    { token: 'markup.bold',             foreground: 'EAEAF2', fontStyle: 'bold' },
    { token: 'markup.italic',           foreground: 'C4C5D6', fontStyle: 'italic' },
    { token: 'markup.inline.raw',       foreground: '00DD44' },
    { token: 'markup.underline.link',   foreground: '3A9EFF' },
    // YAML tokens (for SpecEditorView in EVA-63+)
    { token: 'key.yaml',                foreground: 'FF6A00' }, // at-field-500
    { token: 'string.yaml',             foreground: '00DD44' }, // eva-green-500
  ],
  colors: {
    'editor.background':                    '#0E0F18', // terminal-850
    'editor.foreground':                    '#C4C5D6', // terminal-100
    'editor.lineHighlightBackground':       '#13141F', // terminal-800
    'editor.selectionBackground':           '#FF6A0030',
    'editor.inactiveSelectionBackground':   '#FF6A0018',
    'editorCursor.foreground':              '#FF6A00', // at-field-500
    'editorLineNumber.foreground':          '#4F5070', // terminal-400
    'editorLineNumber.activeForeground':    '#9596AF', // terminal-200
    'editorIndentGuide.background1':        '#212233', // terminal-700
    'editorIndentGuide.activeBackground1':  '#3A3B54', // terminal-500
    'editor.wordHighlightBackground':       '#FF6A0020',
    'editor.findMatchBackground':           '#FF6A0040',
    'editor.findMatchHighlightBackground':  '#FF6A0020',
    'editorWidget.background':              '#13141F', // terminal-800
    'editorWidget.border':                  '#3A3B54', // terminal-500
    'input.background':                     '#212233', // terminal-700
    'input.border':                         '#3A3B54', // terminal-500
    'input.foreground':                     '#C4C5D6', // terminal-100
    'scrollbarSlider.background':           '#3A3B5440',
    'scrollbarSlider.hoverBackground':      '#4F507080',
    'scrollbarSlider.activeBackground':     '#4F5070C0',
  },
}

monaco.editor.defineTheme('eva-dark', EVA_DARK_THEME)

// ─── YAML schema registration ───────────────────────────────────────────────
// Full schema lives in frontend/public/eva-schema.json (EVA-62).
// enableSchemaRequest lets monaco-yaml fetch it from the dev/prod static server.
// fileMatch targets the model URI used by SpecEditorView (path="eva-program.yaml").
configureMonacoYaml(monaco, {
  validate: true,
  enableSchemaRequest: true,
  schemas: [
    {
      uri: '/eva-schema.json',
      fileMatch: ['eva-program.yaml'],
    },
  ],
})

// ─── Web worker registration ────────────────────────────────────────────────
// This must run before any <Editor> component mounts. The yaml label is served
// by the monaco-yaml worker; everything else falls back to Monaco's default.
window.MonacoEnvironment = {
  getWorker(_moduleId: string, label: string) {
    if (label === 'yaml') {
      return new YamlWorker()
    }
    return new EditorWorker()
  },
}
