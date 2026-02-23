import EditorWorker from 'monaco-editor/esm/vs/editor/editor.worker?worker'
import YamlWorker from './workers/yaml.worker?worker'

// Register Monaco web workers. This must run before any <Editor> component
// mounts. The yaml label is served by the monaco-yaml worker; everything else
// (editor core, TypeScript, JSON, …) falls back to Monaco's default worker.
window.MonacoEnvironment = {
  getWorker(_moduleId: string, label: string) {
    if (label === 'yaml') {
      return new YamlWorker()
    }
    return new EditorWorker()
  },
}
