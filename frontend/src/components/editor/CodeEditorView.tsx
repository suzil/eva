import { useEffect, useState } from 'react'
import { Loader2 } from 'lucide-react'
import { useUiStore } from '../../store/uiStore'
import { useFile, useWriteFile } from '../../api/hooks'
import { FileTabBar } from './FileTabBar'
import { FileEditor } from './FileEditor'

export function CodeEditorView() {
  const openFiles = useUiStore((s) => s.openFiles)
  const activeFilePath = useUiStore((s) => s.activeFilePath)
  const activeCodebaseId = useUiStore((s) => s.activeCodebaseId)
  const setActiveFilePath = useUiStore((s) => s.setActiveFilePath)
  const closeFile = useUiStore((s) => s.closeFile)

  const [editMode, setEditMode] = useState(false)
  const [isDirty, setIsDirty] = useState(false)
  const [localContent, setLocalContent] = useState('')

  const { data: fileEntry, isLoading, isError } = useFile(activeCodebaseId, activeFilePath)
  const writeFile = useWriteFile(activeCodebaseId ?? '')

  // When the loaded file changes, reset local editing state.
  useEffect(() => {
    if (fileEntry) {
      setLocalContent(fileEntry.content)
      setIsDirty(false)
      setEditMode(false)
    }
  }, [fileEntry])

  // Also reset dirty/edit state when the active path switches before the new
  // file has loaded (avoids stale dirty indicator on the incoming tab).
  useEffect(() => {
    setIsDirty(false)
    setEditMode(false)
  }, [activeFilePath])

  const handleChange = (value: string) => {
    setLocalContent(value)
    setIsDirty(value !== (fileEntry?.content ?? ''))
  }

  const handleSave = () => {
    if (!activeCodebaseId || !activeFilePath || !isDirty) return
    void writeFile.mutateAsync({ path: activeFilePath, content: localContent }).then(() => {
      setIsDirty(false)
    })
  }

  const handleToggleEditMode = () => {
    setEditMode((prev) => !prev)
  }

  // Empty state — no files open yet
  if (openFiles.length === 0) {
    return (
      <div className="flex h-full w-full items-center justify-center">
        <p className="font-display text-xs uppercase tracking-widest text-terminal-500">
          Open a file from the codebase panel
        </p>
      </div>
    )
  }

  return (
    <div className="flex h-full w-full flex-col overflow-hidden">
      <FileTabBar
        tabs={openFiles}
        activePath={activeFilePath}
        isDirty={isDirty}
        editMode={editMode}
        onSelect={(path) => setActiveFilePath(path)}
        onClose={(path) => closeFile(path)}
        onToggleEditMode={handleToggleEditMode}
      />

      <div className="relative flex-1 overflow-hidden">
        {isLoading && (
          <div className="absolute inset-0 z-10 flex items-center justify-center bg-terminal-850">
            <Loader2 size={16} className="animate-spin text-terminal-400" />
          </div>
        )}

        {isError && (
          <div className="flex h-full items-center justify-center">
            <p className="font-display text-xs uppercase tracking-widest text-nerv-red-400">
              Failed to load file
            </p>
          </div>
        )}

        {fileEntry && (
          <FileEditor
            language={fileEntry.language}
            content={localContent}
            readOnly={!editMode}
            onChange={handleChange}
            onSave={handleSave}
          />
        )}
      </div>
    </div>
  )
}
