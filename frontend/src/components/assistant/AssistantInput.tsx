import { useCallback, useEffect, useRef, useState } from 'react'
import { ArrowUp } from 'lucide-react'
import { SlashCommandMenu } from './SlashCommandMenu'

interface AssistantInputProps {
  onSend: (text: string) => void
  disabled?: boolean
  /** Pre-fill the textarea with this text without auto-sending. Cleared by the parent after being consumed. */
  initialValue?: string | null
  onInitialValueConsumed?: () => void
}

/**
 * Multi-line textarea with a Send button. Typing `/` at the start of the
 * input (or after only whitespace) opens the SlashCommandMenu autocomplete.
 * Selecting a command inserts it as the input text.
 * Enter sends; Shift+Enter inserts a newline.
 */
export function AssistantInput({ onSend, disabled = false, initialValue, onInitialValueConsumed }: AssistantInputProps) {
  const [value, setValue] = useState('')
  const [showSlashMenu, setShowSlashMenu] = useState(false)
  const textareaRef = useRef<HTMLTextAreaElement>(null)

  // When a pre-fill value arrives (e.g. from GraphProposalCard Revise), populate and focus
  useEffect(() => {
    if (initialValue) {
      setValue(initialValue)
      textareaRef.current?.focus()
      onInitialValueConsumed?.()
    }
  }, [initialValue]) // eslint-disable-line react-hooks/exhaustive-deps

  // The query passed to SlashCommandMenu is the text after the leading `/`
  const slashQuery = showSlashMenu ? value.replace(/^\//, '') : ''

  // Auto-resize textarea up to 5 rows
  useEffect(() => {
    const ta = textareaRef.current
    if (!ta) return
    ta.style.height = 'auto'
    const lineHeight = parseInt(getComputedStyle(ta).lineHeight, 10) || 20
    const maxHeight = lineHeight * 5
    ta.style.height = `${Math.min(ta.scrollHeight, maxHeight)}px`
  }, [value])

  const handleChange = useCallback((e: React.ChangeEvent<HTMLTextAreaElement>) => {
    const next = e.target.value
    setValue(next)
    // Show slash menu when the input starts with `/` (optionally preceded by whitespace)
    setShowSlashMenu(/^\s*\/\S*$/.test(next) || next === '/')
  }, [])

  const handleSend = useCallback(() => {
    if (disabled || !value.trim()) return
    onSend(value.trim())
    setValue('')
    setShowSlashMenu(false)
  }, [disabled, value, onSend])

  const handleKeyDown = useCallback(
    (e: React.KeyboardEvent<HTMLTextAreaElement>) => {
      if (showSlashMenu) {
        // Arrow/Enter/Escape are handled by SlashCommandMenu's document listener;
        // suppress default Enter behaviour here so it doesn't also send.
        if (e.key === 'Enter' || e.key === 'ArrowUp' || e.key === 'ArrowDown' || e.key === 'Escape') {
          e.preventDefault()
          return
        }
      }

      if (e.key === 'Enter' && !e.shiftKey) {
        e.preventDefault()
        handleSend()
      }
    },
    [showSlashMenu, handleSend],
  )

  const handleSlashSelect = useCallback((command: string) => {
    // Insert command followed by a space so the user can type arguments
    setValue(command + ' ')
    setShowSlashMenu(false)
    textareaRef.current?.focus()
  }, [])

  const handleSlashClose = useCallback(() => {
    setShowSlashMenu(false)
    textareaRef.current?.focus()
  }, [])

  return (
    <div className="relative">
      {showSlashMenu && (
        <SlashCommandMenu
          query={slashQuery}
          onSelect={handleSlashSelect}
          onClose={handleSlashClose}
        />
      )}

      <div
        className={[
          'flex items-end gap-2 rounded border bg-terminal-900 px-3 py-2',
          disabled
            ? 'border-terminal-700 opacity-60'
            : 'border-terminal-600 focus-within:border-magi-blue-500',
        ].join(' ')}
      >
        <textarea
          ref={textareaRef}
          rows={1}
          value={value}
          onChange={handleChange}
          onKeyDown={handleKeyDown}
          disabled={disabled}
          placeholder={disabled ? 'MAGI is thinking…' : 'Ask MAGI or type /'}
          className="flex-1 resize-none bg-transparent text-sm text-terminal-100 placeholder-terminal-500 outline-none"
        />

        <button
          type="button"
          onClick={handleSend}
          disabled={disabled || !value.trim()}
          aria-label="Send message"
          className={[
            'flex h-6 w-6 shrink-0 items-center justify-center rounded',
            disabled || !value.trim()
              ? 'cursor-not-allowed text-terminal-600'
              : 'bg-magi-blue-500 text-terminal-900 hover:bg-magi-blue-400',
          ].join(' ')}
        >
          <ArrowUp className="h-3.5 w-3.5" />
        </button>
      </div>
    </div>
  )
}
