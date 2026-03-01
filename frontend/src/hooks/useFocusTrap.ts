import { RefObject, useEffect } from 'react'

const FOCUSABLE_SELECTOR =
  'a[href],button:not([disabled]),textarea:not([disabled]),input:not([disabled]),select:not([disabled]),[tabindex]:not([tabindex="-1"])'

interface FocusTrapOptions {
  /** When true the hook traps Tab but does not steal initial focus.
   *  Use this when the component manages its own initial focus. */
  skipInitialFocus?: boolean
}

export function useFocusTrap(
  ref: RefObject<HTMLElement | null>,
  active: boolean,
  { skipInitialFocus = false }: FocusTrapOptions = {},
) {
  useEffect(() => {
    if (!active || !ref.current) return
    const container = ref.current

    const getFocusable = (): HTMLElement[] =>
      Array.from(container.querySelectorAll<HTMLElement>(FOCUSABLE_SELECTOR))

    if (!skipInitialFocus) {
      getFocusable()[0]?.focus()
    }

    const handleKeyDown = (e: KeyboardEvent) => {
      if (e.key !== 'Tab') return
      const els = getFocusable()
      if (els.length === 0) return
      const first = els[0]
      const last = els[els.length - 1]
      if (e.shiftKey) {
        if (document.activeElement === first) {
          e.preventDefault()
          last?.focus()
        }
      } else {
        if (document.activeElement === last) {
          e.preventDefault()
          first?.focus()
        }
      }
    }

    container.addEventListener('keydown', handleKeyDown)
    return () => container.removeEventListener('keydown', handleKeyDown)
  }, [active, skipInitialFocus, ref])
}
