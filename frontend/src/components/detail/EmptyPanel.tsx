import { MousePointerClick } from 'lucide-react'
import { useProgram } from '../../api/hooks'
import { useUiStore } from '../../store/uiStore'
import type { ProgramState } from '../../types'

const STATE_LABELS: Record<ProgramState, { label: string; className: string }> = {
  draft: { label: 'Draft', className: 'text-terminal-400 border-terminal-500' },
  active: { label: 'Active', className: 'text-eva-green-400 border-eva-green-800' },
  paused: { label: 'Paused', className: 'text-warn-amber-400 border-warn-amber-800' },
  archived: { label: 'Archived', className: 'text-terminal-500 border-terminal-600' },
}

export function EmptyPanel() {
  const selectedProgramId = useUiStore((s) => s.selectedProgramId)
  const { data: program } = useProgram(selectedProgramId ?? '')

  if (!selectedProgramId || !program) {
    return (
      <div className="flex flex-1 flex-col items-center justify-center gap-3 p-4 text-center">
        <MousePointerClick className="h-8 w-8 text-terminal-600" />
        <p className="text-xs text-terminal-500">
          Select a program to begin authoring
        </p>
      </div>
    )
  }

  const stateStyle = STATE_LABELS[program.state]

  return (
    <div className="flex flex-1 flex-col gap-4 overflow-y-auto p-4">
      {/* Program summary */}
      <div className="rounded-lg border border-terminal-500 bg-terminal-800/50 p-3">
        <div className="mb-1 flex items-center gap-2">
          <span className="text-sm font-semibold text-terminal-50">{program.name}</span>
          <span
            className={`rounded border px-1.5 py-0.5 text-[10px] font-medium ${stateStyle.className}`}
          >
            {stateStyle.label}
          </span>
        </div>
        <p className="text-[11px] text-terminal-400">
          Updated {new Date(program.updatedAt).toLocaleDateString()}
        </p>
      </div>

      {/* Getting-started tips */}
      <div className="space-y-2">
        <p className="font-display text-[10px] uppercase tracking-widest text-terminal-300">
          Getting started
        </p>
        <TipItem step="1" text="Drag a Trigger from the palette onto the canvas" />
        <TipItem step="2" text="Add an Agent and connect the trigger's event port to the instruction port" />
        <TipItem step="3" text="Click any node to configure it here" />
        <TipItem step="4" text="Click Save to persist your graph" />
      </div>
    </div>
  )
}

function TipItem({ step, text }: { step: string; text: string }) {
  return (
    <div className="flex items-start gap-2.5">
      <span className="flex h-4 w-4 shrink-0 items-center justify-center rounded-full bg-terminal-700 text-[9px] font-bold text-terminal-300">
        {step}
      </span>
      <p className="text-[11px] leading-relaxed text-terminal-400">{text}</p>
    </div>
  )
}
