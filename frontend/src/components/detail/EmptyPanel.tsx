import { MousePointerClick } from 'lucide-react'
import { useProgram } from '../../api/hooks'
import { useUiStore } from '../../store/uiStore'
import type { ProgramState } from '../../types'

const STATE_LABELS: Record<ProgramState, { label: string; className: string }> = {
  draft: { label: 'Draft', className: 'text-gray-400 border-gray-700' },
  active: { label: 'Active', className: 'text-green-400 border-green-800' },
  paused: { label: 'Paused', className: 'text-amber-400 border-amber-800' },
  archived: { label: 'Archived', className: 'text-gray-500 border-gray-700' },
}

export function EmptyPanel() {
  const selectedProgramId = useUiStore((s) => s.selectedProgramId)
  const { data: program } = useProgram(selectedProgramId ?? '')

  if (!selectedProgramId || !program) {
    return (
      <div className="flex flex-1 flex-col items-center justify-center gap-3 p-4 text-center">
        <MousePointerClick className="h-8 w-8 text-gray-700" />
        <p className="text-xs text-gray-500">
          Select a program to begin authoring
        </p>
      </div>
    )
  }

  const stateStyle = STATE_LABELS[program.state]

  return (
    <div className="flex flex-1 flex-col gap-4 overflow-y-auto p-4">
      {/* Program summary */}
      <div className="rounded-lg border border-gray-800 bg-gray-900/50 p-3">
        <div className="mb-1 flex items-center gap-2">
          <span className="text-sm font-semibold text-white">{program.name}</span>
          <span
            className={`rounded border px-1.5 py-0.5 text-[10px] font-medium ${stateStyle.className}`}
          >
            {stateStyle.label}
          </span>
        </div>
        <p className="text-[11px] text-gray-500">
          Updated {new Date(program.updatedAt).toLocaleDateString()}
        </p>
      </div>

      {/* Getting-started tips */}
      <div className="space-y-2">
        <p className="text-[10px] font-semibold uppercase tracking-wider text-gray-600">
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
      <span className="flex h-4 w-4 shrink-0 items-center justify-center rounded-full bg-gray-800 text-[9px] font-bold text-gray-400">
        {step}
      </span>
      <p className="text-[11px] leading-relaxed text-gray-500">{text}</p>
    </div>
  )
}
