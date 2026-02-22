export function CanvasContainer() {
  return (
    <div className="relative flex flex-1 flex-col items-center justify-center bg-gray-950">
      {/* Grid dot pattern background */}
      <div
        className="pointer-events-none absolute inset-0"
        style={{
          backgroundImage:
            'radial-gradient(circle, #374151 1px, transparent 1px)',
          backgroundSize: '24px 24px',
        }}
      />
      <p className="relative text-sm text-gray-600">
        Canvas — react-flow (EVA-19)
      </p>
    </div>
  )
}
