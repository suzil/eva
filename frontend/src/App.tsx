import { AppShell } from './components/shell/AppShell'
import { CommandPalette } from './components/shell/CommandPalette'
import { ErrorBoundary } from './components/shell/ErrorBoundary'

export default function App() {
  return (
    <>
      <ErrorBoundary>
        <AppShell />
      </ErrorBoundary>
      <CommandPalette />
    </>
  )
}
