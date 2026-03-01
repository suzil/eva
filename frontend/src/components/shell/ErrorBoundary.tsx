import { Component, type ErrorInfo, type ReactNode } from 'react'
import { AlertTriangle } from 'lucide-react'

interface Props {
  children: ReactNode
}

interface State {
  hasError: boolean
  error: Error | null
}

export class ErrorBoundary extends Component<Props, State> {
  constructor(props: Props) {
    super(props)
    this.state = { hasError: false, error: null }
  }

  static getDerivedStateFromError(error: Error): State {
    return { hasError: true, error }
  }

  componentDidCatch(error: Error, info: ErrorInfo) {
    console.error('[Eva] Uncaught render error:', error, info)
  }

  private handleReload = () => {
    window.location.reload()
  }

  render() {
    if (this.state.hasError) {
      return (
        <div className="flex h-full w-full flex-col items-center justify-center gap-4 bg-terminal-950 text-white">
          <AlertTriangle className="h-10 w-10 text-nerv-red-400" />
          <div className="text-center">
            <p className="text-lg font-semibold">Something went wrong</p>
            <p className="mt-1 max-w-sm text-sm text-terminal-300">
              {this.state.error?.message ?? 'An unexpected error occurred.'}
            </p>
          </div>
          <button
            onClick={this.handleReload}
            className="rounded bg-terminal-700 px-4 py-2 text-sm font-medium text-white hover:bg-terminal-600"
          >
            Reload
          </button>
        </div>
      )
    }

    return this.props.children
  }
}
