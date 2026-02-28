import { useEffect, useRef } from 'react'
import type { AssistantMessage } from '../../types'
import { MessageBubble } from './MessageBubble'

interface MessageListProps {
  messages: AssistantMessage[]
  isStreaming: boolean
}

export function MessageList({ messages, isStreaming }: MessageListProps) {
  const bottomRef = useRef<HTMLDivElement>(null)

  useEffect(() => {
    bottomRef.current?.scrollIntoView({ behavior: 'smooth' })
  }, [messages.length, isStreaming])

  return (
    <div className="flex flex-1 flex-col gap-2 overflow-y-auto py-3">
      {messages.map((msg, idx) => (
        <MessageBubble key={idx} message={msg} />
      ))}

      {isStreaming && (
        <div className="flex justify-start px-3">
          <div className="flex items-center gap-1 rounded-lg bg-terminal-900 px-3 py-2">
            <span className="h-1.5 w-1.5 animate-pulse rounded-full bg-magi-blue-500" style={{ animationDelay: '0ms' }} />
            <span className="h-1.5 w-1.5 animate-pulse rounded-full bg-magi-blue-500" style={{ animationDelay: '150ms' }} />
            <span className="h-1.5 w-1.5 animate-pulse rounded-full bg-magi-blue-500" style={{ animationDelay: '300ms' }} />
          </div>
        </div>
      )}

      <div ref={bottomRef} />
    </div>
  )
}
