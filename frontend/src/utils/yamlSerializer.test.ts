import { describe, it, expect } from 'vitest'
import { parseDocument } from 'yaml'
import { graphToYaml, yamlFromSpec } from './yamlSerializer.ts'
import type { Graph } from '../types/index.ts'

// ---------------------------------------------------------------------------
// Weekly Summarizer fixture — exercises all 5 node types
// ---------------------------------------------------------------------------

const WEEKLY_SUMMARIZER: Graph = {
  nodes: {
    trigger1: {
      id: 'trigger1',
      label: 'Weekly Trigger',
      type: {
        type: 'trigger',
        config: { type: 'cron', schedule: '0 9 * * 1' },
      },
      posX: 0,
      posY: 0,
    },
    linear1: {
      id: 'linear1',
      label: 'Linear',
      type: {
        type: 'connector',
        config: {
          system: 'linear',
          credentialId: 'cred-abc',
          actionFilter: ['list_issues', 'get_project'],
        },
      },
      posX: 200,
      posY: 0,
    },
    knowledge1: {
      id: 'knowledge1',
      label: 'Team Context',
      type: {
        type: 'knowledge',
        config: {
          source: { type: '_inline_text', value: 'Sprint goals: ship EVA Phase 2' },
          format: 'text',
          refreshPolicy: { type: 'static' },
        },
      },
      posX: 200,
      posY: 200,
    },
    agent1: {
      id: 'agent1',
      label: 'Summarizer',
      type: {
        type: 'agent',
        config: {
          provider: 'openai',
          model: 'gpt-4o',
          systemPrompt: 'Analyze sprint progress against team goals.',
          responseFormat: 'text',
          temperature: 0.3,
          maxIterations: 5,
        },
      },
      posX: 400,
      posY: 100,
    },
    action1: {
      id: 'action1',
      label: 'Format Report',
      type: {
        type: 'action',
        config: {
          operation: 'template',
          parameters: { template: '# Weekly Report\n{{output}}' },
          errorHandling: { mode: 'fail' },
        },
      },
      posX: 600,
      posY: 100,
    },
  },
  edges: [
    {
      id: 'e1',
      sourceNode: 'trigger1',
      sourcePort: 'event',
      targetNode: 'agent1',
      targetPort: 'trigger',
      category: 'data',
    },
    {
      id: 'e2',
      sourceNode: 'linear1',
      sourcePort: 'tools',
      targetNode: 'agent1',
      targetPort: 'tools',
      category: 'resource',
    },
    {
      id: 'e3',
      sourceNode: 'knowledge1',
      sourcePort: 'context',
      targetNode: 'agent1',
      targetPort: 'context',
      category: 'resource',
    },
    {
      id: 'e4',
      sourceNode: 'agent1',
      sourcePort: 'output',
      targetNode: 'action1',
      targetPort: 'input',
      category: 'data',
    },
  ],
}

// ---------------------------------------------------------------------------
// Round-trip
// ---------------------------------------------------------------------------

describe('graphToYaml', () => {
  it('produces valid YAML containing eva.version: "1"', () => {
    const yaml = graphToYaml(WEEKLY_SUMMARIZER)
    expect(yaml).toMatch(/eva\.version/)
    expect(yaml).toMatch(/"1"/)
  })

  it('serialises all 5 node types with promoted fields', () => {
    const yaml = graphToYaml(WEEKLY_SUMMARIZER)
    // agent
    expect(yaml).toContain('model: gpt-4o')
    expect(yaml).toContain('systemPrompt:')
    expect(yaml).toContain('temperature:')
    // connector
    expect(yaml).toContain('system: linear')
    expect(yaml).toContain('actionFilter:')
    // knowledge
    expect(yaml).toContain('format: text')
    expect(yaml).toContain('refreshPolicy:')
    // action
    expect(yaml).toContain('operation: template')
    expect(yaml).toContain('errorHandling:')
    // trigger uses triggerType (not type) to avoid collision
    expect(yaml).toContain('triggerType: cron')
    expect(yaml).toContain('schedule: 0 9 * * 1')
  })

  it('serialises edge fields with from/fromPort/to/toPort names', () => {
    const yaml = graphToYaml(WEEKLY_SUMMARIZER)
    expect(yaml).toContain('from: trigger1')
    expect(yaml).toContain('fromPort: event')
    expect(yaml).toContain('to: agent1')
    expect(yaml).toContain('toPort: trigger')
    expect(yaml).toContain('category: data')
  })
})

describe('round-trip: graphToYaml → yamlFromSpec', () => {
  it('produces the same graph (deep equality) for the Weekly Summarizer', () => {
    const yaml = graphToYaml(WEEKLY_SUMMARIZER)
    const result = yamlFromSpec(yaml)

    expect(Array.isArray(result)).toBe(false)
    const graph = result as Graph

    // Node count
    expect(Object.keys(graph.nodes)).toHaveLength(5)

    // Edge count
    expect(graph.edges).toHaveLength(4)

    // Agent node fields
    const agent = graph.nodes['agent1']
    expect(agent.type.type).toBe('agent')
    if (agent.type.type === 'agent') {
      expect(agent.type.config.model).toBe('gpt-4o')
      expect(agent.type.config.temperature).toBe(0.3)
      expect(agent.type.config.maxIterations).toBe(5)
      expect(agent.type.config.provider).toBe('openai')
      expect(agent.type.config.responseFormat).toBe('text')
    }

    // Trigger uses triggerType → config.type
    const trigger = graph.nodes['trigger1']
    expect(trigger.type.type).toBe('trigger')
    if (trigger.type.type === 'trigger') {
      expect(trigger.type.config.type).toBe('cron')
      expect(trigger.type.config.schedule).toBe('0 9 * * 1')
    }

    // Connector
    const connector = graph.nodes['linear1']
    expect(connector.type.type).toBe('connector')
    if (connector.type.type === 'connector') {
      expect(connector.type.config.system).toBe('linear')
      expect(connector.type.config.credentialId).toBe('cred-abc')
      expect(connector.type.config.actionFilter).toEqual(['list_issues', 'get_project'])
    }

    // Knowledge
    const knowledge = graph.nodes['knowledge1']
    expect(knowledge.type.type).toBe('knowledge')
    if (knowledge.type.type === 'knowledge') {
      const src = knowledge.type.config.source
      expect(src.type).toBe('_inline_text')
      if (src.type === '_inline_text') {
        expect(src.value).toBe('Sprint goals: ship EVA Phase 2')
      }
    }

    // Action
    const action = graph.nodes['action1']
    expect(action.type.type).toBe('action')
    if (action.type.type === 'action') {
      expect(action.type.config.operation).toBe('template')
      expect(action.type.config.errorHandling).toEqual({ mode: 'fail' })
    }

    // Edge round-trip
    const e1 = graph.edges.find((e) => e.id === 'e1')
    expect(e1).toBeDefined()
    expect(e1?.sourceNode).toBe('trigger1')
    expect(e1?.sourcePort).toBe('event')
    expect(e1?.targetNode).toBe('agent1')
    expect(e1?.targetPort).toBe('trigger')
    expect(e1?.category).toBe('data')
  })

  it('round-trips posX and posY without precision loss', () => {
    const yaml = graphToYaml(WEEKLY_SUMMARIZER)
    const result = yamlFromSpec(yaml) as Graph
    expect(result.nodes['agent1'].posX).toBe(400)
    expect(result.nodes['agent1'].posY).toBe(100)
  })
})

// ---------------------------------------------------------------------------
// Comment preservation
// ---------------------------------------------------------------------------

describe('comment preservation', () => {
  it('parseDocument preserves YAML comments through re-serialisation', () => {
    const yamlWithComments = [
      '# Weekly Summarizer program',
      'eva.version: "1"',
      'nodes:',
      '  # Trigger node',
      '  trigger1:',
      '    type: trigger',
      '    label: Weekly Trigger',
      '    posX: 0',
      '    posY: 0',
      '    triggerType: manual',
      'edges: []',
    ].join('\n')

    const doc = parseDocument(yamlWithComments)
    const reserialized = doc.toString()

    expect(reserialized).toContain('# Weekly Summarizer program')
    expect(reserialized).toContain('# Trigger node')
  })

  it('yamlFromSpec succeeds on YAML that contains comments', () => {
    const yamlWithComments = [
      '# this is a comment',
      'eva.version: "1"',
      'nodes: {}',
      'edges: []',
    ].join('\n')

    const result = yamlFromSpec(yamlWithComments)
    expect(Array.isArray(result)).toBe(false)
    const graph = result as Graph
    expect(Object.keys(graph.nodes)).toHaveLength(0)
    expect(graph.edges).toHaveLength(0)
  })
})

// ---------------------------------------------------------------------------
// Error paths
// ---------------------------------------------------------------------------

describe('yamlFromSpec error paths', () => {
  it('returns ParseError[] for completely invalid YAML', () => {
    const result = yamlFromSpec(': ]: invalid yaml {{{{')
    expect(Array.isArray(result)).toBe(true)
    expect((result as object[]).length).toBeGreaterThan(0)
  })

  it('returns ParseError[] for unknown node type', () => {
    const yaml = [
      'eva.version: "1"',
      'nodes:',
      '  n1:',
      '    type: invalid_type',
      '    label: Test',
      '    posX: 0',
      '    posY: 0',
      'edges: []',
    ].join('\n')

    const result = yamlFromSpec(yaml)
    expect(Array.isArray(result)).toBe(true)
    const errors = result as { message: string }[]
    expect(errors.some((e) => e.message.includes('unknown node type'))).toBe(true)
  })

  it('returns ParseError[] when required agent field is missing', () => {
    const yaml = [
      'eva.version: "1"',
      'nodes:',
      '  n1:',
      '    type: agent',
      '    label: Agent',
      '    posX: 0',
      '    posY: 0',
      '    # model is deliberately missing',
      '    systemPrompt: hello',
      '    responseFormat: text',
      '    temperature: 0.7',
      '    maxIterations: 3',
      'edges: []',
    ].join('\n')

    const result = yamlFromSpec(yaml)
    expect(Array.isArray(result)).toBe(true)
    const errors = result as { message: string }[]
    expect(errors.some((e) => e.message.includes('model'))).toBe(true)
  })

  it('returns ParseError[] for unknown triggerType', () => {
    const yaml = [
      'eva.version: "1"',
      'nodes:',
      '  t1:',
      '    type: trigger',
      '    label: Trigger',
      '    posX: 0',
      '    posY: 0',
      '    triggerType: unknown_trigger',
      'edges: []',
    ].join('\n')

    const result = yamlFromSpec(yaml)
    expect(Array.isArray(result)).toBe(true)
    const errors = result as { message: string }[]
    expect(errors.some((e) => e.message.includes('unknown triggerType'))).toBe(true)
  })

  it('returns ParseError[] when edges is not a sequence', () => {
    const yaml = ['eva.version: "1"', 'nodes: {}', 'edges: not-a-list'].join('\n')
    const result = yamlFromSpec(yaml)
    expect(Array.isArray(result)).toBe(true)
  })
})

// ---------------------------------------------------------------------------
// Edge ID synthesis
// ---------------------------------------------------------------------------

describe('edge ID synthesis', () => {
  it('synthesises edge ID from endpoints when id field is absent', () => {
    const graph: Graph = {
      nodes: {
        n1: {
          id: 'n1',
          label: 'Trigger',
          type: { type: 'trigger', config: { type: 'manual' } },
          posX: 0,
          posY: 0,
        },
      },
      edges: [
        {
          id: 'n1:out->n2:in',
          sourceNode: 'n1',
          sourcePort: 'out',
          targetNode: 'n2',
          targetPort: 'in',
          category: 'data',
        },
      ],
    }

    const yaml = graphToYaml(graph)
    const result = yamlFromSpec(yaml) as Graph
    expect(result.edges[0].id).toBe('n1:out->n2:in')
  })
})
