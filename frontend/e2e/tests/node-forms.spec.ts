/**
 * Node form and detail-panel E2E tests.
 *
 * Covers: editing a node label via the NodePanel, KnowledgeForm source tabs,
 * TriggerForm manual/cron switching, the EdgePanel (source→target display),
 * and deleting a node from the canvas.
 *
 * All tests pre-populate the graph via the API so they don't rely on
 * drag-and-drop (already covered in graph-authoring.spec.ts).
 */

import { test, expect } from '../fixtures/eva-fixture'
import { makeTriggerAgentGraph } from '../helpers/api'
import type { Graph } from '../helpers/api'

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/** Simulate an HTML5 drag from the node palette to the canvas. */
async function dragNodeFromPalette(page: import('@playwright/test').Page, nodeType: string, x: number, y: number) {
  await page.evaluate(
    ({ nodeType, x, y }) => {
      const canvas = document.querySelector('.react-flow__pane') as HTMLElement | null
      if (!canvas) throw new Error('ReactFlow pane not found')
      const dt = new DataTransfer()
      dt.setData('application/eva-node-type', nodeType)
      const makeEvent = (type: string) =>
        new DragEvent(type, { dataTransfer: dt, bubbles: true, cancelable: true, clientX: x, clientY: y })
      canvas.dispatchEvent(makeEvent('dragover'))
      canvas.dispatchEvent(makeEvent('drop'))
    },
    { nodeType, x, y }
  )
}

/** Build a minimal graph with a Knowledge node only. */
function makeKnowledgeOnlyGraph(): Graph {
  const id = crypto.randomUUID()
  return {
    nodes: {
      [id]: {
        id,
        label: 'My Knowledge',
        posX: 200,
        posY: 200,
        type: {
          type: 'knowledge',
          config: { source: { type: '_inline_text', value: 'hello' }, format: 'text', refreshPolicy: { type: 'static' } },
        },
      },
    },
    edges: [],
  }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

test.describe('Node forms and detail panel', () => {
  test('editing the node label in NodePanel updates the input value', async ({ app, programId, apiHelpers }) => {
    await apiHelpers.saveGraph(programId, makeTriggerAgentGraph())
    const program = await apiHelpers.getProgram(programId)

    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    await app.page.waitForSelector('.react-flow__node', { state: 'visible', timeout: 10_000 })

    // Click the Trigger node to open NodePanel
    await app.page.locator('.react-flow__node').filter({ hasText: 'Trigger' }).click()

    // NodePanel label input should appear
    const labelInput = app.page.locator('[aria-label="Node label"]')
    await expect(labelInput).toBeVisible({ timeout: 5_000 })
    await expect(labelInput).toHaveValue('Trigger')

    // Edit the label
    await labelInput.fill('My Trigger')
    await expect(labelInput).toHaveValue('My Trigger')
  })

  test('NodePanel Save button saves label changes', async ({ app, programId, apiHelpers }) => {
    await apiHelpers.saveGraph(programId, makeTriggerAgentGraph())
    const program = await apiHelpers.getProgram(programId)

    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    await app.page.waitForSelector('.react-flow__node', { state: 'visible', timeout: 10_000 })
    await app.page.locator('.react-flow__node').filter({ hasText: 'Trigger' }).click()

    const labelInput = app.page.locator('[aria-label="Node label"]')
    await expect(labelInput).toBeVisible({ timeout: 5_000 })
    await labelInput.fill('Renamed Trigger')

    // Click Save in NodePanel
    await app.page.locator('button:has-text("Save")').last().click()

    // Success message should appear
    await expect(
      app.page.locator('text=Saved successfully').first()
    ).toBeVisible({ timeout: 10_000 })
  })

  test('TriggerForm shows manual trigger description', async ({ app, programId, apiHelpers }) => {
    await apiHelpers.saveGraph(programId, makeTriggerAgentGraph())
    const program = await apiHelpers.getProgram(programId)

    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    await app.page.waitForSelector('.react-flow__node', { state: 'visible', timeout: 10_000 })
    await app.page.locator('.react-flow__node').filter({ hasText: 'Trigger' }).click()

    // The Manual trigger shows its description text
    await expect(
      app.page.locator('text=fires when you click').first()
    ).toBeVisible({ timeout: 5_000 })
  })

  test('TriggerForm switching to cron shows cron expression input', async ({
    app,
    programId,
    apiHelpers,
  }) => {
    await apiHelpers.saveGraph(programId, makeTriggerAgentGraph())
    const program = await apiHelpers.getProgram(programId)

    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    await app.page.waitForSelector('.react-flow__node', { state: 'visible', timeout: 10_000 })
    await app.page.locator('.react-flow__node').filter({ hasText: 'Trigger' }).click()

    // TriggerForm radio buttons: "Manual" and "Cron schedule"
    const cronRadio = app.page.locator('input[type="radio"][value="cron"]')
    await expect(cronRadio).toBeVisible({ timeout: 5_000 })
    await cronRadio.click()

    // Cron expression input should appear (placeholder "0 9 * * 1")
    await expect(
      app.page.locator('input[placeholder="0 9 * * 1"]').first()
    ).toBeVisible({ timeout: 5_000 })
  })

  test('KnowledgeForm shows Inline source tab as default', async ({ app, programId, apiHelpers }) => {
    await apiHelpers.saveGraph(programId, makeKnowledgeOnlyGraph())
    const program = await apiHelpers.getProgram(programId)

    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    await app.page.waitForSelector('.react-flow__node', { state: 'visible', timeout: 10_000 })
    await app.page.locator('.react-flow__node').filter({ hasText: 'My Knowledge' }).click()

    // KnowledgeForm source tabs: Inline, File, URL, Library
    await expect(app.page.locator('button:has-text("Inline")').first()).toBeVisible({ timeout: 5_000 })
    await expect(app.page.locator('button:has-text("File")').first()).toBeVisible()
    await expect(app.page.locator('button:has-text("URL")').first()).toBeVisible()
    await expect(app.page.locator('button:has-text("Library")').first()).toBeVisible()
  })

  test('KnowledgeForm Library tab shows search input', async ({ app, programId, apiHelpers }) => {
    await apiHelpers.saveGraph(programId, makeKnowledgeOnlyGraph())
    const program = await apiHelpers.getProgram(programId)

    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    await app.page.waitForSelector('.react-flow__node', { state: 'visible', timeout: 10_000 })
    await app.page.locator('.react-flow__node').filter({ hasText: 'My Knowledge' }).click()

    // Click Library tab
    await app.page.locator('button:has-text("Library")').first().click()

    // Library search input should appear (data-testid="library-search")
    await expect(
      app.page.locator('[data-testid="library-search"], input[placeholder*="Search entries"]').first()
    ).toBeVisible({ timeout: 5_000 })
  })

  test('EdgePanel shows source and target info when an edge is clicked', async ({
    app,
    programId,
    apiHelpers,
  }) => {
    await apiHelpers.saveGraph(programId, makeTriggerAgentGraph())
    const program = await apiHelpers.getProgram(programId)

    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    await app.page.waitForSelector('.react-flow__edge', { state: 'visible', timeout: 10_000 })

    // ReactFlow renders a transparent wide interaction path on top of the visual path
    // to make edges easier to click. We must click the interaction path, not the visual path.
    const interactionPath = app.page.locator('.react-flow__edge-interaction').first()
    await expect(interactionPath).toBeVisible({ timeout: 5_000 })

    // Force-click at the midpoint of the edge by evaluating in-browser
    await app.page.evaluate(() => {
      const path = document.querySelector('.react-flow__edge-interaction') as SVGPathElement | null
      if (!path) throw new Error('No edge interaction path found')
      const len = path.getTotalLength()
      const mid = path.getPointAtLength(len / 2)
      const rect = path.closest('svg')?.getBoundingClientRect()
      if (!rect) throw new Error('No SVG rect')
      const x = rect.left + mid.x
      const y = rect.top + mid.y
      path.dispatchEvent(new MouseEvent('click', { bubbles: true, cancelable: true, clientX: x, clientY: y }))
    })

    // EdgePanel should show "Source" and "Target" labels
    await expect(
      app.page.locator('text=Source').first()
    ).toBeVisible({ timeout: 5_000 })
    await expect(
      app.page.locator('text=Target').first()
    ).toBeVisible({ timeout: 5_000 })
  })

  test('deleting a node via Delete key removes it from the canvas', async ({
    app,
    programId,
    apiHelpers,
  }) => {
    await apiHelpers.saveGraph(programId, makeTriggerAgentGraph())
    const program = await apiHelpers.getProgram(programId)

    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    await app.switchEditorTab('Graph')
    await app.page.waitForSelector('.react-flow__node', { state: 'visible', timeout: 10_000 })

    // Click the Agent node to select it
    const agentNode = app.page.locator('.react-flow__node').filter({ hasText: 'Agent' })
    await agentNode.click()

    // The selected node should be highlighted
    await app.page.waitForTimeout(300)

    // Press Delete or Backspace to remove the node
    await app.page.keyboard.press('Delete')

    // The Agent node should disappear from the canvas
    await expect(
      app.page.locator('.react-flow__node').filter({ hasText: 'Agent' })
    ).not.toBeVisible({ timeout: 5_000 })
  })

  test('dragging a Knowledge node onto canvas adds it', async ({ app, programId, apiHelpers }) => {
    const program = await apiHelpers.getProgram(programId)

    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)
    await app.switchEditorTab('Graph')
    await app.page.waitForSelector('.react-flow__pane', { state: 'visible' })

    const canvasBox = await app.page.locator('.react-flow__pane').boundingBox()
    expect(canvasBox).not.toBeNull()

    await dragNodeFromPalette(
      app.page,
      'knowledge',
      canvasBox!.x + canvasBox!.width / 2,
      canvasBox!.y + canvasBox!.height / 2,
    )

    await expect(
      app.page.locator('.react-flow__node').filter({ hasText: 'Knowledge' })
    ).toBeVisible({ timeout: 5_000 })
  })

  test('dragging an Action node onto canvas adds it', async ({ app, programId, apiHelpers }) => {
    const program = await apiHelpers.getProgram(programId)

    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)
    await app.switchEditorTab('Graph')
    await app.page.waitForSelector('.react-flow__pane', { state: 'visible' })

    const canvasBox = await app.page.locator('.react-flow__pane').boundingBox()
    expect(canvasBox).not.toBeNull()

    await dragNodeFromPalette(
      app.page,
      'action',
      canvasBox!.x + canvasBox!.width / 2,
      canvasBox!.y + canvasBox!.height / 2,
    )

    await expect(
      app.page.locator('.react-flow__node').filter({ hasText: 'Action' })
    ).toBeVisible({ timeout: 5_000 })
  })

  test('dragging a Connector node onto canvas adds it', async ({ app, programId, apiHelpers }) => {
    const program = await apiHelpers.getProgram(programId)

    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)
    await app.switchEditorTab('Graph')
    await app.page.waitForSelector('.react-flow__pane', { state: 'visible' })

    const canvasBox = await app.page.locator('.react-flow__pane').boundingBox()
    expect(canvasBox).not.toBeNull()

    await dragNodeFromPalette(
      app.page,
      'connector',
      canvasBox!.x + canvasBox!.width / 2,
      canvasBox!.y + canvasBox!.height / 2,
    )

    await expect(
      app.page.locator('.react-flow__node').filter({ hasText: 'Connector' })
    ).toBeVisible({ timeout: 5_000 })
  })

  test('Auto-layout button is visible in author mode', async ({ app, programId, apiHelpers }) => {
    await apiHelpers.saveGraph(programId, makeTriggerAgentGraph())
    const program = await apiHelpers.getProgram(programId)

    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    // Auto-layout button should be visible (it is enabled when nodes.length > 0)
    await expect(
      app.page.locator('[aria-label="Auto-layout"]')
    ).toBeVisible({ timeout: 5_000 })
  })

  test('Auto-layout button is disabled for an empty graph', async ({ app, programId, apiHelpers }) => {
    const program = await apiHelpers.getProgram(programId)

    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    // No nodes — Auto-layout should be disabled
    await expect(
      app.page.locator('[aria-label="Auto-layout"]')
    ).toBeDisabled({ timeout: 5_000 })
  })
})
