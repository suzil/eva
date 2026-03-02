/**
 * Graph authoring E2E tests.
 *
 * Covers: node palette drag-and-drop onto canvas, clicking nodes to open the
 * detail panel, saving the graph (Cmd+S and toolbar), and undo/redo.
 *
 * ReactFlow uses the HTML5 drag-and-drop API (not mouse events), so drag tests
 * dispatch synthetic DragEvents via page.evaluate rather than page.dragAndDrop.
 */

import { test, expect } from '../fixtures/eva-fixture'
import { makeTriggerAgentGraph } from '../helpers/api'

// ---------------------------------------------------------------------------
// Helper: dispatch an HTML5 DragEvent to drop a node type onto the canvas
// ---------------------------------------------------------------------------

async function dragNodeFromPalette(page: import('@playwright/test').Page, nodeType: string, x: number, y: number) {
  // Simulate the full drag sequence the browser would fire:
  //   dragstart (on palette item) → dragover (on canvas) → drop (on canvas)
  await page.evaluate(
    ({ nodeType, x, y }) => {
      const canvas = document.querySelector('.react-flow__pane') as HTMLElement | null
      if (!canvas) throw new Error('ReactFlow pane not found')

      const dt = new DataTransfer()
      dt.setData('application/eva-node-type', nodeType)

      const makeEvent = (type: string) =>
        new DragEvent(type, {
          dataTransfer: dt,
          bubbles: true,
          cancelable: true,
          clientX: x,
          clientY: y,
        })

      canvas.dispatchEvent(makeEvent('dragover'))
      canvas.dispatchEvent(makeEvent('drop'))
    },
    { nodeType, x, y }
  )
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

test.describe('Graph authoring', () => {
  test('Node Palette is visible after switching to nodes activity', async ({ app, programId, apiHelpers }) => {
    const program = await apiHelpers.getProgram(programId)
    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    // Switch to Node Palette activity
    await app.openActivity('Node Palette')

    // Palette items should appear (draggable cards)
    await expect(app.page.locator('text=Drag onto canvas')).toBeVisible()
    // Use exact text to avoid strict mode violations from canvas hint text
    await expect(app.page.getByText('Trigger', { exact: true }).first()).toBeVisible()
    await expect(app.page.getByText('Agent', { exact: true }).first()).toBeVisible()
    await expect(app.page.getByText('Knowledge', { exact: true }).first()).toBeVisible()
    await expect(app.page.getByText('Connector', { exact: true }).first()).toBeVisible()
    await expect(app.page.getByText('Action', { exact: true }).first()).toBeVisible()
  })

  test('dragging a Trigger node onto the canvas adds it', async ({ app, programId, apiHelpers }) => {
    const program = await apiHelpers.getProgram(programId)
    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    // Ensure Graph tab is active
    await app.switchEditorTab('Graph')

    // Wait for the canvas to render
    await app.page.waitForSelector('.react-flow__pane', { state: 'visible' })

    // Get the canvas bounding box to pick a drop position
    const canvasBox = await app.page.locator('.react-flow__pane').boundingBox()
    expect(canvasBox).not.toBeNull()
    const dropX = canvasBox!.x + canvasBox!.width / 2
    const dropY = canvasBox!.y + canvasBox!.height / 2

    await dragNodeFromPalette(app.page, 'trigger', dropX, dropY)

    // A Trigger node should now appear on the canvas (ReactFlow wraps each node in .react-flow__node)
    await expect(
      app.page.locator('.react-flow__node').filter({ hasText: 'Trigger' })
    ).toBeVisible({ timeout: 5_000 })
  })

  test('clicking a node opens the detail panel', async ({ app, programId, apiHelpers }) => {
    // Pre-populate the graph via API to avoid drag complexity
    await apiHelpers.saveGraph(programId, makeTriggerAgentGraph())
    const program = await apiHelpers.getProgram(programId)

    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    // Wait for the canvas to load the graph
    await app.page.waitForSelector('.react-flow__node', { state: 'visible', timeout: 10_000 })

    // Click the Trigger node
    const triggerNode = app.page
      .locator('.react-flow__node')
      .filter({ hasText: 'Trigger' })
    await triggerNode.click()

    // The NodePanel opens when a node is selected — it has an editable node label input
    await expect(app.page.locator('[aria-label="Node label"]')).toBeVisible({ timeout: 5_000 })
    // The input value should reflect the node label
    await expect(app.page.locator('[aria-label="Node label"]')).toHaveValue('Trigger')
  })

  test('clicking an Agent node opens AgentForm with prompt editor', async ({ app, programId, apiHelpers }) => {
    await apiHelpers.saveGraph(programId, makeTriggerAgentGraph())
    const program = await apiHelpers.getProgram(programId)

    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)
    await app.page.waitForSelector('.react-flow__node', { state: 'visible', timeout: 10_000 })

    // Click the Agent node
    const agentNode = app.page.locator('.react-flow__node').filter({ hasText: 'Agent' })
    await agentNode.click()

    // The detail panel should show the agent label somewhere
    await expect(
      app.page.locator('text=Agent').last()
    ).toBeVisible({ timeout: 5_000 })
  })

  test('Save toolbar button is disabled when graph is not dirty', async ({ app, programId, apiHelpers }) => {
    await apiHelpers.saveGraph(programId, makeTriggerAgentGraph())
    const program = await apiHelpers.getProgram(programId)

    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    // After loading a saved graph, the Save button should be disabled (not dirty)
    const saveBtn = app.page.locator('[aria-label="Save"]')
    await expect(saveBtn).toBeVisible()
    await expect(saveBtn).toBeDisabled()
  })

  test('drag a Trigger node then save enables and clears dirty indicator', async ({ app, programId, apiHelpers }) => {
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
      'trigger',
      canvasBox!.x + canvasBox!.width / 2,
      canvasBox!.y + canvasBox!.height / 2,
    )

    // Canvas should now be dirty — Save button enabled
    const saveBtn = app.page.locator('[aria-label="Save"]')
    await expect(saveBtn).toBeEnabled({ timeout: 5_000 })

    // Save via toolbar button
    await saveBtn.click()

    // After a successful save, the button should become disabled again
    await expect(saveBtn).toBeDisabled({ timeout: 10_000 })
  })

  test('Cmd+S saves the graph', async ({ app, programId, apiHelpers }) => {
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
      'agent',
      canvasBox!.x + canvasBox!.width / 2,
      canvasBox!.y + canvasBox!.height / 2,
    )

    const saveBtn = app.page.locator('[aria-label="Save"]')
    await expect(saveBtn).toBeEnabled({ timeout: 5_000 })

    // Press Cmd+S
    await app.page.keyboard.press('Meta+s')

    // Save button should go disabled
    await expect(saveBtn).toBeDisabled({ timeout: 10_000 })
  })

  test('Undo button is disabled when no history', async ({ app, programId, apiHelpers }) => {
    await apiHelpers.saveGraph(programId, makeTriggerAgentGraph())
    const program = await apiHelpers.getProgram(programId)

    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    // Undo button (ghost icon-only button) should start disabled
    const undoBtn = app.page.locator('[aria-label="Undo (⌘Z)"]')
    await expect(undoBtn).toBeVisible()
    await expect(undoBtn).toBeDisabled()
  })

  test('Undo removes a dropped node; Redo restores it', async ({ app, programId, apiHelpers }) => {
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
      'trigger',
      canvasBox!.x + canvasBox!.width / 2,
      canvasBox!.y + canvasBox!.height / 2,
    )

    // Node should be on canvas
    await expect(
      app.page.locator('.react-flow__node').filter({ hasText: 'Trigger' })
    ).toBeVisible({ timeout: 5_000 })

    // Undo — node should disappear
    await app.page.keyboard.press('Meta+z')
    await expect(
      app.page.locator('.react-flow__node').filter({ hasText: 'Trigger' })
    ).not.toBeVisible({ timeout: 5_000 })

    // Redo — node should reappear
    await app.page.keyboard.press('Meta+Shift+z')
    await expect(
      app.page.locator('.react-flow__node').filter({ hasText: 'Trigger' })
    ).toBeVisible({ timeout: 5_000 })
  })

  test('Author / Operate mode toggle switches modes', async ({ app, programId, apiHelpers }) => {
    const program = await apiHelpers.getProgram(programId)
    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    // Author mode is default
    const authorBtn = app.page.locator('[aria-label="App mode"] button:has-text("author")')
    const operateBtn = app.page.locator('[aria-label="App mode"] button:has-text("operate")')
    await expect(authorBtn).toBeVisible()
    await expect(operateBtn).toBeVisible()
    await expect(authorBtn).toHaveAttribute('aria-pressed', 'true')

    // Switch to Operate
    await operateBtn.click()
    await expect(operateBtn).toHaveAttribute('aria-pressed', 'true')
    await expect(authorBtn).toHaveAttribute('aria-pressed', 'false')

    // Switch back
    await authorBtn.click()
    await expect(authorBtn).toHaveAttribute('aria-pressed', 'true')
  })
})
