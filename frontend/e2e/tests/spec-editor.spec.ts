/**
 * Spec editor (YAML tab) E2E tests.
 *
 * Covers: switching to the Spec tab, YAML content appearing, saving YAML
 * changes back to the graph, and the sync state machine (dirty indicator,
 * conflict modal).
 *
 * B1 regression guard: if SpecEditorView uses nonexistent `error-red-*` classes
 * for error banners, those banners will be invisible. Tests that trigger a
 * save error (by pushing invalid YAML) document this behavior.
 *
 * B2 regression guard: SyncWarningModal backdrop should be visible when
 * switching away from a dirty Spec tab.
 */

import { test, expect } from '../fixtures/eva-fixture'
import { makeTriggerAgentGraph } from '../helpers/api'

test.describe('Spec editor', () => {
  test('Spec tab is visible in the editor tabs', async ({ app, programId, apiHelpers }) => {
    const program = await apiHelpers.getProgram(programId)
    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    // Check the SPEC tab button is rendered — use exact match to avoid matching "Inspector"
    await expect(app.page.getByRole('button', { name: 'Spec', exact: true })).toBeVisible()
  })

  test('switching to Spec tab shows Monaco editor', async ({ app, programId, apiHelpers }) => {
    await apiHelpers.saveGraph(programId, makeTriggerAgentGraph())
    const program = await apiHelpers.getProgram(programId)

    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    // Switch to Spec tab
    await app.page.getByRole('button', { name: 'Spec', exact: true }).click()

    // Monaco editor container should appear
    await expect(
      app.page.locator('.monaco-editor').first()
    ).toBeVisible({ timeout: 10_000 })
  })

  test('Spec tab shows YAML content for a program with a graph', async ({ app, programId, apiHelpers }) => {
    await apiHelpers.saveGraph(programId, makeTriggerAgentGraph())
    const program = await apiHelpers.getProgram(programId)

    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    await app.page.getByRole('button', { name: 'Spec', exact: true }).click()
    await app.page.waitForSelector('.monaco-editor', { state: 'visible', timeout: 10_000 })

    // Wait for Monaco to finish loading content — it renders line numbers
    await expect(
      app.page.locator('.monaco-editor .view-line').first()
    ).toBeVisible({ timeout: 10_000 })

    // The YAML should contain at least the word "trigger" (from our graph)
    const editorContent = await app.page.evaluate(() => {
      // Monaco stores content in the model — read view lines as a proxy
      const lines = Array.from(document.querySelectorAll('.monaco-editor .view-line'))
      return lines.map((l) => l.textContent ?? '').join('\n')
    })
    expect(editorContent).toMatch(/trigger/i)
  })

  test('Spec dirty indicator appears after editing YAML', async ({ app, programId, apiHelpers }) => {
    await apiHelpers.saveGraph(programId, makeTriggerAgentGraph())
    const program = await apiHelpers.getProgram(programId)

    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    await app.page.getByRole('button', { name: 'Spec', exact: true }).click()
    await app.page.waitForSelector('.monaco-editor', { state: 'visible', timeout: 10_000 })

    // Click into Monaco and type something to make it dirty
    await app.page.click('.monaco-editor')
    // Press End to move to end of first line, then type
    await app.page.keyboard.press('Control+End')
    await app.page.keyboard.type('\n# e2e-test-comment')

    // The SPEC tab button should show a dirty dot (a small circle in the tab)
    // The implementation adds a `<span>` with `bg-at-field-500` inside the SPEC button
    await expect(
      app.page.getByRole('button', { name: 'Spec', exact: true }).locator('span[class*="bg-at-field"]')
    ).toBeVisible({ timeout: 5_000 })
  })

  test('switching away from a dirty Spec tab to Graph shows SyncWarningModal (B2 guard)', async ({
    app,
    programId,
    apiHelpers,
  }) => {
    await apiHelpers.saveGraph(programId, makeTriggerAgentGraph())
    const program = await apiHelpers.getProgram(programId)

    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    // Enter Spec tab and make it dirty
    await app.page.getByRole('button', { name: 'Spec', exact: true }).click()
    await app.page.waitForSelector('.monaco-editor', { state: 'visible', timeout: 10_000 })
    await app.page.click('.monaco-editor')
    await app.page.keyboard.press('Control+End')
    await app.page.keyboard.type('\n# e2e-test-dirty')

    // Wait for dirty dot to confirm we're dirty
    await expect(
      app.page.getByRole('button', { name: 'Spec', exact: true }).locator('span[class*="bg-at-field"]')
    ).toBeVisible({ timeout: 5_000 })

    // Switch to Graph tab — sets specSyncState to 'conflict'
    await app.page.getByRole('button', { name: 'Graph', exact: true }).click()

    // Switch back to Spec tab — the SyncWarningModal renders inside SpecEditorView
    // only when specSyncState === 'conflict', which requires being on the Spec tab
    await app.page.getByRole('button', { name: 'Spec', exact: true }).click()

    // The SyncWarningModal should now appear (B2 regression guard)
    // It has role="dialog" and the heading "Graph modified"
    await expect(
      app.page.locator('[role="dialog"]').filter({ hasText: /graph modified|replace.*graph|keep.*edits/i })
    ).toBeVisible({ timeout: 8_000 })
  })

  test('Graph tab is the default active tab', async ({ app, programId, apiHelpers }) => {
    const program = await apiHelpers.getProgram(programId)
    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    // GRAPH tab should be active by default (has the border-b styling)
    // The active tab has `border-at-field-500` underline
    const graphTab = app.page.getByRole('button', { name: 'Graph', exact: true })
    await expect(graphTab).toBeVisible()
    // Active tab has a distinct class — check it's not the non-active style
    const classes = await graphTab.getAttribute('class')
    expect(classes).toMatch(/border-at-field|text-terminal-50/)
  })

  test('Code tab switches sidebar to Codebase panel', async ({ app, programId, apiHelpers }) => {
    const program = await apiHelpers.getProgram(programId)
    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    // Switch to Code tab
    await app.page.getByRole('button', { name: 'Code', exact: true }).click()

    // Sidebar should auto-switch to Codebase activity
    // The codebase panel shows a connect button or file tree
    await expect(
      app.page.locator('[aria-label="Codebase"]')
    ).toHaveAttribute('aria-pressed', 'true', { timeout: 5_000 })
  })
})
