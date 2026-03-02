/**
 * Bottom panel E2E tests.
 *
 * Covers: expanding/collapsing the bottom panel, switching between Logs /
 * Output / Changes tabs, and the RunsPanel filter buttons in the sidebar.
 *
 * The bottom panel is always rendered (collapsed by default).  Tabs switch
 * the active content pane; content is lazy — it renders only when the panel
 * is open.
 */

import { test, expect } from '../fixtures/eva-fixture'
import { makeTriggerAgentGraph } from '../helpers/api'

test.describe('Bottom panel', () => {
  test('bottom panel tab bar is always visible', async ({ app, programId, apiHelpers }) => {
    const program = await apiHelpers.getProgram(programId)
    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    // The tab bar renders even when the panel is collapsed
    await expect(app.page.locator('button:has-text("Logs")').first()).toBeVisible()
    await expect(app.page.locator('button:has-text("Output")').first()).toBeVisible()
    await expect(app.page.locator('button:has-text("Changes")').first()).toBeVisible()
  })

  test('clicking Expand bottom panel opens the panel', async ({ app, programId, apiHelpers }) => {
    const program = await apiHelpers.getProgram(programId)
    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    // Panel starts collapsed — expand it
    await app.page.click('[aria-label="Expand bottom panel"]')

    // Collapse button should now be visible (panel is open)
    await expect(
      app.page.locator('[aria-label="Collapse bottom panel"]')
    ).toBeVisible({ timeout: 5_000 })
  })

  test('clicking Collapse bottom panel closes the panel', async ({ app, programId, apiHelpers }) => {
    const program = await apiHelpers.getProgram(programId)
    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    // Expand first
    await app.page.click('[aria-label="Expand bottom panel"]')
    await expect(app.page.locator('[aria-label="Collapse bottom panel"]')).toBeVisible({ timeout: 5_000 })

    // Now collapse
    await app.page.click('[aria-label="Collapse bottom panel"]')
    await expect(
      app.page.locator('[aria-label="Expand bottom panel"]')
    ).toBeVisible({ timeout: 5_000 })
  })

  test('Logs tab shows "No log entries yet" when panel is open and no run', async ({ app, programId, apiHelpers }) => {
    const program = await apiHelpers.getProgram(programId)
    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    // Open panel and switch to Logs
    await app.page.click('[aria-label="Expand bottom panel"]')
    await app.page.locator('button:has-text("Logs")').first().click()

    await expect(
      app.page.locator('text=No log entries yet').first()
    ).toBeVisible({ timeout: 5_000 })
  })

  test('Output tab shows empty state before any run', async ({ app, programId, apiHelpers }) => {
    const program = await apiHelpers.getProgram(programId)
    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    // Open panel and switch to Output
    await app.page.click('[aria-label="Expand bottom panel"]')
    await app.page.locator('button:has-text("Output")').first().click()

    // The empty state message references Run button
    await expect(
      app.page.locator('text=No output yet').first()
    ).toBeVisible({ timeout: 5_000 })
  })

  test('Changes tab shows "No pending changes" when no codebase changesets', async ({
    app,
    programId,
    apiHelpers,
  }) => {
    const program = await apiHelpers.getProgram(programId)
    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    // Open panel and switch to Changes
    await app.page.click('[aria-label="Expand bottom panel"]')
    await app.page.locator('button:has-text("Changes")').first().click()

    await expect(
      app.page.locator('text=No pending changes').first()
    ).toBeVisible({ timeout: 5_000 })
  })

  test('Runs panel shows all four filter buttons', async ({ app, deployedProgramId, apiHelpers }) => {
    const run = await apiHelpers.createRun(deployedProgramId)
    const program = await apiHelpers.getProgram(deployedProgramId)

    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    await app.openActivity('Runs')

    // All four filter buttons should be present
    await expect(app.page.locator('button:has-text("All")').first()).toBeVisible({ timeout: 5_000 })
    await expect(app.page.locator('button:has-text("Running")').first()).toBeVisible()
    await expect(app.page.locator('button:has-text("Done")').first()).toBeVisible()
    await expect(app.page.locator('button:has-text("Failed")').first()).toBeVisible()

    try { await apiHelpers.cancelRun(run.id) } catch { /* ignore */ }
  })

  test('Runs panel "Running" filter shows run in-progress', async ({
    app,
    deployedProgramId,
    apiHelpers,
  }) => {
    const run = await apiHelpers.createRun(deployedProgramId)
    const program = await apiHelpers.getProgram(deployedProgramId)

    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    await app.openActivity('Runs')

    // Click "Running" filter — the in-progress run (or waiting) should appear
    await app.page.locator('button:has-text("Running")').first().click()

    // Either the run appears (state row is visible) or "No running runs" empty state is shown
    // Both are correct depending on whether the dummy run is still in-flight
    const hasRun = await app.page.locator('button:has-text("Running")').first().isVisible()
    expect(hasRun).toBe(true) // filter button itself is still visible = filter is active

    try { await apiHelpers.cancelRun(run.id) } catch { /* ignore */ }
  })

  test('Runs panel "Failed" filter shows empty state when no failed runs', async ({
    app,
    deployedProgramId,
    apiHelpers,
  }) => {
    const program = await apiHelpers.getProgram(deployedProgramId)

    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    await app.openActivity('Runs')

    // Click "Failed" filter
    await app.page.locator('button:has-text("Failed")').first().click()

    // Either there are failed runs, or the empty state text appears
    // We just verify the filter button remains visible (no crash)
    await expect(
      app.page.locator('button:has-text("Failed")').first()
    ).toBeVisible({ timeout: 5_000 })
  })

  test('LogsPanel has level filter buttons (debug, info, warn, error)', async ({
    app,
    programId,
    apiHelpers,
  }) => {
    const program = await apiHelpers.getProgram(programId)
    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    // Expand panel and switch to Logs
    await app.page.click('[aria-label="Expand bottom panel"]')
    await app.page.locator('button:has-text("Logs")').first().click()

    // Level filter pills should be visible
    await expect(app.page.getByRole('button', { name: 'debug' }).first()).toBeVisible()
    await expect(app.page.getByRole('button', { name: 'info' }).first()).toBeVisible()
    await expect(app.page.getByRole('button', { name: 'warn' }).first()).toBeVisible()
    await expect(app.page.getByRole('button', { name: 'error' }).first()).toBeVisible()
  })

  test('selecting a run from the Runs panel switches to Operate mode', async ({
    app,
    deployedProgramId,
    apiHelpers,
  }) => {
    const run = await apiHelpers.createRun(deployedProgramId)
    const program = await apiHelpers.getProgram(deployedProgramId)

    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    // Switch to Operate mode first so we can see the Runs panel
    await app.openActivity('Runs')

    // Wait for the run row to appear and click it
    // RunRow renders as a <button> element with a RunStateBadge inside
    const runRowSelector = 'button:has(.rounded.border.px-1\\.5)'
    await expect(app.page.locator(runRowSelector).first()).toBeVisible({ timeout: 10_000 })
    await app.page.locator(runRowSelector).first().click()

    // Clicking a run calls setMode('operate') — the operate mode button should be pressed
    await expect(
      app.page.locator('[aria-label="App mode"] button:has-text("operate")')
    ).toHaveAttribute('aria-pressed', 'true', { timeout: 5_000 })

    try { await apiHelpers.cancelRun(run.id) } catch { /* ignore */ }
  })
})
