/**
 * Deploy and run E2E tests.
 *
 * Covers the deploy workflow (Draft → Active), the Run button, run streaming
 * (bottom panel appears), pause/resume, and cancellation.
 *
 * Tests that interact with a live run while it is in-progress require the
 * backend to have EVA_ANTHROPIC_API_KEY set.  Those tests are skipped
 * automatically when the key is absent.
 *
 * How it works:
 *   - When EVA_ANTHROPIC_API_KEY is set, deployedProgramId uses a graph with
 *     provider: 'anthropic', model: 'claude-3-5-haiku-20241022'.  The run takes
 *     ~2–10 s, so the Cancel button is visible long enough to assert.
 *   - Without the key, runs fail in <100 ms (dummyLLMClient) — fine for tests
 *     that only need a run to start, but not for Cancel button tests.
 *
 * Note: EVA_ANTHROPIC_API_KEY must be set in BOTH the backend process
 * (so envAnthropicClient is initialised at startup) and the test runner process
 * (so the fixture chooses the Anthropic graph).
 */

import { test, expect, HAS_ANTHROPIC_KEY } from '../fixtures/eva-fixture'
import { makeTriggerAgentGraph } from '../helpers/api'

test.describe('Deploy and run', () => {
  test('Deploy button is visible for draft programs', async ({ app, programId, apiHelpers }) => {
    await apiHelpers.saveGraph(programId, makeTriggerAgentGraph())
    const program = await apiHelpers.getProgram(programId)

    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    // Deploy button should be visible for a draft program
    await expect(app.page.locator('[aria-label="Deploy"]')).toBeVisible()
  })

  test('Run button is visible for draft programs', async ({ app, programId, apiHelpers }) => {
    await apiHelpers.saveGraph(programId, makeTriggerAgentGraph())
    const program = await apiHelpers.getProgram(programId)

    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    // Run button should be visible
    await expect(app.page.locator('[aria-label="Run"]')).toBeVisible()
  })

  test('deploying a program changes state to active and switches to Operate mode', async ({
    app,
    programId,
    apiHelpers,
  }) => {
    await apiHelpers.saveGraph(programId, makeTriggerAgentGraph())
    const program = await apiHelpers.getProgram(programId)

    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    // Click Deploy
    await app.page.click('[aria-label="Deploy"]')

    // Deploy success banner should appear (banner text: "Deployed. Program is now active")
    await expect(
      app.page.locator('text=Deployed').first()
    ).toBeVisible({ timeout: 20_000 })

    // Breadcrumb badge should now show "active"
    await app.waitForProgramState('active')

    // Mode should switch to Operate
    await expect(
      app.page.locator('[aria-label="App mode"] button:has-text("operate")')
    ).toHaveAttribute('aria-pressed', 'true', { timeout: 5_000 })
  })

  test('Deploy shows validation errors for an empty graph', async ({ app, programId, apiHelpers }) => {
    // Program has no graph nodes — deploy should fail validation
    const program = await apiHelpers.getProgram(programId)

    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    await app.page.click('[aria-label="Deploy"]')

    // Validation error panel should appear
    await expect(
      app.page.locator('text=Deploy failed')
    ).toBeVisible({ timeout: 10_000 })
  })

  test('Pause button is visible for active programs', async ({ app, deployedProgramId, apiHelpers }) => {
    const program = await apiHelpers.getProgram(deployedProgramId)

    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    // An active program should show the Pause button
    await expect(app.page.locator('[aria-label="Pause"]')).toBeVisible({ timeout: 5_000 })
  })

  test('pausing an active program changes state to paused', async ({ app, deployedProgramId, apiHelpers }) => {
    const program = await apiHelpers.getProgram(deployedProgramId)

    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    await app.page.click('[aria-label="Pause"]')

    // State badge should become "paused"
    await app.waitForProgramState('paused')

    // Pause button should disappear; Resume button should appear
    await expect(app.page.locator('[aria-label="Resume"]')).toBeVisible({ timeout: 10_000 })
  })

  test('resuming a paused program changes state to active', async ({ app, deployedProgramId, apiHelpers }) => {
    // Pause via API, then resume via UI
    await apiHelpers.pauseProgram(deployedProgramId)
    const program = await apiHelpers.getProgram(deployedProgramId)

    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    // State should be "paused"
    await app.waitForProgramState('paused')

    // Click Resume
    await app.page.click('[aria-label="Resume"]')

    // State badge should become "active"
    await app.waitForProgramState('active')
  })

  test('Run opens the bottom panel on Output tab', async ({ app, deployedProgramId, apiHelpers }) => {
    const program = await apiHelpers.getProgram(deployedProgramId)

    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    // Click Run — this starts the run regardless of LLM key
    await app.page.click('[aria-label="Run"]')

    // The bottom panel should open — the "Output" tab button appears in the tab bar
    // When the bottom panel opens and switches to Output, the OutputPanel content container
    // becomes visible. We check that the panel has expanded (the "Collapse" button appears).
    await expect(
      app.page.locator('[aria-label="Collapse bottom panel"]')
    ).toBeVisible({ timeout: 15_000 })
  })

  test('Cancel button appears while a run is in progress', async ({ app, deployedProgramId, apiHelpers }) => {
    // Requires a real LLM call to keep the run in-progress long enough to assert.
    // With no key, the run fails in <100 ms and the Cancel button never persists
    // long enough for Playwright's polling to detect it.
    test.skip(!HAS_ANTHROPIC_KEY, 'Requires EVA_ANTHROPIC_API_KEY set in both backend and test runner')

    const program = await apiHelpers.getProgram(deployedProgramId)

    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    await app.page.click('[aria-label="Run"]')

    // The Cancel button should appear while the run is active (B5 regression guard)
    await expect(app.page.locator('[aria-label="Cancel"]')).toBeVisible({ timeout: 30_000 })

    // Clean up — cancel the live run
    await app.page.locator('[aria-label="Cancel"]').click()
  })

  test('cancelling a run removes the Cancel button', async ({ app, deployedProgramId, apiHelpers }) => {
    test.skip(!HAS_ANTHROPIC_KEY, 'Requires EVA_ANTHROPIC_API_KEY set in both backend and test runner')

    const program = await apiHelpers.getProgram(deployedProgramId)

    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    await app.page.click('[aria-label="Run"]')

    // Wait for Cancel to appear (run is in-progress — real LLM call with Anthropic)
    const cancelBtn = app.page.locator('[aria-label="Cancel"]')
    await expect(cancelBtn).toBeVisible({ timeout: 30_000 })

    // Click Cancel
    await cancelBtn.click()

    // Cancel button should disappear; Run button should reappear
    await expect(app.page.locator('[aria-label="Run"]')).toBeVisible({ timeout: 10_000 })
  })

  test('Runs panel lists runs after clicking Runs activity', async ({ app, deployedProgramId, apiHelpers }) => {
    // Create a run via API first
    const run = await apiHelpers.createRun(deployedProgramId)
    const program = await apiHelpers.getProgram(deployedProgramId)

    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    // Navigate to Runs panel
    await app.openActivity('Runs')

    // The Runs panel shows filter buttons ("All", "Running", "Completed", "Failed")
    await expect(
      app.page.locator('button:has-text("All")').first()
    ).toBeVisible({ timeout: 5_000 })

    // Cancel the run if it's still running
    try { await apiHelpers.cancelRun(run.id) } catch { /* ignore */ }
  })

  test('WebSocket status indicator appears after starting a run', async ({ app, deployedProgramId, apiHelpers }) => {
    const program = await apiHelpers.getProgram(deployedProgramId)
    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    // Start a run — this activates the WebSocket stream and sets wsConnected in the UI store
    await app.page.click('[aria-label="Run"]')

    // The WS status dot renders once the run stream connects (B4 regression guard).
    // ws.onopen fires and calls setWsConnected(true) before any run_state event
    // arrives, so this works regardless of LLM key availability.
    await expect(
      app.page.locator('[aria-label="Backend connected"], [aria-label="Backend disconnected"]')
    ).toBeVisible({ timeout: 20_000 })

    // Clean up: cancel if still running
    const cancelBtn = app.page.locator('[aria-label="Cancel"]')
    if (await cancelBtn.isVisible()) {
      await cancelBtn.click()
    }
  })
})
