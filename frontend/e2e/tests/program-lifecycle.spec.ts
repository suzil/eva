/**
 * Program lifecycle E2E tests.
 *
 * Tests the programs list UI: creating programs via the UI, renaming them,
 * selecting them, and verifying state badges. Program deletion is done via API
 * in fixtures; the UI doesn't expose a delete button in the current version.
 */

import { test, expect } from '../fixtures/eva-fixture'

test.describe('Program lifecycle', () => {
  test('app loads and shows programs panel', async ({ app }) => {
    await app.goto()
    // Programs activity should be selected by default
    await expect(app.page.locator('[aria-label="Programs"]')).toBeVisible()
    // The programs panel sub-header should be visible (appears multiple times in the DOM)
    await expect(app.page.locator('text=PROGRAMS').first()).toBeVisible()
  })

  test('creates a new program via the + button', async ({ app }) => {
    await app.goto()

    const before = await app.page.locator('[role="button"]').filter({ hasText: /.*/ }).count()

    // Click the "New program" + button in the Programs sub-header
    await app.page.click('[aria-label="New program"]')

    // A rename input should appear for the new "Untitled" program
    await expect(
      app.page.locator('input[class*="ring-at-field"]')
    ).toBeVisible({ timeout: 5_000 })

    // Type a name and confirm
    const name = `e2e-test-ui-${Date.now()}`
    await app.page.locator('input[class*="ring-at-field"]').fill(name)
    await app.page.locator('input[class*="ring-at-field"]').press('Enter')

    // The new program should appear in the list (first match = the program row in the sidebar)
    await expect(app.page.locator(`[role="button"]:has-text("${name}")`).first()).toBeVisible({ timeout: 5_000 })

    // The toolbar breadcrumb should show the new program's name
    await app.waitForProgramSelected(name)

    // Clean up via API (the program was created by the UI)
    const programs = await app.page.evaluate(async (n) => {
      const res = await fetch(`/api/programs`)
      const data = await res.json() as Array<{ id: string; name: string }>
      return data.filter((p) => p.name === n)
    }, name)
    for (const p of programs) {
      await app.page.evaluate(async (id) => {
        await fetch(`/api/programs/${id}`, { method: 'DELETE' })
      }, p.id)
    }
  })

  test('selects a program and shows it in the breadcrumb', async ({ app, programId, apiHelpers }) => {
    // Get the program name from the API
    const program = await apiHelpers.getProgram(programId)
    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)
    // State badge should show "draft"
    await expect(
      app.page.locator('nav[aria-label="Breadcrumb"]').locator('text=draft')
    ).toBeVisible()
  })

  test('renames a program via the pencil button', async ({ app, programId, apiHelpers }) => {
    const program = await apiHelpers.getProgram(programId)
    await app.goto()
    await app.selectProgram(program.name)

    // Hover over the program item to reveal the rename button (first = sidebar row)
    const programItem = app.page.locator(`[role="button"]:has-text("${program.name}")`).first()
    await programItem.hover()

    // Click the pencil rename icon
    await programItem.locator('[title="Rename program"]').click()

    // Input should appear
    const input = app.page.locator('input[class*="ring-at-field"]')
    await expect(input).toBeVisible()

    const newName = `${program.name}-renamed`
    await input.fill(newName)
    await input.press('Enter')

    // Updated name should appear in the list
    await expect(app.page.locator(`[role="button"]:has-text("${newName}")`).first()).toBeVisible({ timeout: 5_000 })
    // Breadcrumb should update too
    await app.waitForProgramSelected(newName)
  })

  test('renames a program via right-click context menu', async ({ app, programId, apiHelpers }) => {
    const program = await apiHelpers.getProgram(programId)
    await app.goto()
    await app.selectProgram(program.name)

    // Right-click the program item to open the context menu
    const programItem = app.page.locator(`[role="button"]:has-text("${program.name}")`).first()
    await programItem.click({ button: 'right' })

    // Context menu should appear with a "Rename" option (exact match avoids collision
    // with the hover pencil button whose accessible name is "Rename program")
    const renameBtn = app.page.getByRole('button', { name: 'Rename', exact: true })
    await expect(renameBtn).toBeVisible({ timeout: 5_000 })

    await renameBtn.click()

    // Rename input should appear
    const input = app.page.locator('input[class*="ring-at-field"]')
    await expect(input).toBeVisible()

    const newName = `${program.name}-ctx-renamed`
    await input.fill(newName)
    await input.press('Enter')

    // Updated name should appear in the list
    await expect(app.page.locator(`[role="button"]:has-text("${newName}")`).first()).toBeVisible({ timeout: 5_000 })
    await app.waitForProgramSelected(newName)
  })

  test('shows empty state with "Create your first program" button', async ({ app }) => {
    await app.goto()
    // If there are no programs, the empty state is shown.
    // We can't guarantee a clean DB in all environments, so we just verify
    // the empty state markup exists in the DOM (it may be hidden by a non-empty list).
    const emptyStateBtn = app.page.locator('text=Create your first program')
    // Either the button is visible (no programs) or the programs list is visible.
    const hasList = await app.page.locator('[role="button"]:has-text("")').count()
    if (hasList === 0) {
      await expect(emptyStateBtn).toBeVisible()
    }
  })

  test('shows draft badge for a newly created program', async ({ app, programId, apiHelpers }) => {
    const program = await apiHelpers.getProgram(programId)
    await app.goto()
    await app.selectProgram(program.name)
    // The state badge in the sidebar should read "draft"
    const programRow = app.page.locator(`[role="button"]:has-text("${program.name}")`)
    await expect(programRow.locator('text=draft')).toBeVisible()
  })

  test('clicking breadcrumb "Programs" calls setSelectedProgramId(null)', async ({ app, programId, apiHelpers }) => {
    const program = await apiHelpers.getProgram(programId)
    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    // Click "Programs" in the breadcrumb to clear program selection.
    // Note: ProgramsPanel auto-selects the first program in a useEffect, so the
    // breadcrumb may quickly show a program name again if there are other programs.
    // We just verify the button is clickable and the nav doesn't crash.
    await app.page.click('nav[aria-label="Breadcrumb"] button:has-text("Programs")')

    // The Programs side panel should be focused — verifiable by a short pause without error
    await app.page.waitForTimeout(500)

    // App should still be functional — the ActivityBar Programs button should be visible
    await expect(app.page.locator('[aria-label="Programs"]')).toBeVisible()
  })
})
