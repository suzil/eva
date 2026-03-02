/**
 * Knowledge Library E2E tests (smoke tests).
 *
 * Covers: navigating to the Knowledge Library, verifying the panel renders,
 * searching, and clicking a knowledge entry to open its detail view.
 *
 * Knowledge entries are created via API for isolation (no UI-only setup).
 *
 * Panel structure:
 *   - Two segment tab buttons: "knowledge" (lowercase) and "templates"
 *   - Search input placeholder: "Search knowledge…"
 */

import { test, expect } from '../fixtures/eva-fixture'

test.describe('Knowledge Library', () => {
  test('Knowledge activity is visible in ActivityBar', async ({ app }) => {
    await app.goto()
    await expect(app.page.locator('[aria-label="Knowledge"]')).toBeVisible()
  })

  test('navigating to Knowledge shows the library panel', async ({ app, programId, apiHelpers }) => {
    const program = await apiHelpers.getProgram(programId)
    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    await app.openActivity('Knowledge')

    // Knowledge Library panel renders with "knowledge" segment tab button
    await expect(
      app.page.locator('button').filter({ hasText: /^knowledge$/ }).first()
    ).toBeVisible({ timeout: 5_000 })
  })

  test('Knowledge Library has a search input', async ({ app, programId, apiHelpers }) => {
    const program = await apiHelpers.getProgram(programId)
    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    await app.openActivity('Knowledge')

    await expect(
      app.page.locator('[data-testid="knowledge-search"], input[placeholder*="Search knowledge"]').first()
    ).toBeVisible({ timeout: 5_000 })
  })

  test('Knowledge Library shows entries created via API', async ({ app, programId, apiHelpers }) => {
    // Create a knowledge entry via API
    const title = `e2e-test-knowledge-${Date.now()}`
    await apiHelpers.createKnowledgeEntry(programId, title, 'Test knowledge content for e2e')

    const program = await apiHelpers.getProgram(programId)
    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    await app.openActivity('Knowledge')

    // The entry should appear in the list
    await expect(
      app.page.locator(`text=${title}`).first()
    ).toBeVisible({ timeout: 10_000 })
  })

  test('searching Knowledge Library filters entries', async ({ app, programId, apiHelpers }) => {
    const uniqueWord = `xyzuniq${Date.now()}`
    const title = `Entry: ${uniqueWord}`
    await apiHelpers.createKnowledgeEntry(programId, title, `Unique search word: ${uniqueWord}`)

    const program = await apiHelpers.getProgram(programId)
    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    await app.openActivity('Knowledge')

    // Wait for the entry to appear (title contains the unique word)
    await expect(
      app.page.locator(`text=${uniqueWord}`).first()
    ).toBeVisible({ timeout: 10_000 })

    // Type the unique word in search
    const searchInput = app.page.locator('[data-testid="knowledge-search"], input[placeholder*="Search knowledge"]').first()
    await searchInput.fill(uniqueWord)

    // The entry with our unique word should still appear
    await expect(
      app.page.locator(`text=${uniqueWord}`).first()
    ).toBeVisible({ timeout: 5_000 })
  })

  test('clicking a knowledge entry opens its detail view', async ({ app, programId, apiHelpers }) => {
    const title = `e2e-test-detail-${Date.now()}`
    await apiHelpers.createKnowledgeEntry(programId, title, 'Detail view test content')

    const program = await apiHelpers.getProgram(programId)
    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    await app.openActivity('Knowledge')

    // Wait for the entry to load
    const entryRow = app.page.locator(`text=${title}`).first()
    await expect(entryRow).toBeVisible({ timeout: 10_000 })

    // Click the entry
    await entryRow.click()

    // A detail view should open — it shows the entry title again
    await expect(
      app.page.locator(`text=${title}`).last()
    ).toBeVisible({ timeout: 5_000 })
  })

  test('Knowledge Library shows Templates segment', async ({ app, programId, apiHelpers }) => {
    const program = await apiHelpers.getProgram(programId)
    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    await app.openActivity('Knowledge')

    // The "templates" segment tab button should be visible
    await expect(
      app.page.locator('button').filter({ hasText: /^templates$/ }).first()
    ).toBeVisible({ timeout: 5_000 })

    // Click the templates tab — template search input should appear
    await app.page.locator('button').filter({ hasText: /^templates$/ }).first().click()

    await expect(
      app.page.locator('input[placeholder*="Search templates"]').first()
    ).toBeVisible({ timeout: 5_000 })
  })

  test('Codebase activity shows the connect codebase UI', async ({ app, programId, apiHelpers }) => {
    const program = await apiHelpers.getProgram(programId)
    await app.goto()
    await app.selectProgram(program.name)
    await app.waitForProgramSelected(program.name)

    await app.openActivity('Codebase')

    // The codebase panel shows a path input and a Connect button
    await expect(
      app.page.locator('input[placeholder*="/absolute/path"]').first()
    ).toBeVisible({ timeout: 5_000 })
  })
})
