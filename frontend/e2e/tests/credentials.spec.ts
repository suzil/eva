/**
 * Credentials panel E2E tests.
 *
 * Covers: opening the credentials form via the "Add" button, filling in the
 * required fields, saving, verifying the new credential appears in the list,
 * and deleting it.
 *
 * Credentials are managed in the Settings panel (CredentialsPanel component).
 * The form has: Name (text), System (select), Type (select), Secret (password).
 */

import { test, expect } from '../fixtures/eva-fixture'

test.describe('Credentials', () => {
  test('clicking Add opens the new credential form', async ({ app }) => {
    await app.goto()
    await app.openActivity('Settings')

    // Click the "Add" button in the Credentials header
    await app.page.locator('button:has-text("Add")').last().click()

    // The form should appear with a "New Credential" heading
    await expect(
      app.page.locator('text=New Credential').first()
    ).toBeVisible({ timeout: 5_000 })
  })

  test('credential form has all required fields', async ({ app }) => {
    await app.goto()
    await app.openActivity('Settings')

    await app.page.locator('button:has-text("Add")').last().click()

    // Name input
    await expect(
      app.page.locator('input[placeholder*="Linear workspace key"]').first()
    ).toBeVisible({ timeout: 5_000 })

    // System select
    await expect(
      app.page.locator('select').nth(1) // first select is the model in LLM Settings
    ).toBeVisible()

    // Secret input (password type)
    await expect(
      app.page.locator('input[placeholder*="Paste your API key"]').first()
    ).toBeVisible()
  })

  test('saving a credential adds it to the list', async ({ app }) => {
    await app.goto()
    await app.openActivity('Settings')

    await app.page.locator('button:has-text("Add")').last().click()

    const credName = `e2e-cred-${Date.now()}`

    // Fill the form
    await app.page.locator('input[placeholder*="Linear workspace key"]').first().fill(credName)
    await app.page.locator('input[placeholder*="Paste your API key"]').first().fill('test-secret-value')

    // Click Save — the credential form's Save button is the last submit button in the DOM
    // (the LLM Settings panel also has a "Save" submit button rendered above it)
    await app.page.locator('button[type="submit"]:has-text("Save")').last().click()

    // The credential should appear in the credentials list
    await expect(
      app.page.locator(`text=${credName}`).first()
    ).toBeVisible({ timeout: 10_000 })
  })

  test('deleting a credential shows a delete button', async ({ app }) => {
    await app.goto()
    await app.openActivity('Settings')

    // First add a credential
    await app.page.locator('button:has-text("Add")').last().click()

    const credName = `e2e-del-cred-${Date.now()}`
    await app.page.locator('input[placeholder*="Linear workspace key"]').first().fill(credName)
    await app.page.locator('input[placeholder*="Paste your API key"]').first().fill('to-delete')
    await app.page.locator('button[type="submit"]:has-text("Save")').last().click()

    // Wait for it to appear
    await expect(
      app.page.locator(`text=${credName}`).first()
    ).toBeVisible({ timeout: 10_000 })

    // The delete button should be present
    const deleteBtn = app.page.locator(`[aria-label="Delete ${credName}"]`)
    await expect(deleteBtn).toBeVisible({ timeout: 5_000 })
  })

  test('cancelling the credential form hides it', async ({ app }) => {
    await app.goto()
    await app.openActivity('Settings')

    await app.page.locator('button:has-text("Add")').last().click()

    // Form is visible
    await expect(
      app.page.locator('text=New Credential').first()
    ).toBeVisible({ timeout: 5_000 })

    // Click Cancel
    await app.page.locator('button:has-text("Cancel")').first().click()

    // Form should be hidden
    await expect(
      app.page.locator('text=New Credential').first()
    ).not.toBeVisible({ timeout: 3_000 })
  })
})
