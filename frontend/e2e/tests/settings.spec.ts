/**
 * Settings panel E2E tests (smoke tests).
 *
 * Covers: navigating to Settings via ActivityBar, verifying LLM Settings and
 * Credentials panels appear, and basic interaction with the LLM settings form.
 *
 * The Settings panel header is "MAGI / LLM Provider".
 * Provider is selected via buttons (OpenAI / Anthropic), NOT a <select>.
 * The model is a <select> element.
 */

import { test, expect } from '../fixtures/eva-fixture'

test.describe('Settings', () => {
  test('Settings activity is visible in ActivityBar', async ({ app }) => {
    await app.goto()
    await expect(app.page.locator('[aria-label="Settings"]')).toBeVisible()
  })

  test('navigating to Settings shows LLM Settings panel', async ({ app }) => {
    await app.goto()
    await app.openActivity('Settings')

    // The panel header is "MAGI / LLM Provider"
    await expect(
      app.page.locator('text=MAGI / LLM Provider').first()
    ).toBeVisible({ timeout: 5_000 })
  })

  test('LLM Settings form has a provider selector', async ({ app }) => {
    await app.goto()
    await app.openActivity('Settings')

    // Provider is shown as two buttons: "OpenAI" and "Anthropic"
    await expect(
      app.page.locator('button:has-text("OpenAI"), button:has-text("Anthropic")').first()
    ).toBeVisible({ timeout: 5_000 })
  })

  test('LLM Settings form has a model dropdown', async ({ app }) => {
    await app.goto()
    await app.openActivity('Settings')

    // The model is a <select> element (one select, for the model)
    await expect(app.page.locator('select').first()).toBeVisible({ timeout: 5_000 })
  })

  test('LLM Settings form has an API key field', async ({ app }) => {
    await app.goto()
    await app.openActivity('Settings')

    // API key input (password type)
    await expect(
      app.page.locator('input[type="password"]').first()
    ).toBeVisible({ timeout: 5_000 })
  })

  test('Settings panel shows Credentials section', async ({ app }) => {
    await app.goto()
    await app.openActivity('Settings')

    // Credentials panel header
    await expect(
      app.page.locator('text=Credentials').first()
    ).toBeVisible({ timeout: 5_000 })
  })

  test('Credentials section has an "Add credential" button', async ({ app }) => {
    await app.goto()
    await app.openActivity('Settings')

    await expect(
      app.page.locator('[aria-label="Add credential"], button:has-text("Add"), button:has-text("New credential")').first()
    ).toBeVisible({ timeout: 5_000 })
  })

  test('switching provider updates the model list', async ({ app }) => {
    await app.goto()
    await app.openActivity('Settings')

    // Wait for the model select to be visible
    const modelSelect = app.page.locator('select').first()
    await expect(modelSelect).toBeVisible({ timeout: 5_000 })

    // Get current model
    const initialModel = await modelSelect.inputValue()

    // Click the other provider button
    const openaiBtn = app.page.locator('button:has-text("OpenAI")').first()
    const anthropicBtn = app.page.locator('button:has-text("Anthropic")').first()

    // Determine which to click by checking aria-pressed or active styling
    const openaiActive = await openaiBtn.evaluate((el) =>
      el.className.includes('at-field') || el.className.includes('border-at-field')
    )
    if (openaiActive) {
      await anthropicBtn.click()
    } else {
      await openaiBtn.click()
    }

    // The model dropdown should update — verify the model changed
    await expect(modelSelect).toBeVisible()
    const newModel = await modelSelect.inputValue()
    // After provider switch the default model is set to the first in the new provider's list
    expect(newModel.length).toBeGreaterThan(0)
    expect(newModel).not.toEqual(initialModel)
  })
})
