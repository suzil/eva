import { test as base, expect, type Page } from '@playwright/test'
import * as api from '../helpers/api'

/**
 * True when the backend has been started with EVA_ANTHROPIC_API_KEY set.
 * When present, deployedProgramId uses an Anthropic-provider graph so that
 * runs make a real LLM call (taking ~2-10 s) instead of failing in <100 ms.
 * This is required for timing-sensitive tests such as the Cancel button tests.
 */
export const HAS_ANTHROPIC_KEY = !!process.env.EVA_ANTHROPIC_API_KEY

// ---------------------------------------------------------------------------
// Page object helpers shared by all tests
// ---------------------------------------------------------------------------

export class EvaApp {
  constructor(public readonly page: Page) {}

  /** Navigate to the app and wait until the programs list is visible. */
  async goto() {
    await this.page.goto('/')
    // The ActivityBar is always rendered — wait for it to confirm the app loaded
    await this.page.waitForSelector('[aria-label="Programs"]', { state: 'visible' })
  }

  /** Click the Programs activity bar item to make the programs list visible. */
  async openPrograms() {
    await this.page.click('[aria-label="Programs"]')
  }

  /** Select a program in the programs list by its name (exact text). */
  async selectProgram(name: string) {
    // Use first() since the canvas may also create a role="button" element with a matching label
    await this.page.locator(`[role="button"]:has-text("${name}")`).first().click()
  }

  /** Wait until the toolbar breadcrumb shows the given program name. */
  async waitForProgramSelected(name: string) {
    await expect(
      this.page.locator('nav[aria-label="Breadcrumb"]').getByText(name)
    ).toBeVisible({ timeout: 10_000 })
  }

  /** Click a toolbar button by its aria-label. */
  async clickToolbarButton(label: string) {
    await this.page.click(`[aria-label="${label}"]`)
  }

  /** Wait until the toolbar shows a given program state badge. */
  async waitForProgramState(state: string) {
    await expect(
      this.page.locator('nav[aria-label="Breadcrumb"]').locator(`text=${state}`)
    ).toBeVisible({ timeout: 15_000 })
  }

  /** Switch the editor tab (Graph / Code / Spec). */
  async switchEditorTab(tab: 'Graph' | 'Code' | 'Spec') {
    await this.page.click(`button:has-text("${tab.toUpperCase()}")`)
  }

  /** Open the MAGI assistant via the ActivityBar button. */
  async openMagi() {
    await this.page.click('[aria-label="MAGI Assistant"]')
  }

  /** Open the Command Palette via the toolbar ⌘K button. */
  async openCommandPalette() {
    await this.page.click('[aria-label="⌘K"]')
  }

  /** Navigate to a sidebar activity (Programs, Knowledge, Settings, etc.). */
  async openActivity(label: string) {
    await this.page.click(`[aria-label="${label}"]`)
  }
}

// ---------------------------------------------------------------------------
// Custom fixture type
// ---------------------------------------------------------------------------

type EvaFixtures = {
  /** A pre-created e2e-test program (cleaned up after each test). */
  programId: string
  /** A pre-created e2e-test program that already has a Trigger→Agent graph saved and is deployed. */
  deployedProgramId: string
  /** API helpers for direct REST calls. */
  apiHelpers: typeof api
  /** Page object wrapper. */
  app: EvaApp
}

// ---------------------------------------------------------------------------
// Fixture implementation
// ---------------------------------------------------------------------------

export const test = base.extend<EvaFixtures>({
  // Fresh program for each test
  programId: async ({}, use) => {
    const program = await api.createProgram(`e2e-test-${Date.now()}`)
    await use(program.id)
    // Cleanup: ignore errors if the test already deleted it
    try { await api.deleteProgram(program.id) } catch { /* already deleted */ }
  },

  // Deployed program with a valid graph.
  // When EVA_ANTHROPIC_API_KEY is set the graph uses the Anthropic provider so
  // that runs make a real LLM call instead of failing in <100 ms.
  deployedProgramId: async ({}, use) => {
    const program = await api.createProgram(`e2e-test-deployed-${Date.now()}`)
    const graph = HAS_ANTHROPIC_KEY
      ? api.makeTriggerAgentGraphForAnthropic()
      : api.makeTriggerAgentGraph()
    await api.saveGraph(program.id, graph)
    await api.deployProgram(program.id)
    await use(program.id)
    try { await api.deleteProgram(program.id) } catch { /* already deleted */ }
  },

  // API helpers passthrough
  apiHelpers: async ({}, use) => {
    await use(api)
  },

  // EvaApp page object — resets localStorage before each test
  app: async ({ page }, use) => {
    // Clear any persisted Zustand state (MAGI conversation history, etc.)
    await page.addInitScript(() => {
      localStorage.clear()
    })
    await use(new EvaApp(page))
  },
})

export { expect }
