# The NERV Interface — Phase 2 Demo

A step-by-step walkthrough of all 6 Phase 2 features across 8 demo steps.
Estimated time: 15–20 minutes.

---

## Prerequisites

| Requirement | Details |
|-------------|---------|
| Backend + frontend running | `make dev` from the repo root |
| LLM API key | `EVA_LLM_API_KEY` (OpenAI) **or** `EVA_ANTHROPIC_API_KEY` (Anthropic) set in `.env` |
| Credential encryption key | `EVA_CREDENTIAL_KEY` set in `.env` (any 32+ character string) |

**`.env` example** (project root):
```
EVA_CREDENTIAL_KEY=demo-credential-key-change-in-production
EVA_LLM_API_KEY=sk-...
# EVA_ANTHROPIC_API_KEY=sk-ant-...   # alternative to OpenAI
```

---

## Setup

Run once before the demo (backend must be running):

```bash
make seed
```

This creates two programs and registers the Eva codebase:
- **Weekly Project Summarizer** — 5-node program for demo steps 1, 2, 8
- **Code Review Pipeline** — 2-node program (no connector wired) for demo steps 5, 6, 7
- **Eva codebase** connected to the Weekly Summarizer — background knowledge extraction starts immediately (entries appear in the Knowledge Library within ~10 seconds)

To reset and re-seed from scratch:

```bash
make reset-db
make dev     # restart backend
make seed
```

---

## Demo Steps

### Step 1 — EVA Aesthetic *(Feature A: Evangelion Theme)*

Open `http://localhost:5173`.

**What to show:**
- Dark NERV-style canvas with scan-line texture
- Rajdhani display font on activity bar labels and headings
- Green glow (`eva-green`) on node borders and port handles
- Red/amber accent colors (`nerv-red`, `warn-amber`) on error states
- NERV monospace typography throughout the UI

**Expected:** The app loads cleanly with the full EVA aesthetic. No default browser fonts or plain white backgrounds.

---

### Step 2 — Spec Tab *(Feature D: Declarative YAML View)*

1. Click the Programs icon in the ActivityBar (top-left stack icon).
2. Select **Weekly Project Summarizer** from the list.
3. Click the **SPEC** tab in the tab bar above the canvas.

**What to show:**
- The program graph serializes to human-readable YAML with `eva.version: "1"` at the top
- All 5 nodes appear as YAML keys with their config inline
- Monaco editor is active with the `eva-dark` theme (green comment glyphs, amber strings)

**Live edit:**
- Change the trigger schedule from `"0 9 * * 1"` to `"0 10 * * 1"` (9am → 10am)
- Press `Cmd+S` (or click **Save Spec**)
- Click the **GRAPH** tab — the Weekly Trigger node now shows `10:00` in its schedule badge

**Expected:** Bidirectional sync works. Editing YAML updates the graph.

---

### Step 3 — Knowledge Library *(Feature E: Auto-Knowledge)*

1. Click the **Knowledge** icon in the ActivityBar (book icon).
2. A program must be selected — if not, select Weekly Project Summarizer first.

**What to show:**
- The **Knowledge** segment shows entries grouped by source type
- **codebase** group contains:
  - `File Tree` — full directory snapshot
  - `Language Distribution` — TypeScript, Haskell, and other file counts
  - `Key Files` — manifests and entry points (cabal, package.json, etc.)
  - `Dependencies` — parsed from `eva.cabal` and `package.json`
  - `Git Metadata` — current branch, dirty flag, recent commits
  - `Codebase Structure Summary` — LLM-generated overview (may take ~10s after first seed)
- Click any entry to expand it in the Detail Panel (right side)
- Use the search bar to filter by keyword (e.g. `"haskell"`)

**Note on Linear entries (optional):** If a Linear credential is configured in Settings, clicking **Refresh** in the knowledge context re-runs the Linear extractor and adds a "Project Inventory", "Workflow States", and "Recent Activity Summary" group under `linear`.

---

### Step 4 — MAGI Generates a Program *(Feature B: AI Assistant)*

1. Press `⌘K` to open the **CommandBar**.
2. Type: *"Create a program that scans Linear for issues opened in the last 24 hours and drafts a triage comment for each one using an Agent"*
3. Press `Enter` — MAGI sends the request to the backend.

**What to show:**
- MAGI's `propose_graph` tool produces a `GraphProposalCard` in the CommandBar results
- Click **Preview on Canvas** — a semi-transparent `GraphPreviewOverlay` appears on top of the canvas, showing the proposed 4-node graph with dashed-border preview nodes
- An amber **"Preview Mode"** banner floats above the canvas with three actions: **Accept**, **Edit in Chat**, **Cancel**
- Click **Accept** — the overlay dissolves and the proposed graph materializes as a real react-flow graph
- The program is auto-named "Issue Triage" (or whatever MAGI chose) in the programs list
- Click **Save** in the Toolbar to persist it

**Expected:** MAGI's proposal passes backend graph validation (`validateGraph`) before the preview card appears — invalid proposals are shown as error messages in the card instead.

---

### Step 5 — Prompt Hint: Unlinked Connector *(Feature F: Prompt Assistance)*

1. Select **Code Review Pipeline** from the Programs list.
2. Click the **Code Reviewer** agent node on the canvas.
3. The Detail Panel opens showing the AgentForm.

**What to show:**
- An amber warn banner (`PromptHints`) appears below the system prompt field
- The hint reads: the agent's system prompt references code review but no Connector node is wired to provide tools
- The banner has a CTA: **"Get detailed suggestions from MAGI →"**
- Click it — the Detail Panel tab switches to **MAGI** and the AssistantInput is pre-filled with `/improve`
- MAGI responds with actionable suggestions (add a Connector node, wire it to the agent's `tools` port, etc.)

**Expected:** The amber banner fires deterministically — it is a rule-based check, zero LLM cost.

---

### Step 6 — Template Insertion *(Feature F: Prompt Assistance)*

Still on the **Code Review Pipeline** program with the **Code Reviewer** agent node selected:

1. In the AgentForm, click the **Use Template** button (below the system prompt textarea).
2. The **TemplatePicker** modal opens — it lists the 7 built-in templates by category.
3. Select **Code Reviewer** from the Reviewer category.
4. Click **Use** — the template body replaces the current system prompt.

**What to show:**
- The inserted prompt contains `{{code}}` — a template variable placeholder
- The **Variable Bindings** section appears below the prompt, listing `code` as a required binding
- The `{{code}}` field has a dropdown — select the **instruction** port (the incoming data port) as the source
- Click **Save** in the Toolbar — the binding is persisted to the graph

**Expected:** The `{{code}}` variable is resolved at runtime from the bound port's message payload.

---

### Step 7 — Code Tab *(Feature C: Codebase Integration)*

1. Click the **CODE** tab in the tab bar above the canvas (the third tab after GRAPH and SPEC).
2. The ActivityBar switches to show the **Codebase** panel on the left.

**What to show:**
- The `Eva` codebase appears in the CodebasePanel (registered by `make seed`)
- Click the codebase to expand the file tree
- Navigate to `backend/src/Eva/Engine/Handlers/` → click `Agent.hs` to open it
- The file opens in a Monaco editor tab at the top of the code panel
- The `eva-dark` theme renders: green for Haskell comments (`--`), amber for string literals, blue for keywords

**Expected:** Files open in Monaco with the `eva-dark` theme. Multiple files can be open simultaneously in the FileTabBar (max 8, LRU eviction).

---

### Step 8 — Spec Export *(Feature D: Declarative YAML View)*

1. Select **Weekly Project Summarizer** from the Programs list.
2. Click the **SPEC** tab.

**What to show:**
- The full program is serialized to YAML — this is its portable definition
- The YAML is valid and can be committed to version control
- Press `Cmd+S` or click the **Save Spec** button
- The YAML is PUT back to the backend (`PUT /api/programs/:id/spec`) and the graph is updated

**Expected:** The spec round-trips cleanly — saving YAML and switching back to GRAPH shows the same graph with no data loss.

---

## Feature Coverage Summary

| Step | Feature Cluster | What it exercises |
|------|----------------|-------------------|
| 1 | A — EVA Aesthetic | NERV theme, Rajdhani font, glow tokens |
| 2 | D — Declarative View | SPEC tab, YAML serialize/edit, Cmd+S sync back |
| 3 | E — Auto-Knowledge | KnowledgeLibrary, codebase extraction, FTS search |
| 4 | B — AI Assistant (MAGI) | CommandBar, propose_graph tool, GraphPreviewOverlay |
| 5 | F — Prompt Assistance | PromptHints (7 rules), MAGI `/improve` prefill |
| 6 | F — Prompt Assistance | TemplatePicker, VariableBindingPanel, {{variable}} syntax |
| 7 | C — Codebase Integration | CODE tab, CodebasePanel, FileEditor (Monaco + eva-dark) |
| 8 | D — Declarative View | YAML export, spec round-trip |

---

## Troubleshooting

**"No LLM client configured" — agent step fails**

The `EVA_LLM_API_KEY` (OpenAI) or `EVA_ANTHROPIC_API_KEY` (Anthropic) env var is not set, or `.env` was not loaded. Make sure it is present in the project-root `.env` file before running `make dev`.

**Knowledge Library shows no entries**

The background extraction triggered by `make seed` may still be running. Wait ~10 seconds and refresh the Knowledge Library. If entries still do not appear, check the backend logs — the LLM summary step may have failed (non-fatal; the 5 structural entries are inserted without the LLM and should always be present).

**Seed fails with "connection refused"**

The backend is not running. Start it with `make dev`, wait for the `"Eva backend starting"` log line, then re-run `make seed`.

**WebSocket events not streaming (LLM tokens do not appear)**

Check that `vite.config.ts` uses the object form for the proxy with `ws: true`:
```ts
proxy: { '/api': { target: 'http://localhost:8080', ws: true } }
```
The bare string shorthand (`proxy: { '/api': 'http://localhost:8080' }`) silently drops WebSocket upgrades.

**SPEC tab shows "Conflict" banner**

This fires when the YAML has been edited but not yet saved back to the graph. Click **Save Spec** (`Cmd+S`) to resolve, or **Discard** to revert to the graph-source YAML.

**`make seed` succeeds but "Code Review Pipeline" is missing**

The seed script is idempotent — run `make seed` again. If the issue persists, check that the backend responded with HTTP 201 for the program creation step (visible in verbose `curl -v` mode).

**MAGI graph proposal fails validation**

If MAGI's `propose_graph` tool returns an error card, the proposed graph failed `validateGraph`. The error message in the card indicates which validation rule triggered (missing required port, cycle detected, incompatible edge types, etc.). Type a follow-up in the CommandBar to refine the request.
