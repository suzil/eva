#!/usr/bin/env bash
# Creates the Phase 2 "NERV Interface" demo programs and codebase via the
# Eva REST API.
#
# Usage:
#   ./scripts/seed-demo.sh [BASE_URL]
#
# BASE_URL defaults to http://localhost:8080. The backend must be running.
#
# What it creates:
#   1. "Weekly Project Summarizer" — 5-node program (Cron, Knowledge, Linear
#      Connector, Agent, Action). Demonstrates the Spec tab and YAML export.
#   2. "Code Review Pipeline" — 2-node program (Manual Trigger + Agent with no
#      connector wired). Demonstrates PromptHints, TemplatePicker, and Code tab.
#   3. Eva codebase registration + background knowledge extraction (demo step 3).
#
# After seeding:
#   1. Open http://localhost:5173 (dev) or http://localhost:8080 (Docker)
#   2. See docs/demo-nerv-interface.md for the full 8-step walkthrough

set -euo pipefail

BASE_URL="${1:-http://localhost:8080}"

echo "Seeding NERV Interface demo at ${BASE_URL} ..."

# ---------------------------------------------------------------------------
# 1. Weekly Project Summarizer
# ---------------------------------------------------------------------------
echo ""
echo "--- Weekly Project Summarizer ---"

# Idempotent re-seed: delete any existing program with that name
EXISTING_ID=$(curl -sf "${BASE_URL}/api/programs" \
  | python3 -c "
import json, sys
programs = json.load(sys.stdin)
match = next((p['id'] for p in programs if p['name'] == 'Weekly Project Summarizer'), None)
if match: print(match)
" 2>/dev/null || true)

if [ -n "${EXISTING_ID}" ]; then
  curl -sf -X DELETE "${BASE_URL}/api/programs/${EXISTING_ID}" > /dev/null
  echo "  Deleted existing program: ${EXISTING_ID}"
fi

PROGRAM_JSON=$(curl -sf -X POST "${BASE_URL}/api/programs" \
  -H "Content-Type: application/json" \
  -d '{"name": "Weekly Project Summarizer"}')

PROGRAM_ID=$(echo "${PROGRAM_JSON}" | python3 -c "import json,sys; print(json.load(sys.stdin)['id'])")
echo "  Created program: ${PROGRAM_ID}"

curl -sf -X PUT "${BASE_URL}/api/programs/${PROGRAM_ID}/graph" \
  -H "Content-Type: application/json" \
  -d '{
  "nodes": {

    "trigger-1": {
      "id": "trigger-1",
      "label": "Weekly Trigger",
      "posX": 100.0,
      "posY": 150.0,
      "type": {
        "type": "trigger",
        "config": {
          "type": "cron",
          "schedule": "0 9 * * 1"
        }
      }
    },

    "knowledge-1": {
      "id": "knowledge-1",
      "label": "Team Context",
      "posX": 100.0,
      "posY": 380.0,
      "type": {
        "type": "knowledge",
        "config": {
          "source": {
            "type": "_inline_text",
            "value": "Team: Eva development team\n\nProject: Eva — a visual prompt programming IDE that lets you build and run LLM-powered automation programs using a graph of typed nodes (Trigger, Agent, Knowledge, Connector, Action).\n\nMilestones:\n  M1 Foundation — complete\n  M2 Graph Engine — complete\n  M3 Canvas UI — complete\n  M4 Agent Runtime — complete\n  M5 Knowledge & Connectors — complete\n  M6 Operational Mode — complete\n  M7 Polish & Demo — in progress\n\nReport format:\n  1. Executive Summary (2–3 sentences)\n  2. Completed This Week\n  3. In Progress\n  4. Blockers (if any)\n  5. Next Week\n\nTone: concise and professional. Use bullet points. Include Linear issue identifiers where relevant."
          },
          "format": "text",
          "refreshPolicy": { "type": "static" }
        }
      }
    },

    "connector-1": {
      "id": "connector-1",
      "label": "Linear",
      "posX": 100.0,
      "posY": 610.0,
      "type": {
        "type": "connector",
        "config": {
          "system": "linear",
          "actionFilter": ["list_issues"]
        }
      }
    },

    "agent-1": {
      "id": "agent-1",
      "label": "Summarizer",
      "posX": 450.0,
      "posY": 380.0,
      "type": {
        "type": "agent",
        "config": {
          "provider": "anthropic",
          "model": "claude-sonnet-4-20250514",
          "systemPrompt": "You are a project analyst generating a structured weekly progress report for a software team.\n\nYou have access to Linear (project management). Use the list_issues tool to fetch the current issues from the Eva project. Filter for issues that are in progress, recently completed, or blocked.\n\nUse the provided team context (available in your context section) to understand project milestones, priorities, and the expected report format.\n\nProduce a structured weekly summary following the format in the team context. Be specific: reference issue identifiers and titles. Keep the report concise — aim for under 400 words.",
          "responseFormat": "text",
          "temperature": 0.3,
          "maxIterations": 5
        }
      }
    },

    "action-1": {
      "id": "action-1",
      "label": "Format Report",
      "posX": 800.0,
      "posY": 380.0,
      "type": {
        "type": "action",
        "config": {
          "operation": "template",
          "parameters": {
            "template": "# Weekly Project Summary\n\n{{input}}\n\n---\n*Generated by Eva*"
          },
          "errorHandling": { "mode": "fail" }
        }
      }
    }

  },
  "edges": [
    {
      "id": "edge-trigger-agent",
      "sourceNode": "trigger-1",
      "sourcePort": "event",
      "targetNode": "agent-1",
      "targetPort": "instruction",
      "category": "data"
    },
    {
      "id": "edge-knowledge-agent",
      "sourceNode": "knowledge-1",
      "sourcePort": "content",
      "targetNode": "agent-1",
      "targetPort": "context",
      "category": "resource"
    },
    {
      "id": "edge-connector-agent",
      "sourceNode": "connector-1",
      "sourcePort": "tools",
      "targetNode": "agent-1",
      "targetPort": "tools",
      "category": "resource"
    },
    {
      "id": "edge-agent-action",
      "sourceNode": "agent-1",
      "sourcePort": "output",
      "targetNode": "action-1",
      "targetPort": "input",
      "category": "data"
    }
  ]
}' > /dev/null

echo "  Graph saved (5 nodes, 4 edges)"

# ---------------------------------------------------------------------------
# 2. Code Review Pipeline
# ---------------------------------------------------------------------------
echo ""
echo "--- Code Review Pipeline ---"

EXISTING_CRP=$(curl -sf "${BASE_URL}/api/programs" \
  | python3 -c "
import json, sys
programs = json.load(sys.stdin)
match = next((p['id'] for p in programs if p['name'] == 'Code Review Pipeline'), None)
if match: print(match)
" 2>/dev/null || true)

if [ -n "${EXISTING_CRP}" ]; then
  curl -sf -X DELETE "${BASE_URL}/api/programs/${EXISTING_CRP}" > /dev/null
  echo "  Deleted existing program: ${EXISTING_CRP}"
fi

CRP_JSON=$(curl -sf -X POST "${BASE_URL}/api/programs" \
  -H "Content-Type: application/json" \
  -d '{"name": "Code Review Pipeline"}')

CRP_ID=$(echo "${CRP_JSON}" | python3 -c "import json,sys; print(json.load(sys.stdin)['id'])")
echo "  Created program: ${CRP_ID}"

# 2-node graph: Manual Trigger → Agent (no connector wired → PromptHints fires)
curl -sf -X PUT "${BASE_URL}/api/programs/${CRP_ID}/graph" \
  -H "Content-Type: application/json" \
  -d '{
  "nodes": {
    "trigger-crp": {
      "id": "trigger-crp",
      "label": "Run Trigger",
      "posX": 120.0,
      "posY": 200.0,
      "type": {
        "type": "trigger",
        "config": { "type": "manual" }
      }
    },
    "agent-crp": {
      "id": "agent-crp",
      "label": "Code Reviewer",
      "posX": 480.0,
      "posY": 200.0,
      "type": {
        "type": "agent",
        "config": {
          "model": "gpt-4o",
          "systemPrompt": "Review the provided code. Identify bugs, style issues, and suggest improvements.",
          "responseFormat": "text",
          "temperature": 0.2,
          "maxIterations": 3
        }
      }
    }
  },
  "edges": [
    {
      "id": "edge-trigger-agent-crp",
      "sourceNode": "trigger-crp",
      "sourcePort": "event",
      "targetNode": "agent-crp",
      "targetPort": "instruction",
      "category": "data"
    }
  ]
}' > /dev/null

echo "  Graph saved (2 nodes, 1 edge — no connector wired, triggers PromptHints)"

# ---------------------------------------------------------------------------
# 3. Codebase registration
# ---------------------------------------------------------------------------
echo ""
echo "--- Codebase registration ---"

# Resolve the repo root: this script lives in scripts/, so ../  gives the root.
REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"

# Connect the codebase to the Weekly Summarizer program.
# The backend will fire background knowledge extraction automatically.
CB_JSON=$(curl -sf -X POST "${BASE_URL}/api/programs/${PROGRAM_ID}/codebase" \
  -H "Content-Type: application/json" \
  -d "{\"path\": \"${REPO_ROOT}\"}" 2>&1 || true)

if echo "${CB_JSON}" | python3 -c "import json,sys; d=json.load(sys.stdin); print(d.get('id',''))" 2>/dev/null | grep -q .; then
  CB_ID=$(echo "${CB_JSON}" | python3 -c "import json,sys; print(json.load(sys.stdin)['id'])")
  echo "  Registered Eva codebase: ${CB_ID}"
  echo "  Root path: ${REPO_ROOT}"
  echo "  Background knowledge extraction started (Language Stats, File Tree,"
  echo "  Key Files, Dependencies, Git Metadata + LLM Summary)."
  echo "  Entries appear in the Knowledge Library within ~10 seconds."
else
  echo "  Warning: codebase registration returned an error — ${CB_JSON}"
  echo "  Knowledge Library entries will not be pre-populated."
  echo "  To register manually: POST /api/programs/<id>/codebase {\"path\": \"${REPO_ROOT}\"}"
fi

# ---------------------------------------------------------------------------
# 4. Summary
# ---------------------------------------------------------------------------
echo ""
echo "======================================================================"
echo "Seed complete."
echo ""
echo "Programs created:"
echo "  Weekly Project Summarizer  ${PROGRAM_ID}"
echo "  Code Review Pipeline       ${CRP_ID}"
echo ""
echo "Open the UI:"
echo "  Dev:    http://localhost:5173"
echo "  Docker: http://localhost:8080"
echo ""
echo "See docs/demo-nerv-interface.md for the full 8-step NERV Interface demo."
echo ""
echo "Before running the Weekly Summarizer, ensure:"
echo "  a) EVA_LLM_API_KEY or EVA_ANTHROPIC_API_KEY is set."
echo "  b) (Optional) To demo Linear tools: Settings → Add credential (linear),"
echo "     open Weekly Summarizer, select the Linear node, assign credential, Save."
echo "======================================================================"
