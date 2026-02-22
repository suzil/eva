#!/usr/bin/env bash
# Creates the "Weekly Project Summarizer" demo program via the Eva REST API.
#
# Usage:
#   ./scripts/seed-demo.sh [BASE_URL]
#
# BASE_URL defaults to http://localhost:8080. The backend must be running.
#
# What it creates:
#   - A program named "Weekly Project Summarizer"
#   - 5 nodes: Cron Trigger, Knowledge (team context), Linear Connector,
#               GPT-4o Agent (Summarizer), Action (Format Report)
#   - 4 edges wiring the graph
#
# After seeding:
#   1. Open Settings in the UI and add your Linear API key as a credential
#   2. Open the program, click the "Linear" connector node, and select the credential
#   3. Click Save, then Run (or wait for Monday 9am cron)

set -euo pipefail

BASE_URL="${1:-http://localhost:8080}"

echo "Seeding demo program at ${BASE_URL} ..."

# ---------------------------------------------------------------------------
# 0. Delete any existing "Weekly Project Summarizer" (idempotent re-seed)
# ---------------------------------------------------------------------------
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

# ---------------------------------------------------------------------------
# 1. Create the program
# ---------------------------------------------------------------------------
PROGRAM_JSON=$(curl -sf -X POST "${BASE_URL}/api/programs" \
  -H "Content-Type: application/json" \
  -d '{"name": "Weekly Project Summarizer"}')

PROGRAM_ID=$(echo "${PROGRAM_JSON}" | python3 -c "import json,sys; print(json.load(sys.stdin)['id'])")
echo "  Created program: ${PROGRAM_ID}"

# ---------------------------------------------------------------------------
# 2. Build and PUT the graph
# ---------------------------------------------------------------------------
#
# Node JSON envelope (per Aeson dropPrefix "node" + TaggedObject NodeType):
#   { "id": "...", "label": "...", "posX": N, "posY": N,
#     "type": { "type": "<tag>", "config": { ... } } }
#
# Ports used:
#   trigger  → event (data)
#   knowledge→ content (resource)
#   connector→ tools (resource)
#   agent    ← instruction (data), ← context (resource), ← tools (resource)
#             → output (data)
#   action   ← input (data)

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
echo ""
echo "Done. Program ID: ${PROGRAM_ID}"
echo ""
echo "Next steps:"
echo "  1. Open the UI:"
echo "       Docker:  http://localhost:8080"
echo "       Dev:     http://localhost:5173"
echo ""
echo "  Before clicking Run, make sure:"
echo "    a) An LLM API key is set (EVA_LLM_API_KEY or EVA_ANTHROPIC_API_KEY)."
echo "       Without it the agent step will fail with 'no LLM client configured'."
echo "    b) (Optional) To use Linear tools: Settings → Add credential (system: linear),"
echo "       then open the program, click the 'Linear' connector node, select the"
echo "       credential and Save. Without this the agent runs on knowledge context only."
echo ""
echo "  2. Click Run in the toolbar."
echo "     Watch the agent stream tokens in the Output panel."
echo "     If the run fails, the Output panel will show the error message."
