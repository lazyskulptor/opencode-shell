# Conversation pipeline

This document is the detailed architecture reference for contributors. Start
with [CONTRIBUTING.md](../CONTRIBUTING.md) or [AGENTS.md](../AGENTS.md) for the
short workflow. The final module-by-module guide will be completed with the
extraction; this initial contract prevents the move from changing ownership.

## Flow and ownership

```
command → observation → lifecycle → presentation
```

- **Commands** express user intent: prompt submit/abort, permission replies,
  question answers/rejections, and manual resync.
- **Observations** are HTTP snapshots, validated SSE deltas, and request
  outcomes. They are facts, not rendering instructions.
- **Lifecycle** derives turn phase, interaction blocking, hydration, polling,
  and Composer readiness from observations and local requests.
- **Presentation** turns the current model into protected transcript regions,
  interaction cards, transient status, and one writable Composer.

The dependency direction is:

```
opencode-shell-state.el
        ↑
opencode-shell-interaction.el   opencode-shell-response.el
        ↑                         ↑
                 opencode-shell.el
```

`opencode-shell-state.el` stays pure and has no package-module dependency. It
owns the turn record plus terminality, aggregate phase, and polling predicates.
Composer readiness is a pure conjunction of authoritative turn terminality,
local submission settlement, human-interaction settlement, and initial
hydration. `opencode-shell--request-status` is only a display/log summary and is
never readiness authority. `opencode-shell--composer-visible-p` is the sole UI
adapter and consumes the authoritative three-resource hydration state.
`opencode-shell-interaction.el` owns pure, kind-keyed in-flight transitions and
ID-based pending removal. The major mode wraps transport with those primitives:
only a matching callback may settle or apply a permission outcome, so a stale
callback cannot clear a newer request. Permission policy, resolved records,
anchors, refreshes, logs, and user messages remain explicit in
`opencode-shell.el`. Permission and question operations may run concurrently,
but duplicate operations of the same kind are rejected. Their endpoint,
payload, confirmation, refreshes, and user-message policies remain separate.
`opencode-shell-response.el` is the single payload-free adapter for raw assistant
parts. It normalizes tool spellings, name/status fallbacks, text/reasoning,
terminal evidence, response phase, completion blockers, lifecycle labels, and
ordered tool names. Network payloads stay in observations; presentation and
logs consume only these neutral helpers.
`opencode-shell--response-display` is the sole propertized response projector.
Initial insertion, incremental updates, forced rerenders, and interaction-card
relocation all consume its bytes and properties; `opencode-shell--insert-turn-blocks`
only establishes user/response bounds and read-only protection.
Initial session hydration tracks messages, permissions, and questions as three
authoritative resources. Only successful current-generation callbacks settle a
resource; unchanged and empty snapshots still count, while failures remain
visible and block Composer readiness until polling or `g r` succeeds. SSE keeps
its existing incremental role and never substitutes for the initial snapshots.
`opencode-shell.el` owns public commands, buffer-local state, network effects,
marker coordination, and the major mode. Do not add a generic send/receive
framework: share narrow invariants, while retaining explicit domain policies.
`opencode-shell-reload` reloads the lower-level state, interaction, and response
modules before the main mode so live development follows the same acyclic order
as normal `require` loading.

## Non-negotiable contracts

- Interactive paths and transport callbacks never wait for I/O or directly
  render. See [async-runtime.md](async-runtime.md).
- HTTP snapshots are authoritative; validated session-scoped SSE events are the
  incremental path. Both converge through the message cache/reducer.
- A callback must validate buffer liveness and generation before changing state.
- Hidden buffers update state but do not edit text or markers.
- Raw tool payloads, credentials, server URLs, directories, and response bodies
  never reach presentation, logs, tests, screenshots, or commits.
- Composer readiness is derived state. A non-complete turn, local submission,
  pending human interaction, or incomplete initial hydration blocks it.
- Rendered history is read-only; rerenders preserve Composer draft, point,
  markers, undo history, and window state.

## Module ownership

| Module | Owns | Allowed effects |
| --- | --- | --- |
| `opencode-shell-state.el` | turns, phase, polling, readiness, hydration | none |
| `opencode-shell-interaction.el` | kind/ID begin, match, finish, pending removal | none |
| `opencode-shell-response.el` | part normalization, completion, tool names | none |
| `opencode-shell.el` | commands, HTTP outcomes, buffer-local model, projection | network and visible-buffer edits |
| `opencode-shell-event.el` | validated event classification | none |
| `opencode-shell-async.el` | shared runtime, keyed delivery, timers | processes, timers, queued callbacks |

## Data flow

1. A command records local intent and starts an asynchronous request.
2. A generation-guarded callback or validated event updates observations.
3. Pure state/interaction/response helpers derive lifecycle facts.
4. `opencode-shell--schedule-render` coalesces presentation for a live, visible
   buffer; hidden buffers retain dirty state without editing text.
5. `opencode-shell--response-display` projects one canonical response while
   marker/render helpers protect history and preserve the Composer.

## Where should a change go?

- Add a turn/readiness/hydration rule: state module and state unit table first.
- Add a permission/question identity rule: interaction module; keep endpoint and
  payload policy explicit in `opencode-shell.el`.
- Add an assistant-part spelling or completion rule: response module; never pass
  raw tool input to presentation or logs.
- Change bytes, faces, spacing, or markers: canonical projector/render helpers
  plus property-aware pipeline or acceptance coverage.
- Change SSE parsing, routing, cadence, or reconnect policy: follow the layer map
  in [async-runtime.md](async-runtime.md), not the conversation model.

## Change checklist

1. Identify the owning layer before editing.
2. Add a deterministic characterization, unit, integration, or acceptance test
   first; use the detailed guidance in [testing.md](testing.md).
3. Keep asynchronous state reconciliation separate from presentation.
4. Run `make verify` and update this document when a contract changes.
5. Use [testing.md](testing.md) to select the narrow RED suite, then verify both
   source and compiled execution.
