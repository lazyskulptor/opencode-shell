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

The target dependency direction is:

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
adapter; the temporary hydration adapter remains ready until authoritative
resource hydration is introduced.
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

## Change checklist

1. Identify the owning layer before editing.
2. Add a deterministic characterization, unit, integration, or acceptance test
   first; use the detailed guidance in `docs/testing.md` once it is added.
3. Keep asynchronous state reconciliation separate from presentation.
4. Run `make verify` and update this document when a contract changes.
