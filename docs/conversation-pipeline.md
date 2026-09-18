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
