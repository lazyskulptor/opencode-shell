# AI contributor guide

## Start here

1. Read `docs/conversation-pipeline.md` and `docs/async-runtime.md`.
2. Run `make test` to establish a source baseline.
3. Make the smallest ownership-correct change using
   `RED → GREEN → REFACTOR → VERIFY`.
4. Finish with `make verify` and `git diff --check`.

## File map

- `opencode-shell-state.el`: pure turn, readiness, polling, and hydration state.
- `opencode-shell-interaction.el`: pure kind/ID interaction transitions.
- `opencode-shell-response.el`: pure, payload-free assistant-part normalization.
- `opencode-shell.el`: commands, buffer/network effects, markers, and presentation.
- `opencode-shell-{sse,event,async}.el`: protocol, event validation, and runtime.
- `test/*-test.el`: unit/integration tests; `test/opencode-shell-acceptance-test.el`
  covers editor-visible invariants.

## Non-negotiable rules

- Never add synchronous I/O, waits, or callback-time rendering.
- Never expose credentials, URLs, directories, session text, response bodies, or
  raw tool payloads in logs, tests, screenshots, issues, or commits.
- Snapshots remain authoritative; SSE remains validated and session-scoped.
- Preserve Composer draft, point, markers, viewport, undo, and hidden-buffer rules.
- Do not bypass source and compiled test runs.

See `docs/testing.md` for commands and test ownership. A change is done only when
focused regression coverage and `make verify` pass and user-facing contracts are
documented.
