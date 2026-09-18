# Testing guide

Use `RED → GREEN → REFACTOR → VERIFY`: first demonstrate the missing contract,
implement only enough to satisfy it, remove local duplication, then run source
and compiled suites.

## Test ownership

| Area | Primary test |
| --- | --- |
| SSE framing and transport | `test/opencode-shell-sse-test.el` |
| Event validation and routing | `test/opencode-shell-event-test.el` |
| Pure lifecycle/hydration | `test/opencode-shell-state-test.el` |
| Pure interaction transitions | `test/opencode-shell-interaction-test.el` |
| Pure response normalization | `test/opencode-shell-response-test.el` |
| Cross-layer pipeline contracts | `test/opencode-shell-pipeline-test.el` |
| Commands, rendering, and regressions | `test/opencode-shell-test.el` |
| Editor-visible invariants | `test/opencode-shell-acceptance-test.el` |

Characterization tests freeze existing behavior before a move. Unit tests own
pure tables and transitions. Pipeline tests cover boundaries and callbacks.
Acceptance tests cover bytes, properties, markers, drafts, point, viewport,
undo, hidden buffers, and coalesced rendering.

## Commands

Run one source suite:

    emacs -Q --batch -L . -L test -L test/fixtures \
      -l test/opencode-shell-state-test.el -f ert-run-tests-batch-and-exit

Run a named regression:

    emacs -Q --batch -L . -L test -L test/fixtures \
      -l test/opencode-shell-pipeline-test.el \
      --eval "(ert-run-tests-batch-and-exit 'test-name)"

Run all source tests with `make test`. Run the required final verification with
`make verify`; it cleans, runs source tests, byte-compiles every module and test,
then runs the compiled suite. If source edits appear ignored, run `make clean`:
an older `.elc` can still win when `load-prefer-newer` is nil.

## Deterministic async tests

- Capture success and failure callbacks instead of sleeping or using real I/O.
- Invoke callbacks out of order and more than once to prove ID/generation guards.
- Use explicit authoritative empty snapshots; nil data is still successful data.
- Stub `get-buffer-window` for visible/hidden cases and drain keyed work with
  `opencode-shell-async-drain` when testing scheduled presentation.
- Assert state before presentation, then assert bytes/properties/markers after
  the scheduled render.
- Use synthetic payload tokens and assert they never reach display or logs.

Every bug fix needs a failing regression. Every module move needs focused unit
coverage plus existing characterization. Do not weaken assertions to accommodate
timing or ordering changes.
