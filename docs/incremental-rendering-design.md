# Incremental rendering / reconciliation design

Status: ① REVERTED (SSE over SSH forward was unreliable); root freeze was a
native-comp artifact. ②③④⑤ not needed. See Outcome.

## Correction (why ① was reverted)

Enabling SSE for the SSH-forwarded profile backfired: the /event stream connects
but silently stalls over the tunnel, so completion events arrive late or not at
all and a turn sticks in `receiving` until a manual `g r`. Meanwhile the freeze ①
was meant to avoid (the ~284 ms full resync) turned out to be a cleared-eln-cache
artifact — natively compiled it is ~31 ms, not a freeze. So the authoritative
2-second poll is both fast enough and reliable. ① was reverted; remote/forwarded
profiles poll, only a local server uses SSE. Retained fixes: `g r` escalation +
cooldown, append-path blank line, and a lifecycle invariant that (re)starts
polling whenever an active turn has no runtime (and on a buffer becoming visible),
so a turn can never be left stuck with nothing to update it.

## Outcome (measured after ① + Emacs restart)

With SSE enabled and the code natively compiled, the real session (≈2069
messages, 400 turns) measured `normalize-turns` at ~31 ms and the forwarded-port
synchronous connect at ~0 ms. The earlier ~284 ms figure was inflated by running
non-natively (the eln cache had been cleared); a restart restored native speed.
Combined with ① removing the recurring full-snapshot resync, the user reports the
freeze "much improved". Steps ②③④⑤ are therefore left unimplemented — reopen this
plan only if a specific path (initial load / gap repair) is measured to freeze
again. The append-path blank-line bug fix shipped independently.

This document plans the removal of whole-transcript synchronous work that freezes
Emacs (single-threaded) on prompt submit and on a hidden buffer becoming visible.
It is a tracking doc: each step records what it reuses, what is new, its risk, a
test plan, and a status checkbox so work is not lost mid-way.

Read [conversation-pipeline.md](conversation-pipeline.md) and
[async-runtime.md](async-runtime.md) first; this design preserves their layer
ownership and non-negotiable contracts.

## Problem

Emacs runs one thread, so any synchronous O(whole-transcript) Lisp work blocks
the entire editor, not just the session buffer. Two such passes remain in the
steady state:

1. `opencode-shell--normalize-turns` re-walks every message envelope on each
   authoritative snapshot (resync). Measured: ~284 ms over 2033 envelopes on a
   390-turn / 359 KB session.
2. The full-rebuild path of `opencode-shell--render-turns` plus
   `opencode-shell--fontify-transcript` re-projects and re-fontifies the whole
   buffer.

These fire on prompt submit (remote profiles force a resync after POST) and when
a hidden buffer flushes deferred renders / re-lays tables on a width change. The
cost scales with transcript size, so large sessions freeze Emacs "once each
time".

## Goal (invariant)

> In steady-state use, no operation is O(whole transcript). The only O(n) pass is
> the first open of a session, and even that is chunked so first paint is fast and
> input stays responsive.

## Non-goals (explicitly excluded)

- **Virtualized / windowed rendering** (render only the viewport). Would make cost
  O(viewport) but breaks the plain-text-buffer model (whole-buffer isearch, copy,
  markers, `occur`) and has no good Emacs-native support. Too invasive.
- **External compute process** (offload parsing to a companion binary). Real
  parallelism, but turns a pure-elisp MELPA package into one with a binary
  dependency, and large-payload IPC marshalling may eat the gains.

We keep the buffer model and pure elisp; we finish the already-incremental design
rather than replace it.

## Why this is enhancement, not rewrite

The architecture is already incremental-capable. Reused as-is:

- `opencode-shell--normalize-turns` already reuses turn objects by identity
  (`eq`-preserving via its turn index / old-set); only the full O(n) walk is new
  cost to avoid.
- `opencode-shell--cache-message-snapshot` already computes `:changed-turns` and
  `:force`.
- `opencode-shell--render-turns` already has an append-only incremental path
  (update changed turns, append new turns).
- `opencode-shell--response-display` is the single response projector (one place
  to cache).
- `opencode-shell--schedule-render` / `--flush-render` / `--render-dirty` /
  `--render-dirty-turns` already coalesce and defer for hidden buffers.
- `opencode-shell--fontify-transcript` already marks turns `fontified` to skip
  settled turns.
- `--schedule-table-layout` already uses `run-with-idle-timer` (the chunking
  pattern).
- SSE framing/routing/recovery (`sse.el`, `async.el`, `event.el`,
  `recovery.el`) is complete and tested — the hardest piece is done.

New code is shallow: one render cache, one snapshot-diff shortcut, one batch
scheduler, one `while-no-input` wrapper.

## Steps (in apply order: low → high risk)

Each step is independently shippable with its own tests.

### ① SSE incremental for forwarded profiles — DONE (needs reload)

- [x] Enable SSE when the base URL host is loopback (SSH port-forwarded profiles
  included) via `opencode-shell--profile-sse-eligible-p`, used in
  `opencode-shell--start-polling`.
- Effect: submit skips its fallback resync once SSE is connected; polling backs
  off to reconcile cadence; the recurring full normalize/render stops firing in
  steady state. The remaining full passes are cold-start and gap-repair only.
- Reuse: all SSE/runtime machinery. New: one predicate + one call-site flag.
- Risk: low–medium (tunnel drop → fallback, already handled).
- Tests: `opencode-shell-forwarded-profile-enables-sse` (added).

### ② `while-no-input` guard on heavy renders — NOT STARTED

- [ ] Wrap the full-rebuild branch of `opencode-shell--render-turns` and
  `opencode-shell--refresh-table-layout` so user input aborts the pass and
  reschedules it (via the existing dirty/`schedule-render` path).
- Effect: does not speed the pass up, but "typing freezes Emacs" disappears.
- Reuse: existing render + dirty/coalesce infra; logic unchanged, only wrapped.
- Risk: low. Must guarantee aborted renders leave markers/Composer consistent
  (re-flag dirty, do not partially mutate — abort before buffer edits or make the
  edit atomic).
- Tests: characterization that an interrupted render reschedules and the buffer
  is unchanged until a clean pass completes.

### ③ Per-turn render cache — DONE

Implemented: `opencode-shell--cached-response-display` caches each turn's projected
string in a weak `eq` table keyed by turn identity, validated by
`--turn-state-signature` + render width. `--insert-turn-blocks` (full rebuild) and
`--update-turn-response` (reconcile/stream) use it, so every resync re-projects
only changed turns instead of the whole transcript — the per-prompt freeze path.
The permission-status turn is never cached. Tests:
`opencode-shell-acceptance-response-cache-skips-unchanged-turns`, and the whole
property/acceptance suite confirms byte-identical output. Still O(bytes) for the
raw buffer insert on a full rebuild and O(messages) for normalize (~31 ms native),
so initial load (⑤) and ④ remain if measurement shows they still freeze.

### (superseded) original ③ plan — NOT STARTED

- [ ] Cache the propertized output of `opencode-shell--response-display` keyed by
  (turn content signature + render width). In `--render-turns`, reuse the cached
  string for unchanged turns; recompute only changed turns. Invalidate on turn
  content change and on width change.
- Effect: even a full rebuild costs O(changed turns) for projection + fontify;
  only raw buffer insertion stays O(bytes).
- Reuse: single projector, append-only path, `fontified`-property skip, existing
  turn content/state signatures (`--turn-state-signature`).
- Risk: medium — cache invalidation correctness; must not leak stale faces or
  violate read-only/marker invariants.
- Tests: cache hit on unchanged turn; miss on content/width change; byte/property
  equality vs non-cached render (reuse property-aware pipeline tests).

### ④ Diff-based normalize — NOT STARTED

- [ ] When an authoritative snapshot does run (cold-start / gap-repair), process
  only envelopes whose id→content hash changed, instead of re-walking all. Build
  on `--cache-message-snapshot`'s change set.
- Effect: the rare full normalize drops from ~284 ms to tens of ms.
- Reuse: existing object-reuse in `--normalize-turns`, existing changed-turn
  computation.
- Risk: medium — reconciliation correctness after SSE gap / event loss must be
  preserved (authoritative snapshot must still be able to remove messages absent
  from the snapshot; see `authoritative` in `--render-messages`).
- Tests: snapshot that adds / edits / removes a message reconciles identically to
  the full walk; empty / unchanged snapshot is a cheap no-op.

### ⑤ Chunked initial load — NOT STARTED

- [ ] On first open, render the latest N turns (viewport) first, then insert older
  turns in idle-time batches (or on scroll). Normalize in batches.
- Effect: removes the last O(n) synchronous burst; fast first paint.
- Reuse: `run-with-idle-timer` pattern, dirty/coalesce infra, append path.
- Risk: medium–high — point/scroll/search behaviour during partial load; markers
  and `--transcript-end` must stay valid across batches; a submit during load
  must not corrupt ordering.
- Tests: partial-load ordering; submit during load; scroll triggers remaining
  batches; final state equals one-shot render.

## Contracts to preserve (from conversation-pipeline.md)

- Layer ownership: caches/schedulers live in `opencode-shell.el`; pure cores
  (`state`/`interaction`/`response`/`event`) stay effect-free.
- Callbacks validate buffer liveness and `--generation` before mutating.
- Hidden buffers update state but never edit text or markers.
- Rerenders preserve Composer draft, point, markers, undo history, window state.
- Rendered history is read-only; the writable Composer is the only editable
  region.
- No raw tool payloads / credentials / URLs / directories in presentation, logs,
  tests, screenshots, or commits.

## Testing / workflow

- One deterministic RED test per step before the change (repo rule); run both
  source and compiled (`make verify`).
- Keep async reconciliation separate from presentation.
- Update this doc's checkboxes and any changed contract as each step lands.

## Known bug (append-path turn separation) — FIXED

Status: [x] fixed (standalone, ahead of step ③).

Symptom: right after submitting a prompt, the new prompt line sticks to the
previous turn's last line (no blank separator); it self-corrects on the next full
reconcile render.

Root cause (verified by char dump on a reproduced submit):

- A complete response ends `"<text>\n\n"`, but its second `\n` carries the
  `opencode-shell-composer-label` property and `--transcript-end` points at that
  same `\n`.
- `--submit` uses the append-only branch of `opencode-shell--render-turns`, which
  inserts the new turn at `--transcript-end` — i.e. before that label `\n` — so
  the appended user block gets only one preceding newline and glues to the
  previous response. The full-render path is correct because it inserts turns
  contiguously and adds the label once at the end.

Fix (applied): in the append branch of `opencode-shell--render-turns`, when
`known-count > 0` and the insertion point is preceded by a single newline, insert
one read-only separator newline before the first appended turn, restoring the
blank line and matching the full-render layout. Guarded by `known-count > 0` so
the first turn and the full-render path are untouched. Regression test:
`opencode-shell-acceptance-appended-prompt-keeps-blank-line`.

## Open questions

- ③ cache key: reuse `--turn-state-signature` as the content signature, or add a
  cheaper per-turn hash?
- ⑤ batch size / scheduling cadence, and whether to load on scroll vs purely on
  idle.
- Interaction of ④ with the `authoritative` removal semantics on gap-repair.
