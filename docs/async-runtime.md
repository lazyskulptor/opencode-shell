# Asynchronous runtime principles

OpenCode Shell treats Emacs input latency as a correctness property. Network
activity, background sessions, and event bursts must not make editing the
composer noticeably pause.

This document owns runtime concurrency. See
[conversation-pipeline.md](conversation-pipeline.md) for state/presentation
ownership and [testing.md](testing.md) for deterministic callback tests.

## Invariants

1. Interactive and timer hot paths never perform synchronous network or process
   waits. Do not introduce `url-retrieve-synchronously`, `sleep-for`, or a busy
   `accept-process-output` loop.
2. Transport callbacks do not edit transcript or tabulated-list buffers. They
   validate buffer lifetime and generation, reconcile state, mark UI dirty, and
   enqueue keyed work.
3. Keyed work is latest-value coalesced and drained after 0.25 seconds of Emacs
   idle time; it never uses a wall-clock fallback that can interrupt unrelated
   editing. Intermediate text/tool display uses this same keyed queue as
   completion and interaction changes. Closing a buffer, changing generation,
   reloading, or stopping a runtime cancels pending work.
4. Hidden buffers do not render. Their authoritative state remains current and
   is rendered once when a window displays them again.
5. A server/profile owns at most one SSE connection and one runtime cadence,
   independent of the number of transcript buffers.
6. Validated, session-scoped `/event` message and part payloads are the primary
   incremental update path. Snapshot endpoints remain authoritative for initial
   load, reconnect, integrity checks, unsupported events, permissions, questions,
   and polling fallback.
7. SSE failure never disables progress. Nonblocking polling takes over and a
   bounded timer-based reconnect policy restores transiently failed streams.
   Protocol/configuration failures and repeated transport failures open the SSE
   circuit for that runtime while polling remains active.
8. Buffer text and markers are changed only on Emacs's main thread. The runtime
   minimizes that work instead of attempting unsafe worker-thread rendering.
9. Transport loss pauses the existing HTTP polling timer and any SSE reconnect
   cadence immediately instead of retrying in the background. For SSH-forwarded
   profiles (including profiles whose remote scope is inferred from a TRAMP
   `:directory`), going offline also terminates any owned tunnel process so a
   half-dead tunnel cannot hold the local port open. All `url-retrieve'
   buffers disable `url-http' keep-alive retries (`url-http-no-retry') and
   neutralize process sentinels and exit-query flags before buffer teardown so
   dropped or timed-out connections never trigger interactive kill prompts.
   For remote/SSH profiles reconnection happens only on explicit user action (`opencode-shell--resync'
   via `g r' / `C-c C-r', or `opencode-shell-restart' via `g R' / `C-c C-k').
10. Once SSH forwarding and endpoint health return, subscribed remote transcripts
    reuse the existing full authoritative resync in their original buffers.
    Idle unsubscribed buffers resync when displayed or explicitly requested;
     connection recovery never resets server-owned turn progress or Composer drafts.

Ordinary non-remote loopback HTTP services recover transient connection failures
using the existing bounded recovery scheduler under a separate local key. There
is one health attempt per shared runtime, never an automatic service restart.
Successful health resumes polling and queues authoritative resync for subscribers;
it does not itself settle hydration. Authentication/configuration/decoding errors
are not retryable. Final unsubscribe and reset cancel health deadlines and probes;
stale health callbacks cannot revive a replaced runtime. Exhaustion requires an
explicit successful request or manual recovery.

## Runtime layers

- **Protocol parser:** `opencode-shell-sse.el` incrementally converts binary HTTP
  headers, chunked transfer framing, and SSE fields into event/error values. Its
  feed operation is independent of input boundaries and has no process, timer,
  buffer, logging, or runtime-registry access.
- **Connection transport:** the same module owns one process, immutable attempt
  token, parser state, and header deadline. Its public boundary is start, stop,
  parsed-event callback, typed-error callback, and connection-state inspection.
  It does not reconnect, poll, reconcile snapshots, or render.
- **Application events:** `opencode-shell-event.el` decodes JSON once, validates
  the OpenCode 1.18.30 type and identity fields, and reduces unknown or malformed
  data to a bounded snapshot hint. It never logs raw payloads.
- **Async runtime:** `opencode-shell-async.el` shares one connection per server,
  owns session-aware subscribers, reconnect backoff/circuit policy, fallback and
  integrity polling, and keyed delivery. It never parses wire bytes.
- **State:** buffer-local ID-indexed message envelopes, delta and snapshot merge,
  generation checks, request deduplication, and changed-turn dirty flags. It does
  not modify displayed text. Live transcripts recreate a missing cache at this
  ownership boundary; stale work after a major-mode transition is discarded.
- **Presentation:** idle, visible-only, coalesced transcript/browser rendering.
  The same canonical response projector handles interim and final output.
  The bounded right-growing spinner is an overlay driven by its own animation
  timer and reset by current-session transcript progress, not polling. Composer
  text, point, markers, and undo history remain stable.
  Automatic renders (periodic, SSE, completion, width-driven table relayout, and
  load/reload migration) never issue explicit viewport commands such as
  `set-window-start`, `set-window-point`, `recenter`, or `scroll-*`; redrawing must
  not commandeer the window scroll position. They may still move point for text
  edits, and Emacs's native redisplay can adjust the view to keep point visible,
  so the guarantee is "no explicit scroll command from background updates", not
  "the screen never moves". User scrolling and turn navigation stay authoritative.

## Lifecycle

Opening a live transcript subscribes it to its profile runtime. The first
subscriber starts SSE or polling fallback. Validated events route only to the
matching session and update message or part state without a snapshot read.
Update/removal events coalesce by message or part identity, so their final order
is preserved within a delivery burst. Unsupported message, permission, and
question events reconcile only their affected resource; malformed or unclassified
events and the first event after reconnect reconcile all polling state.
Periodic (and `session.status`/`idle`) reconciliation stays cheap: it reconciles
`/permission`, `/question`, and `/status`, and probes `GET /session/:id' for its
`time.updated` marker (a few hundred bytes). A confirmed marker tracks the last
authoritative snapshot and advances *only* when a full message snapshot actually
applies: the first probe (unknown baseline) and any advance request one recovery
when SSE is quiet, an advance seen while SSE is delivering is *deferred*
(`--pending-session-updated`) rather than consumed, and a failed, skipped, or
still-in-flight fetch leaves the advance pending so the next probe retries it.
Each request captures its target marker before sending. Only an authoritative model
application with an unchanged message revision confirms that captured marker;
it cannot confirm a newer marker observed while the request was in flight.
The partial unseen-message merge used after a revision race does not confirm
recovery, and a superseded sequence or application exception leaves it pending. A
change SSE missed (e.g. only a `session.status` event arrived) is therefore still
confirmed once SSE goes quiet, and no observed advance is silently dropped. Idle
or SSE-covered ticks never re-download a large transcript.
Pending changes have a 30-second maximum SSE deferral measured from their first
observation, not the latest event or marker advance. The first reconciliation
after expiry attempts recovery even if metadata events keep arriving. Actual
message requests are at least 30 seconds apart on this periodic path, including
failures and revision races; skipped in-flight requests do not reset the clock.
Active transcripts and running fallback runtimes also check integrity after 120
seconds without an authoritative message application, even with an unchanged
metadata marker. Settled idle sessions are not awakened for this check. These
bounds use the existing poll/idle cadence, not a new high-frequency timer; manual
resync and initial/reconnect hydration keep their immediate fetch semantics.
Initial hydration, reconnect, fallback-with-changes, and explicit resync always
fetch the full snapshot; unsupported events keep an immediate full recovery.
Re-fetching a large transcript every tick is the freeze this avoids.
Optionally (`opencode-shell-async-snapshot-fetch`, off by default), the full
`GET /session/:id/message' fetch for a change is run in a `curl' subprocess.
Curl handles HTTP outside Emacs, but stdout reception and JSON parsing still run
on Emacs's main thread; this is not an off-thread parsing guarantee. The result is
applied under the same generation, buffer-liveness, and SSH-epoch guards as
the `url-retrieve' path (failures mirror its SSH/epoch handling), which remains
the default and the stub-compatible fallback. The subprocess path claims its
single terminal transition exactly once — whether from a terminal process status
(not a raw event string) or the request deadline — and delivers its callback only
to the first claimer, so a timeout plus a later sentinel cannot double-report.
It bounds the request with `opencode-shell--url-retrieve-timeout` (killing a hung
process and cleaning its buffer and header temp file), cleans up the same
resources when the process cannot be started, and runs cleanup even if a terminal
callback signals. The body parse itself still runs on the main thread
(subprocess-side compaction is a further, optional optimization).
Decoding failures invoke the error callback; an exception from the success
callback propagates after cleanup and must not invoke a second callback.
The async runtime keeps only owner-scoped cancellation functions; transport owns
process, deadline, and temporary-resource cleanup. Buffer kill/mode change,
unsubscribe, reset/reload, and ordinary async cancellation invoke that same
idempotent terminal path without delivering success/failure or marking a server
offline. Guarded requests also release their own deadline and in-flight slot.
Killing the response buffer (including a guard timeout) cancels its subprocess.
The final unsubscribe,
buffer cleanup, package reload, and server restart cancel pending idle jobs,
timers, requests where possible, and stream processes.

Protocol/configuration errors switch directly to polling. Transport closure and
header timeout reconnect with exponential backoff until the bounded failure
budget opens the circuit. A successful parsed event resets that budget. Runtime
logs include only the error type/reason and circuit state, never event data,
headers, URLs, directories, credentials, or response bodies. Successful routine
message, permission, and question snapshots do not append per-request log lines;
failures and semantic lifecycle transitions remain visible.

## Review checklist

- Does any interactive/timer callback wait for I/O?
- Can repeated events or responses collapse into one latest-state operation?
- Can a stale generation or killed buffer still be mutated?
- Does a hidden buffer perform text or marker work?
- Does another transcript create another server-level stream or cadence timer?
- Can a session-scoped event wake an unrelated transcript or request full history?
- Does parser behavior remain identical for arbitrary byte segmentation?
- Does transport code avoid reconnect/poll/UI policy, and does runtime code avoid
  HTTP/chunk/SSE parsing?
- Is snapshot reconciliation still authoritative after disconnect/reconnect?
- Are payloads, directories, auth values, and response bodies absent from logs?
- Do source and compiled ERT pass via `make verify`?
