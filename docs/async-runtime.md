# Asynchronous runtime principles

OpenCode Shell treats Emacs input latency as a correctness property. Network
activity, background sessions, and event bursts must not make editing the
composer noticeably pause.

## Invariants

1. Interactive and timer hot paths never perform synchronous network or process
   waits. Do not introduce `url-retrieve-synchronously`, `sleep-for`, or a busy
   `accept-process-output` loop.
2. Transport callbacks do not edit transcript or tabulated-list buffers. They
   validate buffer lifetime and generation, reconcile state, mark UI dirty, and
   enqueue keyed work.
3. Keyed work is latest-value coalesced and drained at idle time. Closing a
   buffer, changing generation, reloading, or stopping a runtime cancels it.
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
  The bounded right-growing spinner is an overlay driven by its own animation
  timer and reset by current-session transcript progress, not polling. Composer
  text, point, markers, and undo history remain stable.

## Lifecycle

Opening a live transcript subscribes it to its profile runtime. The first
subscriber starts SSE or polling fallback. Validated events route only to the
matching session and update message or part state without a snapshot read.
Update/removal events coalesce by message or part identity, so their final order
is preserved within a delivery burst. Unsupported message, permission, and
question events reconcile only their affected resource; malformed or unclassified
events and the first event after reconnect reconcile all polling state. Periodic
low-frequency reconciliation covers lost events.
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
