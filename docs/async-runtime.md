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
6. `/event` is a wake-up hint. Snapshot endpoints and existing message-ID
   reconciliation are authoritative for completion, permissions, questions,
   and transcript content.
7. SSE failure never disables progress. Nonblocking polling takes over and a
   bounded timer-based reconnect policy restores the stream when possible.
8. Buffer text and markers are changed only on Emacs's main thread. The runtime
   minimizes that work instead of attempting unsafe worker-thread rendering.

## Runtime layers

- **Transport:** asynchronous HTTP, shared SSE lifecycle, reconnect, and polling
  fallback. It emits privacy-safe state-change signals only.
- **State:** buffer-local snapshot reconciliation, generation checks, request
  deduplication, and dirty flags. It does not modify displayed text.
- **Presentation:** idle, visible-only, coalesced transcript/browser rendering.
  Composer text, point, markers, and undo history remain stable.

## Lifecycle

Opening a live transcript subscribes it to its profile runtime. The first
subscriber starts SSE or polling fallback. Events schedule keyed snapshot reads;
periodic low-frequency reconciliation covers lost events. The final unsubscribe,
buffer cleanup, package reload, and server restart cancel pending idle jobs,
timers, requests where possible, and stream processes.

## Review checklist

- Does any interactive/timer callback wait for I/O?
- Can repeated events or responses collapse into one latest-state operation?
- Can a stale generation or killed buffer still be mutated?
- Does a hidden buffer perform text or marker work?
- Does another transcript create another server-level stream or cadence timer?
- Is snapshot reconciliation still authoritative after disconnect/reconnect?
- Are payloads, directories, auth values, and response bodies absent from logs?
- Do source and compiled ERT pass via `make verify`?
