# Changelog

## 0.1.0 - 2026-09-11

- Add compact session browser, transcript, prompting, abort, model/agent selection, polling, pending permission/question actions, safe Markdown presentation, and ERT coverage.
- Add named local and remote profiles with stable selection, directory matching,
  profile-scoped request settings, native/TRAMP-to-server path mapping, and
  isolated browser/transcript buffers for same-named session IDs.
- Add on-demand auth-source resolution without retaining or logging secrets.
- Add health-aware, bounded local server start plus explicit start, owned-only
  stop, and restart commands; remote profiles never auto-start and killing a
  transcript does not stop an owned server.
- Share local server lifecycle by canonical endpoint, coalesce health/start
  work across project profiles, preserve callback profile isolation, and reject
  conflicting lifecycle configuration for a shared endpoint.
- Preserve the single-server variables and the historical directory argument to
  `opencode-shell-sessions` while allowing that command to accept a profile.
- Group browser and new-session launches at the Projectile, project.el, or Git
  project root, with current-directory fallback and existing profile mapping.
- Add an unbound interactive session-fork function with minibuffer selection of
  an exclusive prompt boundary.
- Add a separate unbound directory-scope function that updates the current
  transcript buffer in place without reopening or migrating the server session.
- Add a server-shared SSE wake-up runtime with nonblocking polling fallback,
  idle-coalesced state delivery, visible-only transcript/browser rendering, and
  deterministic cleanup of queued work, streams, and timers.
- Isolate HTTP/chunk/SSE parsing and connection races in `opencode-shell-sse.el`,
  with split-boundary tests, typed failures, bounded reconnect/circuit fallback,
  strict interval validation, and stale-connection suppression.
- Document asynchronous runtime invariants that prohibit synchronous I/O and
  callback-driven buffer rendering on interactive hot paths.
- Keep polling and animation presentation-only for unchanged snapshots: use a
  bounded right-growing overlay spinner, reset it on current-session transcript
  progress, and preserve transcript text, markers, cursor, viewport, draft, and
  undo state.
- Coalesce adjacent SSE and reconciliation wakes, suppress successful routine
  snapshot request logs, and retain errors plus semantic lifecycle diagnostics.
- Apply validated message and part SSE deltas to their target session without a
  full-history request, while retaining authoritative snapshots for initial,
  reconnect, integrity, unsupported-event, and polling-fallback recovery.
- Reconcile fallback snapshots linearly and render only changed response blocks,
  keeping overlay spinner redisplay and unrelated Emacs buffers responsive
  for long resumed sessions.
- Recover live transcript caches before timer-delivered hash operations, discard
  stale work after mode transitions, coalesce update/removal events by entity,
  scope unsupported-event reconciliation by resource, and cascade user-message
  removals through locally indexed assistant children.
