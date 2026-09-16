# opencode-shell

Unofficial Emacs client for sessions owned by an OpenCode 1.18.30 HTTP server.
It is an independent early beta workflow, not an ACP bridge.

## Development install

Add this checkout to `load-path`, then `(require 'opencode-shell)`. For the
original single-server setup, the local endpoint defaults to
`http://127.0.0.1:4199`; configure `opencode-shell-base-url` to override it and
optionally configure `opencode-shell-directory`, then run `M-x opencode-shell`
to select a server and open sessions for the current project root. Root detection
prefers active Projectile and `project.el` projects, then a Git root, and falls
back to the current Emacs directory.

## Profiles

`opencode-shell-profiles` is a list of named plists. A profile can select its
`:base-url`, client `:directory`, server `:workspace`,
authentication source, and local server lifecycle settings. Session inventory
is scoped to the invocation buffer's `default-directory`, translated to the
server-native absolute path.
`M-x opencode-shell` always selects a server first. Browser and new-session
commands map the detected client project root to the server workspace, so calls
from nested directories share one project session history. Each alias also generates
`opencode-shell-<alias>-sessions` and `opencode-shell-<alias>-start`, such as
`opencode-shell-local-sessions` and `opencode-shell-local-start`. The start
command creates a title-less session in the current directory and opens it.

Profile `:name` values and identity keys must be unique; set an explicit `:id`
when an identity must survive a name or URL change. A string `:match` is a
canonical directory root, while regex matching is opt-in via `:match-regexp`.
Root matching chooses the most specific match. Remote profiles match TRAMP directories by
their remote identity and translate between the native local/remote client path
and `:workspace` path visible to the server, preserving relative descendants.
Local profiles use the same root-to-workspace translation. This keeps request directory/workspace
scoping consistent without sending TRAMP syntax to the server. Buffers are
profile-qualified, so identical session IDs on different servers never share a
browser or transcript buffer.

Local profiles may provide an argv-style start command, server working
directory, health path, and bounded startup timeout. Lifecycle is shared by
canonical base URL: profiles using one endpoint reuse one health probe, start
attempt, owned process, stop/restart state, and exit cleanup while retaining
their own directory, authentication, sessions, and capabilities. Profiles for
one endpoint must specify compatible start command, health path, server
directory, timeout, health authentication header, and `:stop-on-exit` setting;
configuration conflicts fail before startup. `<alias>-start`,
`M-x opencode-shell-status`, and `M-x opencode-shell-restart` provide explicit
control. Internal cleanup only terminates a live process started and owned
by this Emacs client. Killing a browser or transcript does not stop that
process. Owned processes are stopped once when Emacs exits unless the shared
profiles consistently set `:stop-on-exit` to nil. Profiles using a TRAMP
directory or a non-loopback server URL are remote and are never auto-started;
`:remote` can also force remote classification.

Canonical lifecycle URLs treat loopback host spellings, a trailing slash, and
explicit default HTTP/HTTPS ports as the same endpoint. A healthy server not
started by this client is tracked as non-owned for shared configuration
validation and is never stopped. A failed probe cannot replace a still-live
owned process; use the explicit restart command to replace it.

## Commands

Each session browser is fixed to one server-native directory and encoded in its
buffer identity. Use `/` for a text filter, `g` to
refresh, `c` to create in that fixed directory, `RET` to open, and `d` for
confirmed delete. Child sessions created by subagents are hidden by default;
use `T` to show or hide them without deleting their server history. `RET` uses
the selected row's exact server-reported directory, which is
then immutable for transcript history, prompts, aborts, permissions, questions,
and status requests. A transcript buffer has a multiline composer after
`Prompt> ` at its bottom; `RET` inserts a newline, while `C-c C-c` or `s-RET`
submits it. Submitted prompts and polled responses above the composer are
read-only. Use `C-c C-v` to select the model and `C-c C-m` to select the agent;
both selections appear with the generated session title in the transcript header
and affect subsequent prompt payloads. The title updates on session open and an
explicit `g` resync when the session snapshot reports OpenCode's generated title.
Each prompt carries a stable message ID. Periodic history polling is the sole
transcript data path and reconciles responses without deleting known history.
Each recurring poll is limited to message history, session status, and pending
permissions; session metadata and model/agent capabilities are full-resync data.
The transcript shows sending, waiting, receiving, recovering, aborting, or error
state and never replaces an already completed response with stale data. Pending
status uses a right-growing progress animation that advances on the UI-only
`opencode-shell-animation-interval` (0.2 seconds by default). It restarts from
its first frame for each network poll and does not issue requests. Reasoning-only
updates show `Thinking`; text or tool activity shows `Receiving`.
Assistant completion comes from the matching message's `time.completed`
metadata together with either a genuinely terminal `finish` value or a
terminal `error`; `step-finish` is retained only as compatible evidence when
`finish` is absent. A `finish` of `tool-calls` means another step is coming
and is never treated as completion, and a completed tool or idle session
status does not complete the whole assistant turn; running tools keep the
response active. When a turn ends via a terminal error — auth failure, a
token/output-length or context limit, a content filter, a provider/API error,
or an abort — the transcript shows a bounded `[name: message]` reason line
after the response instead of silently rendering a blank one. Submitting a new
prompt automatically settles any older nonterminal turn as interrupted; history
reconciliation applies the same rule to persisted sessions where a later prompt
already proves an older turn is orphaned. A successful explicit abort likewise
settles the active turn before resync. These safeguards do not use session idle
alone as completion evidence.

Completed assistant pipe tables fit the selected transcript window by wrapping
long cells. Resizing recalculates conventional tables outside fenced code; all
other Markdown stays unchanged, and each turn retains the verbatim source.

API requests are correlated by a short ID in `*OpenCode Shell Log*` before a
session exists and in a session-specific log buffer afterwards. Open the relevant
buffer with `M-x opencode-shell-log`. Non-poll requests log their start; all
outcomes log method, path, status, and duration. Successful message-history polls
emit only their outcome to limit noise. Customize
`opencode-shell-log-requests` to disable logs. Bodies, query parameters,
authentication headers, and error response bodies are never logged.
Model completion is limited to providers reported as connected by the server;
agent completion shows only server-advertised visible primary agents.
`g` resyncs, `a` aborts, and `P`/`Q` retain the explicit permission/question
flows. These maps work in vanilla Emacs and receive mode-local Evil normal-state
bindings when Evil is available.

`M-x opencode-shell-fork-session` is available in an idle transcript without a
dedicated keybinding. Its minibuffer lists user prompts in chronological order;
choosing one forks immediately before that prompt. The returned session opens in
its reported directory.

`M-x opencode-shell-move-session-directory` is a separate unbound function for
a transcript. It reads a destination, normalizes it to that project's root, and
updates the same buffer's server request scope and Emacs `default-directory`.
The client persists this directory override, so the session moves from the old
project browser to the destination browser even when the server retains its
original directory metadata.
The session ID, visible history, composer, and polling state stay in that buffer.
This is a client-side scope change; it does not mutate persisted server metadata.

Use `?` in either the browser or transcript for its context-specific Transient
menu. Global `C-c o l` opens the browser, `C-c o s` starts a session, `C-c o b`
selects only live transcript buffers, and `C-c o f` selects a canonical
newest-first server session. The session selector marks live transcripts as
active and recent inactive sessions with distinct faces, then opens or reuses the
selection like `find-file`.
In transcript Evil normal state, `?` opens this help and `RET` submits the
composer. Insert state keeps `?` as text input and `RET` as a newline.

Pending permissions appear one at a time in a boxed read-only block above `Prompt>`. Use
`C-c C-p` to jump there. In either Evil insert or normal state, use `C-c C-y`
to allow once, `C-c C-l` to always allow, or `C-c C-n` to
reject. The request context remains visible and the composer draft is preserved.
Permission IDs are authoritative: repeated server snapshots and reply callbacks
do not duplicate a box or resolved result. Distinct pending IDs remain separately
queued. A reply replaces the current card with its result and advances to the next
pending card. Persistent `always` matching is delegated to OpenCode without an
additional Emacs confirmation. During an active response, `Prompt>` stays hidden and polling continues
until both the assistant response and every permission request are settled.
Pending questions use the same single inline interaction area after permissions.
The card shows `Waiting for answer`; use `RET` or `a` to answer and `r` to reject.
Questions block prompt readiness until their authoritative snapshot clears, and
each action immediately resyncs question and tool state.

The session buffer uses normal text editing rather than `special-mode`. Only the
bottom composer is writable; each submitted user prompt and its response are
separate read-only regions owned by a buffer-local turn record. Poll updates
replace turn regions without changing composer text or point. Evil's ordinary
insert commands are left intact and entering insert state focuses the composer.
Transcript, status, and interaction-card updates do not enter undo history.
Submitting starts a fresh composer undo history, so undo cannot restore a sent
draft; edits to the current draft remain normally undoable.
An ambiguous `prompt_async` failure keeps the attempted turn visible and polls
history for its stable ID instead of automatically submitting it again.
Observed tool parts add payload-free `TOOL> name` lines (for example `bash`,
`edit`, or `write`) without exposing tool inputs or outputs.

For polling diagnostics, leave `opencode-shell-log-requests` enabled and run
`M-x opencode-shell-log` from the transcript buffer. Lifecycle lines are emitted
when state changes and name the local blockers, completion metadata, tool-state
counts, pending permission count, and submit reconciliation state. Routine
unchanged polls are coalesced. Logs intentionally exclude message/reasoning text,
tool input/output, permission descriptions and patterns, request bodies, query
parameters, directories, authorization values, and server error bodies.
The network cadence remains controlled independently by
`opencode-shell-poll-interval` (2 seconds by default); spinner ticks are not logged
as polls because they perform no network work.

Async runtime lines report only bounded control state such as
`transport=sse-connected`, `transport=fallback`, reconnect delay, poll wakeups,
and deferred hidden rendering. They never include SSE payloads, transcript text,
request bodies, directories, or authorization values. A healthy local runtime
normally shows one SSE connection regardless of the number of transcript buffers;
fallback lines followed by reconnect lines indicate automatic recovery.

## Asynchronous runtime principles

Interactive commands never wait synchronously for network or process I/O.
Transport callbacks reconcile authoritative server snapshots but do not directly
rewrite transcript or browser buffers. UI work is coalesced and applied at idle
time, and hidden buffers retain dirty state without spending time rendering it.
OpenCode's `/event` SSE endpoint is the preferred wake-up path, shared per server;
low-frequency snapshot polling remains the recovery and compatibility path.

These rules apply to all new runtime work. See
[`docs/async-runtime.md`](docs/async-runtime.md) for the architecture, fallback,
cancellation, cleanup, and review checklist.

## Security

Use TLS and server/network access controls for remote servers. The legacy
`opencode-shell-auth-function` remains supported. Profiles can instead name an
auth-source lookup, resolved on demand through the injectable auth-source
resolver. Values are used verbatim (no `Bearer` prefix is added); set
`:auth-header` to use a custom header name instead of `Authorization`. The same
header is sent on JSON API calls and health probes. Returned values are scoped to the request: the client
does not retain them in profiles, buffers, lifecycle state, or logs, and never
prints request headers. Do not place tokens or private server addresses in
public configuration.

## Limitations

This beta polls instead of streaming SSE. It intentionally defers general rich Markdown/tool rendering, folding, retention pruning, partial assistant streaming, pagination, and file/diff review. Permission and question handling is deliberately explicit and never auto-approves. The API contract targets legacy OpenCode 1.18.30 and may require changes for newer releases.

## Acceptance check

Run `make verify` before loading a changed checkout. It deletes stale bytecode,
runs source ERT, compiles the package and tests, then runs compiled ERT.

For a live check, restart Emacs, close old OpenCode transcript buffers, open or
create a localhost session, enter insert state with several of `i`, `a`, `A`,
`o`, and `O`, then submit two multiline Korean/Markdown prompts. Confirm each
submitted prompt and response is read-only, the bottom composer remains
writable, and polling does not move or erase a draft being edited.
