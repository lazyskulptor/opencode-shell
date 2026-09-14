# opencode-shell

Unofficial Emacs client for sessions owned by an OpenCode 1.18.30 HTTP server.
It is an independent early beta workflow, not an ACP bridge.

## Development install

Add this checkout to `load-path`, then `(require 'opencode-shell)`. For the
original single-server setup, the local endpoint defaults to
`http://127.0.0.1:4199`; configure `opencode-shell-base-url` to override it and
optionally configure `opencode-shell-directory`, then run `M-x opencode-shell`
to select a server and open sessions for the current Emacs directory.

## Profiles

`opencode-shell-profiles` is a list of named plists. A profile can select its
`:base-url`, client `:directory`, server `:workspace`,
authentication source, and local server lifecycle settings. Session inventory
is scoped to the invocation buffer's `default-directory`, translated to the
server-native absolute path.
`M-x opencode-shell` always selects a server first. Each alias also generates
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

Each session browser is fixed to one server-native directory, shown in its
header and reflected in its buffer identity. Use `/` for a text filter, `g` to
refresh, `c` to create in that fixed directory, `RET` to open, and `d` for
confirmed delete. `RET` uses the selected row's exact server-reported directory, which is
then immutable for transcript history, prompts, aborts, permissions, questions,
and status requests. A transcript buffer has a multiline composer after
`Prompt> ` at its bottom; `RET` inserts a newline, while `C-c C-c` or `s-RET`
submits it. Submitted prompts and polled responses above the composer are
read-only. Use `C-c C-v` to select the model and `C-c C-m` to select the agent;
both selections appear in the header and affect subsequent prompt payloads.
Each prompt carries a stable message ID. Periodic history polling is the sole
transcript data path and reconciles responses without deleting known history.
The transcript shows stable sending, waiting, receiving, recovering, aborting,
or error state and never replaces an already completed response with stale data.
Pending status cycles `·`, `··`, `···` once per message-history poll. Reasoning-only
updates show `Thinking`; text or tool activity shows `Receiving`. The dots indicate
polling activity, not estimated progress.
Assistant completion comes from the matching message's `finish` and
`time.completed` metadata, with `step-finish` retained for compatible server
payloads. A completed tool or idle session status does not complete the whole
assistant turn; running tools keep the response active.

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

The session buffer uses normal text editing rather than `special-mode`. Only the
bottom composer is writable; each submitted user prompt and its response are
separate read-only regions owned by a buffer-local turn record. Poll updates
replace turn regions without changing composer text or point. Evil's ordinary
insert commands are left intact and entering insert state focuses the composer.
An ambiguous `prompt_async` failure keeps the attempted turn visible and polls
history for its stable ID instead of automatically submitting it again.

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

This beta polls instead of streaming SSE. It intentionally defers rich Markdown/tool rendering, folding, retention pruning, partial assistant streaming, pagination, and file/diff review. Permission and question handling is deliberately explicit and never auto-approves. The API contract targets legacy OpenCode 1.18.30 and may require changes for newer releases.

## Acceptance check

Run `make verify` before loading a changed checkout. It deletes stale bytecode,
runs source ERT, compiles the package and tests, then runs compiled ERT.

For a live check, restart Emacs, close old OpenCode transcript buffers, open or
create a localhost session, enter insert state with several of `i`, `a`, `A`,
`o`, and `O`, then submit two multiline Korean/Markdown prompts. Confirm each
submitted prompt and response is read-only, the bottom composer remains
writable, and polling does not move or erase a draft being edited.
