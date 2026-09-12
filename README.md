# opencode-shell

Unofficial Emacs client for sessions owned by an OpenCode 1.18.30 HTTP server.
It is an independent early beta workflow, not an ACP bridge.

## Development install

Add this checkout to `load-path`, then `(require 'opencode-shell)`. For the
original single-server setup, the local endpoint defaults to
`http://127.0.0.1:4199`; configure `opencode-shell-base-url` to override it and
optionally configure `opencode-shell-directory`, then run
`M-x opencode-shell-sessions`. Existing calls such as
`(opencode-shell-sessions "/work/project")` remain supported.

## Profiles

`opencode-shell-profiles` is a list of named plists. A profile can select its
`:base-url`, client `:directory`, server `:workspace`, session-list root,
authentication source, and local server lifecycle settings. Local profiles
default `:session-list-directory` to the local user home. Remote profiles must
set it to an absolute server-native home/root path, never TRAMP syntax. Use
`M-x opencode-shell-launch` to pick
the profile matching the current directory; use a prefix argument to choose a
profile explicitly. `M-x opencode-shell-open-profile` always prompts. The
session browser also accepts a profile directly, while retaining the legacy
directory argument described above.

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
configuration conflicts fail before startup. `M-x opencode-shell-start-server`,
`M-x opencode-shell-stop-server`, and `M-x opencode-shell-restart-server`
provide explicit control. Stop only terminates a live process started and owned
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

The session browser queries the active profile's `:session-list-directory`, so
it shows that server's sessions across project directories regardless of the
current buffer. Its `p` directory filter and `/` text filter are view-only;
`A` clears both. Use `g` refresh, `c` create, `RET` open, and `d` confirmed
delete. `RET` uses the selected row's exact server-reported directory, which is
then immutable for transcript history, prompts, aborts, permissions, questions,
and status requests. A transcript buffer has a multiline composer after
`Prompt> ` at its bottom; `RET` inserts a newline, while `C-c C-c` or `s-RET`
submits it. Submitted prompts and polled responses above the composer are
read-only. Use `C-c C-v` to select the model and `C-c C-m` to select the agent;
both selections appear in the header and affect subsequent prompt payloads.
`g` resyncs, `a` aborts, and `P`/`Q` retain the explicit permission/question
flows. These maps work in vanilla Emacs and receive mode-local Evil normal-state
bindings when Evil is available.

The session buffer uses normal text editing rather than `special-mode`. Only the
bottom composer is writable; each submitted user prompt and its response are
separate read-only regions owned by a buffer-local turn record. Poll updates
replace turn regions without changing composer text or point. Evil's ordinary
insert commands are left intact and entering insert state focuses the composer.
A failed `prompt_async` request marks the attempted turn as failed and restores
the submitted draft for retry when no newer draft exists.

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
