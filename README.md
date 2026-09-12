# opencode-shell

Unofficial Emacs client for sessions owned by an OpenCode 1.18.30 HTTP server.
It is an independent early beta workflow, not an ACP bridge.

## Development install

Add this checkout to `load-path`, then `(require 'opencode-shell)`. For the
original single-server setup, configure `opencode-shell-base-url` and
optionally `opencode-shell-directory`, then run
`M-x opencode-shell-sessions`. Existing calls such as
`(opencode-shell-sessions "/work/project")` remain supported.

## Profiles

`opencode-shell-profiles` is a list of named plists. A profile can select its
`:base-url`, client `:directory`, server `:workspace`, authentication source,
and local server lifecycle settings. Use `M-x opencode-shell-launch` to pick
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
directory, health path, and bounded startup timeout. Launch reuses an already
healthy server and coalesces concurrent requests into one start attempt per
profile. `M-x opencode-shell-start-server`,
`M-x opencode-shell-stop-server`, and `M-x opencode-shell-restart-server`
provide explicit control. Stop only terminates a live process started and owned
by this Emacs client. Killing a browser or transcript does not stop that
process. Owned processes are stopped when Emacs exits. Profiles using a TRAMP
directory or a non-loopback server URL are remote and are never auto-started;
`:remote` can also force remote classification.

## Commands

The session browser uses `g` refresh, `/` filter, `c` create, `RET` open, and `d` confirmed delete. Transcript buffers use `g` resync, `p` prompt, `a` abort, `m` model, `A` agent, `P` permission, `Q` question, and `?` help. These maps work in vanilla Emacs and receive mode-local Evil normal-state bindings when Evil is available.

Prompts use minibuffer history. Transcript Markdown remains exact raw text with safe fontification for common headings, lists, quotes, links, inline code, and fenced blocks. Code and HTML are never evaluated.

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

This beta polls and fully resynchronizes instead of streaming SSE. It does not render tables specially, stream partial assistant text, offer a multiline composer, paginate large histories, or expose file/diff review. Permission and question handling is deliberately explicit and never auto-approves. The API contract targets legacy OpenCode 1.18.30 and may require changes for newer releases.
