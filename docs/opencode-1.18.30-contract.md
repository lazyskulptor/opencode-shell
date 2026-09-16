# OpenCode 1.18.30 API Contract

`opencode-shell` targets these OpenCode HTTP endpoints:

| Method | Endpoint | Purpose |
| --- | --- | --- |
| `GET` | `/health` | Check server readiness; profiles may override the path |
| `GET` | `/session` | List sessions for exact `directory`; `limit` overrides the default 100-row truncation |
| `POST` | `/session` | Create a session |
| `POST` | `/session/:id/fork` | Fork before optional `messageID`; omit it to copy all history |
| `GET` | `/session/status` | Read session status |
| `DELETE` | `/session/:id` | Delete a session |
| `GET` | `/session/:id/message` | Read transcript messages |
| `POST` | `/session/:id/prompt_async` | Submit a prompt |
| `POST` | `/session/:id/abort` | Abort generation |
| `GET` | `/provider` | List providers and models |
| `GET` | `/agent` | List agents |
| `GET` | `/permission` | List pending permissions |
| `POST` | `/permission/:id/reply` | Answer a permission request |
| `GET` | `/question` | List pending questions |
| `POST` | `/question/:id/reply` | Answer a question |
| `POST` | `/question/:id/reject` | Reject a question |

Permission list snapshots may overlap reply callbacks. The client therefore keys
pending and resolved state by permission `id`, treats duplicate snapshots and
callbacks idempotently, and does not declare an active response ready while a
permission is pending or its reply is in flight. The list is the server's current
pending snapshot. The UI presents only its first current-session request,
replaces that card with the explicit reply result, and then advances to the next
request. `once` resolves one request; server-side `always` may resolve other
matching same-session requests, while `reject` may remove all remaining requests
in that session. Later snapshots remove those siblings from the local queue
without synthetic result lines. Emacs sends `always` directly and leaves
persistent matching to OpenCode. Every reply outcome, success or failure, immediately refetches `/permission`
rather than waiting for the next poll tick, so implicitly settled siblings
correct on screen sooner; if a `/permission` request is already in flight, the
refetch is deferred until that request settles, whether it succeeds or fails.

Question list responses are likewise authoritative current pending snapshots.
Pending questions and question replies in flight block prompt readiness just like
permissions; timer polling includes `/question` so user input requests cannot be
hidden behind a perpetually running question tool.
The shared inline interaction area gives permissions priority and otherwise shows
one current-session question. Reply/reject success triggers an immediate polling
resync so the associated tool and assistant history can leave their running state.

Session list requests include an absolute server-native `directory` and a high
`limit`; the observed OpenAPI also exposes `workspace`, `scope`, `path`, `roots`,
`start`, and `search`. Other requests may include `directory` as appropriate.
The normalized session collection also supplies generated titles for transcript
headers and the canonical newest-first session selector.

Fork requests inherit the current directory query unless an explicit destination
is supplied. A `messageID` boundary is exclusive: the named message and every
later message are omitted. An empty request body copies the complete history.
The client's directory-change function issues no API request; it only changes the
current transcript buffer's directory query scope for subsequent requests and its
Emacs `default-directory`. OpenCode 1.18.30 exposes no persisted session-directory
update field.
JSON request and response shapes follow OpenCode 1.18.30. This contract contains no Athena or
Aider concepts.

The client deliberately uses `/session/:id/message` polling as its only
transcript data path. It does not require `/event`; stable message identities and
monotonic history reconciliation provide recovery after request interruption.

A session may be absent from `/session/status` while its history still contains an
assistant message with a running tool and no `step-finish`. This is not completion:
the client keeps the turn active and exposes the authoritative reasoning/tool phase.

For an assistant envelope attached to the active user message, `info.finish`
(excluding the non-terminal `"tool-calls"` value, which means another step of
the same turn is coming) or `info.error`, plus `info.time.completed`, is the
primary message-level completion evidence. `step-finish` is compatible
fallback evidence only when `info.finish` is entirely absent. Neither a
completed/error tool part nor `/session/status` idle completes the turn by
itself, and any running or pending tool part blocks readiness regardless of
`info.error`. `Prompt>` is restored and polling stops only after that message
evidence is present and pending permission work is settled.

A newly submitted prompt is also a deterministic generation boundary: the client
settles any older nonterminal turn as interrupted before creating the new active
turn. History reconciliation performs the same repair when persisted history
already contains a later user turn after an orphaned one. A successful explicit
abort locally settles the active turn before resync, and stale incomplete history
cannot reopen either local settlement; later authoritative terminal metadata may
replace the fallback reason. This orphan repair does not make
`/session/status` idle sufficient completion evidence for the newest active turn.
These invariants are exercised by the metadata-only
`test/fixtures/completion-polling-regression.el` sequence; it intentionally
contains no conversation, reasoning, tool payload, path, or credential data.
