# OpenCode 1.18.30 API Contract

`opencode-shell` targets these OpenCode HTTP endpoints:

| Method | Endpoint | Purpose |
| --- | --- | --- |
| `GET` | `/health` | Check server readiness; profiles may override the path |
| `GET` | `/session` | List sessions for exact `directory`; `limit` overrides the default 100-row truncation |
| `POST` | `/session` | Create a session |
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
JSON request and response shapes follow OpenCode 1.18.30. This contract contains no Athena or
Aider concepts.

The client deliberately uses `/session/:id/message` polling as its only
transcript data path. It does not require `/event`; stable message identities and
monotonic history reconciliation provide recovery after request interruption.

A session may be absent from `/session/status` while its history still contains an
assistant message with a running tool and no `step-finish`. This is not completion:
the client keeps the turn active and exposes the authoritative reasoning/tool phase.

For an assistant envelope attached to the active user message, `info.finish` plus
`info.time.completed` is the primary message-level completion evidence.
`step-finish` remains compatible explicit evidence. Neither a completed/error tool
part nor `/session/status` idle completes the turn by itself, and any running or
pending tool part blocks readiness. `Prompt>` is restored and polling stops only
after that message evidence is present and pending permission work is settled.
These invariants are exercised by the metadata-only
`test/fixtures/completion-polling-regression.el` sequence; it intentionally
contains no conversation, reasoning, tool payload, path, or credential data.
