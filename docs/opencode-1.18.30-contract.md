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

Permission list snapshots may overlap reply callbacks. The client therefore keys
pending and resolved state by permission `id`, treats duplicate snapshots and
callbacks idempotently, and does not declare an active response ready while a
permission is pending or its reply is in flight.
| `GET` | `/question` | List pending questions |
| `POST` | `/question/:id/reply` | Answer a question |
| `POST` | `/question/:id/reject` | Reject a question |

Session list requests include an absolute server-native `directory` and a high
`limit`; the observed OpenAPI also exposes `workspace`, `scope`, `path`, `roots`,
`start`, and `search`. Other requests may include `directory` as appropriate.
JSON request and response shapes follow OpenCode 1.18.30. This contract contains no Athena or
Aider concepts.

The client deliberately uses `/session/:id/message` polling as its only
transcript data path. It does not require `/event`; stable message identities and
monotonic history reconciliation provide recovery after request interruption.

A session may be absent from `/session/status` while its history still contains an
assistant message with a running tool and no `step-finish`. This is not completion:
the client keeps the turn active and exposes the authoritative reasoning/tool phase.
