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
| `GET` | `/event` | Directory-scoped SSE bus events; first frame is `server.connected` |
| `GET` | `/provider` | List providers and models |
| `GET` | `/agent` | List agents |
| `GET` | `/permission` | List pending permissions |
| `POST` | `/permission/:id/reply` | Answer a permission request |
| `GET` | `/question` | List pending questions |
| `POST` | `/question/:id/reply` | Answer a question |
| `POST` | `/question/:id/reject` | Reject a question |

Session list requests include an absolute server-native `directory` and a high
`limit`; the observed OpenAPI also exposes `workspace`, `scope`, `path`, `roots`,
`start`, and `search`. Other requests may include `directory` as appropriate.
JSON request and response shapes follow OpenCode 1.18.30. This contract contains no Athena or
Aider concepts.

Live verification against 1.18.30 confirmed `text/event-stream` frames formatted
as `data: <JSON>` followed by a blank line. The initial envelope contains stable
`id`, type `server.connected`, and `properties`. The client treats events only as
change notifications and always reconciles authoritative session message history,
so a dropped, duplicated, or split SSE frame cannot silently drop transcript data.
