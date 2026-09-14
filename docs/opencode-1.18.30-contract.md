# OpenCode 1.18.30 API Contract

`opencode-shell` targets these OpenCode HTTP endpoints:

| Method | Endpoint | Purpose |
| --- | --- | --- |
| `GET` | `/health` | Check server readiness; profiles may override the path |
| `GET` | `/session` | List all sessions in the supplied directory scope; 1.18.30 exposes no session pagination parameters |
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

Requests may include the OpenCode `directory` query parameter. JSON request and
response shapes follow OpenCode 1.18.30. This contract contains no Athena or
Aider concepts.
