# Contributing to opencode-shell

Thanks for improving the Emacs client.

## Before you start

- Search existing issues and pull requests before opening a new one.
- Keep each change focused. Avoid unrelated refactors or formatting churn.
- Do not include credentials, server URLs, session content, tool input/output, or
  other private OpenCode data in commits, fixtures, issues, or screenshots.

## Development setup

1. Install Emacs 27.1 or newer.
2. Clone the repository.
3. Load the checkout in Emacs with `(add-to-list 'load-path "/path/to/opencode-shell")`
   and `(require 'opencode-shell)`.
4. Run `make verify` before submitting a pull request. It runs source tests,
   byte-compilation, and compiled tests.

Choose the owner before editing: pure lifecycle and hydration belong in
`opencode-shell-state.el`, interaction identity in `opencode-shell-interaction.el`,
assistant-part semantics in `opencode-shell-response.el`, and commands/network/
markers/presentation in `opencode-shell.el`. The detailed map and change recipes
are in [`docs/conversation-pipeline.md`](docs/conversation-pipeline.md).

## Change guidelines

- Preserve the asynchronous runtime contracts in
  [`docs/async-runtime.md`](docs/async-runtime.md): no synchronous network I/O
  on interactive paths, no callback-time UI rendering, and no raw event logging.
- Add deterministic ERT coverage for behavior changes. Include regression tests
  for ordering, editor-state preservation, or hidden-buffer behavior when relevant.
- Follow `RED → GREEN → REFACTOR → VERIFY`; see
  [`docs/testing.md`](docs/testing.md) for focused commands and suite ownership.
- Keep user-facing documentation current when commands, configuration, lifecycle,
  protocol, or privacy behavior changes.
- Follow the surrounding Emacs Lisp style and keep public symbols documented.

## Pull requests

Use a concise title that describes the outcome. In the description, explain:

1. What changed and why.
2. How you tested it, including the exact command when possible.
3. Any compatibility, privacy, or migration consideration.

Small, reviewable pull requests are preferred. By contributing, you agree that
your contribution is licensed under the repository's [MIT License](LICENSE).
