# Changelog

## 0.1.0 - 2026-09-11

- Add compact session browser, transcript, prompting, abort, model/agent selection, polling, pending permission/question actions, safe Markdown presentation, and ERT coverage.
- Add named local and remote profiles with stable selection, directory matching,
  profile-scoped request settings, native/TRAMP-to-server path mapping, and
  isolated browser/transcript buffers for same-named session IDs.
- Add on-demand auth-source resolution without retaining or logging secrets.
- Add health-aware, bounded local server start plus explicit start, owned-only
  stop, and restart commands; remote profiles never auto-start and killing a
  transcript does not stop an owned server.
- Share local server lifecycle by canonical endpoint, coalesce health/start
  work across project profiles, preserve callback profile isolation, and reject
  conflicting lifecycle configuration for a shared endpoint.
- Preserve the single-server variables and the historical directory argument to
  `opencode-shell-sessions` while allowing that command to accept a profile.
- Group browser and new-session launches at the Projectile, project.el, or Git
  project root, with current-directory fallback and existing profile mapping.
- Add an unbound interactive session-fork function with minibuffer selection of
  an exclusive prompt boundary or complete-history copy.
