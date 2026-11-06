# Changes

- Report a missing master key by name from the native backend too, as
  the Eio backend does.
- Clarify opam descriptions of package functionality and interfaces.

- Add a repeatable service migration script with previews, verified imports,
  private backups and optional systemd/launchd restarts.

- Add encrypted local secret storage, private-file references and an Eio API
  with explicit lifetime and refresh semantics.
