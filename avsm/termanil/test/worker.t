The integrated binary resolves its worker without reading a user profile.

  $ ../bin/termanil_cli.exe --check-worker
  termanil/v4

Use a temporary local configuration and no network settings.

  $ cat > config.toml <<'EOF'
  > [dooit]
  > root = "tasks"
  > EOF
  $ printf '(termanil/v4 Tasks)\n' | ../bin/termanil_worker.exe --config config.toml
  (termanil/v4(ok(Tasks_loaded()("Store not initialized. Capture mail or run dooit init."))))
  $ test ! -e tasks

The worker validates its protocol before loading credentials.

  $ printf '(termanil/v9 Tasks)\n' | ../bin/termanil_worker.exe --config missing.toml
  (termanil/v4(error"Incompatible termanil worker protocol"))

Unknown configuration keys are errors.

  $ printf '[mail]\nprofiel="personal"\n' > invalid.toml
  $ printf '(termanil/v4 Mailboxes)\n' | ../bin/termanil_worker.exe --config invalid.toml
  (termanil/v4(error"unknown config key: profiel"))

Demo mode ignores live configuration and persists flags across worker processes.

  $ printf '(termanil/v4 Mailboxes)\n' | ../bin/termanil_worker.exe --demo-dir demo --config missing.toml
  (termanil/v4(ok(Mailboxes_loaded(((id inbox)(name Inbox)(unread 3)(inbox true))((id archive)(name Archive)(unread 0)(inbox false))((id drafts)(name Drafts)(unread 0)(inbox false))((id sent)(name Sent)(unread 0)(inbox false))))))
  $ printf '(termanil/v4(Set_seen((service https://mail.example.test/jmap/session)(account personal)(id email-1))true))\n' | ../bin/termanil_worker.exe --demo-dir demo
  (termanil/v4(ok(Message_changed((service https://mail.example.test/jmap/session)(account personal)(id email-1))Seen true)))
  $ printf '(termanil/v4 Mailboxes)\n' | ../bin/termanil_worker.exe --demo-dir demo
  (termanil/v4(ok(Mailboxes_loaded(((id inbox)(name Inbox)(unread 2)(inbox true))((id archive)(name Archive)(unread 0)(inbox false))((id drafts)(name Drafts)(unread 0)(inbox false))((id sent)(name Sent)(unread 0)(inbox false))))))
  $ test -s demo/server.json
