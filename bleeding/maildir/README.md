# Maildir for OCaml and Eio

The `maildir` library stores messages in the Dovecot Maildir layout. Its
top module is `Maildir`, with the public submodules `Maildir.Dotlock` for
scoped exclusive dotlocks and `Maildir.Keywords` for the `dovecot-keywords`
mapping and filename flag letters.

System flags live in filename letters and custom keywords in the lowercase
letters that `dovecot-keywords` maps. A message's modification time is its
arrival date. Flag changes preserve the basename, timestamps, the Passed flag
and extra filename fields. Keyword-map updates and directory scans take
`dovecot-uidlist.lock`, and a busy lock fails immediately. Existing nonempty
`.imap-flags` or `.imap-dates` directories require offline migration and are
never ignored or removed.

Tests live under `test/` and run with

    dune build @bleeding/maildir/runtest
