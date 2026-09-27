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

Mutations take a `Maildir.writer`, the capability that `Maildir.with_writer`
grants under an exclusive application lease, and a writer used after its
callback returns raises `Maildir.Writer_expired`. Format and policy failures
are `Maildir.error` results. I/O failures raise `Eio.Io`, and lock contention
and stale observations raise the exceptions the interface documents.

The odoc page [`doc/index.mld`](doc/index.mld) states the format limits,
and its program is the compiled example
[`test/examples/writer.ml`](test/examples/writer.ml).

Tests live under `test/` and run with

    dune build @bleeding/maildir/runtest
