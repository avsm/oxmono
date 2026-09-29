# Maildir for OCaml and Eio

The `maildir` library stores messages in the Dovecot Maildir layout. Its
top module is `Maildir`, with the public submodules `Maildir.Dotlock` for
scoped exclusive dotlocks and `Maildir.Keywords` for the `dovecot-keywords`
mapping and filename flag letters.

System flags live in filename letters and custom keywords in the lowercase
letters that `dovecot-keywords` maps. A message's modification time is its
arrival date. Flag changes preserve the basename, timestamps, the Passed flag
and extra filename fields. Scans, keyword checks, keyword-map updates and
mutations take the Dovecot metadata lock `dovecot-uidlist.lock`, and an
existing lock raises `Maildir.Metadata_lock_busy` at once. A nonempty
`.imap-flags` or `.imap-dates` directory is refused with `Legacy_metadata`.
It needs offline migration and is never removed.

Mutations take a `Maildir.writer`, the capability that `Maildir.with_writer`
grants under an exclusive application lease on `.imap-writer.lock`. A
second lease raises `Maildir.Writer_lock_busy`, and a writer used after its
callback returns raises `Maildir.Writer_expired`. Dovecot and other external
programs do not take the lease. Format and policy failures are
`Maildir.error` results. I/O failures raise `Eio.Io`, and lock contention
and stale observations raise the exceptions the interface documents.

The odoc page [`doc/index.mld`](doc/index.mld) states the format limits,
and its program is the compiled example
[`test/examples/writer.ml`](test/examples/writer.ml).

Tests live under `test/` and run with

    dune build @bleeding/maildir/runtest
