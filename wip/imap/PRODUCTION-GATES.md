# IMAP production gates

The [README](README.md) and public interfaces describe implemented behavior.
[ACCEPTANCE.md](ACCEPTANCE.md) records release requirements. This checklist
consolidates the earlier review journals. It is not a new live-test report or
a production-readiness claim.

## Implemented foundations

The package has protocol, Eio, store and sync libraries. Maildir is a separate
sibling package. Scoped mailbox handles, scoped Maildir writers, SQLite staging
and journals, blob storage, standard Maildir metadata, extension witnesses and
cross-protocol oracle tests are present. Do not reimplement the older plans for
these features. Review current interfaces and tests instead.

## Remaining acceptance work

- [ ] Map every acceptance requirement to a named current test, identifying
  untested schedules and unsupported behavior. Check installed interfaces and
  dependent applications after public API changes.
- [ ] Enumerate file fsync, rename, directory-fsync and transaction boundaries
  for imports, APPEND, flags, deletions and publication. Inject process crashes
  at each boundary and assert old-or-new recovery.
- [ ] Cover command-send, server-mutation, receipt, journal and pair-commit
  boundaries. Keep ambiguous outcomes held without blind replay. Preserve
  duplicate occurrences and stale-writer/CAS protection.
- [ ] Extend randomized restart schedules, concurrent initialization and schema
  corruption checks. Review blob collection against concurrent reference
  creation, not just bounded enumeration.
- [ ] Establish the supported concurrency contract with Dovecot and external
  Maildir writers. Test keyword updates, renames, replacement, deletion and
  INTERNALDATE preservation under concurrent changes.
- [ ] Resolve unattended stale-dotlock recovery without stat-then-unlink races.
  Existing locks are refused. The application writer lease does not bind
  external programs that ignore it.
- [ ] Document repair or migration for rejected nonempty legacy metadata
  directories. Preserve unknown keyword evidence rather than treating an
  unreadable message as absent.
- [ ] Complete the resource measurements in ACCEPTANCE.md, including current
  large-mailbox runs, blob collection and non-APPEND/FETCH transfer paths.
- [ ] Exercise watcher overflow, backpressure, reconnect and checkpoint
  boundaries. Audit deadlines, retries, pool shutdown and cancellation.
- [ ] Build a capability/enablement/fallback/error matrix for rev1/rev2,
  QRESYNC/CONDSTORE, UIDPLUS/MOVE, SORT/THREAD/ESORT, SEARCHRES/PARTIAL, BINARY,
  COMPRESS, MULTIAPPEND, LITERAL-/LITERAL+, NOTIFY, UIDBATCHES, UIDONLY,
  MESSAGELIMIT/SAVELIMIT and OBJECTID/OBJECTID+. Keep draft modes explicit.
- [ ] Extend Cyrus IMAP/JMAP round trips to the full MIME/duplicate corpus,
  Deleted-message counts and server-specific unrepresentable flags. Existing
  body and keyword tests do not close the complete fidelity matrix.
- [ ] Rerun the independent Stalwart baseline and OBJECTID+ fixtures after
  protocol changes, and Dovecot TLS/authentication/shared-Maildir/crash tests
  after ownership changes. Record fixture versions, skips and actual reruns.
- [ ] Build API documentation and verify examples and recovery commands before
  claiming release readiness.

## Ownership review

The store's [Database implementation](lib/store/database.ml) asserts contended
mode crossing around a mutex-protected connection. Recheck every access and
switch-release path when changing it. The
[store mode test](test/store/test_modes.ml) is a guard, not a substitute for
that ownership audit. Maildir's [writer contract](../maildir/lib/maildir.mli)
and [dotlock contract](../maildir/lib/dotlock.mli) state the external-writer
limitations.

## Verification entry points

From the repository root:

```sh
dune build --profile release-check @wip/imap/all @wip/maildir/all
dune build --force @wip/imap/runtest @wip/maildir/runtest
```

Live tests require explicit fixtures. See the
[Cyrus oracle](test/oracle/README.md), [Dovecot](test/dovecot/README.md),
[Stalwart baseline](test/stalwart/README.md),
[OBJECTID+ fixture](test/stalwart_v16/README.md) and
[scale suite](test/scale/README.md). Verify the [standards corpus](spec/README.md)
offline. A skipped live test does not satisfy an interoperability gate.

The checked-in [body measurements](test/dovecot/body-memory-normal-results.csv)
are historical sampled RSS/reserved-heap observations. They do not establish
peak live-payload memory or prove the 8 MiB buffering target. Keep future
measurement methods and claims equally explicit.

SCRAM, SASL security layers, durable MULTIAPPEND batching and complete proxy
frontends remain separate or deferred scope, as described in ACCEPTANCE.md.
