# IMAP remaining work

Updated 2026-09-27. This is the current review/handoff checklist, not a claim of
production readiness. Design requirements remain in [IMAP-SPEC.md](IMAP-SPEC.md)
§§14–15; historical evidence is in
[PRODUCTION-GATES.md](bleeding/imap/PRODUCTION-GATES.md). Start a code review with
[IMAP-REVIEW-CHECKPOINT.md](IMAP-REVIEW-CHECKPOINT.md).

## 0. Restructuring in progress (accepted 2026-09-27)

This section is the resumable state of the interface restructuring accepted
from the Phase 1 interface review. A resumed session starts here: read the
environment rules, find the first step whose status is not `done`, and
continue it. Every step ends with a clean build, a clean test run, one commit,
and this table updated with the commit hash. A subagent doing a step updates
its own row and the step notes below, and nothing else in this file.

### Environment

All dune commands run in the OxCaml switch, from the repository root:

    opam exec --switch=5.2.0+ox -- dune build @bleeding/imap/all
    opam exec --switch=5.2.0+ox -- dune build @bleeding/imap/runtest --force

Live-server tests skip without their environment variables. ocamlformat is not
usable in that switch, so match the surrounding formatting by hand and keep
lines within 80 columns. Never regenerate a golden file to make a test pass.
Work is on branch `minus39`. One commit per step, with a one-line imperative
message and no trailers. Interfaces follow the `doc-style` rules: `[f x] is`,
full sentences, no colons or em dashes joining clauses, defaults stated for
every optional argument, no history in the prose. Implementations carry no
comments unless the code cannot say it.

### Steps

| # | Step | Status | Commit |
|---|------|--------|--------|
| 0 | Baseline commit of the untracked IMAP tree and its shared-library edits | todo | |
| R | Phase 2 implementation review by subagent, one per module, findings in section 0.R | in progress | |
| F | Apply Phase 2 correctness fixes in severity order, then dead code, redundancy, comments | todo | |
| 1 | Plan item 8: strip duplicated docs from core Eio `.mli` and private store `.mli` to one-line internal contracts; rename `Imap_store.Sync` to `Journal` | todo | |
| 2 | Plan items 1 to 3: `Imap.Capability`, typed `Response.Capability`/`Enabled`, `Error.Unsupported`, typed `Client.capabilities`/`enabled`/`has`/`enable` | todo | |
| 3 | Plan item 12: `spool` as a private library shared by `imap.sync` and test/io; drop copy_files | todo | |
| 4 | Plan item 10: standalone `maildir` package at `bleeding/maildir/`; no `imap` or `sqlite3-eio` dependency; `Local_inventory` in sync; `with_writer` capability; typed errors; `Dotlock` public | todo | |
| 5 | Plan item 5a: dissolve `Proto` into `Imap.Uid`, `Uidvalidity`, `Modseq`, `Uid_set` with `equal`, `compare`, `pp`; unify identifier shapes across `Selected` | todo | |
| 6 | Plan item 5b: move vocabulary types out of `Command`; `Command.error` a real type; label mailbox arguments; `Mailbox_name.t` private; `Client.list` returns `Mailbox_name.t` | todo | |
| 7 | Plan item 5c: `Imap.Search` and `Imap.Fetch_item`; one `Selected.fetch` replacing the six fifty-UID fetchers | todo | |
| 8 | Plan item 6: one `Client.append` and `append_many`; typed flags on APPEND | todo | |
| 9 | Plan item 4: extension witness submodules on `Client` and `Selected`, each with `require` | todo | |
| 10 | Plan item 9: `with_mailbox` reentrancy returns `State` instead of blocking | todo | |
| 11 | Sync moves: `Ctx` record, single `Imap_sync.Error.t`, `Repair` module, `Plan` module, one APPEND inspection, drop `Engine.run_once` if unused | todo | |
| 12 | CLI on cmdliner with one term per command and a single `deletion_policy` option | todo | |
| 13 | `imap.mli` facade, `.mld` pages, `(documentation)` stanza, dune-project dependency fixes | todo | |
| 14 | Plan item 7: `Imap_eio.Mailbox` strategy layer | todo | |
| 15 | Redocumentation pass under doc-style over every public interface | todo | |

Decisions taken: extension witnesses rather than plain submodules; `maildir`
becomes its own package now; the `imap` package split into protocol, eio and
sync packages is deferred to match the JMAP sibling; the strategy layer is
the last step.

### Step notes

Step 1. The facade `lib/eio/imap_eio.mli` is the single documented copy of
Auth, Error, Transport, Client, Selected and Pool. Each core `.mli` keeps its
signature and gets a one-sentence synopsis plus one-line value docs at most.
The same for `lib/store/sync_journal.mli`, `operation_intent.mli` and
`blob_store.mli`, which `lib/store/imap_store.mli` re-exports. Guard:
`test/api/check.sh` must still pass.

Step 2. `Imap.Capability` in `lib/protocol/` with constructors carrying
parameters where the wire does: `Auth of string`, `Thread of algorithm`,
`Compress of [ `Deflate ]`, `Utf8 of [ `Accept | `Only ]`,
`Context of [ `Search | `Sort ]`, `Messagelimit of int64`,
`Savelimit of int64`, and `Other of string`. Provide `of_wire`, `to_wire`,
`equal`, `compare`, `pp`, and a set with `mem`, `of_list`, `to_list`.
Capability names compare case-insensitively per RFC 9051. Seed the
constructor list from the capability strings enumerated by the Phase 2
reviews of `client.ml` and `selected.ml`. `Error.Unsupported of
Imap.Capability.t` replaces every `State`/`Limit` text returned for a missing
or unenabled extension. `Client.enable` is the general RFC 5161 ENABLE.

Step 3. Move `lib/sync/spool.ml{,i}` into a private library stanza
(`(library (name imap_sync_spool) (package imap))`) that `imap.sync` and
`test/io` both link. Delete the copy_files rules.

Step 4. Create `bleeding/maildir/` with its own `dune-project`, package
`maildir`, library `maildir`, module `Maildir`, public submodule
`Maildir.Dotlock`. Dependencies: eio, eio.unix, unix, digestif, mail-flag,
plus optint only if the implementation needs it. Remove the
`internal_date` field, the `?internal_date` argument and
`upload_internal_date`; `imap.sync` gets a private adapter between mtime and
`Imap.Internal_date`. Move `paged_inventory`, `with_inventory_pages`,
`inventory_count`, `inventory_find`, `inventory_page` and the SQLite staging
into a private `Local_inventory` module in `imap.sync`. Maildir keeps `scan`,
a bounded fold over entries, and `with_unchanged_occurrence`. Replace
`with_writer_lock` with `with_writer : t -> (writer -> 'a) -> 'a` and make
`append`, `set_flags`, `remove` and `recover` take the writer. `open_dir` and
`scan` return `(_, error) result` over a typed error. Delete `lib/maildir/`
and `imap.maildir`; update every consumer and test. Move `test/maildir` and
`test/dotlock` under `bleeding/maildir/test/`.

Steps 5 to 9 are ordered so the tree builds after each. Step 9 groups: on
the lease Condstore, Qresync, Uidplus, Move, Binary, Searchres, Sort, Esort,
Thread, Partial, Preview, Objectid, Objectid_plus, Uidbatches, Messagelimit,
Notify; on the connection Acl, Quota, Metadata, Notify, Objectid_plus,
Multiappend, Compress. Each group has `require` returning a witness whose
operations take it instead of the bare lease or client. The base `Selected`
keeps search, fetch, store, copy, expunge, noop and idle.

### 0.R Phase 2 findings

Filled in by the review batches. Each finding is `file:line`, one sentence,
severity in `[]`. Fixes applied in step F are ticked here.


## Current baseline — do not reimplement

Five public libraries now separate protocol, Eio, Maildir, store and sync.
CRAM-MD5, verified TLS/STARTTLS, streamed bodies, standard Maildir flags,
Dovecot keywords, SQLite staging/journals and many modern extensions exist.
Custom Maildir metadata sidecars are no longer written.

The latest full release-check build passes. Recent store tests and 49 bridge
fault cases pass; all 29 live Dovecot cases passed after store/GC changes,
including shared Maildir metadata interchange and APPEND/EXPUNGE crash recovery.
These results do not prove every failure boundary or concurrent schedule.
A subsequent forced JMAP run passed 798 cases (43 unconfigured live skips).
Fresh Cyrus runs passed both IMAP oracle cases and all 45 separate JMAP oracle
cases with no live skips. The subsequently added cross-protocol case also passes; remaining fidelity
coverage is listed in §5.

## 1. Finish ownership and structural review

- [ ] Finish implementation/dead-code review against the accepted interface
  report; review shared `mail-flag` and `sqlite3-eio` changes with their callers.
- [ ] Introduce a scoped Maildir writer capability. Mutating operations must
  require a live lease; test escaped handles, competing writers, cancellation,
  stale observations and basename/inode replacement.
- [ ] Move disk-backed Maildir inventory responsibilities to the appropriate
  sync/storage layer without weakening bounded scanning or snapshot lifetimes.
- [ ] Review remaining large modules by purpose, especially mailbox publication,
  synchronization journals and CLI configuration. Split only where ownership
  and dependency clarity improve; avoid recreating tiny public libraries.
- [ ] Revalidate installed public interfaces and dependent applications after
  public API changes. Preserve shared IMAP/JMAP flag type identity.

## 2. Close durability and recovery gaps

- [ ] Enumerate every file fsync/rename/directory-fsync and SQLite transaction
  boundary for APPEND, local import, FLAGS, remote/local deletion and publication.
  Add subprocess crashes with old-or-new recovery assertions at each boundary.
- [ ] Cover disconnect/crash between command send, server mutation, tagged
  receipt, journal observation and pair commit. Ambiguous outcomes must remain
  held, never blindly replayed; duplicate candidates must remain distinct.
- [ ] Extend model-based randomized mutation/crash schedules and compare restart
  convergence with a fresh mailbox model, including UIDVALIDITY resets and CAS
  races. Existing directed cases are not the complete SPEC §14.2 matrix.
- [ ] Test concurrent schema initialization/migration and remaining structural
  corruption cases; preserve immediate cleanup on failed opens and atomic
  journal/pair/cursor transitions.
- [ ] Establish writer ownership for blob GC across relevant processes, or make
  the required offline/quiescent operational boundary enforceable. The bounded
  iterator exists; it does not itself prevent a writer adding references.

## 3. Establish Maildir interoperability and migration guarantees

- [ ] Test concurrent Dovecot/local keyword updates, flag renames, message
  replacement/deletion and INTERNALDATE preservation. Sequential shared-tree
  interoperability already passes; it is not adversarial concurrency coverage.
- [ ] Resolve safe unattended stale-dotlock recovery. Current behavior refuses
  existing locks and requires offline intervention. Do not restore the unsafe
  stat-then-unlink stale-lock reclamation race.
- [ ] Document the precise supported concurrency contract, including external
  tools that do not honor the application writer lease.
- [ ] Provide an explicit migration/repair path for old nonempty `.imap-flags`
  and `.imap-dates` trees. They are currently rejected. Preserve occurrence
  identity, dates and custom keywords, and handle Dovecot's 26 keyword slots
  without silent loss. Decide whether this is an offline tool or documented
  export/import procedure and test interrupted migration.

## 4. Complete resource and operational gates

- [ ] Complete large-body measurements for 1/10/100 MiB: normal GC, retained
  live heap, RSS, throughput and less-compressible input across plain, TLS,
  STARTTLS and compression. Distinguish sampled values from absolute peaks,
  managed heap from native buffers, and runtime overhead from payload buffering.
- [ ] Extend measurements to hashing/file sinks, blob hydration and other body
  transfer paths. The current probe covers streamed APPEND/FETCH only.
- [ ] Run current 100,001-occurrence and opt-in million-occurrence scale suites
  after inventory changes; record peak memory and journal paging behavior.
- [ ] Measure bounded blob GC at large scale; its new iterator uses 256-name
  batches and indexed reachability, while compatibility wrappers collect lists.
- [ ] Audit deadlines/progress timeouts, retry/backoff, pool/watcher shutdown,
  redacted diagnostics, profiles and metrics. Document ownership and test
  timeout/cancellation paths without treating uncertain sends as retryable.
- [ ] Complete whole-watcher overflow/backpressure/checkpoint tests. Typed
  NOTIFICATIONOVERFLOW and IDLE completion tests already exist.

## 5. Finish extension and independent-server acceptance

- [ ] Build a current per-extension matrix linking capability negotiation,
  enabled modes, fallback behavior, limits/errors, named scripted tests and live
  evidence. An exported function is not sufficient conformance evidence.
- [ ] Audit SPEC §14.1 fragmentation, malformed input, scalar boundaries,
  partial responses, literal failures and lifecycle coverage against actual
  tests. Fill gaps instead of treating existing suite labels as proof.
- [ ] Check core rev1/explicit rev2 plus QRESYNC/CONDSTORE, UIDPLUS/MOVE,
  SORT/THREAD/ESORT, SEARCHRES/PARTIAL, BINARY, COMPRESS, MULTIAPPEND,
  LITERAL-/LITERAL+, NOTIFY, UIDBATCHES, UIDONLY, MESSAGELIMIT/SAVELIMIT and
  OBJECTID/OBJECTID+ against that matrix. These are audit targets, not a list of
  entirely missing implementations. Keep draft OBJECTID+ explicitly gated.
- [x] Add and run a mandatory Cyrus test for JMAP import → IMAP exact body,
  IMAP APPEND → JMAP exact download, and bidirectional standard/custom keyword
  changes. The new `test_cross_protocol.ml` passes alongside both IMAP oracle
  cases; required mode fails when either endpoint is missing.
- [x] Cover mapping rejection for overlength keywords, Recent/unknown system
  flags and semantic collisions; live-test custom `seen` separately from
  system Seen, and Deleted invisibility in JMAP get/query with restoration.
- [ ] Extend cross-protocol coverage to the full synthetic MIME/duplicate
  corpus, mailbox counts for Deleted messages and server-specific unsupported
  flag behavior. Directed mapping tests do not close the complete fidelity
  matrix or implement a proxy conversion policy.
- [ ] Rerun pinned Stalwart baseline and separate OBJECTID+ fixtures after the
  protocol/identity changes. Rust source inspection is not interoperability.
- [ ] Retain current Dovecot CRAM-MD5, TLS, shared Maildir and crash tests after
  ownership changes; retain authentication failure/redaction coverage.

## 6. Release documentation and final validation

- [ ] Consolidate historical checkpoints into accurate current contracts and
  architecture documentation; remove stale sidecar and old-layout descriptions.
- [ ] Build API documentation cleanly and verify examples and recovery commands.
- [ ] Run required IMAP and JMAP suites, dependent application builds, mandatory
  live fixtures and the workspace release-check integration gate. Record actual
  reruns, cached results and skips separately.
- [ ] Audit every SPEC acceptance requirement against named current evidence
  before claiming production readiness. Keep unresolved ambiguities visible.

## Explicitly deferred or separate scope

- SCRAM and SASL security layers are deferred in the current SPEC. Keep that
  limitation visible; CRAM-MD5 does not encrypt subsequent traffic.
- A complete IMAP/JMAP proxy frontend is a separate product decision (M9).
  Reusable identity, flags, streaming and durable-state boundaries are required
  now; neither server frontend is implemented by these client libraries.
- Durable MULTIAPPEND batch journaling is not implied by the low-level API.
  Current durable sync dispatches individually journaled APPEND operations.

## Immediate handoff

Review the checkpoint before another ownership refactor. The normal-GC,
random-ASCII and STARTTLS memory-probe additions now pass all 18 live
size/transport combinations (36 APPEND/FETCH measurements). Evidence is in
`bleeding/imap/test/dovecot/body-memory-normal-results.csv`. Maximum sampled
additional RSS was 11,419,648 bytes; reserved major heap grew by at most
4,803,952 bytes. Normal mode does not measure live heap: these figures cannot
be compared directly to the 8 MiB live-payload target. Sampling, throughput,
truly incompressible binary input and other transfer paths remain open. The
fixture was removed after the run.
Most IMAP files remain untracked, so inspect the tree directly rather than only
`git diff`. No commit has been created for this checkpoint.
