# Production acceptance gates

This audit records the state on 2026-09-27. Passing the existing tests does not
establish production readiness. The acceptance criteria remain IMAP-SPEC
sections 14 and 15, together with the requested standard Maildir format and
module restructuring.

## Evidence boundaries

- All 64 pinned reference documents pass byte-length and SHA-256 verification.
  This establishes corpus integrity, not current errata coverage.
- The completed compression release-check run passed 49 bridge-fault cases and
  27 live Dovecot cases. The latter includes compressed sessions and IDLE.
- Earlier milestone results in IMAP-SPEC are historical evidence. They must be
  rerun where subsequent changes affect their behavior.
- Interface presence establishes an available operation, not RFC conformance.
  No implementation review has been performed as part of this acceptance audit.

The offline release-check build and all targeted IMAP test aliases completed
successfully on 2026-09-27. Dune reran 9 mirror, 8 flag-sync, 17 CLI, 7 deletion,
6 Maildir, 25 SQLite store and 1 resource-scale tests. The scale test took
35.798 seconds. The protocol, Eio, bridge-fault, I/O and policy aliases used
cached successful results; this was not a forced rerun of every test. The
output is in `/tmp/imap-production-baseline.log` for this workspace session.
No live fixture was started by this command.

## Open release gates

| Requirement | Current evidence or gap | Evidence required to close |
| --- | --- | --- |
| Standard Maildir metadata | Standard metadata implemented; 11 local-format tests and live sync pass | Dovecot keyword mapping, filename flags and mtime dates; cross-tool read/write tests; no silent metadata loss |
| Stable local identity | Maildir observations and writer preconditions remain caller-constructible/conventional | Private observations and scoped writer capability; basename/UID stability, stale and escaped-handle tests |
| Logical public boundaries | Five public libraries now exist; internal ownership and module splits remain | Complete internal module splits and private interfaces, installed-interface checks and affected consumer validation |
| CRAM-MD5 and Dovecot | All 29 current live cases pass after store/GC changes, including shared Maildir, TLS and crash recovery; see latest checkpoint | Retain challenge/failure/redaction coverage and rerun live CRAM tests after ownership changes |
| SQLite durability | Store tests cover schema migration, restart, staged publication and CAS | Preserve atomic journal/pair/cursor transitions through the store split; rerun process-crash and corruption scenarios |
| Full crash-boundary coverage | SPEC section 17 explicitly leaves additional fsync/send/receipt/pair-commit injection work | Enumerated crash points with old-or-new recovery assertions; no ambiguous mutation replay |
| Concurrent external Maildir tools | Current contract says external programs do not honor the app writer lease | Dovecot-compatible metadata locking and concurrent rename/keyword/date tests; explicit supported concurrency contract |
| Large-mailbox bounds | Existing scale suite and historical million-file runs | Current 100,001-file regression plus million-file release run after inventory relocation; recorded peak memory |
| Large-body bounds | SPEC requires 1/10/100 MiB measurements and less than 8 MiB additional live payload buffering | Repeatable measurements across relevant plain/TLS/compressed transport paths, distinguishing managed heap from RSS |
| MULTIAPPEND | Capability-gated streamed API, ordered bounded receipts, scripted failure/cancellation tests and live Dovecot correspondence test implemented | Durable sync currently dispatches individual journaled APPENDs; batch journaling/recovery is a separate workflow, not implied by the low-level API |
| Session command surface | Client NOOP/LOGOUT and selected-lease NOOP implemented; bounded shutdown validates BYE and completion | Retain scripted completion/cancellation/limit checks and live server validation; application deadlines remain explicit Eio scopes |
| Non-synchronizing literals | APPEND and MULTIAPPEND negotiate small non-synchronizing literals; named 4095/4096/4097, mixed-batch, binary and revision-negotiation tests pass | Keep boundary/fallback tests and live APPEND regression coverage; larger bodies intentionally use synchronizing literals |
| Resource-limit completeness | RFC 9738 SAVELIMIT is a COPY/APPEND capability, not a separate response code; MESSAGELIMIT is typed. Batch preflight and partial SAVE handle rejection have directed tests | Retain command-specific partial/uncertain behavior and checkpoint coverage; no SAVELIMIT response variant is required |
| Notification overflow | NOTIFY setup reports overflow; IDLE preserves it before/after continuation, consumes DONE completion, and remains usable; flood limits close safely | Watch rescans after every wakeup by construction; retain whole-watcher checkpoint tests and broader scheduling/backpressure coverage |
| Operational client | Watch exposes connect/scan deadlines; common client deadlines, profiles and metrics are not established here | Document the actual timeout/ownership/configuration/observability contracts and test them at the relevant layer |
| Cross-protocol interoperability | Shared mail-flag and Cyrus integration exist | Current mandatory Cyrus IMAP/JMAP body and keyword round trips, including explicitly unrepresentable mappings |
| Independent servers | SPEC records Stalwart baseline and OBJECTID+ fixture runs | Rerun both pinned fixtures after protocol/identity changes; retain explicit draft mode |
| Modern extension completeness | Interfaces cover many extensions, but not every acceptance requirement has linked tests | A per-extension capability/fallback/error matrix with named offline and live cases; unsupported features stated explicitly |
| Documentation | README/SPEC describe the growing implementation, including soon-obsolete sidecars | Consistent public contracts, format transition instructions, architecture pages and clean documentation build |

The absence of a public API in this table is evidence of a surface gap. It is
not evidence that every internal behavior related to that feature is absent.
Likewise, a test label is not sufficient to prove all requirements of its RFC.

SCRAM and SASL security layers are explicitly deferred by the current SPEC.
They must remain disclosed limitations; CRAM-MD5 support is a separate explicit
requirement and does not imply confidentiality for subsequent traffic.

The proxy frontend is a separate product decision in milestone M9. Reusable
identity, flags, streaming and durable-state boundaries are required now; neither
a complete IMAP server nor a JMAP server is claimed by the current client.

## Next implementation order

1. Continue the implementation review and ownership work in `../../IMAP-STRUCTURE-REVIEW.md`.
2. Complete the module and resource-ownership changes, preserving the current
   offline and live baseline.
3. Implement the standard Maildir format, including explicit legacy handling,
   keyword overflow, timestamp limits and concurrent metadata locking.
4. Close the crash-boundary and resource-bound gates against the new layout.
5. Complete the command/extension matrix and implement the remaining required
   capabilities with independent transcript and server validation.

Do not mark the goal complete while any required gate is open or supported only
by historical or indirect evidence.

## Restructuring validation update

The five-library source layout builds with release-check. After the first review
fixes, Eio, Maildir, SQLite store, bridge-fault, CLI, flag-sync, deletion, policy
and spool test aliases passed. Store coverage includes the new pending-journal
blob-root assertions; Maildir includes a new flag-order/duplicate test; Eio
includes IDLE control-literal preservation and limit checks. Output for this
workspace session is `/tmp/imap-restructure-check2.log`.

The review also found authentication diagnostic redaction, journal
input invariant and Maildir sidecar problems. Subsequent checkpoints below record
which were fixed. Those remain release blockers;
the directory consolidation alone does not satisfy the resource-ownership plan.


The standard Maildir conversion now passes local-format, fault, live Dovecot and
scale tests. Direct same-directory Dovecot co-access, an explicit legacy migration
utility, additional lock-loss/crash schedules and the outstanding client/Selected
review findings remain open. See the second-batch findings in the structure review.


The Client/Selected correctness review fixes pass the scripted Eio, 49 fault and
27 live Dovecot cases. Authentication diagnostics, missing SEARCH evidence,
COPYUID correspondence, lease failure cleanup, METADATA-SERVER gating and
structured FETCH order have directed regressions. Public internal capabilities,
stronger journal invariants and the remaining structural work are still open.
The workspace validation log is `/tmp/imap-client-review-live.log`.


The Eio public capability boundary and journal evidence checks now have directed
regressions. Journal validation rejects contradictory occurrence identity,
content, flags, dates and destination receipts without committing a pair.
Release-check, 25 existing store cases plus the directed evidence executable,
flag/deletion suites, 49 bridge-fault cases and 27 live Dovecot cases pass.
Validation log: `/tmp/imap-journal-evidence-live.log`. Store decomposition,
Maildir ownership/inventory separation, direct Dovecot filesystem co-access,
legacy migration and the remaining extension/durability gates remain open.


### Direct Dovecot Maildir interoperability

The optional `IMAP_DOVECOT_SHARED=1` fixture mode shares only a generated mail
tree, uses the host UID/GID for Dovecot mail processes, and discovers INBOX via
`doveadm mailbox path`. The live test writes with Imap_maildir and reads through
Dovecot, then changes flags/keywords through IMAP and reads them locally. It
checks exact body bytes, system flags, additive keyword mapping, INTERNALDATE,
local flag renaming after Dovecot changes, and targeted expunge of the same file.
All 28 live Dovecot tests passed in this mode; the fixture was removed afterward.
Log: `/tmp/imap-maildir-coaccess-final.log`. Dune now tracks fixture environment
variables explicitly. This proves sequential interoperability on shared files;
concurrent lock-loss and process-crash schedules remain separate open gates.
Legacy sidecar migration and the structural ownership/inventory work remain open.


### Bounded stale metadata lock inspection (superseded below)

Maildir stale-owner inspection now opens a nonblocking descriptor, verifies its
regular-file identity against the inspected path, and reads at most 1,025 bytes
before deciding whether a lock is eligible for recovery. This removes the
unbounded reopen/read race when the path is replaced or the lock grows. The
final unlink still requires the inspected inode to remain at the lock path.

Directed tests recover a dead local PID and preserve live, foreign-host,
malformed, oversized, empty, FIFO, directory and symlink locks. Release-check,
all 12 Maildir cases and 49 bridge-fault cases pass (`/tmp/imap-lock-recovery.log`).
This does not complete adversarial concurrent replacement, cancellation or
crash-point coverage; those remain open production gates.


### Scoped dotlock ownership and recovery correction

Metadata locking is now a private `Dotlock` module, separating filesystem
ownership from Maildir occurrence/keyword handling. An outer cancellation-protected
finalizer owns the descriptor immediately after open, including failures before
stat completes. Refresh is serialized, writes through the owned descriptor,
and checks path identity before and after. Refresh handles expire at scope exit.
Directed tests cover return, exception, cancellation, subsequent acquisition,
escaped refresh, concurrent refresh without owner-content corruption, and
replacement before explicit refresh or callback cleanup.

This review supersedes the preceding automatic stale-owner recovery checkpoint.
The bounded reader did not solve the check/unlink race between two reclaimers:
one could unlink the other's newly created live lock. Automatic stale lock
reclamation has therefore been removed, not declared production-safe. All
existing lock files now produce Writer_lock_busy, including dead-owner files;
recovery currently requires offline removal while every Maildir user is stopped.
Safe unattended recovery interoperating with Dovecot remains an explicit open
production requirement. Inode checks also cannot make cleanup atomic against
an uncooperative process replacing paths between syscalls.

Release-check, the directed dotlock executable, 12 Maildir cases and 49
bridge-fault cases pass. Log: `/tmp/imap-dotlock-lifetime-final.log`.


### RFC 3502 MULTIAPPEND checkpoint

`Client.append_message` describes borrowed streams and `Client.append_messages`
streams 1..1000 nonempty messages with per-message flags and INTERNALDATE under
one connection lock. Multiple messages require MULTIAPPEND; no sequential
fallback weakens atomicity. Every argument is validated before APPEND dispatch,
with 1 MiB total syntax and bounded per-argument syntax. The existing OBJECTID+
destination guard applies before mutation. Single and batch APPEND share one
64 KiB streaming implementation, synchronizing each literal and carrying reply
budgets across the whole exchange.

Receipts retain input-message UID order, reject duplicates/cardinality mismatch,
and bound expansion by message count. Missing APPENDUID means known success with
unknown identity. A tagged rejection before a later literal consumes none of
that source and remains a definite atomic rejection. Short source, lost response,
invalid UID correspondence, unexpected continuation after the final literal,
or a partial notice followed by success closes and reports uncertainty;
cancellation closes and propagates. This is a low-level API, not a durable batch
journal or automatic replay mechanism. Current sync retains individual journaled
APPEND operations.

Release-check, all scripted Eio/protocol suites, 49 bridge-fault cases and all 29
live Dovecot cases passed. Directed tests cover exact wire separation, bounded
source reads, capability/preflight rejection, later-literal NO, missing/invalid
receipts, giant ranges, cancellation in the second source, aggregate reply
budgets and contradictory completion. Dovecot confirms input-order receipts,
exact message bodies, per-message flags and INTERNALDATE. Fixture removed.
Logs: `/tmp/imap-multiappend-live.log`, `/tmp/imap-multiappend-final.log`.


### Connection lifecycle checkpoint

Client now provides NOOP and graceful LOGOUT; Selected provides lease-scoped
NOOP. Polling retains unsolicited updates in wire order and does not advance
durable checkpoints. LOGOUT requires BYE followed by the matching tagged OK,
rejects malformed/repeated continuations and missing completion, and closes
on every outcome, including rejection and cancellation. Its metadata and reply
count budgets match the other session exchanges. Immediate Client.close remains
available; callers apply Eio timeout scopes when shutdown needs a deadline.

Scripted tests cover ordinary polling, selected update order and expired leases,
normal logout, missing/repeated BYE, wrong tags, continuations, rejection, EOF,
cancellation and response limits. Release-check, all scripted Eio suites and
29 live Dovecot tests pass. The live authentication case now verifies connection
and selected NOOP followed by graceful logout. Fixture removed after validation.
Log: `/tmp/imap-lifecycle-live.log`. Remaining production gates are unchanged.


### RFC 9738 capability and saved-result limits

Corrected the acceptance audit: SAVELIMIT is a capability restricting COPY and
APPEND, not a separate response code and not a SEARCH SAVE limit. Both capability
forms use the MESSAGELIMIT response code. MULTIAPPEND now checks advertised
MESSAGELIMIT/SAVELIMIT counts before dispatch, rejects malformed advertised
values, and permits batches exactly at the limit.

Directed SEARCHRES tests prove tagged or untagged partial MESSAGELIMIT SAVE
completion cannot mint a handle and invalidates the previous saved handle.
SAVELIMIT alone does not restrict SEARCH. SAVE COUNT validation also rejects an
unexpected ESEARCH PARTIAL field rather than treating it as complete evidence.
Release-check and all scripted Eio suites pass; these capability/error paths
were verified with controlled transcripts rather than a new live-server run.
Log: `/tmp/imap-save-limits-final.log`.


### Negotiated non-synchronizing APPEND literals

APPEND and MULTIAPPEND now use non-synchronizing literals up to 4096 octets when
LITERAL-, LITERAL+, or effective IMAP4rev2 permits them. Larger literals remain
synchronizing even with LITERAL+; this is permitted by RFC 7888 and avoids
unbounded speculative uploads. A dual-revision advertisement requires successful
rev2 activation unless an explicit literal capability independently permits it.
Binary APPEND still separately requires BINARY. Batch messages choose their
literal mode independently and share the same streaming buffer/reply budgets.

Directed tests cover 4095/4096/4097 bytes with rev1, LITERAL-, LITERAL+ and rev2;
failed/successful dual-revision activation; literal8 markers; mixed-mode batches;
tagged rejection and illegal continuations. Release-check, all scripted Eio and
protocol tests, 49 bridge-fault cases and 29 live Dovecot cases passed. The live
suite exercises sync/crash recovery and single, binary and multi-message uploads
through the changed path. Fixture removed. Logs:
`/tmp/imap-literal-modes-final.log`, `/tmp/imap-literal-modes-live.log`.


### Notification overflow regression checkpoint

Reviewed RFC 5465 section 5.8 against NOTIFY setup, IDLE and Watch. Existing
NOTIFY methods report setup overflow; IDLE retains the typed overflow update,
and Watch runs a fresh durable scan after every wakeup rather than advancing a
checkpoint from notification data. No runtime change was required for those
paths. The selected API now explicitly documents that overflow disables NOTIFY
registration and requires reconciliation before re-registration.

New directed tests place NOTIFICATIONOVERFLOW both before and after the IDLE
continuation, verify completion updates retain wire order, then issue NOOP and
UNSELECT to prove tagged completion was consumed. Notification floods before
continuation and during completion hit the aggregate response budget and close
the connection. Release-check and all scripted Eio suites pass.
Log: `/tmp/imap-notify-overflow.log`. This is controlled-transcript evidence;
whole-watcher crash/checkpoint schedules remain a separate production gate.


### Store ownership and journal decomposition

The store implementation now has private purpose-based modules:

- `Database` owns the shared SQLite handle, mutex, statement lifetimes and
  transaction helper.
- `Schema` opens/configures connections, validates supported schemas and applies
  migrations under that same transaction owner.
- `Record_codec` defines persisted IMAP scalar/scope representations.
- `Sync_journal` owns occurrence pairs, conflicts and mutation evidence.
- `Imap_store` retains mailbox staging/publication, the legacy append journal
  and blob operations; its public API is unchanged.

There are no extra public libraries, additional connection owners or nested
transaction boundaries. The former ~2,000-line implementation root is now
850 lines; blob and mailbox publication decomposition remains open.

Review identified existing initialization defects and fixed them during the
split. Invalid blob directories are checked before acquiring SQLite. Failed
initialization closes the acquired handle immediately while preserving the
original exception/backtrace. Read-only version and schema checks share one
read transaction, preventing mixed snapshots during concurrent migration.
Schema validation now checks the expected unique occurrence index definitions,
not merely column/index names; missing, nonunique or differently filtered pair
identity indexes are rejected.

Release-check, all 25 existing store cases (including v2..v13 migrations and
process restarts), directed journal/schema guard tests, flag/deletion suites and
49 bridge-fault cases pass. The new guard test repeatedly opens damaged schemas
within one long-lived switch and, on Linux, confirms no database descriptors
remain. Log: `/tmp/imap-store-boundaries-final.log`. Live server tests were not
repeated for this database boundary change; concurrent migration scheduling and
remaining structural/production gates are still open.


### Blob and command journal boundaries

`Blob_store` and `Operation_intent` now own content-addressed files/references
and command recovery respectively. Both are private modules within `imap.store`,
with explicit interfaces and the same `Database.t` owner. Cursor decoding lives
in `Record_codec`, so neither module depends on the public facade. Public type
constructors, `Imap_store.Blob`, and all existing entry points remain compatible.
No schema or transaction behavior changed. `Imap_store` is now 455 lines,
focused on mailbox staging and publication plus public re-exports.

Release-check and the store, bridge-fault, flag-sync and deletion suites pass,
including migration, process restart and >100k-row staging coverage. Validation
log: `/tmp/imap-store-purpose-check.log`. Live servers were not rerun for this
internal extraction. Unbounded orphan inventory and the remaining Maildir
ownership/inventory work remain open.


### APPEND journal metadata validation

New command intents now require a 64-character lowercase SHA-256 digest and
parse supplied INTERNALDATE values with the protocol date parser before
insertion. This closes the two validation gaps identified during the command
journal extraction review. Rejected preparation leaves no persisted intent and
the ID can immediately be reused for valid preparation.

Historical rows remain readable without rewriting their evidence. A directed
restart test injects an old invalid digest/date, reopens the store, verifies
inspection and pending-intent enumeration, and explicitly rejects that intent.
New tests also cover digest length/alphabet/prefix/control errors, impossible
dates, invalid clocks/zones, and preservation of valid timezone spellings.
Release-check, the new validation test, all store tests and 49 bridge-fault
cases pass (`/tmp/imap-intent-validation.log`). No schema migration or live
server behavior changed. Other production gates remain open.


### Bounded blob orphan collection

`Imap_store.Blob.iter_orphan_candidates` and `reap_orphans_iter` now scan
archives with batches of at most 256 names and indexed per-hash reachability
checks. Snapshot references and unresolved entries in either journal remain GC
roots. Write-open creates two auxiliary partial indexes without changing the
schema version or stored row representation. Existing list-returning APIs are
compatible wrappers and explicitly retain their unbounded result-list cost.

Directory handles close on normal return, callback exceptions and cancellation.
Reaping syncs the directory after any attempted unlink, including when a callback
fails or is cancelled. Callbacks run before that final sync and therefore do not
constitute durable removal receipts. All writers must still remain quiescent,
including writers in other processes; these APIs do not acquire a global writer
lease.

Release-check, store and 49 bridge-fault tests pass. A directed 1,031-file test
crosses multiple batches, rejects duplicate/missing visits, reads the store from
callbacks, interrupts reaping by exception and cancellation, checks directory
handle closure on Linux, and finishes collection afterward. SQLite query-plan
checks verify indexed SEARCH for all three root lookups. Existing store tests
verify roots in both journals survive reopening and collection. Logs:
`/tmp/imap-blob-stream-final.log` and `/tmp/imap-blob-query-plan.log`.
Large-scale peak-memory measurements and global writer ownership remain open;
this change establishes a bounded inventory implementation, not completion of
those broader production gates.


### Live Dovecot validation after store and GC changes

The required Dovecot suite was rerun against a fresh pinned 2.4.5 fixture with
shared filesystem mode enabled after the store decomposition, APPEND intent
validation and streaming blob GC changes. All 29 cases passed in 11.741 seconds
(`/tmp/imap-store-gc-live.log`). This includes successful/rejected CRAM-MD5,
verified TLS and STARTTLS, compression, ordered MULTIAPPEND receipts, direct
Dovecot/local Maildir metadata interchange, and durable APPEND/UID EXPUNGE
process-crash recovery. The disposable `imap-store-gc-check` container and its
owned certificate/Maildir tree were removed afterward.

This establishes the current Dovecot integration baseline. It does not close
the full crash-point matrix, adversarial concurrent Maildir access, large-body
memory measurements, or the separate Cyrus/Stalwart revalidation gates.


### Large-body streaming measurement baseline

The new opt-in Dovecot `body_memory.exe` probe runs in a fresh process per
transport/size, generates bytes incrementally, and checks exact APPEND/FETCH
lengths and SHA-256. All 1/10/100 MiB combinations passed over plain, TLS,
DEFLATE and TLS+DEFLATE with CRAM-MD5. The largest additional sampled live
managed heap was 80,408 bytes; additional sampled RSS peaked at 6,025,216 bytes.
Reproducible instructions and checked-in CSV evidence are in
`bleeding/imap/test/dovecot/README.md` and `body-memory-results.csv`.

The probe samples at MiB boundaries and forces collection for live-word counts.
This establishes a retained-memory baseline with a compressible payload, not
absolute peaks under normal GC or incompressible input. Those measurements,
STARTTLS-specific measurements and other streaming paths remain open. The
owned Dovecot fixture was removed after all 12 cases completed successfully.


### Review-checkpoint JMAP and Cyrus verification

A forced JMAP regression run passed 798 cases; 43 opt-in live cases skipped
without a configured fixture (`/tmp/imap-jmap-checkpoint-tests.log`). Against a
fresh isolated Cyrus fixture, the required IMAP oracle passed both cases
(`/tmp/imap-review-cyrus.log`) and the separate JMAP oracle passed all 45 cases
with no skips (`/tmp/imap-review-jmap-live.log`). The owned fixture was removed.

Inspection corrected an evidence overstatement: the current IMAP oracle has no
JMAP/LMTP calls. Its existing tests and separate JMAP tests do not establish
bidirectional body/keyword round trips. Those tests still need implementation;
`IMAP-TODO.md` and the oracle README now state that explicitly. Shared protocol
regressions pass, but the cross-protocol acceptance gate remains open.


### Cross-protocol oracle implemented

`test/oracle/test_cross_protocol.ml` now exercises JMAP import → IMAP exact body,
IMAP APPEND → JMAP exact download, JMAP keyword changes → IMAP SEARCH and IMAP
STORE → JMAP Email/get. The test checks BODY.PEEK leaves the initially unseen
message unseen, validates the destination mailbox and cleans up its isolated
mailbox through JMAP. It reuses the JMAP harness source in the private test build
without making the harness a public library.

All three Cyrus cases passed in a fresh fixture (`/tmp/imap-cross-live.log`).
Release-check passes. Separate executable checks confirm hermetic skip without
configuration and required-mode failure when either endpoint is missing.
The owned fixture was removed. This supersedes the earlier missing-roundtrip
finding for these paths; unrepresentable keywords and the full cross-protocol
MIME/duplicate corpus remain open in `IMAP-TODO.md`.


### Cross-protocol flag fidelity

The oracle now distinguishes the custom keyword `seen` from system Seen across
JMAP import, IMAP SEARCH/STORE and JMAP Email/get. It asserts the RFC 8621 §4.1.1
rule that IMAP Deleted messages disappear from JMAP get/query, then return with
keywords intact after removal of Deleted. The first attempted test incorrectly
expected the Email to remain visible; both Cyrus evidence and the local RFC
confirmed the required invisibility behavior, and the assertion was corrected.
A future proxy must implement visibility/count semantics, not merely discard
Deleted during keyword conversion.

Offline tests preserve an overlength IMAP keyword while rejecting its JMAP
validation, reject false semantic aliases for Recent/unknown system flags and
colliding names, and exclude Deleted from shared-to-JMAP conversion. These do
not constitute a complete proxy mapping API. Release-check and all four oracle
cases pass (`/tmp/imap-cross-flags-final.log`); the fixture was removed.
Mailbox-count and broader MIME/duplicate fidelity coverage remain open.
