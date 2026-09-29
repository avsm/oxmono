# IMAP review checkpoint — 2026-09-27

The implementation is ready for review, not a production-release claim.
No commit was created. Most IMAP files are still untracked, so ordinary
`git diff` alone does not show the implementation. Review `bleeding/imap/`
directly, plus the tracked shared-library changes listed by `git status`.

## Start here

- `bleeding/imap/README.md`: public API and operational contracts.
- `IMAP-STRUCTURE-REVIEW.md`: original interface review and subsequent changes.
- `bleeding/imap/lib/`: five public libraries grouped into protocol, Eio,
  Maildir, store and synchronization.
- `bleeding/imap/PRODUCTION-GATES.md`: requirements and remaining evidence gaps.
- `IMAP-SPEC.md`: detailed design and historical implementation checkpoints.

The latter reports contain historical checkpoints; later entries supersede
older implementation descriptions. They are not a single current test report.

## Implemented and exercised

- Eio IMAP client with CRAM-MD5, verified TLS/STARTTLS, scoped selected-mailbox
  access, streaming bodies and modern extension APIs.
- Standard Maildir filename flags, Dovecot keyword mappings and mtime dates;
  custom metadata sidecars are rejected rather than newly written.
- SQLite cursor/snapshot staging, mutation journals, occurrence pairs and
  content-addressed blobs, with compare-and-swap publication and recovery.
- Five public library boundaries; private Eio implementation and store modules
  share explicit resource owners.
- Recent fixes reject invalid metadata in new APPEND intents, preserve legacy
  evidence, and add bounded streaming blob GC with exception/cancellation
  cleanup and indexed reachability checks.

## Latest verification

- Full IMAP release-check build passes, including the latest memory probe.
- Store tests cover migration/restart, >100k-row staging, schema guards,
  operation evidence, APPEND validation and interrupted streaming blob GC.
- All 49 bridge-fault cases passed after the store/GC changes.
- All 29 Dovecot cases passed after those changes, including CRAM-MD5,
  TLS/STARTTLS, compression, shared Maildir and process-crash recovery.
- The original memory probe passed 12 combinations of 1/10/100 MiB and four
  transports, with exact length/digest checks. Checked-in CSV records sampled
  retained-heap/RSS measurements, not absolute peaks.

Latest work not yet live-tested: normal-GC sampling, random-ASCII payloads and
STARTTLS modes in `test/dovecot/body_memory.ml`. These extensions build; they
must not be inferred to have passed from the older CSV. No fixture remains
from the preceding live runs, and no build/test process was left running by
this checkpoint.

## Material remaining work

1. Finish ownership and module review, especially scoped Maildir writers and
   moving inventory responsibilities to the appropriate layer.
2. Complete the enumerated crash-boundary matrix, including fsync, send,
   receipt and pair-commit boundaries; preserve ambiguity without replay.
3. Establish supported concurrent Dovecot/Maildir behavior and safe unattended
   stale-lock recovery. Current stale locks require offline intervention.
4. Complete normal-GC/less-compressible body measurements, other streaming
   paths, and current large/million-occurrence scale runs with peak memory.
5. Finish the per-extension capability/fallback/error matrix and rerun current
   mandatory Cyrus/JMAP and Stalwart interoperability checks.
6. Complete format-transition instructions, timeout/operational contracts and
   documentation consistency/build checks.

This is several substantive implementation and validation passes, not final
cleanup. A credible release estimate depends on the ownership/concurrency
review and the remaining failure-injection results. SCRAM and SASL security
layers remain explicitly deferred in the specification. A complete proxy
frontend is a separate product scope, not implemented by these client APIs.


Follow-up: the memory-probe additions subsequently passed all 18 normal-GC,
random-ASCII transport/size cases. See [IMAP-TODO.md](IMAP-TODO.md) for current
remaining work and measurement limitations; the original checkpoint above
records the earlier review state.


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
