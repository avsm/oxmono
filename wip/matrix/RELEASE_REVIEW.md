# First-release review — 2026-09-08

The backup-restore defects identified in this review are fixed. Recoverable
crypto transactions, cross-process profile token refresh and executable release
validation gates are also implemented. The local suite and fresh Synapse,
Dendrite and vodozemac checks pass.

**Distribution remains blocked on publishing the restructured HTTPz packages.**
The checked public repository lacks four dependencies used by this SDK; a
fresh-user installation cannot be approved from the current floating pin.
MSC4108 OAuth/MAS remains explicitly experimental until full two-role live
validation is available. Completing every newer Rust UI feature is not needed
for an initial release. Filesystem access remains the at-rest security boundary,
and reliable OCaml key zeroisation is not claimed.

The original findings below describe the reviewed baseline; each disposition
records the subsequent implementation in this working tree.

## Comparison and limits

- OCaml: `4e3ff84e464a40ba695c6fe5411f268372a1ccb4`, with the pending
  `matrix-chat` package rename and disclosure cleanup applied.
- Original Rust baseline: `523b5af53a8fd9fae9e2bc981bfb01ac86fd2890`.
- Rust checkout reviewed: `9aea12c33d3970aec19b989c4d6a485e65d6bbc6`.
- Reviewed the existing parity/audit dispositions, screened the intervening
  change list (33 commits touching 34 source files across
  SDK/base/crypto/UI/SQLite), and inspected relevant Rust/OCaml backup,
  persistence, send-queue, profile, thread and timeline paths. This is
  a focused release review, not an exhaustive new cryptographic audit or a
  symbol-for-symbol audit of every Rust crate.

`PARITY_PLAN.md` records historical completion. `PARITY_INVENTORY.md` is a
textual inventory of older commits, not evidence of current semantic parity.
`TODO.md` remains the implementation queue; the release additions are FR1–FR4.

## Fix before 0.1

### FR1 — Validate the backup before enabling a supplied recovery key

**Resolved, with CLI regression coverage. Original P1 finding:** In the reviewed
baseline, `backup_restore_run` fetches
the current version but ignores its `algorithm` and `auth_data`. It installs
the user-supplied key with that version, downloads the backup and saves the
configuration. Per-session decryption failures do not fail the overall import.

A black-box test of the actual release-built CLI against a temporary localhost
homeserver reproduced all three cases below. Each server returned an empty
backup, which also demonstrates why checking the imported count is inadequate.

| Server metadata | Original CLI result | Original saved state |
|---|---|---|
| Valid algorithm, different public key | Exit 0; `Imported 0 room keys` | Wrong public/private key associated with the server version |
| Unsupported algorithm | Exit 0; `Imported 0 room keys` | Unsupported version enabled with the supplied key |
| Missing public key in `auth_data` | Exit 0; `Imported 0 room keys` | Malformed version enabled with the supplied key |

This falsely reports successful recovery and persists an invalid upload target.
Any caller subsequently uploading through that configured encryption machine
can encrypt keys to a public key that disagrees with the version's metadata.
The latter is a consequence of the configuration and uploader code; the
black-box reproduction did not upload or overwrite any room keys.

Rust's backup activation deserializes the algorithm metadata and checks the
supplied key against it before installing a new backup configuration. See
[the reviewed Rust activation path](https://github.com/matrix-org/matrix-rust-sdk/blob/9aea12c33d3970aec19b989c4d6a485e65d6bbc6/crates/matrix-sdk/src/encryption/backups/mod.rs#L836).
OCaml already has the needed check in `Backup.current_version_state`, and
`backup_enable_run` uses it. Reuse that check in restore before mutating the
machine or fetching backed-up keys.

Acceptance: reject different keys, unsupported algorithms and malformed
metadata; leave every existing crypto file unchanged on rejection; never
issue the keys GET or a backup PUT for a rejected version. A matching empty
backup must still succeed with zero imports, and a matching populated backup
must restore a decryptable event. Keep the validated version fixed throughout
the operation.

### FR2 — Remove recovery keys from command-line arguments

**Resolved with `--recovery-key-file FILE`. Original P1 finding:** The reviewed
`recovery_key_term` required
the recovery key as a positional argument, and the generated help recommended
that form. The key therefore reaches process arguments and can enter shell
history. Password login and QR grant already use file/environment inputs.

Add a recovery-key file input or a protected prompt, and remove the positional
secret from the supported invocation and examples before establishing the
first public CLI interface. A file-only initial implementation is sufficient.
Test valid input, empty/unreadable files, argument rejection, and diagnostics
that never reproduce the key. Cover the CLI entry point, not only the decoder.

The implemented CLI validates metadata before even loading the crypto machine,
keeps the validated version fixed, and reads keys from a file. Seven real CLI
subprocess tests cover the acceptance criteria above, including decryption
using the persisted imported key. Positional keys are rejected without echo.

## Release validation gates

### FR3 — Prove installation from the distributed sources

A clean copy containing only tracked source files plus `matrix-chat.opam`,
without the `ocaml-httpz` symlink or any build products, passed
`dune build -p matrix-chat @install`. This proves the Matrix release target
builds against the currently installed dependencies.

It does not prove a fresh user's dependency resolution. The switch still has
`fetch`, `fetch-httpz`, `httpz`, `proffer` and `proffer-httpz` at version `dev`;
the local HTTPz checkout is `38af268e089a09cec9b3e6eb6b08b1845e3883f7`.
The former CI recipe pinned floating Git URLs. Fetching the public repository
on 2026-09-08 established that HEAD
[`fc8c45336b91ed31b3a3556dd27860621f06b918`](https://github.com/avsm/httpz/tree/fc8c45336b91ed31b3a3556dd27860621f06b918)
is the older OxCaml project: only `httpz.opam` exists, with none of the other
four required manifests. This is a confirmed distribution blocker, independent
of the historical scanner warning. `test/release-install.sh` now validates
those manifests from an immutable Git revision before creating a fresh OCaml
5.5 switch and installing an extracted release archive. It cannot pass until
an appropriate dependency source is published. Public opam availability was
not otherwise established by this review.

Before publishing to opam, verify the dependency releases are available and
install the actual source archive in a fresh OCaml 5.5 switch using appropriate
version bounds. For an earlier Git-tag preview, provide tested immutable pins
and exercise those exact pins in CI. Test the installed `matrix-chat.*`
libraries and both executables. This is release engineering, not a request to
expand the protocol implementation.

### FR4 — Make the advertised interoperability checks a release gate

Run the existing Synapse reference harness and the vodozemac oracle on the
release candidate. Fail the release job when a required fixture/oracle is
absent, rather than accepting its ordinary developer-mode `SKIP`. Wire an
isolated runner or retain a reproducible release-job artifact if live CI is
not yet available. Keep Dendrite's limited portability results distinct from
the full Synapse suite.

The existing U0/P2.3 requirement for a complete two-role MSC4108 login against
MAS/OIDC is still open. Either validate both roles, persistence ordering,
check-code rejection, cancellation and new-device event decryption, or mark
the QR OAuth flow experimental in the first release. The rendezvous-only
fixture does not establish that the complete login works. No additional QR
algorithm implementation was identified in this review.

## Additional work before a production claim

**Crypto transactions: implemented.** The reviewed code used only odd/even
markers, leaving no recoverable snapshot after a process died. Rust commits
crypto changes in a [database transaction](https://github.com/matrix-org/matrix-rust-sdk/blob/9aea12c33d3970aec19b989c4d6a485e65d6bbc6/crates/matrix-sdk-sqlite/src/crypto_store.rs#L1172).
The OCaml JSON backend now writes a complete 0600 redo journal before changing
any component and removes it after the final even marker. `load` completes a
pending transaction before returning a snapshot. Native file replacements sync
the parent directory; interrupted clear operations also recover. Existing
profiles need no migration. Legacy interrupted saves without a journal and
malformed/inconsistent journals still fail closed.

The restart tests end a child process after failure at each of six component
writes and seven clear deletions. A fresh handle recovers without retaining the
writer's in-memory machine. They check byte-coherent components, the original
device identity, one-time keys, Megolm ratchet position and subsequent event
decryption, secrets, trust policy and stale-writer rejection. Separate cases
cover pre-marker recovery and malformed/mismatched journals. This exercises
process death and I/O failure, not a physical power-cut test of the host's disk.

**Cross-process token refresh: implemented.** Eio client and OAuth helpers
accept `~store`; `omatrix` enables it for Matrix and OAuth sessions. A separate
cancellable lock covers the exchange, with short profile locks for reloading
and committing. A waiting process adopts an existing rotation, sync metadata is
preserved, and logout/re-login cannot be overwritten. Persistence completes
before notification callbacks; those callbacks must not write credentials.

An uncertainty marker prevents reusing a token after process death, network
failure, cancellation or an uncommitted storage failure. The conservative
policy requires re-login even for transient exchange errors. This avoids an
unsupported promise of atomicity between a remote token issuer and local disk.
Tests cover two processes, concurrent metadata, logout/re-login, cancellation
of lock waits/exchanges, process death and persisted OAuth adapter reuse.

**Auth follow-up: both findings fixed.** Fresh login previously left a malformed
refresh marker in place, making its instruction to log in again ineffective.
`Profile_store.save_login`, used by all CLI login flows, now waits for active
refresh and durably persists new credentials before removing any old marker.
Ordinary metadata writes and failed credential writes cannot retire it. OAuth
discovery/validation previously ran after marker creation, unnecessarily
blocking a token that had never been exchanged. Preparation now precedes the
marker for both reactive and proactive refresh; token exchange still starts
only after the marker is durable and retains the conservative error policy.

**Encrypted local storage: explicitly deferred.** Rust offers optional encrypted
SQLite stores. This release retains plaintext tokens, private keys, session
state and journals, with no local unlock prompt or keyring integration. Newly
created profile directories/files use 0700/0600; existing directory permissions
are unchanged. Filesystem access controls, including for copies/backups, remain
the security boundary. README documents it and TODO records the future key
management, encryption and migration requirements. Reliable secret zeroisation
also remains limited by OCaml's GC. Neither is a completed production feature.

## Rust changes that do not block 0.1

| Rust behavior | OCaml assessment | Disposition |
|---|---|---|
| Pinned-event removal refresh (`9aea12c33`) | `Pinned_events.refresh` filters loaded IDs against the new set and reconciles the entire result; it does not use Rust's faulty subset shortcut. | No equivalent missing fix identified. |
| Deduplicated thread catch-up tokens (`37a0ae4f1`, `d872bba8f`) | `Thread_subscriptions.queue_catchup_token` already deduplicates before saving. | Already implemented. |
| Queue timeline redactions (`7a2a8996c`) | `Room_timeline.redact` already calls `Send_queue.send_redaction`. | Already implemented. |
| Filtering custom timeline events (`0fa94b835`) | The existing `Presentation.t -> bool` filter can express custom state/message filtering, with custom-state coverage. | No new enum hierarchy required. |
| Clearing remote timeline bookkeeping (`40389ad88`) | OCaml cache/forget/discard paths clear their own projections and relation indexes; they do not use Rust's separate remote-event index. | No direct patch to port; retain lifecycle tests. |
| Immediate own-profile updates and default profile subscription (`3cec059ca`, `9b2d01524`) | Wire codecs, sync fold and observers exist, but profile setters only issue HTTP requests and do not update the local projection immediately. | UI follow-up; add a service-level local update if immediate feedback is promised. |
| Find room-list preview candidates by background back-pagination (`7c3c908d5`, `38ffc4eb0`) | `Room_list.latest_of_info` uses resident events and the saved latest event; it can show no preview if neither is suitable. | Optional UI follow-up, not a bot/SDK release blocker. |
| Preserve thread/reply relations while editing unsent text (`7def4e660`) | No general pending-text edit API exists; queued attachment-caption editing and edits of server event IDs are separate APIs. | Add pending-text editing only with its relation-preservation tests; do not claim this Rust UI feature already exists. |
| X.509 identity/signing updates | X.509 remains outside the declared scope. | Keep deferred. |

The existing lazy-cache cancellation/concurrent-hydration tests, cache memory
budget behavior, wildcard verification cancellation and optional pooled
transport validation remain worthwhile follow-ups. Widgets, MatrixRTC,
IndexedDB, local encrypted search, experimental crypto/event formats, and QR
image rendering/scanning should stay outside the first release unless a
consumer specifically requires them.

## Evidence collected

- Clean source-copy release build: passed, using the existing development
  dependency switch; no sibling source checkout included.
- The package rename's build, examples, CLI smoke checks and full local test
  alias had already passed in this session; OAuth loopback tests required
  execution outside the socket-restricted sandbox.
- Explicit vodozemac run during this review: 24 Olm/Megolm tests passed,
  including all seven live oracle cases, with no oracle skips.
- Actual release CLI backup-restore probe: all three invalid metadata/key
  scenarios reproduced exit 0 and persisted an invalid backup configuration.
  Synthetic credentials and temporary profiles only; no real server touched.
- The initial review did not rerun Synapse/Dendrite or MAS/OIDC. Their
  historical results are not presented as a new release-candidate validation.

Local reproduction and logs are retained in
`/tmp/matrix-chat-release-review-glm0o6m0/`, including
`probe_backup_restore.py`, `backup-restore-probe.jsonl`, `release-build.log`
and `olm-interop.log`.

## Implementation validation — 2026-09-08

- Before the auth follow-up below, the local suite had 64 executables, 1,446
  cases/checks and three source guards.
- `test/release-check.sh` passed with the required locked vodozemac oracle,
  Synapse 60/60 in 109.840s and the separate Dendrite smoke profile 5/5 in 1.573s.
  Both temporary homeservers and their data were removed by the runner.
- Release-run evidence is in `/tmp/matrix-chat-release-check.8qBro8ju/`.
- Full OAuth/MAS two-role validation remains open; CLI help now marks MSC4108
  QR login experimental. Dependency publication remains FR3, not a passed gate.
- The final clean source archive built and installed all seven `matrix-chat.*`
  libraries; a separate installed-library consumer linked the unchanged Matrix
  modules, and both installed executable help commands passed. Evidence and the
  source archive are in `/tmp/matrix-chat-release-final-i3s08joa/`. This used the
  existing dependency switch and does not substitute for fresh resolution.
- Required missing-oracle and missing-homeserver checks both exited nonzero.
  The fresh-install preflight rejected the fetched public HTTPz revision with
  `Published revision lacks fetch.opam`, before creating an opam switch. Its
  log is in `/tmp/matrix-chat-release-install.6yqJM04t/`.

## Auth follow-up validation — 2026-09-08

- The new actual-CLI login test reproduced the malformed-marker defect before
  the fix. Four reactive/proactive OAuth discovery/validation retry cases also
  failed before the fix because preparation had left a marker behind.
- All 11 added cases now pass: the CLI login case, six OAuth cases (including
  two that preserve conservative exchange failure handling), and four profile
  cases covering marker recovery, failed writes, concurrent refresh/login and
  cancellation during preparation. The affected full suites pass 8 CLI, 87
  OAuth and 24 profile cases.
- The local build, install manifest, libraries, executables, examples, test
  alias and API documentation passed with the vodozemac oracle required. The
  suite now contains 64 executables, 1,457 cases/checks and three source guards.
  Formatting and whitespace checks passed. The existing unresolved
  `Invalid_argument` documentation reference still emits a warning.
- The build/test log is `/tmp/matrix-auth-followup-tests.log`; before-fix
  reproductions are `/tmp/matrix-auth-login-before.log` and
  `/tmp/matrix-auth-preparation-before.log`.
- Synapse/Dendrite and the clean source archive were not rerun for this auth
  follow-up; the release-run and archive evidence above predates these changes.
  Encrypted local storage remains deferred, as documented above and in README.
