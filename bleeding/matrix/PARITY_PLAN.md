# ocaml-matrix vs matrix-rust-sdk: gap analysis and parity plan

> **First-release review, 2026-09-08.** [RELEASE_REVIEW.md](RELEASE_REVIEW.md)
> compares the current tree with Rust `9aea12c33`. The backup-restore fixes,
> recoverable crypto transactions and cross-process token refresh are now
> implemented. Dependency publication and release validation remain distinct gates. Historical parity completion
> below is not release approval. See TODO FR1–FR4 for the current checklist.

> **2026-09-05.** The module names below predate the restructuring that split
> `matrix-chat.cli` out of `matrix-chat.client`, moved the driver loops into
> `matrix-chat.eio`, and renamed `Sync_service` to `Base_client`, `Session.Store` to
> `Profile_store`, `Read_receipts` to `Read_state` and `Matrix_ui.Timeline` to
> `Room_timeline`, among others. They are left as they were written.
> `STATUS.md` and `README.md` name the modules the tree has now.

## Status: original port and bounded follow-up complete

Every phase in the original plan below is done. Commit range `1ebb09d..HEAD`:

| Commit(s) | Phase / work |
|---|---|
| `1ebb09d` | this plan |
| `619d85f` | Phase 1, MSC4186 simplified sliding sync |
| `0e23e6f` `b2a7083` | Phase 1, CS API coverage (§B) |
| `b2294e0` `140fa14` | Phase 1, Olm/Megolm libolm-compatible rewrite (§A) |
| `601f969` `975260c` | Phase 1, E2EE server API, secret storage, backup keys (§A) |
| `8d012cf` | the encoder sweep (26 dead request codecs) |
| `97e6a35` `e7002ef` | Phase 2, OAuth 2.0 next-gen auth (§D) |
| `ada0a9d` `19bacdb` | Phase 2, SAS/QR verification completion |
| `5cb9618` `30bf10d` | Phase 2, the encryption service |
| `20fd164` `95ed6f2` | Phase 2, base client state (§C) |
| `d6e4623` `90deecf` | structural JSON construction (the UIAA splicing bug) |
| `aa77f78` | Phase 3, encryption wired into sync and send |
| `5656732` | Phase 3 — `Session.Pickle` encodes; duplicate codecs deleted |
| `d4f68e6` | Phase 3, `omatrix` encrypts, verifies and backs up |
| *(this commit)* | Phase 3, documentation reconciliation |

### Verification at original completion (historical)

From a clean `_build` on the final tree:

- `dune build --root .` — clean, no warnings.
- `dune runtest --root . test` — green. **18 executables, 561 checks**:
  `test_matrix_proto` 79, `test_session` 15, `test_matrix_client` 42,
  `test_cs_api` 54, `test_encoders` 115, `test_json_safety` 29,
  `test_e2ee_api` 46, `test_olm` 19, `test_verification` 28, `test_oauth` 29,
  `test_encryption` 10, `test_base_client` 25, `test_sliding_sync` 25,
  `test_e2ee_integration` 3, `test_matrix_ui` 21, `test_room_list` 14, `test_text` 4, `test_lb64` 3.
- **Olm and Megolm interoperate with vodozemac 0.9 in both directions**, over
  `test/vodozemac-oracle` (7 interop cases, plus recorded fixtures that run
  without cargo).
- `matrix-chat.client` — 54 modules, every one with an `.mli`.
  `matrix-chat.eio` — 40 modules, 37 of them wrappers; only `http.mli` exists.
  `matrix-chat.ui` — 9 modules, every one with an `.mli`.
- `dune build --root . @doc` — 228 warnings, all unresolved cross-package
  roots the narrow `@doc` universe cannot see (217 × `Eio.Io`,
  6 × `Invalid_argument`, 4 × `Fetch`/`Fetch_httpz`, 1 × `Jsont`).
- CS API: every area in §B is implemented except `peeking`, `retention` and
  `rendezvous` (MSC4108); typed `/admin/whois` is now covered by `Admin` and
  requires server-admin authorization.

### Gaps recorded at original completion (historical, 2026-09-01)

This list explains what “complete” meant for the original, deliberately narrow
scope. It is not the current work queue: the live status is in `STATUS.md`, and
the actionable list is in `TODO.md`. In particular, a Synapse integration suite
and the central `matrix-chat.ui` path landed after this snapshot.

Honestly, and by choice except where noted:

- **At this snapshot `matrix-chat.ui` covered only the central `matrix-sdk-ui`
  path** — bounded observables, safe presentation events, a shared event cache,
  timeline aggregation/pagination/local echoes, a filtered room list and Eio
  runtime wiring. Directory/search, room details, spaces, notifications,
  thread lists and the durable bounded thread projection have since landed.
- **The UI event cache has memory and direct SQLite backends.** The base SDK
  state remains one JSON file per profile; there is no IndexedDB or store
  encryption. SQLite defaults to ciphertext-only persistence.
- **QR login (MSC4108) is absent**, and with it the rendezvous endpoints and
  the ECIES secure channel. The separate RFC 8628 device grant is now driven by
  both the library and `omatrix`.
- **At the initial snapshot history sharing was absent.** The pure MSC4268
  bundle codec/build/import and durable shared-history metadata have since
  landed. `Matrix_ui.Runtime.join` now captures an inviter from the current
  invited-room state before `/join`, records the acceptance gate only after a
  successful join, persists it, prunes future or at-least-24-hour-old records
  at startup, and reconciles every record against current room state on sync.
  Trust-gated transport and join/recovery orchestration have since landed.
- **At the initial snapshot a completed SAS did not sign or publish anything**;
  the current `Verification_service` publishes with a supplied private
  cross-signing capability. Credential-driven SSSS/recovery import has since
  landed; automatic credential discovery remains explicitly deferred.
- **At this snapshot sliding sync did not feed the base client**; its `State`
  was a separate, simpler room-list stand-in. The common fold, transactional
  migration and adaptive classic fallback have since landed.
- **At this snapshot dehydrated devices moved bytes but did not dehydrate** —
  there was no Olm account pickling into `device_data`. The manager and
  pickle/upload/rehydrate/rotation lifecycle have since landed.
- **No QR image rendering or scanning** (`qrc` is not a dependency), and no
  widgets, content scanner, search index or MatrixRTC beyond the event types.
- **At this snapshot nothing had been run against a real homeserver.** Since
  then, the current `test/integration` reference has grown to 60 Synapse
  scenarios, and the bounded Dendrite v0.15.2 portable-room core has also been
  exercised. Its named five-case core passes 5/5; a separate peeking probe is
  explicitly rejected with `M_GUEST_ACCESS_FORBIDDEN`, while optional threaded
  receipts time out, MSC2246 preallocation is rejected despite advertised
  MSC3916, simplified sliding sync is unavailable and federation is disabled.

The full list, with reasoning, is in `TODO.md`.

### Follow-up audit against current matrix-rust-sdk (2026-09-03)

The comparison is pinned so another agent can reproduce it:

- ocaml-matrix: `2c7bb435348043ce2ddb5a957497cac5ccd4633a`;
- `../matrix-rust-sdk`: `523b5af53a8fd9fae9e2bc981bfb01ac86fd2890`
  (`matrix-sdk` 0.18 development tree).

The original endpoint and crypto port is real, but “feature-complete” must not
be read as API or behavioural parity with that Rust revision. The re-audit
found these additional work packages. `ROADMAP.md` orders them and `TODO.md`
contains the file-level handoff and acceptance tests.

| Priority | Newly explicit delta | Rust reference area | Routed to |
|---|---|---|---|
| P0 | Persist the full room-state projection: create/version/type, predecessor/successor/tombstone, join/history/guest rules, retention, pins, aliases and service members | `matrix-sdk-base/src/room/room_info.rs` | 1.B / TODO “Sync and base client” |
| P0 | Derive sender identity and trust from cross-signing, preserve verification violations, classify unable-to-decrypt causes and retry late keys | `matrix-sdk-crypto/src/olm/group_sessions/sender_data.rs`; `matrix-sdk-ui/src/unable_to_decrypt_hook.rs` | 1.C–1.D / E2EE TODOs |
| P0 | Encrypt and authenticate attachment bytes; the current code models only the `file` JSON shape | `matrix-sdk-crypto/src/file_encryption/attachments.rs` | 2.I / TODO “Media” |
| P0 | Apply classic versus sliding-sync missing-OTK-count semantics and expose only negotiated SAS representations | `matrix-sdk-crypto/src/{olm/account.rs,machine/mod.rs,verification/sas/mod.rs}` | 1.C–1.D / TODO P0.4–P0.5 |
| P1 | Grow the persistent send queue into a dependency graph for uploads, thumbnails, edits/reactions/redactions and abort races | `matrix-sdk/src/send_queue/` | 2.I / TODO “Sync and base client” |
| P1 | Add the persistent per-(room,root) thread cache: ordered root/replies, pagination metadata and per-thread receipts/unread subscriptions with forget cleanup | `matrix-sdk/src/client/thread_subscriptions.rs`; `matrix-sdk/src/event_cache/caches/thread/` | 2.H, 2.K / TODO “Sliding sync” and “UI models” |
| P1 | Implement high-level retention, dehydration/recovery, room-key history/file migration, MSC4108 QR login and the pinned Rust MSC4388 codec/probe boundary (**complete; only P2.3's external OAuth/MAS validation remains**) | `matrix-sdk/src/room/privacy_settings.rs`; `matrix-sdk/src/encryption/{dehydrated_devices.rs,recovery/}`; `matrix-sdk-crypto/src/{types/qr_login/,file_encryption/key_export.rs}` | 1.C, 2.G, 2.J |
| P1 | Store sliding-sync profiles globally and per room; keep MSC4262 distinct from optional MSC4426 user/call status | `matrix-sdk/src/sliding_sync/`; `matrix-sdk-base/src/store/` | 2.H |
| P2 | Fill high-level room/UI services: previews, knock moderation, composer drafts, edit history, notification settings, live locations, paginators and room directory | `matrix-sdk/src/room/`; `matrix-sdk-ui/src/` | 2.K |
| Deferred | IndexedDB, widgets, MatrixRTC, content scanner/search index, MSC3956/MSC4274 event/gallery work, X.509 identity, encrypted state/push secrets, Olm/Megolm v2 and Element-specific recent-item stores | feature-gated or platform-specific Rust crates/features | “Deferred from matrix-rust-sdk” |

### Follow-up implementation update (through 2026-09-05)

- **Complete:** the P0 room-state projection, classic/sliding missing-OTK
  distinction and negotiated SAS presentation. State is versioned, migration
  safe and authoritative full-state snapshots remove stale entries.
- **Identity core and UTD lifecycle landed:** validated cross-signing/device
  identity and rotation history, explicit trust requirements, authenticated
  room-key sender metadata, structured UTD causes, UI late-key request/dedup
  and in-place re-decryption. SAS now publishes own-device or other-user
  signatures when given matching private keys, and a public durable/grace-period
  UTD hook reports a real Synapse backup late-key transition. The UI now
  refreshes room-scoped identity warnings after each sync and distinguishes a
  durable unverified-key TOFU pin violation from the verification latch;
  `Secrets.open_secret_store` and `import_cross_signing` now validate and
  atomically load matching SSSS/recovery seeds after a fresh own-user query.
  The credential remains application-supplied. The core returns private
  material to its caller; the completed QR CLI path publishes it only into a
  freshly encrypted remote SSSS store.
- **Attachment transport, event graph and cache policy complete:** Matrix
  AES-CTR/JWK/SHA-256 attachment encryption, verified buffered helpers and
  authenticated streaming upload/download are covered through an encrypted
  room against Synapse. Generic queue dependency persistence/resolution and
  recursive cancellation have landed; an in-flight cancellation also persists
  one optional-reason compensating redaction across either restart boundary.
  Typed original/thumbnail nodes, monotonic progress and clear/encrypted result
  substitution now survive restart; `send_attachment` persists the full graph
  atomically and the event alone is visible. Pending-caption mutation, the
  deterministic media cache/cleanup policy and replaceable network fetcher are
  complete; the cache remains ciphertext-only for encrypted nodes.
- **Retention complete:** typed stable state with a legacy read fallback,
  validated MSC1763 server configuration, per-room/default precedence and
  independent clamps survive store reload. An unsupported configuration
  endpoint returns no effective policy, matching the pinned Rust behaviour;
  Synapse covers that branch live.
- **Refresh, Olm repair and portable-key foundations landed:** expiry-aware
  proactive refresh shares the reactive lock, persists the login deadline,
  detaches Eio completion from the initiating request and safely hands the
  updated session/deadline to a reentrant persistence hook. A failed Olm decrypt over a
  session strictly older than one hour force-claims a replacement and queues a
  persistent encrypted `m.dummy`, including Rust's rollback behavior. The
  portable room-key armour/KDF/cipher/MAC layer is wired to machine-store
  import/export and passes pinned fixtures in both Rust/OCaml directions. Pure
  MSC4268 bundle build/import is present, and withholding facts retain their
  authenticated sender provenance across restart without changing bundle JSON.
  The durable inviter gate is now wired into `Runtime.join`, with strict
  24-hour/startup expiry and full current-membership cleanup. Network bundle
  receive/transport, missing-backup-key fetch, trust re-query, retry and
  malformed/404 lifecycle handling are complete in the trusted pre-join
  history workflow.
- **Verification:** a fresh build of the repository-owned library, executable,
  example and test aliases and `dune runtest test` pass with 61 hermetic
  executables and 1,352 test cases. With the repository's pinned Synapse
  v1.159.0 container on `127.0.0.1:8008`, `dune build @integration` passes all
  60 live scenarios in 112.910s. The Tangled workflow remains non-live because
  Docker/socket/network/path requirements are not established for its runner.

The audit corrected an earlier status overclaim: decoding encrypted-file JSON
was not encrypted-media support. The byte encryption, hashing, authenticated
streaming transport and persistent application send graph/cache now exist; the
replaceable network media fetcher is also complete.

### Current bounded follow-up (through 2026-09-05)

This bounded parity wave also closes these narrower behavioural gaps without
changing the original historical scope:

- **OAuth device authorization and reactive refresh:** RFC 8628
  request/response codecs, the Eio bounded monotonic pending/slow-down poll
  loop, and `omatrix login --device-code` are implemented. Authenticated
  requests now opt into serialized, exact-token-attributed `M_UNKNOWN_TOKEN`
  refresh with one replay for buffered and GET-stream requests; one-shot POST
  streams and unauthenticated requests are never replayed. OAuth refresh uses
  fresh metadata, refresh tokens are retained, and `omatrix` persists rotated
  Matrix and OAuth credentials. Expiry-aware proactive refresh is implemented
  as an opt-in client policy and shares the reactive refresh serialization.
- **Backup and withholding safety:** `omatrix backup enable` checks the current
  server version before generating keys, is idempotent for a matching local
  version, and refuses divergent or missing local keys. New and repaired backup
  auth data carries the matching cross-signing master-key signature followed by
  the current device signature.
  Outbound `m.room_key.withheld` notices and their acknowledgements now persist
  with the original transaction/body, so retries remain idempotent and sent
  notices stay suppressed after restart.
- **Verification request selection:** the first `ready` emits one directed
  `m.accepted` cancel to each other originally requested concrete device, with
  duplicate/later ready events suppressed; a received cancel is likewise
  relayed once to the other concrete recipients. Wildcard `To_device.All`
  requests intentionally have no fan-out because “all except this device” is
  not representable safely.
- **Media and sync CLI:** media download, thumbnail, config, URL-preview and
  effectful MXC URL requests select authenticated Matrix 1.11/MSC3916 or
  legacy v3 routes from `/versions`, retaining unknown config/preview fields.
  `omatrix sync` now runs
  `Sync_service.run` under a nested switch, stops at positive `--count`, and
  cancels/saves cleanly on SIGINT/SIGTERM.
- **Application-service wave (2026-09-04):** room routing/permalink/DM/role and
  invite projections, network and cache edit histories, queued replies and
  static locations, notification settings and one-event notification lookup,
  and the bidirectional room paginator are implemented. OAuth metadata has a
  Cache-Control-aware per-client cache, and `omatrix` exposes the complete Eio
  HTTP policy.
  Paged room-directory and server-event searches, a deterministic mutual-link
  space DAG, immutable lazy-loaded room-member details and a raw thread-root
  paginator are now implemented too. The rich thread list consumes persisted
  `ThreadInfo`, shares detached root/latest events through the Runtime cache and
  owns its lifecycle. A bounded durable per-(room,root) projection now joins
  ordered root/reply identity, pagination metadata, receipt/unread
  subscriptions and forget cleanup, retaining each root and at most 255
  replies. It deliberately reuses the shared EventCache/EventStore rather than
  claiming current Rust's independent persisted `LinkedChunk`/lazy-loading
  topology.
- **Receipt, verification, profile and dehydration continuity closeout:**
  high-level timeline receipt controls are complete; encrypted in-room SAS now
  validates the room relation, target user, device, timestamp and exact event
  allowlist, ignores own echoes, and routes same-user competing-request
  cancellations through each original room or device set. Display names,
  avatars and stable/MSC4133 custom profile fields have explicit JSON-null
  clear operations; MSC4262 sync profile updates retain their existing
  update/null/drop semantics. Live dehydration coverage now drains peer
  room-key material, rehydrates it and demonstrates encrypted continuity and
  decryption.
- **Boundary/foundation wave (2026-09-04):** MSC4388 payloads round-trip the
  pinned Rust vector, discover the unauthenticated rendezvous capability and
  stop at Rust's typed unsupported secure-channel result. MSC4108 now has
  strict codecs, conditional rendezvous, vodozemac-compatible ECIES, a typed
  two-party secure-handshake core, OAuth/login approval/auth messages, secret
  handover and complete Eio flow. `omatrix qr login` and `omatrix qr grant`
  provide the two textual Base64 roles with mandatory check-code confirmation,
  progress, bounded cancellation and encrypted SSSS seed/backup persistence.
  The authenticated session is saved before encryption state and subsequent
  network or secret work. As in
  Rust's FFI, raster rendering/scanning belongs to the caller; the isolated
  flag-enabled fixture validates unauthenticated rendezvous
  create/PUT/GET/delete, two-party ECIES/bidirectional encrypted messages and
  cleanup/cancel; it is not MAS/OIDC. Full OAuth/MAS two-role validation still
  needs a deployment that supplies OAuth and rendezvous.
  MSC4262 profile requests and arbitrary update/null/drop responses feed the
  common state. Public standalone sliding state is retired, old slots migrate
  transactionally through `Base_client`, and `Adaptive_sync` supplies the
  project-specific classic-sync fallback.
- **Continuity/OAuth wave (2026-09-04):** SSSS key opening accepts the Rust
  PBKDF2-first/Base58-fallback credential rules, optional well-known secrets
  are imported only after a fresh validated own-user `/keys/query`, and the
  cross-signing import is atomic on stale or malformed seeds. A full
  self-signing import validates UTF-8, re-queries and marks the own device
  verified; the core does not silently persist private material. The Eio
  `Verification_service` can consume this store handle. Default OAuth browser
  registration now sends the exact already-bound loopback URI, while caller
  metadata remains untouched. Reactive refresh and CLI persistence of rotated
  credentials are implemented. Expiry-aware proactive refresh has injected
  clock/window semantics; login expiry survives the CLI profile; Eio refresh
  completion is runtime-owned; OAuth `invalid_grant` emits typed invalidation;
  and metadata caching honors response `Cache-Control`. Automatic credential
  discovery and general local encrypted key persistence remain explicitly
  deferred. MSC4108's
  application/OAuth/handover/Eio flow and both textual CLI roles are complete;
  imported QR secrets are placed in a fresh encrypted remote SSSS store. Raster
  presentation/scanning is an application boundary, while a live QR scenario
  still requires an OAuth/MAS/rendezvous-capable deployment. Network
  stampede protection is per client rather than cross-process. Persistence
  hooks run after the shared refresh promise is resolved, so reentrant
  authenticated calls do not deadlock.
- **Persistence/crypto scheduling wave (2026-09-04):** session, base and crypto
  snapshots share the profile advisory lock. Exact-byte fingerprints reject
  stale base writers; even/odd crypto generations reject stale handles and
  fail closed across interrupted multi-file saves. Backup uploads are
  deterministic 100-session batches, serialized per driver and checkpointed
  after every successful request. Olm lookup follows decrypt recency while a
  separate successful-use LRU retains four sessions per peer; both timestamps
  survive migration and restart. Capability-resolved MXC URLs now reuse the
  media endpoint decision. Stale Olm sessions now use the pinned Rust strict
  one-hour force-claim plus persistent `m.dummy` repair. Crypto's component
  files remain non-atomic as a group. General age-based Olm expiry, a finite
  one-time-key retry cap, paged whole-backup restore and backup ETag
  coordination would all be optional hardening: pinned and current Rust also
  have no general expiry or finite cap and use the same unpaginated whole-backup
  GET. Targeted room/session restore already exists; backup operations are
  version-addressed and disable on a missing or changed active version.

### Current implementation closeout (2026-09-05)

The implementation tracked by this follow-up was baselined at ocaml-matrix
`948229e367f16abcc7b609310585904cdff0983d`, still compared with pinned Rust
`523b5af53a8fd9fae9e2bc981bfb01ac86fd2890`; subsequent fixes are recorded in
`TODO.md`, and adjacent Rust HEAD
`f4b9512df23332fce1bd26037ef7a2387af2ced2` was consulted only for separately
labelled advice. `TODO.md` is the executable handoff.

- **Validated:** 61 hermetic executables and 1,352 test cases pass. The pinned
  Synapse 1.159.0 reference passes 60/60 scenarios in 112.910s. Dendrite
  v0.15.2 passes its named 5/5 portable core cases; a separate peeking probe is
  explicitly rejected with `M_GUEST_ACCESS_FORBIDDEN`, and the documented
  capability failures do not enlarge the Synapse count.
- **Peeking and delayed events are implemented:** the legacy peeking
  `initialSync`/`events` family is exposed with strict initial-sync event
  validation and lossless stream chunks, while MSC4140 has both the
  Synapse-compatible legacy send/state/update/list family and the current
  unified scheduling, direct GET/list and path-action update family. The two
  route generations remain explicit and are never probed by replaying a
  mutation or silently falling back after an ambiguous result.
- **QR delivery complete at the SDK/CLI boundary:** the MSC4108 creation route
  is distinct from MSC4388 discovery, both textual `omatrix` roles are wired,
  and the new device persists its OAuth session at authentication before later
  failure points. Imported identity and optional backup secrets go to encrypted
  SSSS, never the unencrypted local crypto snapshot. Raster image handling is
  caller-owned exactly as at Rust's raw-byte boundary. The isolated
  flag-enabled fixture validates unauthenticated rendezvous create/PUT/GET/
  delete, two-party ECIES/bidirectional encrypted messages and cleanup/cancel,
  but is not MAS/OIDC. Only full OAuth/MAS two-role end-to-end validation on an
  OAuth/rendezvous-capable deployment remains; Synapse's isolated rendezvous
  fixture supplies no MAS/OIDC service. MSC4388 remains a separate typed
  unsupported channel.
- **Thread/sliding work complete within the declared boundary:** the durable
  shared-cache thread projection, retaining each root and at most 255 replies,
  is covered across restart/stale/forget races;
  public standalone sliding state is retired, legacy slots migrate
  transactionally, and `Adaptive_sync` implements automatic classic fallback
  as an OCaml project enhancement. Neither is an invitation to claim current
  Rust's independent lazy-loaded thread `LinkedChunk` implementation.
- **False parity gaps retired:** pinned and current Rust have no general
  age-based Olm expiry, no finite OTK/claim retry limit, and no paginated
  whole-backup restore. OCaml matches those boundaries and also exposes targeted
  room/session restore. Treat changes there, a whole-exchange HTTP deadline,
  backup ETags, encrypted local stores and other explicitly deferred surfaces
  as new design work, not unfinished pinned parity.

---

Branch `port/opam-deps`, on top of the opam-deps port (`PORT_PLAN.md`).
Reference trees (read-only, in the session scratchpad — paths given to agents):

- `../matrix-rust-sdk` — the Rust SDK (crates `matrix-sdk`, `-base`, `-crypto`, `-ui`).
- `$SCRATCH/vodozemac` — the Rust Olm/Megolm implementation the SDK uses.
- `$SCRATCH/olm/docs/{olm,megolm,signing}.md` — the libolm wire-format specs.
- `$SCRATCH/matrix-spec` — the Matrix spec (`content/client-server-api/modules/*.md`,
  `data/api/client-server/*.yaml`).
- `$SCRATCH/ruma/crates/ruma-client-api/src` — every CS endpoint, one file each.

## Scope

The original scope targeted **protocol client** (`matrix-sdk` endpoint coverage
+ `matrix-sdk-base` state handling) and **E2EE** (`matrix-sdk-crypto`). Since
then this tree has adopted a `matrix-chat.ui` layer and SQLite event-cache
persistence, so the follow-up audit includes correctness and continuity of the
UI surfaces already claimed. It does not make every Rust crate or experimental
feature a release blocker. Platform-specific backends, widgets, MatrixRTC,
content scanning/search indexing and opt-in crypto/identity experiments remain
explicitly deferred in `TODO.md`.

## Original gap inventory (historical)

The entries below are the inputs to the completed 2026-09-01 port. They
describe the tree before those phase commits, not the current implementation.

### A. E2EE — blocking

| Gap | Rust reference | Evidence in this tree |
|---|---|---|
| **Olm wire format is home-grown**: no version byte, no pre-key message, `ratchet_key|index|ciphertext` framing; X3DH ephemeral key never leaves the sender | `vodozemac/src/olm/{messages,session}`; `olm.md` | `olm.ml` `Session.encrypt/create_outbound` (earlier smoke test) |
| **Megolm framing/export not spec-shaped**; `export_session_key` exports at current index, import assumes 0 | `vodozemac/src/megolm/{message,session_keys}.rs`; `megolm.md` | `olm.ml` Megolm |
| No interop tests against a real implementation | vodozemac test suite | none |
| **E2EE not wired into sync or send**: to-device events, `device_lists`, `device_one_time_keys_count` are parsed then ignored; nothing encrypts on send; `m.room.encrypted` never decrypted | `matrix-sdk-crypto/src/machine`, `matrix-sdk/src/encryption`, `room/mod.rs::send` | `sync.ml`, `messages.ml` have zero references to `Olm` |
| No `/sendToDevice` | `to_device::send_event_to_device` | no path in `lib/` |
| No `/keys/device_signing/upload`, `/keys/signatures/upload` | `keys::upload_signing_keys`, `upload_signatures` | cross-signing keys generated, never published |
| No key backup server API (`/room_keys/version`, `/room_keys/keys`) | `matrix-sdk-crypto/src/backups`, `matrix-sdk/src/encryption/backups` | `backup.ml` is crypto only |
| No secret storage (SSSS, `m.secret_storage.v1.aes-hmac-sha2`) / recovery | `secret_storage.rs`, `encryption/recovery` | absent |
| No key gossiping (`m.room_key_request`, `m.forwarded_room_key`) | `gossiping/` | absent |
| Verification incomplete: SAS emoji table 20/64, no MAC stage, no full state machine; QR structures without encode/decode; `verify_cross_signing_signature` checks presence not validity | `verification/{sas,qrcode.rs,machine.rs}`, `matrix-sdk-qrcode` | `verification.ml` |
| Recovery key is base64 not base58; `parse_recovered_key` stub | `backups/keys/recovery_key.rs` | `backup.ml` |
| No dehydrated devices (MSC3814) | `dehydrated_devices.rs` | absent |

### B. Client-Server API coverage (non-E2EE)

Missing entirely (ruma module → endpoint):
`discovery` (`/versions`, `/capabilities`, `/.well-known/matrix/client`),
`to_device`, `backup`, `search`, `user_directory`, `threads` (`/rooms/{id}/threads`),
`reporting` (`/rooms/{id}/report[/{eventId}]`, `/users/{id}/report`),
`tag` (`/user/{u}/rooms/{r}/tags`), `openid`, `thirdparty`, `alias`
(`/rooms/{id}/aliases`; `PUT/DELETE /directory/room/{alias}` partially),
`room::upgrade_room`, `membership::joined_members`, `room::get_event_by_timestamp`,
`session::get_login_token` (MSC3882), `account::check_registration_token`/
`register_available`/`request_registration_token_via_{email,msisdn}`,
`account::request_openid_token`, `dehydrated_device`, `delayed_events`
(MSC4140), `peeking`, `retention`, `profile` extended fields (MSC4133),
`notifications` (`/notifications`), `rendezvous` (MSC4108 QR login),
`oauth_server_metadata` (`/auth_metadata`, next-gen auth).

### C. Sync and state

- Sliding sync implements dead MSC3575; Rust speaks **MSC4186 simplified sliding sync**
  (`/_matrix/client/unstable/org.matrix.simplified_msc3575/sync`).
- `Store` (1001 lines), `Send_queue`, `Timeline` are unreferenced — no
  `matrix-sdk-base`-style room state maintenance across syncs, no
  offline send queue actually sending.
- No local push-rule evaluation (`notification_settings`, ruma `push` evaluation) —
  unread/highlight counts can only come from the server.
- No read-receipt computation (`read_receipts.rs`).

### D. Authentication

- Only legacy password/token login. Rust has OAuth 2.0 (MSC3861 / Matrix 1.15):
  `/auth_metadata` discovery, dynamic client registration, PKCE auth-code flow,
  refresh, revoke, QR login (MSC4108).

## Original dependency assessment (historical)

**None required.** Everything below is built from deps already in
`dune-project`: base58 (≈30 lines, inline), PBKDF2 (`kdf.pbkdf`), AES-CTR and
AES-CBC (`mirage-crypto`), HMAC-SHA256 (`digestif`), HKDF (`kdf.hkdf`), Ed25519 /
X25519 (`mirage-crypto-ec`), PKCE (`digestif` + `base64`).

Optional, **not added** unless you say so: `qrc` (opam, dbuenzli) to render a QR
code in the `omatrix verify` terminal UI. Without it we print the QR payload
as base64 and support scanning-side only.

Test-time only, not an opam dep: a Rust `cargo` project under
`test/vodozemac-oracle/` (vodozemac from crates.io) used to generate Olm/Megolm
interop vectors; the OCaml test skips when the binary is absent.

## Original phases (completed)

Each agent: own worktree on this branch, disjoint files, commit, report.
Every new module gets an `.mli`, a `matrix_eio` wrapper where it does I/O,
and `fetch.mock` tests.

### Phase 1 (parallel)

**Olm/Megolm interop rewrite.** `olm.ml/.mli`, `session.ml` `Pickle`
section only, new `test/test_olm.ml`, new `test/vodozemac-oracle/` (Rust).
Implement libolm wire formats exactly (`olm.md`/`megolm.md`, cross-check
vodozemac): pre-key message v3, normal message v3 (protobuf-style tags, 8-byte
MAC), X3DH, double ratchet, Megolm message v3 (index, ciphertext, MAC, Ed25519
sig), `SessionKey` export (v2) and `ExportedSessionKey` (v1), `m.room_key`
content. Fix the two known bugs. Build an oracle binary that reads JSON commands
on stdin (`create_account`, `create_outbound_session`, `encrypt`, `decrypt`,
`megolm_*`) so the OCaml test proves both directions.

**E2EE server API, secret storage, backup keys.** New
`to_device.ml`, `room_keys.ml`, `secret_storage.ml`; `keys.ml` (+ signing
uploads); `backup.ml` (base58 recovery key, `parse_recovered_key`,
auth-data signing); `dehydrated_device.ml`; `matrix_eio` wrappers; tests.

**CS API coverage.** New `server.ml` (versions/capabilities/
well-known), `search.ml`, `notifications.ml`, `threads.ml`, `report.ml`,
`tags.ml`, `openid.ml`, `thirdparty.ml`, `delayed_events.ml`; additions to
`rooms.ml` (upgrade, aliases, joined_members, timestamp_to_event), `auth.ml`
(login token, register/available, registration tokens), `profile.ml`
(MSC4133 fields), `directory.ml`; wrappers; tests.

**Simplified sliding sync (MSC4186).** Rewrite `sliding_sync.ml/.mli`
against the Rust `sliding_sync/` crate and ruma `sync::sync_events::v5`;
new `matrix_eio/sliding_sync.ml`; loop with `pos`, lists, room subscriptions,
extensions (e2ee, to_device, account_data, receipts, typing); tests.

### Phase 2 (parallel, after Phase 1 merge)

**Encryption service (the crypto machine, wired).** New
`encryption.ml/.mli` + `crypto_store.ml` + `matrix_proto/matrix_crypto_event.ml`
+ `matrix_eio/encryption.ml`. Sync-side: OTK count → upload; `device_lists` →
`/keys/query`; to-device `m.room.encrypted` → Olm decrypt → `m.room_key` /
`m.forwarded_room_key` / `m.room_key_request` (gossip reply) /
`m.key.verification.*` (dispatch to Verification); timeline
`m.room.encrypted` → Megolm decrypt. Send-side: `encrypt_room_event`
(outbound Megolm, share keys to all devices lacking them via claim → Olm →
`/sendToDevice`, rotation). Backup: upload pending sessions, restore from
backup. Two-machine tests over a mock homeserver.

**Verification completion.** `verification.ml/.mli`,
`matrix_event.ml` (verification content types), `matrix_eio/verification.ml`.
Full SAS (64 emoji, decimals, commitment, `hkdf-hmac-sha256.v2` MACs, the
request→ready→start→accept→key→mac→done state machine, cancellation codes),
QR (`MATRIX` binary format v2, modes 0/1/2, `m.reciprocate.v1`), real
cross-signing signature verification, device/identity trust updates.

**Base client state.** `store.ml`, `send_queue.ml`, `sync.ml`,
`timeline.ml`, new `sync_service.ml`, new `push_evaluator.ml`, new
`read_receipts.ml`, `matrix_eio/{sync,send_queue,matrix_eio}.ml`. Room state
maintained across syncs (name/topic/avatar/members/dm flag/unread), send queue
that really sends with retry and local echo, push-rule evaluation
(`event_match`, `contains_display_name`, `room_member_count`,
`sender_notification_permission`, `event_property_is/contains`), receipts.

**OAuth 2.0 next-gen auth.** New `oauth.ml/.mli`,
`matrix_eio/oauth.ml`, `bin/omatrix/omatrix.ml` `login --oauth`: `/auth_metadata`
(+ `/auth_issuer` fallback), OIDC discovery, dynamic client registration
(RFC 7591), PKCE S256, auth-code flow with a loopback redirect listener,
token refresh/revoke, device scope `urn:matrix:client:device:*`.

### Phase 3 (sequential)

**integration + docs.** Hook `Encryption` into `Sync_service` /
`Matrix_eio.run_sync`; `omatrix` gains `verify`, `backup`, encrypted `msg`
and `login --oauth`; reconcile `STATUS.md`, `CHANGES.md`, `TODO.md`,
this file's status section. Then a clean build/test pass by me.
