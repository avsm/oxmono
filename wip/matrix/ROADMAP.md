# ocaml-matrix: from feature-complete to fit for use

> **2026-09-02.** The module names below predate the restructuring that split
> `matrix-chat.cli` out of `matrix-chat.client`, moved the driver loops into
> `matrix-chat.eio`, and renamed `Sync_service` to `Base_client`, `Session.Store` to
> `Profile_store`, `Read_receipts` to `Read_state` and `Matrix_ui.Timeline` to
> `Room_timeline`, among others. They are left as they were written.
> `STATUS.md` and `README.md` name the modules the tree has now.

## Status: Phase 0 and parity implementation complete within the declared boundary; one external MSC4108 validation item open, 2026-09-05

`PORT_PLAN.md` moved the tree onto opam-repository packages and `PARITY_PLAN.md`
closed its original scoped port against matrix-rust-sdk. A new audit shows that
this did not mean full API or behavioural parity. `TODO.md` stays the current,
itemised source of truth. This document orders that work; when it and
`TODO.md`/`STATUS.md` disagree, the latter two win.

Progress since this plan was proposed (checked against `TODO.md` and
`STATUS.md` on 2026-09-05, OCaml tree at
`a76431516fe6982259eb3eb29b3a1db696f05a54`; Rust comparison tree at
`523b5af53a8fd9fae9e2bc981bfb01ac86fd2890`):

That Rust SHA remains the reproducible parity baseline. The current adjacent
checkout, `f4b9512df23332fce1bd26037ef7a2387af2ced2`, was also consulted for
implementation advice. It is newer than the baseline and has not been
exhaustively audited, so this roadmap does not equate completion of the pinned
checklist with full parity to the sibling's current HEAD.

- **0.1 done.** `test/integration/` runs against a real Synapse 1.159.0: 60
  scenarios (`test_homeserver.ml` plus the `scenario_*.ml` files) in 109.397s,
  covering rooms, E2EE, `matrix-chat.ui` and `matrix-chat.bot`. The isolated flag-enabled
  MSC4108 fixture additionally validates unauthenticated rendezvous
  create/PUT/GET/delete, two-party ECIES/bidirectional encrypted messages, and
  cleanup/cancel;
  it is deliberately not MAS/OIDC coverage. `.tangled/workflows/build.yml`
  remains non-live because its Docker/socket/network/path requirements have not
  been established for the runner; the harness itself is complete.
- **Second-homeserver check bounded.** `test/integration/dendrite.sh` pins
  Dendrite v0.15.2 and its named `dendrite-core` rooms profile passes 5/5. Its
  explicit legacy-peeking probe is rejected with
  `M_GUEST_ACCESS_FORBIDDEN`. Threaded receipts time out; MSC2246 preallocation
  is rejected despite advertised MSC3916, simplified sliding sync is
  unavailable, and federation is disabled. These capability outcomes do not
  change the 60-scenario Synapse reference.
- **0.2 done.** `.ocamlformat` pins `0.29.0`, the tree is formatted with it,
  `dune build @doc` is warning-free with Dune 3.24.2/odoc 3.2.1, and CI creates
  an explicit `ocaml-base-compiler.5.5.0` switch before dependency resolution.
- **1.E substantially done**, by a different route than described below: the
  event cache is already a chunk list with gaps and `paginate_gap`
  resolution (matrix-rust-sdk's `LinkedChunk` shape), landed alongside the
  `matrix-chat.ui`/`matrix-chat.bot` work rather than as a Phase 1 agent. Persisted room
  prefixes now load one chunk at a time before remote pagination. What is left
  — an O(chunks) list spine and a trim that cannot always describe its cut — is
  tracked as finer-grained work under "UI models" in `TODO.md`, not this
  section's acceptance test. `reconcile_by` already bounds pathological
  reorders with a reset threshold.
- **Phase 3, item 1 done.** `matrix-chat.cli` was split out of `matrix-chat.client` in
  the 2026-09-02 restructuring (`e6049f7`); `omatrix`, `matrix-bot` and the
  examples depend on it. Items 2–5 of the historical Phase 3 plan remain
  below as release-shape notes.
- **The P0 parity cores have moved.** The complete classic-sync room-state
  projection, classic/sliding OTK split and negotiated SAS presentation are
  done. Derived identity/trust, SAS signature publication with supplied private
  keys, the durable/grace-period UTD lifecycle and encrypted-attachment
  streaming transport now land together with mock and live coverage. Room-scoped
  identity warnings refresh after sync and now distinguish a persisted TOFU
  pin violation from the verification latch; SSSS/recovery import now loads
  matching private cross-signing seeds after a fresh own-user key query and a
  full self-signing import re-queries and verifies the own device. The
  credential-driven import receives its credential from the application and
  holds decrypted material in memory only; local `Crypto_store` keys remain
  unencrypted at rest, while QR/recovery seeds are stored in encrypted remote
  SSSS. Atomic attachment substitution and race-safe caption mutation,
  generic memory/SQLite media stores, persistent queue wiring, local-to-MXC
  replacement and orphan/retention cleanup have landed. Room forgetting now
  invalidates old sync responses and cleans base/store/cache/receipt/thread and
  queue state, including best-effort `m.direct` removal. Room previews/knock
  moderation and the smaller OAuth, profile-store,
  unread, sliding-presence, bot-event and runtime-lifecycle gaps have also
  landed. The second parity pass also landed configurable Eio HTTP policy
  knobs, well-known privacy policy, OAuth provenance/routed logout, bounded
  registration UIAA, stable/MSC2967 scope and MSC4191 action compatibility,
  sliding capability probing, direct unstable endpoint paths, the offline
  retry state, configurable timeline filtering, room-list Space/tombstone
  filters, a one-shot bot command mode with meaningful exit status, encrypted
  edit validation, retained fallback-key handover, authoritative bot member
  refresh/send readiness and a deadline-based verification CLI. The current
  bounded pass also adds the RFC 8628 device-grant driver, idempotent backup
  enable checks with master-key and device signatures, outbound
  withholding notices, concrete-device verification `m.accepted` fan-out,
  capability-aware media download/thumbnail/config/preview routes with unknown
  field retention, and graceful `omatrix sync` cancellation/save. The next
  application-service wave landed room conveniences, network/cache edit
  history, queued replies/static locations, notification settings/lookup, the
  room paginator, a Cache-Control-aware OAuth metadata cache, exact bound-loopback OAuth
  registration and all `omatrix` HTTP-policy
  flags. The latest bounded work also completes durable MSC4308 subscription
  catch-up, a fully restartable common sliding-sync snapshot, the Rust OTK
  pool/publication split, the one-shot recovery lifecycle with fresh-login
  backup restoration, and trusted MSC4268 pre-join history transport.
  Both legacy Peeking APIs (`/initialSync` and `/events`) and both current and
  legacy MSC4140 delayed-event APIs are implemented. Peeking is still a server
  portability limitation on Dendrite, whose named profile reports
  `M_GUEST_ACCESS_FORBIDDEN`. The final boundary audit also makes persisted
  queue decoding granular, validates forwarded-key identity, preserves
  cancellation/raw backtraces across cleanup, and attaches secret-free
  structured context to owned Eio I/O operations. The historical 1.A and Phase
  2 narratives remain below for context; the active parity status is the
  execution-order table and has one open external-validation item.
- **The pinned-Rust re-audit adds work to 1.B–1.D and 2.G–2.K.** The important
  additions are full room-state persistence, sender-identity/UTD semantics,
  encrypted attachments, a dependency-aware media send queue, thread
  subscriptions and cache topology, high-level dehydration/recovery, both QR
  login formats, and several room/UI services. The handoff checklist is at the
  top of `TODO.md` and expanded under the owning sections.

Where the tree stands now (`STATUS.md` is authoritative; this is a snapshot):

| | |
|---|---|
| Libraries | `matrix-chat.proto` 19, `matrix-chat.client` 91, `matrix-chat.eio` 51, `matrix-chat.cli` 1, `matrix-chat.ui` 20, `matrix-chat.ui.sqlite` 1, `matrix-chat.bot` 10 |
| Tests | 62 hermetic executables, 1,418 cases/checks and three source guards, plus `test/integration`: Synapse 1.159.0 60/60 in 109.397s and Dendrite v0.15.2 5/5 in 1.535s |
| Interop | Olm and Megolm byte-compatible with vodozemac 0.9 in both directions |
| Homeservers that have answered a request from this SDK | Synapse 1.159.0 and Dendrite v0.15.2 (`dendrite-core` rooms profile 5/5) |

## Follow-up parity audit: execution order

The completed audit compares the pinned trees named above, not an unpinned idea
of “latest”. Newer sibling-HEAD observations are separately labelled. Existing
workstream names remain stable; `+` below means the scope was enlarged by the
audit. P0/P1/P2 in the first column are audit priorities, not the historical
phase numbers embedded in workstream names. Agents should take one row, read
its linked `TODO.md` section, and keep unrelated rows out of the same change.
Large umbrella rows are split into lettered checkboxes there.

| Order | Workstream | Current state | Depends on | Completion gate |
|---|---|---|---|---|
| P0 | **1.B+ complete room state** | **Core complete** | — | A save/reload preserves every state field used by display names, permissions, encryption, retention and room lifecycle; old store files migrate. |
| P0 | **1.C+/1.D+ crypto identity and decryption safety** | **Complete; credential-driven recovery import** | 1.B membership data | Decrypted events carry derived sender trust and a stable unable-to-decrypt cause; late keys re-decrypt cached events and report through a durable grace-period hook; identity rotation cannot silently remain verified; matching SSSS/recovery seeds import atomically after a fresh own-user query; supplied private keys publish successful SAS; room-scoped typed pin/verification warnings recompute after sync. Stale Olm sessions use Rust's one-hour `m.dummy` repair. Credential discovery remains caller-owned. |
| P0 | **2.I+ encrypted attachment pipeline** | **Complete** | 1.A timeouts/stream errors | Known vectors prove AES-CTR encryption plus SHA-256 ciphertext integrity; upload/download stream without buffering and encrypted files round-trip through an encrypted room. |
| P0 | **1.C/1.D crypto edge semantics** | **Complete** | — | Classic/sliding missing OTK counts diverge correctly, and SAS exposes only negotiated emoji/decimal representations. |
| P1 | **2.I+ persistent send graph** | **Complete for queued sends** | attachment pipeline | Atomic queue-local attachment graphs, typed clear/encrypted result substitution, original/thumbnail upload payloads, monotonic progress, cancellation/race-safe captions and memory/SQLite local-media lifecycle all survive restart. The replaceable network media fetcher is complete as a separate seam. |
| P1 | **2.H+/2.K+ threads and cache topology** | **Complete** | 1.B | Persisted `ThreadInfo`, rich server-backed `Thread_list`, shared durable per-(room,root) identity with ordered root/replies and pagination metadata, Runtime lifecycle, pinned projection, thread-focused `/relations`, high-level timeline receipt controls, receipts/backfill, prioritized pagination, cancellable unread/receipt subscriptions and forget cleanup are covered across restart and stale-response races. |
| P1 | **2.J+ retention** | **Complete** | 1.B | Room and server retention combine into an effective policy and survive store reload, including server override/clamp and unsupported-endpoint fallback. |
| P1 | **2.J+/2.K+ room previews and knocks** | **Client layer complete** | 1.B | Summary/stripped-state previews and persisted seen/accept/decline/ban transitions have mock and Synapse coverage. A separate reactive UI projection was not part of the bounded acceptance test. |
| P1 | **2.G+ device continuity** | **Complete; external live validation separate** | 1.C, 1.D, 2.F | Recovery, reactive Manager/UIAA identity reset, conditional rebackup, device-key preupload, dehydrated-device pickle/upload/rehydrate/rotation manager and live peer-key dehydration continuity (drain, rehydrate, import, decrypt) are complete. MSC4108 uses its exact creation endpoint, distinct from MSC4388 discovery; both `omatrix qr grant` and `omatrix qr login` are implemented, including textual Base64 new-device payloads, profile preflight, mandatory check-code/progress/cancel/timeout, OAuth persistence before encryption state and subsequent network work, and fresh encrypted SSSS publication. Rust FFI returns raw QR bytes, so raster rendering/scanning is application-owned. Full OAuth/MAS two-role end-to-end validation is P2.3 because the isolated Synapse rendezvous fixture supplies neither service. |
| P1 | **2.H+ common sliding fold/profile extensions** | **Complete; adaptive fallback is a project enhancement** | 1.B, thread updater | Typed updates feed the common `Base_client` fold, profiles/reactivity/restart and MSC4262 update/null/drop persistence are covered, and the client profile API separately exposes JSON-null clearing for display names, avatars and custom fields. MSC4308 stale transactions and thread-before-common durability are guarded, and public legacy aliases plus private legacy-slot conversion are complete; only decoder compatibility remains. Presence wake/cancellation, endpoint selection and unknown-extension round trips are covered. `Adaptive_sync` automatic classic-sync fallback is implemented as a project enhancement, not pinned Rust parity; optional MSC4426 status/call data stays feature-gated. |
| P1 | **Room-forget lifecycle** | **Complete for existing stores/views** | 1.B, send graph, MSC4308 | A successful server forget removes base and persisted room/receipt/cache/thread/queue/media state, detaches in-flight sends and rejects pre-forget sync responses; direct rooms get best-effort `m.direct` cleanup. Every future cache view must join this lifecycle. |
| P1 | **Replaceable media fetcher** | **Complete** | completed media store | `Media_fetcher` provides an injectable cache-aware file/thumbnail capability, verifies encrypted bytes before release, never network-fetches local URIs, and caches only successful results. |
| P2 | **2.G external OIDC/rendezvous validation** | **TODO (external validation)** | complete 2.G flow | The isolated flag-enabled fixture covers unauthenticated rendezvous create/PUT/GET/delete, two-party ECIES/bidirectional encrypted messages and cleanup/cancel, but is not MAS/OIDC. P2.3 remains only full OAuth/MAS two-role end-to-end validation: provide a configured OIDC provider and rendezvous service, exercise both QR roles and the exact MSC4108 creation endpoint, and accept only when profile preflight, OAuth persistence ordering, check-code/progress/cancel/timeout, encrypted SSSS publication and test-event decryption pass; keep MSC4388 discovery separate. |
| P2 | **2.K+ application services** | **Complete** | cache/thread work | Drafts, room/network edit revisions, room conveniences/details, notification settings/lookup, live locations, directory/event search, deterministic space graphs, room/raw-thread pagination and the bounded shared `EventCache`/`EventStore` per-(room,root) thread projection are complete, including relation pagination, receipt/backfill, prioritization and view invalidation. The thread projection is not Rust's independent persisted `LinkedChunk` implementation; ordinary room history does now use lazy persisted-prefix hydration. |

Do not pull Rust opt-in experiments into P0/P1 incidentally. Encrypted state
events (MSC3956/MSC4362), inline galleries (MSC4274), push secrets (MSC4385),
Olm/Megolm v2 algorithms, X.509 identity, widgets, MatrixRTC, IndexedDB and
Element-specific recent-item stores must remain separate proposals unless a
user promotes them.

## How the order was chosen

1. **Keep real-server feedback early.** Synapse 1.159.0 now answers 60
   scenarios in 109.397s; the named Dendrite `dendrite-core` rooms profile
   answers 5/5, while its explicit legacy-peeking probe returns
   `M_GUEST_ACCESS_FORBIDDEN`. Judgements about OAuth, unstable prefixes,
   retention servers that implement MSC1763 configuration, thread subscriptions
   and other homeservers are still based on mocks or source reading. Extend the
   harness with each workstream and add a second implementation before calling
   behaviour portable.
2. **Silent wrong behaviour before missing features.** The restart recipient,
   late-key retry, OTK-fold, SAS-presentation, durable upload dependency and
   room-forget resurrection bugs are now covered. Remaining examples include a
   replay-safe POST handed back on 429, unread counts drifting across partial
   sync windows and a crypto snapshot interrupted between its component files.
   Each is worse for a user than the absence of externally validated QR
   OIDC/rendezvous infrastructure.
3. **Structure that taxes every later change goes before the changes.** The
   `matrix-chat.eio` wrapper boilerplate, the CLI dependencies in `matrix-chat.client`,
   and the sliding-sync path that cannot feed `Sync_service` all make the
   feature work in Phase 2 more expensive than it needs to be, but they also
   touch every file, so they go last where they conflict with nobody.
4. **Parity for its own sake last.** Admin `/whois` is complete, while
   convenience facades remain deferred. Peeking no longer belongs in that
   bucket: both legacy APIs are implemented. Retention no longer belongs in
   that bucket: the re-audit found durable room-state and effective-policy
   semantics behind the endpoint.

Each workstream below names the `TODO.md` entries it closes, the files it
lives in, and what "done" means. Sizes are S (a day), M (a few days), L (a
week or more) for one agent.

## Phase 0 — a homeserver answers (sequential)

### 0.1 Integration harness against Synapse — L

`test/integration/test_homeserver.ml`, a new executable that reads
`MATRIX_TEST_HOMESERVER` and prints `SKIP` when it is unset, so `dune runtest`
stays hermetic. `test/integration/synapse.sh` starts Synapse from a pinned
`ghcr.io/element-hq/synapse` image with `enable_registration_without_verification`
and `experimental_features.msc3575_enabled` — Synapse's name for the flag that
serves MSC4186's endpoint, not `msc4186_enabled` — and the same step belongs in
`.tangled/workflows/build.yml` before the test step. Synapse rather
than a lighter server because sliding sync and the MSC paths in `TODO.md` are
defined by what Synapse does.

Scenarios, each a real request path from `omatrix`'s point of view:

- register two users, create an encrypted room, invite, join, set name and
  topic, send and receive a plain message through `Sync_service.run`;
- send an encrypted message from one client, decrypt it in the other's sync,
  including the `/keys/upload`, `/keys/query`, `/keys/claim` and to-device
  legs the encryption service drives;
- SAS verification between the two over the loop (`test_e2ee_integration.ml`
  already scripts this against the mock; the same script runs here);
- key backup create, upload, restore into a fresh machine; secret storage
  round trip;
- media upload and authenticated download; a thumbnail;
- one sliding-sync `sync_once`;
- a redaction, a reaction and an edit reaching `Matrix_ui.Timeline` with the
  aggregation the unit test expects.

The harness is complete locally and every endpoint family in `STATUS.md` has
been exercised by at least one request. CI execution remains non-live until
the runner's Docker/socket/network/path requirements are established. Expected
fallout, to be fixed in this phase because the harness will not pass without
them: URL escaping of `!` and
`:`, Synapse's default rate limits (which pull the retry item from 1.A
forward), and whatever the `get_joined_rooms`-style lenient codecs hide.

### 0.2 Tooling debts — S

- `.ocamlformat` pins 0.29.0 and the tree is formatted with it; `dune build
  @fmt` is clean over `lib/`, `test/` and `example/`.
- `dune build @doc` was revalidated on 2026-09-03 with Dune 3.24.2/odoc 3.2.1:
  it succeeds with four existing ambiguous-reference warnings and no
  cross-package unresolved-root warnings; the old 228-warning report is not
  reproducible and needs no package-file change.
- The httpz opam pins stay until `fetch`, `fetch-httpz` and `httpz` are
  released; this is an external gate, not work. Record the CI image's OCaml
  version once 0.1 has run.

## Phase 1 — silent wrong behaviour (parallel, five agents)

### 1.A HTTP client hardening — S

`lib/matrix_client/client.ml`, `error.ml`, and the direct HTTPz setup in
`lib/matrix_eio/client.ml`.

- **Transport baseline already present:** `matrix-chat.eio` uses `Fetch_httpz.std`,
  whose idempotent-method retry policy honours delta and
  HTTP-date `Retry-After`, keeps cookies, rate-limits per origin and applies
  connect/per-I/O idle timeouts. Its Matrix request veto additionally admits
  only replayable `/keys/query` POSTs. A caller-supplied `Matrix_client` fetch
  keeps the caller's policy.
- **Landed:** HTTPz's request-level veto gates both response and connection
  retries; `Matrix_client.Http_retry` admits only canonical `/keys/query`
  POSTs, and the Eio/CLI defaults install it. Key claim, key upload, sync and
  all other POSTs remain vetoed.
- **Landed:** an `Error.Policy_denied` constructor matched before the general
  `Eio.Io` case, so a refused off-origin redirect is not a `Network_error`.
- **Landed:** map normalized `Fetch.Tls_failure` into a distinct `Error.t`.
- **Landed:** pass the parsed IP to `Tls.Config.client` for IP-literal
  homeservers.
- **Landed:** expose retry/limit/cookie/timeout choices through
  `Fetch_httpz.std`; every `omatrix` network command exposes matching
  policy flags, including off-origin OAuth transport.
- **Landed:** a client-wide `Query`/`Do_not_query` well-known policy is inherited
  by derived clients and OAuth discovery, with no-request mock coverage.
- **Landed:** off-origin well-known delegation is validated through a fresh,
  origin-restricted unauthenticated client in the Eio facade. The pure API uses
  an explicit callback and retains its no-callback compatibility result; public
  discovery probes do not forward bearer credentials.

The mock gate now distinguishes a replay-safe POST retry, a whole-call timeout,
a denied redirect and a TLS alert. The remaining live gate is a 0.1 run against
Synapse's default rate limits.

### 1.B Sync persistence and push — M

`lib/matrix_client/sync_service.ml`, `store.ml`, `push.ml`,
`push_evaluator.ml`, `lib/matrix_proto/matrix_event.ml`, `matrix_sync.ml`.

**Implementation update:** the first two bullets landed as a versioned generic
state projection with migration, completeness semantics and authoritative
full-state replacement. The old-redaction and stripped-state codec bullets
have landed too. Synced/stored push rules activate before same-response
evaluation, threaded receipts/unread positions retain their thread scope, and
latest-event plaintext persistence is available only under an explicit store
policy. Session, base and crypto snapshots now share the
persistent profile lock. Base saves reject stale byte fingerprints; crypto
saves reject stale generations and fail closed around interrupted multi-file
transactions.

- Members and power levels into `Store.room_info`, so a restarted client
  shares keys with the full device list and `@room` applies. Closes the
  `omatrix msg` workaround of calling `/joined_members` per send.
- Bring the rest of `matrix-sdk-base`'s durable `RoomInfo` projection with
  them: room version/create/creators/type, predecessor/successor/tombstone,
  aliases, history visibility, join rule, guest access, retention, pinned
  event ids, service members, and the state/encryption-sync completeness
  flags. Preserve unknown state separately instead of silently dropping it.
  Version the JSON store and migrate the current shape on load.
- `m.push_rules` account data now hydrates the common fold before same-response
  notification evaluation and survives restart; `Push.condition` carries the
  values needed by `event_property_is` and `event_property_contains`. A first
  `/pushrules` fetch remains an optional Eio bootstrap for an account whose
  sync/store has not supplied the event yet.
- A `redacts` member on `Raw_event`, so pre-v11 redactions are recognised.
- A `Stripped_event` codec in `matrix-chat.proto`, used by `Matrix_sync.Invited_room`
  and `Sliding_sync.invite_state` both.
- Threaded receipts kept per thread; local unread counts recomputed from the
  events the room still holds when a receipt arrives, not accumulated.
- `latest_event` stored decrypted under an explicit plaintext policy on
  `Store`, mirroring `Matrix_ui.Event_store.plaintext_policy`.
- A lock file for `Store.on_disk` and `Session.Store`, so two processes on
  one profile fail loudly instead of clobbering `base_state.json`.

Done when `test_base_client.ml` reloads a store and finds members and power
levels, the push tests use the server's ruleset shape, and 0.1 shows a
restarted client encrypting to every device.

### 1.C E2EE robustness — L

`lib/matrix_client/encryption.ml`, `olm.ml`, `backup.ml`,
`lib/matrix_eio/sync_service.ml`.

**Implementation update:** structured UTD classification, the
`Matrix_ui.Runtime` late-key request/re-decryption loop and the public durable
`Utd_hook` with grace timing/telemetry landed, as did derived sender metadata,
the classic/sliding missing-OTK split, `on_encryption_error`, and
one-generation fallback-key retention and persistence. Fallback creation time
and unpublished state now survive restart and drive Rust's strictly-older-than
one-week rotation policy. A real Synapse backup restore exercises the late-key
report. Outbound withholding has landed with durable sent-notice bookkeeping,
and trusted secret-gossip values and request lifecycle are durable too. Olm
selection/four-session retention and serialized, checkpointed backup batching
have landed; stale-session unwedge now force-claims after Rust's strict
one-hour cutoff and retains one encrypted `m.dummy` through acknowledgement.
General Olm age expiry, a finite dropped-OTK retry cap and paged whole-backup
restore are optional hardening/scale design, not pinned/current Rust parity:
both Rust and OCaml use only the one-hour stale-decrypt unwedge (no general
expiry), retry failed upload batches then generate replacement batches
indefinitely, and use 15-second-to-15-minute indefinite backoff for omitted
claims. Whole-backup restore uses unpaginated `GET /room_keys/keys`; targeted
room/session restore exists.
Backup ETag coordination is optional OCaml hardening, not pinned/current Rust
parity: Rust addresses backup versions, serializes operations, and disables on
`NotFound`/`WrongRoomKeysVersion`.

- **Landed:** the retry loop joins `room_change.undecrypted` to
  `Encryption.outcome.new_sessions`, sends the `m.room_key_request` that
  `request_room_key` already builds, re-decrypts when the key lands, and
  reports the result as a `redecrypted` list on `changes`. `Matrix_ui`'s
  `Event_cache.set_decrypted` is the consumer.
- **Landed:** the three-value caller-set verification flag on decrypted events
  was replaced
  with derived sender data: unknown device, device info, sender unverified,
  sender verified, and verification violation (including a previously verified
  identity that changed). UTD causes such as pre-join history, withheld codes,
  unsigned/unknown devices and backup-disabled/unconfigured are classified with
  enough structured data for UI shields and telemetry without leaking event
  content.
- **Landed:** `m.room_key.withheld` for devices the gossip policy refuses. The sent-notice
  retry and sent-notice deduplication now persist in the crypto snapshot,
  including the original transaction identifier and body across restart.
- **Landed:** Olm lookup prefers the session that last decrypted and a
  separate successful-use LRU keeps four per device, with recency persisted
  across restart. Failed decrypts over a sufficiently old session force a new
  claim and persist one encrypted `m.dummy` until acknowledged, including the
  empty-claim and clock-rollback cases. General age expiry is not part of Rust
  or OCaml parity.
- **Landed:** fallback keys are kept for one round after rotation and
  persist across restart; persisted creation time and unpublished state drive
  Rust's weekly age policy, including omitted reports and clock rollback.
  Failed upload batches retry and low server counts generate replacements
  indefinitely, matching Rust; a finite retry cap is optional hardening.
- **Landed:** an absent `signed_curve25519` count is zero for classic `/sync`
  but no change for MSC4186; distinct fold paths and transition tests cover it.
- **Landed:** secret sharing over to-device answers `m.secret.request` from our own
  verified devices, accepts `m.secret.send` for the cross-signing and backup
  keys. Values, cancellations and exact encrypted retries now persist across
  restart. QR login in 2.G hands keys over this way.
- **Landed:** `backup_pending` uses deterministic 100-session batches,
  serializes calls per driver and checkpoints every success. Targeted
  `restore_from_backup` supports room/session subsets; whole restore uses
  unpaginated `GET /room_keys/keys`, matching Rust. Paged whole restore and an
  ETag guard are optional OCaml hardening, not pinned/current Rust parity; Rust
  uses version-addressed backup operations and disables on
  `NotFound`/`WrongRoomKeysVersion`.
The late-key, outbound-withheld and Olm-unwedge mock/restart cases are done.
General session expiry remains optional hardening, not a parity gate.

### 1.D Verification and trust — M

`lib/matrix_client/verification.ml`, `encryption.ml`,
`lib/matrix_eio/verification_service.ml`.

**Implementation update:** the identity store, previously-verified/rotation
facts, explicit decryption trust policy, negotiated SAS accessors and SAS
signing/upload path landed. Publication validates the exact MACed key and
matching private identity, handles both own-device and other-user targets and
suppresses trust on upload failure. `Matrix_ui.Room_identity` now recomputes
per-room identity warnings after sync, and credential-driven private-key
loading is complete. In-room transport now tracks the request event ID, uses
strict room relations/recipient/device/time validation, filters own echoes and
passes a complete encrypted-room SAS flow against Synapse. Exact request
replays are retained; different active requests from the same user cancel both
through their original transports. Verification requests also retain concrete
recipients and issue one directed `m.accepted` cancellation to each non-selected
device; a received cancel is relayed once to the other concrete recipients,
while wildcard requests deliberately have no fan-out. Commitment still uses
the decoded/re-encoded start content rather than the exact received JSON, a
ruma/pinned-Rust limitation rather than an OCaml-only parity gap.

The original design bullets below are retained as historical audit context;
the implementation update and gate statement above are authoritative.

- An identity store in `Crypto_store` that `device_lists` invalidates, so
  `Verified` means the chain was checked, not that someone said so.
- Persist whether an identity was previously verified and publish a per-room
  identity-status change stream. Make the decryption trust requirement
  explicit (`untrusted`, cross-signed-or-legacy, or cross-signed) and test key
  rotation/withdrawal as well as the happy path.
- In-room verification transport through `/rooms/{id}/send`, with the flow
  id filled from the timeline's event id.
- `m.accepted` fan-out cancels the other concrete devices' flows once; a
  wildcard recipient set cannot safely express “all except the selected one”.
- `Verification_service.confirm` hands the prompt to the caller as a promise;
  human interaction is caller-owned and must not block sync progress.
- Commit over the raw `m.key.verification.start` JSON rather than our
  re-encoding.
- `Sas.emoji` and `Sas.decimals` return `None` unless that representation was
  selected in `m.key.verification.accept`; the confirmation callback receives
  only negotiated representations.

The signature-upload service gate is covered by full sync-driven mock flows,
including homeserver rejection and stale private keys. In-room SAS now has
strict room relation/recipient/device/timestamp validation, own-echo filtering
and same-user competing-request cancellation through each flow's original
transport. The separate first-ready fan-out cancels only the other originally
targeted concrete devices. Credential-driven private signing-key import now
completes that gate; the historical live `/keys/query` publication assertion is
not a separate parity item.

### 1.E UI event cache, second design — L

> **Done, substantially — see the status note above.** The chunk-list-with-gaps
> shape below landed as part of the `matrix-chat.ui`/`matrix-chat.bot` restructuring, not
> as this agent. Persisted tail loading and one-chunk hydration have since
> landed. The bullets below are historical; the residual O(chunks) spine and
> tokenless-trim work are tracked under "UI models" in `TODO.md`.

`lib/matrix_ui/event_cache.ml`, `event_store.ml`, `timeline.ml`.

The cache is one flat list per room with one token, which is why a limited
sync drops history and `trim` forgets what it cut. Replace it with
matrix-rust-sdk's shape: a room is a list of chunks, each with its own
pagination tokens, and a `Gap` between chunks that `Timeline.paginate_gap`
resolves in place. A limited sync then inserts a gap instead of resetting.

- `Event_store.room` grows chunks; SQLite gains a chunk table and per-event
  upserts keyed by `stable_id`, so a save is proportional to the delta.
- SQLite calls move to a worker via `Eio_unix.run_in_systhread`.
- `Timeline.refresh` memoises `Presentation.of_event` per `stable_id` and
  updates the reaction, edit and redaction tables incrementally.
- `Timeline.close` unsubscribes and drops the runtime's entry.
- A ciphertext-only cache is re-decrypted on load through 1.C's hook.
- `Text.sanitize_html` allows `img` with an `mxc://` source and a
  caller-supplied hook. Capability-resolved media URLs now exist, but the UI
  still needs to install that hook and attach auth for a v1 route.
- Virtual items for the read marker and typing; membership-change grouping.

Done when `test_matrix_ui.ml`'s limited-sync case keeps `a b c`, shows a gap,
and fills it from a mock `/messages`; a QCheck property that chunk order plus
gap resolution always yields the server's order.

## Phase 2 — features (parallel, after Phase 1 merges)

### 2.F OAuth completion — M

`lib/matrix_client/oauth.ml`, `client.ml`, `session.ml`.

Strict metadata URL validation has landed: issuers and endpoints require HTTPS
and a host by default, issuer queries/fragments are rejected, and
`~allow_insecure` is the explicit local-development escape hatch. Landed:
session provenance and routed Matrix/OAuth logout, including token revocation.
The high-level browser flow also has a five-minute default timeout and uses
`Client.with_access_token` for provisional `/whoami`. Stable and MSC2967 scope
dialects plus stable and MSC4191 account-action spellings are selected from
metadata without mixing dialects. The RFC 8628 device grant request and
bounded monotonic poll loop have also landed in the library/Eio wrapper.
Expiry-aware proactive refresh is available on `Client.t` and through the Eio
Matrix/OAuth facades. It uses an injected clock and early window, shares the
reactive refresh lock, carries the new absolute expiry through an atomic
session callback, retries later after failure and sends a one-shot stream only
once. Login expiry is persisted and restored by `omatrix`; the Eio facade owns
refresh completion independently of the initiating request; OAuth
`invalid_grant` emits a typed invalidation notification; metadata caching honors
`Cache-Control`; and the bounded RFC 7591 metadata extensions are modeled.
Reactive `M_UNKNOWN_TOKEN` refresh uses serialized exact-token attribution and
one safe replay. Protection is per client rather than cross-process for the
network request. Profile session persistence locks, re-reads and atomically
updates the latest file. Base and crypto stores share that profile lock and
reject stale handles. The shared refresh promise is resolved before its
callback, so reentrant authenticated calls do not deadlock. Password login
can request refresh tokens, and the CLI does so.
`omatrix login` exposes the browser and headless
`--device-code` flows.

### 2.G Device continuity: QR login, dehydration and recovery — L

Depends on 1.C for secret sharing and on 2.F for the device grant. MSC4108 now
has strict codecs, conditional rendezvous, the exact MSC4108 creation endpoint,
a vodozemac-compatible ECIES channel, typed two-party secure handshake,
OAuth/login approval and authentication messages, secret handover and the
complete Eio flow. The distinct `IO_ELEMENT_MSC4388` format and unauthenticated
homeserver probe have landed without conflating them with verification QR; at
the pinned Rust revision its secure channel remains typed unsupported. Both
`omatrix qr grant` and `omatrix qr login` are implemented, including a textual
Base64 new-device payload. Profile preflight, mandatory check-code,
progress/cancellation/timeout, OAuth persistence before encryption state and
subsequent network work, and fresh encrypted SSSS publication are covered.
Rust FFI returns raw QR bytes,
so raster presentation/scanning is application-owned. The isolated flag-enabled
fixture validates unauthenticated rendezvous create/PUT/GET/delete, two-party
ECIES/bidirectional encrypted messages and cleanup/cancel, but is not MAS/OIDC.
Full OAuth/MAS two-role end-to-end coverage remains the separate P2.3 external
validation TODO because the isolated Synapse fixture supplies neither service.

Turn `Dehydrated_device` from an opaque transport into a manager: support
probe, create, rehydrate, delete, weekly rotation and lifecycle notifications.
The support probe, endpoints, SSSS-protected interoperable libolm/vodozemac
pickle key, signed device/OTK/fallback-key upload, bounded paged event drain,
room-key import, delete-after-complete-drain and clock-controlled weekly
rotation manager are complete. Lifecycle notifications, retry/error boundaries,
live peer-key continuity through rehydration and live Synapse coverage are also
present.

Recovery and trusted history sharing are no longer part of that open slice.
`Recovery` implements ordered check/enable/recover/repair/key rotation/disable/
delete-all, restores all cross-signing seeds plus the optional backup secret,
and preserves Rust's partial-write boundaries. A fresh login recovers an old
Megolm session through SSSS and backup. MSC4268 transport restores a missing
backup key before upload, authenticates the MSC4147 sender proof, re-queries
inviter trust after join, retries transient downloads and imports only
  same-room sessions from a cross-signed inviter; malformed/404 media is
  discarded, and a live trusted join decrypts pre-join history. Reactive
  recovery state, UIAA/password identity reset, conditional rebackup and
  device-key preupload, OAuth credential handoff and dehydration are complete.

P2.3 is external validation, not an implementation gap: the ordinary Synapse
fixture leaves MSC4108 off, while the isolated flag-enabled fixture covers
unauthenticated rendezvous and its two-party encrypted channel but not
MAS/OIDC. Provide a configured OIDC provider and rendezvous service,
exercise both QR roles through the exact MSC4108 creation endpoint, and keep
the MSC4388 discovery probe separate.
Reverse-proxy the rendezvous service under the homeserver origin because the
CLI's safe default rejects QR-supplied cross-origin URLs; `TODO.md` records the
policy-gated transport work required if that is impossible.
Accept only a run that proves profile preflight, OAuth-session persistence
before encryption state and subsequent network work,
check-code/progress/cancellation/timeout behavior,
fresh encrypted SSSS publication of private seeds and the optional backup key,
and new-device decryption of a test event.

### 2.H Sliding sync into the base client — M

The common path is now crash-safe: typed MSC4262 profile patches, including
explicit field and whole-user deletion, survive restart; MSC4308 capability
discovery, mutations, paged catch-up, subscription rows and ranges are
complete; and applied extension/thread changes are durable before the new
sliding position. One-shot, loop and stream APIs send configured presence on
every poll. The public parallel `Sliding_sync_state` path is retired. The
private legacy-slot decoder and transactional conversion are complete,
preserving common-cursor precedence, rollback and restart behavior; only
decoder compatibility remains. Common folding, cancellable profile
subscriptions, retry wake/cancellation, stale MSC4308 guards and endpoint
selection are covered. `Adaptive_sync` automatic classic `/sync` fallback is
implemented as a project enhancement, not pinned Rust parity. Keep MSC4426
`m.status`/`m.call` opt-in.

### 2.I Media, encrypted attachments and queued sends — L

Capability-aware download, thumbnail, config, URL-preview and effectful MXC URL
routing between authenticated Matrix 1.11/MSC3916 and legacy media endpoints
has landed, with unknown config/preview fields retained.
`mxc_to_http_resolved` consults the same version cache; the pure
`mxc_to_http` and explicit browser-safe `mxc_to_http_unauthenticated` helpers
remain for callers that intentionally choose a route without discovery.
Streaming encrypted upload/download and MSC2246 preallocated async upload have
landed, including typed local/server expiry and overwrite handling. Generic
queue-local dependency edges, atomic event-ID/upload-result resolution, durable
original/thumbnail upload nodes, restart, monotonic byte progress and
transitive cancellation are also complete.
The generic serialized `Media_store` backend seam and memory/SQLite
implementations have landed, keyed by local/MXC URI plus requested
file/thumbnail format with Rust-compatible retention defaults (20 MiB per item,
400 MiB total, 60 days and daily cleanup). The send queue protects local bytes
before saving its request, serves retries from the store, publishes local URIs
for echoes, moves successful uploads idempotently to the MXC key and reconciles
crash orphans. Runtime accepts separate durable queue/media stores and owner
identity. Content-scanner policy remains a separate concern; the replaceable
network fetcher itself is complete.

AES-CTR encryption/decryption, SHA-256 over ciphertext, strict encrypted-file
metadata validation, verified buffered helpers and authenticated streaming
upload/download have landed. Download spools ciphertext and releases plaintext
only after verification; the live Synapse case sends the resulting metadata
through an encrypted room and downloads it as the receiver. The durable generic
`Send_queue` graph now creates the original, optional thumbnail and one visible
attachment event atomically. Typed clear/encrypted results propagate across
restart and replace `url`/`file` plus thumbnail fields before plain or encrypted
send; progress, cancellation and the in-flight compensating-redaction race are
durable. Pending captions mutate the durable event node, while an in-flight
edit becomes one last-write-wins replacement with a stable transaction and
exactly-once local echo migration. Local-media replacement, cancellation,
orphan cleanup, SQLite restart and retention scheduling are complete; encrypted
nodes cache ciphertext rather than plaintext.
Ordinary text edits now have `Send_queue.send_edit` and a sanitising
`Room_timeline` wrapper,
persisting and retrying as generic event nodes.

### 2.J CS API completeness — M

Both legacy Peeking APIs and the current and legacy MSC4140 delayed-event
shapes are implemented. Remaining work is extending the explicitly documented
unstable-prefix fallback set selected from `/versions` `unstable_features`;
existing capability helpers already consult those flags rather than merely
decoding them. Event
reports now omit Matrix 1.18's removed `score` member while retaining the OCaml
argument as an ignored compatibility shim. A typed `RoomEventFilter` shared
by sync and search, plus structural JSON in `Account.ignore_user` and
`State.set_*`, have landed. Authenticated absolute-path helpers have also
landed, and MSC3814 dehydration now uses its direct unstable path without the
old `/..` canonicalisation trick.

Retention is complete: `Matrix_client.Retention` models and writes stable
`m.room.retention`, reads the legacy spelling, fetches MSC1763 server
defaults/limits, validates policy ranges and computes the effective room policy
with per-room override, independent clamps and Rust-compatible unsupported
fallback. The state survives a `Base_client` store reload; hermetic tests cover
missing/room-only/server-only/override/clamped cases and Synapse covers the
stable-state plus unsupported-server path. Room previews and knock-request
moderation are also complete at the client layer: summary/state-derived
previews, persisted seen state, exact accept/decline/ban transitions and a live
Synapse workflow. A separate reactive UI projection is optional design work,
not an unfinished item in the bounded P2.1 checklist.

### 2.K The rest of matrix-sdk-ui — L

On top of 1.E, the notification client/settings service, composer drafts,
live locations, room conveniences/details, network/cache edit histories,
directory and server-event search, a deterministic space graph and room/raw
thread-root paginators have landed. The bounded P2.1e services are complete.
The richer cache-backed thread list is now present: bundled latest events and
reply counts, persisted `ThreadInfo`, sync updates and Runtime lifecycle. The
bounded shared `EventCache`/`EventStore` per-(room,root) thread projection is
complete, including ordered roots/replies, pagination metadata,
unread/receipt subscriptions and forget cleanup. It is not Rust's independent
persisted `LinkedChunk` implementation; this thread-specific architecture does
not negate the lazy persisted-prefix loading used for ordinary room history.
Thread-focused
`/relations`, receipts/backfill, prioritized pagination and view invalidation
are also complete.

Split this into independently reviewable services rather than one large port:

- extend the landed drafts/edit history/send-state projection only as P1.1's
  persistent media/dependency graph requires;
- add room-preview and knock-request observables over the completed client
  services; the directory/search and member/service-member projections are now
  landed;
- keep the landed notification and live-location services aligned as their
  underlying push/cache models evolve.

Each service needs close/unsubscribe semantics, restart tests for persisted
state, and a mock contract test shaped from the pinned Rust implementation.

## Phase 3 — shape and release (sequential)

1. **`matrix-chat.cli`. Done in `e6049f7`** (2026-09-02). `Cmd` moved out of
   `matrix-chat.client`; `omatrix`, `matrix-bot` and the examples depend on the new
   library. `cmdliner`, `logs.cli`, `fmt.tty` and `fmt.cli` still sit on
   `matrix-chat.bot`'s dependency list (it builds `bin/matrix-bot`), not on
   `matrix-chat.client`'s or `matrix-chat.eio`'s — see the dependency table in
   `STATUS.md`.
2. **`matrix-chat.eio` by construction.** One `Eio_of_result` adapter (or a functor
   over the result-returning signature) generates the 38 mechanical wrappers,
   so the `.mli` files are generated rather than hand-written; the 15
   unwrapped modules get a wrapper or a recorded reason.
3. **`omatrix`.** **Landed:** `backup enable` checks and matches an existing
   server/local version before uploading, signs new auth data with the
   cross-signing master key and current device, `sync` runs on
   `Sync_service.run`'s fiber and shuts down cleanly on Ctrl-C/SIGTERM, and
   `verify` uses a deadline. `msg` restores durable joined-member state and
   fetches `/members` only when completeness requires it.
4. **Session store.** Result-returning saves; `?root` so stores are testable
   without touching `HOME`; chmod on a pre-existing file; on finding a
   `session.toml`, print one line asking for `omatrix login` rather than
   migrating.
5. **Release.** Reconcile `STATUS.md`, `TODO.md` and `CHANGES.md` against the
   tree as the last commit did; drop the httpz pins once released; tag 0.1.0.

## Not planned

IndexedDB (no JavaScript target), widgets, MatrixRTC beyond the event types
(including authenticated RTC transport discovery), the content scanner and
search index, experimental Olm v2 and Megolm v2 algorithms, encrypted state
and extensible events (MSC3956/MSC4362), push secrets (MSC4385), X.509 identity
verification, MSC4274 inline media galleries, MSC4426 status/call automation,
HTTP/2 (waits on httpz), Element-specific recent emoji/recent-room stores, and
a libolm pickle importer unless a user turns up with an account to migrate.

## Where each TODO.md section lands

| `TODO.md` section | Workstream |
|---|---|
| Parity re-audit handoff | P0/P1/P2 priority table above; detailed phase owners below |
| Build / packaging | 0.2, Phase 3.5 |
| HTTP layer | 1.A; `matrix-chat.eio` `.mli`s and the `Cmd` split in Phase 3 |
| Auth / OAuth | 2.F, 2.G |
| CS API gaps | 2.J; rendezvous in 2.G |
| Sync and base client | 1.B; the persistent send graph in 2.I |
| UI models | 1.E, 2.K; cache/thread topology starts in 2.H |
| Sliding sync | 2.H; `Stripped_event` in 1.B |
| E2EE: Olm and Megolm | 1.C; attachment crypto in 2.I |
| E2EE: key management and backup | 1.C; trust in 1.D; continuity in 2.G |
| Verification | 1.D |
| Media | 2.I, including cache and encrypted attachments |
| Session and persistence | Phase 3.4; locking in 1.B |
| omatrix | Phase 3.3 |
| Test infrastructure | 0.1; second homeserver and repeatable audit in TODO P2.2 |
| Deferred | Not planned; previously deferred UI/cache items promoted to 1.E/2.K |
