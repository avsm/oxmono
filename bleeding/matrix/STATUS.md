# ocaml-matrix — implementation status

**Status: the original targeted endpoint/E2EE milestone is complete and runs
against Synapse; parity with the current matrix-rust-sdk is not complete.**

Builds clean on OCaml 5.5. The validated suite has 64 hermetic test
executables, 1,457 cases/checks and three source guards. Synapse 1.159.0
validation covers 60 scenarios in 109.397s when `MATRIX_TEST_HOMESERVER` names
a running homeserver. Olm and
Megolm are byte-for-byte interoperable with vodozemac in
both directions, proven by `test/test_olm.ml` against a Rust oracle. The same
optional oracle checks both the current and legacy-compatible SAS MAC encodings
over a complete flow and independent vectors. Every assertion below was
checked against the tree; where something is missing it says so.

The 2026-09-08 release hardening adds validated backup restore with file-based
key input, recoverable crypto save/clear journals, native directory sync, and
opt-in cross-process Eio refresh persistence (enabled by `omatrix`).
`test/release-check.sh` passed the full Synapse suite in 109.840s and the separate
five-scenario Dendrite smoke profile in 1.573s, with required vodozemac execution.
`test/release-install.sh` provides the fresh-switch distribution gate. The
published HTTPz HEAD still lacks four required packages, so that gate remains
blocked on dependency publication. MSC4108 OAuth/MAS login is explicitly
experimental pending complete two-role live validation. See `RELEASE_REVIEW.md`.

The audit-remediation implementation baseline is reproducible at ocaml-matrix
`a76431516fe6982259eb3eb29b3a1db696f05a54`. The earlier feature baseline is
`948229e367f16abcc7b609310585904cdff0983d`. The historical 2026-09-03 parity
audit and 2026-09-04 implementation baseline were recorded at
`2c7bb435348043ce2ddb5a957497cac5ccd4633a`; `../matrix-rust-sdk`
`523b5af53a8fd9fae9e2bc981bfb01ac86fd2890` remains the pinned comparison
baseline. `PARITY_PLAN.md` distinguishes completed historical phases from this
follow-up; `ROADMAP.md` orders it and the checkbox handoff at the top of
`TODO.md` is authoritative.

The pinned Rust SHA above remains the comparison baseline. The adjacent Rust
checkout at `f4b9512df23332fce1bd26037ef7a2387af2ced2` was also read for newer
implementation advice, but it has not been exhaustively audited. Consequently,
“matches Rust” below means the pinned baseline unless a newer observation is
called out; this document does not claim full parity with current Rust HEAD.

## Current implementation update (2026-09-05)

The full Synapse reference harness passes 60 scenarios in 109.397s. A bounded,
named Dendrite v0.15.2 smoke profile covers the portable rooms core and passes
all five core cases (5/5); its peeking case is explicitly rejected with
`M_GUEST_ACCESS_FORBIDDEN`. The optional threaded-receipt case times out,
MSC2246 preallocation is rejected
despite Dendrite advertising MSC3916 media, simplified sliding sync is not
advertised, and federation is disabled. These are portability outcomes, not
additional Synapse scenarios.

`test/integration/run-both.sh` now starts fresh, uniquely named Synapse and
Dendrite fixtures together, runs the complete Synapse reference suite before
the additive Dendrite core profile, and purges both fixtures on success,
failure or signal. Its latest 2026-09-05 validation passed Synapse 60/60 in
109.397s and Dendrite 5/5 in 1.535s; an independent post-run check found no
containers, fixture data or harness processes left behind.

The previously unfinished U1/U2 families are now implemented: `Peeking`
provides compatibility-checked legacy `/initialSync` and `/events` APIs with an
Eio facade,
and `Delayed_events` exposes current MSC4140 scheduling, retrieval and action
routes alongside the unchanged legacy defaults. Their route, body, error and
decoder boundaries are covered by `test_peeking` and the expanded
`test_cs_api` checks. This does not claim that Dendrite supports peeking;
its named smoke profile reports `M_GUEST_ACCESS_FORBIDDEN` explicitly.

The result-returning client can now apply an optional monotonic deadline to a
complete logical HTTP operation. The same deadline covers transport retries
and backoff, response-body or stream-callback consumption, and time spent
waiting for automatic refresh/replay; parent cancellation remains distinct.
The Eio client exposes this as `?request_timeout`. Live fixture container names
and data directories are independently configurable, and every purge is
guarded by path checks and a fixture-specific sentinel.

The default Eio and CLI transports now use HTTPz's request-level retry veto.
They retain Fetch's normal idempotent-method retries and additionally retry
only the replayable, read-like `POST /_matrix/client/v3/keys/query` route,
including below a homeserver deployment prefix. Key claim, key upload, sync,
near-miss paths and every other POST remain vetoed for both response-status and
connection-failure retries. Caller-supplied fetchers retain their own policy.

Every explicit `matrix-chat.eio` operation now attaches a stable structured
operation context when raising `Eio.Io`. The common wrapper captures the raw
backtrace before `Eio.Exn.reraise_with_context`; cancellation and non-I/O
exceptions pass through. Direct client path/flow/random/persistence operations,
OAuth and QR lifecycles, sync/encryption/recovery services, UI/bot persistence
and SQLite systhreads follow the same rule. Best-effort boundaries preserve
cancellation, and result/log diagnostics remove userinfo, query and fragment
instead of retaining Fetch's exact wire URL.

MSC4108 now has a strict codec, conditional rendezvous transport, a
vodozemac-compatible ECIES channel and a typed two-party secure handshake. The
application and Eio layers complete OAuth device authorization, login approval
and authentication messages, secret export/import, device-key upload/trust
gating, backup, cleanup and handover. `omatrix qr login` creates textual
Base64 using the exact MSC4108 creation endpoint (not MSC4388 discovery),
requires an unused profile and check-code confirmation, reports progress and
supports cancellation and timeout. It saves the OAuth session immediately
after authentication, before encryption state and subsequent network work,
then creates a fresh encrypted remote SSSS store with cross-signing seeds and
an optional backup key,
protected by a passphrase file or a one-time printed recovery key. `omatrix qr
grant` takes a credential file or `MATRIX_SSSS_CREDENTIAL` and uses same-origin
rendezvous. The Rust public/FFI boundary supplies raw QR bytes; raster
rendering/scanning and the mandatory confirmation callback remain caller/UI
owned, not SDK parity gaps. The current Synapse image has no MAS/OIDC, so full
two-role OAuth QR live coverage remains external. MSC4388 remains the
separate typed unsupported channel described below.

The isolated MSC4108 fixture on port 8009 passes rendezvous, ECIES and cleanup
validation, but has no MAS/OIDC. Full two-role OAuth QR live validation remains
P2.3. CI still does not run the Docker live harness.

The thread work now persists `ThreadInfo`, provides the rich server-backed
`Thread_list`, owns its lifecycle in `Runtime`, projects pinned events through
the shared bounded out-of-band registry, and provides a room-context
`Event_focused` view. A bounded durable per-(room,root) thread-cache projection
now joins ordered event IDs, pagination metadata, receipts/unread and scoped
subscriptions across sync, relations, restart and forget. It is complete
within the shared `EventCache`/`EventStore` projection: each thread retains its
root and at most 255 replies, rather than using current Rust's independent
persisted `LinkedChunk`/lazy-loading design. The reactive, persisted
`Own_profile` observer and common `Base_client` profile fold have landed;
`Room_timeline` now exposes high-level receipt lookup/send/mark-as-read APIs
with timeline-position ordering, thread scoping, own-event fallback, monotonic
suppression and marked-unread clearing semantics. Profile display name and
avatar can be cleared with JSON `null`, and namespaced custom profile fields
can be set and deleted.

General room history now has a separate lazy persistence path: cold
`EventCache` construction reads validated layout/stable-ID metadata plus only
the newest events chunk, and back-pagination hydrates one stored predecessor
before using the network. Memory and SQLite share this contract, while the
bounded thread projection described above deliberately remains shared rather
than becoming a second independent Rust-style linked chunk.

In-room SAS uses room transport throughout and, in an encrypted room, every
raw request/follow-up envelope is `m.room.encrypted`. It validates the target
user, known device, timestamp, relation and exact event allowlist, and ignores
local echoes. Exact request replays retain the session; a distinct active
request from the same user cancels both through each flow's original room or
device recipient set. Transactional private legacy-slot migration is complete
and only private decoder compatibility remains for sliding sync.
`Adaptive_sync` supplies
automatic classic fallback as a project enhancement, not pinned Rust parity.

The first correctness/consolidation wave is complete. `Matrix_proto.Json.Codec`
is the checked wire-JSON facade, and `Signed_json` now validates canonical
Matrix integers, UTF-8 and unique members before signing. Reviewed persistence
schemas retain numeric-string and pre-Jsont-0.2 migration decoding without
relaxing wire responses. Identifier parsing covers current server-name grammar,
historical Unicode localparts, length limits and domainless room IDs. Secret
comparisons use `Eqaf`; OCaml 5.5 stdlib endian/UTF-8 helpers and
`Ptime_clock.now` replace local duplicates. SQLite event/media creation and
cleanup cross the Eio systhread boundary, and queued upload bytes persist
through the unpadded Matrix Base64 representation used by the retry path.
Configured, discovered, OAuth and QR URLs cross one HTTPz-backed validation
boundary; homeserver base paths are retained and endpoint paths are appended
beneath them. Dynamic endpoint segments now pass through one checked,
pre-parsed HTTPz URI-template layer, with Matrix path semantics and a source
guard against ad-hoc percent encoding. JSON-file stores share one
cancellation-safe 0600 atomic writer, and push/ACL matching shares the
Matrix-specific glob engine.

Recovery P1.3d is complete: the reactive `Recovery.Manager`, UIAA password
identity reset, conditional rebackup and device-key preupload are covered by a
live Synapse flow. OAuth reauthentication/credential handoff remains separate
follow-up work. Dehydrated-device pickle/create/rehydrate/manager support is
implemented, including live real peer-key dehydration continuity and real-key
unit decrypt. The replaceable media fetcher is complete, and backup restore is
available via the whole unpaginated GET plus targeted room/session APIs;
neither should be listed as an open gap.

Targets Matrix spec v1.17. The spec is not vendored — see
<https://spec.matrix.org/v1.17/>.

## At a glance

| Layer | State |
|---|---|
| Client-Server API | Broad endpoint coverage, including strict legacy peeking, current/legacy delayed events, room/server retention policy and typed server-admin whois; several high-level room workflows remain |
| E2EE | Olm/Megolm, key sharing, gossip, backup, SSSS, cross-signing and SAS/verification QR are wired; derived sender trust, SSSS/recovery cross-signing import, stale-session `m.dummy` repair, portable room-key file crypto, in-room SAS transport, SAS publication, durable UTD reporting/retry and authenticated encrypted-attachment streaming are present, with the remaining boundaries below |
| Sync | `/sync` v3 and MSC4186 sliding sync share the common `Base_client` fold for rooms, profiles and extensions; MSC4308 includes the stale-response guard and save-before-position ordering. Public standalone sliding state is retired; transactional private legacy-slot migration is complete and only private decoder compatibility remains. `Adaptive_sync` provides automatic classic fallback as a project enhancement, not pinned Rust parity. Runtime state distinguishes failed, offline, live and stopped |
| Auth | Password, token/SSO, reactive and expiry-aware proactive refresh, persisted login expiry, bounded registration UIAA and OAuth 2.0 with bounded loopback and RFC 8628 device-grant login, stable/MSC2967 scopes and stable/MSC4191 account actions; Eio refresh is runtime-owned, OAuth invalidation is typed, and session provenance routes Matrix versus OAuth logout/revocation. MSC4108 application/OAuth/authentication/handover, Session progress/cancellation/persistence callback and Eio flows are complete; `omatrix qr login`/`qr grant` provide the textual Base64 flow, while raster rendering/scanning and the mandatory confirmation callback remain caller/UI-owned. The isolated port-8009 fixture passes rendezvous/ECIES/cleanup but has no MAS/OIDC; full two-role OAuth QR live validation remains P2.3. MSC4388 is the separate typed unsupported channel. |
| UI models | Room list and timeline over a shared chunked cache with persisted lazy-tail loading and one-chunk local hydration before network pagination; room/directory/event-search/member facades, deterministic space graphs, network/cache edit histories, queued reply/edit/static-location sends, notification settings and one-event notification lookup, bidirectional room and raw thread-root paginators, Space/tombstone-deduplication filters, room previews/knock moderation and live locations are complete. `Room_timeline` exposes high-level receipt lookup/send/mark-as-read APIs with timeline ordering, thread scope, own-event fallback, monotonic suppression and marked-unread clearing. Persisted `ThreadInfo`, rich server-backed `Thread_list`, Runtime lifecycle, pinned-event projection, room-context `Event_focused`, thread relations/receipts/backfill, prioritized invalidation and the bounded durable per-(room,root) shared-cache projection are present. The thread projection uses shared `EventCache`/`EventStore`, retaining each root and at most 255 replies; it is not a second independent Rust `LinkedChunk`. Room/thread composer drafts persist. |
| Bots | `matrix-chat.bot`: typed and raw custom events off the event cache, one ordered handler fiber per room, commands with generated help, plugins as `spec -> spec`, per-room cursors in a `Plugin_store`, signals, authoritative member refresh and room-send readiness, plus shared long-running/one-shot command modes with shell-visible status |
| Persistence | Versioned JSON room/crypto/sliding state plus optional SQLite UI event and media stores (`matrix-chat.ui.sqlite`). UI event stores expose a compatible eager backend and an opt-in lazy metadata/tail/single-chunk contract; incremental layout and chunk replacements preserve unloaded rows. Profile, base, crypto and plugin snapshots share a unique-temporary 0600 atomic writer with file sync; profile snapshots additionally share a persistent advisory lock, stale base/sliding writers are rejected and crypto transactions recover complete snapshots from private redo journals. Native parent directories are synced after atomic replacement; non-native filesystems retain only file-sync guarantees. The send queue persists protected local media, crash-safe local-to-MXC replacement and orphan/retention cleanup. A successful room forget deletes all existing room-scoped projections, cache rows and queue state |
| Verified against a server | Synapse 1.159.0 through the full `test/integration` harness; Dendrite v0.15.2 through the bounded portable-room smoke run; no federation |

## Client-Server API coverage

Endpoint paths were grepped out of `lib/matrix_client/`. Areas taken from the
ruma `ruma-client-api` module list.

| Area | Status | Module |
|---|---|---|
| Discovery — `/versions`, `/capabilities`, `/.well-known/matrix/client` | ✅ | `Server` (HTTPz-validated/canonical URLs, retained Ruma-compatible base-path prefixes, cached Ruma-default policy helpers, configurable well-known privacy policy and unauthenticated off-origin delegation validation) |
| Login, logout, refresh, whoami, registration | ✅ | `Auth` |
| Login token (MSC3882), `/register/available`, registration-token validity | ✅ | `Auth`; login-token route is selected from `/versions` (Matrix 1.7 or MSC3882) |
| UIAA — password, token, recaptcha, email, msisdn, dummy | ✅ | `Uiaa` |
| OAuth 2.0 — `/auth_metadata`, dynamic registration, PKCE, refresh, revoke | ✅ | `Oauth` (including session provenance, routed logout and exact bound-loopback registration) |
| `/sync`, filters, long polling | ✅ | `Sync` |
| Simplified sliding sync (MSC4186) | ✅ | `Sliding_sync` |
| Rooms — create, join, leave, forget, invite/kick/ban, members, power levels | ✅ | `Rooms`; `Runtime.forget` also clears local base/store/cache/receipt/thread/queue/media state and best-effort updates `m.direct` |
| Room upgrade, `/aliases`, `/joined_members`, `timestamp_to_event` | ✅ | `Rooms` |
| Messages — send, redact, `/messages`, `/context`, all msgtypes | ✅ | `Messages` |
| Relations — reactions, edits, replies, threads | ✅ | `Relations` |
| Threads — `/rooms/{id}/threads` | ✅ | `Relations` |
| Thread subscriptions/catch-up (MSC4308) | ✅ | `Thread_subscriptions`; capability probe, mutations, changes, durable rows/ranges, stale-response guard and sliding save-before-position ordering |
| Room state get/set, typed common events, unknown-event retention | ✅ | `State`; `Store.room_info` durably projects current state, including retention and stable/legacy service-member hints |
| Profile, incl. extended fields (MSC4133 / Matrix 1.16) | ✅ | `Profile`; display name/avatar clearing sends JSON `null`, custom-field routes are selected from `/versions`, and custom fields can be deleted |
| Presence | ✅ | `Presence` |
| Typing, receipts, read markers | ✅ | `Typing`, `Receipts` |
| Account data, 3PIDs, password, deactivate, ignored users | ✅ | `Account`, `Account_data` |
| Devices — list, get, update, delete | ✅ | `Devices` |
| Media — upload, preallocation, download, thumbnail, config and URL previews | ✅ | `Media`; MSC2246 reserve/fill has typed expiry/overwrite failures, `/versions` selects authenticated v1.11/MSC3916 or legacy v3 read routes, and unknown config/preview fields are retained |
| Push rules and pushers | ✅ | `Push`; `Base_client` activates synced/stored `m.push_rules` before notification evaluation |
| Notifications — `/notifications` | ✅ | `Notifications` |
| Search and user directory | ✅ | `Search` |
| Room directory, aliases, public rooms, room summary | ✅ | `Directory`, `Rooms` |
| Room preview and knock moderation | ✅ | `Room_preview`, `Knock_requests`; persisted preview/seen state and invite/kick/ban actions |
| Spaces — `/hierarchy` | ✅ | `Spaces` |
| Tags — `/user/{u}/rooms/{r}/tags` | ✅ | `Tags` |
| Reporting — event, room and user | ✅ | `Report` |
| OpenID token | ✅ | `Openid` |
| Third-party protocols, locations, users | ✅ | `Thirdparty` |
| VoIP — `/voip/turnServer`, call events | ✅ | `Calls` |
| To-device — `/sendToDevice` | ✅ | `To_device` |
| Keys — upload, query, claim, device signing, signatures | ✅ | `Keys` |
| Key backup — `/room_keys/version`, `/room_keys/keys` | ✅ | `Room_keys`, `Backup` |
| Device verification — SAS, QR, cross-signing | ⚠️ | SAS runs over to-device and encrypted room transport with strict flow validation and Rust-compatible request collision handling. `Session` owns QR/authentication orchestration and the caller supplies the mandatory confirmation callback; raw QR bytes cross the public/FFI boundary, while raster presentation/scanning remains caller/UI-owned |
| Dehydrated devices (MSC3814) — transport and continuity state | ✅ | `Dehydrated_device`; interoperable account pickle, create/upload, rehydrate/delete, manager lifecycle, live peer-key continuity and real-key unit decrypt |
| Delayed events (MSC4140) | ✅ | `Delayed_events` (current and legacy route families) |
| **Peeking** — `/events`, per-room `initialSync` | ✅ | `Peeking` (legacy low-level APIs; Dendrite reports `M_GUEST_ACCESS_FORBIDDEN`) |
| **Retention** — `m.room.retention`, MSC1763 configuration and effective policy | ✅ | `Retention`; stable state with legacy read fallback, validation, override/clamp semantics and unsupported-server fallback |
| **Rendezvous / QR login** (MSC4108 flow; MSC4388 codec/probe) | ⚠️ | `omatrix qr login`/`qr grant` complete textual Base64 MSC4108 login/grant with progress, cancellation, timeout, exact creation endpoint, unused-profile check, immediate OAuth-session persistence and fresh encrypted SSSS setup. The isolated port-8009 fixture passes rendezvous/ECIES/cleanup but has no MAS/OIDC; only full two-role OAuth QR live validation remains P2.3. Raw QR bytes, raster rendering/scanning and the mandatory confirmation callback are deliberate caller/UI boundaries. MSC4388 remains a separate typed unsupported channel |
| **Admin** — `/admin/whois` | ✅ | `Admin` (server-admin authorization required; no broader Synapse admin API) |

`Dehydrated_device` now pickles an Olm account with the interoperable
vodozemac/libolm-compatible format, creates/uploads and rehydrates/deletes
devices, persists its SSSS key and last uploaded ID, and has manager and live
peer-key continuity coverage plus a real-key unit decrypt. `Delayed_events`
supports both the deprecated Synapse-compatible query-parameter shape and the
current MSC4140 body/path family.

## End-to-end encryption

Olm and Megolm were rewritten against the libolm specifications and
cross-checked against vodozemac 0.9.

| Feature | Status | Notes |
|---|---|---|
| Olm — X3DH, double ratchet, pre-key and normal message v3 | ✅ | `Olm.Session`; AES-256-CBC, HMAC-SHA256 truncated to 8 bytes, libolm's one-zero-byte HKDF salt |
| Megolm — message v3, hash ratchet, Ed25519 signature | ✅ | `Olm.Megolm` |
| Session key export — signed v2, unsigned v1 | ✅ | Importers start at the index in the blob, so a mid-stream share decrypts |
| **vodozemac interop, both directions** | ✅ | `test/test_olm.ml`: OCaml→vodozemac and vodozemac→OCaml, for Olm and Megolm, plus ratchet conformance and signature acceptance |
| Recorded interop vectors | ✅ | `test/fixtures/olm/{olm,megolm}_vodozemac.json`, checked even without the oracle |
| Room key sharing on send | ✅ | `Encryption.encrypt_room_event` asks for a `Room_key_share`: claim OTKs → Olm-encrypt `m.room_key` → `/sendToDevice` |
| One-time-key replenishment | ✅ | A 50-key public target is replenished from a 5,000-key private reserve; pending batches, published IDs and the server count persist, while classic `/sync` maps an absent count to zero and MSC4186 retains the prior count. Dropped OTK claims use the pinned/current Rust indefinite replacement/claim backoff; a finite cap is optional design/scale hardening |
| Fallback-key handover | ✅ | Rotation retains exactly one prior key for in-flight Olm handshakes; current and prior keys persist, while only the current key is published |
| Stale Olm-session repair | ✅ | A failed decrypt over a session strictly older than one hour force-claims a replacement and persists one encrypted `m.dummy` until acknowledgement; this is the pinned Rust unwedge rule, not a general Olm age-expiry requirement; rollback and empty-claim cases match the pinned Rust SDK |
| Key gossiping | ✅ | `m.room_key_request` / `m.forwarded_room_key`, plus bounded verified-device `m.secret.request` / `m.secret.send`; values, request/cancellation state and exact encrypted retries survive restart |
| Outbound key withholding | ✅ | `m.no_olm` and `m.blacklisted` notices persist their exact transaction/body and sent state, so retries and acknowledgement deduplication survive restart |
| Key backup | ✅ | `m.megolm_backup.v1.curve25519-aes-sha2`, upload and restore |
| Secret storage (SSSS) | ✅ | `m.secret_storage.v1.aes-hmac-sha2`, PBKDF2-SHA512, base58 recovery key |
| Cross-signing — generate, upload, verify | ✅ | `Cross_signing`; real Ed25519 verification over canonical JSON, not a presence check |
| SAS verification | ⚠️ | Full state machine and crypto; strict encrypted in-room request/follow-up transport validates target/device/time/reference, exact replays are retained and distinct same-user requests cancel through their original transports. Emoji/decimal accessors enforce negotiation, while the first request `ready` performs separate one-shot `m.accepted` fan-out to the other concrete recipient devices. Matching private identities can be loaded from SSSS/recovery and completion signs/uploads an own device or another user's master key. The recovery credential remains caller-supplied and private keys remain memory-only |
| QR verification | ⚠️ | `Verification.Qr`: binary `MATRIX` v2 payload, modes 0/1/2, `m.reciprocate.v1`. **No image rendering or scanning** — `qrc` is not a dependency |
| Dehydrated devices | ✅ | Interoperable account pickle, create/rehydrate/delete, durable key/device continuity, manager lifecycle, live peer-key continuity and real-key unit decrypt |
| Derived sender identity / trust policy | ✅ | Validated owner/device chains, persisted prior verification and rotation violations, distinct durable TOFU pin violations, session ownership and all three Rust trust requirements; `Matrix_ui.Room_identity` publishes typed per-room pin/verification warnings; `Secrets.import_cross_signing` supplies matching private signing keys after a fresh `/keys/query` |
| Late-key retry and structured UTD causes | ✅ | Stable causes and structured missing-session/index errors; the UI requests/deduplicates keys and re-decrypts in place, while a public grace-period hook durably deduplicates reports and records late-decryption telemetry |
| Encrypted media | ✅ | AES-CTR/JWK/SHA-256, strict metadata and verified streaming pass an encrypted-room Synapse round trip; the persistent queue substitutes clear/encrypted original/thumbnail results, while memory/SQLite media stores provide local echoes, protected cache reads, crash-safe local-to-MXC replacement and scheduled retention/orphan cleanup |
| Shared room history / recovery manager | ✅ | Recovery check/enable/recover/repair/reset/disable/delete-all coordinates SSSS, cross-signing, markers and backup; whole unpaginated backup GET plus targeted room/session APIs restore old events on a fresh login. Trusted MSC4268 sharing restores missing backup keys, validates MSC4147 sender proof, re-queries trust, transports/imports after join, retries transient errors and discards malformed/404 media; a live join decrypts pre-join history. Reactive Manager/UIAA identity reset, conditional rebackup and device-key preupload are complete; OAuth credential handoff remains separate |
| Passphrase-protected room-key file import/export | ✅ | `Room_key_export` authenticates the interoperable armored format, `Encryption` imports/exports durable sessions and metadata, and pinned fixtures prove both Rust→OCaml and OCaml→Rust decoding |

Verification request fan-out sends the `m.accepted` cancellation only to
concrete devices retained by the request; a wildcard `To_device.All` is left
without fan-out because it cannot safely mean “all except the selected device”.
If one concrete recipient cancels first, that cancellation is likewise relayed
once to the other concrete recipients.

### Deliberate ecosystem quirks

These look wrong and are correct. Changing them would make this SDK
incompatible with every other Matrix client.

- **The key-backup MAC covers the empty string, not the ciphertext**
  (`Backup.encrypt_session_data`). A libolm bug the wire format is stuck with;
  the spec, libolm, vodozemac and matrix-rust-sdk all do this. MSC4048
  proposes a fixed algorithm version.
- **Olm's HKDF uses a one-zero-byte salt**, not an empty one — a different
  function, and libolm's choice.
- **`Verification.Sas.Mac_method.Hkdf_hmac_sha256` reproduces a base64
  buffer-overrun bug** so that the deprecated MAC agrees with old clients.
  `Hkdf_hmac_sha256_v2` is negotiated whenever the peer offers it.
- **Megolm session ids are the base64 Ed25519 public key**, as everywhere
  else in Matrix.

### Gossip policy

`Encryption` answers an `m.room_key_request` only when all three hold: the
sender is our own user, it is not our own device, and that device's local
trust is exactly `Verified`. Anything else is refused, and the outcome's
`Room_key_request` reports `answered = false`. A successful SAS remains local
when `Verification_service` has no private signing identity; when a matching
one is supplied, the service publishes the own-device or other-user signature
before reporting success and changing local trust.

### Wiring

Encryption is not a side library — it runs inside the sync loop and the send
path, covered by `test/test_e2ee_integration.ml` (two clients, two machines,
a mock homeserver routing between them).

| Path | Where |
|---|---|
| One-time key counts → `/keys/upload` | `Encryption_driver.sync_hook`, driven from `Matrix_eio.Sync_service` |
| `device_lists` → `/keys/query` | same |
| To-device `m.room.encrypted` → Olm decrypt → room keys, gossip, verification | same |
| Timeline `m.room.encrypted` → Megolm decrypt | `Matrix_eio.Sync_service.apply ?encryption`, filling `room_change.decrypted` / `.undecrypted` |
| Cached UTD event → deduplicated key request → in-place re-decryption | `Matrix_ui.Runtime`, after a successful sync and whenever a timeline opens |
| UTD observation → grace/durable dedup → definite or late report | `Matrix_ui.Utd_hook`, wired by `Runtime` |
| Send → outbound Megolm, share keys, `m.room.encrypted` | `Matrix_eio.Send_queue.start ?encryption ?members` |
| `m.key.verification.*` → SAS completion → optional signature upload | `Matrix_eio.Verification_service` |
| Encrypted media → authenticated streaming upload/download | `Matrix_client.Media`, spooling ciphertext before plaintext release |

`Encryption` is the pure machine: it holds device lists, Olm and Megolm
sessions and backup state, and returns the requests it wants made rather than
making them. `Encryption_driver` joins one to a client and a `Crypto_store`
and performs them, returning `result`; `Matrix_eio.Encryption` is the same
driver raising `Eio.Io`.

`Crypto_store` persists the machine as JSON under the profile directory
(`crypto_state.json`, mode 0600, in a 0700 directory): devices and local trust,
validated identities and rotation history, the decryption trust requirement,
inbound-session sender metadata, validated withholding evidence, tracked users,
room settings, backup state and published-OTK bookkeeping. Writes use a
temporary file plus atomic rename. Private keys are stored base64 and **not**
encrypted at rest. Encrypted local storage is explicitly deferred; there is no
local unlock prompt or OS keyring integration. Tokens, private keys, session
state and recovery journals rely on filesystem access controls, including for
copies and backups. New profile directories are 0700 and secret-bearing files
are 0600; existing directory permissions are not tightened automatically.

## Sync

- **`Sync`** — `/sync` v3, filters, long polling, incremental `next_batch`.
- **`Base_client`** — the `matrix-sdk-base` equivalent, and pure: folding a
  response into a versioned generic current-state projection, rebuilding typed
  room summaries, members and power levels from it, tracking state/encryption
  completeness, evaluating push rules on plaintext and computing receipts,
  all reported as a `changes` value.
- **`Matrix_eio.Sync_service`** — the loop around `Base_client`, with
  reconnect backoff, persistence through `Store`, and the hook that runs the
  encryption driver over each response before anything else sees it. Local
  destructive operations advance a lifecycle generation, so a response fetched
  before a room forget cannot later fold or publish the forgotten room.
- **`Send_queue`** — sends over HTTP with retry, reusing one persisted
  transaction id across attempts, and encrypts through `Encryption` when the
  room is encrypted. Generic same-room dependency edges and their resolved
  event IDs survive restart; a failed parent blocks and a cancelled parent
  deterministically removes its transitive dependants. Forgetting a room drops
  its entire persisted graph and queue-owned media; an in-flight request is
  detached until its callback returns and cannot issue a compensating
  redaction or erase a deliberate post-forget request.
- **`Read_state`** — each room's read position and the unread counts drawn
  from it, retaining independent main/thread server and local positions.
- **`Push_evaluator`** — local evaluation of `event_match`,
  `contains_display_name`, `room_member_count`,
  `sender_notification_permission`, `event_property_is` and
  `event_property_contains`.
- **`Sliding_sync`** — MSC4186 endpoints over the wire shapes in
  `Matrix_proto.Sliding_sync`, with the `pos` loop in
  `Matrix_eio.Sliding_sync`; one-shot, loop and stream calls optionally send
  `set_presence` on every poll. Namespaced MSC4262 profile requests and
  arbitrary update/delete patches are typed and accumulated deterministically.
  The service-backed loop folds rooms, profiles, account data and extensions
  into `Base_client`, persists MSC4308 catch-up before the common position,
  rejects stale responses, and saves before callbacks. The public
  `Sliding_sync_state` path is retired; transactional private legacy-slot
  migration is complete and only private decoder compatibility remains.
  `Adaptive_sync` provides automatic fallback to classic `/sync` as a project
  enhancement, not pinned Rust parity.

## Authentication

Password, token/SSO, refresh, registration, bounded UIAA and OAuth 2.0 (`/auth_metadata`
discovery with MSC2965 and well-known fallbacks, RFC 7591 dynamic client
registration, PKCE S256, the authorisation-code grant over a loopback
listener, the RFC 8628 device-authorisation grant, refresh and RFC 7009
  revocation). Successful metadata discovery honors typed `Cache-Control` and
  `Expires` lifetimes with a 24-hour cap/fallback, monotonic cache age,
  explicit invalidation and stale-on-refresh-failure behavior.
  `Auth.register_uiaa` retains the
full registration body for one callback-driven retry. Session provenance routes
Matrix versus OAuth logout. The browser wait is bounded to five minutes by
default, and the provisional `/whoami` attaches the bearer token without
manufacturing a user id. Default dynamic browser registration uses the exact
ephemeral loopback URI already bound for that flow; caller-supplied metadata
remains unchanged. Scope selection accepts the stable and deployed
MSC2967 dialects without mixing them; account-management discovery likewise
maps stable and MSC4191 action names to the same typed actions.

`Qr_login` keeps the distinct `IO_ELEMENT_MSC4388` payload and
unauthenticated rendezvous discovery boundary. Like the pinned Rust SDK, an
opposite-intent MSC4388 scan reaches a typed unsupported-channel result; it is
not claimed as an end-to-end login. MSC4108 now has strict payload codecs,
conditional rendezvous, vodozemac-compatible ECIES and a typed two-party
secure-handshake core. Its application/OAuth/authentication/handover and Eio
flows are complete; Session progress/cancellation and its private-seed
persistence callback are complete, as are the `omatrix qr login` and `qr grant`
textual Base64 boundaries. Raw QR bytes cross the public/FFI boundary; raster
rendering/scanning and the mandatory confirmation callback remain caller/UI
owned. The current Synapse image has no MAS/OIDC; full two-role OAuth QR live
coverage remains P2.3, while the isolated port-8009 fixture covers rendezvous,
ECIES and cleanup.
Authenticated clients react to `M_UNKNOWN_TOKEN`, or proactively at an
injected absolute expiry/early window: one refresh is serialized per exact
failed token/generation, then buffered and GET-stream requests are replayed
once; one-shot POST streams and unauthenticated requests are not replayed.
Refresh tokens are retained, OAuth
refresh uses fresh metadata, password login can explicitly request a refresh
token, and `omatrix` does so. Login lifetimes become absolute expiry values,
persist in the profile and are restored into proactive refresh. Eio owns
refresh completion on the client switch rather than the initiating request,
and OAuth `invalid_grant` emits a typed session-invalid notification. Matrix
and OAuth rotations use a locked re-read/update/atomic-rename session
transaction. `omatrix login` offers both browser (`--oauth`) and headless
device-code (`--device-code`) OAuth flows.

## Libraries

Every `.ml` in every library has an `.mli`.

| Library | Modules | State |
|---|---|---|
| `matrix-chat.proto` | 19 | Protocol types and jsont codecs. The event families are separate compilation units re-exported under `Matrix_proto.Event`; `Base64`, `Common`, `Signed_json`, `Push` and `Sliding_sync` are the rest of the wire vocabulary |
| `matrix-chat.client` | 91 | Result-returning SDK over a `Fetch.t`; one logical request per call, with an optional injected monotonic whole-operation deadline and no owned driver loop; includes the Matrix-aware `Http_retry` policy and generic `Media_store` backend seam |
| `matrix-chat.eio` | 51 | Eio wrapper raising `Eio.Io`, and the driver loops |
| `matrix-chat.cli` | 1 | cmdliner terms for a Matrix command line |
| `matrix-chat.ui` | 20 | Observables, presentation, event/notification/directory/search clients, room list, room timeline, live locations, UTD reporting, room identity and runtime wiring |
| `matrix-chat.ui.sqlite` | 1 | `Matrix_ui_sqlite`, SQLite `Event_store.t` and `Media_store.t` backends |
| `matrix-chat.bot` | 10 | `Bot`, `Event`, `Room`, `Sent`, `Args`, `Plugin_store`, `Context`, `Logging` and the `Main` command line |

51 of `matrix-chat.client`'s 91 modules have a same-named `matrix-chat.eio` compilation
unit. The other 40 are pure crypto/state helpers or entry points, including
`Encrypted_attachment` and `Media_store`; callers normally reach them through
`Matrix_eio.Client.base`. `matrix-chat.eio` adds three modules of its own:
`Sync_service` (the `/sync` loop over `Base_client`), `Verification_service`
(a `Verification.Flow` driven from that loop) and the `Matrix_eio` entry point.

## omatrix

The installed CLI. Session state is per-profile under
`$XDG_DATA_HOME/matrix/profiles/NAME/`, as JSON.

| Command | Does |
|---|---|
| `login` | Password, or `--oauth` for the browser flow |
| `logout` | Clear the session and invalidate the token |
| `whoami` | Show the current session |
| `msg` | Send to a room or user, encrypting when the room is encrypted |
| `sync` | Follow the timeline, decrypting as it goes; positive `--count` stops after exactly that many responses, while Ctrl-C/SIGTERM cancels the nested poll and saves encryption state |
| `keys init` | Create or restore this device's encryption keys |
| `verify` | Verify another device with emoji (SAS), with an absolute `--timeout` |
| `qr login` | Create a textual Base64 MSC4108 payload, complete OAuth device activation, persist the session before follow-on work, and create a fresh encrypted SSSS store; requires an unused profile and supports timeout/cancellation |
| `qr grant` | Read a textual Base64 MSC4108 payload from stdin, use an SSSS credential file or `MATRIX_SSSS_CREDENTIAL`, confirm the check code and grant same-origin access |
| `backup enable` / `restore` / `status` | Manage the server-side room key backup; enable is idempotent for a matching current version and refuses divergent local/server keys |

The argument terms come from `matrix-chat.cli`, shared with `matrix-bot`. A
password is read from the file `--password-file` names or from
`$MATRIX_PASSWORD`; there is no flag that takes the password itself.

## Dependencies

The development switch uses local pins for `fetch`, `fetch-httpz`, `httpz`,
`proffer` and `proffer-httpz`. The currently published HTTPz repository lacks
the restructured packages; FR3 in `RELEASE_REVIEW.md` records the publication
gate and fresh-install check. These packages require **OCaml >= 5.5**, so the
SDK does too.

- `matrix-chat.proto` — `jsont`, `ptime`
- `matrix-chat.client` — `matrix-chat.proto`, `fetch`, `eio` (for `env#secure_random`),
  `jsont`, `xdge`, `uri`, `ptime`, `base64`, `cstruct`, `mirage-crypto`
  (AES), `mirage-crypto-ec` (Ed25519/X25519), `kdf` (HKDF, PBKDF2),
  `digestif` (SHA-256), `logs`, `fmt`
- `matrix-chat.eio` — the above plus `fetch-httpz`, `httpz`, `proffer` and
  `proffer-httpz`; its default transport is
  `Fetch_httpz.std` with `Matrix_client.Http_retry.default ~homeserver`; its
  `Httpz_tls.system` configuration supplies lazy system-trust TLS. A custom
  authenticator uses
  `Fetch_httpz.std ~retry:(Matrix_client.Http_retry.default ~homeserver)
  ~https:(Httpz_tls.client ~authenticator)`.
- `matrix-chat.cli` — `matrix-chat.client`, `matrix-chat.proto`, `fetch`, `cmdliner`, `fmt`,
  `logs`, `uri`
- `matrix-chat.ui` — `matrix-chat.client`, `matrix-chat.eio`, `matrix-chat.proto`, plus `markup`,
  `uutf`, `uunf`, `uucp`, `uuseg`
- `matrix-chat.ui.sqlite` — `matrix-chat.ui`, `matrix-chat.proto` and `sqlite3`
- `matrix-chat.bot` — all of the above plus `cmdliner`, `fmt.cli`, `fmt.tty`,
  `logs.cli`, `logs.fmt`, `eio_main`, `ptime.clock.os`
- Tests — `alcotest`, `http`, `eio.mock`, `fetch.mock`, `qcheck-core`,
  `qcheck-alcotest`

**Randomness is a capability.** `matrix-chat.client` does not depend on
`mirage-crypto-rng` and reads no global generator: `Matrix_client.Random`
wraps `env#secure_random` and every generator takes it as `~random`. HTTPz
initializes its TLS RNG lazily inside its own transport.

## Tests

64 executables and 1,457 cases/checks against `fetch.mock`, in-process fixtures
or a temporary local SQLite database, plus three source guards and the live
Synapse harness.

| Executable | Checks | Covers |
|---|---|---|
| `test_matrix_proto` | 100 | Checked Matrix JSON primitives/maps and hostile wire text, identifier grammar, event codec round trips, sync fixtures, pre-v11 redactions and typed stripped state events |
| `test_signed_json` | 3 | Canonical Matrix integer/value validation, duplicate and UTF-8 rejection, negative zero and Unicode key ordering |
| `test_session` | 24 | `Session` jsont codecs including current/previous fallback keys, Matrix/OAuth auth provenance, backward-compatible access-token expiry and legacy persisted integers, `Session_pickle` round trips and negative decodes, and commit-after-authentication Megolm ratcheting |
| `test_matrix_client` | 118 | Relative v3 and authenticated absolute-path request construction including retained homeserver prefixes, checked HTTPz route-template expansion and adversarial Matrix path segments, canonical URL/origin validation and exact query preservation, redacted transport diagnostics, auth header, bounded checked response decoding including duplicate members and depth, streaming-response scope/error mapping, typed policy/TLS failures, whole-operation deadlines across response/stream consumption, retry backoff and refresh with parent-cancellation preservation, origin restriction, transaction-id safety, serialized reactive and expiry-aware proactive refresh (including boundary, retry, reentrancy and one-shot rules), expiry-aware `Auth`, capability-resolved MXC URLs, MSC2246 preallocated media, encrypted upload/download, recursive E2EE log redaction, and typed atomic/profile-store failure boundaries |
| `test_composer_draft` | 3 | Room/thread draft isolation, lossless attachment and thumbnail metadata round trips, dirty-versus-flush persistence, restart recovery, clear semantics and missing-attachments compatibility |
| `test_live_locations` | 5 | Exact beacon start/stop/location sends, timeout and state errors, reactive replacement/redaction/close behavior, expiry and reload without resurrection |
| `test_matrix_ui` | 97 | Presentation classification as ruma's `membership_change`, preview eligibility, bounded observable reconciliation with granular and reset paths, incrementally indexed timeline aggregation/local echoes including caption refresh, replacement migration, compensating-redaction replacement and late-sync immunity, queued edits and configurable event filters, cache-only edit revision ordering/deduplication/redaction/provenance validation, `Gap`/`Timeline_start`/`Read_marker`, high-level receipt lookup/send/mark-as-read semantics, chunked cache with gap insertion and resolution (QCheck property), formatted reply/edit and extensible static-location sends, request lookup and pagination results, `Room_list.find`, `Event_cache.position`, ID/alias-aware `Runtime.join`/`leave` including durable inviter-gate recording and refused-join behavior, per-room typing-user observation, complete room-forget cleanup including discarded open timeline handles, `m.direct`, persistent queues and in-flight sync/send races, Offline/Live/Stopped runtime transitions and timeline cleanup on `Runtime.stop`, late-key request/dedup/re-decryption, isolated encryption-error callbacks, durable UTD grace/timing and incremental SQLite storage, bounded durable thread-cache event identity/order/metadata/subscriptions/restart/forget |
| `test_cli_backup` | 8 | Real CLI subprocesses: fresh login retires corrupt refresh markers; metadata/key rejection without profile changes, file errors, positional secret rejection without echo, empty backup and restored Megolm decryption |
| `test_profile_recovery` | 24 | Process death at every crypto write/clear boundary, journal validation and replay, account/OTK/ratchet/trust continuity, stale writers, concurrent process refresh, sync metadata, logout/re-login, new login coordination and failed-write recovery, retryable preparation and cancellable locks/exchanges |
| `test_matrix_bot` | 13 | `matrix-chat.bot` with no server: `Plugin_store` in memory and in a file (concurrent unique-temporary atomic writes, cleanup on rename failure, a broken file renamed aside), `Args.parse` and `argv`, spec construction and the generated command list, one-shot context/status/state-saving semantics, room-send readiness, and a bot over a mock `/sync` — event order, custom-event JSON/type delivery, membership reasons, own events and notices dropped, a raising handler leaving the room running, filters, cursor restart, commands, replies and `Sent.t` completion/timeout; the live notifier case covers authoritative member refresh after invite→join |
| `test_attachment` | 13 | AES-CTR/SHA-256 Rust vector, arbitrary chunk boundaries, metadata validation, tamper rejection, streaming length checks, output-failure propagation and shared authenticate-before-release download/spool tests |
| `test_media_store` | 22 | Local/MXC and thumbnail-key separation, Rust-compatible 20 MiB/400 MiB/60-day defaults, size and expiry retention, protection/ignore-retention flags, LRU eviction, key replacement/removal, generic backend dispatch, SQLite open/schema failures, restart/cadence/cleanup and idempotent close |
| `test_base64` | 4 | Canonical padded and unpadded input at every tail length, strict malformed/nonminimal rejection, and a full-byte round trip |
| `test_matching` | 6 | `Presentation.Html` sanitiser including `img` with `mxc://` only, canonical bounded image dimensions and Matrix colors, resolver hook and correct HTML void elements, and `Matching`'s fuzzy scorer |
| `test_room_list` | 19 | `Room_list` against rust-sdk's `room_list_service`: name normalization, sections, Space and exact tombstone version deduplication, every `Filter.t` constructor and combinator, both sorters including the recency-scale trap, preview eligibility/decryption and encryption-safe edit replacement, the summary fallback, diff-stream replay |
| `test_cs_api` | 93 | The endpoint modules added for CS API coverage, including validated well-known/authentication URLs, same-origin base-path discovery, bounded registration UIAA, stable/unstable fallback selection, cached Rust-default capability helpers, current/legacy delayed-event routes, and unauthenticated off-origin discovery validation |
| `test_peeking` | 9 | Checked legacy `/initialSync` and `/events` codecs, route/query encoding, optional-member handling, enum/shape rejection, Matrix error mapping, bodyless GET requests, and Synapse-compatible mixed event-stream raw JSON chunks |
| `test_encoders` | 128 | Per-codec regressions, strict typing-user decoding, receipt-driven stable `m.marked_unread` clearing and failure ordering, plus registration metadata and an 89-row guard driving every body-carrying entry point |
| `test_json_safety` | 28 | Hostile values through every JSON builder; member injection, duplication, round-trip fidelity |
| `test_e2ee_api` | 84 | `To_device`, `Room_keys`, SSSS key opening/optional reads and cross-signing import, `Backup`, Rust-compatible passphrase-protected portable room-key files with an explicit PBKDF work policy, `Dehydrated_device` create/rehydrate/delete, real-key decrypt and persistence boundaries |
| `test_olm` | 24 | Olm and Megolm self-tests including fallback rotation/retention/persistence, decrypt/usage recency and four-session LRU policy, recorded vectors, **7 vodozemac interop cases** |
| `test_verification` | 46 | Canonical signed-JSON validation, SAS state machine, negotiated emoji/decimal presentation, 64 emoji, MACs including the optional vodozemac normal/legacy SAS oracle, QR encode/decode, `next_method` codec/SAS/QR compatibility, in-room request validation, bounded per-device/global inbound flows, duplicate retention and cross-transport competition cancellation, cross-signing and private-identity publication |
| `test_oauth` | 87 | Discovery with typed Cache-Control/Expires precedence and monotonic lifetime cases, validated IDNA/authority/query-preserving endpoints, flow-specific browser/device metadata validation, redacted endpoint diagnostics, Proffer-served exact bound-loopback registration, hostile/slow request handling and deterministic listener shutdown, bounded RFC 7591 metadata, stable/MSC2967 scope selection, stable/MSC4191 account-action aliases, PKCE, code and RFC 8628 device grants, bounded browser wait, access-token-only `/whoami`, detached reactive/proactive OAuth refresh with typed invalidation and callback isolation, persisted refresh with retryable discovery/validation and conservative exchange failures in both modes, provenance-aware logout/revoke, and the pinned Rust MSC4388/MSC4108 URL, typed-header, expiry and payload boundaries |
| `test_encryption` | 62 | Two machines: key share and durable secret-request lifecycle, sender trust/ownership, UTD classification/withholding with persisted authenticated sender provenance, classic/sliding OTK semantics, weekly fallback-key rotation/restart and strict one-hour Olm unwedge/`m.dummy` repair, portable store import/export and pure MSC4268 bundles, authenticated MSC4268 inbound receipt with MSC4147 signed device proof and valid MXC/metadata, forwarded-key derived-session/canonical-sender validation, latest-per-room/sender persistence, strict 24-hour pending-bundle expiry and restart migration, encryption state from joined/invited/knocked/left rooms, request-error callbacks, strict version-1 backup-key import, serialized 100-session backup batches/checkpoints and crypto generation persistence |
| `test_encryption_account` | 5 | Account bootstrap, device keys and identity-preserving account ownership boundaries |
| `test_base_client` | 92 | `Push_evaluator` plus same-sync/persisted `m.push_rules`, room-specific own display names, `Read_state`, durable state projection/migration/completeness, stable/unstable marked-unread precedence across restart, `Base_client`, granular persisted-queue decoding and strict malformed graph/content/cache quarantine, collision-safe opaque transaction IDs and atomic random-source failure paths, atomic attachment graphs, typed media-upload substitution/progress/restart/cancellation, durable pending/in-flight caption mutation and replacement/cancellation races, durable DAG cancellation and in-flight compensating redactions, `Timeline`, and stale-writer-safe `Store` snapshots |
| `test_base_sliding` | 12 | Common `Base_client` application of sliding rooms, account data, receipts, typing, E2EE cursor and profile updates |
| `test_adaptive_sync` | 9 | Native sliding discovery, classic fallback, unsupported-endpoint fallback, shared cursor, retry, stop, cancellation and structured anonymous-session operation errors |
| `test_sliding_sync` | 56 | MSC4186 codecs, the `pos` loop, durable common room/list/extension/profile snapshots, stale-writer protection, native `/versions` capability discovery, MSC4308 catch-up ordering and stale-response guarding, MSC4262 profile-patch accumulation, presence and subscription wake/cancel behavior |
| `test_sliding_service` | 5 | Common service commit/hook ordering, cancellable profile subscriptions, persistence failure isolation, generation reset and to-device gating |
| `test_sliding_service_loop` | 11 | Common-fold resume, stale-generation/controller cancellation including MSC4308 thread state, unknown-position reset, presence wake, legacy migration and cursor precedence |
| `test_dehydrated_manager` | 8 | Manager startup/key creation, cached-only mode, callback lifecycle, failed rehydrate preservation, rotation/retry, skip-rehydrate and successful-delete boundaries |
| `test_dehydrated_pickle` | 5 | Dehydrated account pickle codec, malformed input and vodozemac/libolm interoperability boundary |
| `test_qr_ecies` | 5 | MSC4108 ECIES round trips, malformed/non-contributory keys, pending-channel consumption and the vodozemac fixture |
| `test_qr_secure_channel` | 16 | MSC4108 two-party handshake, typed authentication messages, check-code rejection, poisoning/concurrency and malformed-auth boundaries |
| `test_qr_application` | 14 | MSC4108 application login/grant approval, OAuth device authorization, authentication, handover, cleanup and progress/error callbacks |
| `test_qr_eio_application` | 6 | Eio application protocol ordering, grant cancellation and malformed bundle boundaries |
| `test_qr_secrets` | 9 | MSC4108 secret export/import, backup round trips and malformed private-identity/backup failures |
| `test_e2ee_integration` | 16 | Two clients over a mock homeserver: encrypted send/sync/decrypt, encrypted in-room SAS transport, client/store/account binding, SAS, own-device and other-user signature publication, rejection, stale-key failure and mismatched user-signing identity |
| `test_retention` | 5 | Policy precedence/clamping and validation, endpoint paths, unsupported-server fallback, stable state and store reload |
| `test_room_identity` | 3 | Initial and post-sync room-scoped typed verification/pin warnings, membership filtering and acknowledgement clearing |
| `test_nullable_responses` | 6 | Explicit-null optional fields in device, profile, presence, notification, search and directory responses while request/store codecs remain strict |
| `test_room_preview_knock` | 5 | Persisted invite/knock preview facts, MSC3266 and state fallbacks, federation hints, knock metadata/seen restart and exact invite/kick/ban requests |
| `test_well_known_policy` | 4 | Global well-known query/deny policy, inheritance and OAuth discovery behavior |
| `test_admin` | 3 | Typed server-admin whois decoding, omitted/nullable metadata, malformed responses and exact authenticated request construction |
| `test_cli_http_policy` | 5 | CLI policy defaults, validation, conversion and socket-free backend construction |
| `test_notification_settings` | 7 | Notification modes, keyword/rule updates and idempotent push-rule mutations |
| `test_notification_client` | 6 | Event notification lookup, decryption and duplicate suppression |
| `test_paginator` | 4 | Context ordering, directional exhaustion, reentrancy, rollback, not-found and overlap deduplication |
| `test_back_pagination` | 13 | Prioritized per-room backpagination, receipt-target backfill, serialization, cancellation and retry ordering |
| `test_thread_paginator` | 8 | Thread-root page/token progression, event-id deduplication, filter reset, failure retry, state publication and Rust-compatible loading/end no-ops |
| `test_room_conveniences` | 4 | ACL/IP-safe room routing and permalinks, direct targets and batched DM mutation, version-aware roles and invite projections |
| `test_room_details` | 4 | Complete-cache no-I/O, authoritative lazy-member replacement, duplicate/fallback labels, role/account/service classification, failure preservation and unknown-room rejection |
| `test_room_directory_search` | 4 | Public-room page/reset/remote-server request contracts, terminal no-op, failure retry and Rust-compatible result-count page calculation |
| `test_search_service` | 4 | Server event-search relevance/page order, reset, token retry, terminal and overlapping request guards |
| `test_space_graph` | 4 | Mutual space links, removed/left parents, deterministic cycle breaking, multiple parents and explicit-order/timestamp traversal |
| `test_relations_revisions` | 5 | Exhaustive network edit history, cyclic-page/event deduplication, sender/type/relation/redaction validation and raw relation filters |
| `test_http_policy` | 5 | `Fetch_httpz.std` option wiring, custom TLS construction, canonical Matrix route classification, and selective POST status/connection retry behavior |
| `test_event_store_external` | 4 | External event-store backend dispatch, persistence and lifecycle contracts |
| `test_lazy_event_store` | 21 | Lazy metadata/tail loads, one-chunk hydration, local-before-network pagination, stable identity and gap preservation, hidden local-echo promotion, retryable malformed/missing/store failures, incremental/flush/forget behavior, plaintext policy, memory/SQLite validation and post-hydration SQLite restart topology |
| `test_media_fetcher` | 10 | Replaceable cache-aware file/thumbnail fetching, encrypted verification, cache promotion and local-URI rejection |
| `test_pinned_events` | 11 | Pinned-event projection, fetch/cache behavior, reactive updates and lifecycle cleanup |
| `test_push_bootstrap` | 6 | Push-rule bootstrap, same-sync activation, malformed-update preservation and restart |
| `test_recovery` | 17 | One-shot state derivation, stable/unstable marker precedence and malformed data, ordered enable/recover/repair/reset/disable/delete-all effects, partial-write boundaries and master-plus-device backup signatures |
| `test_homeserver` | 60 | **Against Synapse 1.159.0 in 109.397s**: registration and whoami, a room's create/name/topic/invite/join/members, text and caption-mutated atomic original/thumbnail attachment sends through the queue and out of the other client's sync with byte-exact media downloads, persisted room preview and knock accept/decline/ban, public-directory/event search, thread-root pagination and lazy member refresh, legacy and MSC2246 preallocated media uploads plus a thumbnail, one MSC4186 `sync_once`, a room id with `!` and `:` in the path, stable room-retention state and the unsupported MSC1763 policy fallback; an encrypted room end to end, an encrypted attachment upload/event/receiver-side streaming download, the Megolm session ratcheting and a reply, SAS over the sync loops including strict encrypted in-room SAS transport, a key backup a second login restores while driving durable timed UTD reporting, recovery enable/rotate/disable plus fresh-login SSSS backup restore, trusted pre-join-history import and live peer-key dehydration continuity; `matrix-chat.ui` reaction/edit/redaction aggregation with its diff stream plus network edit-history ordering and a queued static-location round trip, room-list ordering and Unicode filtering, back-pagination across a limited sync, reload from SQLite, a local echo from queued to synced, state events as typed items, pagination to the timeline start reported as `Reached_start`, own reactions, a sanitised formatted reply and redactions, accepting an invite through `Runtime.join`, an encrypted room through the runtime, a sync that cannot succeed, live typing users and complete runtime forget with stale-handle cleanup; room-list sections from tags/DMs/invites/leaves with diff replay, unread counts across a read receipt, name against activity order, search normalization, previews skipping state, an encrypted room's plaintext preview, `m.marked_unread`, edited previews; a disjoint limited sync kept behind a gap and filled in place, ciphertext-only reload decrypted through the runtime, the read marker moving with a receipt, `Runtime.stop`; `matrix-chat.bot` invited to an encrypted room, joining by itself, answering `!ping` with a reply relation and `!help` with its generated list, both decrypted by the human, stopping with no fiber left, and restarting over the same state and SQLite store to skip and then handle what arrived while it was down; the five example plugins and the notifier (`bin/matrix-bot`) driven end to end through `Bot.run` — echo and commands in encrypted rooms, the moderator's strikes kept across a restart, the logger's back-fill read back out of SQLite cold; the `v1` relations, hierarchy and room-summary endpoints, plus the explicit legacy peeking capability outcome. Skipped unless `MATRIX_TEST_HOMESERVER` is set; see `test/integration/synapse.sh` |

### The vodozemac oracle is optional

`test/vodozemac-oracle/` is a Rust project (vodozemac from crates.io) that the
Olm interop tests drive over stdin. It is **not** built by dune and needs
`cargo`. Without it those 7 cases print `SKIP` and pass; the recorded fixtures
under `test/fixtures/olm/` still run. To build it:

```
cd test/vodozemac-oracle && cargo build --release
```

The test finds the binary by walking up from the working directory, or from
`$VODOZEMAC_ORACLE`. CI does not build it.

## Known limitations

The most consequential current gaps are below. The handoff details and
acceptance tests are in `TODO.md`.

1. **Synapse remains the only full reference harness.** The hermetic suite is
   entirely `fetch.mock`; the 60-scenario integration reference is Synapse
   1.159.0 (109.397s). The scripts support independently named containers/data
   directories and sentinel-guarded purge. The combined local runner supplies
   unique names and an exit cleanup trap, but CI still needs per-job port/socket
   ownership, job wiring and a Docker-enabled runner.
   Dendrite v0.15.2 has a named five-case portable-room smoke profile (5/5
   passing). Its peeking case is explicitly rejected with
   `M_GUEST_ACCESS_FORBIDDEN`; threaded receipts time out, MSC2246 preallocation
   is rejected despite advertised MSC3916, simplified sliding sync is
   unavailable and federation is disabled. No federation is exercised.
2. **The default transport has no HTTP/2 or connection reuse.** `fetch-httpz`
   opens a connection per request. Applications can inject the optional
   `Fetch_curl.std` backend through the existing `~fetch` seam for pooling and
   HTTP/2, but `fetch-curl` is not yet in opam-repository or this project's
   CI/release validation. The optional `Client.with_request_timeout`
   whole-operation deadline bounds either transport but does not itself provide
   pooling.
3. **Some consumers still sit outside the durable room projection.** Classic
   `/sync` and the service-backed sliding-sync path share the `Base_client`
   fold, including profiles and MSC4308 stale-response protection. The public
   `Sliding_sync_state` path is retired; transactional private legacy-slot
   migration is complete and only private decoder compatibility remains.
   `Adaptive_sync` supplies automatic classic fallback as a project enhancement,
   not pinned Rust parity. Decrypted
   `latest_event` persistence is deliberately opt-in through `Store_plaintext`;
   the secure default retains ciphertext.
4. **Private-key loading still requires an application-supplied credential.**
   `Secrets.open_secret_store` validates PBKDF2 passphrases and Base58 recovery
   keys, and `Secrets.import_cross_signing` atomically imports matching private
   signing keys after a fresh own-user `/keys/query`. The credential is not
   retained or logged, and the core keeps imported private material in memory;
   the QR CLI publishes received seeds only to encrypted remote SSSS. There is
   no automatic credential discovery or encrypted local key store. Reactive
   refresh after `M_UNKNOWN_TOKEN`, expiry-aware proactive refresh, profile persistence of
   the issuing endpoint's initial expiry, runtime-owned Eio completion and
   typed OAuth `invalid_grant` notification are implemented. Refresh stampede
   protection is per `Client.t` by default. Passing `~store` to Eio client/OAuth
   refresh helpers adds a separate cancellable cross-process exchange lock,
   reloads completed rotations and persists before notifying callbacks.
   `omatrix` enables this for both Matrix and OAuth sessions. A failed or
   interrupted exchange leaves an uncertainty marker and requires re-login
   before potentially consumed tokens can be reused. OAuth discovery/validation
   happens before marking a token uncertain and remains retryable. Fresh login
   waits for refresh and persists new credentials before clearing any old marker,
   including malformed data; failed writes and metadata updates retain it. Session,
   base and crypto persistence now share the profile lock; session updates
   re-read the latest file, base snapshots reject stale byte fingerprints and
   crypto snapshots reject stale generations. Refresh resolves its shared
   operation before invoking notification callbacks, including reentrant ones;
   with `~store`, credentials have already been persisted and callbacks must
   not write them again.
   Separately, OCaml strings and Mirage Crypto key values are GC-managed and
   may be copied, so the SDK cannot promise reliable in-memory zeroisation.
   One-shot protocols discard secret references promptly and transient random
   read buffers are cleared, but long-lived keys and intermediate immutable
   strings can remain until reclaimed. Deployments whose threat model includes
   process-memory inspection should isolate the crypto process and restrict
   core dumps, swap and debugger access.
5. **Media fetching is replaceable at the client boundary.** `Media_fetcher`
   provides the cache-aware default and an injectable file/thumbnail fetcher;
   encrypted bytes are verified before release and local send-queue URIs never
   reach the network. Content-scanner policy can use this seam. IndexedDB and
   other platform-specific stores remain deferred.
6. **Crypto snapshots recover interrupted writes and clears.** Every profile
   store uses the persistent advisory lock. Base snapshots reject stale loaded
   byte fingerprints; crypto snapshots reject stale generations. Complete 0600
   redo journals precede crypto component writes and survive until an even
   commit marker is durable. Load replays a pending journal after process death,
   preserving the account, ratchets and trust metadata. Native replacements sync
   the file and parent directory. Legacy odd markers without a journal and
   malformed journals still fail closed and require explicit recovery. Raw
   `Profile_store` component reads do not provide this snapshot guarantee.
7. **Sliding sync retains only private migration compatibility.** The
   service-backed MSC4186 response folds rooms, profiles, account data and
   extensions into `Base_client`; MSC4308 catch-up is persisted before the
   common position and stale responses cannot mutate either state. The public
   standalone `Sliding_sync_state` path is retired; transactional private
   legacy-slot migration is complete and only private decoder compatibility
   remains. `Adaptive_sync` provides automatic classic `/sync` fallback as a
   project enhancement, not pinned Rust parity.
8. **Thread continuity is a bounded shared-cache projection.** Persisted
   `ThreadInfo`, rich server-backed `Thread_list`, Runtime lifecycle, pinned
   projection, room-context `Event_focused`, thread relations, receipts/backfill,
   prioritized invalidation and durable per-(room,root) event identity/order,
   pagination metadata, receipts/unread, subscriptions and forget cleanup are
   present. This deliberately does not claim a second independent Rust
   `LinkedChunk` for threads; the bounded shared `EventCache`/`EventStore`
   projection retains each root and at most 255 replies. Ordinary room history
   separately supports lazy persisted-prefix hydration.
9. **Device continuity has one external QR-login validation gap.** MSC4108
   application, OAuth/device authorization, authentication, secret handover and Eio flows
   are complete, as are the strict codec, conditional rendezvous,
   vodozemac-compatible ECIES and typed two-party handshake. The textual Base64
   `omatrix qr login`/`qr grant` boundaries, Session progress/cancellation and
   persistence callback are complete; raw QR bytes and the mandatory
   confirmation callback cross the public/FFI boundary. Raster
   rendering/scanning remains caller/UI-owned as a deliberate boundary. The
   isolated port-8009 fixture passes rendezvous, ECIES and cleanup, but has no
   MAS/OIDC; only full two-role OAuth QR live validation remains P2.3.
   MSC4388 remains the separate typed unsupported channel. Recovery Manager,
   UIAA identity reset, conditional rebackup, device-key preupload and
   dehydration pickle/create/rehydrate/manager flows are complete.

## Not in scope

Not release blockers unless scope changes: store encryption, widgets, the
content scanner/local search index, MatrixRTC, IndexedDB, experimental Olm v2
and Megolm v2, MSC3956 extensible encrypted events, MSC4274 inline media
galleries, MSC4362 encrypted state, MSC4385 push secrets, X.509 identity,
MSC4426 status/call automation, and Element-specific recent-item stores. The
`matrix-sdk-ui` layer and event-cache persistence were out of the original
parity scope and are no longer wholly absent: `matrix-chat.ui` and
`matrix-chat.ui.sqlite` cover their central path, and the bounded P1.2/P2.1 service
checklist is complete.
