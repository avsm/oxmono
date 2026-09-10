## unreleased

Bot invitations now identify their sender from stripped membership state,
including pending invitations returned at startup and later invitations.

Rename the opam package to `matrix-chat` to avoid the existing terminal UI
package. Public Dune libraries use `matrix-chat.*`; OCaml module names and
the `omatrix` and `matrix-bot` executable names are unchanged. Remove the
AI disclosure metadata and contributor instructions requiring it.

Backup restore now requires `--recovery-key-file FILE` and validates the
server's backup algorithm and public key before loading or changing crypto
state. Positional keys are rejected without echoing them. CLI regression tests
cover rejection without mutation, empty backups and restored event decryption.

Crypto snapshot saves and clears now retain a private redo journal. Loading
replays interrupted transactions after process death; stale handles still
cannot overwrite the recovered generation. Native atomic replacements sync
the parent directory as well as the file. Tests interrupt each component write
and deletion, then recover the identity, one-time keys, Megolm ratchets, secrets
and trust policy in a fresh process.

Eio client/OAuth refresh helpers accept `~store` for cross-process coordination.
`omatrix` enables it for both Matrix and OAuth sessions. Token exchanges use a
separate cancellable lock, reload rotations from other processes, preserve sync
metadata and persist before notifying callbacks. Interrupted or failed
exchanges leave a marker requiring re-login before potentially consumed tokens
can be reused. Concurrent logout never resurrects the login. Fresh login now
waits for active refresh and durably saves new credentials before retiring an
old or malformed marker. Failed writes and metadata updates retain that marker.
OAuth discovery and validation run before marking a token uncertain, so their
failures remain retryable in both reactive and proactive refresh.

Encrypted local storage is explicitly deferred: profile tokens, private keys,
session state and journals remain unencrypted, with no local unlock prompt or
keyring integration. Filesystem access remains the at-rest security boundary;
reliable OCaml key zeroisation is not claimed. README and TODO document the
current boundary and the requirements for a future encrypted store.

Release scripts require the live oracle and homeservers and provide an
isolated fresh-switch installation check. The published HTTPz repository still
lacks four required packages; distribution remains blocked on publishing the
restructured dependency source. The MSC4108 OAuth/MAS CLI is explicitly
experimental until complete two-role live validation exists.

URI-template expansion uses `Httpz_uri.Template` from `httpz.uri`.
`matrix-chat.client` declares that Findlib dependency alongside `httpz` for HTTP
dates; both libraries come from the existing `httpz` opam package.

Two large pieces of work, neither released yet: the port off the `nox-*`
package family onto opam-repository packages plus the `fetch` HTTP capability,
and closing the feature gap against matrix-rust-sdk. The development tree builds with five locally pinned packages (`fetch`,
`fetch-httpz`, `httpz`, `proffer` and `proffer-httpz`). The published Git source
still lacks the restructured packages, as recorded in `RELEASE_REVIEW.md`.

The parity baseline for these notes is matrix-rust-sdk
`523b5af53a8fd9fae9e2bc981bfb01ac86fd2890`. The adjacent checkout at
`f4b9512df23332fce1bd26037ef7a2387af2ced2` was consulted for later design
advice, but has not been exhaustively audited; references to Rust parity below
mean the pinned baseline unless a newer observation is named explicitly.

The unreleased crypto snapshot API adds optional authenticated sender
provenance to the public `Encryption.withheld` record. Code constructing that
record directly must initialise `sender_user` (normally to `None`); snapshots
written before the field existed continue to load.

E2EE is the headline: Olm and Megolm are now byte-for-byte interoperable with
libolm and vodozemac in both directions, and encryption is wired into the sync
loop and the send path rather than sitting beside them.

### Feature implementation baseline

This documentation snapshot is reconciled to code SHA
`948229e367f16abcc7b609310585904cdff0983d`: the hermetic suite has 59
executables and 1,295 checks, while the main Synapse harness passes 59
scenarios in 112.266s. The isolated MSC4108 rendezvous harness runs on port
8009 and passes rendezvous creation, bidirectional ECIES-encrypted messaging
and cleanup/cancellation; it has no MAS/OIDC, so the full two-role OAuth QR
flow remains an external deployment check.

The delivered UI and verification surfaces include high-level
`Room_timeline` receipt controls, clearable display-name/avatar/custom profile
fields, encrypted in-room SAS routing and sender/device hardening, and
consistent cancellation of competing verification requests. Live MSC3814
dehydrated-device coverage now preserves real room keys through the
create/get/delete/rehydrate continuity path.

### Added

- **Security and correctness audit hardening.** Attacker-controlled decode and
  persistence paths no longer contain `assert false`/`failwith`, with a source
  guard preventing their return. Malformed persisted queue attachment content,
  extension content and cache identifiers are quarantined. Nested request
  bodies redact E2EE secrets;
  inbound Megolm ratchets commit only after authentication; backup import
  distrusts unauthenticated outer metadata; retry, KDF and inbound-verification
  work are bounded; Base64 is canonical; authenticated media spooling is
  shared; and persistence failures remain typed through rollback/flush paths.
  The audit dispositions in `TODO.md` distinguish completed fixes from the
  excluded at-rest-encryption/packaging work and the remaining foreign
  secure-memory design.

- **Matrix-aware HTTP retries.** The Eio and CLI default transports use
  HTTPz's request-level retry veto to add the replayable, read-like
  `POST /_matrix/client/v3/keys/query` route to Fetch's ordinary idempotent
  method policy. Exact canonical origin/path matching appends that route to the
  configured homeserver, retaining deployment prefixes while excluding key
  claim, key upload, sync, mutations, near misses and encoded separators from
  both status and connection-failure retries. `Matrix_client.Http_retry`
  exposes the same policy for injected HTTPz or curl transports. The OAuth
  loopback listener also supplies the monotonic clock required by current
  Proffer.

- **Legacy peeking and current delayed-event APIs.** `Matrix_client.Peeking`
  and its Eio facade expose per-room `initialSync` and the historical event
  stream without disguising unsupported server behavior as `/sync` fallback.
  `Delayed_events` retains its Synapse-compatible legacy defaults while adding
  the current MSC4140 unified send, direct get/list and unauthenticated
  capability-style action routes. Checked codecs, escaped routes and live
  Synapse peeking cover the boundaries; Dendrite's explicit peeking probe
  remains a reported `M_GUEST_ACCESS_FORBIDDEN` portability result.

- **Proffer-owned OAuth loopback redirects.** Browser OAuth now uses Proffer
  and HTTPz for request parsing, limits and typed responses instead of carrying
  a private HTTP/1 parser/framer. The listener binds an ephemeral loopback port
  before registration, accepts only its exact GET route, survives malformed,
  slow, oversized and wrong-path requests, emits non-cacheable HTML, and closes
  deterministically after the valid callback.

- **Checked endpoint route templates.** Matrix endpoint modules now expand
  pre-parsed HTTPz URI templates through one checked `Matrix_client.Route`
  layer. It preserves Matrix identifier punctuation while encoding path
  delimiters, retains empty state keys and base-path prefixes, rejects missing
  or invalid bindings, and has a source guard against reintroducing ad-hoc
  endpoint percent encoding.

- **Typed OAuth and MSC4108 cache/validator headers.** OAuth metadata uses the
  shared Fetch Cache-Control codec, honors HTTP-date Expires with normal
  precedence and ages cached results on a monotonic clock. Rendezvous channels
  retain typed strong/weak ETags and media parameters, encode conditional
  requests through Fetch, and enforce the earliest parsed or caller-provided
  expiry.

- **Shared URL and persistence boundaries.** Configured, well-known, OAuth and
  QR URLs now use HTTPz's validated canonical representation for policy and
  transport. Homeserver base-path prefixes are retained and endpoint paths are
  appended beneath them as in Ruma; userinfo, invalid ports and homeserver
  query/fragment components are rejected before `Uri.t` can reinterpret wire
  text. Profile, base, crypto and plugin files share a cancellation-safe 0600
  atomic writer, and push rules/server ACLs share one Matrix glob matcher.

- **Reproducible CI compiler.** The Tangled workflow creates a local
  `ocaml-base-compiler.5.5.0` switch before pinning or resolving packages,
  instead of relying on the OCaml version in Nixery's moving nixpkgs snapshot.

- **Checked JSON and runtime boundary cleanup.** `Matrix_proto.Json.Codec`
  centralizes checked wire codecs and generic JSON validation; canonical signed
  JSON rejects fractions, unsafe integers, invalid UTF-8 and duplicate names
  while retaining the raising compatibility API. Jsont's permissive migration
  integers are confined to reviewed on-disk schemas, preserving numeric-string
  and pre-0.2 stores without relaxing Matrix responses. Matrix identifiers now
  follow current server-name and domainless-room grammar. Secret comparisons
  use `Eqaf`, OCaml 5.5 stdlib/Ptime helpers replace local byte/time routines,
  SQLite open/schema/close work runs on an Eio systhread, and queued upload
  bytes persist as unpadded Matrix Base64 for retry.

- **Coordinated profile snapshots.** Base and crypto stores now share the
  persistent profile advisory lock with session updates. Base snapshots hash
  the exact loaded bytes and reject a stale writer instead of overwriting a
  newer snapshot. Crypto snapshots use an even/odd generation marker around
  their multi-file transaction: stale handles reject before mutation and an
  interrupted write leaves an odd marker that fails closed until an explicit
  retry or clear completes it. Snapshot temporary files are created at 0600.

- **Bounded crypto queues and Olm recency.** Key-backup uploads are stable,
  100-session batches, serialized per encryption driver and checkpointed after
  every successful request so a later failure retries only the remainder. Olm
  session lookup now prefers the session that most recently decrypted, while
  successful encryption/decryption drives a separate four-session-per-device
  LRU; precise recency timestamps survive old-pickle migration and restart.
  A failed decrypt over an existing session now follows Rust's strict one-hour
  unwedge cutoff, including clock rollback: it force-claims a replacement and
  persists one encrypted `m.dummy` until acknowledgement. The pinned/current
  Rust behavior has no general Olm session-expiry requirement beyond this
  one-hour unwedge; optional expiry is design/scale hardening. Backup
  rotation/deletion handling follows Rust's version-addressed behavior. HTTP
  ETag coordination is optional project hardening, not a pinned-Rust parity
  requirement.

- **Portable room-key files.** `Room_key_export` reads and writes the Matrix
  Megolm-session armour using PBKDF2-HMAC-SHA512, AES-256-CTR and
  HMAC-SHA256. It clamps the IV counter bit, validates the unsigned 32-bit KDF
  count, accepts stable and legacy shared-history fields, and decrypts the
  pinned matrix-rust-sdk fixture. `Encryption` imports and exports those
  sessions while retaining sender, forwarding and shared-history metadata;
  a deterministic OCaml fixture is also decrypted and compared field-for-field
  by the pinned Rust SDK. Wrong passwords and ciphertext tampering are rejected
  before plaintext JSON is exposed. Pure MSC4268 bundle codecs/build/import
  are included; `Runtime.join` captures a current invited-room inviter before
  `/join` and records the durable acceptance gate only after a successful
  response. Startup expiry is strict at 24 hours, and sync reconciles every
  gate against current room state, clearing `Left`, `Knocked`, and missing
  rooms. The alias-aware `Runtime.join_room` resolves an alias before reading
  invitation state, forwards the original alias and explicit `via` list to
  `/join`, and records the inviter against the room ID returned by the
  homeserver. Authenticated inbound `m.room_key_bundle` receipt now checks the
  MSC4147 signed `sender_device_keys` proof, validates the MXC URI and
  encrypted-file metadata, and durably retains the latest bundle per
  room/sender. After joining, the receiver re-queries inviter trust, downloads
  and decrypts the bundle, imports only same-room keys from a cross-signed
  inviter, retries transient failures and discards malformed or missing media.
  The sender restores missing keys from backup before building the bundle. A
  trusted pre-join-history flow passes against Synapse.

- **Retention-aware media storage.** `Media_store` has generic, in-memory and
  SQLite backends keyed by MXC/local URI plus requested file or thumbnail
  format. It uses the pinned matrix-rust-sdk defaults — 20 MiB per item,
  400 MiB total, 60 days since last access and daily cleanup — and supports
  protection, LRU/expiry cleanup and idempotent key replacement/removal. The
  send queue protects local upload bytes before its own record is persisted,
  serves retries from that cache, moves successful uploads to their remote MXC
  key and reclaims cancellations or crash orphans. `Runtime.create` can build
  this durable queue directly from caller-owned queue and media stores.

- **Capability-resolved MXC URLs.** `Media.mxc_to_http_resolved` and its Eio
  facade consult the cached server versions and choose authenticated v1 media
  paths for Matrix 1.11/MSC3916 servers or the legacy v3 path otherwise. The
  existing pure helpers remain available when callers already know the route.

- **SSSS cross-signing recovery.** `Secrets.open_secret_store` validates the
  Rust-compatible passphrase/recovery-key forms and
  `Secrets.import_cross_signing` atomically restores matching private
  cross-signing seeds after a fresh own-user `/keys/query`. Secret reads and
  writes validate UTF-8; the backup recovery key uses Rust's unpadded-Base64
  secret representation. A full self-signing import re-queries and marks the
  own device verified. The Eio `Verification_service` accepts this handle;
  credentials are caller-supplied and private material remains memory-only.

- **Recovery lifecycle.** `Recovery` checks stable and unstable account data in
  Rust's order, enables SSSS plus a signed room-key backup, recovers all three
  cross-signing seeds and the optional backup secret, rotates the SSSS key,
  repairs only typed missing/inconsistent backup-key cases, disables one backup
  or deletes every version, and preserves every documented partial-write
  boundary. Mock coverage includes malformed markers and repair ordering; live
  Synapse coverage enables, rotates and disables recovery, then restores an old
  Megolm session through SSSS on a fresh login.

- **Reactive and proactive access-token refresh.** Authenticated clients can opt into a
  serialized refresh after `M_UNKNOWN_TOKEN`, attributed to the exact failed
  token to prevent stampedes. Buffered and GET-stream requests replay once;
  one-shot POST streams and unauthenticated requests do not. OAuth refresh
  fetches fresh metadata, refresh tokens are retained, and the password-login
  CLI explicitly asks supporting homeservers to issue one. `omatrix` persists
  rotated Matrix/OAuth credentials through a locked re-read/update/rename, so
  concurrent session metadata and token changes are preserved. A separate
  expiry-aware constructor refreshes at an injected-clock deadline/window,
  shares proactive/reactive work, updates the session and absolute expiry in
  one critical section, and safely permits a reentrant persistence hook.
  Password/token/OAuth login expiry now survives the versioned `omatrix`
  profile and is restored on startup. Eio owns refresh completion on the client
  switch, so initiating-request cancellation cannot abandon a rotation; OAuth
  `invalid_grant` has a typed one-shot notification. Network refresh
  serialization remains per client.

- **Durable outbound key withholding.** Pending and acknowledged
  `m.room_key.withheld` notices now survive crypto-store reloads. Retries keep
  their original transaction identifier and exact JSON body, acknowledged
  notices stay suppressed after restart, and rotating a Megolm session
  discards only notices scoped to the retired session while retaining
  device-wide `m.no_olm` suppression. Incoming direct and MSC4268-bundled
  withholding facts retain the authenticated sender user across restart, while
  bundle wire JSON remains interoperable and provenance-free.

- **Sync-hydrated push rules.** A valid `m.push_rules` account-data event is
  active before notifications in that same response are evaluated and restores
  from the base store after restart. Malformed events preserve the preceding
  rules. Event reports now emit Matrix 1.18's reason-only body; the obsolete
  optional `score` argument remains an ignored source-compatibility shim.

- **Exact OAuth loopback registration.** Default browser dynamic registration
  now uses the exact ephemeral loopback URI already bound for the flow, while
  caller-supplied metadata remains unchanged. The registered URI is reused for
  authorization and token exchange. Callback parsing rejects duplicate
  response parameters across both the query and fragment.

- **MSC4388 QR-login boundary.** `Matrix_client.Qr_login` and its Eio facade
  encode/decode the `IO_ELEMENT_MSC4388` payload, round-trip the pinned Rust
  vector, probe the unauthenticated rendezvous endpoint, treat 403/404 as a
  capability outcome and return the same typed unsupported secure-channel
  result as the pinned Rust revision. MSC4108 now has strict codecs,
  conditional rendezvous, vodozemac-compatible ECIES and a typed two-party
  secure-handshake core. Its transport-independent application protocol and
  Eio orchestration drive both OAuth device-authorisation roles through
  explicit approval, authenticated device activation, secret handover, device
  signing/trust and backup setup. The Eio Session adds progress, cancellation
  and an optional private-seed persistence callback. `omatrix qr login` and
  `qr grant` complete the textual Base64 flow, with exact MSC4108 creation,
  unused-profile and check-code checks, immediate OAuth-session persistence,
  fresh encrypted SSSS setup, credential-file/environment input and
  same-origin rendezvous. Raw QR bytes cross the public/FFI boundary; raster
  display/scanning and the mandatory confirmation callback remain caller/UI
  owned. An isolated flag-enabled Synapse fixture validates the MSC4108
  rendezvous/ECIES layer; it supplies no OAuth/MAS service, so only the full
  two-role OAuth login remains an external deployment check. MSC4388 remains
  the separate typed unsupported channel.

- **Room-transported verification.** `Verification_service.request_in_room`
  tracks the request event ID, encrypts follow-ups when needed and is fed
  decrypted room events by both sync loops. Strict room/to-device addressing,
  target/device/timestamp validation, own-echo filtering and an exact room
  event allowlist prevent cross-transport confusion. Exact replays retain the
  active request; a different request from the same user cancels both flows
  with `m.user`, routed through each flow's original room or device set. A full
  SAS exchange passes through an encrypted Synapse room.

- **High-level timeline receipts and clearable profiles.** `Room_timeline`
  exposes single/batched receipt sends, `mark_as_read` and latest-own-receipt
  lookup with Rust-compatible own-event redirection, raw relation targets,
  explicit thread scope, monotonic local state and atomic `/read_markers`
  batching. Display names, avatars and stable/MSC4133 custom profile fields
  each have an explicit JSON-null clear operation in the pure and Eio APIs.

- **Dehydrated-device lifecycle.** `Dehydrated_device` now creates an
  independent Olm account, uploads signed device, one-time and fallback keys,
  and encrypts an interoperable `org.matrix.msc3814.v1.olm` legacy pickle.
  Rehydration uses a temporary account, defensively drains the paged to-device
  queue, imports real room keys into the unchanged primary identity and deletes
  only after a complete drain. The Eio manager publishes lifecycle events,
  preserves the old SSSS key after failed recovery, uploads immediately and
  rotates weekly from the local cache. Clock-controlled tests cover retry and
  cancellation; Synapse covers create/get/delete/rehydrate plus a real peer
  room-key queued for the dehydrated device, imported into a fresh machine and
  used to decrypt the captured encrypted room-event echo without changing the
  primary identity.

- **Durable send dependencies and uploads.** Generic same-room send-queue
  entries can name queue-local parents. Unresolved edges and typed event/upload
  results survive restart; failed parents block their children, successful
  parents resolve atomically, and cancelling a parent removes its transitive DAG
  once in deterministic callback order. Original/thumbnail uploads persist
  clear bytes or encrypted ciphertext plus validated metadata, stream to media
  transport and report monotonic progress only reaching the terminal total
  after durable propagation. Cancelling an in-flight send persists an optional
  reason and turns a successful race into exactly one redaction with a distinct
  stable transaction id; failures remove the local DAG. `send_attachment`
  persists the original, optional thumbnail and one visible event atomically;
  clear/encrypted results replace conflicting URL/file fields before plain or
  encrypted send. Cancellation discards a concurrently completing upload,
  while a compensating redaction replaces rather than duplicates its local
  echo. Pending captions now update the durable attachment node in place;
  edits racing the final event keep its wire payload stable and persist one
  last-write-wins replacement with a distinct retry-stable transaction ID.
  Caption removal restores the logical filename, caption-owned vendor extras
  cannot reappear, and local echoes migrate exactly once without overwriting a
  sync-confirmed event. The full edited upload/event/download path passes
  against Synapse. The queue now protects local upload bytes before persisting
  its graph, reads retries from that cache, moves successful uploads to the
  remote MXC key and removes cancellation/crash orphans. Memory and SQLite
  media stores cover restart, retention and cleanup cadence, and
  `Runtime.create` accepts separate durable queue/media stores.

- **Complete local room forgetting.** After the homeserver accepts `/forget`,
  `Runtime.forget` invalidates older in-flight sync responses and removes the
  room from the base/store projection, receipts, open timeline and event-cache
  chunks, thread-subscription rows, queue graph and queue-owned local media.
  In-flight queue transports are detached: their callbacks cannot recreate a
  request or send a compensating redaction, while a deliberate new post-forget
  request remains independent. Existing observable handles are cleared, and
  room-list/identity projections refresh immediately. A direct room also gets
  Rust-style best-effort `m.direct` GET/PUT cleanup; failure is logged without
  undoing the already successful room forget, while a successful update is
  reflected in the persisted local account-data projection.

- **Cross-signing TOFU pins.** Validated other-user identities persist their
  first master-key pin independently of verification history. Rotation now
  exposes a distinct pin violation; acknowledging the new pin does not mark it
  verified, while interactive trust re-pins and verifies. Room identity
  warnings distinguish pin and verification violations.

- **Persisted sliding-sync and MSC4262 profile state.** Sliding-sync requests
  can enable the namespaced profiles extension. Responses validate user IDs,
  preserve
  arbitrary update fields and explicit null deletions, and drop a complete
  profile on user null. The separate sliding state persists its complete
  versioned room, list, extension, profile and position snapshot, restores it
  before polling, saves before callbacks/position advance, rejects stale
  writers and clears on `M_UNKNOWN_POS`. `Own_profile` now provides the
  reactive persisted observer. Service-backed loops fold rooms, account data,
  receipts, typing, E2EE cursors and profiles into the common `Base_client`
  store, persist before callbacks, publish cancellable profile subscriptions,
  and wake an in-flight poll or retry delay for effective presence/subscription
  changes. The public standalone `Sliding_sync_state` path is retired;
  transactional private legacy-slot migration is complete and only private
  decoder compatibility remains. `Adaptive_sync` provides project-level
  automatic classic-sync fallback, not pinned Rust parity.

- **MSC4308 thread subscriptions.** Capability probing, get,
  subscribe/unsubscribe and paged changes are typed. Subscription rows and
  catch-up ranges persist, and the sliding loop saves applied changes before
  accepting the new position, so restart cannot skip a range. Updates run
  under the common service generation lock, so a stale in-flight response
  cannot commit thread state after lifecycle invalidation. Rich
  cache-backed `ThreadInfo`, server-backed `Thread_list`, Runtime lifecycle,
  pinned and event-focused views now land in the shared UI package. Threaded
  relations, pagination, receipt backfill and invalidation are wired, along
  with a complete bounded durable per-(room,root) shared-cache projection
  joining ordered event identity, pagination metadata, receipts/unread,
  subscriptions and forget cleanup. It uses the shared `EventCache`/`EventStore`
  projection, retaining each root and at most 255 replies, rather than current
  Rust's independent persisted `LinkedChunk`/lazy-loading thread-cache design.

- **Rust-compatible one-time-key bookkeeping.** `Olm.Account` maintains a
  50-key public target backed by a 5,000-key private reserve. `Encryption`
  signs only unpublished keys, persists pending batches, published IDs and the
  server count, and applies upload acknowledgements idempotently. Classic sync
  treats an omitted count as zero while sliding sync retains the previous
  count. Fallback rotation retains one previous key for in-flight handshakes
  and follows the observed-support/one-week policy across restart.

- **Server-admin whois.** `Matrix_client.Admin` and its `Matrix_eio` wrapper
  decode typed devices, sessions and nullable connection metadata from
  `GET /_matrix/client/v3/admin/whois/{userId}`, with safe user-id path
  escaping and exact authenticated JSON requests. The homeserver must grant
  server-admin authorization; no broader Synapse admin API is implied.

- **Capability-aware endpoint paths.** Thread listing, room/user reporting,
  login-token minting and MSC4133 profile fields read `/versions` first and
  select the stable Matrix path whenever supported, otherwise the exact ruma
  unstable path. Request-log tests cover stable and unstable paths, queries
  and bodies; no operation retries a mutation after an error.

- **Server metadata is cached per client.** Successful `/versions` and
  `/capabilities` responses are reused by later calls on the same client;
  failures are retried, `Server.invalidate_cache` clears both entries, and
  clients derived with a new access token start with an empty cache. This is a
  best-effort cache without HTTP cache-header expiry or centralized route
  policy.

- **Homeserver capability policy helpers.** The pure and Eio `Server` facades
  expose password, 3PID, login-token, room-version, account-moderation and
  forced-forget queries as well as profile-field policy. Absent capabilities
  use Ruma's exact defaults; malformed advertised capabilities remain errors,
  and all helpers share the per-client response cache.

- **Live locations.** `Matrix_client.Rooms` can start/stop shares and send
  reference-related beacons; `Matrix_ui.Live_locations` projects active
  shares from durable room state and the shared event cache with deterministic
  clock-driven expiry.

- **Discovery, space and member services.** Reactive public-room and
  server-event searches now retain page order, tokens, terminal/error state and
  reset semantics. `Space_graph` projects cached mutual links into a
  deterministic DAG. `Room_details` exposes disambiguated active members,
  roles and human/service counts, fetching one authoritative `/members`
  snapshot only for an incomplete cache. A raw thread-root paginator adds
  reset, deduplication, retry and observable state; the richer persisted
  `ThreadInfo` list and server-backed `Thread_list` now have Runtime lifecycle
  wiring. Homeserver discovery can
  validate an off-origin well-known delegation through a caller callback; the
  Eio facade supplies a fresh origin-restricted unauthenticated client, and
  public well-known/version probes never forward the session bearer token.

- **Trusted secret gossip.** `Encryption` can request and serve
  `m.secret.request` / `m.secret.send` between another locally known, verified
  device of the same user. Requests are plaintext to-device events; replies
  are Olm-encrypted. Requests are replaced and cancelled by secret name,
  retries keep their transaction and content, and matching responses are
  accepted only while outstanding. Registered/received values, requests,
  cancellations and queued replies now survive crypto-store reloads; cached
  Olm ciphertext is retained exactly so retrying a transaction after restart
  cannot advance the ratchet and change its body.

- **Composer drafts.** `Matrix_client.Composer_draft` persists independent
  room and thread drafts through `Store.Slot`, including reply/edit modes and
  image, video, audio and file metadata with thumbnails. Attachment bytes use
  lossless base64 and durations use integer milliseconds; setters dirty the
  store and leave flushing to the caller.

- **An integration harness against a real homeserver.** `test/integration/`
  holds `synapse.sh` (up/down/logs/url/status around a pinned
  `ghcr.io/element-hq/synapse` container), a committed `homeserver.yaml` with
  open registration, no federation, rate limits raised out of the way and
  simplified sliding sync enabled — Synapse spells that flag `msc3575_enabled`
  even though the endpoint it serves is MSC4186's — and an Alcotest executable
  that skips unless `MATRIX_TEST_HOMESERVER` is set, so `dune runtest` stays
  hermetic. `dune build @integration` runs it. The first seven scenarios cover
  registration and `whoami`, a room's create/name/topic/invite/join/members, a
  message plus an atomic original/thumbnail attachment graph through
  `Send_queue` and out of the other client's `Sync_service`, media upload and
  authenticated download with a thumbnail, one MSC4186
  `sync_once`, and a room id whose `!` and `:` reach the path unescaped — which
  Synapse accepts, settling one of `TODO.md`'s open questions. Five more drive
  `matrix-chat.ui` against the same server: reaction/edit/redaction aggregation with
  its `Observable` diff stream, room-list ordering and Unicode-caseless
  filtering, back-pagination across the gap a limited sync leaves,
  reload from SQLite, and a local echo's life from queued to synced. A further
  eleven cover the round that brought `matrix-chat.ui` in line with
  `matrix-sdk-ui`: state events as typed items, pagination to the timeline
  start, own reactions and redactions, an encrypted room through the runtime,
  a sync that cannot succeed, and the room list's sections, unread counts
  across a read receipt, sorters, search normalization and previews.
- **Room previews and knock moderation.** `Matrix_client.Room_preview` combines
  persisted state, MSC3266 summaries and a state/joined-members fallback while
  retaining create/predecessor, tombstone/successor and service-member facts.
  `Knock_requests` derives typed requests with profile/reason metadata,
  atomically persists exact seen event ids, and accepts, declines or bans with
  the expected membership calls. Pure and Eio APIs have hermetic coverage, and
  a live Synapse scenario drives the whole workflow.
- **A live scenario covers application discovery.** It publishes and
  finds a public room, searches a real indexed event, lists a real thread root
  and refreshes a deliberately incomplete member cache. The throwaway Synapse
  configuration explicitly permits directory publication because Synapse
  1.126 and newer deny it to non-admin users by default.
- **A live scenario covers queued attachments.** One atomic graph
  uploads distinct original and thumbnail bytes, substitutes their generated
  MXCs into one visible `m.image` event, removes stale alternate fields, and
  downloads both payloads byte-for-byte through Synapse.
- **The bounded parity cleanup around existing services.** OAuth `/whoami` now
  authenticates without a fake user id and browser login times out after five
  minutes by default; profile stores accept an explicit root and tighten every
  replacement to mode 0600; stable marked-unread source precedence survives a
  restart and successful receipts/read markers clear it on the server; sliding
  sync can send presence on every poll; invite, knock and leave state inform
  encryption; custom bot events retain their raw JSON; and `Runtime.stop`
  closes every timeline it handed out.
- **The second parity pass closes the remaining bounded surfaces.**
  `Fetch_httpz.std` exposes retry, rate-limit, cookie and timeout policy;
  `Client` carries a global well-known `Query`/`Do_not_query` choice; OAuth
  sessions retain their provenance so logout routes through Matrix or token
  revocation; registration has a bounded callback-driven UIAA retry; and
  sliding sync can probe its native feature through `/versions`.
- **Room conveniences and pagination.** `Matrix_client.Room` now provides
  routing/permalinks, role and invite projections and idempotent DM marking;
  the room paginator tracks both directions, rejects overlapping transitions,
  rolls back failures and deduplicates overlapping event pages. Raw thread-root
  pagination is also present; the rich cache-backed thread list now consumes
  persisted `ThreadInfo` and is lifecycle-managed by `Runtime`.
- **Edit and location conveniences.** Network edit history exhausts relation
  pages and rejects wrong-sender, redacted, state or malformed replacements;
  timelines queue replies and legacy-plus-extensible static locations through
  the same durable request handle as ordinary messages.
- **Notification services.** A command-batched notification-settings service
  manages room/default modes, poll rules, keywords and mention compatibility.
  The UI notification client fetches one missing event without inserting it
  into room topology, decrypts and evaluates it against current push context,
  and hands ownership to the shared cache if sync later supplies it.
- **OAuth discovery cache and CLI HTTP policy.** Successful OAuth metadata is
  cached per client for 24 hours, with explicit invalidation and stale fallback
  after a failed synchronous refresh. Every `omatrix` network command now
  exposes retry, rate, concurrency and connect/idle timeout controls, including
  off-origin OAuth transport.
- **Room-list and runtime parity details are now explicit.** `Runtime` reports
  `Offline` while a failed sync is being retried; `Room_list.Filter` includes
  typed `Space` and tombstone successor deduplication; timelines accept a
  caller event filter while retaining relation aggregation and UTD visibility;
  and the HTML sanitizer emits actual void elements (`br`, `hr`, `img`).
- **Another bounded compatibility round.** `Client.Http` has authenticated
  absolute-path JSON helpers and dehydration no longer relies on a `/..`
  route; OAuth chooses complete stable or MSC2967 scope dialects and accepts
  deployed MSC4191 account-action aliases; `Matrix_bot.Main` can select a
  long-running bot or a one-shot action inside one login envelope, and
  `matrix-bot --notify` now exposes send failure in its process status.
- **Malformed room responses fail closed.** A 2xx response missing its required
  `joined_rooms`, `chunk`, `joined` or `aliases` collection is a JSON error,
  not fabricated empty room state.
- **Encrypted edits keep their provenance.** Room-list preview replacement now
  rejects a plaintext edit of an encrypted event and requires `m.new_content`
  on a decrypted encrypted edit, matching the pinned Rust validator.
- **Fallback-key rotation retains the prior key and follows its age.**
  `Olm.Account` accepts
  in-flight pre-key messages against the current or immediately previous
  fallback key, publishes only the current key, and persists both through the
  account pickle and profile crypto store. `Encryption` waits until server
  support is observed, persists the current key's creation time and unpublished
  state, and rotates only when strictly older than Rust's one-week threshold
  (also recovering from clock rollback). Applications can explicitly forget
  the prior key. A server that repeatedly drops one-time keys follows the
  pinned/current Rust behavior: indefinite replacement/claim backoff; a finite
  dropped-OTK cap is optional design/scale hardening.
- **Bot sends and CLI verification have explicit readiness bounds.** A
  `Matrix_bot.Room` reports and waits for complete encryption/member state;
  `Room.sync_members` fills an incomplete recipient set through authoritative
  `/members` state, the notifier uses both, and `omatrix verify --timeout` now
  applies an absolute monotonic deadline instead of counting sync rounds.
- **Queued text edits.** `Send_queue.send_edit` produces the exact `m.replace`
  fallback and `m.new_content`, using the ordinary persisted/retried event
  node. `Room_timeline.send_edit` sanitises Matrix HTML and gives the target an
  optimistic edited local echo.
- **`matrix-chat.ui` follows `matrix-sdk-ui` where a client would notice.**
  `Presentation` classifies `m.room.member` exactly as ruma's
  `membership_change` does and types the other state events rust-sdk models,
  so a timeline item reads "@alice:localhost joined" rather than
  `m.room.member`; `Timeline` hides room-configuration state (a deliberate
  deviation, recorded in `TODO.md`), adds a `Timeline_start` virtual item at
  the room's beginning, and can be closed. `Runtime.sync_state` gains `Failed`
  and `start` an `?on_error`. `Room_list` gains `Filter.t`, the composable
  counterpart of rust-sdk's `filters`, sorts by `lexicographic [latest_event;
  recency; name]` without mixing the two timestamp scales, and previews only
  what `Presentation.is_preview_worthy` — rust-sdk's `filter_timeline_event` —
  admits, preferring decrypted plaintext. `Text.search_key` strips combining
  marks so "cafe" finds "Café".
- **The UI event cache holds a room as chunks and gaps**, as rust-sdk's
  linked chunk does: a disjoint limited sync inserts a `Gap` the timeline
  shows in place and `Timeline.paginate_gap` fills; `trim` leaves a gap with
  the token to refetch what it cut; SQLite is written incrementally
  (`Event_store.apply`, schema 2) on a system thread rather than the domain;
  presentations are memoised per event; a `Read_marker` virtual item follows
  `m.fully_read`; a ciphertext-only cache is re-decrypted when a timeline opens
  under a runtime with encryption; `Runtime.stop` cancels the services and
  publishes `Stopped`.
- **Room-list search and unread as in rust-sdk**: `Text.fuzzy_score` and
  `Room_list.Filter.Fuzzy` with `Room_list.score`; `m.marked_unread` folded
  from room account data into `Sync_service.room_info` and the unread filters;
  previews resolve the newest edit onto its target; the sanitiser keeps `img`
  with an `mxc://` source and takes a `?resolve_mxc` hook.
- **`HACKING.md`** states how interfaces, implementation comments and dead
  code are judged: an `.mli` is a manual page, a comment must say why, and an
  unused endpoint binding is not dead.
- **`Matrix_proto.Base64`**, base64 as Matrix writes it: unpadded on output,
  padded or unpadded on input. It replaces the three lenient decoders
  `Verification`, `Secret_storage` and `Backup` each kept.
- **`Matrix_proto.Json`**, the JSON member helpers the client modules each
  used to carry a private copy of.
- **`matrix-chat.bot`, a library for writing bots.** A bot is a `Bot.spec`:
  handlers registered against a typed `Event.t` — messages, commands, edits,
  reactions, redactions, stickers, polls, memberships, profile changes, room
  state, invites, joins, leaves and the sync state. Every registration takes
  the spec last, so a partial application is a plugin, any `spec -> spec` is
  one, and a bot is a pipeline of them. `Bot.run` owns the concurrency: events are collected off
  `Event_cache.events` rather than the aggregated timeline, one bounded
  `Eio.Stream` and one dispatcher fiber per room keeps a room's handlers in
  order while rooms proceed concurrently, a handler that raises is reported
  and its room carries on, and a send answers through a `Sent.t` over an
  `Eio.Promise`. `Command.parse` and `Bot.command` give commands with
  generated `help`; `Bot.only`, `in_rooms` and `from_users` scope a plugin;
  `State` keeps per-plugin JSON under the profile in one atomically written
  file, which is where each room's cursor lives, so a bot built with
  `~backlog:`Handle` replays what arrived while it was down and the default
  `Skip` does not. `Context.connect` restores the profile's session, crypto
  store and event store; `Main.run` is the command line around it, with
  `SIGINT` and `SIGTERM` handled through a condition a fiber waits on to call
  `Bot.stop`.
- **Five example plugins and one executable** in `bin/matrix-bot`: `Echo`,
  `Commands` (replies, a generated `!help`, a reaction, a state event and an
  `~admin:true` command), `Welcome` (typed membership, room state and profile
  changes), `Moderator` (redact, warn, kick, with the strike counts in
  `Bot.state` so a restart does not forgive anybody) and `Logger`
  (`Room.backfill` from the `Joined` handler, then every `Event.t` printed);
  `Notify` is not a plugin but the cron shape — one send, awaited, then
  `Bot.stop`. `matrix-bot` composes them, one `--plugin` flag each, and runs
  `--notify ROOM --body TEXT` as a mode. All driven against Synapse by
  `test/integration/scenario_bots.ml`, three of them in encrypted rooms.
- **End-to-end encryption tested against a real homeserver.**
  `test/integration/scenario_e2ee.ml` adds five scenarios in which two freshly
  registered users each run an `Encryption` machine inside a `Sync_service`
  fibre with an encrypting `Send_queue`: an encrypted room end to end (the
  plaintext out of the reader's `room_change.decrypted`, `m.room.encrypted` on
  the wire, and a device list on each side naming the other's device), a second
  message in the same Megolm session and a reply in the other direction, a
  complete SAS flow driven by `Verification_service` over `/sendToDevice` after
  which both machines mark the peer `Verified`, a key backup a *second real
  login* of the same user restores and decrypts an earlier event with, and the
  backup key stored in and read back out of SSSS. Synapse rejected nothing any
  of it sent.
- **Working end-to-end encryption.** `Encryption`, the crypto machine, runs
  inside the sync loop: one-time key counts drive `/keys/upload`,
  `device_lists` drives `/keys/query`, to-device `m.room.encrypted` is Olm
  decrypted and dispatched, and encrypted timeline events are Megolm
  decrypted. On send, an outbound Megolm session is created, its key claimed
  and Olm-shared to every device that lacks it, and the payload goes out as
  `m.room.encrypted`. Persisted by `Crypto_store`.
- **Olm and Megolm rewritten to the libolm wire formats** — pre-key and normal
  message v3, X3DH, the double ratchet, Megolm message v3, signed v2 session
  keys and unsigned v1 exports. Cross-checked against vodozemac 0.9 through
  `test/vodozemac-oracle`, in both directions.
- **Key gossiping**: `m.room_key_request` and `m.forwarded_room_key`, answered
  only for our own user's other devices when they are locally verified.
- **Key backup**: `Room_keys` (`/room_keys/version`, `/room_keys/keys`) and a
  spec-shaped `Backup` with base58 recovery keys, upload and restore.
- **Secret storage (SSSS)**: `m.secret_storage.v1.aes-hmac-sha2`, PBKDF2
  passphrase keys, `Base58`.
- **Cross-signing**: generation in `Verification`, publication through
  `Keys.upload_signing_keys` and `Keys.upload_signatures`, and real Ed25519
  verification over canonical JSON.
- **Complete SAS and QR verification**: all 64 emoji, decimals, commitment,
  `hkdf-hmac-sha256.v2` MACs, the full request→ready→start→accept→key→mac→done
  state machine with every cancellation code, and the binary `MATRIX` v2 QR
  payload with `m.reciprocate.v1`. `Matrix_eio.Verification_service` drives a
  flow from the sync loop.
- **OAuth 2.0 next-generation authentication** (`Oauth`): `/auth_metadata`
  discovery with MSC2965 and well-known fallbacks, RFC 7591 dynamic client
  registration, PKCE S256, the authorisation-code grant over a loopback
  listener, refresh and RFC 7009 revocation. `omatrix login --oauth`.
- **A real base-client layer**: `Sync_service` maintains room state across
  syncs (name, topic, avatar, members, DM flag, heroes, unread and highlight
  counts), `Push_evaluator` evaluates push rules locally, `Read_receipts`
  computes receipts, and `Send_queue` genuinely sends with retry over one
  persisted transaction id. `Store` and `Timeline` are no longer skeletons.
- **New Client-Server API modules**: `Server` (versions, capabilities,
  well-known), `Search` (room events and user directory), `Notifications`,
  `Threads`, `Report`, `Tags`, `Openid`, `Thirdparty`, `Delayed_events`
  (MSC4140), `To_device`, `Dehydrated_device` (MSC3814). Plus room upgrade,
  `/aliases`, `/joined_members`, `timestamp_to_event`, the MSC3882 login
  token, `/register/available`, registration-token validity, and MSC4133
  extended profile fields.
- **Sliding sync speaks MSC4186** (simplified sliding sync) — request and
  response codecs, the `pos` loop, room lists, subscriptions and the e2ee,
  to-device, account-data, receipts and typing extensions.
- **`omatrix` gained encryption**: `keys init`, `verify`, `backup
  enable`/`restore`/`status`, encrypted `msg` and a decrypting `sync`.
- `Matrix_client.Random`, the randomness capability wrapping
  `env#secure_random`; `Matrix_eio.Http` (`https` and `client`), the httpz +
  `tls-eio` + `ca-certs` backend producing a `Fetch.t`; `Client.get_bytes` and
  `Client.post_bytes` for raw binary requests.
- `Media.upload`, `download`, `thumbnail` and `get_config` are implemented
  (they were stubs) against the authenticated endpoints under
  `/_matrix/client/v1/media` (Matrix v1.11 / MSC3916).

### Follow-up fixes (2026-09-03)

- **RFC 8628 device authorization.** OAuth now includes device-code request
  and bounded monotonic polling, including pending, slow-down, denial and
  expiry handling. `omatrix login --device-code` exposes the headless flow
  alongside browser login, while the library and Eio wrapper remain available
  to applications.
- **Safe, cross-signed backup enable.** `omatrix backup enable` checks the
  server's current version before generating anything, reuses a matching local
  backup by uploading pending keys, and refuses divergent or missing local
  keys. New and repaired backup auth data is signed first by the matching
  cross-signing master key and then by the current device, while recovery still
  accepts a caller-supplied identity without a master secret for compatibility.
- **Withholding and verification selection.** Refused room-key shares now send
  idempotent `m.no_olm`/`m.blacklisted` notices with durable sent-notice
  bookkeeping. Verification requests send one directed `m.accepted`
  cancel to each other concrete device after the first `ready`, and relay a
  received cancel once; wildcard requests deliberately do not fan out.
- **Capability-aware media and graceful sync.** Media downloads, thumbnails,
  config and URL previews select authenticated or legacy routes from
  `/versions`, retaining unknown config/preview fields. `omatrix sync` uses
  `Sync_service.run`, preserves exact positive `--count`, and cancels/saves
  cleanly on SIGINT/SIGTERM.
- **Preallocated media upload.** `Media.create_content_uri` and
  `upload_preallocated` implement MSC2246's reserve-then-fill flow, including
  the optional local expiry deadline, typed overwrite/legacy-expiry failures,
  exact path escaping, an Eio wrapper and a live Synapse round trip.
- **Strict MXC media identifiers.** `Media.Mxc.of_string` now rejects media ids
  outside Matrix's ASCII letter/digit/`-`/`_` grammar, matching ruma and keeping
  encoded path separators out of authenticated media routes.
- **Cached edit revision history.** `Room_timeline.edit_revisions` returns the
  original event and valid `m.replace` revisions chronologically, deduplicates
  event ids, skips redacted/invalid edits and enforces encrypted-event
  provenance. It intentionally does not fetch revisions outside the cache.

### Current implementation update (2026-09-05)

This update begins at ocaml-matrix code SHA
`a76431516fe6982259eb3eb29b3a1db696f05a54`; the additions below also describe
subsequent unreleased work. The historical parity audit
baseline remains recorded above: pinned matrix-rust-sdk
`523b5af53a8fd9fae9e2bc981bfb01ac86fd2890` and the 2026-09-03/04 ocaml-matrix
snapshot `2c7bb435348043ce2ddb5a957497cac5ccd4633a`.

- **Adversarial audit correction.** Persisted send-queue restore decodes and
  validates each generic JSON element independently, so one wrong-typed member
  cannot discard unrelated requests. Malformed records, real upload parents
  and dependent descendants are quarantined rather than repaired into sends.
  Duplicate/out-of-order graph and transaction identifiers, invalid optional
  transactions, event/extension shapes and cache ownership are checked; random
  failure during cancellation or retry rolls state back, and live transaction
  allocation remains unique even with a repeating source. Forwarded room-key
  import verifies the session ID derived from the key and uses its canonical
  sender key. Exact Megolm/export key lengths, bounded forwarding chains,
  explicit portable-export KDF policy, Matrix OAuth response-mode metadata and
  encrypted-stream output failures are also pinned by hostile tests.
- **Lazy UI room storage.** `Event_store.Lazy_S`/`v_lazy` adds validated
  layout/tail and single-chunk reads without breaking eager backends. A cold
  room cache decodes only its newest chunk, retains older stable identities and
  gaps as metadata, and hydrates one local predecessor before remote
  back-pagination. Incremental writes and flush retries preserve unloaded rows;
  detached plaintext and restored queued echoes move into physical resident
  chunks without duplicates. Memory and SQLite implement the same contract,
  with SQLite disk work kept on the Eio systhread boundary. A 21-case
  instrumented acceptance suite covers malformed data, topology preservation,
  restart, plaintext policy and failure retry.
- **Structured Eio operation errors.** A private client helper and
  `Matrix_eio.Error.with_context` attach stable, secret-free operation labels
  to direct path/flow/random/persistence failures and every explicit raising
  endpoint wrapper. Raw backtraces are captured before cleanup and re-raised
  intact; cancellation and non-I/O exceptions pass through, while protected
  cleanup prevents an interrupted unlock or rendezvous close from masking the
  original failure. OAuth, QR, sync, encryption/recovery, UI, bot and SQLite
  boundaries follow the same policy. Transport results strip userinfo, query
  and fragment from diagnostics instead of retaining Fetch's exact wire URL.
- **QR application flow.** MSC4108 OAuth approval, typed authentication
  messages, secret handover and both Eio roles are implemented and mock-tested
  through device signing/trust and backup setup. `omatrix qr login` creates a
  textual Base64 payload through the exact MSC4108 creation endpoint, requires
  an unused profile and check-code confirmation, reports progress and supports
  cancellation/timeout. It persists the OAuth session immediately after
  authentication, then creates a fresh encrypted remote SSSS store with
  cross-signing seeds and an optional backup key, protected by a passphrase
  file or one-time recovery key. `qr grant` reads a credential file/environment
  value and uses same-origin rendezvous. Raw QR bytes cross the public/FFI
  boundary; raster rendering/scanning and the mandatory confirmation callback
  remain caller/UI-owned. The isolated MSC4108 rendezvous harness passes
  creation, bidirectional ECIES-encrypted messaging and cleanup/cancellation
  on port 8009, but has no MAS/OIDC; the full two-role OAuth QR flow remains
  an external deployment check. MSC4388 is still the separate typed
  unsupported boundary.
- **Thread and profile services.** Persisted `ThreadInfo`, server-backed
  `Thread_list`, Runtime lifecycle, shared detached roots/latest replies,
  pinned events and room/thread-focused `Event_focused` views are available.
  Receipt-target backfill, prioritized pagination and lifecycle invalidation
  are also wired. A bounded durable per-(room,root) shared-cache projection now
  joins ordered event IDs, pagination metadata, receipts/unread, scoped
  subscriptions and forget cleanup across restart. It uses the shared
  `EventCache`/`EventStore` projection, retaining each root and at most 255
  replies; it is not current Rust's independent persisted
  `LinkedChunk`/lazy-loading thread-cache parity.
  Service-backed sliding sync now uses the common `Base_client` fold and
  cancellable profile stream; public standalone state is retired,
  transactional private legacy-slot migration is complete and only private
  decoder compatibility remains. Automatic classic fallback is provided by
  `Adaptive_sync` as a project enhancement, not pinned Rust parity.
- **Nonblocking verification prompts.** `Verification_service` delivers SAS
  confirmation off the sync fiber, retains a one-shot pending prompt and lets
  callers answer it explicitly, so a blocking UI callback cannot stall sync.
- **Backup version handling.** Rotation and deletion of the active backup use
  Rust-compatible version-addressed behavior: stale uploads are not
  acknowledged, and a missing/changed active version disables the local
  uploader durably. HTTP ETag coordination remains optional project hardening,
  not a pinned-Rust parity claim.
- **Recovery, dehydration and media.** Recovery Manager/UIAA password identity
  reset, conditional rebackup and device-key preupload are complete. The
  interoperable MSC3814 create/rehydrate/drain/rotation manager and its live
  Synapse lifecycle are complete too, including continuity of real room keys
  through create/get/delete/rehydrate. Replaceable media fetching and backup
  restore via the whole unpaginated GET plus targeted room/session APIs are
  implemented.
- **Second-homeserver smoke coverage.** The named Dendrite v0.15.2 profile
  passes the five portable rooms-core cases (5/5). Its peeking case is
  explicitly rejected with `M_GUEST_ACCESS_FORBIDDEN`; threaded receipts time
  out, MSC2246 preallocation is rejected despite advertised MSC3916,
  simplified sliding sync is absent, and federation is disabled. The full
  Synapse reference harness now passes 60 scenarios in 109.397s; the hermetic
  suite has 62 executables, 1,418 cases/checks and three source guards. The
  same combined run passed Dendrite 5/5 in 1.535s and left no owned container,
  data directory or harness process. An earlier historical snapshot recorded
  58 scenarios in 114.562s;
  that measurement is retained only for release-note provenance.
- **Whole-operation request deadlines.** Core clients can opt into one
  monotonic deadline spanning transport retry/backoff, response or stream
  consumption, and time awaiting automatic refresh/replay; the Eio facade
  exposes the duration at construction. Parent cancellation remains distinct.
  Pathological UI list reconciliations now collapse to one `Reset` after a
  bounded granular-diff budget. The Synapse, Dendrite and MSC4108 scripts also
  accept independently named containers/data directories and refuse unsafe or
  unmarked purge targets.

### Changed (breaking)

- `Matrix_eio.Oauth.Loopback.create` now takes `~env` so Proffer receives the
  caller's Eio clock as well as its network capability. The package is still
  unreleased; direct users of this low-level helper must pass their standard
  environment.

- **The libraries were restructured.** Nothing here has been released, so
  there is no migration path; these are the names the tree now uses.

  - `matrix-chat.cli` is a new library holding the cmdliner terms that were
    `Matrix_client.Cmd`, so `matrix-chat.client` no longer depends on `cmdliner`,
    `fmt.tty`, `fmt.cli` or `logs.cli`.
  - `matrix-chat.ui.sqlite` is a new library holding the SQLite event store that
    was `Matrix_ui.Event_store.sqlite`, as `Matrix_ui_sqlite.create`, so
    `matrix-chat.ui` no longer depends on `sqlite3`.
  - `matrix-chat.client` performs one logical request per call and owns no driver
    loop. An optional whole-operation timeout uses an injected monotonic clock;
    the long-lived loops remain in `matrix-chat.eio`, and the pure fold they drive is
    `Matrix_client.Base_client`, which was
    `Matrix_client.Sync_service`; the loop around it keeps the name as
    `Matrix_eio.Sync_service`.
  - Every module in `matrix-chat.eio` has an `.mli`.
  - `matrix-chat.client` renames: `Session.Store` to `Profile_store`,
    `Session.Pickle` to `Session_pickle`, `Read_receipts` to `Read_state`.
    `Threads` folded into `Relations`, `Room_preview` into `Directory` and
    `Rooms`, `Json_util` into `Json_codec`. `Account_data` split from
    `Account`, `Secrets` from `Secret_storage`, `Cross_signing` from
    `Verification` (which now holds `Sas`, `Qr` and `Flow`), and
    `Oauth.Token` from `Authorization`. `Crypto_key` is new and holds the
    Ed25519 and Curve25519 key types every other module now takes.
    `Encryption` is the pure machine, `Encryption_driver` performs its
    requests against a client and returns `result`, and
    `Matrix_eio.Encryption` is that driver raising. `Olm` is split into the
    `Account`, `Session`, `Megolm` and `Machine` compilation units it
    re-exports. `Backup` is no longer a machine: `Backup.Decryption_key` and
    `Backup.Recovery_key` are the key types. `Sliding_sync_state` holds what
    was `Sliding_sync.State`.
  - `matrix-chat.proto` gains `Push` (the push-rule model, which was
    `Matrix_client.Push`), `Sliding_sync` (the MSC4186 wire shapes),
    `Base64` (which was the `matrix.lb64` library), `Common` and
    `Signed_json`. `Matrix_event` is split into per-family compilation units
    re-exported under `Matrix_proto.Event`, so a caller writing
    `Matrix_proto.Event.Foo` is unaffected. `Id` presents every identifier
    through one signature and `Event.Timestamp` is abstract.
  - `matrix-chat.ui` renames: `Timeline` to `Room_timeline`, and `Text` splits
    into `Presentation.Html` and `Matching`.
  - `matrix-chat.bot` renames: `State` to `Plugin_store`, `Command` to `Args`.
    `Logging` gains an `.mli`.
  - **`--password` and `-p` are gone from every command line.** A password is
    read from the file `--password-file` names, or from `$MATRIX_PASSWORD`.
    A command line is readable by every process on the machine.

- **`Client.create` takes the HTTP capability and the Eio environment.**
  ```
  val create :
    config:config -> fetch:_ Fetch.t ->
    < secure_random : _ Eio.Flow.source ; .. > -> t
  ```
  The library never constructs an HTTP client, picks a TLS backend or seeds a
  random generator. `Matrix_eio.Client.create ~sw ~env ~homeserver ?fetch ()`
  supplies a default backend, so Eio-layer callers are unaffected.
- **Randomness is an explicit capability.** Every key, nonce, IV, flow id and
  transaction id generator takes `~random:Random.t` from `Client.random`.
  Affected: `Keys.generate_ed25519`, `Keys.generate_curve25519`,
  `Keys.generate_one_time_keys`, `Backup.generate_backup_key` and its
  encryption entry points, `Verification.generate_ed25519_key`,
  `Verification.generate_cross_signing_keys`, `Verification.generate_flow_id`,
  the SAS and verification-request constructors, and `Olm` and `Send_queue`.
- **`Matrix_proto.Id.Transaction_id.generate` is removed** — it used the
  unseeded, non-cryptographic stdlib `Random`. Use
  `Transaction_id.of_bytes : string -> t` with bytes from
  `Matrix_client.Random.txn_id`; `matrix_proto` must not depend on Eio.
- **Session files changed from TOML to JSON with no migration.** The store
  writes `session.json`, `device.json`, `one_time_keys.json`,
  `olm_sessions.json`, `megolm_inbound.json` and `megolm_outbound.json`.
  Existing `.toml` profiles under `$XDG_DATA_HOME/matrix` are ignored — run
  `omatrix login` again.
- **The Olm pickle format changed** with the wire-format rewrite. Session
  files written by an earlier build fail to load; those sessions must be
  re-established.
- `Fetch.t` and `Eio.Flow.source` now appear in the public signature of
  `matrix-chat.client`, and `val jsont : t Jsont.t` replaces `val tomlt : t
  Tomlt.t` on every `Session` submodule.
- `Media.download` and `Media.thumbnail` return
  `(string * string option, Error.t) result`; the content type is `None` when
  the server sends no `Content-Type`, where before it was a bare `string`.
- **Minimum OCaml is now 5.5**, because `fetch` and `httpz` require it.
- Dependency swaps: `nox-json` → `jsont`, `nox-crypto` → `mirage-crypto`,
  `nox-crypto-ec` → `mirage-crypto-ec`, `nox-kdf.hkdf` → `kdf`,
  `nox-xdg.eio` → `xdge`, `requests` → `fetch` (+ `fetch-httpz`, `tls-eio`,
  `ca-certs`, `x509`, `domain-name` in `matrix-chat.eio`), `tomlt` → `jsont`.
  `mirage-crypto-rng` is **removed from `matrix-chat.client`** and remains only in
  `matrix-chat.eio`, where `Matrix_eio.Http.client` seeds the process-global
  generator that `tls-eio` requires. `eio_main` moved from `with-test` to a
  regular dependency, because the installed `omatrix` links it.

### Fixed
- **Three endpoints that never worked.** `Relations.get_relations`,
  `Spaces.get_hierarchy` and `Room_preview.get_summary` are served under
  `/_matrix/client/v1` but were requested under `v3`, and the summary codec
  expected MSC3266's unstable member names; all three answer against Synapse
  and the live suite covers them.

Both of these were found by the first requests this SDK ever made to a real
homeserver.

- **`Auth.register` could not register against any real server.** It sent no
  UIAA `auth` object and had no way to accept one, but even a homeserver with
  `enable_registration_without_verification` answers the first
  `POST /register` with a 401 and the flow `[["m.login.dummy"]]`. It now takes
  `?auth:Uiaa.auth_data`, which the caller fills in from the challenge; so does
  `Matrix_eio.Auth.register`.
- **`Rooms.get_joined_members` rejected Synapse's reply.** Synapse sends
  `"avatar_url": null` rather than omitting the member, and `opt_mem` accepts
  only an absent member or a string. Absent and null now both decode to `None`.
- **Key generation no longer raises `Unseeded_generator`.** `Mirage_crypto_rng`
  was never seeded anywhere in the tree, so every key and nonce path failed at
  runtime. Randomness now comes from Eio's `secure_random`.
- **1:1 Olm could not complete a handshake.** `Session.create_outbound`
  generated an X3DH ephemeral key and threw the public half away, and the wire
  format carried neither the ephemeral key nor the one-time key id, so nothing
  a peer received was decryptable. There is now a real pre-key message.
- **Megolm key export lost its message index.** `export_session_key` exported
  the ratchet at its current index while `from_room_key` assumed 0, so a key
  shared after the first `encrypt` decrypted nothing. Exports now carry their
  index and importers start there.
- **`Session.Pickle` could decode but not encode**, so the crypto state could
  not be saved. Every codec has an `~enc`, the `pickle_*` functions return a
  result instead of raising, and `Crypto_store`'s duplicate codecs are gone.
- `Megolm.Inbound.from_room_key` and `Machine.receive_room_key` return
  `(_, string) result` instead of fabricating random ratchet state when a
  session key fails to unpickle.
- **No `failwith` remains in `lib/`** — every parse and crypto failure is a
  `result`. MACs are compared in constant time and Diffie-Hellman rejects
  non-contributory keys.
- `Eio.Cancel.Cancelled` is re-raised rather than swallowed by the HTTP error
  mapping.

### Security

- **JSON bodies were built by string splicing.** `Uiaa` interpolated
  caller-supplied `password`, `token`, `session`, `sid`, `client_secret`,
  `user_id`, `address` and `response` straight into JSON string literals with
  `Printf.sprintf`. A password containing `"` or `\` produced a body no
  homeserver could parse; a value shaped like `x","admin":true,"y":"` closed
  the string early and injected members into the `auth` object the caller
  never asked for — reachable from `Keys.upload_signing_keys ~auth` and
  `Auth.get_login_token ~auth`. `Uiaa.add_auth_to_body` had the mirror
  problem: it cut the body's outer braces off with `String.sub`, checked its
  length against the untrimmed body, and emitted a **duplicate `auth` member**
  when the body already carried one. Every JSON body in `lib/matrix_client/`
  is now built as a `Jsont.json` value; `test/test_json_safety.ml` (29 cases)
  feeds each builder quotes, backslashes, control characters, a member
  injection payload and non-ASCII, and requires byte-exact round trips with
  exactly the members expected.
- **34 request codecs could decode but not encode**, leaving 30 public
  functions dead on arrival. They declared required members with
  `Jsont.Object.mem` and no `~enc`, so `Client.encode_body` answered
  `Error (Json_error "No encoder for member …")` and the call failed before
  any HTTP request was built — invisible to any test that only decodes.
  Affected: `Account` (3PID, password, deactivate, ignore/unignore),
  `Devices`, `Directory` (create_alias, set_visibility), `Keys` (upload,
  query, claim), `Presence`, `Profile` (set_displayname, set_avatar_url),
  `Push`, `Receipts`, `Relations` (reaction, edit, reply, thread), `Rooms`
  (invite, kick, ban, unban), `State` (set_name, set_topic, set_avatar) and
  `Typing`. `test/test_encoders.ml` now drives every body-carrying entry point
  reachable from `Matrix_client` (86 rows) and fails if any cannot encode.
- Requests are narrowed with `Fetch.restrict ~under:[origin]`, so no redirect
  or caller-supplied path can reach a server other than the homeserver, and
  bearer tokens are scoped and redacted via `Fetch.with_credentials`.
- **The key-backup MAC deliberately covers the empty string, not the
  ciphertext** (`Backup.session_mac`), matching a libolm bug that the spec,
  vodozemac and matrix-rust-sdk all reproduce. MACing the ciphertext would
  make our backups unreadable by every other client.

### Removed
- **Dead weight across the tree**, found by a name-based scan and judged by
  `HACKING.md`: `Olm.Machine`'s own Megolm session store and key-upload
  bookkeeping, `Keys.create_device_keys` (wrong algorithm names, non-canonical
  signing) with its Eio wrapper, forty internal-only values `Verification`
  exported, `Backup`'s unproduced signature-verification and request types,
  `Crypto_store`'s partial load/save forms, `Olm.Megolm.Ratchet`, the
  `Observable.List` mutators nothing called, `Room_list`'s flag-record preset
  (`Filter.t` expresses it), `Runtime.apply_changes`, `Rooms.create_room`,
  `Room_preview.resolve_alias`, `Room_preview`'s copy of the published room
  list (`Rooms.public_room` gains the `join_rule` and `room_type` it carried,
  and `Rooms.get_public_rooms` and `Directory.search` are the bindings), and the `matrix-chat.eio` presence, receipt and
  typing wrappers that only pinned an argument. `example/simple_bot.ml` and
  `example/send_dm.ml`, superseded by `bin/matrix-bot/echo.ml` and
  `omatrix msg`.
- **Interface prose rewritten as manual pages** in every `.mli` and every
  `matrix-chat.eio` module: present tense, behaviour and failure cases only, no
  history, no `@param` for self-evident arguments; implementation comments
  cut to the ones that say why. `dune build @doc` warnings fall from 224 to
  109, all cross-package roots.

- **HTTP/2 support.** `fetch-httpz` is HTTP/1.1 without connection reuse; the
  previous `requests` backend spoke HTTP/2.
- **MSC3575 sliding sync**, replaced by MSC4186 simplified sliding sync.
- **~1900 lines of unreferenced code** from `Store` (the `STATE_STORE`,
  `CRYPTO_STORE` and `EVENT_CACHE_STORE` module types and their in-memory
  implementations), `Timeline` (`LinkedChunk`, the per-room cache, the
  duplicated room-info fields) and `Send_queue` (the media-upload kind, the
  dependency graph, the caller-supplied send function). None of it had a
  caller; what replaced it is smaller and wired up.
- The stale `spec/matrix-spec` submodule declaration and the
  `(vendored_dirs vendor)` stanza, neither of which referred to anything in
  the tree.

## v0.1.0 (unreleased)

Initial release of ocaml-matrix, a pure OCaml Matrix SDK.

### Features

- `matrix_proto`: Matrix protocol types with bidirectional JSON codecs using jsont
- `matrix_client`: Matrix client SDK with session persistence and HTTP requests
- `matrix_eio`: Eio-idiomatic wrapper with switches, fibres, and Eio.Io errors
- `matrix-chat.ui`: Eio-based reactive room lists and timelines, safe Matrix HTML
  presentation, local echoes, back-pagination, and a memory or SQLite event
  cache
- `omatrix`: CLI tool with cmdliner subcommands for login, sync, and messaging
- Support for encrypted room creation and direct message rooms
