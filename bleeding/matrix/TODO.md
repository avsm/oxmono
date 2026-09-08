# TODO

Known gaps, deferred work and sharp edges, by area. Each entry says where the
gap is and what a change needs to do. User-facing status is in `STATUS.md`;
ordering is in `ROADMAP.md`; `PORT_PLAN.md` and the original phases in
`PARITY_PLAN.md` are historical.

The historical sections mostly describe work that was never done, or was done
narrowly on purpose. The module/library re-audit immediately below also records
newly discovered correctness defects and duplicated infrastructure. Treat its
unchecked boxes as the current implementation queue; the older area notes
remain the detailed behavioural record.

## First-release gates (reviewed 2026-09-08)

[RELEASE_REVIEW.md](RELEASE_REVIEW.md) reviews the current tree against Rust
`9aea12c33d3970aec19b989c4d6a485e65d6bbc6`. These additions are independent
of the historical parity completion and assume an experimental 0.1 release.

- [x] **FR1 — Validate backup restore before changing crypto state.** Restore
  checks `Backup.current_version_state` before loading the encryption machine
  or requesting keys. CLI tests reject different keys, unsupported algorithms
  and malformed metadata without profile changes, and cover empty/populated
  backups plus restored event decryption.
- [x] **FR2 — Take backup recovery keys through a file.**
  `--recovery-key-file FILE` replaces the positional secret. CLI tests cover
  valid, empty, missing and malformed input and rejection without key echo.
- [ ] **FR3 — Publish and verify the distributed dependency chain.**
  A clean source copy builds against installed `dev` dependencies. The published
  `avsm/httpz` HEAD `fc8c45336b91ed31b3a3556dd27860621f06b918` was fetched and
  contains the old OxCaml package; it lacks `fetch.opam`, `fetch-httpz.opam`,
  `proffer.opam` and `proffer-httpz.opam`. Publish the restructured packages,
  then run `test/release-install.sh` with their full immutable Git revision and
  the extracted release archive. The script verifies manifests before creating
  an isolated OCaml 5.5 switch. Public opam distribution additionally needs
  published dependency releases and appropriate version bounds.
- [x] **FR4 — Require real-server and oracle evidence for the release.**
  `test/release-check.sh` builds the locked oracle and requires its execution,
  then runs local tests and fresh isolated homeservers with cleanup and logs.
  The 2026-09-08 run passed seven Olm/Megolm oracle cases, the full 60-scenario
  Synapse reference suite and five Dendrite smoke scenarios. Required missing
  fixtures fail instead of skipping. MSC4108 OAuth/MAS is labelled experimental
  in the CLI; U0/P2.3 remains required before claiming a validated two-role flow.

- [x] **Production persistence — recover interrupted crypto snapshots.**
  Complete private redo journals precede component changes; native parent
  directories are synced. Restart tests cover each write/deletion boundary,
  pre-marker recovery, invalid journals, identity/ratchet/trust continuity and
  rejection of stale handles. Interrupted legacy saves without journals still
  fail closed and require explicit recovery.
- [x] **Production auth — coordinate profile token refresh across processes.**
  `~store` on Eio client/OAuth refresh helpers serializes the exchange, reloads
  other processes' rotations and persists before notification. `omatrix` uses
  this for both login methods. Tests cover two processes, concurrent sync,
  logout, process death, cancellable waiting/exchange and OAuth adapter reuse.
  Ambiguous failures conservatively require re-login; the coordinator never
  retries a potentially consumed token automatically.
- [x] **Auth follow-up — recover fresh logins and retry OAuth preparation.**
  Fresh login waits for active refresh and persists credentials before clearing
  any old marker, including malformed data. Failed writes and ordinary metadata
  updates retain the marker. OAuth discovery/validation precedes marker creation;
  failures or cancellation there remain retryable. Once exchange begins, the
  conservative recovery policy still applies. Regression coverage includes the
  real CLI, concurrent login/refresh, and reactive/proactive OAuth adapters.
- [ ] **Deferred — encrypted local storage.** First release keeps profile
  tokens, private keys, session state and journals unencrypted behind filesystem
  access controls (new directories 0700, files 0600). No local unlock prompt or
  keyring integration is implemented. A future design must cover key providers
  for desktop keyrings, passphrases and unattended services; authenticated
  encryption of secret-bearing files, journals and temporary files; crash-safe
  migration; and key-loss recovery. This is explicitly outside the current
  release scope. See the storage boundary in `README.md`.

Reliable zeroisation retains the documented GC boundary. Previously deferred
platform/experimental features remain
outside this release scope. The review also records nonblocking newer Rust UI
deltas in profile local updates, room-list preview back-pagination and
pending-text editing.

## Security/correctness audit follow-up (updated 2026-09-05)

`AUDIT.md` is the immutable finding snapshot. Commit
`304e677ec8c6d8caf607f373a23fc3ba9f095f5f` implements the first remediation
wave. Commit `3d12d35c531884296565f671def6670c4e1b6282` attempted the post-fix
`invalid_arg` audit; the adversarial rereview in
`d2c230c122d1c176b5fe5b430f41735326397927` corrects its graph and cache
assumptions. The final adversarial pass in
`a76431516fe6982259eb3eb29b3a1db696f05a54` also makes decoding granular when
one record has a wrong-typed member, preserves cancellation/backtraces across
cleanup, verifies forwarded-room-key identity from the imported key, and adds
structured operation context at Eio boundaries. These commits were checked
against the
adjacent matrix-rust-sdk at
`f4b9512df23332fce1bd26037ef7a2387af2ced2`. This section records the current
disposition so a later agent should not reimplement already-correct behaviour
from the original finding text.

### Completed audit fixes

- [x] **A0.1 — Remove partial attacker-facing paths and prevent their return
  (H3, S2).** Every `assert false` and `failwith` under `lib/` was replaced by a
  total construction or typed error. This includes attachment metadata,
  dehydrated-device encoders, send-queue restore, OAuth form/listener handling,
  push-rule selection, QR/recovery/runtime projections, sliding-sync codecs and
  atomic-write exhaustion. A second exhaustive `invalid_arg`/raise review found
  and fixed the remaining malformed send-queue restore paths; all remaining
  sites are public preconditions, intentional Eio/control-flow exceptions or
  exceptions converted at a typed storage boundary.
  `test/check_no_partial_failures.sh` is a `runtest` source guard. When extending
  it, distinguish an invalid public precondition from remote or persisted data;
  do not mechanically ban all `invalid_arg` uses.

- [x] **A0.2 — Redact nested E2EE material from debug bodies (H4).** One
  recursive policy now replaces credential, session, pickle, recovery,
  secret-storage and private-key member values at every object/array depth while
  retaining nonsensitive diagnostic shape. The test captures both sides of a
  real request path and proves marker strings cannot reach the logger.

- [x] **A0.3 — Make Megolm and backup imports transactional and distrust outer
  backup hints (M1, M2).** Inbound Megolm decrypt advances a copied ratchet and
  commits it only after signature, MAC and padding checks. Backup import accepts
  only unsigned version-1 exported session keys, checks the derived session ID
  against the map key, ignores unauthenticated outer
  `first_message_index`/`forwarded_count`/`is_verified`, and documents that the
  legacy empty-string MAC requires callers to validate and pin backup
  `auth_data` and its public key.

- [x] **A0.4 — Bound retry, KDF and verification work (M3, M9, M10).** Locally
  computed send-queue backoff has injected `[0.5,1.5)` jitter and a hard cap;
  explicit `retry_after_ms` is exact, and `M_UNKNOWN` is retried only once.
  Verification admits at most eight active inbound flows per device and 128
  globally, routes an over-cap cancellation, and the Eio service removes
  terminal flow/prompt/report bookkeeping after final delivery. Portable
  room-key import rejects PBKDF2 counts over 2,000,000 before running the KDF.

- [x] **A0.5 — Preserve persistence failures as values (M5, L5).** The shared
  atomic writer returns a typed error for an invalid target or 100 temporary
  collisions, and profile/base/crypto/plugin callers commit in-memory
  generation, fingerprint and dirty state only after success. Sliding-sync and
  thread-subscription rollback now returns a combined error if restoring the
  previous slot also fails; exception paths log rollback failure and re-raise
  the original exception.

- [x] **A0.6 — Centralize canonical Base64 and authenticated media spooling
  (M4, M7, S4).** `Matrix_proto.Base64.decode` is the single validator for
  canonical standard padded or unpadded input and rejects whitespace,
  URL-safe characters, malformed padding and nonminimal trailing bits.
  `Encrypted_attachment.decrypt_spooled` owns the two-pass authenticate-before-
  output primitive used by `Media`. The caller-owned spool contract now states
  exclusive 0600 creation, quota and finally/unlink responsibilities; filename
  query encoding and response content types already have focused tests.

- [x] **A0.7 — Harden remaining runtime boundaries (M6, L2, L6).** OAuth turns
  unexpected Fetch form bodies and listener address families into typed errors;
  existing negative coverage pins HTTPS-by-default metadata, PKCE S256, state
  checks and loopback behavior. HTML image dimensions are positive canonical
  decimals capped at 16384, Matrix colors are strict hex values, and the shared
  replaceable media fetcher uses `Atomic.t` rather than an unsynchronised ref.

- [x] **A0.8 — Finish the adversarial queue, import and exception rereview.**
  Persisted send-queue entries are first decoded as generic JSON and then
  checked independently, so one wrong-typed member cannot make every unrelated
  queued request disappear. Invalid records, their real same-room upload
  parents and all descendants are quarantined; duplicate graph/transaction
  identities, invalid optional transaction IDs, impossible cache ownership and
  malformed event shapes remain unsendable. Cancellation and retry transaction
  IDs are allocated before mutation, and secure-random failures roll state back
  before re-raising with the original backtrace. Forwarded room keys compare
  the declared session ID with the ID derived from the imported key and retain
  the parsed canonical sender key rather than an unauthenticated outer value.
  Profile-lock and QR-rendezvous cleanup run cancellation-protected, recovery
  observers propagate cancellation, and focused hostile tests cover each
  boundary.

### Rechecked findings that need no implementation

- [x] **A1.1 — QR one-shot and rendezvous lifecycle (M8).** The pending ECIES
  secret reference is cleared before any fallible key agreement/authentication,
  repeat establishment returns `Pending_consumed`, and rendezvous close/expiry
  are terminal and already covered by double-use/closed-channel tests. Reliable
  wiping of the underlying GC-managed key is the separate A2.1 design below.

- [x] **A1.2 — Eio error surface (M11).** `matrix-chat.client` is the result-returning
  API. `matrix-chat.eio` is deliberately its raising convenience layer, with every
  conversion centralized and documented in `Matrix_eio.Error.unwrap`; callers
  wanting values use the corresponding client function through `Client.base`.
  Duplicating every function as `*_result` would add a third API surface without
  changing failure semantics.

- [x] **A1.3 — Presence, rotation, transaction IDs and report score (M12,
  L1, L3, L4).** Presence omission for `Online` is documented and round-trip
  tested. The real encrypted send path rotates immediately after the configured
  message limit. Transaction IDs come from 128 secure random bits; the
  deterministic `of_bytes` helper requires caller-supplied secure input and has
  no library call sites. Current matrix-rust-sdk no longer sends report `score`;
  the OCaml compatibility argument is explicitly deprecated, ignored and
  tested absent on the wire.

### Remaining design work and evaluated simplifications

- [ ] **A2.1 — Design an honest secure-memory representation (H2).** Current
  Ed25519/Curve25519 and symmetric crypto APIs retain keys in immutable OCaml or
  Mirage Crypto values that the GC and libraries may copy. The landed policy
  documents this, clears owned transient random buffers and consumes one-shot
  references promptly, but does not claim zeroisation. A real implementation
  needs a separately reviewed foreign/locked mutable buffer abstraction,
  crypto primitives that do not immediately copy it back into strings, explicit
  destroy/ownership rules for account, Olm/Megolm, SSSS, backup, export and QR
  keys, and tests at the C boundary. Merely applying `Bytes.fill` to one copy is
  not completion. Until then recommend process isolation and OS restrictions on
  core dumps, swap and debugger access.

- [x] **A2.2 — Keep the `matrix_eio` wrappers explicit (S1).** The review found
  20 small wrappers (about 850 lines), but their type re-exports, optional and
  labelled argument shapes, store-only operations and hand-curated public odoc
  are not described by one useful functor. Generating interfaces would create a
  second signature language and make the installed API harder to review; an
  ML-only generator would save too little after its schema and drift checks are
  counted. Behavior-owning modules remain handwritten, and the explicit
  result/raising boundary remains as described by A1.2. If this is revisited,
  generate only allowlisted `.ml` implementations while retaining checked-in
  `.mli` files and require a measured net reduction.

- [x] **A2.3 — Do not abstract semantically distinct store pipelines (S5).**
  `Profile_store` has ordinary typed absent/malformed handling, `Crypto_store`
  deliberately substitutes/logs malformed state inside an odd/even generation
  transaction, and `Store` owns version migration, dirty state, locking and a
  fingerprint compare-and-swap. The only common fragment left is a few lines of
  JSON encoding plus the already-shared `Profile_store.atomic_write`; another
  helper would add surface while obscuring transaction semantics. Reconsider
  only if a future backend produces a larger genuinely identical unit with
  byte-identical and stale-writer failure tests.

- [x] **A2.4 — Keep examples standalone teaching material (S6).** No example or
  binary enables insecure transport, and the examples already use
  `Matrix_eio.run_sync` rather than reimplementing filter/presence behavior.
  Their READMEs deliberately show the small login/setup sequence;
  `Matrix_bot.Context` already centralizes the richer bot lifecycle. Depending
  on `matrix-chat.bot` would obscure the lessons and invert layering. A future local
  `bin/omatrix/Omatrix_context` may reduce CLI-only profile/session setup, but it
  should not become a generic library abstraction unless multiple applications
  first demonstrate the same OAuth, persistence and cancellation lifecycle.

The audit's H1 at-rest StoreCipher work and S7 dependency/pin packaging work
are intentionally excluded from this remediation run by user direction. They
remain known limitations, not completed boxes.

## Module/library re-audit implementation queue (updated 2026-09-05)

This queue audits current OCaml code
`a76431516fe6982259eb3eb29b3a1db696f05a54`, the adjacent Rust SDK HEAD
`f4b9512df23332fce1bd26037ef7a2387af2ced2`, and the local HTTPz checkout at
`5c9ed3758bbca4f5865cc83737258b72f2be92b7`. It is additive to the pinned
parity baseline below: fixing an item here must not silently widen the Rust
feature-gated parity target.

Take the boxes in dependency order. A box is complete only when its public API,
all call sites, hostile-input/unit tests, package metadata and the relevant
status text land together. Prefer one independently reviewable commit per box.

### R0 — fix now: JSON and trust-boundary correctness

- [x] **R0.1 — Put every Matrix Jsont codec behind one checked facade.** Add a
  protocol-level `Matrix_proto.Json.Codec` (implemented in `matrix_json.ml` or
  a dedicated `matrix_json_codec.ml` and re-exported there) with
  `type 'a t = 'a Jsont.t`. It should expose the subset of Jsont constructors
  and combinators used by this tree, rather than blindly exposing every raw
  primitive. All new and migrated codecs should be built through this module.

  The facade owns the semantic checks once:

  - Build `int` and `int64` by mapping the checked number codec, not by mapping
    Jsont's stock integer codecs: the latter have already accepted strings or
    truncated fractions before a user map can inspect them. They accept JSON
    numbers only, reject numeric strings, reject non-finite or fractional
    values before conversion, and reject values
    outside Matrix's interoperable `[-(2^53)+1, (2^53)-1]` range. Encoding must
    never fall back to a JSON string. Provide signed/unsigned and explicit
    bounded variants in terms of this one checked primitive rather than
    repeating range maps in endpoint modules.
  - Smaller integer codecs (`int8`, `int16`, `int32`, `uint8`, `uint16`) apply
    the same integral-before-conversion rule. Jsont 0.3 documents truncation for
    its stock integer codecs, so wrapping only `int`/`int64` is insufficient.
  - A floating-point codec, for schemas which genuinely say `number`, rejects
    NaN and infinities on both decode and encode. It must remain distinct from
    the integer codecs.
  - Checked string and generic-JSON codecs reject invalid UTF-8 produced by an
    OCaml caller. The generic JSON walk checks strings, member names and finite
    numbers recursively. Decide and test duplicate-object-member policy once;
    signed/canonical JSON must not admit an ambiguous object. Jsont's Bytesrw
    decoder is last-member-wins, so walking the decoded generic value is too
    late to detect duplicates from wire text. If the chosen policy rejects
    them, enforce it in this same facade's untrusted-string decode path before
    handing tokens to the Jsont codec, as well as when validating caller-built
    generic values for encoding.
  - Put uniform string maps, identifier-keyed maps, nullable response members,
    RFC3339 `Ptime` values and other genuinely cross-package combinators beside
    these primitives. Move the duplicates from `Matrix_string_map`,
    `Matrix_client.Json_codec`, sliding sync, `To_device` and `Server` onto it.
    URI validation stays at the client/Fetch boundary rather than adding `uri`
    or Fetch/Eio dependencies to `matrix-chat.proto`.
  - If old profile/pickle formats need Jsont's permissive number-or-string
    decoding, expose that only under an explicitly named `Legacy` submodule.
    Wire codecs must never use it accidentally.

  Migrate the 142 direct library `Jsont.int`/`Jsont.int64` uses, starting with
  `matrix-chat.proto` wire types, then `matrix-chat.client` requests/responses, then UI
  and persistence. Add an `rg`-based guard test which permits the unsafe stock
  integer primitives only inside the facade/legacy implementation. Acceptance
  tests must cover numeric strings, fractional positive and negative values,
  `1e999`, both safe-integer limits and the values immediately outside them,
  platform `int` bounds, invalid UTF-8, and encode as well as decode failures.

  Keep depth/resource enforcement at the I/O boundary so `matrix-chat.proto` stays
  pure: make `Client.Http.decode_response` the single untrusted string decoder
  and implement it with `Fetch.Json.decode_string'` (default depth 128). Route
  Matrix error decoding, redaction parsing, OAuth/rendezvous JSON and all other
  network-body decodes through it. Test depths 128 and 129 and retain the
  existing 64 MiB body limit. Do not copy HTTPz's depth scanner into this tree.

  **Completed 2026-09-05.** `Matrix_proto.Json.Codec` now owns the checked
  primitives, generic-value validation, duplicate-name rejection and legacy
  compatibility codecs; wire codecs and untrusted body decoding use the
  facade. The permissive migration codecs are allowlisted to nine persistence
  modules and shared persistence-only timestamp/raw-event wrappers, including
  nested event timestamps and unsigned ages. Numeric-string and pre-Jsont-0.2
  fixtures cover the migration paths.

- [x] **R0.2 — Make signed JSON actually canonical.** Rebuild
  `Matrix_proto.Signed_json` on the R0.1 recursive canonical-value validator.
  The previous implementation emitted fractional values, permitted `2^53`,
  and could emit an exponent for larger values, although Matrix canonical JSON
  permits only integers through `2^53-1`. Reject non-integral, non-finite,
  out-of-range, invalid-UTF-8 and ambiguous-object input before emitting any
  bytes; render negative zero as `0`.

  Prefer a result-returning entry point for signing paths. A compatibility
  wrapper may retain the present `Invalid_argument` API, but crypto code must
  not sign after validation failed. Add the Matrix canonical JSON examples,
  both numeric limits, one-beyond-limit values, fractional values, negative
  zero, Unicode key ordering, invalid UTF-8 and duplicate-name cases. Re-run
  device-key, one-time-key, cross-signing and SAS signature fixtures.

  **Completed 2026-09-05.** `Signed_json.canonical_json_result` validates
  through the facade, emits only Matrix-safe integers and renders negative zero
  as `0`; the raising `canonical_json` API remains source-compatible and
  signing paths validate before signing.

- [x] **R0.3 — Bring Matrix identifiers up to the current grammar.** Replace
  `Matrix_id.Server_name`'s character-class check with complete DNS, IPv4,
  bracketed-IPv6 and optional numeric-port validation. `Domain_name` and
  `Ipaddr` may validate the host pieces, but preserve the original spelling and
  case because Matrix server names are case-sensitive.

  Accept historical user localparts containing any valid non-surrogate Unicode
  scalar other than colon/NUL, including the empty localpart; continue to mark
  those as non-conforming for new IDs. Apply the specified 255-byte complete-ID
  limits to user IDs, room IDs, aliases and event IDs. Represent the domainless
  room IDs introduced in Matrix 1.16; change `Room_id.server_name` to an option
  or replace it with an explicitly legacy accessor, then audit every caller.
  Keep room IDs opaque outside parsing. Import current Ruma/Rust identifier
  vectors and add malformed ports, IPv4/IPv6, Unicode, empty historical user,
  length-boundary and domainless-room tests.

  **Completed 2026-09-05.** Server-name parsing now validates DNS, IPv4,
  bracketed IPv6 and ports; historical Unicode localparts, 255-byte limits and
  domainless room IDs are covered by the identifier tests and callers use the
  optional room server name.

- [x] **R0.4 — Use one validated HTTP URL/origin representation.** Parse
  configured, discovered, OAuth and rendezvous URLs with
  `Fetch.Middleware.Url` at their trust boundaries. Delete the copies of
  `default_port`, `same_origin`, query flattening and partial HTTP-URL checks in
  `Client`, `Server`, `Oauth`, `Qr_login_rendezvous` and `Matrix_eio.Qr_login`.
  Keep public `Uri.t` values where source compatibility matters, but retain a
  canonical validated URL internally.

  `Client.config` must reject userinfo, query and fragment instead of failing
  later or letting `Uri.with_path` inherit a configured query. Handle a trailing
  slash explicitly. Check the Matrix specification and Rust behaviour for a
  non-root base path, then either join the path prefix correctly or reject it at
  configuration time; never silently discard it. Validate well-known
  `base_url` before use and test alternate IPv4 spellings, default ports,
  case-normalized origins, IDNA hosts, userinfo, query/fragment, trailing slash,
  redirects and the chosen base-path policy.

  **Completed 2026-09-05.** `Client.Url` wraps the validated HTTPz URL used by
  transport policy and owns canonical scheme/host/port parsing, malformed-port
  rejection, origin comparison and endpoint expansion. Configured and
  discovered base-path prefixes are retained and Matrix paths are appended
  beneath them, matching Ruma's path builder; queries and fragments remain
  forbidden on homeserver bases. Well-known, OAuth, MSC4388 and MSC4108 parse
  their wire strings before converting to the source-compatible `Uri.t`, so
  IDNA, userinfo and malformed authorities cannot be reinterpreted by `Uri`.
  Repeated OAuth/rendezvous queries survive request routing, and the duplicate
  origin helpers are gone.

### R1 — fix now: remove HTTP code already supplied by HTTPz

- [x] **R1.1 — Remove `Matrix_eio.Http`.** `Fetch_httpz.std` now defaults to
  `Httpz_tls.system`, which already performs lazy cancellation-safe system trust
  loading, DNS/IP peer validation, SNI/ALPN setup and RNG initialization. A
  custom authenticator is expressed as
  `Fetch_httpz.std ~https:(Httpz_tls.client ~authenticator)`, so the Matrix
  wrapper no longer owns a distinct policy.

  Replace the defaults in `Matrix_eio.Client`, `Matrix_eio.Oauth` and `omatrix`
  with `Fetch_httpz.std`; preserve every retry, cookie, rate-limit, concurrency
  and timeout CLI option. Update the two HTTP-policy tests to exercise the
  direct backend, remove the `Http` re-export and files, and update examples,
  odoc, `README`, `STATUS`, `ROADMAP`, `PORT_PLAN` and `CHANGES` without
  rewriting historical release notes. Remove direct `tls`, `tls-eio`,
  `ca-certs`, `x509`, `domain-name` and `mirage-crypto-rng` dependencies if
  `dune external-lib-deps` confirms no remaining owner.

  **Completed 2026-09-05.** The wrapper module and files are gone; Eio,
  OAuth, CLI and examples construct `Fetch_httpz.std` directly, including the
  direct `Httpz_tls.client ~authenticator` custom-TLS path. Policy tests cover
  retry, cookie, rate, concurrency and timeout wiring.

- [x] **R1.2 — Use typed HTTP headers and the now-available date parser.** In
  OAuth metadata discovery replace the hand-written comma/quote/max-age parser
  with `Fetch.Header.cache_control`. Preserve `no-store`, `no-cache`, the
  24-hour cap, stale-on-refresh-failure and malformed-value fail-safe behaviour.
  Honor `Expires` after applying normal Cache-Control precedence, using
  `Fetch.Header.expires` and `Httpz.Date`; the old note that no robust parser is
  available is now stale. Use a monotonic/injected clock for cache age rather
  than making TTL correctness depend on wall-clock jumps.

  Change the MSC4108 transport to carry typed ETags and media types and build
  `If-Match`/`If-None-Match` with the matching Fetch header codecs. Accept
  `text/plain` with legal parameters, parse `Expires` into `expires_at`, and
  retain the earliest caller/server expiry. Cover quoted directives, extensions,
  weak/malformed ETags, parameterized content types, all supported HTTP-date
  forms, clock rollback and already-expired responses.

  **Completed 2026-09-05.** OAuth metadata now delegates Cache-Control
  grammar to `Fetch.Header.cache_control`, applies `Expires` only after normal
  directive precedence, converts its wall deadline into a capped lifetime and
  ages entries with an injectable monotonic clock. MSC4108 responses carry
  typed ETags/media types, conditional requests use the Fetch codecs, and the
  client adapter parses `Expires` with `Httpz.Date` while preserving the
  earliest caller/server deadline. Tests pin quoted extensions, malformed and
  weak validators, parameterized `text/plain`, all three HTTP-date forms,
  rollback, inclusive expiry and precedence.

- [x] **R1.3 — Replace the OAuth loopback HTTP implementation with Proffer.**
  The current module manually parses request lines and writes response framing.
  Add/pin `proffer` and `proffer-httpz` from the current HTTPz source (they are
  not installed in the present switch), serve one exact `GET /callback` route,
  read its query through `Proffer.Req`, and emit the existing HTML with typed
  `Content-Type` and `Cache-Control: no-store`.

  Start on loopback port zero, obtain the selected port from `on_listening`
  before dynamic OAuth registration, and resolve Proffer's stop promise only
  after a valid callback. Preserve the present behaviour of continuing after
  malformed/wrong-path requests and propagate parent cancellation. Test slow or
  oversized heads, wrong methods and paths, disconnects, repeated bad requests,
  a successful callback, shutdown and exact redirect-URI registration.

  **Completed 2026-09-05.** `Matrix_eio.Oauth.Loopback` now gives Proffer one
  exact decoded route, reads the typed request query and returns Proffer's
  typed HTML response with `Cache-Control: no-store`. Port readiness is
  resolved before registration; malformed HTTP or OAuth traffic cannot consume
  the callback;
  the server runs under its own child switch so resolving its stop promise
  closes the listening socket while parent cancellation still propagates.
  Hermetic socket tests cover partial disconnects, wrong path/method, oversized
  and slow heads, canonical encoded custom paths, the successful query and
  post-response shutdown. Package metadata now names `proffer` and
  `proffer-httpz` directly.

- [x] **R1.4 — Centralize endpoint expansion with HTTPz URI templates.** The 28
  client modules currently combining `Printf.sprintf` and `Uri.pct_encode`
  should use pre-parsed `Httpz.Uri_template` values through one internal Route
  helper. Use path-segment expansion for Matrix IDs/event types/state keys and
  typed query construction for query values. Do not change all endpoints in one
  unreviewable rewrite: land the helper and adversarial vectors first, then
  migrate coherent endpoint families while retaining the encoder guard.

  Acceptance must cover slash, percent, space, Unicode, `!`, `#`, `:`, empty
  state key, already-percent-looking input and repeated query keys. Run the
  existing escaped-room-ID live case against both Synapse and Dendrite after the
  migration. Add `httpz` as a direct `matrix-chat.client` dependency if its public
  template API is referenced directly.

  **Completed 2026-09-05.** `Matrix_client.Route` parses each endpoint template
  once, rejects non-path syntax and incomplete/duplicate/unknown bindings, and
  expands UTF-8 values with the Matrix/Ruma path-segment encode set on top of
  `Httpz.Uri_template`. All endpoint modules that previously combined
  `Printf.sprintf` with `Uri.pct_encode` use the helper; only the five
  Matrix.to permalink encodes remain outside it. Adversarial tests cover
  reserved delimiters, Unicode, already-encoded-looking input, empty state
  keys, invalid bindings, retained homeserver prefixes and repeated queries,
  and a source guard prevents direct endpoint encoding from returning.

### R2 — fix now: small library and runtime consolidations

- [x] **R2.1 — Use `Eqaf` for secret/digest equality.** Replace the independent
  loops in `Olm_primitives.ct_equal` and
  `Encrypted_attachment.Decryptor.equal_bytes` with `Eqaf.equal`, add the direct
  dependency, and keep fixed-length protocol validation before comparison where
  length is part of the public format. Run Olm/Megolm, attachment-tamper, backup,
  secret-storage, SAS and QR crypto tests.

  **Completed 2026-09-05.** Both secret-byte comparators use `Eqaf.equal`,
  with fixed-length validation retained at protocol boundaries.

- [x] **R2.2 — Use OCaml 5.5 and Ptime helpers instead of local byte/time
  routines.** Replace the duplicate UTF-8 validators with
  `String.is_valid_utf_8`, QR 16-bit readers with `String.get_uint16_be`, and key
  export 32-bit readers/writers with the standard `String`/`Bytes` big-endian
  functions plus explicit unsigned conversion. Replace
  `Ptime.of_float_s (Unix.gettimeofday ())` where an absolute instant is wanted
  with `Ptime_clock.now`; keep TTLs, deadlines and retry delays monotonic and
  injectable. Preserve binary vectors byte-for-byte and test boundary lengths.

  **Completed 2026-09-05.** The OCaml 5.5 string/bytes endian helpers,
  `String.is_valid_utf_8` and `Ptime_clock.now` replace the duplicated local
  routines while injected monotonic clocks remain in deadline/TTL paths.

- [x] **R2.3 — Move all SQLite open/schema/close work off the Eio domain.** Both
  `Matrix_ui_sqlite.create` and `create_media_store` call `Sqlite3.db_open`
  synchronously, contradicting the backend's claim that every SQLite call uses
  a systhread. Perform open, pragmas, migration/schema validation and error-path
  close inside the appropriate `Eio_unix.run_in_systhread` call. Test malformed
  and future schemas, migration, cancellation, close after partial creation and
  simultaneous event/media handles. Evaluate `caqti-eio` separately; do not
  combine this correctness fix with a wholesale backend rewrite.

  **Completed 2026-09-05.** Event and media SQLite creation, schema work and
  error-path cleanup now run inside `Eio_unix.run_in_systhread`; malformed,
  future-schema, migration and concurrent-handle tests cover the boundary.

- [x] **R2.4 — Consolidate persistence and internal helpers without changing
  semantics.** Introduce one Eio-capability-preserving atomic JSON-file helper
  for profile, base, crypto and plugin stores: unique same-directory temporary
  files, mode 0600, cleanup on every exception and atomic rename. Decide and
  document fsync/directory-fsync guarantees. Fixed `.tmp` names are unsafe for
  concurrent handles. Do not replace these paths with synchronous Bos I/O or
  weaken the existing profile advisory lock.

  Also fold the two Matrix-specific `*`/`?` dynamic-programming matchers into an
  internal `Matrix_glob` with separate whole-string and word-boundary entry
  points. Generic `Re.Glob` has shell/path semantics which do not match Matrix.
  Remove `Room.server_of_user` string reparsing in favour of
  `Id.User_id.server_name`, and share the precise Ptime and map codecs through
  R0.1. These are mechanical commits after the correctness foundations, not a
  reason to redesign the public SDK.

  **Completed 2026-09-05.** Profile, base, crypto and plugin snapshots share a
  cancellation-safe atomic writer using unique same-directory 0600 temporary
  files, file sync, rename and cleanup; native parent directories are now synced after rename (2026-09-08). `Matrix_glob` now owns whole-string and word-boundary Matrix glob
  matching, room routing uses the parsed user ID, and media previews use the
  protocol string-map codec.

- [x] **R2.5 — Attach structured context at every owned Eio I/O boundary.**
  `Matrix_client.Io_context` is the private common wrapper for direct path,
  flow, random-source and file operations. `Matrix_eio.Error.with_context` and
  the optional context on `unwrap`/`raise_client_error` give every raising
  facade operation a stable human-readable label. Both helpers catch exactly
  `Eio.Io`, capture `Printexc.get_raw_backtrace ()` immediately and use
  `Eio.Exn.reraise_with_context`; cancellation and non-I/O/programming
  exceptions therefore pass through unchanged. Cleanup which must finish uses
  `Eio.Cancel.protect`, while callbacks and best-effort persistence either
  preserve cancellation with `Printexc.raise_with_backtrace` or attach context
  only for the log/result they intentionally consume.

  The audit covers client response bodies and streaming callbacks, profile/base
  and crypto persistence, random reads, encrypted-media spools, all explicit
  `matrix-chat.eio` endpoint wrappers, OAuth/QR/recovery/dehydration, sync services,
  UI rollback and persistence, bot state, and SQLite systhreads. Context labels
  contain operations and public resource classes, never access tokens, JSON
  bodies, room content or complete request URLs. Result-returning HTTP
  boundaries deliberately rebuild the typed Eio cause and use a diagnostic URL
  with userinfo, query and fragment removed, because Fetch's lower context may
  contain the exact wire URL. Cancellation, cleanup, redaction and representative
  wrapper tests pass, as do the complete hermetic and two-server live suites.

### U — confirmed unfinished functionality

These are real missing or deliberately incomplete behaviours, distinct from
the implementation cleanup above. The detailed area notes later in this file
remain authoritative.

- [ ] **U0 — Complete external MSC4108 OAuth/MAS validation.** This is the
  existing P2.3 gate below: run both CLI roles against a real issuer/MAS and
  prove persistence-before-network, check-code confirmation, encrypted SSSS
  publication and decryption by the new device. The isolated rendezvous fixture
  is not sufficient.
- [x] **U1 — Implement the peeking endpoint family as explicit legacy/low-level
  APIs.** Implemented in `Matrix_client.Peeking` and its `matrix-chat.eio` facade;
  there is no fallback to `/sync`. There is no current `/peek` route. The
  implementation provides
  `Matrix_client.Peeking.initial_sync` for
  `GET /_matrix/client/v3/rooms/{room_id}/initialSync`, plus `events` and
  `peek_events` for `GET /_matrix/client/v3/events`; the latter adds the
  required `room_id` query and both accept optional `from` and `timeout`.
  Keep `Room_preview` as the preferred modern summary/state API and do not
  silently fall back to `/sync`, whose membership and authorization semantics
  differ.

  Build all three paths through `Route`. Reuse `Event.Raw_event.t`, but define
  separate response records: `initialSync` models membership and visibility as
  optional (including the current `knock` membership), defaults
  account-data/state lists to empty, and keeps its message and state room-event
  records strict. The `/events` chunk is raw JSON because Synapse mixes
  presence objects into it; its `start` and `end` are optional to match the
  pinned Ruma model. Export the pure module and matching raising
  `matrix-chat.eio` facade. Tests must pin percent-encoded room IDs and query tokens,
  absent optional members and invalid enums/shapes, Matrix error mapping and
  the fact that these GETs carry no body. A live world-readable-room scenario
  may treat 404/405/`M_UNRECOGNIZED` as an unsupported legacy capability, but
  must not report unsupported as success without saying so. The adjacent Rust
  SDK at `f4b9512d` has no public peeking API and itself records peeking as a
  TODO, so this is spec parity rather than a Rust implementation to copy.
- [x] **U2 — Add current MSC4140 delayed-event APIs without replacing the
  Synapse-compatible defaults.** Implemented with explicit legacy/current
  entry points in `Matrix_client.Delayed_events` and `matrix-chat.eio`; existing
  `send`, `send_state`, `update` and
  list calls deliberately use the older routes that Synapse and the Rust
  widget layer still use. The explicit current entry points use
  `PUT .../org.matrix.msc4140/rooms/{room_id}/delayed_event/{event_type}/{txn}`.
  Its JSON body contains integer-millisecond `delay`, optional `state_key` and
  raw `content`; optional `org.matrix.msc4354.sticky_duration_ms` belongs in
  the query only when the caller has independently confirmed MSC4354. Generate
  a state-event transaction ID once before sending, or expose `?txn_id`.

  The direct single-event GET and current update form are also implemented:
  `POST .../delayed_events/{delay_id}/{action}` with `{}`. The current update
  deliberately sends no bearer token, following Ruma's verified
  `NoAccessToken` metadata. Current delayed-event decoding requires `content`,
  `delay` and `running_since`, retains `finalised_ts`, and derives
  scheduled/sent/failed/cancelled status. The old `Page.t` list API remains for
  source compatibility even though the current response has no `from` request
  member. Current durations decode as unsigned values, while the legacy list
  retains its historical absent-field defaults. Mock tests pin both
  generations' route/body/auth behavior, negative-delay rejection, the
  MSC4354 one-hour maximum, empty state keys, escaped IDs, status derivation
  and missing fields. No `/versions` flag distinguishes the two route
  generations: the implementation never probes with a mutation, falls back
  after an ambiguous result, or automatically replays update/send operations.
- [ ] **U3 — Finish transport and live-harness coverage.** The default
  `Fetch_httpz.std` transport still has no connection pool or HTTP/2, but the
  existing `~fetch` seam accepts `Fetch_curl.std`, which provides both; the
  optional setup is now documented. `fetch-curl` is not in opam-repository and
  has not yet been added to this project's release/CI matrix. The replay-safe
  POST subtask is complete: HTTPz `5c9ed375` supplies a request-level veto shared
  by response and connection-failure retries, and
  `Matrix_client.Http_retry.default` admits only canonical replayable
  `POST .../_matrix/client/v3/keys/query`. The default Eio and CLI transports
  use it; injected HTTPz/curl transports can reuse it. Mock coverage rejects
  key claim, key upload, sync, near misses, extra segments and encoded
  separators on both retry paths.
  `Client.with_request_timeout` now bounds a complete
  logical exchange, including body consumption and refresh/replay, when a
  monotonic clock is supplied. The fixture scripts now support independently
  named containers/data directories and sentinel-guarded purge. The combined
  local runner owns fresh names, runs the full Synapse reference suite before
  Dendrite's additive core profile, and has an exit cleanup trap. Live CI still
  needs per-job Docker socket/port ownership and job wiring; server-side 429,
  the optional vodozemac oracle and federation remain unexercised by CI. Treat
  these as separate deliverables rather than broadening `Fetch_httpz.std`
  retries.

  One upstream diagnostic capability is still useful after HTTPz
  `5c9ed375`: Fetch can mark query credentials introduced through
  `Credential.Query`, but an application cannot mark ordinary query parameters
  (OAuth metadata endpoints, login hints, opaque Matrix cursors/filters) as
  sensitive before the transport attaches its exact URL to an `Eio.Io`
  context. Specify one request-level sensitivity/redaction policy in HTTPz,
  shared by policy-denial reasons and transport contexts, without changing the
  URL sent on the wire or asking Matrix to parse exception strings. Acceptance
  needs ordinary and credential queries, redirects, `Fetch.Denied`, connection
  and TLS errors, nested `Eio.Exn.add_context`, and proof that path/host remain
  useful while userinfo/query/fragment secrets never reach rendered
  diagnostics. Until that lands, Matrix intentionally strips all three pieces
  when converting transport exceptions to result values.
- [ ] **U4 — Finish the remaining UI scale work.** Persisted room history now
  loads lazily: a cold cache reads the complete chunk layout and stable
  identities but decodes only the newest events chunk, then hydrates one local
  predecessor before any remote back-pagination. `Event_store.Lazy_S` and
  `v_lazy` leave the original eager backend contract compatible; memory and
  SQLite implement the lazy path, incremental writes preserve unloaded rows,
  and malformed/missing chunks fail closed without changing the topology.
  `Room_timeline` maintains reaction/edit/redaction contributions by stable
  event and target across refreshes, removing stale entries on replacement,
  trim and forget; refresh still scans the bounded resident snapshot to
  synchronize positions and project visible items, but no longer reconstructs
  all relation tables. The remaining scale/correctness work is:

  - replace the OCaml chunk-list spine if O(1) interior linked-chunk splices are
    required; current operations are bounded at chunk granularity but O(chunks);
  - decide how an interior trim with no safe pagination token should behave;
    it deliberately remains over budget rather than inventing a false boundary;
  - give the cache its own-user identity before copying Rust's special case for
    a limited all-duplicate window containing only the user's sent events; and
  - add cancellation and concurrent-hydration race coverage beyond the
    deterministic lazy-store acceptance suite.

  `Observable.List.reconcile_by` now bounds pathological reorders with a
  thresholded `Reset`. Typing remains a separate room observable and live
  forward pagination remains sync-driven, matching Rust; automatic MXC
  rewriting is deliberately an explicit renderer callback, as in Rust's
  caller-owned media fetch path. These are API boundaries, not unfinished
  runtime features. The incremental relation index has focused
  add/redact/deduplicate/equal-timestamp/backfill/forget/reinsert coverage.
- [ ] **U5 — Close the recorded verification/crypto edges if promoted.** The
  legacy SAS MAC now has an optional vodozemac `libolm-compat` oracle and live
  vector/flow coverage; wildcard verification cannot fan out an
  “all except selected device” cancellation, and another user's
  individual-device verification is caller-owned. `next_method` now has codec
  and built-in-flow regression coverage: it is retained, while custom chained
  method execution remains an explicit caller/plugin boundary. Crypto storage
  is unencrypted locally; recoverable crypto journals now handle interrupted
  saves and clears. Store encryption and the deferred platform features need
  explicit compatibility/security designs.
- [x] **U6 — Remove misleading unfinished markers and stale documentation.**
  `matrix_sliding_sync.mli` had said `set_presence` was unimplemented although
  the client and Eio loops support it. The presentation fallback is
  `Presentation.Unknown_membership`; every standard membership transition is
  already handled and only custom/unknown strings use that fallback. Keep
  MSC4388's unsupported secure channel and QR raster rendering/scanning
  explicitly labelled as intentional boundaries, not hidden TODOs.

  **Completed 2026-09-05.** Removed the stale `set_presence` claim, renamed the
  presentation fallback to `Unknown_membership` across the public API, bot and
  tests, and retained the explicit MSC4388 secure-channel and QR raster
  boundaries.

### Not library substitutions

Keep the Matrix-specific Olm/Megolm implementation, signature/state machines,
send queue, `Base_client` fold, Observable/Event_cache semantics, HTML
sanitisation, recovery-key Base58 framing and protocol glob rules local. The
available generic OAuth/OIDC, Base58, LRU, React, regular-expression and JSON
deriving packages either cover a different layer or change required wire/state
semantics. Reconsider one only with interoperability fixtures and a measured
reduction in owned code.

## Parity re-audit handoff (updated 2026-09-05)

This is the pickup list produced by comparing ocaml-matrix, updated through
`a76431516fe6982259eb3eb29b3a1db696f05a54`, with the checked-out
`../matrix-rust-sdk` at
`523b5af53a8fd9fae9e2bc981bfb01ac86fd2890`. It supplements the older area
notes below; it does not erase them. Rust feature-gated experiments stay
separate so “parity” does not silently expand whenever an experimental flag
appears. The two revisions above remain the comparison baseline rather than a
claim that Rust HEAD was followed. The adjacent Rust checkout is now at
`f4b9512df23332fce1bd26037ef7a2387af2ced2`; it has been consulted for
implementation advice, but has not had an exhaustive symbol/path audit. Items
derived only from that newer checkout are labelled as such and are not silently
promoted into the pinned parity gate.

At this code revision the hermetic suite has 64 executables, 1,457 test
cases/checks and three source guards. The live Synapse suite passes 60
scenarios in 109.397s. The
separate MSC4108 rendezvous fixture also passes its secure-channel scenario;
it deliberately does not pretend to be an OAuth/MAS deployment.

The implementation follow-through has now completed the bounded send graph and
media-cache lifecycle, MSC4308 transport/catch-up persistence, the recovery and
trusted shared-history lifecycle, complete restartable common sliding-state storage,
Rust-compatible one-time-key pool/publication semantics, and room forgetting
across every existing local store/view/queue. The Synapse suite also exercises
recovery enable/rotate/disable, a fresh-device SSSS recovery
followed by backup restore, and trusted pre-join history import. The open boxes
below have been narrowed to work that is actually absent; in particular, do
not reimplement endpoint/store layers already listed under a checked subtask.
High-level timeline receipt control, clearable profile fields, encrypted
in-room verification transport and Rust-compatible competing-request
cancellation have also landed in this closeout.

An agent should take one checkbox at a time, verify the Rust reference at the
pinned revision, and update the relevant detailed section below as facts
change. A box is done only when its codec/state migration, public API and tests
land together. Every new I/O module also needs its `matrix-chat.eio` wrapper and a
request-body entry in `test_encoders.ml` where applicable.

### P0 — correctness and security

- [x] **P0.1 — Persist a complete room-state projection.** `Store.room_info`
  now owns a versioned, generic current-state projection keyed by
  `(event type, state key)`, including unknown events and the metadata carried
  by full and stripped state. `Base_client` rebuilds members and power levels
  from it after restart, retains typed retention and stable/legacy
  service-member events, and exposes explicit whole-state, member and
  encryption-state completeness. Authoritative full state replaces stale
  entries; limited timelines do not incorrectly downgrade state coverage; a
  leave/invite/knock invalidates joined-room encryption completeness. The v1
  store fixture migrates to v2 and future versions fail closed. Acceptance is
  covered in `test_base_client.ml`, including full-state replacement,
  join/leave/rejoin, restart recipients and service-member human counts.
  Legacy sliding-state migration and public-alias retirement are complete under
  P1.4; the common fold and profile projection own the active path.

- [x] **P0.2a — Finish cryptographic sender identity and trust integration.**
  `Encryption` now validates master →
  self-signing → device
  chains, persists identity and previously-verified/rotation facts, derives
  `Unknown_device`, `Device_info`, `Sender_unverified`, `Sender_verified` and
  `Verification_violation`, and enforces the three Rust trust requirements.
  Authenticated room-key ownership and the claimed Ed25519 key are checked;
  the trust requirement and an explicit session `legacy` bit survive restart,
  so a new backup import cannot pass `Cross_signed_or_legacy` merely because it
  lacks sender metadata. Equal-index keys may enrich missing metadata without
  overwriting conflicting authenticated facts. Tests cover every requirement,
  identity rotation, mismatched ownership, store migration and backup imports.

  `Verification_service` now accepts a private cross-signing identity and, only
  after the exact current key was MACed, signs an own-user device with the
  self-signing key or another user's master key with the user-signing key. It
  uploads the signature under the protocol's bare device/key id, checks the
  homeserver's per-key `failures`, and withholds both local trust and success on
  rejection or a stale/mismatched private identity. Full sync-driven mock tests
  cover both publication shapes and failure paths.

  The room-scoped `Matrix_ui.Room_identity` observable now recomputes warning
  members after sync, scopes them to current room membership and excludes the
  own user. A first-seen other-user identity pins its master key; rotation
  retains that durable pin and produces a distinct `Pin_violation` without
  changing the core identity enum. Pin acknowledgement accepts only the current
  TOFU key and does not upgrade verification, while interactive trust re-pins
  and verifies; `Verification_violation` retains priority when both apply.
  `Secrets.open_secret_store` now validates the default SSSS key using the
  Rust-compatible PBKDF2-first/Base58-fallback rules, and
  `Secrets.import_cross_signing` fetches the three optional well-known seeds,
  refreshes the exact own-user `/keys/query`, and calls the atomic
  `Cross_signing.private_identity_of_secrets` importer. `Verification_service`
  accepts this store handle as a high-level integration path. Tests cover full,
  partial, stale and wrong-key imports plus exact request order.

  The application supplies the passphrase/recovery credential for
  credential-driven import; the credential is never persisted or logged, and
  decrypted imported material is held only in memory. This is separate from
  the local `Crypto_store`, which intentionally persists base64 private keys
  unencrypted at rest; QR/recovery publication puts private seeds in encrypted
  remote SSSS. A full self-signing import validates UTF-8, re-queries the exact
  own-user keys, and marks that own device verified; partial imports remain
  safe and matching. Reactive and expiry-aware proactive token refresh,
  persisted initial expiry, detached Eio completion and typed OAuth
  invalidation are implemented. The one-shot recovery lifecycle and fresh
  restore are complete under P1.3c; reactive recovery observation and the
  UIAA/password identity-reset path are complete under P1.3d. OAuth
  reauthentication/credential handoff remains separate work; the dehydration
  manager is complete under P1.3b. Do not
  replace a stored identity merely because a successful
  key query omitted invalid cross-signing data (Rust skips it too).

- [x] **P0.2b — Finish unable-to-decrypt reporting.** `decrypt_error`
  distinguishes missing sessions from unknown
  message indices and retains room/session/sender/index context. The pure
  classifier matches the pinned Rust causes for MSC4115 pre-join state, device
  age and backup state, trust failures and validated/persisted
  `m.room_key.withheld` evidence. `Matrix_ui.Runtime` caches UTD events,
  deduplicates room-key requests, retries them after successful sync and when a
  timeline opens, and replaces the same cached item when a late room/backup key
  works. Failed requests become eligible for retry, and a newly stored room key
  clears stale withholding evidence. The public `Matrix_ui.Utd_hook` adds a
  configurable grace period, late-decryption timing and sender/server/trust/age
  metadata. A persisted exact 4,096-event FIFO is used instead of Rust's
  probabilistic growable Bloom filter; duplicate pending, reported and reloaded
  observations emit once. `Sync_service` and `Runtime.start` expose
  `on_encryption_error` without dropping the rest of a sync response, and
  cancellation propagates. Unit tests cover expiry, late delivery and durable
  deduplication; the Synapse backup scenario drives the same hook with a real
  event that fails before restore and decrypts after the key arrives.

- [x] **P0.3 — Finish the encrypted attachment crypto/transport pipeline.**
  `Encrypted_attachment` implements Matrix/JWK
  validation, fresh AES-256-CTR keys/IVs, incremental encryption/decryption and
  SHA-256 over ciphertext. Low-level decryption explicitly releases
  unauthenticated chunks before `finish`; verified string/chunk helpers release
  nothing on tamper. `Media.upload_encrypted`, `upload_encrypted_stream` and
  `download_encrypted` connect the primitive to authenticated media transport.
  A pinned Rust vector, arbitrary chunk boundaries, malformed metadata and
  corruption are tested; the streaming upload checks one-shot/replay framing
  and optional exact length. Authenticated `Client.Http.get_stream` keeps the
  response body scoped to a callback and preserves normal Matrix error mapping.
  `Media.download_encrypted_stream` spools ciphertext, verifies its digest
  before releasing plaintext, rechecks the spool while decrypting, avoids the
  64 MiB buffered-response ceiling, and preserves cancellation. The Synapse
  scenario streams an upload, sends its `file` metadata through an encrypted
  room event, then has the receiving user authenticate, stream and decrypt it.

  Application-level upload/thumbnail/event dependencies and the media-store
  lifecycle are complete under P1.1; they remain separate from this
  byte/transport pipeline.

- [x] **P0.4 — Never display a SAS representation that was not negotiated.**
  `Verification_sas.emoji` and `.decimals` gate on the accepted
  `short_authentication_string`; valid decimal-only, emoji-only and normal
  flows cover both the state machine and the confirmation-facing accessors.

- [x] **P0.5 — Distinguish classic-sync and sliding-sync one-time-key count
  semantics.** `process_sync` treats an absent `signed_curve25519` count as
  zero, while `process_sliding_sync` retains the preceding count. The driver
  and Eio layer expose both folds, with `N -> absent` regression tests.

### P1 — pinned Rust core behaviour and labelled newer follow-ups

- [x] **P1.1 — Make `Send_queue` a persistent dependency graph.** The OCaml
  queue gives same-room entries queue-local dependency IDs, persists unresolved
  and typed resolved edges, blocks children behind retry/wedge, resolves a
  successful parent before callbacks, and recursively cancels a dependency DAG
  exactly once in deterministic order. Restore accepts stores predating the
  graph fields and drops malformed dangling/forward edges or upload records
  without discarding unrelated requests. In-flight cancellation records one
  durable optional-reason intent; success converts the same request slot to a
  redaction with a new collision-safe transaction id, while failure removes the
  local DAG. Ordinary edits, reactions and redactions are queue nodes, and
  messages/reactions accept non-overriding vendor `extra_content`.

  Durable original/thumbnail upload nodes now retain role, MIME type, filename,
  exact bytes and optional validated encryption metadata. Encrypted nodes store
  ciphertext only. Clear/encrypted upload results are distinct from event IDs,
  propagate atomically to dependants across restart, and use
  `Media.upload_stream`; progress starts at zero, clamps regressions and emits
  the terminal byte count only after result persistence. Cancellation removes
  an upload and its transitive dependants. These choices follow the staged
  upload/request split in the pinned Rust
  `matrix-sdk/src/send_queue/{mod,upload,progress}.rs`.

  `send_attachment` now validates and persists its complete two- or three-node
  graph in one transition, then exposes only the attachment's base-content
  local echo. Before the event PUT (and before encrypted-room wrapping), it
  resolves each typed dependency by ID and writes clear results as
  `url`/`info.thumbnail_url` or encrypted results as
  `file`/`info.thumbnail_file`, inserting the MXC into validated metadata.
  Generated fields remove the opposite spelling and win over vendor extras.
  Missing or wrong result types wedge safely. Restart before uploads and after
  each result, clear/encrypted and optional-thumbnail shapes, callback
  visibility, cancellation and the live upload/event/download path are covered.

  Caption mutation is also durable. A pending or wedged attachment changes its
  content without rebuilding the upload graph or transaction; an edit racing
  the final event keeps that event's payload stable and stores one
  last-write-wins `m.replace` with a collision-safe transaction ID. Both the
  edit intent and logical filename survive restart. Cancellation wins, and the
  event cache updates or migrates exactly one local echo while preserving an
  event already confirmed by sync. This follows
  `matrix-sdk/src/{send_queue/upload.rs,room/edit.rs}`.

  The media lifecycle is complete too. `Media_store` has generic, in-memory and
  SQLite backends keyed by local/MXC identity plus requested format, with the
  Rust defaults (20 MiB per item, 400 MiB total, 60 days and daily cleanup).
  The queue writes protected local ciphertext/clear bytes before persisting the
  request, reads uploads from the cache, exposes local URIs in attachment
  echoes, moves them idempotently to the remote MXC key, and recovers every
  tested crash boundary. Cancellation and stale-orphan reconciliation remove
  only this queue's local entries; remote entries become evictable. Runtime's
  implicit queue accepts the durable queue/media stores and stable media owner,
  so callers need not construct a queue merely to enable this path. Memory and
  SQLite restart/retention/cadence tests cover the acceptance gate. The
  replaceable network fetcher/content-scanner hook is complete under
  P1.1b; it remains deliberately separate from this queue checkbox.

- [x] **P1.1b — Add a replaceable network media fetcher.** This is a confirmed
  observation from the newer sibling HEAD
  `f4b9512df23332fce1bd26037ef7a2387af2ced2`, not an unfinished requirement
  from the pinned baseline. Use
  `crates/matrix-sdk/src/media.rs` (`MediaFetcher`, `DefaultMediaFetcher` and
  `get_media_content`) as the reference. `Media_fetcher` now provides that
  result-returning file/thumbnail capability and a default adapter around the
  existing capability-resolved `Media` calls. `get_content` checks an optional
  `Media_store` first, never sends a local send-queue MXC to any fetcher,
  authenticates encrypted bytes before release, and caches only success.
  Encrypted plaintext has a metadata-derived cache namespace: a legacy/
  queue-owned ciphertext entry is verified and promoted into it, so a cache
  hit can never accidentally return ciphertext as display bytes. Tamper and
  network failures do not populate the derived entry.

  `Matrix_eio.Client` owns a replaceable fetcher cell shared by token/session
  derivatives, and `Matrix_eio.Media.get_content` preserves the typed media
  and attachment errors. Ten focused checks cover exact request observation,
  replacement, derived-client sharing, memory and SQLite hits, bypass,
  encrypted promotion/tamper, failure non-caching and the local-URI network
  prohibition. Content-scanner policy can now be supplied through this seam;
  it remains deliberately independent of retention and rendering.

- [x] **P1.2a — Implement MSC4308 subscription transport and durable catch-up.**
  `Thread_subscriptions` implements capability discovery, get,
  subscribe/unsubscribe and paged changes, plus persisted per-room/thread
  status and ordered catch-up ranges. The updater saves extension changes and
  the catch-up range before `Sliding_sync` accepts the new position; restart,
  stale-writer conflict and retry tests prove that a crash cannot advance past
  unapplied changes. Classic per-thread receipts and unread calculations retain
  the receipt thread and deduplicate only within it.

- [x] **P1.2b — Finish the persistent per-thread cache projection.**
  `ThreadInfo`, `Thread_list`, the pinned-event projection, room-context
  `Event_focused` view, thread-focused `/relations`, receipt-target backfill,
  prioritized pagination and coordinated invalidation now share one durable
  `(room_id, thread_root_id)` cache identity. It stores the ordered root-plus-
  replies sequence and pagination metadata, ingests sync and `/relations`
  results, and publishes cancellable event/receipt/unread subscriptions.
  Restart, stale-response, cancellation and room-forget tests cover cache rows
  and in-flight backpagination, with the same lifecycle generation as the
  base/UI cache, MSC4308 rows, send graph and queue-owned media.

- [x] **P1.2c — Make room forgetting a complete local lifecycle.** After a
  successful server `/forget`, `Runtime` advances the sync lifecycle generation
  before any best-effort follow-up can yield. Responses fetched against an
  older generation are dropped; an apply already in progress is serialized
  with destructive state mutation. The final cleanup removes the room from
  `Base_client` and its persisted room/receipt state, `Event_cache` plus SQLite
  chunks, open timelines, MSC4308 subscriptions/catch-up rows, room-list and
  identity projections, and the whole persistent send graph. Pending queue
  requests and local media are cancelled/deleted; in-flight requests are
  detached until their callback returns, never persisted or converted to a
  compensating redaction, and cannot delete a deliberate post-forget request.
  Direct rooms follow the pinned Rust ordering with a best-effort fresh
  `m.direct` GET/PUT; success updates the local persisted account-data
  projection while remote failure is logged and does not undo the room forget.
  A failed server `/forget` performs no local cleanup. The persistent
  thread-cache rows and in-flight backpagination participate in this same
  lifecycle and its restart/race tests.

Device continuity was split into review-sized pieces; the implementation is
complete. External live validation is tracked separately below as P2.3:

- [x] **P1.3a — Complete QR login without conflating its formats.** Verification
  QR, MSC4108 login and MSC4388 login payloads remain distinct. The separate
  `Qr_login`/Eio modules complete the pinned Rust MSC4388 boundary: strict
  `IO_ELEMENT_MSC4388` codec/vector checks, unauthenticated rendezvous probing,
  403/404 capability outcomes and the same typed unsupported secure-channel
  result as Rust at that revision. MSC4108 now has strict codecs, conditional
  rendezvous, vodozemac-compatible ECIES, OAuth/login approval and
  authentication messages, secret handover after authentication, and a
  two-role Eio secure-channel flow with mock coverage.

  The exact MSC4108 creation endpoint is distinct from MSC4388 discovery. The
  Eio `Session` owns rendezvous/channel cleanup, mandatory check-code
  confirmation, progress (channel, check code, OAuth, secrets, trust/backup,
  done), cancellation and timeout. Profile preflight runs before login; the
  authenticated OAuth session is persisted immediately before encryption
  state and subsequent network work. Both `omatrix qr grant` and `omatrix qr login` are
  implemented, with the new-device flow generating a textual Base64 MSC4108
  payload. Private seeds and the optional backup key are published to fresh
  encrypted SSSS protected by a passphrase file or a one-time printed Base58
  recovery key. Rust FFI returns raw QR bytes to callers, so raster rendering
  and scanning remain application-owned, not missing SDK parity. MSC4388
  remains a distinct typed unsupported channel; it must not be conflated with
  MSC4108.

- [x] **P1.3b — Complete the dehydrated-device manager.** The MSC3814 endpoint,
  strict interoperable libolm/vodozemac account-pickle codec, independent
  signed V1 device with OTK/fallback publication, paged room-key rehydration,
  bounded drain/delete behavior, lifecycle subscriptions and cached-key weekly
  rotation now match the checked-out Rust implementation's boundaries. Failed
  rehydration preserves the old SSSS/cache key when a replacement was
  requested; skipped rehydration resets directly. The Eio manager keeps
  callback exceptions isolated while propagating cancellation and stops
  rotation before deletion.

  Focused API coverage includes Rust pickle fixtures, wrong-key/tamper checks,
  repeated cursors, the 100,000-event bound, real room-key import, callback
  ordering and manager start/stop/key-preservation/rotation tests. The live
  Synapse boundary covers capability, same-user identity, create/upload/get,
  explicit delete, empty events, rehydrate of an empty queue and primary
  identity preservation. The live scenario now also has Bob send into an
  encrypted room while Alice's dehydrated device is active, rehydrates the
  queued Olm room key into a fresh primary-side machine, and decrypts Bob's
  captured encrypted sync echo without replacing Alice's primary identity.

- [x] **P1.3c — Recovery, shared history and portable room-key files.**
  `Recovery` derives stable/unstable account-data state and implements ordered
  check, enable, recover, reset-key, recover-and-reset, disable, delete-all and
  recover-and-fix-backup flows. It creates/opens SSSS, exports/imports all three
  cross-signing seeds and the optional Base64 backup key, persists the machine
  before later remote work, repairs only Rust's typed missing/decryption or
  inconsistent-key cases, and preserves partial-write ordering. A fresh real
  login now recovers through SSSS, enables the server backup, restores its old
  Megolm session and decrypts the event; live enable/rotate/delete/disable is
  covered separately.

  The MSC4268 flow is also end to end. `Runtime.join`/`join_room` records a
  durable inviter gate only after a successful join, with strict 24-hour and
  current-membership cleanup. The sender restores missing keys from backup
  before building/uploading a bundle. Receipt validates authenticated Olm and
  the MSC4147 device proof, then the post-join sync path re-queries trust,
  downloads/decrypts media, imports only a cross-signed inviter's same-room
  sessions, retries transient failures, and discards malformed/404 payloads.
  A live trusted join decrypts pre-join history. `Room_key_export` additionally
  round-trips the interoperable passphrase-protected file format against Rust
  fixtures while retaining forwarding/shared-history metadata.

- [x] **P1.3d — Add reactive recovery state and identity reset.**
  `Recovery.Manager` caches and immediately publishes state, refreshes after
  sync account-data changes and successful or partial enable/recover/repair/
  disable transitions, and handles cancellation without stranding optimistic
  state. The identity-reset operation replaces the own cross-signing identity,
  invalidates old trust/session state, drives password UIAA, conditionally
  rebacks up, and preuploads device keys; malformed markers, restart, retry and
  remote-failure boundaries are covered by tests and a live Synapse scenario.
  OAuth reauthentication/credential handoff remains in the OAuth work, and the
  dehydration manager is complete under P1.3b.

- [x] **P1.4 — Retire the parallel sliding-sync state.** The common fold,
  typed/persisted MSC4262 profiles and cancellable profile subscriptions,
  MSC4308 stale-transaction ordering, presence wake, room-subscription
  cancellation and unknown-extension wire round-tripping are complete. The
  public `Sliding_sync_state` aliases are retired. The private legacy
  `sliding_sync_state` decoder and its transactional slot conversion are
  complete, with common-cursor precedence, slot consumption, rollback and
  restart coverage; only decoder compatibility remains and callers cannot
  start a parallel fold. `Adaptive_sync` automatic classic `/sync` fallback is
  implemented as a project enhancement, not pinned Rust parity: the pinned
  Rust API uses an explicit version choice.
  Treat optional MSC4426 `m.status`/`m.call` account data and automatic call
  status as a separate checkbox, not part of this acceptance gate.

- [x] **P1.5 — Implement retention as state plus effective policy.**
  `Matrix_client.Retention` validates and reads/writes stable
  `m.room.retention` state with an MSC1763 read fallback, fetches the absolute
  unstable server-configuration endpoint, and computes the pinned Rust
  precedence and independent min/max clamps. Server policy keys must be `"*"`
  or valid room IDs; negative or contradictory policies and limits fail
  validation. A per-room server policy overrides room state, the default policy
  supplies missing room state, and an unsupported configuration endpoint
  (`M_UNRECOGNIZED`, `M_NOT_FOUND`, 404 or 501) returns no effective policy
  without a room-state request, matching the Rust SDK. `Base_client` persists
  and reloads the typed state, `Matrix_eio.Retention` exposes the raising
  wrappers, five hermetic tests cover the table/validation/endpoints/restart,
  and the live Synapse scenario proves stable state plus the unsupported-server
  branch.

- [x] **P1.6 — Add room previews and knock-request moderation.**
  `Matrix_client.Room_preview` trusts persisted joined-room state, refreshes
  invited/knocked/left rooms through MSC3266, falls back to state plus joined
  members and finally to the local projection, and supplies Rust's default
  federation hint for a remote room. It projects create/predecessor,
  tombstone/successor and stable/legacy service-member facts from P0.1.
  `Knock_requests` derives current typed knock membership events, retains their
  profile/reason metadata, persists exact seen event ids atomically, and wraps
  invite/kick/ban as accept/decline/decline-and-ban. Both have `matrix-chat.eio`
  wrappers. Hermetic tests cover restart, fallback and exact requests; the live
  Synapse scenario covers preview plus every moderation transition. Rust's
  public-directory-search middle fallback remains part of P2.1e rather than
  this bounded preview API.

### P2 — high-level services

These are deliberately separate pickups, not one “rest of matrix-sdk-ui”
patch. Each observable service needs deterministic initial state,
close/unsubscribe semantics and a restart test where it owns persisted data.
Reuse P1.2b's cache and P1.6's previews/knocks rather than creating parallel
stores.

- [x] **P2.1a — Composer drafts.** Store, load and clear independent room and
  thread drafts, including reply/edit mode plus attachment and thumbnail
  metadata. `Matrix_client.Composer_draft` follows `ComposerDraft` in
  `matrix-sdk-base/src/store/traits.rs`, using a safely keyed `Store.Slot`
  with lossless base64 attachment bytes and integer-millisecond durations.
  Tests cover room/thread key isolation, attachment round-trip, restart,
  clear and migration from a store with no attachments member.

- [x] **P2.1b — Edit/send conveniences.** Return ordered edit revision history
  from the shared cache (`matrix-sdk/src/room/edit.rs`) and add a small room
  facade for permalinks/routing, invite details, DM target/mark-as-DM,
  power-level roles and static location/reply sends. Sends must return the same
  P1.1 handle/state as ordinary queued messages; redacted or invalid edits must
  not appear as revisions. The cache-only `Room_timeline.edit_revisions` API
  now returns the original plus valid revisions in chronological order, with
  encrypted-event provenance checks; it cannot fetch revisions outside the
  cached event window. `Relations.get_edit_revisions` also exhausts the
  plaintext relations endpoint, deduplicates pages and applies the same
  sender/type/relation/redaction checks. `Matrix_client.Room` supplies cached
  routing, alias/permalink, invite, DM and power-role projections; DM writes
  are batched and idempotent. `Room_timeline.send_reply`, `send_edit` and
  `send_location` all return the ordinary send-queue handle. Focused request
  tests cover invalid and cyclic pages, ACL/IP-literal routing, room-version-12
  creators and DM mutation; the live UI scenario exercises the network edit
  history and static-location round trips.

- [x] **P2.1c — Notification settings and client.** Port the command model in
  `matrix-sdk/src/notification_settings/`: modes, keywords and atomic push-rule
  mutations. Add the UI notification client over one event fetched by id and
  the common push evaluator. Test rule ordering, idempotent updates, encrypted
  event decryption and no duplicate notification when sync later carries the
  fetched event. `Matrix_client.Notification_settings` now computes and applies
  ordered batches for room/default modes, polls, keywords and stable/legacy
  mention rules, publishing local state only after a complete success;
  `Matrix_eio.Notification_settings` supplies raising wrappers.
  `Matrix_ui.Notification_client` first consults the shared cache, otherwise
  fetches `/context?limit=0` into an isolated cache, decrypts when possible,
  evaluates current rules/context/ignored users, distinguishes redacted,
  missing, filtered and undecryptable events, and hands a later sync copy back
  to the shared cache without duplicating it. Thirteen focused checks cover the
  command and lookup contracts.

- [x] **P2.1d — Paginators and thread list.** The room paginator is landed,
  including directional tokens, state guards, rollback, reset, target-not-found
  handling and overlap deduplication. A bounded `Thread_paginator` now pages
  the existing typed `/threads` endpoint, retains server order, deduplicates
  roots by event id, publishes state transitions, resets filters, suppresses
  overlapping/end requests as Rust does and retries failures from the same
  token. Mock tests cover those contracts and the live Synapse scenario lists
  a real root after a threaded reply. The rich server-backed list now consumes
  persisted `ThreadInfo`, receives sync-driven updates, owns its Runtime
  lifecycle and is covered by the live scenario. Thread-focused `/relations`,
  threaded receipt/backfill, prioritized pagination and invalidation are also
  implemented. P1.2b completes their shared durable per-thread event identity;
  raw roots alone no longer define this checkbox.

- [x] **P2.1e — Directory, search, spaces and room details.**
  `Matrix_ui.Room_directory_search` follows the pinned Rust state/reset/page
  contract, including serialized calls, result-count page calculation,
  remote-directory selection, terminal no-ops and failure retry without losing
  the token or results. `Matrix_ui.Search_service` deliberately streams the
  typed server `/search` endpoint because the Rust local search-index crate
  remains deferred; it preserves relevance/page order, exposes loading/errors
  and rejects overlap. `Matrix_client.Space_graph` projects mutual non-empty
  child/parent links between joined rooms into a deterministic cycle-free DAG,
  preserving explicit order/timestamp ordering and promoting a child whose
  only parent was left. `Room_details` and its Eio wrapper expose sorted,
  disambiguated active members, roles, account/service flags and human/service
  counts; an incomplete cache calls `/members` once and returns a refreshed
  immutable base-state snapshot, while a complete cache performs no I/O.
  Twenty focused checks cover page/reset/retry/order, malformed and cyclic
  space state, lazy-member replacement and classification. A live Synapse
  scenario exercises directory publication/search, event search, a
  real thread root and member refresh; its test-only configuration explicitly
  allows publication because Synapse 1.126+ denies it by default.

- [x] **P2.1f — Live locations.** Add start/stop/observe over beacon-info and
  location events, expiry driven by an injected clock, and timeline aggregation
  following `matrix-sdk/src/live_locations_observer.rs` and the corresponding
  UI content. Test replacement, redaction, timeout, stop and reload without
  resurrecting an expired share.

- [x] **P2.2 — Make the parity audit repeatable.**
  `tools/parity-inventory.sh` reads both repositories from immutable Git
  objects, records the full OCaml and pinned Rust SHAs, separates Rust's
  declared/default features from opt-in-only ones, and mechanically lists every
  `matrix-sdk` Rust source/direct public declaration beside every tracked OCaml
  public interface. `PARITY_INVENTORY.md` contains its copy/pasteable
  regeneration command; `--check` rejects a stale artifact. This remains an
  inventory, not a semantic-parity claim, and the separately observed sibling
  HEAD remains labelled rather than folded into the pinned gate.

  `test/integration/dendrite.sh` also pins Dendrite v0.15.2 by manifest digest
  and provides idempotent start/status/core-run/stop commands. The portable
  registration, room lifecycle, messaging, attachment and escaped-room-ID
  five portable rooms-core cases pass there (5/5). Dendrite's absent
  simplified-sliding-sync advertisement, rejected preallocated-media endpoint
  despite advertised MSC3916, and threaded-receipt timeout are recorded as
  capability/interoperability outcomes, not relabelled as test successes;
  federation is disabled. Synapse remains the full 60-scenario reference
  harness. `test/integration/run-both.sh` starts uniquely named instances of
  both servers together, runs that complete Synapse suite before Dendrite's
  additive core profile, and trap-cleans both on success, failure or signal.
  The latest 2026-09-05 live run passed Synapse 60/60 in 109.397s and Dendrite
  5/5 in 1.535s, with no container, data-directory or harness-process leftovers.

- [ ] **P2.3 — Validate the complete MSC4108 login against an external
  OAuth/MAS deployment.** This is external validation, not missing SDK parity.
  The committed `test/integration/msc4108.sh` fixture proves that Synapse
  1.159.0 can expose the experimental rendezvous servlet when it is enabled
  with a dummy delegated-auth endpoint: creation, PUT/GET/delete, two-party
  vodozemac-compatible ECIES, bidirectional encrypted messages and
  cleanup/cancellation all pass. That fixture is not an OAuth issuer or MAS,
  and the ordinary port-8008 fixture intentionally leaves the servlet off.
  Run a configured OAuth/MAS service with the exact MSC4108 creation endpoint
  (and separately verify the MSC4388 discovery probe), then exercise both QR
  roles. Acceptance requires profile preflight,
  OAuth-session persistence before encryption state and subsequent network
  work, mandatory check-code, progress, cancellation and timeout behavior, and
  fresh encrypted SSSS
  publication of the private seeds and optional backup key using a passphrase
  file or one-time Base58 recovery key; `omatrix qr grant` and `omatrix qr
  login` must complete and the new device must decrypt a test event.

  Fixture notes from 2026-09-05: the ordinary Synapse fixture returns 404
  `M_UNRECOGNIZED` for stable and MSC2965 OAuth metadata, MSC4108 rendezvous
  creation, and the separate `io.element.msc4388` rendezvous discovery probe.
  The isolated port-8009 fixture enables only the rendezvous servlet; it still
  has no usable OAuth metadata, issuer, registration or token service.
  The adjacent Rust SDK's secure-channel tests use a mocked rendezvous server
  and its QR example is interactive; it does not supply a live deployment.
  The `synapse-oidc.lab.element.dev` hostname embedded in its recorded MSC4108
  vector no longer resolves, so it is not a reusable fixture.

  The `omatrix qr grant` client adapter deliberately permits only rendezvous
  URLs with the homeserver's scheme, host and effective port. Put an external
  rendezvous implementation behind that origin (for example through the
  homeserver reverse proxy). If the chosen deployment must return a different
  origin, first add an explicit policy-gated transport option and SSRF tests;
  do not silently relax the safe default. Verify both
  `/_matrix/client/v1/auth_metadata` (or its MSC2965 fallback) and
  `/_matrix/client/unstable/org.matrix.msc4108/rendezvous` before starting the
  two interactive CLI roles.

### Explicitly deferred by this audit

- **Deferred unless promoted:** IndexedDB, widgets and capability
  negotiation, MatrixRTC (including authenticated RTC transport discovery),
  content-scanner and search-index crates, store encryption, experimental Olm
  v2/Megolm v2, MSC3956 extensible encrypted events, MSC4274 inline media
  galleries, MSC4362 encrypted state, MSC4385 push secrets, X.509 identity,
  MSC4426 status/call automation, and Element-specific recent emoji/recent room
  stores. These exist in the pinned Rust tree but are platform-specific,
  product-specific or feature-gated; their absence is intentional until scope
  changes.

## Build / packaging

- **`fetch`, `fetch-httpz`, `httpz`, `proffer` and `proffer-httpz` are not in
  opam-repository.** All five come from the development HTTPz tree.
  `.tangled/workflows/build.yml` now requires a published immutable
  `MATRIX_HTTPZ_SOURCE` and checks all five manifests. The checked public HEAD
  lacks four manifests; see FR3.
  Drop the pins once they are released, at which point CI builds from vanilla
  opam-repo. The 2026-09-03 working checkout builds against `httpz`'s current
  `Uriz` transport boundary. The same checkout currently declares
  `uriz_scanner` private while its installed `Httpz.Uriz.Scanner` alias and
  `fetch` require that implementation; a temporary local-only packaging patch
  was needed to install the five packages. Exposing/installing that module
  upstream was a historical packaging requirement; the current confirmed
  gate is publication of the complete source. Specifically, HTTPz commit `d74b6e7` removes
  `uriz_scanner` from `private_modules`; that one-line packaging fix is not an
  ancestor of `ba20631` and must be reapplied before the latter installs
  `fetch` cleanly without a local compiler include-path workaround.
- **CI pins OCaml 5.5.0.** The workflow creates a local
  `ocaml-base-compiler.5.5.0` switch before pinning or resolving dependencies,
  so its compiler no longer depends on the version in Nixery's nixpkgs
  snapshot. This is the minimum accepted by `dune-project` and the current
  `httpz` packages.
- **The default transport has no HTTP/2 or connection reuse.** `fetch-httpz` is
  HTTP/1.1 and opens one connection per request. A long-running client can pass
  `Fetch_curl.std` through the existing `~fetch` seam for pooling and HTTP/2;
  `README.md` records the optional pin/install recipe. `fetch-curl` is not yet
  in opam-repository or this project's CI/release validation.
- `dune-project` lists `eio_main` as a regular dependency, not `:with-test`,
  because the installed `omatrix` links it. Move it back if `omatrix` ever
  splits into its own opam package.
- **`dune build @doc` is warning-free.** Revalidated on 2026-09-04 with Dune
  3.24.2/odoc 3.2.1 after disambiguating the remaining value/type/section
  references. The previously recorded 228 unresolved-root warnings and proposed
  package documentation dependency stanza are not reproducible, so no
  project-file change is needed.

## HTTP layer

- **Policy denials are distinct from transport failures.** `Client.run` maps
  `Fetch.Denied` to non-retryable `Error.Policy_denied` before its general
  `Eio.Io` mapping; `matrix-chat.eio` preserves that distinction. The test `origin
  restriction / cross-origin redirect is denied` proves the off-origin hop is
  refused and never reaches the backend.
- **TLS failures have a typed result.** The httpz backend normalizes setup,
  certificate and mid-stream TLS exceptions to `Fetch.Tls_failure`;
  `Client.run` now maps that before general `Eio.Io` to non-retryable
  `Error.Tls_error`, which `matrix-chat.eio` preserves as `Error.Tls`.
- **HTTPz owns the Eio transport defaults.** `matrix-chat.eio`, OAuth and `omatrix`
  construct `Fetch_httpz.std` directly; it supplies lazy system-trust TLS and
  RNG setup. A custom authenticator is expressed with
  `Fetch_httpz.std ~https:(Httpz_tls.client ~authenticator)`. The former
  `Matrix_eio.Http` wrapper and its process-global RNG setup are removed.
- **A request can have one monotonic whole-operation deadline.**
  `Client.with_request_timeout` bounds the transport, response-body or stream
  callback, retry backoff and time spent awaiting automatic refresh/replay
  without resetting between attempts. `Matrix_eio.Client.create` exposes it as
  `?request_timeout`; omission preserves the unlimited behavior. Deadline
  expiry is a `Network_error`, while parent cancellation still propagates.
- **The Eio HTTP policy is configurable and exposed by the CLI.**
  `Fetch_httpz.std` accepts optional retry, per-origin concurrency,
  minimum-interval, cookie and connect/idle-timeout controls; `omatrix`
  accepts `--retries`, `--rate-limit`, `--max-concurrent`,
  `--connect-timeout` and `--idle-timeout` on every network command, including
  off-origin browser/device OAuth work. Callers can still supply a custom
  `Matrix_client` fetch. The installed Matrix policy admits only the read-like
  `/keys/query` POST in addition to Fetch's ordinary idempotent methods.
- **The global “do not query well-known” privacy switch is implemented.** The
  typed `Client.well_known_policy` configuration is inherited by derived
  clients; `Server.get_well_known`, `Server.discover` and OAuth metadata
  fallback honour `Do_not_query`. Exact `fetch.mock` request logs cover disabled
  and default behavior.
- **POST retry policy is split between Matrix and the transport.** HTTPz
  `5c9ed375` provides `retry_request`, an outer request-level veto applied after
  the retry budget, replayable-body and allowed-method checks and before every
  response-status, connection-failure or custom retry classifier.
  `Matrix_client.Http_retry` owns the route policy: Fetch's normal
  GET/PUT/DELETE/HEAD/OPTIONS behavior is retained, and only canonical
  replayable `POST .../_matrix/client/v3/keys/query` is additionally admitted.
  The endpoint is appended to the configured homeserver and matched by exact
  canonical origin and decoded path, retaining a deployment prefix while
  rejecting nearby, extended and encoded-separator paths. `/keys/claim` can
  consume another one-time key, `/keys/upload` changes state, and sync, sliding
  sync and all other Matrix POSTs remain vetoed. The Eio and CLI defaults
  install this policy; a caller-supplied fetch has exactly the policy its caller
  installed.
  `Send_queue` separately understands Matrix JSON `retry_after_ms` for its
  transaction-idempotent send, while HTTPz continues to own generic HTTP
  `Retry-After` handling.
- **Every `matrix-chat.eio` wrapper is written out by hand.** The 40 wrapper
  modules are mechanical — `Error.unwrap (Matrix_client.X.f (Client.base c) …)`
  around each value, and an `.mli` restating the signature. A functor or a
  single `Eio_of_result` adapter would remove the file-per-module boilerplate,
  at the cost of a public surface odoc cannot flatten.

## Auth / OAuth

- **MSC4108 QR login is implemented; external live validation is P2.3.** The
  exact MSC4108 creation endpoint is distinct from MSC4388 discovery. Strict
  codecs, conditional rendezvous, vodozemac-compatible ECIES, OAuth/login
  approval and authentication messages, secret handover, and the typed
  two-party Eio session are implemented. Profile preflight, mandatory
  check-code, progress, cancellation and timeout are covered; the authenticated
  OAuth session is persisted immediately before encryption state and
  subsequent network work. Both `omatrix qr grant` and `omatrix qr login` are implemented, with
  the new-device flow generating textual Base64 MSC4108. Private seeds and
  the optional backup key are published to fresh encrypted SSSS protected by a
  passphrase file or one-time printed Base58 recovery key. Rust FFI returns raw
  QR bytes, so raster rendering/scanning is application-owned rather than an
  SDK parity gap. MSC4388 remains a separate typed unsupported channel.
  The isolated MSC4108 fixture validates the rendezvous and encrypted-channel
  layers; only the full external OAuth/MAS application roles remain under
  P2.3.
- **The RFC 8628 device-authorisation grant is implemented.**
  `omatrix login --device-code` prints the user/device codes and drives the
  bounded monotonic poll loop; `Oauth.Device_authorization` classifies pending,
  slow-down, denial and expiry responses. Authenticated clients now perform
  opt-in reactive refresh on `M_UNKNOWN_TOKEN`, serializing a single refresh
  per exact failed token and replaying buffered and GET-stream requests once.
  One-shot POST streams and unauthenticated requests are not replayed. OAuth
  refresh fetches fresh metadata; Matrix and OAuth refresh tokens are retained,
  the password-login CLI requests one explicitly, and `omatrix` persists
  rotations through a locked re-read/update/atomic-rename transaction. The
  expiry-aware client variant refreshes at or before an injected-clock early
  window and carries the replacement expiry to its persistence hook.
- **Proactive token refresh is implemented at the client boundary.** It shares
  the reactive exact-token lock, stops a request after refresh failure but lets
  a later request retry, skips sessions without a refresh token, and never
  duplicates a one-shot streaming send. Password, token and OAuth login retain
  the issuing endpoint's lifetime as an absolute deadline in the versioned
  session profile, and `omatrix` installs the expiry-aware variant on reload.
  Older profiles decode the new field as absent.
- **Refresh completion is detached by the Eio facade.** The refresh operation
  is owned by the client switch rather than the initiating request, so caller
  cancellation cannot abandon a successful rotation. OAuth `invalid_grant`
  produces a typed, once-per-refresh session-invalid notification; callback
  failures, including cancellation in detached best-effort persistence, do not
  roll back the committed token or fail the client switch. Stampede protection
  for the network request remains per `Client.t`, but `Profile_store.with_lock`
  and `update_session` serialize session-file read/modify/write across handles
  and processes. Base and crypto snapshots share that lock: base writers compare
  the exact loaded-byte fingerprint, while crypto writers compare a fail-closed
  generation marker. Refresh completion clears/resolves its shared promise
  before invoking persistence callbacks, so a callback may re-enter the same
  authenticated client without deadlocking.
- **OAuth metadata has a best-effort per-client cache.** `Metadata.fetch`
  refreshes discovery and records successful stable, unstable or well-known
  results. `Metadata.fetch_cached` reuses them for the authoritative response
  `Cache-Control: max-age` (capped at 24 hours), then a valid `Expires`, with a
  24-hour fallback when neither supplies a lifetime; `no-cache` revalidates on
  every lookup and `no-store` is never retained. Malformed/negative max-age
  values fail safe to immediate revalidation. Age is measured on an injectable
  monotonic clock, so later wall-clock changes cannot extend or prematurely
  expire an entry. After expiry it refreshes synchronously and returns the
  still-valid stale entry if that refresh fails. Rust refreshes stale metadata
  in a spawned task, but this pure result API has no scheduler to own one.
  Explicit invalidation is scoped to OAuth metadata, credential-derived clients
  start empty, and failures are never cached.
- **OAuth metadata URL validation is strict by default.**
  `Oauth.Metadata.validate` rejects an issuer with a query or fragment and any
  non-`https` issuer or endpoint. `allow_insecure=true` is the explicit escape
  hatch for a local `http` deployment; production flows, including
  `login_with_browser`, retain the secure default.
- **OAuth browser registration uses the exact bound redirect URI.**
  `default_client_metadata` remains a portable port-less template, while
  `login_with_browser` specializes dynamic registration to the ephemeral URI
  already bound by that flow. Caller-supplied metadata is left unchanged; the
  authorization request and token exchange use the same exact URI. The mock
  regression covers strict servers; a real OAuth issuer is outside this
  pinned parity gate.
- **Stable and deployed OAuth dialects are explicit.** The public scope helpers
  expose both the stable tokens and matrix-rust-sdk's MSC2967 spellings;
  authorization defaults to a complete dialect advertised by
  `scopes_supported` (stable first) and keeps the stable default when metadata
  is absent or incomplete. Device-scope parsing rejects mixed/ambiguous tokens.
  Account management recognises MSC4191's session/deactivation aliases and
  emits the spelling the server advertised, again preferring stable. There is
  deliberately no blind retry after an authorization redirect: callers of a
  server which omits `scopes_supported` can pass the unstable scope explicitly.
- **`Registration.client_metadata` covers the Matrix profile plus the bounded
  RFC 7591 extensions used by clients**: sector identifiers, software identity
  and statements, and mutually-exclusive structured `jwks`/`jwks_uri` are now
  encoded and validated. Less common OpenID Connect registration extensions
  remain intentionally outside this small metadata record.
- **OAuth logout is routed, but old session files are ambiguous.**
  `Session.Auth` records `Matrix` versus `OAuth { client_id }`; `omatrix logout`
  now calls Matrix `/logout` for the former and OAuth metadata validation plus
  access/refresh-token revocation for the latter. Session files without the
  optional provenance field decode as the legacy Matrix flow, so an older
  OAuth profile must log in again to gain reliable provenance. A real OAuth
  authorisation server is outside this pinned parity gate; exact mock routing
  and revocation coverage has landed.

## CS API gaps

- **Peeking is implemented as a legacy/low-level family.**
  `Matrix_client.Peeking` covers `/events` and `initialSync` with matching Eio
  wrappers; the current Dendrite smoke profile explicitly reports
  `M_GUEST_ACCESS_FORBIDDEN`, so this remains a server portability limitation,
  not an SDK implementation gap. The MSC4108 rendezvous transport and its
  isolated Synapse fixture are implemented; full OAuth/MAS login validation is
  tracked separately as P2.3.
  `Admin.whois` implements the typed Client-Server `/admin/whois` query; it
  requires server-admin authorization and does not claim any broader Synapse
  admin API.
- **`Dehydrated_device` implements the MSC3814 continuity boundary.** Support
  probing, strict interoperable `org.matrix.msc3814.v1.olm` account pickle,
  independent signed device/OTK/fallback upload, rehydrate/drain/delete,
  lifecycle observation and cached-key weekly rotation are implemented and
  persisted. The Eio manager's focused suite covers restart-safe key behavior,
  callback ordering, failures and cancellation; the Synapse scenario covers
  capability, upload/get/delete, empty events and empty-queue rehydration.
- **Delayed events support both MSC4140 generations.** `Delayed_events` keeps
  the deprecated Synapse-compatible query-parameter shape as the legacy API and
  also exposes current `delayed_event` body/path scheduling, direct retrieval,
  and `.../{delayId}/{action}` updates. The APIs are explicit; no mutation is
  replayed or guessed from `/versions`.
- **Unstable MSC3814 paths are direct.** Authenticated absolute-path JSON
  helpers now sit beside `Client.Http`'s v3-relative helpers, and
  `Dehydrated_device` uses the exact
  `/_matrix/client/unstable/org.matrix.msc3814.v1/dehydrated_device` route.
  Request tests pin method, authorization, query, body and the absence of any
  `/..` canonicalisation dependency. Keep new non-v3 endpoints on these named
  helpers rather than smuggling dot-segments through a relative route.
- **Bounded unstable-prefix fallback is implemented for the documented set.**
  `Relations.list_threads` selects Matrix 1.4 `/v1` or MSC3856;
  `Report.room`/`user` select Matrix 1.13/1.14 or MSC4151/MSC4260;
  `Auth.get_login_token` selects Matrix 1.7 or MSC3882; and profile custom
  fields select Matrix 1.16 (or `uk.tcpip.msc4133.stable`) or MSC4133.
  Each operation reads `/versions` before its request, keeps stable as the
  default whenever advertised, and never retries a mutation after an error.
  Display names, avatar URLs and stable/MSC4133 custom fields now each expose
  an explicit clear operation that emits JSON `null`, matching Rust's
  `Option::None` setters without overloading an empty string. The Eio facade
  and encoder/request regressions cover all three. The exact unstable names
  and paths are pinned to ruma at the audit revision.
- **`Report.event` emits the current reason-only body.** Matrix 1.18 removed
  `score`, so the deprecated optional OCaml argument is retained only for
  source compatibility and ignored on the wire. The pinned Rust SDK likewise
  assigns only `reason` to the current ruma request.
- **Off-origin well-known discovery is implemented without widening a client.**
  `Server.discover` accepts a versions-fetch callback for delegated origins;
  without one the pure API retains the compatible `server_versions = None`
  result. `Matrix_eio.Server.discover` supplies a fresh origin-restricted,
  unauthenticated client using the caller's transport policy and randomness.
  Well-known and delegated `/versions` probes never carry a bearer token.
- **Capability helpers are read-only policy queries.** `Server.capabilities`
  retains `m.profile_fields`, `m.forget_forced_upon_leave` and
  `m.account_moderation` in `custom` as raw JSON. Pure and Eio helpers now
  expose profile, password, 3PID, login-token, room-version, moderation and
  forced-forget policy using Ruma's exact absent defaults, while malformed
  advertised values fail instead of silently defaulting. Successful
  `/versions` and `/capabilities` responses are cached per client with explicit
  invalidation. Mutation endpoints do not automatically preflight these
  advisory values. Centralised stable/unstable route selection and HTTP cache
  expiry remain separate design work.
- **`Search.criteria.filter` is typed.** It reuses
  `Sync.Filter.room_event` and its `room_event_jsont`, the same
  `RoomEventFilter` shape as a `/sync` room timeline. This older audit item was
  closed in the first parity pass; keep the shared codec rather than letting
  search and sync drift apart.
- **Nullable response members need explicit codecs.**
  `Rooms.get_joined_members` failed against a live server with
  `Expected string but found null` because Synapse answers
  `/joined_members` with `{"avatar_url": null, "display_name": …}` rather than
  omitting the member. Fixed there by decoding through
  `Jsont.option Jsont.string` with `~dec_absent:None`. The response codecs for
  device metadata, profile names, presence status, notification tags/tokens,
  search profile/context/pagination strings, and directory room-summary
  text/alias/type/version/encryption and pagination strings now accept both
  omitted and explicit-null values. Request-only and persisted-store codecs
  remain strict. The focused `test_nullable_responses` executable pins
  representative null replies. New response codecs should use the same form;
  a shared `nullable_string` combinator in `matrix-chat.proto` remains optional
  cleanup rather than a reason to loosen request bodies.
- **Registration is user-interactive on every real server.** `Auth.register`
  retains its one-stage `?auth` compatibility API, while
  `Auth.register_uiaa` now drives the bounded `Uiaa.with_uiaa` flow: it retains
  the complete registration body on the one callback-produced retry and
  returns success, a parseable outstanding challenge, or another error. The
  integration harness uses it for Synapse's initial
  `enable_registration_without_verification` dummy challenge. It intentionally
  does not guess unsupported or multi-stage credentials; those remain
  `Uiaa_auth_required` for the caller.
- **Room result containers require their schema members.** Successful
  `joined_rooms`, `members`, `joined_members` and `aliases` responses missing
  the CS API's required collection are now `Json_error`s, rather than being
  mistaken for empty account/room state. Request builders retain defaults for
  optional arrays; response arrays should only use `~dec_absent:[]` where the
  endpoint schema explicitly supplies that default.
- **URL escaping is `Uri`'s `` `Path `` default**, so `!` and `:` stay
  unescaped (`/join/%23lobby:example.org`). Correct per RFC 3986, and the spec's
  examples show fully escaped forms, so a strict router could still disagree —
  but **Synapse 1.159.0 accepts it**: `scenario_rooms.ml`'s "a room id in the
  path" asserts the `!` and the `:` reach the path unescaped and that the
  `/rooms/{id}/state` and `/rooms/{id}/state/{type}/` reads then succeed.
  `/joined_members` goes the other way — it is the one path in `rooms.ml` built
  with `Uri.pct_encode` — and Synapse accepts that too, so both forms work
  against Synapse and only a different server can settle which is right.

## Sync and base client

- **Local unread counts reconcile from a complete cached suffix.** `Runtime`
  recomputes room-wide counts from the shared `Event_cache` whenever the cache
  has a physically complete suffix through the read horizon, and atomically
  applies the result only if the projected sync/cache state is unchanged. With
  partial windows or gaps after the read horizon, `Base_client` retains its
  per-sync accumulation until backfill makes a complete recount possible; that
  limitation is deliberate rather than a claim of exact whole-history counts.
- **Push rules hydrate from account data.** A valid `m.push_rules` event becomes
  active before notification/highlight evaluation for the same sync response,
  and the stored event restores that ruleset after restart. Malformed events
  preserve the active/default rules. A brand-new account which has not yet
  received account data may still bootstrap with `Push.get_push_rules` plus
  `Base_client.with_ruleset`; the pure fold deliberately performs no request.
- **Base-client hero names are deterministic but summary-only.** The durable
  `Base_client` fold sorts hero display names, applies the room/member-count
  fallback and uses user IDs when a hero has no display name. It does not fetch
  authoritative `/members` data or disambiguate duplicate names itself. The
  newer `Room_details` service does those opt-in operations, including one
  authoritative lazy-member fetch when its cache is incomplete.
- **Own display names are room-specific for push evaluation.** `Base_client`
  now takes the current own `m.room.member` display name from each room's
  durable state projection, including a state event and mention arriving in
  the same sync. An explicit member without `displayname` uses the user-id
  localpart, as Rust does; `create ~display_name` remains the compatibility
  fallback until own membership state is known. Restart and fallback behavior
  are covered by the base-client tests.
- **Threaded receipts are retained.** `Read_state` keeps server and local read
  positions per thread, sends receipts with the matching thread scope, and
  computes unread counts independently. `ThreadInfo` persists the projection
  used by the rich thread list, and the durable per-thread cache is complete as
  a bounded shared `EventCache`/`EventStore` projection unifying the shared
  topology, receipt/backfill, pagination and invalidation paths under P1.2b.
  It is not an independent persisted Rust `LinkedChunk`/lazy-loading
  implementation; this thread-specific architecture does not describe the
  general room cache, whose persisted prefix now loads one chunk at a time.
- **The old `Matrix_client.Timeline` remains deliberately minimal.** New UI
  code should use `Matrix_ui.Runtime`: its shared event cache is driven by
  `Matrix_eio.Sync_service`, persists through memory or `matrix-chat.ui.sqlite`,
  marks limited-sync gaps,
  paginates backwards, and aggregates edits/reactions/redactions. A separate
  event-focused room paginator walks both directions, and the notification
  client/settings services are present. Raw thread-root pagination, reactive
  server-event search, deterministic space graphs and room details are also
  available as client services. Remaining `matrix-sdk-ui` gaps are cache-native
  search/space views and encrypted SQLite storage; thread-cache lifecycle and
  invalidation are complete. Typing deliberately remains a separate room
  observable, matching Rust's subscription model rather than a timeline item.
- **`Send_queue` stages and substitutes attachment events.** Its convenience
  API persists the complete original/optional-thumbnail/event graph in one
  transition. Typed clear/encrypted results survive restart and become exactly
  one of `url`/`file` and `info.thumbnail_url`/`info.thumbnail_file` before the
  event send or encrypted-room wrapping; generated members beat extras and
  malformed dependencies wedge without sending placeholders. Upload nodes stay
  out of the event cache, while the event has one base-content local echo.
  Streaming progress, cancellation and the in-flight compensating-redaction
  rules apply to the graph. Caption edits now persist in place before send or
  become one retry-stable replacement when they race the event, with
  exactly-once echo migration. Local media is now cached before queue
  persistence, exposed by local URI, moved idempotently to its remote MXC key,
  and cleaned on cancellation or orphan reconciliation in memory and SQLite.
- **Profile snapshots coordinate and recover across processes.** Session,
  base and crypto stores share one persistent advisory lock. Base snapshots
  reject stale byte fingerprints and crypto snapshots reject stale generations.
  A complete private redo journal precedes crypto component changes; load
  replays it after an interrupted save or clear. Native directory sync orders
  the journal, components and commit marker. Legacy odd markers without a
  journal still need explicit recovery. Profile token refresh has a separate
  cancellable lock and uncertainty marker; `omatrix` uses it by default.
- **Persisted latest-event plaintext is an explicit policy.** The default
  `Ciphertext_only` store keeps an encrypted latest event encrypted across
  restart. A `Store_plaintext` base state installs this batch's decrypted event
  as `room_info.latest_event` and persists it, enabling decrypted room previews
  after restart only when the caller accepts that at-rest disclosure.
- **Deliberate deviations from matrix-rust-sdk**, recorded so they are not
  mistaken for bugs: `filter_timeline_event` accepts `m.room.encrypted` where
  Rust rejects it; the `"and N others"` count is `joined + invited - heroes`,
  matching `compute_display_name_from_heroes` rather than the spec's wording;
  a canonical alias names a room with the full `#room:server`, where Rust uses
  the localpart.

## UI models (`matrix-chat.ui`)

- **A trim that cannot describe its cut leaves the room over budget.** The
  event cache holds a room as an ordered list of chunks — a run of events,
  or a `Gap` carrying the token that fills it — as matrix-rust-sdk's
  `LinkedChunk` does, and `trim` drops the oldest events chunk and puts a
  gap in its place carrying the *following* chunk's left-edge token, so
  filling that gap re-fetches exactly what was cut. When neither the
  following chunk nor the cut one carries a token — which happens where two
  events chunks were joined by a pagination rather than by a sync window —
  the cut cannot be described, and `trim` stops rather than claim a history
  it cannot get back. A room whose whole history came in through
  `paginate_back` can therefore sit above `max_events_per_room`.
  The oldest chunk's own `prev_token` points before that chunk's first event,
  not at a proposed interior cut, so it cannot safely represent a gap before a
  retained suffix.
  Back-pagination does not trim at all, deliberately: a page the user asked
  for is not thrown away by the budget.
- **A limited window that overlaps is merged, where matrix-rust-sdk opens a
  gap.** `handle_sync` in `crates/matrix-sdk/src/event_cache/caches/room/`
  drops the `prev_batch` token only when the sync is not limited or every
  event was a duplicate; a *partial* overlap there removes the copies it
  held and pushes gap-then-window at the back. Here an overlapping window
  merges in place, because the case that matters is the cold `/sync` of a
  process that reloaded its cache from `Event_store`: rust's treatment would
  move the reloaded tail. A window with nothing new in it opens no gap
  either. This differs from Rust's `non_empty_all_duplicates` condition: Rust
  retains the gap when all duplicates were sent by the own user, because they
  may only overlap through send-queue echoes. `Event_cache` does not currently
  own the user identity needed to make that distinction; adding it, passing it
  from `Runtime`, and pinning own/other/mixed sender cases remains TODO.
- **Persisted room chunks now load lazily.** `Event_store.Lazy_S`/`v_lazy` is an
  opt-in extension of the compatible eager `S` path. Its initial read returns
  validated layout metadata, detached events and the newest events chunk;
  `Event_cache` represents older events as metadata-only `Unloaded_events`,
  distinct from server `Gap` chunks, and exposes only the resident tail.
  `Back_pagination` and `Room_timeline` hydrate one predecessor chunk before
  network I/O. Stable event and transaction identities suppress duplicates
  across the unloaded prefix, and queued echoes found there move to the live
  tail without decoding intervening history. Missing, mismatched, empty or
  duplicate chunk data produces a typed codec error and leaves the topology
  retryable. Incremental `Layout`, `Put_event`, `Delete_event` and
  `Replace_events_chunk` updates preserve unloaded rows; failed deltas are
  replayed before a flush. SQLite performs metadata and chunk reads on its Eio
  systhread boundary, while the memory backend implements the same contract.
  The 21-case `test_lazy_event_store` suite covers cold tails, one-chunk growth,
  local-before-network pagination, gaps, hidden overlap/echoes, retryable store
  failures, plaintext policy, invalid writes, forget and deferred malformed
  SQLite rows.

  The in-memory spine is still an OCaml list: splices rebuild it, which is fine
  at chunk granularity (`chunk_capacity` defaults to 128) but is not Rust's O(1)
  linked-chunk insertion. Cancellation and concurrent-hydration races still
  need explicit regressions. The tokenless interior-trim limitation above also
  remains.
- **Presentation and relation contributions are memoised.**
  `Room_timeline.refresh` keeps one
  projection per `stable_id`, valid while the cache holds the very same
  record, so the `markup` sanitiser runs once per event rather than once per
  change; the edit applied over an event is memoised beside it. A private index
  retains each reaction/edit/redaction contribution by stable ID and target,
  updates its memo and physical position, and removes stale contributions after
  replacement, trim or forget. Refresh still walks the bounded cache snapshot
  to synchronize that index and project visible items, but it no longer
  rebuilds global relation tables. `reconcile_by` keeps granular diffs for
  ordinary changes and now falls back to one `Reset` after a private diff
  threshold, bounding its pathological reorder case. Focused tests cover
  incremental addition,
  redaction, sender deduplication/order, equal-timestamp edit ordering,
  backfill, forget and target reinsertion without stale state.
- **Typing notices deliberately remain outside `Room_timeline`.** The runtime
  exposes a per-room `typing_users` observable with strict user-ID validation,
  self filtering, room isolation, empty-list clearing and forget cleanup, so a
  UI can render the latest typing state without parsing sync JSON. This matches
  matrix-rust-sdk's separate room typing subscription: typing is not one of its
  timeline virtual items. Rust's `DateDivider`, `ReadMarker` and
  `TimelineStart` virtual items are all represented here as `Date_divider`,
  `Read_marker` and `Timeline_start`; OCaml additionally exposes each cache
  `Gap` at its own position. The read
  marker follows rust's `update_read_marker`: after the `m.fully_read`
  event, past the run of our own events that follows it, and not shown at
  all when nothing is read, when the marked event is not among the items, or
  when the position is the timeline's end. It falls back to the latest of
  our own read receipts where the account has never set `m.fully_read`,
  which rust does too. The timeline now also exposes Rust-controller-style
  `send_single_receipt`, `send_multiple_receipts` and `mark_as_read` controls.
  They use explicit main/thread scope, redirect ordinary receipts away from
  our own event while allowing `mark_as_read` to force server badge
  recomputation, target the latest raw receipt-capable event even when a
  relation is folded out of the visible projection, prevent local regression,
  keep private read state at least as new as public state and atomically batch
  unthreaded fully-read/public/private markers through `/read_markers`.
  Successful sends update the local receipt view immediately. Membership
  changes are items of their own and are not
  grouped ("and 4 others joined"), which Element does in its own layer
  rather than in `matrix-sdk-ui`. `Room_list` sections are computed from
  tags and `m.direct` only; there is no space-aware filtering. Rich persisted
  thread lists and cache-native search/space views are separate from the room
  timeline: `ThreadInfo`/`Thread_list`, pinned events and room-context
  `Event_focused` are available, as are thread-focused `/relations`, receipt
  backfill, prioritized pagination and invalidation, all using the durable
  per-thread cache. Server-event search and a deterministic space graph are
  separate client services. The one-event
  notification client is likewise separate from `Runtime`.
- **Live forward pagination is deliberately sync-driven.** Rust's
  `Timeline::paginate_forwards` is a no-op for a live-focus timeline because
  sync owns its live edge. OCaml follows the same split: `Room_timeline` fills
  history gaps backwards, while `Messages.Forward` and
  `Event_focused.paginate_forward` walk the forward token for event-focused
  room views, including deduplication, threads and end-of-history handling.
  There is no unfinished live-forward driver to add.
- **Installing an `mxc://` resolver is deliberately caller-owned.**
  `Presentation.Html.sanitize` now keeps `<img>` with `alt`, `title`, `width`, `height`
  and an `mxc://` `src` — any other scheme, or a missing `src`, drops the
  element whole — and takes `?resolve_mxc` so a toolkit can rewrite the `src`,
  which `Presentation.of_event ?resolve_mxc` threads through. Nothing in this
  tree passes one automatically. `Media.mxc_to_http_resolved` now queries the
  client capability cache and chooses authenticated v1 or legacy v3 media URLs;
  an Eio renderer can explicitly adapt that result through its own `Client.t`.
  Rust timeline items likewise retain a media source for caller-driven fetch
  rather than silently rewriting presentation HTML. Keeping projection pure
  avoids hidden capability probes and preserves offline behavior; an unresolved
  `mxc://` stays in the HTML where no browser can load it.
- **The sanitiser emits HTML void elements correctly.** Sanitized element
  signals use `Markup.Ns.html`, so `markup`'s writer emits `<br>`, `<hr>` and
  `<img ...>` without closing tags; exact regressions cover all three.
- **Preview selection intentionally understands decrypted events.** Unlike
  Rust's wire-only predicate, `Presentation.is_preview_worthy` judges an
  encrypted event by available plaintext. It also handles the own user's
  join/invite/knock transitions, actionable third-party knocks using room power
  levels, and `org.matrix.msc3672.beacon_info`; `Room_list` supplies the own
  user and room authorization context. This is a documented product-level
  deviation, not an unfinished parity item.
- **Timeline event-item filtering is configurable.** `Presentation` classifies
  `m.room.member` exactly as ruma's `membership_change` does and models the
  state types matrix-rust-sdk's `AnyOtherStateEventContentChange` models, so
  `Presentation.body` reads "@alice:localhost joined" rather than
  "m.room.member". `Room_timeline.default_event_filter` preserves the existing
  narrower default (meaningful membership/profile changes and selected room
  metadata), while `Room_timeline.create ~event_filter` and
  `Runtime.timeline ~event_filter` let a toolkit include creation, power-level,
  space and other state events. The custom predicate replaces the event-item
  choice but virtual items remain projected, relation events are still
  aggregated, and an undecryptable ciphertext remains visible for retry; use
  `default_event_filter` when adding state types without making edits or
  reactions standalone items.
- **`Runtime.sync_state` now distinguishes `Offline`.** A failed sync is
  published as `Failed`, then `Offline` while `?on_error` elects to retry; a
  successful request returns to `Live`, and a terminal error or
  `Runtime.stop` publishes `Stopped`. This matches the useful state distinction
  in rust-sdk, but does not add a separate `/versions` offline supervisor: the
  next sync request is still the liveness probe, and callers choose the retry
  delay through `on_error`.
- **The fuzzy scorer is fzf's weights, not Skim's numbers.**
  `Matching.fuzzy_score` and `Room_list.Filter.Fuzzy` are matrix-rust-sdk's
  `fuzzy_match_room_name`, and `Room_list.score` exposes the score so a UI can
  rank — which rust-sdk cannot, its filters being booleans. The dynamic program
  is fzf's "v2", which `SkimMatcherV2` is built on, with fzf's published
  weights; it is not a port of the `fuzzy-matcher` crate, so the numbers differ
  from Skim's and only the ordering they induce is meant to match. There is no
  camel-case bonus, because `search_key` folds case before the scorer runs, and
  no cache: `SkimMatcherV2::use_cache(true)` memoizes across calls where each
  `Filter.matches` here rescores from scratch, which is O(needle × name) per
  room per refilter.
- **`Matching.search_key` is more permissive than rust-sdk's
  `normalize_string`,
  deliberately.** rust-sdk lowercases and decomposes to NFD before dropping
  combining marks; we case-fold and decompose to NFKD. So `"STRASSE"` matches
  `"Straße"` here and not there (folding maps the eszett onto `"ss"`), and a
  fullwidth or ligature spelling keys onto the plain one here and not there.
  Both widen the match, which is the safe direction for a search box, but a
  client that wants byte-for-byte parity with Element would need the narrower
  key.
- **Space filtering is intentionally not graph traversal.** rust-sdk's `space`
  filter matches a room whose cached `m.room.create` state has `room_type`
  `m.space`; it does not walk `m.space.child`/`m.space.parent` relationships.
  The room-list projection now derives this boolean from the durable create
  event. Its separate `deduplicate_versions` filter follows Rust's membership
  rule exactly: a joined old room is hidden when its known successor is joined
  or left, but remains beside an invited or knocked successor; a non-joined old
  room is hidden for any known successor. Missing or malformed tombstones are
  conservative, and nested filter combinators retain the same context.
- **`Room_list.Filter.Search` widens the name match onto the room id**, which
  rust-sdk does not do; its `identifiers` filter takes an explicit list of ids,
  which is `Filter.Room_ids`. Keep both in mind when comparing behaviour with
  Element.
- **Sections are ours, not rust-sdk's.** `Room_list.section` is a single label
  per room, computed from membership, then tags, then `m.direct`, so a
  favourite DM is filed under `Favourites` and not `People`, and a knocked room
  falls into `Historical` for want of a section of its own. An Element-style
  list builds its sections by running one filter per section instead, which
  `Filter.t` can now express; `section` and `In_section` exist for the simple
  case and sort first under both `Activity` and `Name`.
- **Preview edit resolution checks the encryption clauses.**
  `Room_list.latest` now collects the newest `m.replace` of every event as it
  scans back and substitutes it for the event it replaces, as `builder.rs`
  does. `Presentation.is_valid_replacement_with_encryption` combines the base
  sender/type/state/relation checks with the `Event_cache.event.clear_event`
  provenance retained by the scan: an encrypted original requires an encrypted
  replacement, and a decrypted encrypted replacement requires
  `m.new_content`. The table is per-scan and unbounded, where rust-sdk's is too,
  so a room whose recent window is nothing but edits builds one entry per
  edited event.
- **`Runtime.join` and `Runtime.leave` do not touch the room models.** Both are
  the `Matrix_client.Rooms` endpoint under the runtime's client, returning
  the `Error.t` rather than raising it, and neither writes the new
  membership into the sync state: `Room_list.find` reports the old one until
  the sync that carries the change arrives. That is one round of the loop
  and not a wait a caller has to arrange, but a UI that wants the invite to
  leave its list the instant it is accepted has to hide it itself.
  `Base_client.state` is what would have to take an amendment, and a
  guessed membership a failed or racing sync then contradicted would be
  worse than a list one response behind. If an encrypted invite has a
  discoverable inviter, `Runtime.join` separately records and persists the
  MSC4268 acceptance gate after a successful HTTP join. `Runtime.join_room`
  resolves aliases before capturing that inviter, forwards the caller's `via`
  list unchanged and records against the returned room ID. Bundle transport and
  import happen later in the sync-driven accepted-join workflow, not inside the
  HTTP join call itself.
- **`Runtime.forget` owns complete local cleanup.** It first requires a
  successful homeserver `/forget`; on failure, local state is untouched. On
  success it invalidates responses fetched before the operation, then removes
  base/store room and receipt state, cached events and SQLite chunks, open
  timelines, thread-subscription rows, queue requests and queue-owned local
  media before refreshing room-list and identity observables. Existing cache
  handles stay empty rather than reloading stale rows. In-flight sends finish
  detached with no compensating redaction, and stale sync or callback
  completion cannot recreate the room. If the cached room was direct, a fresh
  `m.direct` GET/PUT is attempted first; failure is logged and leaves the local
  account-data association available for a later retry, while the room itself
  is still forgotten. This is completed behavior, recorded here so new
  room-scoped stores join the same lifecycle rather than becoming leaks.
- **A restarted client's idea of "what is new" is bounded by its cache.**
  `Event_cache.position` gives the index of an event in a room's synced
  order, so a caller persists the id of the last event it handled and, after
  a restart, takes the cached events after it. Without an `Event_store` a
  fresh process starts empty and the first `/sync` window is all it has, so
  an event older than that window is `None` and cannot be told apart from
  one the cache never saw; the caller has to paginate or keep a store to
  close the difference. Nothing marks which items a `Timeline` produced
  after the model was created either, which is the same question asked of
  the projection rather than of the cache; matrix-rust-sdk has neither.

## Bots (`matrix-chat.bot`)

- **Bot commands can select a long-running or one-shot mode.** `Main.run_mode`
  wraps one `Cmdliner` term and one authenticated `Context.t` around either
  `Run_bot spec` or `Run_once (Context.t -> Cmd.Exit.code)`; the convenience
  `run` and `run_once` entry points use the same envelope. One-shots install no
  signal machinery, return their own status, and save crypto state under
  cancellation protection before context teardown. `matrix-bot --notify`
  uses this path, prints the event id on success and returns
  `Cmd.Exit.some_error` on a failed send. `Notify.spec` remains useful for
  callers already composing specs and for the live bot harness.
- **A bot room exposes send readiness.** `Room.ready_to_send` stays false until
  sync has authoritatively covered encryption state; an encrypted room also
  needs a complete non-empty member list and matching encryption-machine
  settings. `Room.sync_members` follows Rust's pre-send behavior by fetching
  `/members` only when the synchronized set is incomplete, replacing the
  normalized member state and persisting it across restart.
  `Room.await_ready_to_send` waits against the context's monotonic clock with a
  caller deadline, and `matrix-bot --notify` refreshes then waits before
  enqueueing. The low-level send methods remain non-blocking queue operations,
  so handlers that send immediately after a join should use the readiness API.
- **Back-filled history is delivered as ordinary events.** `Room.backfill`
  puts pages into the event cache and the room's collector hands them on, so
  a `Joined` handler that paginates is how a bot reads history — but nothing
  in the `Event.t` says an event is history rather than news, and the order
  is a room's newest events followed by its oldest. The cursor is safe (it
  only moves forward) and a logger does not care; a bot that answers would.

## Sliding sync (MSC4186)

- **The common sliding fold is the only public state path.** Account data,
  receipts, typing, to-device, rooms and profiles route through
  `Base_client`; profile subscriptions, presence wake/cancel and MSC4308
  stale-response guards are covered. Public standalone `Sliding_sync_state`
  aliases are gone. The private legacy-slot decoder and transactional
  conversion are complete; only decoder compatibility remains.
- **Capability discovery and adaptive selection are complete.**
  `Sliding_sync.native_feature` names the pinned Rust SDK's
  `org.matrix.simplified_msc3575` feature, and `is_available` probes `/versions`
  before selecting the native endpoint. `Adaptive_sync` automatically falls
  back to classic `/sync` when native sliding sync is unsupported; this is a
  project enhancement, not pinned Rust parity. `Sliding_sync.is_unsupported`
  recognises 404 / `M_UNRECOGNIZED` for that selector.
- **`include_heroes` is offered but is not an MSC4186 field.** Omitted from the
  JSON unless set; retained only for a legacy sliding-sync proxy. Delete it once
  no such server matters.
- **MSC4308 thread subscriptions and MSC4262 profiles are wired through the
  common fold.** Typed profile patches, explicit-null deletion and whole-user
  removal survive restart; profile subscriptions, capability probing, REST
  mutation, change catch-up and save-before-position ordering are complete.
  Remaining extension gaps are `sticky_events` (MSC4480) and persistence of
  unknown extension members; common projection, profile consumers and the
  MSC4308 stale-transaction fix are complete. The persisted reactive
  `Own_profile` observer is complete.
- **Extension payloads stay as raw JSON**: `to_device.events`,
  `account_data.global`/`.rooms`, `receipts.rooms` and `typing.rooms` decode to
  `Jsont.json`, not typed events.

## E2EE: Olm and Megolm

- **The MSC3814 account pickle is interoperable; session storage is separate.**
  `Olm_dehydrated_pickle` reads and writes the legacy libolm/vodozemac account
  pickle used by `org.matrix.msc3814.v1.olm`, with Rust fixtures and wrong-key
  and tamper coverage. `Session_pickle` still deliberately stores the
  project-local JSON crypto snapshot; older snapshots from before its rewrite
  may need re-establishment and are not MSC3814 payloads.
- **One-time-key publication matches the Rust pool split.** `Olm.Account` keeps
  a 50-key public target and up to 5,000 private keys for inbound handshakes;
  `Encryption` owns the published-ID set and persisted server uploaded count.
  It signs only the unpublished reserve, retries the exact pending upload batch,
  applies acknowledgements idempotently and validates classic/sliding response
  counts before using them. Keeping publication state above the primitive
  account is an internal layering choice, not a behavior gap.
- **Fallback rotation retains one previous key and has the Rust age policy.**
  `generate_fallback_key` moves the current key to a single previous slot;
  inbound pre-key lookup accepts either, only the current key is published,
  both survive account/profile persistence, and
  `forget_previous_fallback_key` drops the old one explicitly. A further
  rotation naturally discards the oldest key. `Encryption` waits until the
  server first reports fallback support, then persists creation time and the
  unpublished-key guard and rotates only when strictly older than one week.
  Omitted later reports, restart and clock rollback follow the pinned Rust
  behaviour. Failed upload batches are retried and low server counts generate
  replacement batches indefinitely; a finite retry cap is optional hardening,
  not pinned/current Rust parity.
- **Olm recency, retention and stale-session unwedge are implemented.**
  `Machine.get_olm_session` deterministically prefers the session that most
  recently decrypted. Successful encrypt/decrypt use drives a separate LRU,
  duplicate IDs are replaced, and at most four sessions per peer are retained;
  precise timestamps migrate and persist. After a failed decrypt, the newest
  session must be strictly older than one hour before a replacement key is
  force-claimed; clock rollback also permits repair. Success queues one exact
  encrypted `m.dummy` whose transaction/ciphertext persist until `mark_sent`,
  while an empty claim retains the wedge. Ordinary empty claims use persisted
  exponential retry suppression, while forced one-hour unwedge claims bypass
  it. Neither Rust nor OCaml applies general age expiry; the one-hour rule is
  only the stale-decrypt unwedge. General expiry is optional hardening, not
  pinned/current Rust parity.
- **No experimental Olm v2 or Megolm v2 algorithms.** The pinned Rust tree can
  opt into `m.olm.v2.curve25519-aes-sha2` and
  `m.megolm.v2.aes-sha2`; OCaml supports the deployed Olm/Megolm v1 algorithms
  only. This is explicitly deferred, not a blocker for the current core target.

## E2EE: key management and backup

- **Outbound `m.room_key.withheld` is durable.**
  `Encryption.encrypt_room_event` sends `m.no_olm` or `m.blacklisted` to the
  affected device and persists the exact transaction id, JSON body and sent
  state in the crypto snapshot. A dropped notice is retried unchanged across
  restart and an acknowledged notice remains suppressed. Megolm rotation
  removes obsolete room-session notices while retaining device-wide
  `m.no_olm` suppression.
- **Secret sharing over to-device is bounded and durable.**
  `m.secret.request` is handled as a plaintext to-device event and
  `m.secret.send` is handled over Olm, with replies
  restricted to another locally known, verified device of our own user. The
  latest request per secret cancels older requests, retries retain their
  transaction id and content, and matching sends are accepted only while
  the request is outstanding. Secret values, requests, cancellations and
  replies persist in the crypto snapshot. Cached encrypted reply bodies are
  stored exactly so an idempotent retry after restart does not advance Olm.
- **Trusted shared-history transport is a high-level workflow.** The sender
  restores missing backup keys, builds/uploads the MSC4268 bundle and shares its
  to-device reference. The receiver requires authenticated Olm plus the MSC4147
  signed device proof, persists latest-per-room/sender state, waits for the
  accepted join, freshly re-queries inviter trust, downloads/decrypts media and
  imports only same-room keys. Transient failures retry; malformed or missing
  media is discarded. Forwarding/shared-history/withholding provenance survives
  restart, and a live trusted join decrypts pre-join history.
- **Recovery has a complete lifecycle and cached state stream.**
  `Recovery` reads both markers and coordinates enable, recover, repair,
  key rotation, disable and delete-all with SSSS/cross-signing/backup state.
  Fresh-device recovery and live lifecycle tests pass. `Recovery.Manager` owns
  immediate reactive observation plus UIAA/password identity reset, conditional
  rebackup and device-key preupload. OAuth reauthentication/credential handoff
  remains separate; dehydration is complete under P1.3b, so do not duplicate
  that orchestration.
- **The forwarding chain is passed through, not extended.** When answering an
  `m.room_key_request`, the `forwarding_curve25519_key_chain` sent is the one the
  session arrived with. Matches matrix-rust-sdk, and the field is deprecated, but
  it is not what the spec describes.
- **One-time-key publication follows the Rust pool semantics.** A failed
  `/keys/upload` batch is retried; successful upload marks those identifiers
  published, while a low server count causes replacement batches to be
  generated indefinitely. An omitted claimed key uses the persisted 15-second
  to 15-minute backoff indefinitely; there is no finite retry cap in Rust or
  OCaml, and an individual successfully uploaded key is not re-offered.
- **`restore_from_backup` supports targeted restore.** Callers can request a
  room/session subset through `Room_keys.get_room_keys`; whole-backup restore
  uses the unpaginated `GET /room_keys/keys` in both Rust and OCaml. Paged
  whole-backup restore is optional scale hardening, not pinned/current Rust
  parity.
- **`backup_pending` is bounded and checkpointed.** It takes stable batches of
  at most 100 sessions, serializes concurrent calls on one encryption driver,
  and saves after each successful request, so a later error leaves only the
  remainder pending. An ETag/concurrent-version guard is optional OCaml
  hardening, not pinned/current Rust parity: Rust addresses backup versions,
  serializes operations, and disables on `NotFound`/`WrongRoomKeysVersion`.
- **`Secret_storage` implements only `m.secret_storage.v1.aes-hmac-sha2`**, and
  `key_of_passphrase` only 256-bit `m.pbkdf2`. Another algorithm is rejected,
  not ignored.
- **A `/keys/query` that names no devices is treated as an answer.** Against
  Synapse 1.159.0, `POST /keys/query` with
  `{"device_keys":{"@bob:localhost":[]}}` for a user who has never called
  `/keys/upload` — and for a user id that does not exist at all — is a 200 with
  `{"device_keys": {"@bob:localhost": {}}, "failures": {}, ...}`: an empty
  object, not an omission and not a failure entry. `receive_keys_query` then
  drops the user from `outdated`, adds them to `tracked` and holds zero devices
  for them, and `ensure_device_lists` re-queries only users that are untracked
  or outdated — so a send that raced the peer's first `/keys/upload` encrypts
  to nobody and says nothing. Recovery rests entirely on Synapse's
  `device_lists.changed` arriving once the peer does upload. This matches the
  pinned Rust SDK, which marks an explicitly returned empty device map up to
  date; retaining it as outdated would be an intentional robustness departure,
  not a parity fix. `scenario_e2ee.ml` waits for each peer's first sync
  response — which is what performs the `/keys/upload` — before creating the
  room, rather than relying on the recovery path.
- **A device with no one-time key left is withheld.** Synapse answers
  `/keys/claim` for an exhausted or unknown device with a
  200 `{"one_time_keys": {}, "failures": {}}`, so
  `Encryption.receive_keys_claim` creates no Olm session, the room-key share
  logs `No Olm session with <device>; not sharing the room key` on
  `matrix-chat.client`, and the message goes out anyway. A durable `m.no_olm`
  `m.room_key.withheld` notice now gives the device a reason and is deduplicated
  across restart. A fallback key covers the common case only if the peer
  uploaded one. Separately, failure to decrypt over an existing Olm session
  now follows Rust's strict one-hour unwedge rule (including clock rollback),
  force-claims a replacement and queues one persistent encrypted `m.dummy`;
  empty claims retain the repair request and acknowledgement clears the dummy.
  Omitted claims use the Rust/OCaml persisted 15-second-to-15-minute backoff
  indefinitely; a finite retry cap is optional hardening, not pinned/current
  Rust parity.
- **Local and remote secret storage are distinct.** `Crypto_store` writes
  base64 private keys to `crypto_state.json` at mode 0600 in a 0700 directory;
  those local keys are intentionally unencrypted at rest, with no passphrase
  or pickle-key wrapping as libolm has. QR/recovery publication stores private
  seeds in encrypted remote SSSS protected by a passphrase-derived or one-time
  recovery key. During credential-driven import the credential is application-
  supplied and decrypted material is memory-only; neither the credential nor
  the remote SSSS key is logged or persisted by the import path.
- **Derived identity changes remain machine-wide at the encryption layer.**
  Room-event decryption derives sender data from validated cross-signing/device
  chains, persists identity rotation and applies an explicit trust requirement.
  Local `set_device_trust` still controls the narrower key-gossip policy.
  `Matrix_ui.Room_identity` provides the room-scoped warning projection and
  refreshes it after sync. `Secrets.import_cross_signing` provides the
  credential-driven private-key loading path after a fresh own-user query.

## Verification

- **SAS publication still needs an application-supplied credential.** A
  `Secrets.store` opened with a passphrase or Base58 recovery key can be passed
  to `Verification_service.create`; it imports matching seeds atomically and
  successful SAS signs and uploads an own-user device or another user's master
  identity. Upload rejection suppresses trust. A service created without a
  private identity/store deliberately retains local-only `Verified` behaviour.
- **QR raster rendering/scanning is application-owned.** The Rust FFI hands raw
  QR bytes to callers; `Qr.encode` / `Qr.decode` likewise produce and consume
  the binary segment. Rendering and scanning are therefore caller/toolkit
  responsibilities, not missing SDK parity. `Verification_service` also leaves
  `m.qr_code.*` flows in the flow table for the caller — it drives SAS only.
- **In-room SAS verification is transported and live-tested.**
  `Verification_service.request_in_room` sends the initial
  `m.room.message` request, retains the server-assigned event ID as the flow
  ID, and sends every follow-up through `/rooms/{id}/send`. Encrypted rooms use
  `Encryption.send_encrypted`; callers may supply the complete membership set,
  with the local and peer users always retained as the bounded fallback.
  Classic and sliding sync route decrypted verification events in order after
  encryption processing and ignore this device's room echoes. Room follow-ups
  require `m.reference` to the request event, the exact event allowlist does
  not misclassify room requests as to-device requests, and the request target,
  known device and 10-minute-old/5-minute-future timestamp bounds are checked
  before a session is created.
- **Verification-request insertion matches Rust's collision rules.** An exact
  same-user/flow replay retains the active session. A differently identified
  active request from the same user cancels both old and new sessions with
  `m.user`, including room-to-room and mixed room/to-device cases; each cancel
  is routed through its original room or device recipient set. Standalone SAS
  starts remain separately addressable, matching this API's existing scope.
- **The commitment is computed over our re-encoding of the start content, not
  the bytes that arrived.** A peer with extra members in its
  `m.key.verification.start` gets a mismatch, because
  `Key_verification_start_content` drops what it does not model. ruma and
  matrix-rust-sdk share the limitation; fixing it means threading the raw
  `Jsont.json` through `Sas.from_start`.
- **The deprecated `hkdf-hmac-sha256` MAC has no direct libolm binary oracle.**
  `Sas.mac_base64_libolm` reproduces the base64 buffer-overrun bug that `.v2`
  exists to fix, transcribed from vodozemac's `calculate_mac_invalid_base64`.
  The optional vodozemac oracle now covers normal and legacy outputs over
  arbitrary vectors and a complete SAS flow. A direct libolm process remains
  unnecessary unless a separate legacy binary is required. We negotiate v2
  whenever the peer offers it.
- **Verification request fan-out is concrete-device only.** When the first
  `m.key.verification.ready` selects one of the originally requested devices,
  `Flow` emits directed plaintext `m.key.verification.cancel` messages with
  `m.accepted` to every other concrete recipient, once. A singleton request
  has no fan-out. A received cancellation is also relayed once to the other
  concrete recipients. A wildcard `To_device.All` cannot express “all except
  the selected device”, so it deliberately emits no fan-out; callers that need
  the Rust behaviour should expand known devices before requesting.
- **`Sas` covers `m.sas.v1` only.** The deprecated `curve25519` key agreement is
  rejected with `m.unknown_method`. `next_method` is retained across JSON codec
  round trips, and regression coverage proves that its presence does not alter
  the built-in SAS or QR-reciprocation flows. No current built-in method needs a
  generic chained-method engine; executing an opaque custom continuation stays
  caller/plugin-owned. There is no verification of another user's individual
  devices — deciding which keys to MAC for a cross-user flow is left to the
  caller through `Sas.identity`.
- **Verification prompts are caller-owned and detached from sync progress.**
  `Verification_service.confirm` publishes the prompt through its promise/
  callback boundary; callers that need human interaction must answer it from
  their own fiber. The sync loop does not claim to host a blocking prompt.
- **SAS now runs over both transports against a real homeserver.**
  `scenario_e2ee.ml` first drives the complete request/ready/start/accept/key/
  MAC/done exchange as `/sendToDevice` traffic between two sync loops. A second
  scenario starts the request inside an encrypted room and proves that every
  matching raw envelope — request and all follow-ups — is
  `m.room.encrypted`, every follow-up relates to the server-assigned request
  event and both devices finish trusted. Both sides see the same emoji and
  decimals. Neither live scenario supplies a self-signing secret, so they
  deliberately prove transport/local trust rather than signature publication;
  the latter remains covered by the mock flows above.

## Media

- **Encrypted media has a dependent attachment event.** Attachment
  AES-CTR/ciphertext SHA-256, strict metadata, verified streaming transport and
  the atomic queue graph are implemented. Encrypted original/thumbnail nodes
  retain ciphertext only, and substituted `file` metadata is applied before
  room-event encryption. The deterministic local-media lifecycle now covers
  local echoes, protected cache reads, local-to-MXC replacement, cancellation,
  crash recovery and orphan cleanup.
- **Media route selection and resolved URLs are capability-aware.**
  Downloads, thumbnails, config and URL previews query `/versions` and use
  authenticated Matrix 1.11 / MSC3916 paths when advertised, otherwise the
  deprecated unauthenticated `/media/v3` paths. The effectful
  `Media.mxc_to_http_resolved` performs the same decision and returns the
  matching URL. The pure `Media.mxc_to_http` still generates the authenticated
  URL without probing, while
  `mxc_to_http_unauthenticated` explicitly generates a browser-safe deprecated
  URL for callers that intentionally choose a route without discovery.
- **URL previews are implemented with the same capability decision** as media
  downloads: `Media.get_url_preview` selects the authenticated v1.11 endpoint
  or the deprecated v3 endpoint after `/versions`. It returns unknown preview
  fields as JSON rather than dropping them.
- **MSC2246 async upload is implemented.** `Media.create_content_uri` reserves
  an MXC URI and retains its optional unused-expiry timestamp;
  `upload_preallocated` refuses a locally expired reservation, fills the exact
  server/media-id pair with an authenticated `PUT`, distinguishes
  `M_CANNOT_OVERWRITE_MEDIA`, and keeps an ambiguous `M_NOT_FOUND` as an
  ordinary request error. It also recognises the explicit legacy Synapse
  expiry error used by the pinned Rust compatibility path. Exact mock requests
  and a real Synapse reserve/upload/overwrite/download round trip cover it.
- **Media-store integration is complete for the send path.** `Media_store`
  provides generic, memory and SQLite backends keyed by MXC/local URI and
  requested file/thumbnail format, with Rust-compatible 20 MiB/400 MiB/60-day
  limits and daily cleanup cadence. `Send_queue` protects local bytes, serves
  upload workers from the cache, publishes local URIs in echoes, performs
  idempotent remote-key replacement and reconciles crash orphans. Runtime can
  construct this durable queue directly. IndexedDB remains deferred.
- **Media fetching has a replaceable client seam.** `Media_fetcher` supplies
  the cache-aware default and an injectable file/thumbnail transport; encrypted
  bytes are verified before release and local queue URIs never reach a network
  fetcher. Content-scanner policy remains separate from cache wiring.
- **`Media.get_config` exposes `m.upload.size` and preserves unknown config
  fields as raw JSON.** It still does not interpret vendor-specific limits or
  add a cache for the capability response.

## Session and persistence

- **No migration from TOML.** Session files changed from `session.toml` &co to
  `session.json` &co; existing `$XDG_DATA_HOME/matrix` profiles are ignored and
  users must `omatrix login` again.

## omatrix

- **`omatrix backup enable` is safe, idempotent and cross-signed.** It
  queries the current server version before generating keys; an equal local
  version uploads pending keys without creating another version, while a
  missing or divergent local key refuses to mutate state. New and repaired
  backup auth data is signed with the matching cross-signing master key before
  the current device signature is added; a caller-supplied identity without a
  master secret retains the device-signature-only compatibility path.
- **`omatrix verify` and `sync` have bounded, clean shutdowns.**
  `verify --timeout SECONDS` defaults to 120, bounds each long poll by the
  remaining time and exits unsuccessfully on expiry. `sync` uses
  `Sync_service.run` under a nested switch, stops exactly at positive
  `--count`, cancels the in-flight poll on Ctrl-C/SIGTERM, restores handlers,
  and saves encryption state before teardown.
- **`omatrix msg` reuses durable joined-member state.** It restores the base
  projection, fetches authoritative members only when completeness requires it,
  and persists that result. A first encrypted message can still need the normal
  `/keys/query` and `/keys/claim` setup for devices with no local Olm session.

## Test infrastructure

- **A real homeserver now answers across 60 scenarios in 109.397s.**
  `test/integration/` runs `test_homeserver.ml` against whatever
  `MATRIX_TEST_HOMESERVER` names, and `test/integration/synapse.sh up` starts a
  pinned `ghcr.io/element-hq/synapse:v1.159.0` under the committed
  `test/integration/synapse/homeserver.yaml`. Green against Synapse 1.159.0:
  registration and `whoami` for two users, `/devices`; room create, `set_name`,
  `set_topic`, `get_name`, `get_topic`, invite, the invite arriving in the
  invitee's `/sync`, join, `Sync_service.members` and `/joined_members` and
  `/members` and `/joined_rooms` agreeing; a text message and a two-upload
  attachment graph through `Send_queue.start` and out of the other client's
  `Sync_service` timeline with the event id the queue was given, followed by
  byte-exact original/thumbnail downloads; room preview plus persisted knock-request
  seen state and invite/kick/ban moderation; public room publication and
  paged directory search, server event search, a real thread-root list and
  authoritative lazy-member refresh; media upload, authenticated
  download byte-for-byte, a 32×32 thumbnail, `/media/config`; one MSC4186 `sync_once`
  listing the room; and a room id with `!` and `:` unescaped in the path.
  The flag that serves sliding sync is `msc3575_enabled`, **not**
  `msc4186_enabled`: Synapse named it for MSC3575 and kept the name when the
  endpoint became MSC4186's (`synapse/config/experimental.py`,
  `experimental.get("msc3575_enabled", True)` — it is on by default in 1.159.0).
  The committed test configuration explicitly permits room-list publication,
  whose upstream default changed to deny in Synapse 1.126. The wider suite also
  covers `Matrix_ui.Room_timeline`, room-list and bot workflows described in
  `STATUS.md`.
- **End-to-end encryption answers too.**
  `test/integration/scenario_e2ee.ml` assembles what an application has to
  assemble — an `Encryption` machine, a `Sync_service.run` fibre running it over
  every response, a `Verification_service` the loop routes to, and a
  `Send_queue.start` that encrypts — for two freshly registered users, and is
  green against Synapse 1.159.0: a room created with `~encrypted:true` (which is
  `m.room.encryption` / `m.megolm.v1.aes-sha2` in `initial_state`), a message
  through Alice's queue arriving as plaintext in `room_change.decrypted` of
  Bob's sync while the event on the wire stays `m.room.encrypted` with the body
  nowhere in its content, and a device list on each side naming the other's
  device — which is only true because `/keys/upload` and `/keys/query` both
  worked. Then a second message in the same Megolm session (same
  `decrypted_session_id`, nothing in `undecrypted`), a reply in Bob's own
  outbound session; an attachment streamed encrypted to the media repository,
  described by a `file` object inside an encrypted room event, then streamed,
  authenticated and decrypted by Bob; full SAS flows over to-device events and
  encrypted room events (see "Verification"); a
  key backup created with `Room_keys.create_version` and restored by a *second
  real login* of Alice's that then decrypts the first device's message fetched
  back with `Messages.get_event`, driving one timed UTD-hook report despite
  duplicate observations and a store reload; and the backup's private key
  stored in SSSS under a passphrase-derived key and read back by re-deriving
  from the published `m.secret_storage.key.<id>` description. Separate live
  workflows drive ordered recovery enable/key rotation/disable/delete-all,
  recover that SSSS and backup on a completely fresh login, and have a trusted
  joining user import an authenticated MSC4268 bundle to decrypt pre-join
  history. The dehydrated-device workflow additionally queues Bob's real Olm
  room-key message for Alice's dehydrated device, imports it during rehydrate
  and decrypts Bob's captured encrypted sync echo in a fresh machine while
  preserving the primary identity. UI scenarios additionally validate
  incoming typing-user projection
  and a complete runtime forget, including immediate projection/cache/queue
  cleanup, stale-handle clearing and a post-forget sync resurrection barrier.
  Synapse rejected
  nothing any of this
  sent: no `/keys/upload`, `/keys/query`, `/keys/claim`, `/sendToDevice`,
  `/room_keys/version`, `/room_keys/keys` or account-data request came back an
  error, and with `Logs` at warning level the only non-UIAA warnings in a whole
  run are the expected 404 from the account-data read that precedes a first
  `store_secret`. `Harness.start_sync` grew a `?verification` argument for it,
  since a flow only runs itself if the sync loop routes both to-device and
  decrypted room verification events.
- **The E2EE scenarios order the key upload before the room.** `encrypted_room`
  waits for each peer's first sync response before creating the room, because
  that response is what performs the `/keys/upload`; a `/keys/query` that beats
  it is answered with an empty device list that the machine takes as final (see
  "E2EE: key management and backup"). The tests pass without the wait on this
  server, so the race is narrow — but it is a real ordering hazard, not a test
  artefact.
- **The integration config raises Synapse's rate limits out of the way.**
  `rc_login`, `rc_registration`, `rc_message`, `rc_joins`, `rc_invites`,
  `rc_admin_redaction`, `rc_3pid_validation` and `rc_media_create` are all set
  to 1000/s in `test/integration/synapse/homeserver.yaml`, so the suite has
  never seen a 429. The default Eio transport's idempotent retries and the
  higher-level send queue's JSON delay handling are therefore unexercised
  against a server. The replay-safe POST policy is covered with mock status and
  connection-failure paths; lowering the rates to Synapse's defaults remains
  the live `Retry-After`/429 test.
- **The vodozemac oracle is optional and needs `cargo`.**
  `test/vodozemac-oracle/` is not built by dune; without it the 7 interop cases
  in `test/test_olm.ml` print `SKIP` and pass, and only the recorded fixtures
  under `test/fixtures/olm/` run. CI does not build it. Build with
  `cd test/vodozemac-oracle && cargo build --release`, or point
  `$VODOZEMAC_ORACLE` at the binary.
- **MSC4108 rendezvous has its own isolated live fixture.**
  `test/integration/msc4108.sh up` starts a second flag-enabled Synapse on port
  8009; `dune build @integration-rendezvous` proves creation, PUT/GET/delete,
  two-party ECIES, bidirectional encrypted payloads and cleanup/cancellation.
  It is intentionally separate from the 60-scenario port-8008 reference and
  supplies no OAuth/MAS behavior. All three fixture scripts now accept
  independently named containers/data directories and guard purge with a
  fixture-specific sentinel. CI still runs only hermetic tests: a live CI job
  needs a runner with a Docker client/socket, reachable host networking,
  shared bind-mount paths, unique per-job port allocation and job wiring. The
  combined local runner already provides unique container/data ownership and
  an exit cleanup trap for both the Synapse reference and Dendrite core runs.
- **Add a row to `test_encoders.ml`'s `encoder guard` whenever a new
  body-carrying endpoint lands.** The guard drives every request-body-building
  entry point reachable from `Matrix_client` (89 rows) and requires both that
  the call did not fail with an encoder error and that a request carrying a body
  reached the network — which a missing `~enc` makes impossible.
- **`Client.secure_random` is never drawn from in `test_matrix_client.ml`** — it
  is handed a fixed source. Key generation and Olm have their own suites.
- **The `Fetch.restrict` guard is only reachable through a redirect.** Every
  path a caller supplies is joined onto `config.homeserver` by `Uri.with_path`,
  so no `Client` entry point can construct an off-origin URL directly; the test
  uses a 302 with an off-origin `Location`.

## Security notes

Recorded so the reasoning is not lost, not because anything is outstanding.

- **JSON is built structurally, not spliced.** `Uiaa` used to interpolate
  caller-supplied `password`, `token`, `session`, `sid`, `client_secret`,
  `user_id`, `address` and `response` straight into JSON string literals with
  `Printf.sprintf`, so a value shaped like `x","admin":true,"y":"` injected
  members into the `auth` object; `add_auth_to_body` cut the outer braces off
  with `String.sub` and could emit a duplicate `auth` member. Fixed across
  `uiaa.ml`, `backup.ml`, `push.ml`, `olm.ml` and `keys.ml`, guarded by
  `test/test_json_safety.ml` (28 cases of hostile input).
- **Signing values are built structurally too.**
  `Encryption.canonical_device_keys` and `Encryption.canonical_one_time_key`
  build `Jsont.Json` values and pass them through checked canonical Matrix JSON;
  they no longer maintain a separate string escaper.
- **Requests are narrowed with `Fetch.restrict ~under:[origin]`**, so no
  redirect or caller-supplied path reaches a server other than the homeserver,
  and bearer tokens are scoped and redacted via `Fetch.with_credentials`.
- **MACs are compared in constant time and Diffie-Hellman rejects
  non-contributory keys** in `olm.ml`; there is no `failwith` anywhere in
  `lib/`, so every parse and crypto failure is a `result`.
- **`Keys.generate_ed25519`, `Keys.generate_curve25519` and
  `Backup.generate_backup_key` raise `Invalid_argument`** if `Mirage_crypto_ec`
  rejects 32 uniformly random bytes. It rejects only a wrong length in
  mirage-crypto-ec 2.4.1, so the branch is unreachable and was not widened to a
  result. If a future curve backend adds range checks, these must become
  result-returning.

## Deferred or feature-gated from matrix-rust-sdk

Out of scope by decision, not oversight — see `PARITY_PLAN.md` §Scope. Thread,
search, space and lazy-cache work that used to be listed here was promoted to
P1.2/P2.1 by the 2026-09-03 audit; notification services have since landed.

- **Platform/storage variants:** IndexedDB, whole-store encryption and further
  SDK state-store backends. The base SDK state remains JSON per profile; the UI
  event cache has SQLite, and P0.3/P1.1 add a narrowly scoped media store.
- **Widgets** and their capability negotiation.
- **MatrixRTC beyond event codecs:** LiveKit focus/session membership,
  authenticated `/_matrix/client/v1/rtc/transports` discovery and automatic
  call membership/status. High-level live-location beacons are unrelated and
  are implemented independently.
- **Content scanner and local search index.** Server-side `/search` and its
  reactive `Matrix_ui.Search_service` are implemented. Only the local encrypted
  search-index/content-scanner parity remains deferred here.
- **Experimental crypto/identity:** Olm v2, Megolm v2, MSC4362 encrypted state
  events, MSC4385 push-secret sharing and X.509 identity verification.
- **MSC3956 extensible encrypted events and MSC4274 inline media galleries.**
  The default attachment dependency graph and media cache are complete;
  feature-gated event/gallery generalisations are not.
- **Experimental MSC4426 `m.status`/`m.call` automation.** The generic MSC4262
  profile wire/state slice and its common-fold/UI wiring are complete, while
  interpreting these experimental fields stays deferred.
- **Element-specific convenience stores:** recent emoji, recently visited rooms
  and product media-preview policy. Add these only with a consumer and a
  portability decision.
- **Rust/platform integration flags:** JavaScript/UniFFI bindings, the
  federation API, SOCKS/embedded-local-server transports and Rust TLS provider
  selection. These are not OCaml API-parity targets. QR raster rendering and
  scanning also remain caller/toolkit responsibilities; this SDK plans the
  payloads and protocols only.
