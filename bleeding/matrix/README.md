# ocaml-matrix

A pure OCaml SDK for the [Matrix](https://matrix.org) chat protocol, targeting
spec v1.17. End-to-end encryption included: Olm and Megolm are byte-for-byte
interoperable with libolm and vodozemac, and encryption is wired into the sync
loop and the send path rather than bolted beside them.

Built on [Eio](https://github.com/ocaml-multicore/eio) for concurrency,
[`fetch`](https://github.com/avsm/httpz) for HTTP and
[`jsont`](https://erratique.ch/software/jsont) for JSON. Needs **OCaml 5.5**.

> **Not production ready.** The hermetic suite runs against a mock transport;
> `test/integration/` is the full Synapse reference harness, while the bounded
> Dendrite smoke harness records portability outcomes. See `STATUS.md`.

The last recorded audit baseline is code SHA
`a76431516fe6982259eb3eb29b3a1db696f05a54`; `CHANGES.md` and `TODO.md` also
describe the subsequent unreleased work in this tree. Parity claims use
the pinned matrix-rust-sdk baseline `523b5af53a8fd9fae9e2bc981bfb01ac86fd2890`.
The adjacent Rust checkout was also inspected at
`f4b9512df23332fce1bd26037ef7a2387af2ced2` for implementation guidance, but
that newer HEAD has not had an exhaustive symbol-for-symbol audit;
`TODO.md` records only confirmed deltas.

## Libraries

The opam package is `matrix-chat`. Dune library names use the `matrix-chat.*`
prefix; OCaml module names remain `Matrix_proto`, `Matrix_client`, `Matrix_eio`,
`Matrix_cli`, `Matrix_ui`, `Matrix_ui_sqlite` and `Matrix_bot`.

| Library | What it is |
|---|---|
| `matrix-chat.proto` | Protocol types — identifiers, event contents, sync and sliding-sync bodies, push rules — with bidirectional `jsont` codecs, plus canonical JSON and the base64 form Matrix writes. No I/O. |
| `matrix-chat.client` | The SDK proper: every Client-Server endpoint, E2EE, `Base_client`, the Matrix-aware `Http_retry` policy, and the generic retention-aware `Media_store` seam, plus the fold of a sync response into room state. Returns `result`, takes HTTP and randomness as capabilities. One request per call; nothing here forks a fiber or waits on a clock. |
| `matrix-chat.eio` | The batteries-included Eio wrapper: supplies a `Fetch.t` from `Fetch_httpz.std` with system TLS and `Matrix_client.Http_retry.default ~homeserver`, raises structured, operation-contextual `Eio.Io` instead of returning `result`, and owns the driver loops — `Sync`, `Sync_service`, `Sliding_sync`, `Send_queue`, `Verification_service`. Configure a custom authenticator with `Fetch_httpz.std ~retry:(Matrix_client.Http_retry.default ~homeserver) ~https:(Httpz_tls.client ~authenticator)`. |
| `matrix-chat.cli` | The cmdliner terms a Matrix command line needs — homeserver, profile, username, password, room, verbosity — one argument each, so a command combines the ones it wants. |
| `matrix-chat.ui` | Eio-based reactive room lists and timelines, safe event presentation, local echoes, pagination, and a shared event cache with lazy persisted-tail hydration. |
| `matrix-chat.ui.sqlite` | SQLite event-cache and media-store backends, including metadata/tail/single-chunk reads so timelines and queued local media survive a restart without eagerly decoding all room history. |
| `matrix-chat.bot` | Bots over `matrix-chat.ui`: a typed event stream, one ordered handler fiber per room, commands with generated help, plugins that compose, per-room cursors in a `Plugin_store` JSON file, and the command line around them. |

### Optional pooled transport

`matrix-chat.eio` defaults to the pure-OCaml `Fetch_httpz.std` backend. It speaks
HTTP/1.1 and opens a connection per request. Applications that need connection
pooling and HTTP/2 can instead pass the optional `fetch-curl` backend through
the existing `~fetch` argument:

`fetch-curl` comes from the same restructured HTTPz source and shares its
publication gate. Install it from the verified dependency revision once that
source is published; it is not a required SDK dependency.

```ocaml
let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let homeserver = Uri.of_string "https://matrix.example.org" in
  let fetch =
    Fetch_curl.std ~sw
      ~retry:(Matrix_client.Http_retry.default ~homeserver) env
  in
  let _client =
    Matrix_eio.login_password ~sw ~env ~fetch
      ~homeserver
      ~user:"@alice:example.org" ~password:"from-a-secret-store" ()
  in
  ()
```

`fetch-curl` negotiates HTTP/2 over TLS by default and shares its connection
cache between requests. Passing
`Matrix_client.Http_retry.default ~homeserver` retains the SDK default: normal
idempotent HTTP methods and the read-like `/keys/query` POST may retry, while
`/keys/claim`, `/keys/upload`, sync and mutations do not.
It is not currently in opam-repository or this project's CI, so the recipe
requires a curl development library and has not been validated by the default
build; the default `Fetch_httpz.std` path remains dependency-free from libcurl.

Plus **`omatrix`**, an installed CLI: `login` (password or `--oauth`),
`logout`, `whoami`, `msg`, `sync`, `keys init`, `verify`, `qr login`, `qr grant`,
`backup enable|restore|status`. A password reaches it through
`--password-file FILE` or `$MATRIX_PASSWORD`; no flag takes the password
itself, because a command line is readable by every process on the machine.
`omatrix login --device-code` and the OAuth library support the RFC 8628
device-code flow for headless applications. The library also exposes the
pinned Rust SDK's MSC4388 QR payload and rendezvous-capability boundary; this
remains a typed unsupported channel. MSC4108 now has strict codecs, conditional
rendezvous, vodozemac-compatible ECIES and a typed two-party secure-handshake
core, plus the OAuth device-authorisation/authentication-message protocol and
Eio orchestration for both roles through device activation, secret handover,
trust and backup setup. `omatrix qr login` creates a textual Base64 MSC4108
payload using the exact MSC4108 creation endpoint, requires an unused profile,
confirms the check code, reports progress, and supports cancellation and a
timeout. It persists the OAuth session immediately after authentication, then
creates a fresh encrypted remote SSSS store containing cross-signing seeds and
an optional backup key; a passphrase file protects it, or a one-time recovery
key is printed. `omatrix qr grant` reads its credential from a file or
`MATRIX_SSSS_CREDENTIAL` and uses same-origin rendezvous. The Rust public/FFI
boundary hands raw QR bytes to the application; raster rendering/scanning and
the mandatory confirmation callback remain caller/UI-owned, not SDK parity
gaps. The isolated MSC4108 rendezvous harness covers creation, bidirectional
ECIES-encrypted messaging and cleanup/cancellation, but has no MAS/OIDC; the
full two-role OAuth QR flow therefore remains external. MSC4388 remains a
separate typed unsupported channel. SSSS can open a passphrase or Base58
recovery key and atomically import matching cross-signing seeds for
`Verification_service`; a
full self-signing import re-queries and marks the own device verified. The
credential remains caller-supplied, and that import path retains decrypted
material only in memory. `Verification_service` routes encrypted in-room SAS
messages with the room context and hardens sender/device handling; distinct
competing verification requests from one sender are cancelled consistently.
Authenticated clients optionally react to an
`M_UNKNOWN_TOKEN` with serialized, exact-token-attributed refresh and one
replay of buffered or GET-stream requests; one-shot POST streams and
unauthenticated requests are never replayed. Matrix and OAuth refresh tokens
are retained, and `omatrix` persists rotated credentials atomically. Proactive
expiry-aware refresh is also available as an opt-in client policy, with Eio
owning its completion on the client switch. For an encrypted invitation,
`Matrix_ui.Runtime.join` records the inviter's durable MSC4268 acceptance gate
only after the homeserver confirms the join; `Runtime.join_room` does the same
for aliases after resolving them before `/join`. `omatrix sync --count N` stops
after exactly N successful responses; Ctrl-C/SIGTERM cancels its in-flight poll
and saves encryption state before shutdown.

## Example

Log in, sync with encryption running, and send an encrypted message:

```ocaml
let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let client =
    Matrix_eio.login_password ~sw ~env
      ~homeserver:(Uri.of_string "https://matrix.example.org")
      ~user:"@alice:example.org" ~password:"hunter2" ()
  in
  let enc =
    Matrix_eio.Encryption.of_env env
      ~user_id:(Matrix_eio.Client.user_id client)
      ~device_id:(Matrix_eio.Client.device_id client)
      ()
  in
  Matrix_eio.run_sync ~sw ~env client ~encryption:enc
    ~on_sync:(fun (r : Matrix_proto.Sync.Response.t) ->
      (match r.rooms with
       | Some rooms -> Format.printf "synced %d rooms@." (List.length rooms.join)
       | None -> ());
      Matrix_eio.Sync.Continue)
    ();
  let room = Result.get_ok (Matrix_proto.Id.Room_id.of_string "!room:example.org") in
  let members =
    List.map fst (Matrix_eio.Rooms.get_joined_members client ~room_id:room)
  in
  ignore
    (Matrix_eio.Encryption.send_encrypted_text enc client room
       ~body:"hello, encrypted world" ~members)
```

`run_sync` forks the loop onto `sw` and returns at once. It folds each response
through the encryption machine before anything else sees it: one-time key
counts drive `/keys/upload`, `device_lists` drives `/keys/query`, to-device
messages are Olm-decrypted and answered, authenticated MSC4268 room-key
bundles are checked against their MSC4147 signed device proof and retained,
and encrypted timeline events come back decrypted.

For a graphical or terminal UI, construct `Matrix_ui.Runtime` around that same
client. `Matrix_ui.Room_list.rooms` and `Matrix_ui.Room_timeline.items` expose
atomic snapshots followed by bounded, race-free diffs; the application remains
responsible only for rendering them. `Room_timeline` also provides high-level
`send_single_receipt`, `send_multiple_receipts`, `mark_as_read` and
`latest_user_read_receipt` controls, with monotonic duplicate suppression.
Clearable display name, avatar URL and custom profile fields are available
through the profile APIs. Runtime can own a persistent send queue
backed by caller-provided queue and media stores; queued attachments keep
protected local bytes through restart and move them to their MXC cache key only
after upload. A successful `Runtime.forget` removes the room from base state,
receipts, event/timeline storage, thread subscriptions and the whole send graph,
detaches in-flight sends so they cannot redact or reappear, and best-effort
removes the room from `m.direct`. Responses fetched before forgetting are not
allowed to resurrect the room. The separate `matrix-chat.ui.sqlite` library backs
both event and media stores and defaults to persisting ciphertext rather than
decrypted event bodies. `bin/matrix-bot/`
holds five `matrix-chat.bot` plugins built on it — echo, commands, welcome,
moderator and a logger that back-fills — plus a one-shot notifier and the
single `matrix-bot` executable that composes them, each run against Synapse by
the integration suite; its README says how to run them.

New to the SDK? [`example/`](example#readme) is a tutorial that walks from a
first login through syncing, rooms, media, encryption and verification to a
`matrix-chat.bot` plugin, each step a short program with a real transcript.

## Build and test

```sh
opam install . --deps-only --with-test
dune build
dune runtest
```

The five HTTP dependencies (`fetch`, `fetch-httpz`, `httpz`, `proffer`,
`proffer-httpz`) currently come from the development HTTPz checkout. The
published `avsm/httpz` HEAD checked on 2026-09-08 is the older OxCaml project
and lacks four of these packages, so the former floating Git pin recipe does
not install this SDK. Publishing the restructured HTTPz packages is a release
blocker; see [the release review](RELEASE_REVIEW.md#fr3--prove-installation-from-the-distributed-sources).

Once a published revision contains all five packages, validate an extracted
release archive independently of the development switch:

```sh
MATRIX_HTTPZ_SOURCE='https://host/httpz.git#FULL_40_DIGIT_COMMIT' \
  test/release-install.sh /absolute/path/to/extracted-release
```

That script verifies the package manifests and creates an isolated OCaml 5.5
switch under `/tmp`. `test/release-check.sh` builds the locked vodozemac oracle,
requires real oracle execution, runs the local suite and starts fresh Synapse
and Dendrite fixtures with cleanup and retained logs. It needs Cargo and Docker.
Missing release fixtures fail these checks. Full two-role MSC4108 OAuth/MAS
login remains experimental pending its separate live validation.

Backup restore takes a protected key file:

```sh
omatrix backup restore --recovery-key-file recovery.key
```

Keep the file readable only by its owner. Restore checks the current backup's
algorithm and public key before touching crypto state or downloading keys.
Crypto saves retain a private redo journal so a restart can recover a complete
account, ratchets and trust state. Native file replacements sync both file and
parent directory. The journal has the same filesystem security boundary as the
existing unencrypted profile files.

**Encrypted local storage is deferred.** Profiles store access/refresh tokens,
private keys and session state without encryption, including refresh markers
and crypto recovery journals. Base64-encoded keys are still plaintext secrets.
New profile directories use mode 0700 and these files use mode 0600; existing
directory permissions are left unchanged. There is no local-storage unlock
prompt, passphrase or OS keyring integration. Protect the profile and its
copies/backups through OS access controls and, where needed, an encrypted
filesystem or volume. Matrix end-to-end encryption protects messages in
transit; it does not encrypt these local files.

`omatrix` coordinates Matrix and OAuth refresh across processes sharing a
profile. SDK callers opt in with `~store` on the Eio client/OAuth refresh
helpers. These persist before notifying `on_session_update`; that callback must
not write credentials. An interrupted or failed exchange can have consumed a
rotating token, so the persisted coordinator conservatively requires a new
login instead of retrying it. OAuth discovery or validation failures occur
before that boundary and can be retried. A successful new `omatrix` login
durably replaces credentials before clearing any old refresh marker, including
a malformed one. SDK callers use `Profile_store.save_login` for new login
credentials; ordinary metadata updates cannot clear a marker. Low-level
refresh without a store keeps its existing per-client behavior.

The suite is 64 executables and 1,457 cases/checks, all against a mock transport
or a local SQLite database, plus three source guards. The integration suite
below is skipped unless a homeserver is named.

### Integration tests against Synapse

`test/integration/` runs the SDK against a real homeserver. `dune runtest`
picks it up too, where it prints `SKIP` and passes unless
`MATRIX_TEST_HOMESERVER` is set, so the default build stays hermetic.
The current harness has 60 scenarios and is green against the pinned Synapse
1.159.0 image in 109.397s, including a caption-mutated atomic queued
original/thumbnail attachment send,
MSC2246 preallocated uploads, encrypted-room media streaming, complete SAS over
both to-device and encrypted-room events, a backup-driven late-key/UTD report,
recovery enable/rotate/disable, fresh-login SSSS recovery and backup restore,
trusted pre-join history import, and stable room-retention state with the
unsupported MSC1763 server-policy fallback, persisted room
previews and knock moderation, network edit-history ordering, a queued static
location through another client's sync, public-directory and server event
search, a live thread root and lazy member refresh, the common sliding-sync
service fold, Adaptive_sync native selection/fallback (a project enhancement,
not pinned Rust parity), legacy world-readable room peeking, and legacy MSC3814
dehydrated-device lifecycle with real-key continuity through rehydration.

`test/integration/synapse.sh` runs a pinned `ghcr.io/element-hq/synapse` image
in Docker, configured by the committed
`test/integration/synapse/homeserver.yaml` — open registration, no federation,
rate limits raised out of the way, and simplified sliding sync (MSC4186, which
Synapse still calls `msc3575_enabled`) plus MSC3814 dehydrated devices turned
on:

```sh
test/integration/synapse.sh up          # idempotent; prints the export line
eval "$(test/integration/synapse.sh url --export)"
dune build @integration
test/integration/synapse.sh down --purge
```

`up` also accepts `MATRIX_TEST_PORT` (default 8008),
`MATRIX_TEST_SYNAPSE_CONTAINER` (default `ocaml-matrix-synapse`) and
`MATRIX_TEST_SYNAPSE_DATA` (default `$TMPDIR/<container-name>`); `status`
reports what is running and `logs` tails the container. Container-specific data
defaults and sentinel-guarded purge let independent runs coexist. Any
homeserver that accepts open registration will do — point
`MATRIX_TEST_HOMESERVER` at it and skip the script.

### Isolated MSC4108 rendezvous harness

The MSC4108 harness is separate from the 60-scenario Synapse suite and uses
port 8009. It passes rendezvous creation, bidirectional ECIES-encrypted
messaging and cleanup/cancellation. It deliberately does not provide MAS/OIDC,
so the full two-role OAuth QR flow remains an external deployment check:

```sh
test/integration/msc4108.sh up
eval "$(test/integration/msc4108.sh url --export)"
dune build @integration-rendezvous
test/integration/msc4108.sh down --purge
```

The second-homeserver smoke harness pins Dendrite v0.15.2. Its named five
portable room-core cases pass (5/5). Its peeking case is explicitly rejected
with `M_GUEST_ACCESS_FORBIDDEN`; optional threaded receipts time out, MSC2246
preallocation is rejected despite the advertised MSC3916 media capability,
and simplified sliding sync is unavailable. Dendrite runs with federation
disabled. Keep these capability outcomes separate from the Synapse reference
harness with the explicit commands:

```sh
test/integration/dendrite.sh up
eval "$(test/integration/dendrite.sh url --export)"
test/integration/dendrite.sh capabilities
test/integration/dendrite.sh run-core
# Explicit capability probe; Dendrite v0.15.2 currently returns non-zero.
test/integration/dendrite.sh run-peeking
test/integration/dendrite.sh down --purge
```

Set `MATRIX_TEST_DENDRITE_CONTAINER`, `MATRIX_TEST_DENDRITE_DATA` and
`MATRIX_TEST_DENDRITE_PORT` to isolate another Dendrite run. Synapse remains the
full reference suite; this Dendrite profile is additional portability coverage.

For a disposable sequential run of both implementations, use the combined
harness. It chooses fresh per-run container/data names, uses an isolated Dune
build directory, runs the full Synapse reference suite first, then Dendrite's
five-case `run-core` profile, and cleans up Dendrite followed by Synapse even
when a test or signal fails:

```sh
test/integration/run-both.sh
# Optional: MATRIX_TEST_RUN_ID=ci-42 MATRIX_TEST_PORT=8108 \
#   MATRIX_TEST_DENDRITE_PORT=18108 test/integration/run-both.sh
```

`MATRIX_TEST_RUN_ID` is validated and collisions are refused. The peeking
capability result is intentionally not folded into this passing run; use
`dendrite.sh run-peeking` separately and expect the pinned Dendrite fixture to
return its explicit `M_GUEST_ACCESS_FORBIDDEN` result.

### The vodozemac oracle (optional)

`test/test_olm.ml` proves Olm and Megolm interoperate with
[vodozemac](https://github.com/matrix-org/vodozemac) in both directions, by
driving a small Rust binary. It needs `cargo` and is **not** built by dune:

```sh
cd test/vodozemac-oracle && cargo build --release
```

Without it those cases print `SKIP` and pass, and the recorded vectors under
`test/fixtures/olm/` still run. CI does not build it.

## Documentation

- **`STATUS.md`** — what is implemented, per spec module, checked against the
  code; the deliberate ecosystem quirks; the top ten limitations.
- **`TODO.md`** — every known gap by area, with the file and the fix.
- **`ROADMAP.md`** — the order those gaps are being worked in.
- **`HACKING.md`** — how interfaces, comments and dead code are judged here.
- **`CHANGES.md`** — release notes, including the security fixes.
- **`PARITY_PLAN.md`** / **`PORT_PLAN.md`** — the completed roadmaps and what
  is still short of matrix-rust-sdk. Both predate the module restructuring and
  name modules that have since moved.

Build the API docs with `dune build @doc`.

## Licence

ISC. See `LICENSE.md`.
