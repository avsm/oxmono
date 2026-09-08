# Zulip port to Fetch, Jsont and Xdge

This is the design plan agreed on 6 September 2026 and used for the port.
The implemented API and setup instructions are in [README.md](README.md),
with measured endpoint coverage in [API_COVERAGE.md](API_COVERAGE.md).
The reference survey below records the pre-port state. Compatibility with
the old API and configuration format was deliberately not retained.

## Agreed scope

- Target the latest Zulip, with `https://eeg.zulipchat.com` as the eventual hosted
  deployment. Use a pinned current release for reproducible local Docker tests;
  maintain no historical server-version compatibility matrix.
- Preserve and correct existing endpoint coverage, and fill gaps identified by
  comparison with the Python SDK's public endpoint helpers. Inventory those gaps
  explicitly rather than expanding to every endpoint in the full REST API.
- Deliver live events with persistent plugin state. Durable inboxes, replay of
  events received during downtime and automatic history catch-up are deferred.
- Use named XDG JSON profiles, with standard zuliprc import as a convenience.
- Include the local Docker integration harness and align bot interfaces with
  Matrix in preparation for a future shared library.

The hosted site is a deployment target, not a dependency of the test suite.
Local fixtures exercise mutations; a future hosted smoke test can use a dedicated
bot and test channel when that deployment is configured.

## Delivery and validation

The port now provides `zulip`, `zulip.eio`, `zulip.bot`, and `zulip.bot.cli`,
with all 74 Python SDK API/event helpers and its three transport utilities
accounted for in the coverage inventory.
The implementation uses the current linked Fetch/httpz APIs, including
`Duration.t` for transport timeouts; application deadlines remain seconds.

Validation completed with 53 offline tests and 14 scenarios against a freshly
initialized Zulip 12.2 Docker stack. The fresh wrapper run also verified automatic
container, volume, network and credential cleanup. `dune build`, package
installation targets, all three examples, and API documentation build
successfully with the linked httpz checkout. The Zulip package and documentation
can also be built with `dune build zulip.install @doc/doc`.

Queue expiry/re-registration, terminal poll failures, saturation, ambiguous
sends, plugin persistence and write failures are tested deterministically
offline. The live suite verifies queue delivery/invalidation and actual bot
replies. It does not yet restart the server during a running bot. Webhook
envelope validation and dispatch are implemented and tested offline; an HTTP
listener remains the application's responsibility. Remote Zulip bot storage is
an endpoint API; persistent plugin state uses the local XDG file store. The
Docker command is usable on a Docker-capable CI runner; no Docker CI job or
hosted smoke test has been run as part of this port.

## Reference checkouts and findings

The references inspected were:

| Checkout | Revision | What to follow |
| --- | --- | --- |
| This library | `775ee87` | Existing endpoint families and examples |
| `../ocaml-httpz` (also symlinked here) | `0bad717` | Fetch capability, httpz backend, forms, credentials, response ownership and mocks |
| `../ocaml-jmap` | `82bf3db` | Pure protocol/Eio separation, injectable `Transport`, structured errors and Fetch tests |
| `../python-zulip-api` | `b1723475` | Zulip parameter encoding, queue lifecycle, bot replies, storage and server CI |
| `../../knot/ocaml-matrix` | `1f18142` | Composable bot specifications, typed events, ordered dispatch, send handles, plugin stores and local integration harness |

There is no `../ocaml-matrix` in this workspace; the Matrix reference above was
found elsewhere under the same source tree. Its relevant interfaces are in
`lib/matrix_bot/`, and its Docker harness is in `test/integration/`.

The existing library has approximately 4,500 lines of implementation plus
examples. A mechanical dependency replacement would preserve several problems:

- `lib/zulip/client.ml` creates a Requests session, guesses content types from
  body contents, puts all `params` in the URL, and discards DELETE bodies.
- `lib/zulip/encode.ml` manually builds JSON inside forms, skips nested objects
  and some array elements, and does not consistently escape structured values.
- Received bot messages and outgoing API messages live in separate models.
  Much of the endpoint API returns raw JSON.
- `lib/zulip/event_queue.ml` silently drops events that fail decoding and advances
  its cursor before dispatch. Its sequence helper takes only the first event
  from a blocking batch and can call `List.hd` on an empty batch.
- `lib/zulip_bot/bot.ml` starts at `-1` instead of the registration cursor,
  retries every two seconds without replacing expired queues, and replies to
  only the sender of a group DM. The Python bot handler preserves recipients.
- Bot storage changes its cache before the server confirms writes and implements
  removal by storing an empty string. The Python client has a real DELETE
  operation in `Client.remove_storage`.
- File and emoji uploads are stubs. The Atom example's `fetch_feed` returns mock
  entries. The regression example is a live bot rather than a hermetic test suite.
- A targeted baseline build of both libraries currently fails because
  `init.bytesrw` and `init.eio` are unavailable. OCaml 5.5 and the httpz symlink
  are present. No server tests were run during planning.

JMAP currently resolves its XDG paths itself. Follow JMAP for transport and
codecs, and use Xdge directly, as Matrix does, for this project's filesystem
layout. Xdge is an Eio library, not just a module rename for `Xdg_eio`.

## Proposed library boundaries

Keep one opam package initially, with these public Dune libraries:

| Library | Modules and responsibility | Direct dependency direction |
| --- | --- | --- |
| `zulip` / `Zulip` | IDs, messages, channels, users, narrows, event payloads, request/response codecs, API error data | Jsont; no Eio or HTTP backend |
| `zulip.eio` / `Zulip_eio` | `Transport`, `Client`, endpoint functions, `Event_queue`, `Auth`, `Profile` | `zulip`, Eio, Fetch, fetch-httpz, Xdge |
| `zulip.bot` / `Zulip_bot` | `Context`, `Bot`, `Event`, `Room`, `Sent`, `Plugin_store` | `zulip.eio` and protocol types |
| `zulip.bot.cli` | `Main`, argument and logging setup | `zulip.bot`, Cmdliner, Eio_main |

The small CLI library keeps executable startup and process-global signal
handling out of the reusable runtime. No Matrix dependency is introduced.

Protocol modules expose concrete types and `t Jsont.t` codecs. Runtime endpoint
modules expose straightforward client functions; shared request construction
stays internal initially. Avoid building a generic endpoint DSL before the
endpoint audit demonstrates a need for one.

Use an abstract `Transport.t`, following JMAP's `Transport.v` and
`Transport.of_fetch`, carrying Fetch and the clocks used for timeouts/backoff.
`Client.create ~transport ~auth` must work with `Fetch_mock.client`; provide an
environment constructor for ordinary programs. Let the runtime own switches and
fibers, and the transport remain reusable configuration.

Adopt result-returning endpoint operations with a structured error type, following
JMAP. Distinguish Zulip API errors, non-JSON HTTP failures, codec errors, transport
failures and timeouts. Preserve HTTP status, Zulip code, extra JSON fields and
relevant retry metadata. Cancellation propagates as cancellation. A small
explicit exception convenience function is possible; avoid maintaining two full
parallel endpoint APIs.

## Transport and wire format

1. Build the default stack using `Fetch_httpz.std`, with system-trust TLS and
   cookies disabled for API-key authentication. Attach Basic credentials through
   `Fetch.Credential` and `Fetch.with_credentials`, scoped to the configured
   server origin. Normalize the site/API prefix once and encode path components
   using httpz URI utilities. Reject cross-origin credential forwarding; use a
   conservative redirect policy for mutations.
2. Use `Fetch.with_response` for every exchange and decode inside its callback.
   Bound JSON/error bodies and release responses after success, decode failure,
   timeout or cancellation. Keep file upload/download paths streaming.
3. Introduce an explicit parameter representation distinguishing strings from
   values to encode as JSON. Match Python's `do_api_query`: strings pass through;
   booleans, numbers, arrays and objects are JSON-encoded. Omission and an explicit
   JSON `null` remain different. GET uses query parameters; other supported
   methods use form bodies as specified by the endpoint.
4. Feed the resulting string pairs into `Fetch.Form.urlencoded`; use
   `Fetch.Form.multipart` for uploads. Remove content sniffing, handwritten form
   escaping and JSON-to-object-to-form conversions. Test DELETE bodies too.
5. Use `Fetch.Json`/Jsont media codecs for HTTP decoding and `Jsont_bytesrw` for
   standalone fixtures and profile files. Preserve structured codec errors.
6. Separate ordinary request deadlines from long-poll deadlines. Request the
   relevant registration state and honor `event_queue_longpoll_timeout_seconds`,
   with the documented 90-second fallback. The httpz backend's default 60-second
   idle timeout also needs explicit adjustment for polling: extending only an
   outer timeout would still interrupt valid polls. Use distinct configurable
   polling/API transport stacks when necessary. Reserve concurrency for sends
   while a poll is outstanding. See [queue registration](https://zulip.com/api/register-queue).
7. Assign retries by operation. Fetch can retry safe reads with bounded backoff;
   the queue runner handles reconnects, and the send queue handles explicit
   rate-limit rejection. Avoid multiplying retries across layers. Do not
   automatically resend a message after an ambiguous timeout or 5xx failure.
   Zulip's `local_id` is an echo correlation value, with no advertised
   idempotency guarantee. Surface an indeterminate send outcome when appropriate.
   See [sending messages](https://zulip.com/api/send-message).

The inspected httpz backend opens a connection per request and speaks HTTP/1.1.
Transport injection leaves room for a pooling backend later; connection pooling
is not part of this port.

## Protocol and endpoint audit

Replace every `Json.codec`/`Json.t` with actual Jsont types and JSON. Keep unknown
fields and enum values where the API is extensible; do not turn malformed known
values into empty lists, zero IDs or empty strings. Preserve absent/null
distinctions and integer IDs without float/string round trips.

Create one received `Message.t`, with typed channel and DM destinations, sender,
content format, timestamp, flags and reactions. Use separate outgoing request
records/builders. Distinguish user, channel, message, recipient and queue-event
IDs at the type level. Use stable numeric user IDs for bot identity and routing.

Audit request spellings and response types against the Python code and current
official endpoint documentation. In particular, API send types and received
message types need separate mappings; DM recipient objects use `id`; the
`automatic_new_visibility_policy` response is an integer, whereas the old codec
expects a string. These details are visible in the official
[message API](https://zulip.com/api/send-message) and
[event examples](https://zulip.com/api/get-events).

Use distinct types/codecs for message-search narrows and queue-registration
narrows. The latter's restricted list-of-pairs format must not reuse the existing
message-search object codec. Record the tested current server feature level and
request only capabilities we implement. Preserve extensible fields and unknown
event types so routine hosted-server updates do not require an immediate client
release, while supporting only the latest Zulip.

Port all existing endpoint families, including currently exposed stubs, in this
order. Maintain a checked-in coverage table listing method, path, request codec,
response codec, feature assumptions and test coverage. Compare this inventory
with every public endpoint helper in the Python SDK, and include missing
operations alongside the relevant family:

| Order | Endpoint family | Main validation |
| --- | --- | --- |
| 1 | Server settings, own user, authentication | Bootstrap and useful error reporting |
| 2 | Send/get/search/edit/delete messages, flags, reactions, rendering | Unicode/form encoding, pagination, channel topics and DMs |
| 3 | Channels, subscriptions, topics and subscribers | Nested subscription parameters and stable IDs |
| 4 | Queue registration, polling and deletion | Live delivery, timeout and recovery |
| 5 | Files, custom emoji and bot storage | Multipart, real deletion and failure behavior |
| 6 | Remaining users, groups, presence, typing, settings, server metadata and scheduled-message operations | Existing coverage accounted for and typed responses |

Keep an explicit raw endpoint escape hatch for API extensions. Completion covers
existing operations and the Python SDK gaps recorded in the inventory. Full
parity with every endpoint in the current Zulip REST API is a separate scope
expansion; replace the README's claim of completeness with measured coverage.

## Event collection and bot runtime

Implement queue management as an explicit state machine: register, poll, back
off, re-register, stop. Start from the returned cursor, account for heartbeats,
deduplicate redelivery within a queue generation and handle empty batches.
`BAD_EVENT_QUEUE_ID` replaces the queue and refreshes necessary initial state;
it must not leave the runtime retrying the same ID forever. Authentication and
permission failures should become terminal states rather than reconnect loops.
This follows Python's `call_on_each_event` and the
[queue-expiry contract](https://zulip.com/api/get-events).

Separate collection progress from handler progress. A batch must be accepted by
the dispatcher before subsequent polling acknowledges it. Concurrent handlers
must not advance a shared processed cursor to the maximum completed event while
earlier events remain pending. Collection and processing state are in memory for
this port. Queue event IDs belong to one queue generation and cannot serve as
restart-safe history cursors.

Unknown event types reach a `Custom` handler with raw JSON. Malformed known
events produce an observable decode failure, with an explicit policy for
quarantining/skipping them; do not silently filter them out or spin forever on
the same batch. Event payloads can be partial. Resolve edit/reaction targets
using a bounded message-location cache and API lookup when available; preserve
unresolved protocol events instead of inventing senders or rooms.

The bot interface should closely resemble Matrix's:

```ocaml
(* Proposed API shape, not code that builds before the port. *)
let ping bot (command : Zulip_bot.Event.command) =
  ignore bot;
  Zulip_bot.Event.reply command.message.envelope "pong"
  |> ignore

let spec =
  Zulip_bot.Bot.v ~name:"example" ~prefix:"!" ()
  |> Zulip_bot.Bot.command ~name:"ping" ~doc:"Check the bot is alive" ping
  |> Zulip_bot.Bot.help
```

- Immutable `Bot.spec`; `type plugin = spec -> spec`; registrations including
  `on`, `on_message`, `command`, `help`, `on_edit`, `on_reaction`, `on_delete`,
  `on_custom`, `on_sync` and `on_error`. Add Zulip-specific subscription/user
  handlers without inventing Matrix equivalents. Match Matrix's command
  dispatch rule: a command is not also an ordinary `on_message` invocation.
- `Context.v` accepts an existing client, clocks and stores; `Context.connect`
  loads a named profile and resolves bot identity. `Bot.run`, `on_start` and
  idempotent `Bot.stop` give the caller control over the lifetime.
- Order handlers by registration, and events within each room. Rooms run
  concurrently under bounded queues and a bounded worker budget. A room is a
  channel or a DM participant set, so all topics of one channel initially share
  ordering. Bound total pending events and evict idle workers. Document that
  overload eventually backpressures collection; an in-memory runner cannot
  promise unbounded isolation from a permanently blocked handler.
- Handler exceptions reach `on_error` and the next handler/event continues;
  cancellation always propagates. Specify that failure of a handler does not
  imply automatic replay of its side effects.
- Default addressing: one-to-one DMs and explicit bot mentions; ignore own
  messages and other bots by default, with options for all messages and group
  DMs. Use event flags and numeric identities. Strip a leading mention for
  command parsing while preserving original content. Group-DM replies retain
  the participant set even when group DMs require mention-based activation.
- `Room` sends and `Event.reply` return `Sent.t`, with status, await and
  cancellation. An await timeout does not cancel the underlying operation.
  Preserve per-room send ordering, settle handles during shutdown, and bound
  the outgoing queue. Reactions/edits/uploads get distinct outcomes as needed.
- Replace the single-return-value `Response.t` handler model with these send
  capabilities, enabling multiple replies and plugins with background work.
- Rebuild the advertised webhook mode as an adapter into the same dispatch
  machinery. Decode the actual envelope (`token`, `trigger`, `message`), validate
  the configured webhook token before dispatch, and reuse an existing context
  instead of fetching identity/storage for every delivery. Define one reply
  path so HTTP response replies and queued API sends cannot duplicate each
  other. The application supplies the HTTP server. Follow the Python botserver
  and the [webhook payload contract](https://zulip.com/api/outgoing-webhook-payload).
- Use typed `Plugin_store.find/set/update/remove` with `Jsont.t`, namespaced by
  plugin and optionally room. Provide memory and atomic local-file stores.
  Updates serialize across fibers. Make remote Zulip bot storage an optional
  backend; cache only confirmed writes, use the real DELETE endpoint, and
  document that local atomic update does not create a distributed transaction.
  See [storage deletion](https://zulip.com/api/remove-bot-storage).

Implement live delivery and local plugin persistence. On restart, register a new
queue and handle new events; do not replay history or persist pending events and
sends. Queue expiration can lose events, and persisting a queue ID cannot repair
that. Expose recovery/gap status and omit `backlog:Handle`. Durable inboxes and
bounded message-history recovery are future work; history cannot reconstruct
every missed reaction, deletion or intermediate edit. Exactly-once bot side
effects are not promised.

## Preparing for a future Matrix/Zulip bot library

Align the handler/command/store/send lifecycle now, and leave extraction of the
shared library to a later change. Common bot behavior should depend on opaque
user, room and message identities plus send/reply/react/store capabilities.
Keep access to the underlying protocol client as an explicit escape hatch.

| Concept | Zulip interpretation | Consequence for a future adapter |
| --- | --- | --- |
| Room | Stable channel ID or normalized DM participants, scoped to account/realm | Display names and emails are not identity keys |
| Thread/destination | Channel plus topic, or a DM participant set | Preserve topic on every reply; it is not Matrix's room description |
| Reply | Same topic/group, optionally a quote or link to a message | Do not claim a native Matrix reply relation exists |
| Message content | Plain text or Zulip Markdown with known format | Distinguish portable text from protocol-specific rich content |
| Bot output | Normal Zulip message sent by a bot account | There is no Matrix `m.notice` equivalent to rely on for loop prevention |
| Administration | Zulip roles, groups and permissions | Use an authorization predicate, not synthetic Matrix power levels |
| Event cursor | Ephemeral queue ID plus queue-event ID | Do not share Matrix sync-token persistence semantics |

Provide a small portable ping/help/counter example using only these capabilities
as an architectural check. The two OCaml packages' types will remain distinct
until a shared signature and adapters are actually extracted.

## Profiles, CLI and dependencies

Use `Xdge.create fs "zulip"` with named JSON profiles. The paths are
`$XDG_CONFIG_HOME/zulip/profiles/<name>.json` for credentials/options and
`$XDG_DATA_HOME/zulip/profiles/<name>/plugins.json` for persistent plugin data.
Reserve XDG state for future runtime checkpoints, and cache for data that can be
recreated. Keep the profile and bot/plugin names separate.

Use Jsont for the profile schema and explicit filesystem capabilities. Establish
one precedence order: explicit CLI options, environment, selected profile.
Report malformed profiles rather than silently falling back to unrelated
credentials. Protect credential files and profile subdirectories explicitly;
Xdge's default directory creation modes alone do not provide private storage.

Retain standard `[api]` zuliprc import as a convenience for credentials downloaded
from the hosted service, while making JSON the primary profile format and
dropping the old custom `[bot]` format. Choose a maintained INI decoder or a
documented, deliberately limited zuliprc reader during implementation. The
missing `init.*` libraries are not solved by replacing nox dependencies, so
resolve this in the first phase. Avoid building a new general-purpose INI parser.

Remove `requests`, `nox-json` and `nox-xdg`. Add explicit `fetch`, `fetch-httpz`,
`jsont`, `jsont.bytesrw` and `xdge` libraries where used, with `httpz.uri`/media
dependencies only where referenced. Remove direct `base64` and `uri` dependencies
if Fetch/httpz absorb their remaining uses. Keep `init` only if the importer
choice requires it. Regenerate `zulip.opam` from `dune-project`.

The inspected Fetch/httpz packages require OCaml 5.5, so raise the package's
current 5.1 minimum accordingly. Tests use Alcotest, `fetch.mock` and Eio mock
clocks/backends. Keep the development symlink local, document setup, and make
package-only build/test commands avoid unrelated httpz example/test aliases.

## Local Docker integration strategy

The Python bindings already have a real-server CI precedent in
`../python-zulip-api/.github/workflows/zulip-ci.yml`. It runs in `zulip/ci` Docker
containers, checks out selected `zulip/zulip` server revisions, provisions the
backend, installs the bindings, then executes `tools/test-api` and selected bot
backend tests. Its ordinary bot tests instead use `StubBotHandler`,
`MockMessageServer`, in-memory storage and recorded HTTP conversations. It does
not include a ready-made local Compose wrapper for a listening test server.

Adopt two corresponding layers here: fast Fetch/mock tests and an explicit real
server suite. For the latter, follow Matrix's developer interface around the
official [docker-zulip Compose stack](https://github.com/zulip/docker-zulip).
That stack runs Zulip, PostgreSQL, RabbitMQ, Redis and memcached. Its current
[Compose source](https://raw.githubusercontent.com/zulip/docker-zulip/main/compose.yaml)
names `ghcr.io/zulip/zulip-server:12.2-0`; treat that as the planning-time
candidate. Verify the latest release when implementing the harness and pin the
chosen image, service images and source revision/digests for reproducibility.
The new image has different configuration from the older Docker Hub image;
follow the [current configuration guide](https://zulip.readthedocs.io/projects/docker/en/stable/how-to/compose-upgrading-from-legacy.html).

Proposed files:

```text
test/integration/
  compose.yaml
  zulip.sh                 # up, run, logs, status, down
  seed.py                  # runs inside the pinned Zulip container
  harness.ml
  scenario_messages.ml
  scenario_channels.ml
  scenario_events.ml
  scenario_bots.ml
  scenario_storage_uploads.ml
  test_server.ml
  dune
  README.md
```

`zulip.sh run` creates an isolated Compose project, waits for migrations and API
readiness, seeds fixtures, runs the OCaml suite, captures failure logs, and tears
down its own resources. `up` keeps a server available for interactive work;
`down --purge` removes that run's data. Follow Matrix's unique run IDs, ownership
checks, signal cleanup and configurable ports. Bind the server only to loopback;
keep databases and queues on the internal Docker network. Local HTTP uses an
explicit test-only allowance in the client, with the server's canonical host and
scheme configured consistently. Test production TLS separately using a local CA.

Bootstrap must require neither browser interaction nor external accounts. Use a
version-pinned Django management script inside the container to create a realm,
an administrator, two ordinary users, two bots, public/private channels, topics
and subscriptions, then emit temporary API credentials and IDs to a JSON fixture
file. Use the server's creation helpers rather than direct SQL, to preserve
required invariants. Resolve and prove those helpers against the selected image
as the first integration task; the production image is not assumed to include
development `populate_db` tooling. Configure local file uploads and a local-only
mail sink/backend so fixture setup does not need SMTP, S3 or another service.

The startup sequence follows the official
[Compose initialization procedure](https://zulip.readthedocs.io/projects/docker/en/latest/how-to/compose-getting-started.html),
with scripted seeding replacing browser-based organization creation. If the
production bootstrap proves disproportionately difficult, build a cached test
image from a pinned server revision using the Python CI provisioning approach.
That is a fallback requiring explicit implementation, not an already-available
OCaml harness.

Use the OCaml client through the actual Fetch/httpz backend for assertions.
Optionally use the Python client to seed/read a few cross-client fixtures:
Python sends, OCaml receives; OCaml sends, Python checks. This helps catch a
matching bug in both the OCaml encoder and decoder. Python/server internals are
test dependencies only.

Required live scenarios include:

- Authentication and invalid keys; channel/topic send and fetch; Unicode and
  reserved characters; one-to-one and group-DM replies; edits, deletions,
  reactions and message pagination.
- Subscribe/unsubscribe and private-channel access; bot mention filtering,
  command/help composition, ignored own/bot messages and independent rooms.
- Register before sending, verify delivery and cursors, explicitly delete a
  queue to force deterministic recovery, and stop during an outstanding poll.
  A server restart scenario supplements queue deletion; it does not assume all
  server restarts invalidate queues in the same way.
- Upload/download byte equality, custom emoji, bot storage set/get/remove and
  separation of plugin namespaces. Failed writes must not appear committed.
- Plugin persistence and clean shutdown; restart with a fresh queue without
  replaying old events, while restoring stored plugin values.
- Webhook envelope/token validation and shared handler behavior, with HTTP
  delivery exercised through a local fixture server when that adapter is wired.

Keep rate-limit timings, ambiguous sends, malformed bodies, retry exhaustion,
response cleanup, queue saturation and exact cancellation races in deterministic
mock tests. Real-server tests should not depend on sleeps or server rate-limit
defaults to manufacture those cases. Wait for observable conditions with
deadlines.

Ordinary `dune runtest` remains offline. Provide a deliberate `@integration`
alias and `test/integration/zulip.sh run` as the self-contained command; a
requested live run must fail clearly when Docker/setup is unavailable instead
of reporting a skipped suite as success. CI runs the same wrapper on a
Docker-capable runner. Once images/dependencies are cached, test execution uses
only local services; initial pulls/builds still require downloads. Test one
pinned current server version and update that pin deliberately as Zulip releases
advance. Startup time and memory use must be measured,
since this is a larger stack than Matrix's single Synapse test container.

## Delivery order and completion checks

| Phase | Deliverable | Exit check |
| --- | --- | --- |
| 1 | Existing/Python SDK endpoint inventory, module signatures, dependencies and JSON profile schema | SDK gaps enumerated; Fetch/Jsont/Xdge build spike succeeds |
| 2 | Docker harness and scripted fixture bootstrap | Fresh local server produces credentials and passes a Python/API smoke probe |
| 3 | Protocol codecs, Fetch client, errors, forms and basic endpoints | Offline encoding/error tests plus OCaml send/fetch against local server |
| 4 | Remaining endpoint families, Python SDK gaps and uploads/storage | Coverage table complete; relevant mock and live scenarios pass |
| 5 | Queue state machine and ordered bot runtime | Recovery, dispatch, send outcomes, backpressure and cancellation tests pass |
| 6 | Plugin API, profiles, CLI, examples and documentation | Ping/help/counter and echo examples run locally; package build and docs succeed |

Introduce tests with the implementation they validate. Use official/Python
fixtures for schemas, request-capture tests for real wire encoding, and
`Fetch.mock` responses/flows for failure behavior. Particularly cover unknown
fields/events, optional/null fields, nested form objects, integer serialization,
HTTP error pages, 429 metadata, response release and never retrying ambiguous
message sends.

Port the echo and regression examples to the new bot interface. Replace the
mock feed example with a clearly documented example or a functioning Fetch/feed
parser implementation if feed support is wanted; do not advertise a fake feed
fetcher as a working feature. Add `Main.run`, `run_once` and plugin composition
examples matching Matrix's usage.

Completion requires building/installing the Zulip libraries without Requests or
nox dependencies, passing the offline suite and fresh-container integration
suite, compiling documented examples, and updating README/API docs and generated
opam metadata. No compatibility shim is required. A reviewable sequence of
commits can follow the phases above; intermediate migration builds need not
preserve the old public interface.
