# Hosted bots and Python SDK parity

This plan follows the completed Fetch/Jsont/Xdge port and the deeper API audit.
The first deployment target is a bot on `https://eeg.zulipchat.com`. Development
and mutation tests use the local Zulip 12.2 Docker stack. No hosted credentials
are needed to implement or validate the library.

## Completion target

The default completion target is full functional coverage of the reference
Python `zulip.Client`: its 71 endpoint helpers, six transport or callback
helpers, and the useful constructor configuration capabilities. Coverage includes
documented request parameters and accessible response data, not just function
names. Python's arbitrary request dictionaries become typed OCaml arguments and
records, with explicit extension access where the server may add fields.

The reference is `../python-zulip-api` at `b1723475`. Server wire contracts are
checked against the official 12.2 OpenAPI schema and route definitions, and
current upstream documentation for newly documented behavior. The schema
contains documentation-only pseudo-paths, so endpoint counts exclude those.

The implementation target is Python Client parity plus the bot-oriented
operations below. Adding every remaining REST endpoint, including mobile push
and video-provider APIs, is a separate, larger completion target.
`API_COVERAGE.md` records the covered Python helpers and the REST families
outside this target.

Maintain the agreed choices: current Zulip only, live event delivery, local
persistent plugin state, XDG JSON profiles, no backwards-compatibility shims,
and a Matrix-like `spec -> spec` bot plugin interface. Durable inboxes, offline
history replay, exactly-once effects, and the eventual shared Matrix/Zulip
library remain outside this implementation.

## OCaml API conventions

- Keep pure protocol values and Jsont codecs in `zulip`; I/O belongs in
  `zulip.eio`, with bot and CLI layers above it.
- Use distinct IDs, variants for closed choices, records for grouped options,
  and optional arguments for small independent options. Use explicit variants
  for unchanged/clear/set when a parameter distinguishes omission from null.
- Keep convenient send helpers returning message IDs, and provide detailed
  response operations when an endpoint also returns visibility changes or
  other useful metadata. Do not silently discard information without an
  accessible detailed/raw response.
- Decode core response fields with Jsont. Preserve unmodeled extension fields
  in responses and events. Raw JSON must be labeled as an extension boundary,
  not claimed as a complete typed model.
- Keep `Client.request` as the generic endpoint escape hatch, with form/query
  encoding, structured errors, explicit JSON-valued parameters and multipart
  support. Preserve the distinction between successful HTTP delivery and a
  Zulip API error.
- Let Fetch own TLS and HTTP customization. Expose request identity and retry
  configuration through ordinary OCaml parameters/capabilities; do not copy
  Python session internals or reproduce every Python configuration spelling.
- Replace obsolete routes with modern equivalents or remove them. The pointer
  GET/POST, GET settings, GET muted-users, and subgroup-by-ID lookup helpers
  must not remain advertised as working server endpoints.

## Phase 1: hosted-bot foundation

### Connection and configuration

Keep Basic credentials scoped to the configured HTTPS origin, system TLS trust
by default, and streamed file downloads with credentials removed on redirects
to external storage. Expose a caller-selected user-agent and transport retry
configuration. Retain explicit custom Fetch/TLS injection for custom CAs and
client certificates. Validate configured deadlines and bound response bodies.

Profiles retain atomic private JSON files and standard zuliprc import. Add a
deliberate local-HTTP option to the context/CLI for the Docker server, leaving
HTTPS as the normal hosted setup. Make startup and terminal authentication
failures visible, and ensure terminal collector failures lead to a failing CLI
exit rather than an apparently successful bot shutdown.

### Queue registration and initial state

Expose the registration controls supported by Python and Zulip: event types,
fetch-event types, narrows, Markdown/gravatar selection, subscriber inclusion,
presence options, client capabilities, and idle timeout. The low-level API must
not hardcode a realm-only initial snapshot. The bot runner can choose a small
appropriate snapshot without constraining other consumers.

Provide typed access to bot-relevant initial state: current user identity,
users, subscribed channels, topic preferences, muted users, presence and
settings. Preserve the complete snapshot and let callers decode additional
fields using their own Jsont codecs. Missing snapshot families are different
from present-but-empty ones.

Provide a low-level callback collector corresponding to Python's event/message
loops, independently of the opinionated bot activation filters. It must expose
recovery, skip protocol heartbeats, acknowledge only accepted batches, preserve
cancellation, and unregister on exit.

### Event and bot behavior

Add typed payload views for messages, edits, deletions, reactions, message flags,
users, channels/subscriptions, user status, topic preferences, user groups and
presence. Preserve unknown event kinds and extension members. Malformed known
payloads must remain distinguishable from unknown future events.

Use registration state and subsequent user events to maintain bot identity and
bot-user classification. A renamed bot must still recognize ID-bearing
mentions. Preserve channel topics and complete group-DM recipients; a channel
remains one ordered room across its topics. Avoid exposing stale message
destinations after moves or processing external dispatch after shutdown.

Registration and polling retries must honor server Retry-After information.
Queue expiry re-registers and reports the live-delivery gap. Terminal auth,
configuration and malformed queue responses stop cleanly. Keep bounded workers,
per-room/global queues, and cancellation-aware backpressure.

Queued sends retry explicit rate-limit rejections only. Preserve indeterminate
outcomes when a response is lost after a possible write. Shutdown settles all
send handles; timeout is not cancellation. Plugin writes remain atomic and
serialized within one process, with failed writes leaving old state intact.

## Phase 2: hosted-bot endpoint coverage

### Messages and files

Complete message query anchors, ID selection, Markdown/gravatar/empty-topic
controls; edit conflict checks and returned detached uploads; flag update
results; detailed send results; typed history and attachments. Add flags by
narrow, read receipts, reporting, temporary file URLs and thumbnail status.
Complete scheduled-message creation/editing as well as list/delete, and add
reminders and drafts where useful to bot applications. Preserve request and
response extension access throughout.

### Channels, topics and permissions

Complete channel creation/update/subscription options, including group-based
permissions, archive/unarchive behavior, folders and topic policies. Add the
dedicated channel creation endpoint and richer subscription operations. Expose
all topic visibility choices rather than a mute/unmute-only API. Make channel
and subscription response models retain documented fields and extensions.

Complete user-group settings and membership/subgroup query controls. Implement
subgroup membership using supported collection operations, or remove the
incorrect endpoint. Shared group-setting codecs should express named groups
and direct member/subgroup settings consistently across endpoint families.

### Users, presence and organization metadata

Complete user query/update/create parameters and returned IDs, typed status and
attachments, profile data, settings access through registration, and presence
delta responses. Remove parameters absent from the reference server instead of
assuming they have an effect. Complete linkifier, custom emoji and custom
profile-field request/response options from the Python helpers. Keep bot storage
set/get/remove semantically correct, including empty versus absent keys.

## Phase 3: remaining Python capabilities and coverage proof

Audit each Python helper against the actual OCaml entry point, every documented
request parameter, useful success data, and failure behavior. Include callbacks,
multipart upload, arbitrary endpoint access, request deadlines, custom HTTP/TLS
configuration, and the file-like message-sending convenience through normal
OCaml send functions. Python alias methods map to one canonical OCaml function.

Replace the name-only inventory with a machine-readable mapping plus an offline
audit that detects missing Python helpers, stale route/method combinations and
unaccounted parameters. Freeze the small schema inventory needed by the audit;
ordinary tests must not download OpenAPI or require a sibling Python checkout.
Distinguish native typed parameters, explicit extensions, intentionally replaced
legacy capabilities, and operations still missing.

Do not claim full parity from a generic raw escape hatch alone. Core documented
fields must have usable types; complete original response data must remain
accessible. Do not describe endpoint reachability as test coverage. Update
`API_COVERAGE.md` with real checks and any remaining limitations.

If the expanded REST target is selected, add the remaining invitations,
organization/domain/export administration, saved snippets/navigation views,
API-key/bootstrap authentication, device/push and video-call operations in
small coherent modules following these same conventions. Count all documented
operations and explicitly account for obsolete aliases and specialist endpoints.

## Validation and delivery

Offline checks cover request encoding (including nested JSON and clear/set
semantics), schema decoding/nullability, lost-response outcomes, Retry-After,
queue expiry, terminal errors, cancellation, backpressure, renamed mentions,
state updates, file permissions and failed storage writes. Each implementation
agent supplies focused tests for its endpoint family.

Extend Docker fixtures and scenarios for channel creation and permissions,
groups/subgroups, topic policies, read flags/history, scheduled messages/drafts,
profile fields/presence, and actual tutorial CLI operation with local profiles.
Exercise a server restart during collection, reconnection and fresh-queue
registration; assert observable outcomes with deadlines rather than fixed
sleeps. Run one final fresh-stack lifecycle with automatic cleanup.

Keep tutorial programs small while adapting changed APIs. Add a hosted-bot
guide covering credentials, channel access, mention activation, profile/state
paths, supervision and restart semantics, a read-only account preflight and a
deliberate test-channel smoke sequence. Actual hosted deployment remains a
separate operation requiring its credentials and intended destination.

Final checks: library and example build, generated API docs/opam metadata,
offline suite and parity audit, expanded Docker suite, formatting, diff review,
and accurate coverage documentation. Preserve the user's uncommitted example
work and `lib/zulip/narrow.mli` edit, and never edit the linked httpz checkout.

## Implementation ownership

After planning, the coordinating agent owns shared transport, queue and bot
runtime, configuration/CLI, documentation, parity audit and integration testing.
Smaller agents independently implement (1) messages/files, (2) channels/groups,
and (3) users/presence/server metadata. Agents own disjoint source and test files;
shared exports and build files are integrated by the coordinator. Only one Dune
process runs in this shared workspace at a time.

## Completed implementation and validation

Implemented and validated on 2026-09-06. The frozen inventory maps all 77
Python helpers to OCaml operations or explicit OCaml replacements, covering
68 canonical REST operations and accounting for their 294 wire parameters.
The audit checks structural coverage; request/response tests and live scenarios
provide behavioral evidence. Response extensions remain accessible without
claiming that every server field has a dedicated OCaml type.

The hosted foundation includes registration state and typed event views,
identity refresh, queue recovery and shutdown, configurable Fetch transport,
private profiles and plugin state, and a working CLI preflight. Endpoint work
adds the planned message/file, channel/group/settings, and account/server
families. The three endpoint families were implemented by smaller coordinated
agents; shared runtime work and integration remained with the coordinator.

Validation passed: 86 offline Alcotest cases, the frozen parity audit, library
and tutorial builds, generated API documentation, opam lint, formatting and
diff checks. A fresh Zulip 12.2 Docker run passed all 18 API/runtime scenarios
in 36.1 seconds after startup, plus the separate tutorial CLI check for profile
import, preflight, HTTP opt-in, live echo, SIGTERM, and invalid credentials.

See [the coverage inventory](API_COVERAGE.md), [hosted deployment guide](HOSTED_BOTS.md),
[tutorials](example/README.md), and [local test instructions](test/integration/README.md).
Deployment to `eeg.zulipchat.com` remains a separate step using the intended
bot account and channels.

The saved-snippet follow-up adds all four operations and a runnable recipe.
The expanded validation passed 91 offline cases and 19 live API/runtime
scenarios plus the tutorial CLI check. Anonymous Fetch/httpz reads also decoded
six public messages from EEG's `Blogs` and `Tessera` channels. No hosted
credentials or mutations were used.
