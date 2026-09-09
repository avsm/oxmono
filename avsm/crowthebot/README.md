# Crowthebot

Crow is a Matrix assistant using the Eio-native OpenRouter client over Fetch.
Each named profile has a separate Matrix device, configuration, whitelist and
SQLite conversation store. Only the configured primary admin can grant access.

## Build and configure

From the monorepo root:

```sh
opam exec --switch=5.2.0+ox -- dune build --profile release-check @avsm/crowthebot/all
alias crowthebot="$PWD/_build/default/avsm/crowthebot/bin/main.exe"

crowthebot init --profile crow-home \
  --admin '@you:example.org' --homeserver https://matrix.example.org
crowthebot login --profile crow-home --username '@crow:example.org'
crowthebot join --profile crow-home '#your-room:example.org'
crowthebot probe --profile crow-home
crowthebot run --profile crow-home
```

Use a dedicated Matrix account for Crow, different from the primary admin.
Create that account using your homeserver's normal registration flow. Supply
its full `@user:server` ID to `--username`. `login`
prompts for its password without echo. For automation, use `--password-file`
with a file owned by you and mode 0600. Session tokens and encryption keys are
saved, so the password is unnecessary on later runs. SSO-only login is not
implemented in this CLI.

Profiles live in `$XDG_DATA_HOME/matrix/profiles/NAME`, defaulting through XDG
to `~/.local/share/matrix/profiles/NAME`. Use another `--profile` for another
bot and run the processes independently. Each profile directory must be owned
by you with mode 0700. A process lock prevents simultaneous Crow processes
from sharing a profile. Do not use the same Matrix profile in another client
while Crow runs.

Edit `crowthebot.json` while the bot is stopped. The defaults are:

| Setting | Default |
| --- | --- |
| `base_url` | `http://sequoia.cl.cam.ac.uk:8000/v1` |
| `model` | `Qwen/Qwen3.8-27B-FP8` |
| `plugins` | `[]` |
| `context_messages` | 20 |
| `context_bytes` | 40000 |
| `max_tokens` | 1024 |

`system_prompt` controls Crow's personality. Set `plugins` to `[]` to disable
compiled extension plugins. Shared memory, cron, feeds and location tools are
built in for the admin and allowed friends. Choose a model in this file and
configure its named endpoint and key with `config openrouter`, as below.
`--api-key-file` remains an explicit override for `probe`, `run` and
`note --generate`, using `base_url` from the profile. That private file must be
owned by you with mode 0600, and its bearer key requires HTTPS. Without named
model configuration or an override, Crow uses the profile endpoint without a
key. Keys are never read from ambient API-key environment variables.

The primary admin is stored in SQLite at initialization. Changing `admin` in
JSON alone is rejected. Create a fresh profile to choose another authority.
Rooms are enabled only by a successful `join`, which resolves an alias to a
stable room ID. Direct-message invitations from the admin or an allowed account
are accepted automatically. Other invitations require an explicit `join`.
Joining another room does not share conversation context with it.

## Named tool configuration and secrets

The local `config` subcommand maintains per-tool named settings under
`$XDG_CONFIG_HOME/crowthebot/secrets/PROFILE`, defaulting to
`~/.config/crowthebot/secrets/PROFILE`. Xdge resolves this area independently of
the Matrix data profile. `CROWTHEBOT_CONFIG_DIR` overrides the `crowthebot`
config directory. The config area must be outside every Matrix profile's data
directory, including its file-tool workspace.

```sh
crowthebot config openrouter add personal --profile crow-home \
  --api-key-file /path/to/openrouter-key
crowthebot config openrouter add work --profile crow-home \
  --url https://model.example/v1 --api-key-file /path/to/work-key
crowthebot config openrouter list --profile crow-home
crowthebot config openrouter select work --profile crow-home
crowthebot config openrouter set work --profile crow-home \
  --url https://model.example/v1 --api-key-file /path/to/replacement-key
crowthebot config openrouter rename work office --profile crow-home
crowthebot config openrouter remove personal --profile crow-home
```

`add` refuses an existing name. `set` writes a complete replacement or creates
a missing name. The first entry is selected automatically. `select` changes
the default. Removing the selected entry clears that selection. If other model
entries remain, explicitly select one before restarting. Renaming preserves
the selection. Names contain 1 to 64 ASCII letters, digits, `-` or `_`.
`list` prints names and selection only. There is no command that prints keys.

Secrets come from an owned 0600 file or a terminal prompt with echo disabled.
Omit `--api-key-file` or `--password-file` to prompt. Secret values are never
command-line arguments. Named endpoints require HTTPS unless the operator
passes `--allow-http` for that connection. A named key is bound to its saved
endpoint, independently of the profile's `base_url`.

Configuration is plaintext in owned 0600 files inside 0700 secret directories.
Symlinks and permissive files are rejected. Updates use an exclusive lock and
atomic file replacement. The configuration commands can run while Crow is
serving. Restart Crow to apply additions, rotations, removals or selection
changes. Running clients retain the credentials loaded at startup.

Configuration commands do not call a model or enter the tool-use database.
Only trusted startup code receives the secret-store capability. It initializes
authenticated clients and passes narrowed capabilities to runtime tools. No
chat command, model tool, memory operation or file workspace can read or alter
the secret store. Matrix device/session credentials remain managed by the
Matrix profile store.

## Talk to Crow

From the primary admin's Matrix account, in an enabled room:

```text
!crow allow @alice:example.org friend
!crow allow @other-ai:example.org bot
!crow people
!crow deny @alice:example.org
!crow help
!crow ask Help me plan my afternoon
!crow feeds add https://example.org/feed.xml
!crow ask Follow the feeds listed in https://example.org/subscriptions.opml
!crow reset
```

`!crow TEXT` also asks the model directly. Only exact Matrix IDs establish
identity. Display names, room power levels, quoted messages and model output
cannot grant access. A `friend` is a human approved by the admin. A `bot` is an
AI account explicitly approved by the admin. In enabled group rooms, both can
use `!crow`, mention Crow using the client's mention picker, or address its
full account ID, for example `@crow:example.org: Help me plan my afternoon`.
Display-name text alone does not trigger Crow. A bare `!crow` or account ID
shows help. Existing commands also work after a leading account-ID mention.
Unknown participants remain silent until the admin checks their identity and
approves them. Membership events and attempted commands populate the unknown
list, capped at 1000 identities. `crowthebot people --profile NAME` inspects
that list locally while Crow is stopped.

In a DM, approved accounts can send ordinary text without a prefix. Commands
such as `help`, `reset` and the admin's `allow` also work without `!crow`.
Start a DM in your Matrix client and invite Crow. A profile can run with no
group rooms enabled. Accepted DM invitations are remembered across restarts.
Existing rooms tagged in Crow's `m.direct` account data also work as DMs.
Crow requires complete membership containing exactly itself and one other
account before enabling prefix-free replies. Adding another participant
disables DM handling. A DM does not become an enabled group room.

Crow ignores its own events, notices, edits, group conversation without an
address and groups not explicitly enabled. Legacy quoted reply fallbacks are
removed before interpreting a command. It sends `m.notice` replies, preventing
the usual bot-to-bot feedback loop. A person has separate context in each room. `reset`
clears their current thread. Revocation removes all their stored threads.
Model and plugin calls have a ten-second interval per person and room.
Requests are processed serially within a profile.

## Shared memory

The profile is the sharing boundary. The admin and allowed friends can store,
search, retrieve and erase any fact across its rooms and DMs:

```text
!crow memory store Alice prefers jasmine tea.
!crow memory search jasmine
!crow memory search "jasmine tea"
!crow memory get 1
!crow memory erase 1
!crow memory list
```

Facts have stable numeric IDs, UTC creation timestamps, the requesting Matrix
account, source room and event, and a `command` or `observation` source. Crow
can use native `memory_store`, `memory_search`, `memory_get` and `memory_erase`
tools during conversations and scheduled actions. It is instructed to remember
useful observations freely, search before duplicating a fact, and treat stored
facts as data. No per-fact confirmation is required. Unknown, revoked and
bot-classified accounts cannot use memory.

Search uses SQLite FTS5 with Unicode tokenization, quoted phrases, Boolean
operators and `prefix*` matching. A search returns up to 20 matches by relevance.
An empty search lists the newest 20 facts. Facts are limited to 2048 bytes and
queries to 256 bytes. Erasure removes the fact and its FTS entry and deletes
linked reminders. It does not rewrite conversation history, Matrix messages
or backups. `reset` clears the current conversation, leaving shared facts intact.

Local commands act as the admin while the profile is stopped:

```sh
crowthebot memory --profile crow-home store 'Alice prefers jasmine tea.'
crowthebot memory --profile crow-home search jasmine
crowthebot memory --profile crow-home get 1
crowthebot memory --profile crow-home erase 1
```

## Reminders and scheduled actions

Ask Crow naturally, for example, “Remind me tomorrow at 09:00 UTC to bring tea
to the meeting.” Crow can store the relevant fact and call `cron_create` with
its ID. Each reminder records its creator, source room, source event, creation
time, instruction and schedule. `cron_list` and `cron_cancel` manage the shared
reminders. The admin and friends can also use explicit commands:

```text
!crow memory store Bring jasmine tea to the meeting.
!crow cron create {"fact_id":1,"instruction":"Remind me to bring tea.","at":"2026-09-10T09:00:00Z"}
!crow cron create {"fact_id":1,"instruction":"Check preparations for the meeting.","cron":"0 9 * * 1-5","until":"2026-09-24T23:59:59Z"}
!crow cron create {"fact_id":1,"instruction":"Review our meeting preparations.","cron":"0 9 * * 1"}
!crow cron list
!crow cron cancel 2
```

Use `at` for a one-off RFC 3339 timestamp with a timezone. Use `cron` for five
UTC fields: minute, hour, day of month, month and weekday. Fields accept numbers,
`*`, ranges, comma-separated lists and steps such as `*/15` or `9-17/2`.
Sunday is 0 or 7. Restricted day-of-month and weekday fields use cron's OR rule.
`until` is an inclusive end time for recurrence. Omitting it recurs indefinitely.
Impossible schedules are rejected after an eight-year calendar search.

While `run` is active, Crow checks due reminders every ten seconds. A firing
calls the configured OpenRouter client with the linked fact, instruction,
schedule and original source. The model may use its authorized tools, and its
answer is sent as a notice replying to the source event. The source room and
creator's access are checked again before delivery. Scheduled model turns share
the conversation engine's serialization and 180-second deadline.

Occurrences are claimed in SQLite before effects. Restarting cannot replay a
claimed occurrence. A crash or failed delivery can lose that occurrence, which
remains recorded as interrupted or failed. Overdue one-off reminders fire on
restart. Recurrences coalesce missed intervals into one action and advance from
the current time. Jobs past `until` do not fire. Revoking a creator cancels their
reminders. Cancelling a job or erasing its linked fact stops subsequent actions
and suppresses an in-flight answer before delivery. Already completed tool
effects and queued Matrix sends cannot be undone.

## Tool log and daily notes

Every explicit, model-requested and scheduled tool use is recorded before
effects in `tool_uses`. Entries retain UTC start/end times, the requesting
account, room, event, source, call ID, tool name, bounded arguments and result,
and success, rejection, error or cancellation status. Unfinished records become
`interrupted` on restart. Logging is independent of Matrix reply delivery.
Local memory and feed operations are logged too.

Memory, cron and location payload text is omitted from the log so those
operations do not create another durable copy of facts or coordinates. Other arguments and
results are limited to 4096 bytes. Logs remain in the profile database without
automatic pruning. The admin and friends can inspect them with
`!crow tools [YYYY-MM-DD [AFTER_ID]]`, which returns up to 20 records. Locally,
`crowthebot tools --profile NAME --day YYYY-MM-DD --after ID` returns up to 100.

Crow checks for missing daily notes every minute while running. Each completed
UTC day gets a note in `daily_notes`, including days without tool activity.
Startup catches up from the day this feature was initialized. Notes use the
same OpenRouter client, endpoint and model as conversation turns. Logs are read
in bounded batches, every record is visited, and long payloads are excerpted.
The summary model has no tools. Only a completed summary replaces a note.
Failures retry later, and existing notes with the same source records are reused.

Read yesterday's note with `!crow note`, or use `!crow note YYYY-MM-DD`.
Local equivalents, while the profile is stopped, are:

```sh
crowthebot note --profile crow-home
crowthebot note --profile crow-home --day 2026-09-09 --generate
```

## Verify Crow in Matrix

Verification is an interactive terminal operation on the saved Matrix device.
Stop `crowthebot run` for this profile first. It uses the same exclusive profile
lock, account and device keys, and does not call the model.

For user verification across clients, first set up cross-signing and recovery
for Crow's account in a Matrix client such as Element. Save **Crow's account**
recovery key in a file owned by you with mode 0600. Then run:

```sh
crowthebot verify --profile crow-home --listen \
  --recovery-key-file /path/to/crow-recovery-key
```

In your own Matrix client, open Crow's profile and start verification. Choose
emoji comparison. Accept the named account and device in the terminal, compare
all seven emoji or all three numbers with your client, and answer `yes` only
when they match. Confirm in the Matrix client too. Crow imports its existing
cross-signing keys, signs its saved device, and publishes the peer's signature
after successful SAS verification. It never resets the account identity or
creates a replacement device. This supplies the cross-signing trust chain used
by clients to remove unverified-user warnings. See the
[Matrix cross-signing specification](https://spec.matrix.org/latest/client-server-api/#cross-signing).

The peer defaults to the primary admin. Supply a full Matrix ID to verify a
different user. Omit `--listen` to send the request from Crow, and use `--room`
with a joined DM's room ID for in-room verification:

```sh
crowthebot verify --profile crow-home \
  --recovery-key-file /path/to/crow-recovery-key \
  --room '!your-dm:example.org' '@you:example.org'
crowthebot run --profile crow-home
```

Without `--recovery-key-file`, verification covers individual devices and local
trust only. It may leave a client-wide user warning. Recovery secrets remain in
memory during the command. Device keys and local trust persist in the profile.
Blank answers, `no`, and EOF refuse confirmation. Mismatches, cancellation,
signature-publication failures and the ten-minute timeout fail the command.
QR scanning is not supported. Verification never grants Crow access: the admin
still uses `allow` to approve another account.

## Feed subscriptions

The admin and friends can follow RSS 1.0/2.0, Atom and OPML URLs by asking Crow
or using commands. Subscriptions are shared across the profile. Each records
its requesting account, room and event. Updates go to that source room or DM.
There is no built-in blogroll or automatic subscription. Old profiles with
`"blogroll"` in `plugins` load with that retired plugin removed.

```text
!crow feeds add https://example.org/feed.xml
!crow feeds add https://example.org/subscriptions.opml
!crow feeds add {"url":"https://example.org/atom.xml","cron":"*/30 * * * *","until":"2026-12-01T00:00:00Z"}
!crow feeds list
!crow feeds status 1
!crow feeds entries 1
!crow feeds poll 1
!crow feeds remove 1
```

Native tools are `feeds_add`, `feeds_list`, `feeds_status`, `feeds_entries`,
`feeds_poll` and `feeds_remove`. Add defaults to hourly polling (`0 * * * *`,
UTC), indefinitely. An optional `until` limits recurrence. Adding a URL again
in the same room returns its existing subscription. Remove and add it again
to change its schedule or owner. Bots and unapproved accounts cannot use feeds.

Adding a URL atomically registers the subscription and cron polling, then
fetches its root document. If the initial fetch fails, the subscription remains
registered for retry and its status shows the failure. An OPML document also
registers an independent cron job for each listed feed, inheriting its schedule
and source. Those jobs start on the next scheduler pass. OPML changes add or
remove jobs on later polls. Include outlines and recursively nested OPML feeds
are not expanded. Each OPML document may contain at most 1000 feed outlines.

The first successful fetch establishes a baseline and caches existing entries
without sending an old-post digest. Later polls stage unseen entries for a
scheduled OpenRouter turn, supplying titles, article links, excerpts and the
subscription's source. Quiet polls make no model call and send no message.
Delivery cursors advance only after Matrix confirms a send. A failed delivery
is retried on a later poll, even if the server returns 304. Overlapping
subscriptions share delivery progress within a room, so a directly followed
feed and the same feed in an OPML list do not both notify it. Different rooms
have independent delivery progress. A crash between a confirmed send and saving
its cursor can cause a repeated notification.

Feeds use conditional GET with saved ETag and Last-Modified values. Repeated
requests for the same document within 60 seconds reuse its cached state.
Failures preserve the last good cache and validators, with exponential backoff
from five minutes to one day. `feeds poll` refreshes the root and makes its
active imported-feed jobs due. It respects the cache and backoff. `feeds status`
shows individual jobs, their cron IDs and errors. `cron cancel ID` stops one
job. Removing a subscription cancels all its jobs and collects cache no other
subscription uses. Revoking its creator cancels polling.

List, status and entries commands return five records per page. Supply `after`
to native tools, or append the last displayed ID to the command, to continue:
`feeds entries 1 42`, `feeds status 1 12`, `feeds list 5`. Entry pages include
short excerpts. The cache keeps the 2000 most recently discovered entries per feed, with bounded
titles, article URLs and summaries. Compact entry-ID hashes remain until the
source is removed, preventing old entries from reappearing after cache pruning.
Atom IDs follow the [Atom specification](https://www.rfc-editor.org/rfc/rfc4287.html#section-4.2.6).
RSS entries without IDs or links use a content hash.

The feed client has GET-only access to public HTTP(S) addresses, no credentials
or cookies, a 30-second deadline and a 2 MiB document limit. It checks resolved
IP addresses before opening sockets, including redirect targets, and refuses
private, loopback and reserved address ranges. It follows at most three redirects
and refuses HTTPS downgrades. XML DTDs, custom entities and excessive nesting
are rejected. Article pages, images and enclosures are not fetched.

Local inspection, polling and removal work while the profile is stopped:

```sh
crowthebot feeds --profile crow-home list
crowthebot feeds --profile crow-home status 1
crowthebot feeds --profile crow-home entries 1
crowthebot feeds --profile crow-home poll 1
crowthebot feeds --profile crow-home remove 1
```

Add subscriptions from Matrix so their notification destination is explicit.
Automated polling and delivery require `crowthebot run`.

## OwnTracks locations

Configure an OwnTracks Recorder connection locally. Supply its base URL before
`api/0/`, along with HTTP Basic credentials, or explicitly use `--anonymous`.
This integration queries Recorder over HTTP and does not subscribe to MQTT.

```sh
crowthebot config owntracks add home --profile crow-home \
  --url https://recorder.example/ --username recorder-user \
  --password-file /path/to/recorder-password
crowthebot config owntracks list --profile crow-home
crowthebot config owntracks select home --profile crow-home
crowthebot run --profile crow-home
```

The admin and friends can ask Crow to discover trackers and attach a person:

```text
!crow Which OwnTracks users and devices are available on home?
!crow Attach Alice's location to user alice, device phone on home.
!crow Where did Alice last report her location?
!crow location sources
!crow location list
!crow location get Alice
!crow location detach Alice
```

Native tools are `location_sources`, `location_devices`, `location_attach`,
`location_get`, `location_list` and `location_detach`. Person labels may be
names or Matrix IDs. They are links to trackers, not proof of identity or
authority. All links and positions are shared across this profile's admin and
friends. Bots, unknown accounts and revoked accounts cannot access them.

Each link records the connection name, OwnTracks user/device, requesting
account, room, event and attachment time. Attaching again replaces the link and
clears its previous fix. `location_get` refreshes by default. Set `refresh` to
false for a cached read. Positions contain latitude, longitude, UTC report time
and accuracy in metres when available. A successful poll with no newer fix
retains the last reported position. A failed poll returns a labelled cached
result and a sanitized error. Crow is instructed to qualify older positions
and retain their timestamps.

Queries cover the preceding seven days by default. Configure
`--lookback-days` from 1 to 31 to change this bound. Only the latest valid fix
is retained. Out-of-range coordinates, invalid accuracy, old fixes outside
the query window and timestamps more than five minutes ahead are discarded.
Each request has a 20-second deadline and a 1 MiB response limit. The initialized
client permits GET requests only to the configured Recorder's list and location
endpoints. It cannot redirect to another origin or use the feed transport's
credentials. Private Recorder addresses are allowed because the operator
chooses the endpoint locally.

List and discovery tools return bounded JSON pages. Pass `next_after` as
`after` for the next page, retaining the same connection and user. A null
`next_after` marks the end. Detaching deletes the typed link and cached fix.
It does not rewrite existing Matrix messages, conversation history, separately
saved memory facts or backups. Renaming a connection leaves existing person
links using the old name. Reattach them to the new name after restarting.

Scheduled model turns can use the same location tools. Ask Crow to create a
memory-linked reminder such as checking Alice's last reported position each
morning. Cron retains the request source and checks the creator's authority
before tools and delivery.

## Structured tool state and capabilities

Tools own typed tables in the profile's SQLite database. Conversational memory
holds selected observations. Operational caches and cursors belong to the tool
that maintains them. The model uses validated operations and stable record IDs
instead of writing SQL or treating memory facts as mutable cache records.

| Feed state | Purpose |
| --- | --- |
| `feeds_subscriptions` | Owner, source room/event, schedule and root URL reference |
| `feeds_sources` | Shared document URLs, format, validators, poll times and errors |
| `feeds_memberships`, `feeds_outlines` | OPML membership, cron-job references and delivery cursors |
| `feeds_entries`, `feeds_seen` | Bounded entry cache and durable deduplication keys |
| `tool_schemas` | Per-tool schema versions |
| `locations_people` | Person/device links, source provenance and latest reported position |

[`Feed_store`](lib/feed_store.mli) owns these invariants and rechecks authority
on each operation. It shares the database transaction lock with `Store`, so
subscriptions, memberships and cron jobs change atomically. Cron targets can
reference either a memory fact or a tool namespace and record ID. Feed polling
uses the `feeds` namespace. Future stateful tools should follow this pattern:
own their schema and migrations, expose typed operations bound to the request,
and give the scheduler stable references. Feed caches never enter the facts FTS
index. Crow can explicitly remember a useful observation from a feed.

[`Location_store`](lib/location_store.mli) follows the same pattern for
person/device links and positions. Its schema is versioned independently and
uses the shared authority lock. Coordinates are operational tool state and do
not enter memory or its FTS index automatically.

An integration needing credentials exports a [`Tool_config.t`](lib/tool_config.mli)
with its name and a Cmdliner term for complete settings. The CLI composes these
terms with the common named-entry operations. `Locations.configuration` and
`Model_config.configuration` supply the current integrations. Adding one does
not require teaching the conversational engine how to read secrets.

[`Plugin.t`](lib/plugin.mli) is a compiled-in extension with a name,
description and `run ~query` function. Register it when constructing
[`Engine.t`](lib/engine.mli), then enable its name in the profile. The engine
exposes each plugin both as an explicit command and a native model tool with a
string query argument. Tool arguments and outputs are bounded. A turn permits
at most three tool calls. Context and plugin output remain untrusted model
input. Tool results do not become administrative commands.

Pass each plugin only the capabilities it requires. The engine does not hand plugins the
Matrix client, SQLite authority store, Eio environment or filesystem root.
Compiled plugin code is trusted and must be reviewed before registration.

The tool boundaries are:

| Component | Capabilities supplied |
| --- | --- |
| Feed tools | Typed feed-state operations and bounded GET callback |
| Feed HTTP transport | Network and clock, with resolved public-address checks |
| Location tools | Typed location state and initialized named Recorder capabilities |
| Recorder client | Scoped GET transport and clocks, with no filesystem or secret-store capability |
| Operator configuration | Xdge config directory and secret input, available only outside chat |
| Memory tools | Four operations bound to the current actor and source |
| Cron tools | Create, list and cancel operations bound to the current actor and source |
| Daily summarizer | Log/note store and a completion callback with a deadline |
| File plugin factory | Eio cwd confined to the profile's private `workspace/` |

Memory, cron, feed and location operations recheck authority in the store, including after an
account is revoked. Tools receive no general database handle or Matrix client.
Clock and model callbacks capture their specific capabilities without retaining
the full Eio environment. Current tools need no filesystem capability.

For a compiled plugin that needs files, construct it with
[`Plugin.with_workspace`](lib/plugin.mli). Its cwd rejects absolute paths,
parent traversal and symlink escapes. The factory also rejects a workspace
symlink that aliases the profile's credentials. Use Eio paths within that cwd.
Native filesystem APIs and process-wide `chdir` bypass this capability model
and are not permitted in such plugins. This is an Eio capability boundary,
not an OS sandbox for untrusted native code.

## Persistence and security limits

`crowthebot.sqlite3` stores authority, enabled rooms, event IDs, bounded
conversation context, accepted DMs, shared facts and their FTS index, tool logs,
daily notes, reminders, occurrence records and typed feed/location state. SQL inputs use bound
parameters. Schema version 5 is recorded, versions 1 through 4 are upgraded
atomically, and unknown versions are refused. Configuration and database files
are created 0600. The native SQLite path and process-lock operations are the
filesystem boundary around an otherwise Eio-based runtime.

Conversation context is stored as plaintext locally, including replies from
encrypted rooms. Accepted prompts and tool results are sent to the configured
model endpoint. Encryption protects Matrix transport, not model inference or
local backups. Authorization verifies a Matrix account ID reported by the
homeserver. It is separate from Matrix device verification and cross-signing.
Completing `verify` does not change the access list.

The model receives recent context within both configured bounds, plus the
system prompt, current request and bounded tool exchange. These are byte and
message limits, not tokenizer-exact limits. Input is capped at 8192 bytes or
half the configured context budget, whichever is smaller. Answers are capped
at 12000 bytes. A model request has a 90-second deadline and a whole turn has
a 180-second deadline, both measured with a monotonic clock.

Event IDs are claimed before effects. The last 2048 per room are retained.
This suppresses retries but means a crash during a turn can lose its reply.
Context advances only after Matrix confirms delivery. A timed-out queued send
can still complete later. Crow skips startup backlog and does not persist the
Matrix event cache, limiting local storage of unrelated room messages.
Failures are logged without prompt text, tokens or raw provider responses.

## Unfinished features

- SSO/OAuth account setup, cross-signing identity creation, QR verification and
  room-key recovery are not exposed by the CLI. Existing cross-signing secrets
  can be imported for SAS verification with `--recovery-key-file`.
- There is no room removal command yet. Stop the profile before changing its
  enabled-room database or create a fresh profile for a different room set.
- Context has no summarization, vector retrieval, tokenizer accounting,
  redaction handling or retention timer. Matrix deletions do not erase saved
  context. `reset` and `deny` remove active rows, not backups.
- No image, audio or file ingestion, shell, browser or remote
  execution plugins are registered. OpenRouter supports vision separately.
- Plugins are compiled into the application. There is no dynamic loading,
  subprocess protocol or interactive approval workflow for future write tools.
- Replies use plain `m.notice` bodies. Streaming display and Markdown-to-Matrix
  formatting remain future work.
- Administrative changes wait behind earlier events in the room. The initial
  implementation favors ordered processing over parallel model turns.
- Matrix room delivery has not been tested against a live account for this
  application. Use a dedicated test room for the first configured run.

## Verification

```sh
opam exec --switch=5.2.0+ox -- dune runtest --force --profile release-check \
  avsm/crowthebot avsm/sortal
crowthebot feeds --profile crow-home list
crowthebot probe --profile crow-home
```

Offline tests exercise authority, roles, denied commands, replay suppression,
rate limits, revocation, context isolation and bounds, failed delivery,
mentions, DM addressing, schema migration, SAS comparison, cross-signing,
refusal and mismatch through two simulated Matrix clients,
model tools, forged permission changes, tool-call budgets, cancellation
recovery, HTTP status handling, feed caching, XML limits, profile isolation,
file permissions and process locking. A native model-adapter regression checks
that empty tool arrays are omitted for Sequoia. Further tests cover shared
memory and Unicode FTS, erase/index consistency, tool failures and cancellation,
restart recovery, daily-note batching and retries, cron calendar rules and
source context, scheduled model tools, revocation during actions, and confined
file-tool paths and symlinks. Feed tests cover RSS/Atom/OPML, conditional polling,
deduplication, overlapping subscriptions, delivery retries, migrations, revocation
and public-address enforcement. Model calls in these tests use Fetch mocks.
OwnTracks tests cover named connections, scoped credentials, bounded queries,
latest-fix selection, stale caches, redacted failures, revocation during lookup,
model tool calls and location erasure. Configuration tests cover named CRUD,
profile isolation, file modes, symlinks, endpoint scoping, locks and CLI help
without secret input. No live Recorder or Matrix account is contacted.

The opt-in probe below uses synthetic identities, an in-memory database and a
printing delivery callback. It sends a prompt to Sequoia and permits the model
to list feed subscriptions. It never connects to Matrix, fetches feeds or reads
an existing profile. This opt-in probe has not been rerun for the feed changes.

```sh
opam exec --switch=5.2.0+ox -- dune exec --profile release-check \
  avsm/crowthebot/test/live/probe.exe
```
