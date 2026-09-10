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

Tool activity is logged to stderr by default, including model calls to tools,
local memory commands and scheduled polling. Each invocation logs its start,
source, actor, room, event, database log ID, outcome and argument/result sizes.
Memory operations add record IDs, match counts and whether a record was found
or erased. Calendar syncs report mirror IDs, discovered calendars, batch sizes,
saved/reused/removed counts, pending work and cursor resets.

Use `run --verbose` (or `-v`) to also log startup, Matrix sync, DM membership,
access decisions, model requests and reply delivery.
`probe --verbose` logs the model and CalDAV checks. Logs omit message bodies,
tool arguments and results, credentials and server response bodies.
When a model turn fails to synthesize a reply, its error log includes the
ordered tool sequence with these metadata, model round and remaining budget.
Model completions also log the finish reason. Location contents and coordinates
remain absent from terminal logs.

```sh
dune exec -- crowthebot run --profile crow-one --verbose
```

For an unanswered DM, wait for live sync and the room's `Watching room` log,
then send a fresh message. Crow skips the initial timeline, including messages sent while
it was stopped. `encrypted_pending` counts incoming messages that cannot reach
Crow until Matrix room keys arrive. `Received text` confirms decryption and
delivery to the handler. An `Ignored` line explains a routing or access check.
After `Accepted`, look for `Model request completed` and `Reply sent`.
`probe` checks the model and all configured CalDAV connections. It does not
confirm Matrix delivery.
For a pending admin DM invitation, startup should log the admin's Matrix ID as
`inviter`, followed by `Accepted invitation` and a `Watching room` entry for
the DM. Invitations use the sender in Matrix's stripped membership state.

To test one calendar connection without calling OpenRouter:

```sh
crowthebot probe --profile crow-one --caldav fastmail --verbose
```

Stop the profile before probing. CalDAV checks authentication, discovery, the
first sync report or ETag inventory for each calendar, and at most one sample
entry per calendar. It reports counts and safe error categories. Calendar
contents are neither printed nor sent to OpenRouter. The check creates no
mirror or polling job and does not advance saved sync cursors. A failure in
one connection does not prevent checking the others. Any failed check gives
exit status 1. Use `--model-only` to check only the model.

Replies include sanitized Matrix HTML rendered from Markdown, with the original
text as a fallback. Emphasis, lists, code blocks, links and tables render in
Matrix clients. Headings become bold paragraphs. Scheduled replies use the
same renderer.

## Live inspection and model provenance

External tools can inspect a running profile through a read-only JSON interface:

```sh
crowthebot inspect --profile crow-one
crowthebot inspect --profile crow-one --section runs
crowthebot inspect --profile crow-one --section tools --after 0 --limit 20
crowthebot inspect --profile crow-one --section traces --after 0 --limit 1
crowthebot inspect --profile crow-one --section context
crowthebot inspect --profile crow-one --section feeds
```

The default `pending` section lists outstanding reminders, including their
schedule, source and memory or tool reference. Every page includes outstanding
counts and `next_after`, which is null at the end. Pass a returned cursor with
`--after` for the next page. Page sizes range from 1 to 100. Other sections are
`reminders`, `runs`, `tools`, `traces`, `context`, `summaries`, `feeds`, `subscriptions`, `polls`,
`locations`, `memory` and `notes`. These local commands do not acquire the bot's
process lock, migrate its database or change the status of running jobs.
The database uses SQLite WAL snapshots so inspection can overlap model and
tool writes.

Every OpenRouter HTTP exchange from `run`, `probe` and `note --generate` is
recorded in the profile's private `crowthebot.sqlite3`, in `model_traces`.
Request and response fields contain the original full JSON text, including
conversation history, reasoning, tool calls and tool results. Each exchange has
an ID, timestamps, HTTP status, completion status and source account/room/event.
Scheduled exchanges retain both their synthetic cron event and original source
event. Daily notes carry their day and `daily-note` source. Concurrent requests
keep separate provenance. Terminal logs include the corresponding trace IDs.
Conversation summaries use the `context-compaction` source. Their traces retain
the previous summary, covered messages, event IDs and model response.

Requests are recorded before sending. HTTP error bodies are retained.
Transport failures retain any received response prefix. Responses exceeding
the existing 1 MiB bound are marked `response-too-large` with their first
1 MiB. An exchange interrupted by a process crash is marked `interrupted` on
restart. Trace status describes the HTTP exchange; an HTTP success can still
contain a model response the agent rejects.

Authentication headers and endpoint URLs are excluded. The trace retains full
conversation and tool content, including facts later erased from memory.
Traces remain until removed by the local operator and are not exposed as chat
tools. The SQLite file and profile directory retain their private permissions.
An external program can also open the same file using SQLite's read-only mode
and query `model_traces`, `tool_uses`, `reminders`, `reminder_runs` and the
tool-owned `feeds_*` and `locations_people` tables directly.

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

Conversation context compacts automatically near 75% of either context limit,
counting the incoming exchange or observation. A tool-free model call merges an
older prefix with its previous summary. Up to eight recent messages remain
verbatim, keeping complete user/assistant exchanges together when they fit.
Smaller context limits or large incoming messages can retain fewer messages.
Summaries are limited to 6000 UTF-8 bytes, or one quarter of `context_bytes` for
smaller profiles. They share the model's history budget with recent messages.
The system prompt, tool definitions and current-turn tool results are additional.

Delivered exchanges are saved before the summarisation request. During that
request the raw buffer can temporarily exceed its usual cap by one exchange.
SQLite saves each summary and its covered-row cursor atomically. Summaries and
recent messages survive restart. Chat summaries are scoped to `(room, sender)`.
Shared room summaries contain only that room's observations. Summaries retain
decisions, open questions and referenced memory/reminder IDs as attributed,
untrusted context. They do not create shared memory facts or grant tool access.
The original model exchanges remain in the provenance log.

Compaction uses a separate completion budget of at least 4096 tokens, with a
shorter summary target. Truncated or malformed output gets one retry with half
that target. Terminal logs report the failure category and sizes without text.
Malformed, empty, oversized or failed summaries leave the previous summary
intact. The existing message and byte caps still trim recent context if needed,
so failed summarisation can lose older detail. Model input is byte-bounded and
compaction can process a prefix in batches when JSON encoding expands it.
`reset` clears the caller's thread summary and invalidates the shared summary
in that room. Revocation clears that user's thread summaries and invalidates
shared room summaries across the profile. In-flight results cannot restore them.

Inspect saved summaries locally with:

```sh
crowthebot inspect --profile crow-one --section summaries
```

`system_prompt` controls Crow's personality. The default takes its cue from
Crow T. Robot in Mystery Science Theater 3000: terse, wry robot banter, short
fragments, useful answers first. No double negatives, rambling follow-ons or
unsolicited offers. Existing profiles using the old built-in prompt adopt this
default on restart. Custom prompts remain unchanged.
Set `plugins` to `[]` to disable
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

OpenRouter keys come from an owned 0600 file or a terminal prompt with echo
disabled. Omit `--api-key-file` to prompt. OwnTracks entries reference the
existing OwnTracks TOML and select an allowed user/device. Recorder credentials
remain in that TOML file. Secret values are never command-line arguments.
Named endpoints require HTTPS unless the operator
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
OpenRouter also judges informal addressing such as “what do you think, crow?”
and clear follow-ups to its answers. Merely discussing Crow, quoting a request
or talking to someone else should leave it silent. A bare `!crow` or account ID
shows help. Existing commands also work after a leading account-ID mention.
Crow does not reply to unknown participants until the admin approves them.
Their messages in enabled rooms still contribute to room observations.
Membership events and attempted commands populate the unknown
list, capped at 1000 identities. `crowthebot people --profile NAME` inspects
that list locally while Crow is stopped.

In a DM, approved accounts can send ordinary text without a prefix. Commands
such as `help`, `reset` and the admin's `allow` also work without `!crow`.
Start a DM in your Matrix client and invite Crow. A profile can run with no
group rooms enabled. Accepted DM invitations are remembered across restarts.
Existing rooms tagged in Crow's `m.direct` account data also work as DMs.
For the primary admin, an existing room containing only Crow and the admin
works even without that marker. Crow remembers the confirmed association
across restarts. It also accepts admin invitations without a DM marker,
checking membership before allowing prefix-free replies.
Crow requires complete membership containing exactly itself and one other
account before enabling prefix-free replies. Adding another participant
disables DM handling. A DM does not become an enabled group room.

Crow ignores its own events, notices and groups not explicitly enabled.
Message edits enter the same request path using `m.new_content`, the editor's
authenticated identity and the edit's event ID. Adding `!crow` or a mention to
an earlier message can trigger a reply. Replayed edits are suppressed. Each
new addressed edit is a fresh request, including edits to an answered message.
Replies attach to the original message, while logs and tool provenance use the
edit event ID. Malformed edit fallbacks are ignored.
Legacy quoted reply fallbacks are
removed before interpreting a command. It sends `m.notice` replies, preventing
the usual bot-to-bot feedback loop. A person has separate context in each room. `reset`
clears their current thread. Revocation removes all their stored threads.
Requests are serialized without a cooldown, so immediate follow-ups are
processed normally. Matrix keeps a bounded queue of 32 events per room.

After an addressed request is authorized and claimed, Crow sends `m.typing`
with a 30-second expiry and refreshes it every 15 seconds during processing.
It stops the refresh fiber before clearing typing and delivering a reply.
Errors, cancellation and turns without replies also clear typing. Typing
requests have a five-second deadline and failures do not prevent replies.
If the homeserver cannot receive a clear, the last notification expires.
Silent room observations and ignored or duplicate messages do not start typing.

Every nonempty text message in an enabled group room goes through a silent
OpenRouter observation call, including messages from unapproved accounts.
That call receives no tools and returns a factual observation plus an addressing
decision. It sees bounded room context and the sender's recent exchanges with
Crow in that room. Addressed messages from approved accounts then
follow the normal command or tool-enabled request path, so a model question
uses two calls. Crow stays quiet unless addressed or executing a scheduled
action. This increases OpenRouter usage in active rooms.
Model-inferred addressing enters natural-language handling. Literal admin and
tool commands still require explicit addressing or a DM. Failed or malformed
judgments leave implicit messages silent. Explicit commands, mentions and DMs
continue working independently of the model's addressing decision. Verbose
logs include `model_addressed=true|false` without message contents.

The `room_observations` table retains bounded message excerpts and model notes
with sender, room, event ID and timestamp. Bounds use the profile's context
message and byte settings. Observations survive restart and inform later
authorized requests and scheduled actions in the same room. They are treated
as untrusted data, separate from shared memory facts and private DM history.
`reset` removes the caller's observations in that room as well as their thread
and affected summaries.
Revocation removes their existing observations, but future messages in enabled
rooms can still be observed without granting access. Provider failures retain
the original excerpt. Full observation exchanges use `room-observation`
provenance in `model_traces`. Crow still skips messages sent while offline.
Requests are processed serially within a profile.

The admin and friends can ask “crow, which rooms are you in?” or “what do you
know about this room?”. `matrix_rooms` lists joined rooms, five per page.
`matrix_room_info` returns a room's name, topic, canonical alias, encryption
state, DM marker, member counts and known joined or invited accounts. Its
`room` argument defaults to the requesting room. Both return `next_after` for
pagination. `members_complete=false` identifies incomplete synchronized
membership. These tools read the live Matrix state without network, filesystem
or message-sending capabilities. They expose no message history or credentials.

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

After a restart, the scheduler waits for live Matrix sync and restored room
handles before claiming work. While sync is offline, due jobs remain pending.

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
!crow feeds search 1 "OCaml"
!crow feeds read 1 42
!crow feeds poll 1
!crow feeds remove 1
```

Native tools are `feeds_add`, `feeds_list`, `feeds_status`, `feeds_entries`,
`feeds_search`, `feeds_read`, `feeds_poll` and `feeds_remove`.
Add defaults to hourly polling (`0 * * * *`,
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

The first complete import establishes a baseline and caches existing entries
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
requests for the same source within 60 seconds reuse its cached state.
Feed-level Atom `next` and `prev-archive` links, including links in RSS
channels, queue the next page through cron after 60 seconds. Relative URLs and
`xml:base` are resolved mechanically. SQLite commits each page with its
continuation cursor, so imports resume after a restart without model calls.
Imports reject cycles and stop at 1000 pages. Entries already imported remain
queryable while later pages are pending. Poll schedules and expiry still apply.
Failures preserve the last good cache and validators, with exponential backoff
from five minutes to one day. `feeds poll` imports the next page or refreshes
the root when the import is complete, and makes its
active imported-feed jobs due. It respects the cache and backoff. `feeds status`
shows individual jobs, their cron IDs and errors. `cron cancel ID` stops one
job. Removing a subscription cancels all its jobs and collects cache no other
subscription uses. Revoking its creator cancels polling.

List, status and entries commands return five records per page. Supply `after`
to native tools, or append the last displayed ID to the command, to continue:
`feeds entries 1 42`, `feeds status 1 12`, `feeds list 5`. Entry pages include
short excerpts and a `next_after` cursor. Pass it as `after` until null.
The SQLite mirror retains entries until their source is removed, including
article content supplied by the feed. `feeds_search` searches titles, summaries
and content with FTS5. `feeds_read` takes a subscription `id`, an `entry` ID
and optional `offset`, returning a UTF-8-safe article slice and `next_offset`.
Queries read SQLite only. Article bodies never enter list responses. Encoded
entry and article pages fit the 4096-byte tool-result limit.
Existing caches migrate automatically and refresh on their next poll to fill
in article content. Entry IDs remain stable when posts are updated.
Atom IDs follow the [Atom specification](https://www.rfc-editor.org/rfc/rfc4287.html#section-4.2.6).
RSS entries without IDs or links use a content hash.

The feed client has GET-only access to public HTTP(S) addresses, no credentials
or cookies, a 60-second deadline and a 64 MiB page limit. Pages may contain up
to 50,000 entries. It checks resolved
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

Link an existing OwnTracks configuration and select the one user/device that
Crow may query. Crow uses the `owntracks.config` library to read the same TOML
as the OwnTracks CLI, including its Recorder URL and HTTP credentials. No
Recorder credentials are copied into Crow's configuration. This integration
queries Recorder over HTTP and does not subscribe to MQTT.

```sh
crowthebot config owntracks set home --profile crow-home \
  --user alice --device 'My Phone'
crowthebot config owntracks list --profile crow-home
crowthebot config owntracks select home --profile crow-home
crowthebot run --profile crow-home
```

The file defaults to `$XDG_CONFIG_HOME/owntracks/owntracks.toml`, falling back
to `~/.config/owntracks/owntracks.toml`. Use `--owntracks-config FILE` for an
explicit path. The file must be owned by you with mode 0600. Crow rejects file
symlinks and refuses startup if the referenced file is inside Matrix profile
data or tool workspaces.

`--user` is the tracked OwnTracks user, separate from the TOML's HTTP Basic
username. `--device` accepts a raw device ID or a display name from
`[[owntracks.devices]]`. Ambiguous names are rejected. A display name is
resolved when saving the entry, which stores the actual device ID. Later alias
changes in the TOML do not change Crow's permitted tracker. Each named entry
allows one pair. Add another named entry to authorize another tracker.

Crow stores only the absolute config path, user/device IDs, lookback and HTTP
policy. Setup validates the local TOML without contacting Recorder. `set`
replaces older entries containing duplicated credentials. Such unrestricted
entries are refused at startup until replaced with an explicit tracker
selection. Change the Recorder password in the original TOML and restart Crow
to load the new credentials. A running instance keeps its initialized client.

The admin and friends can inspect allowed trackers and attach a person:

```text
!crow Which OwnTracks users and devices are available on home?
!crow Attach Alice's location to connection home.
!crow Where did Alice last report her location?
!crow location sources
!crow location list
!crow location get Alice
!crow location detach Alice
```

Native tools are `location_sources`, `location_devices`, `location_attach`,
`location_get`, `location_history`, `location_resolve`, `location_list` and
`location_detach`. Person labels may be
names or Matrix IDs. They are links to trackers, not proof of identity or
authority. All links and positions are shared across this profile's admin and
friends within the operator's configured tracker permissions. Bots, unknown
accounts and revoked accounts cannot access them. Discovery reads the local
selection and cannot enumerate other Recorder users or devices. Optional
`user` and `device` arguments to `location_attach` must match the configured
pair. Omitting them uses that pair automatically.

Each link records the connection name, OwnTracks user/device, requesting
account, room, event and attachment time. Attaching again replaces the link and
clears its previous fix. `location_get` refreshes by default. Set `refresh` to
false for a cached read. Positions contain latitude, longitude, UTC fix time
and accuracy in metres when available. A successful poll with no newer report
retains the last reported position. A failed poll returns a labelled cached
result and a sanitized error. Crow is instructed to qualify older positions
and retain their timestamps. Changing a connection's allowed tracker makes old
links inaccessible, including cached reads and lists. Reattach the person to
the newly configured connection after restarting.

Location results and history include optional `wifi_ssid`, `wifi_bssid`,
`connection_type` (`wifi`, `mobile` or `offline`) and `reported_at` fields.
OwnTracks supplies these when supported by the reporting device. Missing
fields are null. `recorded_at` dates the GPS fix. `reported_at` dates report
construction from OwnTracks `created_at`, which may describe a Wi-Fi update
using an older GPS fix. Cached selection uses report time when available.
A newer report without Wi-Fi data clears the old SSID and BSSID. These values
remain redacted from terminal and tool-audit logs alongside coordinates.

Crow can combine coordinates, accuracy, report age and Wi-Fi identifiers with
shared memory to infer places. A common network name alone does not identify
an office. For example: “crow, I'm in my office. Remember this location and
Wi-Fi network together, and use them to recognise it later.” Learned labels
belong in memory with their evidence and uncertainty. Live Wi-Fi reports stay
in location state. No Wi-Fi lookup is sent to OpenStreetMap.

Queries cover the preceding seven days by default. Configure
`--lookback-days` from 1 to 31 to change this bound. Only the latest valid report
is retained. Out-of-range coordinates, invalid accuracy, old fixes outside
the query window and timestamps more than five minutes ahead are discarded.
Each request has a 20-second deadline and a 1 MiB response limit. The initialized
client permits GET requests only to the configured Recorder's location
endpoint with the selected user/device query parameters. Redirects cannot
change that pair, reach another origin or use a discovery endpoint. Private
Recorder addresses are allowed because the operator chooses the referenced
configuration locally. Use `--allow-http` when that config uses trusted HTTP.

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

`location_history` queries Recorder for a linked person's recent fixes. Supply
`person`, `from` and `to`, using RFC 3339 timestamps within the configured
lookback and no later than now. The range is inclusive. Results are sorted
chronologically and deduplicated, with up to 20 fixes per page. Pass
`next_offset` as `offset` with the same interval until it is null. History
queries do not change the cached latest fix or write shared memory.

`location_resolve` queries OpenStreetMap through Overpass for administrative
areas containing a coordinate and nearby named or addressed features. Supply
`latitude` and `longitude`, optionally a named `connection`, `radius_metres`
(default 500, maximum 2000), and an exact OSM `tag`/`value` filter. Results
include OSM links, attribution and feature centre distances. Nearby features
do not establish an exact address or prove a visit. Map text is untrusted.
Use `offset`/`next_offset` to page through at most 20 containing areas and 100
nearby features. `query_limited` signals that the service cap was reached.

Configure the interpreter in the referenced OwnTracks TOML and restart Crow:

```toml
[owntracks.overpass]
url = "https://overpass-api.de/api/interpreter"
enabled = true
# allow_http = false
```

These are also the defaults when the table is absent. Set `enabled = false`
to disable map queries. Only coordinates and bounded generated queries are
sent to this service. Recorder credentials are excluded. The client permits
POST only to that interpreter, follows no redirects, spaces requests by one
second and limits responses to 1 MiB within 20 seconds. Arbitrary Overpass QL
and other Overture data sources are not exposed.

For example, DM Crow: "Use my location history for the last three hours and
OpenStreetMap to describe the places I passed through, with timestamps and
source links."

## Read-only CalDAV calendars

Fastmail calendars use CalDAV at `https://caldav.fastmail.com/`. Set up a
calendar-only app password in Fastmail's **Settings → Privacy & Security →
Manage app passwords and access**. Use your full Fastmail login address.
Fastmail JMAP API tokens do not authenticate CalDAV. OAuth works with CalDAV
but requires a registered OAuth client and is not implemented by this adapter.
Fastmail's separate MCP token supports calendar searches and reads, but its
published interface does not establish a full-fidelity incremental export.
Crow uses well-known discovery when the server root is not a DAV resource.
The root URL needs no account-specific path.
See [Fastmail authentication](https://www.fastmail.com/dev/) and
[API token scopes](https://www.fastmail.help/hc/en-us/articles/5254602856719-API-tokens).

```sh
crowthebot config caldav set fastmail --profile crow-one \
  --user 'YOUR_FASTMAIL_LOGIN'
crowthebot config caldav select fastmail --profile crow-one
crowthebot probe --profile crow-one --caldav fastmail --verbose
```

The first command prompts without echo. Alternatively use `--password-file`
with an owned 0600 file. `--url` selects another HTTPS CalDAV server.
`--max-response-mib` bounds each response, default 32 MiB. Credentials live in
private Xdge tool configuration. Tools receive no secret-store or filesystem
capability. The HTTP client permits only body-free GET, PROPFIND and the bounded
DAV:sync-collection REPORT and bounded VEVENT agenda REPORT on the configured
HTTPS origin. Write methods,
method overrides, unknown request forms and URL queries are rejected before
transport. Read-only behaviour is enforced by Crow. An app password may permit
writes in other clients. See the [CalDAV security audit](CALDAV_SECURITY.md)
for the enforcement path, regression checks and trust boundary.

Restart Crow, then send:

> Use the fastmail CalDAV connection to mirror my calendars every 15 minutes.
> Cancel the old Fastmail JMAP mirror job if one exists. Show me the sync status.

`caldav_sources`, `caldav_sync`, `caldav_status`, `caldav_search`, `caldav_read`,
`caldav_agenda` and `caldav_agenda_read`
are separate from the JMAP tools. Both backends remain available. A mirror
registers one durable, quiet `caldav` cron job. Each pass handles up to 20
resources from one collection. Remaining work and other due collections continue
at one-minute intervals. Existing jobs keep their owner, source event and
schedule. Cancelling the job stops polling.

The `caldav.eio` and `fetch.dav` libraries perform discovery and RFC 6578 sync.
A persistent queue holds each sync report before downloads begin. Each resource
and its ETag commit together. The sync token advances only after the queued page
is complete. Interrupted work resumes after restart. Expired tokens rebuild a
staging generation while the last complete generation remains readable. Servers
without sync-collection use a complete ETag inventory and bounded downloads.
Unknown or failed member responses abort the page without advancing its token.

SQLite retains original iCalendar GET bodies, content hashes, observed times,
old versions and deletion records. Recurrence rules, overrides, exclusions,
time zones, unknown properties and embedded attachments stay intact. External
attachments remain URLs and are not fetched. Calendar properties are stored as
XML with namespace information. XML lexical details are not retained. Invalid
calendar bodies remain archived and searchable, with `parsed=false`.

Search uses FTS5 over the local mirror and returns five resources per page.
Reads return bounded UTF-8 pages with a stable version ID and `next_offset`.
Follow pagination until it ends. Only current versions are available to the
model. Text search covers series and exceptions. Its index excludes VTIMEZONE
definitions and puts event fields first; existing indexes rebuild on startup
without changing any archived bytes.

For an actual schedule, ask:

> What's on my calendar today in Europe/London? Include recurring meetings.

Crow receives the current date, mirror IDs and calendar time-zone identifiers
in its context. Its prompt directs date questions to `caldav_agenda`. The tool
takes RFC3339 `start` and exclusive `end` instants with explicit offsets, up to
31 days apart. For example, 10 September 2026 in London runs from
`2026-09-10T00:00:00+01:00` to `2026-09-11T00:00:00+01:00`. An optional
`collection` selects one calendar; otherwise every mirrored event calendar is
queried, up to 20 per call.

The CalDAV server expands recurrence rules, exclusions and overrides through a
read-only calendar-query report. Crow saves the expanded source and occurrences
in separate SQLite tool tables. Original series, sync cursors and archived
versions remain intact. This requires network access for a fresh date range and
a server supporting expansion. Failed, truncated or unexpanded responses produce
`complete=false`, with unavailable calendar IDs, rather than a false empty day.

Agenda pages include times, titles, calendar IDs, freshness and completeness.
Follow `next_offset` using the returned `snapshot` ID. Those pages and
`caldav_agenda_read` use the immutable SQLite snapshot, including after restart.
The latter reads full expanded details such as attendees and descriptions.
Complete identical ranges are reused for five minutes. The cache retains up to
20 snapshots and 64 MiB of content, with at most 2,000 occurrences and 16 MiB
per snapshot. Evicted snapshots require a new date query. All-day end dates are
exclusive; floating times remain labelled. Calendar contents remain tool data,
separate from agent memory.

```sh
crowthebot inspect --profile crow-one --section caldav
crowthebot inspect --profile crow-one --section caldav-collections
crowthebot inspect --profile crow-one --section caldav-pending
crowthebot inspect --profile crow-one --section caldav-versions
crowthebot inspect --profile crow-one --section caldav-deletions
crowthebot inspect --profile crow-one --section caldav-agendas
crowthebot inspect --profile crow-one --section caldav-occurrences
```

Calendar contents are omitted from daily tool-use notes and terminal logs.
OpenRouter provenance traces retain calendar text supplied to the model.

## Read-only JMAP calendars

Configure a named connection with a bearer token restricted to calendar reads
on the server. Keep the token out of Matrix. Omitting `--token-file` prompts
without echo. Paste the token itself, without quotes or a `Bearer` prefix.
Surrounding ASCII whitespace is trimmed. The token does not need base64
encoding. A token file must have mode 0600.

Fastmail's [API documentation](https://www.fastmail.com/dev/) gives
`https://api.fastmail.com/jmap/session` as its JMAP session URL. Select a JMAP
API token with read-only access. Its documentation currently lists CalDAV for
external calendar access and says JMAP calendar access is not yet public. A
valid API token therefore does not guarantee that the required calendar
capability is available to Crow.

```sh
crowthebot config calendar set personal --profile crow-one \
  --url https://YOUR-SERVER/.well-known/jmap
crowthebot config calendar select personal --profile crow-one
```

Add `--account-id ID` to select an account instead of the session's primary
calendar account. Discovery, API and attachment download URLs must share the
configured HTTPS origin. `--max-response-mib` bounds each JSON response or
attachment to 32 MiB by default, configurable from 1 to 256. Oversized responses
fail without advancing the affected cursor. Credentials remain in Crow's private
Xdge configuration. The runtime receives an account-scoped JMAP calendar client
and HTTP/clock capabilities, with no filesystem or general JMAP request method.
The server must advertise the `urn:ietf:params:jmap:calendars` capability from
[draft-ietf-jmap-calendars-28](https://datatracker.ietf.org/doc/html/draft-ietf-jmap-calendars-28).
Provider compatibility has only been tested with mock responses.

Restart Crow, then send it:

> Mirror my personal calendar every 15 minutes. Show me the sync status, then
> search the mirror for meetings about the observatory.

Admins and approved friends share the mirror within the profile. The tools are
`calendar_sources`, `calendar_sync`, `calendar_status`, `calendar_search` and
`calendar_read`. Sync registers one persistent `calendar` cron job per connection,
including the requesting account, room and event. Repeated sync calls retain that
job's schedule. Cancel its reported job ID with `cron_cancel` to stop automatic
polling. A cancelled job is not revived by `calendar_sync`.

Each pass reads one page per resource type, then at most two referenced blobs.
Unfinished imports resume after a minute. Regular polls default to every 15
minutes and survive restarts. These polls do not call OpenRouter or post to a
room. A separate memory-linked reminder can ask Crow to query the mirror and
report upcoming plans to its source room.

`Jmap_eio.Calendars` owns the protocol reads. `Jmap_eio.Mirror` owns the generic
snapshot, pagination, change-cursor and recovery algorithm. Crow commits each
page and its cursor atomically in SQLite. Initial event scans stage a complete,
unfiltered base-event collection and catch up from an anchor state before
publishing it. Incremental sync uses separate `/changes` states for calendars,
participant identities and events. It checkpoints `newState` from `/changes`,
even when a subsequent `/get` sees newer data. Expired states and changing query
states start a replacement scan while the last complete mirror stays visible.

The canonical records retain exact JSON bytes, including unknown properties,
numeric spellings, recurrence rules, exclusions, overrides and time zones.
Events explicitly fetch `iCalendar` separately at the same state. Status
reports events without that representation when the server rejects the property.
JMAP blob attachments are cached as binary data. External attachment URLs remain
references and are never fetched automatically. Pending or failed blob downloads
are reported in status. Receipts and observed object versions remain archived,
including deletion records. The archive contains only versions observed since
mirroring began, and grows without a retention limit. It cannot recover server
history between polls or data hidden by server permissions.

Search uses a separate FTS5 index of all nested string values. It returns current
base event/series objects, not an expanded agenda. Recurrence and override rules
must be considered for date questions. Search yields bounded pages and version
IDs. `calendar_read` pages the original JSON with a stable version ID and UTF-8
byte offsets. If that version ceases to be current, search again. Archived
versions are available to the local operator through database inspection.

```sh
crowthebot inspect --profile crow-one --section calendars
crowthebot inspect --profile crow-one --section calendar-cursors
crowthebot inspect --profile crow-one --section calendar-versions
crowthebot inspect --profile crow-one --section calendar-deletions
crowthebot inspect --profile crow-one --section calendar-receipts
crowthebot inspect --profile crow-one --section calendar-blobs
```

Calendar content is redacted from tool-use payloads and terminal diagnostics.
The full OpenRouter trace still records any calendar text supplied to the model.

## Structured tool state and capabilities

Tools own typed tables in the profile's SQLite database. Conversational memory
holds selected observations. Operational caches and cursors belong to the tool
that maintains them. The model uses validated operations and stable record IDs
instead of writing SQL or treating memory facts as mutable cache records.

| Feed state | Purpose |
| --- | --- |
| `feeds_subscriptions` | Owner, source room/event, schedule and root URL reference |
| `feeds_sources`, `feeds_pages` | URLs, validators, poll status and durable pagination progress |
| `feeds_memberships`, `feeds_outlines` | OPML membership, cron-job references and delivery cursors |
| `feeds_entries`, `feeds_seen`, `feeds_fts` | Article mirror, deduplication keys and full-text index |
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
`Calendar_source.configuration` and `Model_config.configuration` supply the current integrations. Adding one does
not require teaching the conversational engine how to read secrets.

[`Plugin.t`](lib/plugin.mli) is a compiled-in extension with a name,
description and `run ~query` function. Register it when constructing
[`Engine.t`](lib/engine.mli), then enable its name in the profile. The engine
exposes each plugin both as an explicit command and a native model tool with a
string query argument. Tool arguments and outputs are bounded. A turn permits
at most six tool calls, followed by a tool-free synthesis request if the budget
is exhausted. Synthesis directives extend the opening system message, keeping
it first for providers that require that ordering. An empty answer or failed
terminal request triggers one additional synthesis request with tools disabled.
If that also fails to produce an answer, Crow delivers a brief fallback and commits
the turn after delivery. Tool actions are not replayed. The diagnostic log
retains the tool sequence and the OpenRouter trace records every request.
Context and plugin output remain untrusted model
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
| Recorder client | GET for one user/device and clocks, with no filesystem or config-reader capability |
| Email reader | Named RO bearer connection, restricted JMAP read methods and typed result cache |
| Email label writer | Separate RW bearer connection, state-checked mailbox membership patches |
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

## JMAP email

Configure reads with a read-only bearer token. Fastmail's JMAP session URL is
the default. Use `--url` for another provider and `--account-id` to select an
account other than the primary mail account.

```sh
dune exec -- crowthebot config email-ro set fastmail --profile crow-one
```

The command prompts without echo. Paste only the token, without `Bearer` or
quotes. `--token-file FILE` reads an owned 0600 file instead. To enable label
updates, configure a separate token with write permission for the intended
mail account:

```sh
dune exec -- crowthebot config email-rw set fastmail --profile crow-one
dune exec -- crowthebot config email-ro list --profile crow-one
dune exec -- crowthebot config email-rw list --profile crow-one
dune exec -- crowthebot run --profile crow-one
```

Each mode supports named `set`, `list`, `select` and `remove` commands. Restart
Crow after changing configuration. The separate `email-ro.json` and
`email-rw.json` files live in the profile's private Xdge secret area. The model
cannot read these files or supply tokens in tool arguments. Read tools never
fall back to an RW connection. The label tool appears only when an RW
connection is configured. Match the account when configuring the same name
in both modes.

Ask Crow:

> Find emails from Alice this week. Read the latest one and its thread.

> Add the Work label to that message and remove its Inbox label.

`email_sources` lists connections. `email_mailboxes` lists mailbox IDs, names,
roles and rights. `email_query` accepts JMAP mail filters and sort comparators,
returns up to 50 IDs and supports `position` pagination. For example:

```json
{"connection":"fastmail","filter":{"operator":"AND","conditions":[{"from":"alice@example.org"},{"after":"2026-09-01T00:00:00Z"}]},"sort":[{"property":"receivedAt","isAscending":false}],"limit":20}
```

`email_read` fetches one email, including text and HTML body values, headers
and attachment metadata. `email_thread` fetches up to ten messages per page.
Reads do not mark mail as seen. Attachments are not downloaded. Follow
`next_position` for further query or thread pages. These are live reads, so
membership can change between requests. Queries include the server's
`queryState`.

`email_update_labels` accepts one message `id` and `add`/`remove` arrays of
mailbox IDs. It uses only its RW connection. It patches individual memberships,
preserves other labels and requires at least one remaining mailbox. An
`ifInState` precondition rejects concurrent changes without retrying the write.
Neither path can send, create or destroy messages. Content and keywords cannot
be changed. The HTTP boundary enforces these restrictions independently of
the token's permissions, including when a broad token is placed in RO config.

Tool results are immutable snapshots in the profile's `email_results` SQLite
table, separate from memory. Small results contain `data`. Large results
contain `chunk` and `next_offset`. Use `email_page` with the same `result_id`
and that offset, concatenating chunks to reconstruct the JSON. Pages fit the
engine's 4096-byte cap after escaping. Paging never refetches the message.
Each snapshot is capped at 32 MiB. The cache keeps at most 50 results and
64 MiB of JSON. Snapshots stop being readable after 24 hours. Expired rows
are deleted on the next cache insertion. Snapshots and provenance survive
restarts. Admins and allowed friends share access within the profile.

```sh
dune exec -- crowthebot inspect --profile crow-one --section email-results
```

Inspection lists provenance and byte counts without loading message bodies.
Normal tool logs omit email arguments and contents. Terminal logs show starts,
outcomes, result IDs and sizes. Full OpenRouter provenance traces still retain
email text supplied to the model, independently of cache expiry. Email content
is untrusted input and does not authorize tool actions.

## Persistence and security limits

`crowthebot.sqlite3` stores authority, enabled rooms, event IDs, bounded
conversation and room context, accepted DMs, shared facts and their FTS index, tool logs,
daily notes, reminders, occurrence records and typed feed/location state. SQL inputs use bound
parameters. Schema version 7 is recorded, versions 1 through 6 are upgraded
atomically, and unknown versions are refused. Configuration and database files
are created 0600. The native SQLite path and process-lock operations are the
filesystem boundary around an otherwise Eio-based runtime.

Conversation context is stored as plaintext locally, including replies from
encrypted rooms. Enabled group messages, accepted prompts and tool results are sent to the configured
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
Conversation threads advance only after Matrix confirms delivery. Silent room
observations are retained independently. A timed-out queued send
can still complete later. Crow skips startup backlog and does not persist the
Matrix event cache. Room observation excerpts use the configured context bounds.
Failures are logged without prompt text, tokens or raw provider responses.

## Unfinished features

- SSO/OAuth account setup, cross-signing identity creation, QR verification and
  room-key recovery are not exposed by the CLI. Existing cross-signing secrets
  can be imported for SAS verification with `--recovery-key-file`.
- There is no room removal command yet. Stop the profile before changing its
  enabled-room database or create a fresh profile for a different room set.
- Context has no vector retrieval, tokenizer accounting,
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
OwnTracks tests cover config references, alias resolution, credential rotation,
restricted tracker selection, scoped credentials, bounded queries,
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
