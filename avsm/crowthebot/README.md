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
| `plugins` | `["blogroll"]` |
| `context_messages` | 20 |
| `context_bytes` | 40000 |
| `max_tokens` | 1024 |

`system_prompt` controls Crow's personality. Set `plugins` to `[]` to disable
all tools. To use OpenRouter itself, change `base_url` to
`https://openrouter.ai/api/v1`, choose a model, and pass `--api-key-file` to
`probe` and `run`. The private key file must be owned by you and mode 0600.
Keys are never read from ambient API-key environment variables. Bearer keys
require HTTPS. The default Sequoia connection is HTTP and uses no key.

The primary admin is stored in SQLite at initialization. Changing `admin` in
JSON alone is rejected. Create a fresh profile to choose another authority.
Rooms are enabled only by a successful `join`, which resolves an alias to a
stable room ID. Invitations are never accepted automatically. Joining another
room does not share conversation context with it.

## Talk to Crow

From the primary admin's Matrix account, in an enabled room:

```text
!crow allow @alice:example.org friend
!crow allow @other-ai:example.org bot
!crow people
!crow deny @alice:example.org
!crow help
!crow ask Help me plan my afternoon
!crow blogroll Anil
!crow ask Use the blogroll tool to find Anil's feed
!crow reset
```

`!crow TEXT` also asks the model directly. Only exact Matrix IDs establish
identity. Display names, room power levels, quoted messages and model output
cannot grant access. A `friend` is a human approved by the admin. A `bot` is an
AI account explicitly approved by the admin. Both must use `!crow` commands.
Unknown participants remain silent until the admin checks their identity and
approves them. Membership events and attempted commands populate the unknown
list, capped at 1000 identities. `crowthebot people --profile NAME` inspects
that list locally while Crow is stopped.

Crow ignores its own events, notices, edits, ambient conversation and rooms
not explicitly enabled. It sends `m.notice` replies, preventing the usual
bot-to-bot feedback loop. A person has separate context in each room. `reset`
clears their current thread. Revocation removes all their stored threads.
Model and plugin calls have a ten-second interval per person and room.
Requests are processed serially within a profile.

## Blogroll and plugins

The blogroll plugin reads
[Anil's OPML blogroll](https://anil.recoil.org/network/blogroll.opml), caches it
for one hour, and lists or searches subscription names and URLs. It does not
fetch the listed feeds, follow include outlines or discover additional URLs.
Its Fetch capability permits GET beneath the fixed URL, with redirects disabled.
Bodies are limited to 2 MiB. Sortal's [OPML parser](../sortal/lib/feed/sortal_feed_opml.mli)
rejects DTDs, custom entities, excessive nesting and unsafe URL schemes.

[`Plugin.t`](lib/plugin.mli) is a compiled-in extension with a name,
description and `run ~query` function. Register it when constructing
[`Engine.t`](lib/engine.mli), then enable its name in the profile. The engine
exposes each plugin both as an explicit command and a native model tool with a
string query argument. Tool arguments and outputs are bounded. A turn permits
at most three tool calls. Context and plugin output remain untrusted model
input. Tool results do not become administrative commands.

Pass each plugin only the capabilities it requires. The blogroll plugin
receives a restricted Fetch client. The engine does not hand plugins the
Matrix client, SQLite authority store, Eio environment or filesystem root.
Compiled plugin code is trusted and must be reviewed before registration.

## Persistence and security limits

`crowthebot.sqlite3` stores authority, enabled rooms, event IDs and bounded
conversation context. SQL inputs use bound parameters. Schema version 1 is
recorded and unknown versions are refused. Configuration and database files
are created 0600. The native SQLite path and process-lock operations are the
filesystem boundary around an otherwise Eio-based runtime.

Conversation context is stored as plaintext locally, including replies from
encrypted rooms. Accepted prompts and tool results are sent to the configured
model endpoint. Encryption protects Matrix transport, not model inference or
local backups. Authorization verifies a Matrix account ID reported by the
homeserver. It is separate from Matrix device verification and cross-signing.
An admin must verify a new contact before granting access.

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

- SSO/OAuth account setup, device verification, cross-signing and key recovery
  are not exposed by the CLI. Password authentication and stored device keys
  are supported.
- There is no room removal command yet. Stop the profile before changing its
  enabled-room database or create a fresh profile for a different room set.
- Context has no summarization, vector retrieval, tokenizer accounting,
  redaction handling or retention timer. Matrix deletions do not erase saved
  context. `reset` and `deny` remove active rows, not backups.
- No image, audio or file ingestion, scheduled jobs, shell, browser or remote
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
crowthebot blogroll Anil
crowthebot probe --profile crow-home
```

Offline tests exercise authority, roles, denied commands, replay suppression,
rate limits, revocation, context isolation and bounds, failed delivery,
model tools, forged permission changes, tool-call budgets, cancellation
recovery, HTTP status handling, feed caching, XML limits, profile isolation,
file permissions and process locking. A native model-adapter regression checks
that empty tool arrays are omitted for Sequoia.

The opt-in probe below uses synthetic identities, an in-memory database and a
printing delivery callback. It sends a prompt to Sequoia and permits the model
to call the public blogroll plugin. It never connects to Matrix or reads an
existing profile. This complete model/tool workflow passed on 2026-09-08.

```sh
opam exec --switch=5.2.0+ox -- dune exec --profile release-check \
  avsm/crowthebot/test/live/probe.exe
```
