# Changes

- Include a cached memory overview in each authorized turn within the existing
  context budget, with bounded tool results and request text.

- Keep shared-room posts brief and direct. Remove automatic requester
  attribution from messages sent through `matrix_send`.

- Add bounded memory overviews and range expansion with model-written summaries.
  Erasing a fact atomically clears the derived summary cache.

- Add `fresh` to `location_get`, which asks the phone for a new OwnTracks fix
  over MQTT and waits up to 30 seconds for it.

- Pass images to the model when Crow answers, fetched only then.
  `image_messages` turns this off.
- Send no text reply when a tool already posted to the requesting room.

- Send voice notes as Opus in Ogg with a waveform, which voice players expect,
  instead of bare AAC.

- Add `matrix_voice_note`, which speaks a requested reply with the
  `speech_voice` voice, Grandpa (English (UK)) by default, and posts it as a
  Matrix voice message.

- Tell the model that `[voice message]` text is a transcribed voice note, so
  it stops claiming it cannot hear voice messages.

- Accept a greeting before Crow's name, as in "Hey, Crow." from a voice
  transcript, and log each voice transcript with its sender.

- Transcribe Matrix voice messages on this machine with Apple's speech
  recogniser and handle them as text. `voice_messages` and `voice_locale`
  configure it.

- Add `matrix_send`, which posts a requested message to a joined room the
  requester belongs to, or to an existing DM with the admin or a friend.

- Store room messages as context without a model call and send them with the
  next addressed request. Addressing Crow by name opens or closes a message.
- Back off failing recurring reminders and cancel a reminder after 8
  consecutive failures.
- Run model turns, compaction and probes through Agentkit's `Chat`, `Turn` and
  `Summary`, so DS4 sees roles and its system prompt and every tool call
  passes one fail-closed guard.

- Repeat the tool-free synthesis directive as a final user message, so models
  that were mid-way through tool calls still answer instead of returning nothing.
- Add `group=stays` and `order=newest` to `location_history`, clamp an end time
  up to an hour ahead to now, and report the current time on interval errors.

- Add `improvement_record` and `improvement_list`, which let Crow append
  requests to improve itself to a Markdown file that coding agents can read.

- Disable reasoning for compaction requests and ask for words instead of bytes,
  so reasoning models no longer exhaust the budget. Accept fenced summary JSON.
- Raise the default `max_tokens` to 4096, mark replies cut off at the limit and
  keep two fifths of larger `context_messages` windows verbatim after compaction.
- Accept plain HTTP `base_url` values on loopback hosts.

- Reject fractional or string IDs and pagination offsets in calendar, email,
  feed and location tools instead of silently coercing them to integers.

- Add read-only CalDAV agenda queries with server-expanded recurrences, bounded
  SQLite snapshots and complete/partial results. Supply mirror and time-zone
  context to Crow, and remove VTIMEZONE noise from existing text indexes.

- Keep synthesis instructions in the opening system message for strict model
  providers. Recover failed terminal requests without repeating tool actions.

- Add bearer-authenticated JMAP email reads, threads and queries, with separate
  label-write credentials and bounded, persistent result pages.

- Add a typed JMAP mail adapter with separate reader and label-writer
  capabilities, server-limited paging and relative API URL support.

- Recover empty model answers with one tool-free synthesis retry, then a visible
  fallback. Preserve tool provenance without replaying actions.

- Show tool starts, outcomes, memory record IDs and calendar sync progress on
  stderr by default, with contents and credentials omitted.

- Fix Fastmail root-URL discovery through the standard well-known endpoint.
  Report missing CalDAV resources as HTTP 404 instead of a generic sync error.

- Extend `probe` with CalDAV authentication, discovery, sync and sample reads.
  Use `--caldav NAME` for a calendar connection or `--model-only` for OpenRouter.

- Enforce CalDAV mirror reads at the HTTP request boundary. Reject write methods,
  method overrides and unknown tool arguments before transport.

- Add read-only CalDAV mirrors alongside JMAP, with app-password configuration,
  restartable polling, original iCalendar archives and local search.
- Retry truncated context summaries within a separate completion budget. Report
  compaction causes and Matrix join failures without duplicate generic errors.

- Trim pasted calendar token whitespace and explain malformed input without
  exposing credentials. Disable terminal echo before displaying secret prompts.

- Mirror read-only JMAP calendars with restartable incremental sync, exact source
  archives, attachment caching, profile-wide search and quiet scheduled polling.

- Compact older room and DM context into persistent summaries, preserving recent
  exchanges, provenance and reset boundaries. Inspect them with `--section summaries`.

- Expose reported Wi-Fi SSID/BSSID and report time with OwnTracks locations,
  so Crow can infer contextual places using coordinates and remembered labels.

- Let OpenRouter judge informal addressing and follow-ups, and handle edited
  messages. Add Matrix room inspection and Crow T. Robot's terse personality.
- Mirror large, paginated feeds into SQLite with restartable cron imports,
  full-text search and bounded article pages for model queries.

- Remove the chat cooldown, allow six tools per turn and show refreshed typing
  notifications. Log tool sizes and the sequence when final synthesis fails.

- Fix admin DM invitations being ignored with `inviter=unknown` by reading
  Matrix's stripped invitation state in the bot library.

- Observe enabled-room messages through OpenRouter with persistent room context.
  Accept prefix-free admin DMs even when Matrix omits the direct-room marker.
- Add bounded OwnTracks history and OpenStreetMap location resolution through
  an Overpass interpreter configured in the linked OwnTracks TOML.

- Add live JSON database inspection, full OpenRouter exchange provenance and
  Markdown rich replies. Defer cron claims until Matrix rooms are ready.
- Add `run --verbose` and `probe --verbose` CLI diagnostics for Matrix routing,
  decryption, model calls, tools and reply delivery without logging bodies.

- Reuse the OwnTracks CLI configuration and credentials, with each Crow
  connection restricted to one configured user/device pair.

- Add shared OwnTracks person/location tools and private named Xdge tool
  configuration for Recorder connections and OpenRouter keys.
- Replace the fixed blogroll with shared RSS, Atom and OPML subscriptions,
  cron polling, typed feed caches and new-entry notifications.
- Persist tool-use logs, shared searchable facts and daily OpenRouter notes.
- Add memory-linked cron actions and narrow tool capabilities with confined
  file workspaces.
- Respond to Matrix mentions and approved DMs, including direct invitations.
- Add terminal SAS verification with existing cross-signing recovery keys.

- Add a Matrix assistant with isolated profiles, primary-admin authority,
  SQLite context and whitelists, OpenRouter responses and a bounded blogroll tool.
