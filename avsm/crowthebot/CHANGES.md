# Changes

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
