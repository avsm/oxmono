# CalDAV mirror security audit

Audited on 2026-09-10. No agent-accessible remote mutation path was found.
Read-only access is enforced by the client capability even when the app password
has server-side write permission. The audit also tightened the HTTP request
policy and runtime argument validation.

## Enforcement

- `Caldav_tools` exposes sources, sync, status, search, read and agenda reads. Arguments select
  configured connections and local records. There is no method, URL, request
  body, header, credential, upload, deletion or configuration argument. Unknown
  and duplicate arguments are rejected before network access.
- `Engine` rejects unknown tool names and reserves the `caldav_` prefix against
  plugins. Runtime plugins are empty. The model has no shell, general HTTP or
  secret-store tool. A request from the admin cannot override these limits.
- `App.caldav_tools` loads credentials from private Xdge configuration. It
  creates a separate Fetch client. `Caldav_source.initialize` wraps that client
  in `Caldav_http.read_only` before attaching credentials. Fetch wrapping does
  not mutate the underlying client or export credentials to other tools.
- `Caldav_source.t` is abstract. It exposes identity, discovery, sync reports
  resource reads and bounded agenda expansion. Its credentialed `Caldav_eio.Client.t`, Fetch handler and
  password are not exported. No filesystem or process capability is passed in.
- `Caldav_http.read_only` permits only body-free GET, PROPFIND at depth zero or
  one, DAV:sync-collection REPORT at depth zero with a 20-resource limit, and
  calendar-query REPORT at depth one for VEVENT expansion. Agenda requests must
  match the canonical query tree exactly, including attributes, with identical
  UTC filter and expansion bounds spanning at most 31 days. Arbitrary report
  properties, extensions, filters and unbounded expansion are rejected.
  XML requests must have the expected structure and namespaces. Unknown report
  types, write methods, method overrides, scheduling headers, URL queries,
  streaming bodies and XML requests over 64 KiB are rejected before transport.
  All requests remain on the configured HTTPS origin.
- The policy is inside the credential wrapper. Calls through the underlying
  CalDAV library's put, add, delete, delete-calendar, create-calendar or property
  setters still meet that policy. An outer Fetch wrapper cannot remove it.
- `Fetch_dav.request` disables redirects for DAV operations. Well-known
  discovery resolves a redirect and then issues a restricted PROPFIND. Missing
  or redirected origin roots try that discovery path once. The destination
  must remain on the same HTTPS origin. Resource
  reads also validate the calendar subtree and reject traversal and encoded
  path separators.
- `Caldav_store` has SQLite, a mutex and time callbacks. Its deletions affect
  only the local mirror. It has no HTTP capability or dependency on the source
  client. Remote deletion notices remove local visibility, never remote data.
- `Caldav_agenda_store` has the same restricted local capabilities. Cache
  eviction only deletes local snapshots. Every page and raw resource read checks
  admin or approved-friend access. Expanded resources retain provenance and do
  not replace the original mirror or advance sync cursors. Failed members and
  unexpanded recurrence rules cannot become a supposedly complete agenda.
- Scheduled CalDAV jobs call the same `Caldav_tools.poll` and restricted source.
  Mechanical sync does not invoke OpenRouter or accept arbitrary actions.
- `probe` uses the same restricted source for discovery, one sync report or
  inventory per calendar, and one sample read per calendar. It has no mirror
  store capability and sends no calendar contents to OpenRouter.

The Fetch policy was traced through `Fetch.with_credentials`, nested
`Fetch.restrict` calls, `Fetch_dav.Session.connect` and the HTTPZ transport.
The transport serializes the checked method without converting a read into
another method. Cookie handling is disabled by the application.

## Regression evidence

`test_caldav_read_only.ml` uses a backend that would accept writes if reached.
It checks that event and calendar mutation APIs fail before transport. It also
attempts raw mutation methods, method overrides, arbitrary and malformed REPORT
bodies, streamed bodies, URL queries, plaintext and foreign destinations,
permission widening and direct Fetch handler access. It verifies that discovery
and the supported sync REPORT still work, and that DAV GET does not follow a
redirect.

`test_caldav.ml` drives the engine with a model that requests deletion and tries
to pass a DELETE method to sync. Neither causes an HTTP request. It checks that
cron uses the same mirror path without a model call. Existing tests cover exact
iCalendar storage, restart recovery, pagination, local deletion handling,
authorization and cancellation.

`test_caldav_probe.ml` exercises discovery, sync reports, ETag inventories and
sample reads through the restricted source. It checks failure stages, continued
checks after failure, bounded sample reads and redaction of private contents.
`probe.t` checks CLI selection and failure status without network access.

`test_caldav_agenda.ml` checks exact bounded reports, recurring occurrences,
moved exceptions, cancelled events, exclusive all-day ends, incomplete responses,
cache pagination and restart, access revocation, and reindexing with original
bytes preserved. It rejects arbitrary query extensions, oversized or open-ended
ranges, floating bounds, other component types and mismatched filter bounds
before the accepting HTTP backend is reached.

Run the scoped checks with:

```sh
opam exec --switch=5.2.0+ox -- dune runtest --force --profile release-check avsm/crowthebot
opam exec --switch=5.2.0+ox -- dune build --profile release-check @avsm/crowthebot/all
```

## Trust boundary

This protects remote calendars from agent tool calls, including hostile prompts,
calendar descriptions and invented tool calls. The compiled Crow process, its
HTTP backend and host remain trusted. Eio capabilities are not an OS sandbox for
arbitrary native code. Someone who obtains the app password independently can
use its server-side permissions outside Crow. The server must implement the
read methods according to the supported DAV protocols.

A future writable calendar integration must receive a separate explicit
capability. It must not widen this mirror policy.
