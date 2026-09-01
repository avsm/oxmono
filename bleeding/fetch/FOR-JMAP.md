# What the JMAP client needed from Fetch and Proffer

Written from `ocaml-jmap/eio/` on 2026-09-02, revised 2026-09-03. It asked
for three pieces of generic HTTP work the JMAP client was carrying because
Fetch did not: a `text/event-stream` reader with a reconnect loop, a
redirect follower that extends a credential's scope hop by hop, and a
`Media` decode error that keeps a location and the codec library's own
error value. **All three have landed.** What follows is the map from the
request to what exists, so the client knows what to delete.

## 1. `text/event-stream` — landed

- `Fetch.Sse`: `type event = { name; data; id; retry }`, `media_type`,
  `decode ?max_event`, `connect ~sw ?headers ?last_event_id ?max_event`,
  and `subscribe ~sw ~clock ?backoff_initial ?backoff_max ?capacity
  ?retryable` with `events`, `last_event_id`, `result` and `close`.
- Framing is as asked: CRLF, LF and lone CR; a lone CR dispatches without
  waiting on a possible LF; comments, the one-space delimiter, joined
  `data`, an empty `event` naming `message`, a NUL in `id` ignored, a
  non-decimal `retry` ignored, a partial final block dropped, and
  `max_event` counting the block rather than the line. One leading UTF-8
  BOM is removed; event text is passed through as bytes, with no UTF-8
  validation.
- A server `retry` is clamped into `[0.1, backoff_max]`, so neither a zero
  nor an astronomical value governs reconnection.
- Writers: `Proffer.Sse` (`send`, `comment`, `retry`, `respond`) and
  `Fetch_mock.Sse` for hermetic tests.

## 2. Redirects that extend a credential's scope — landed

- `Fetch.Redirect`: `decision` (`Follow`, `Follow_within_scope`, `Stop`),
  `config`, `v`, `default`, `same_site` and `within_site`.
- `fetch` and `with_response` take `?redirect:Redirect.config`, with
  `?redirects` and `?allow_downgrade` kept as shorthands.
- `with_credentials ~extend:true` widens its scope to the origin of a
  `Follow_within_scope` hop for the rest of the chain, and `Fetch.scope`
  reports the scope in force.
- `Fetch.url` is the URL as the caller wrote it: a `Query` credential the
  wrapper appended is not in it, so a session URL persisted from it holds
  no token. Such a credential is redacted in traces and error context too.

## 3. `Media.error` with locations and typed detail — landed

- `Httpz.Media`: `Loc`, `type detail = ..` with `No_detail`, `malformed =
  { message; loc; detail }`, and `error = Unsupported | Malformed of
  malformed | Too_large of int`.
- `Media.v ~decode_reader` and `Media.decode_reader` decode straight from
  a `Bytesrw.Bytes.Reader.t`, so a large JSON body need not be copied into
  a string first.
- `Media_jsont` adds `type Media.detail += Jsont of Jsont.Error.t` and
  fills in `loc` from the `Textloc`, so `Json_error of Jsont.Error.t` is
  recovered from `detail` unchanged.

Not landed: `Media.register_detail_pp`. `Media.pp_error` prints the
message and location; a consumer that wants a library-specific rendering
matches its own `detail` constructor itself.
