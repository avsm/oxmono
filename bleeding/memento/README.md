# Memento

Memento datetime negotiation and TimeMaps for OxCaml. The library follows
[RFC 7089](https://www.rfc-editor.org/rfc/rfc7089.html) and the
[Memento implementation guide](https://mementoweb.org/guide/howto/).

| Library | Module | Use |
| --- | --- | --- |
| `memento` | `Memento` | Dates, Link relations, response metadata, JSON TimeMaps, capture selection |
| `memento.fetch` | `Memento_fetch` | Discover TimeGates, negotiate dates, fetch TimeMaps |
| `memento.wayback` | `Memento_wayback` | Query Wayback URLs and captured versions |
| `memento.proffer` | `Memento_proffer` | Read Accept-Datetime, serve TimeGates, Mementos and TimeMaps |

## Wayback command

```sh
memento versions https://example.org/ --latest --limit 5
memento versions https://example.org/ --from 2020 --until 2024 --json
memento urls https://example.org/docs/ --limit 100
```

Use `dune exec -- memento ...` before installation. `versions` prints UTC
capture time, recorded HTTP status, media type and playback URL, separated by
tabs. It includes all indexed statuses. `urls` prints original URLs below a
prefix, collapsed by CDX URL key. Both support `--json`, `--limit` (default
100, maximum 10000) and `--timeout` (default 30 seconds). No automatic retries
or page downloads occur. Empty results are successful queries with empty
output or an empty JSON array. Results at the limit may be incomplete.

The `memento.wayback` library exposes the same two queries to existing Fetch
clients. It uses the Internet Archive's
[CDX API](https://github.com/internetarchive/wayback/tree/master/wayback-cdx-server),
whose tabular JSON is separate from the Memento JSON TimeMap format. Replies
are bounded at 4 MiB. HTTP errors, including rate limiting, are reported.

## Find a replacement for a broken link

Pass your existing Fetch client. It retains its URL restrictions, logging,
connection policy, deadlines and retries. No transport or archive is hardcoded.

```ocaml
let archived_version client ~timegate original =
  let datetime =
    match Memento.Datetime.of_json "2024-01-01T00:00:00Z" with
    | Ok t -> t | Error e -> invalid_arg e
  in
  match Memento_fetch.find ~timegate client ~datetime original with
  | Error message -> Error message
  | Ok { capture = Some capture; _ } -> Ok capture.uri
  | Ok _ -> Error "TimeGate supplied no distinct archived URL"
```

`timegate` is the full archive negotiation endpoint for this original URL.
Supplying it skips the broken origin entirely. Without it, `find` sends HEAD
at the original, reads its advertised TimeGate even from an error response,
and follows that endpoint. Negotiation sends Accept-Datetime, follows up to
five redirects, and requires a successful response with Memento-Datetime and
an original link. It does not download the archived page. Fetch that selected
URL with your existing client when needed. Direct 200 TimeGate responses use
Content-Location when available. A negotiating resource without a distinct
Memento URL has no `capture` and needs a datetime-bearing GET to retrieve the
selected representation.

For a choice of captures, fetch a known archive TimeMap instead:

```ocaml
let alternative client ~timemap_uri ~datetime =
  match Memento_fetch.timemap ~limit:1048576 client timemap_uri with
  | Error message -> Error message
  | Ok map ->
      match Memento_fetch.captures map with
      | Error message -> Error message
      | Ok captures -> Ok (Memento.nearest datetime captures)
```

`nearest` breaks ties in favour of the earlier capture. Remote TimeGates may
use a different selection policy. A capture's timestamp is not its
Last-Modified value. Availability is not guaranteed by its presence in a map.

TimeMap fetching handles JSON and RFC 7089 link-format bodies. It fetches one
page, bounds body bytes (16 MiB by default), and bounds JSON nesting at 128.
Page and index traversal is explicit. Use `Timemap.references` for JSON and
`Fetch.Header.link_rel` or `Fetch.Header.link_has_rel` for link-format maps. Link targets are retained
as received. JSON URIs are absolute.

Transport failures and cancellation propagate as Fetch/Eio exceptions.
Protocol, media and decoding failures return `Error`.

For archive.org index lookup, use `Memento_wayback.versions` and select among
its returned captures. The package does not scrape archived HTML, save pages
or automatically replace application links.

## JSON TimeMap description

`Memento.Timemap.jsont` implements the
[JSON TimeMap guide](https://mementoweb.org/guide/timemap-json/):

- Basic maps have `original_uri` and `mementos.list`.
- Paged maps additionally have `pages.prev` and `pages.next`, when applicable.
- Indexed maps have `timemap_index` instead of `mementos`.
- Formats, first/last/closest captures, interval bounds, `archive_id`, and
  `memento_compliant` are typed. Compliance encodes as `"yes"` or `"no"`.

The codec uses `list` and `timemap_index`, following the examples rather than
`all` and `indexes` in conflicting parts of the guide. `timemap_uri` is an
object of format URLs. Missing or null page references mean no adjacent page.
Bounds are optional, following the prose. Reversed bounds are rejected.
Unknown extension members are accepted and discarded. RFC 3339 timestamps
normalize to UTC and retain fractional seconds. URI values are validated with
the RFC 3986 parser and may use schemes other than HTTP.

The single codec permits index children with no page links and first/last
outside their local subset, as required by the guide. Basic first/last
membership is a server obligation because the wire shape does not distinguish
basic maps from index children without page links.

## Serve an archive with Proffer

In a TimeGate handler, read `Memento_proffer.accept_datetime req`, select a
capture with your policy, and call `Memento_proffer.timegate respond ~original
capture`. It emits 302, Location, Vary and original/memento links. Treat invalid
Accept-Datetime as a bad request. Choose an application default when absent.
An archive with no captures should respond 404.

At the immutable representation URL, pass
`Memento_proffer.memento_headers ~original capture` to the response carrying
the captured bytes. Add timegate, timemap and adjacent capture links as needed.
Memento timestamps are a promise that the representation will remain stable.

Serve a JSON map with `Memento_proffer.json_timemap respond map`. RFC 7089 also
requires link-format support. Serve that with `link_timemap respond links`,
including exactly one original link and a datetime on each memento link.
These helpers compile inside portable handlers and can capture immutable
module-level capture and TimeMap values.

## Build and test

```sh
dune build --profile release-check @bleeding/memento/all
dune runtest --force --profile release-check bleeding/memento/test
```

Tests use Fetch and Proffer mocks. They exercise the protocol and shared
codecs without making requests to archive.org or any other archive.
