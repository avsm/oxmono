# Migrating `requests` → `fetch`

Analysis of replacing `bleeding/requests` with `bleeding/fetch` (vendored from
taposaur) as the HTTP client across the monorepo. Produced 2026-08-05.

**STATUS: COMPLETE (2026-08-05).** All 14 consumers migrated and
`bleeding/requests` deleted; see commits b744c14 through e68e244. The gaps
below were closed by `fetch-signature` (RFC 9421, which also fixed a latent
requests bug that broke apubt's signed delivery) and `fetch-cmdliner`
(flag-compatible `Requests.Cmd` replacement). `conpool` and `cookeio` remain
in-tree as standalone libraries with no consumers. Known inert flags:
`--no-proxy`, and `--follow-redirects`/`--max-redirects` for generated
openapi calls (fetch redirects are per-request, default 10 hops).

## Backend choice

`fetch-curl` is the practical backend for oxmono2: it is the only one that
covers what `requests` provided without new code — TLS via libcurl + system
trust (`?tls_verify`), connection pooling (`?max_connections_per_host`,
`?max_total_connections`), HTTP/2 (`?http_version:` `` `Auto ``, `?multiplex`),
proxy (`?proxy`), timeouts (`?timeout ?connect_timeout`). `fetch-cohttp` has
**no built-in TLS** (caller must supply `https : Uri.t -> conn -> conn`; no
example exists) and no connection reuse.

`Fetch_curl.std ~sw env` stacks a cookie jar, per-origin flow control
(6 concurrent) and retries; `Fetch_curl.v ~sw ()` is the bare client.

## Key findings

1. **`requests.oauth` has zero consumers** — atp's xrpc-auth is AT Protocol
   app-password session auth, apubt's Mastodon OAuth is hand-rolled PKCE.
   Drop it, don't port it.
2. **`requests_h2` has zero consumers.** `fetch-curl` gives HTTP/2 for free.
3. **No consumer names `Conpool` or `Cookeio` directly** — they only reach
   consumers inside `Requests.create`, and almost every call is the bare
   `Requests.create ~sw env`. Only kwargs used anywhere:
   `~follow_redirects:true` (bushel), `~timeout` and `~default_headers`
   (apubt).
4. **Four consumers are ~98% machine-generated** (immich 1218, peertube 1316,
   typesense 459, karakeep 217 call sites): migrating
   `avsm/openapi/lib/openapi_codegen.ml` (16 sites, lines ~1663-1754 and
   ~2248-2386) + regeneration unlocks them all. typesense/karakeep have
   working `@gen` rules; immich/peertube need their `dune.inc` rules restored
   (specs are in-tree: `immich-openapi-specs.json`, `peertube-openapi.yaml`).
5. **`Requests.Cmd` (14-flag cmdliner term + `setup_log_sources`) has no
   fetch counterpart** — needed by immich, typesense, peertube.

## Mechanical mapping

| `requests` | `fetch` |
|---|---|
| `Requests.t` | `Fetch.plain` |
| `Requests.create ~sw env` | `Fetch_curl.std ~sw env` |
| `Requests.get t url` | `Fetch.get ~sw t url` or `Fetch.with_response t` `` `GET `` `url fn` |
| `Requests.post ?body` | `Fetch.post ~sw ~body` |
| `Response.status_code r` | `Fetch.status r` |
| `Response.ok r` | `let s = Fetch.status r in s >= 200 && s < 300` (no helper; define locally) |
| `Response.body r` | `Fetch.body r` (both `Eio.Flow.source_ty Eio.Resource.t`) |
| `Response.text r` | `Eio.Flow.read_all (Fetch.body r)` |
| `Response.json r` / `jsonv c r` | `Jsont_bytesrw.decode_string c (Eio.Flow.read_all (Fetch.body r))` |
| `Response.headers r` | `Fetch.headers r : Http.Header.t` |
| `Response.content_type r` | `Fetch.header Fetch.Header.content_type r` |
| `Headers.empty \|> set` | `Fetch.Header.[ accept, [ pref v ]; raw n v; ... ]` (typed codecs) |
| `Body.of_string mime s` | `Fetch.String s` + `Header.[ content_type, media mime ]` |
| `Body.form ps` | `Fetch.Form.urlencoded ps` (returns headers *and* body) |
| `Body.multipart` | `Fetch.Form.multipart [ field ...; file ...; stream ... ]` |
| `Body.of_stream` | `Fetch.stream ?length flow` |
| `set_auth (Auth.bearer ~token)` | `Fetch.with_credentials ~scope:[origin] Credential.[ Bearer (fun () -> token) ]` |
| `set_default_header "x-api-key" k` | `Credential.[ Header ("x-api-key", fun _ -> k) ]` — **not** `with_headers`, which raises on auth-bearing names |
| `~follow_redirects:true` | default (10 hops); `?redirects:0` to disable |
| `Requests.Retry.config` | `Fetch.Retry.config` (field-for-field) + `Fetch.with_retry ~clock ~random` |
| `Timeout.create` | `Fetch_curl.v ?timeout ?connect_timeout` or `Eio.Time.with_timeout` |
| `Response_limits.make` | `?max_response` |
| `~verify_tls:false` | `Fetch_curl.v ~tls_verify:false` |
| `Cookeio_jar` / `~persist_cookies` | `Fetch_cookies.Jar.of_file ~clock path` + `with_jar`, or `Fetch_curl.std ~cookies:(` `` `File p ``)` |
| `Http_client.make_request_streaming` | `Fetch.fetch ~sw` + `Fetch.body r` (already a flow) |
| `Requests.Error.*` | `Fetch.error` raised as `Eio.Io (Fetch.E e, _)` — existing `Eio.Io` handlers keep working |

Sharp edges:
- `with_credentials` has a mandatory `~scope` and rejects `http://` origins
  unless `~allow_insecure:true` (matters for localhost typesense/immich dev).
- `with_headers` raises `Invalid_argument` on `Authorization`/`Cookie`/
  `Proxy-Authorization` — secrets must go through `Credential`.
- `Fetch.with_response` opens its own switch — use it wherever the body is
  fully drained (every consumer except perma-proxy) so signatures don't need
  to grow a `~sw`.
- Setting `Host`/`Content-Length`/`Transfer-Encoding` manually raises
  `Invalid_request`.

## Gaps

| Gap | Who | Fix |
|---|---|---|
| RFC 9421 HTTP Message Signatures | apubt only | port `bleeding/requests/lib/features/signature.ml` (~1000 LOC, coupled only to `Headers.t`) as a `fetch.signature` middleware; only `String` bodies signable |
| `Requests.Cmd` cmdliner layer | immich, typesense, peertube | write `fetch.cmdliner` mapping the 14 flags onto `Fetch_curl.v`/`std`, incl. source tracking and `setup_log_sources ~verbose_http` (fetch has no `Logs.Src`) |
| `~params` query args | typesense (1 site) | build query with `Uri` (Middleware.Url.set_query_params is not caller-reachable) |

`fetch.cookies` vendors its own `publicsuffix`, duplicating
`bleeding/publicsuffix` — reconcile before both end up linked in one binary.

## Migration order (easiest → hardest)

1. **bushel/lib_sync** — 67-line `Bushel_http` shim, no public API change. ← pilot
2. **zotero-translation** — one file, POST-only.
3. **webfinger** — GET-only, but `Requests.t` is in its public mli; only caller is apubt (blocked), so coordinate.
4. **tessabot** — one POST helper + 4 copy-pasted session blocks.
5. **linkedin** — GET/POST, bearer, caller-supplied session.
6. **sortal** — conditional-request caching maps onto typed `Fetch.Header` codecs.
7. **openapi generator** — 16 sites; gates everything below.
8. **karakeep** (regenerates via `@gen`), 9. **typesense** (+Cmd gap, `~params`),
10. **immich** (+Cmd, needs `dune.inc` gen rule restored),
11. **peertube** (+Cmd, `Body.form` password grant, needs gen rule),
12. **atp** — `Requests.t` in four public mlis; one atomic change across 5 packages.
13. **httpz-perma-proxy** — internal streaming API; rewrite of `perma_cache.ml:455-520`, not a substitution.
14. **apubt** — blocked on RFC 9421.

## Hygiene (unrelated, fix while touching)

`bleeding/atp/xrpc-auth.opam`, `bleeding/atp/bin/standard-site/standard-site.opam`
and `bleeding/atp/bin/tangled/tangled.opam` list `requests` in dune `libraries`
but omit it from opam `depends` (relying on transitivity through `atp-xrpc`).
