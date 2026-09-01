# Security audit record

Date: 2026-09-02. Scope: full tree (`httpz/`, `fetch/`, `proffer/`), verified
by code review plus running code/wire-level reproductions. Findings marked
**confirmed** were reproduced against the built library (OCaml 5.5.0); findings
marked **refuted** were investigated and shown not to be vulnerabilities — they
are recorded so future audits do not re-flag them.

## Confirmed findings

### 1. Cookie `Domain` equal to a public-suffix host creates a domain cookie

- `httpz/cookie/lib/cookie.ml:373-383` (`check_public_suffix`),
  `cookie.ml:437-445` (caller).
- RFC 6265 §5.3 step 5: when the Domain attribute is a public suffix
  *identical to the canonicalized request host*, the attribute must be ignored
  and the cookie stored host-only. This code accepts the attribute and stores
  `host_only=false`.
- Exploit (verified end-to-end, `cookie_repro/`): a response from
  `https://s3.amazonaws.com/` (or `github.io`, `herokuapp.com`, `pages.dev` —
  all private-section PSL entries, and the vendored loader includes the PRIVATE
  section) with `Set-Cookie: sid=x; Domain=s3.amazonaws.com` is attached to
  **every** `*.s3.amazonaws.com` origin. Works purely via redirects, since the
  jar is consulted per hop and `cookie` is stripped cross-origin but
  re-attached from the jar for the redirect target.
- The existing test `httpz/cookie/test/test_cookie.ml:146-150` codifies the
  buggy behavior ("exact-match public suffix allowed") and must be corrected
  with the fix.

### 2. Non-canonical IP literals accepted as URL hosts (SSRF blocklist bypass)

- `fetch/lib/url.ml:26-66` (`valid_host_char`/`check_host`).
- Accepted and resolved to loopback by both backends (verified):
  `2130706433`, `0x7f000001`, `127.1`, `127.0.1`, `0177.0.0.1` (octal),
  `[::ffff:127.0.0.1]` (proven to reach an `127.0.0.1` listener).
  Trailing-dot hosts (`localhost.`, `example.com.`) are also accepted.
- All policy layers (`Fetch.restrict`, credential scopes, `with_limits`
  buckets, cookie jar) compare the raw host string, so a string IP blocklist
  is bypassable; allowlists hold. `Redirect.is_ipv4`
  (`fetch/lib/redirect.ml:22-40`) is dotted-quad-only.
- DNS rebinding (allowlisted name resolving to loopback) is unaddressed and
  undocumented; the only escape hatch is a custom `connect`.
- Cookie/jar effect: trailing-dot and alternate spellings create *disjoint*
  namespaces (fragmentation, not leakage).
- Fix direction: in `check_host`, normalize inet_aton-parseable reg-names to
  canonical dotted-quad and strip one trailing dot. `[::ffff:…]` cannot be
  rejected syntactically without breaking IPv6 — document the resolved-IP
  check posture instead.

### 3. Request head-size check measures buffered bytes, not the head

- `httpz/lib/httpz.ml:206-209`: `Httpz.parse` compares total buffered bytes
  (head + coalesced body) against `max_header_size` (16 KiB). The response
  parser does it correctly (`httpz/lib/res.ml:479`, checks `body_off` after
  parsing).
- Verified on the wire against `Proffer_httpz.run`: POST with
  `Content-Length: 20000` sent head+body in one write → `413 Payload Too
  Large`, connection closed; same request with split writes → `200 OK`. Large
  uploads therefore fail or succeed depending on TCP segmentation.
- Fails closed; the connection close prevents poisoning. Fix: move the check
  after `parse_headers_loop` on `body_off` (diff and red regression tests were
  prepared in `httpz/test/test_httpz.ml` and `proffer/test/test_httpz.ml`).

### 4. No timeouts in the pure-OCaml client backend

- `fetch/httpz/fetch_httpz.ml` has no connect/read/total-time caps. A
  stalling server pins the connection, fiber, and one of the 6 per-origin
  `with_limits` slots forever. The curl backend defaults to a 30 s connect
  timeout only (`fetch/curl/fetch_curl.ml:1190-1194`); no total-time default.

### 5. `Negotiate.v` ignores q-values; no 406

- `proffer/lib/negotiate.ml:46-61`: `Accept: application/json;q=0` against a
  JSON variant still serves JSON with 200 (verified). `select_ranked` (used by
  `Negotiate.encode`) does skip `q <= 0`, so the two paths are inconsistent.
  No-match falls back to the first variant instead of 406 (RFC 9110 §12.5.1).
  Partially documented in `negotiate.mli`, but the inconsistency is not.

### 6. `Cache.memoize` is unbounded and quadratic

- `proffer/lib/cache.ml:18-38`: no size cap; expiry pruning runs only on the
  miss path. Distinct keys grow the assoc list forever and inserts are O(n)
  each (20k inserts ≈ 9 s, measured). No shipped component uses it (only
  `proffer/example/7-cache` with a constant key), so it is a public-API
  misuse hazard rather than a live vulnerability.

### 7. `date.ml` reads past the allocation on short rfc850 values

- `httpz/lib/date.ml:245-283`: `parse_rfc850` has no length guard (unlike
  `parse_imf_fixdate`'s `len < 29`); `parse_time` peeks up to 5 bytes past
  `off+len` via `Bytes.unsafe_get`. Network-reachable via `If-Modified-Since`
  (in-repo callers copy values to exact-size heap buffers, so there is no
  slack). Not observable in practice: the GMT check requires the position to
  land exactly at the span end, so result corruption is impossible and no
  crash reproduced under churn. Fix: add the length guard (memory-safety
  hygiene).

### 8. Empty `Host:` accepted; absolute-form authority not cross-checked

- `httpz/lib/target.ml:267-286` (`valid_host` returns true on empty),
  `httpz/lib/httpz.ml:222-223` (only *missing* Host is rejected).
- Verified: `GET /` with `Host:` empty → 200; `GET http://b/…` with
  `Host: a` → 200. Benign in proffer today (no vhost routing, no `Req.host`
  accessor), but any application doing vhost logic off `Req.header "Host"` or
  absolute-target redirection sees `Some ""` instead of a 400. Fix: reject
  empty Host in `valid_host`; optionally compare absolute-form authority
  against Host per RFC 9112 §3.2.2.

### 9. curl backend strips framing headers for codings it does not decode

- `fetch/curl/fetch_curl.ml:1117-1126`: `Content-Encoding`/`Content-Length`
  are removed whenever `content-encoding` is present, but libcurl only decodes
  gzip. A `br`/`zstd`-labeled body is passed to the caller still compressed
  but presented as decoded. The httpz backend checks the value
  (`fetch_httpz.ml:204-210`) — mirror `is_gzip`.

### 10. Smaller items (low/informational)

- Public-suffix check fails open on `Pubsuffix` errors
  (`cookie.ml:377-378`, `Error _ -> Ok ()`) — not reachable via normal
  Set-Cookie flows (refuted as a vulnerability), but should fail closed.
- Jar size caps count only name+value bytes, not Path/Domain
  (`cookie_jar.ml:170-173`); no byte budget across subdomains.
- `Cookie.v`, `Cookie_jar.make`, `of_netscape` skip name/value validation; a
  value like `x; admin=1` becomes a second cookie pair in emitted headers
  (local-trust boundary — the backing file is 0600 and atomically written).
- `fetch_httpz.ml:659-668` opens a plaintext TCP connection before
  discovering there is no `~https` provider for an `https:` URL. No HTTP bytes
  are sent, but a surprising outbound connect occurs.
- No server write timeout (`proffer/httpz/proffer_httpz.ml:113-135,580-590`);
  read-side slowloris is covered (idle 75 s / request 15 s) but a
  slow-draining client pins its connection fiber (bounded by
  `max_connections = 512`).
- Router collapses `//` and trailing `/` (`proffer/lib/pct.ml:125-134`,
  `route.ml:53`) — a normalization differential for apps doing prefix
  authorization in a front proxy.
- `Range` multipart boundary uses the non-crypto `Random` default
  (`httpz/lib/range.ml:280-289`), deterministically seeded across restarts.
  Not reachable from any shipped backend.
- `url.ml:36-42` `looks_like_ipv6` is charset-only: `[:::]`, `[99999::1]`
  accepted, fail closed at DNS time. Deliberate heuristic.
- 307/308 cross-origin redirects re-send a replayable body after only header
  stripping (`fetch.ml:407-431`) — matches browser behavior; document it.
- SSE `retry:` from the server is honored uncapped (`fetch.ml:933-939`,
  ~11 days for `retry: 999999999`).
- Decode-failure errors can quote body fragments into logs
  (`media_jsont.ml:36-45`).
- 32-bit: `Max-Age=9999999999` silently becomes a session cookie;
  far-future `to_netscape` dates raise.
- Server-side effective body cap is the 32,767-byte read window
  (`proffer_httpz.ml:56-57,384`); httpz's `max_content_length = 100 MB`
  default is unreachable through proffer (proffer_httpz.mli does document
  ~32 KiB, so only the httpz default is misleading).
- `Route.int` accepts OCaml int syntax (`0x1F`, `1_000`, `+3`)
  (`proffer/lib/route.ml:36`).
- HTTP/1.[2-9] accepted as 1.1 and leading-zero Content-Length/chunk sizes
  accepted — deliberate, tested, but parser-differential axes if this stack
  ever sits behind a stricter frontend.

## Investigated and refuted (not vulnerabilities)

- **CRLF injection via middleware-attached headers**: `check_request` runs at
  the *innermost dispatch*, after every middleware transformation, on the
  exact request the backend sends (`fetch/lib/middleware.ml:197-227`).
  Verified on both backends: a Bearer token containing `\r\n` is rejected
  with `Invalid_request` before any byte reaches the wire. The raw writers
  (`httpz/lib/res.ml:286-292`, `fetch_httpz.ml:312-328`) are unchecked by
  design and safe because only validated callers reach them. Note: libcurl's
  `CURLOPT_HTTPHEADER` passes embedded CR/LF through (verified at byte level,
  libcurl 8.18.0) — a hazard only for users of raw `ocurl` without the Fetch
  layer.
- **`url.ml` `Stack_overflow` on huge authorities**: `has_non_ascii` is
  properly tail-recursive; a 10 MB authority parses fine.
- **Fails-open public-suffix arm reachable**: `Error _ -> Ok ()` in
  `check_public_suffix` cannot be reached via normal Set-Cookie flows
  (suffix-match and non-empty preconditions rule it out).
- **Smuggling via TE/CL, duplicate CL, bare LF, chunk framing, obs-fold,
  header octets, buffer arithmetic**: thoroughly defended and tested; no
  findings.

## Repro artifacts

- `cookie_repro/` — end-to-end demo of finding 1 (`dune exec
  cookie_repro/repro.exe`).
- `scratch/` — CRLF-injection probes for the refuted claim.
- `httpz/test/test_httpz.ml`, `proffer/test/test_httpz.ml` — regression tests
  for finding 3, intentionally red against the unfixed library.

# Second pass (2026-09-02, same day)

Hunting analogues of the first-pass bug classes (unguarded unsafe reads,
limits on aggregate vs parsed lengths, string host comparisons, RFC state
transitions, unbounded growth, integer truncation, non-tail recursion,
unchecked wire writers) plus previously unaudited surfaces (typed-field
parsers in `fetch/lib/header.ml`, body codecs, Eio concurrency lifecycle,
proffer server semantics). All items below were verified with running code;
repro programs live in `scratch_verify/` and `audit/` (untracked, not part of
`@install`).

## New confirmed findings

### S1. cmarkit markdown decode is exponential on nested link brackets (DoS)

- `fetch/lib/media_cmarkit.ml:8`, `proffer/lib/media_cmarkit.ml:8` decode
  attacker-controlled bodies with `Cmarkit.Doc.of_string` (cmarkit 0.4.0),
  which has no nesting bound and runs ~2^n on balanced `[...[...]...]`.
  Measured: depth 24 → 1.3 s, depth 26 → 5.3 s, depth 28 → >20 s. A
  **~60-byte** body pins a domain core effectively forever; body-size limits
  are useless against it.
- Reachable both ways: a Proffer route using `Proffer.Markdown` accepts the
  POST (handler runs on the connection fiber → whole domain stalls); a Fetch
  client decoding `Fetch.Markdown` from an untrusted server hangs.
- Upstream flaw in cmarkit's link-stack handling; mitigation needs an
  upstream fix or a fuel/timeout wrapper.

### S2. proffer_httpz checks streamed length after the bytes are on the wire

- `proffer/httpz/proffer_httpz.ml:292-309` (`write_outcome`, `Stream`
  branch): the handler's writer runs to completion and is flushed, then
  `body_written` is compared to the declared `Content-Length` and
  `invalid_arg` raised — too late. Verified on the wire: a handler declaring
  `~length:3L` and writing `"ok\n" ^ "HTTP/1.1 200 OK\r\n...\r\nowned"`
  delivers the forged response bytes downstream under a keep-alive head.
- Requires an app-level length bug (e.g. proxying upstream bytes with a
  precomputed length), but the library has the budget at sink time and
  doesn't enforce it. The exception path closes the connection, so reuse
  poisoning is prevented — but bytes already sent are not retractable.
- Fix: enforce the declared budget inside `sink_for` and abort at the first
  excess byte (the client side already does exactly this with `Limited`,
  `fetch/httpz/fetch_httpz.ml:230-257`).

### S3. Cookie `is_ip` misses hex/octal IP spellings (cross-address cookie leak)

- `httpz/cookie/lib/cookie.ml:105-113` recognizes only `:`-containing or
  all-digits-and-dots hosts. Verified: a server reached as `http://0x7f.1/`
  can set `Domain=1`, which is stored as a suffix-matching domain cookie and
  then sent to other `.1`-suffixed spellings such as `0xc0.0.0.1`
  (192.0.0.1). `check_public_suffix` skips the PSL lookup because
  `is_ip "1"` is true. Sibling of first-pass finding 2 (url.ml host
  spellings); `Redirect.is_ipv4` has the same narrowness but fails closed.
- Fix: `domain_suffix_matches` should use a parser-grade IP check, not a
  character-class heuristic.

### S4. Preconditions on unsafe methods silently dropped (no 412)

- `proffer/lib/backend.ml:90-114` (`not_modified`) returns `false` for any
  non-GET/HEAD method; `If-Match`, `If-Unmodified-Since`, `If-Range` are not
  implemented at all. RFC 9110 §13.1.2 requires 412 on a matching
  `If-None-Match` for PUT/DELETE. Verified: `PUT` with `If-None-Match: "v1"`
  against a matching etag returns 200 — optimistic-concurrency protection
  silently absent while both sides believe it was honored. The mli documents
  only GET/HEAD behavior. Also: a *future* `If-Modified-Since` yields 304
  (RFC 9110 §13.1.3 says ignore it), letting a client pin 304s indefinitely
  (`backend.ml:108-113`).

### S5. `Fetch.Sse.subscribe` close can wedge the owning switch permanently

- `fetch/lib/fetch.ml:897-906,1028-1039`: `finish ~block:true` performs a
  blocking, cancellation-protected `Eio.Stream.add` of `` `End``. If the
  consumer closes the subscription while the stream is at capacity and stops
  draining, the daemon never finishes; `Switch.await_idle` waits for daemons
  via an uncancellable path, so the owning scope can never end — outer
  cancellation does not recover it. The mli says `` `End`` is the final item
  "when the stream has room"; only the `~block:false` path checks room.
  Repro: `audit/repro_sse.exe` hangs with exit 2.
- Fix: guard all `finish` paths with the room check.

### S6. A raising `on_error` kills the whole proffer server

- `proffer/httpz/proffer_httpz.ml:586-590` passes user `on_error` straight
  into `Eio.Net.run_server`, which calls it inside the connection fiber — a
  raise fails the server switch, closing the listener. Amplifier:
  `Backend.run` calls `on_error` before writing the 500
  (`proffer/lib/backend.ml:189,218`); if it raises there, `accept_fork`
  calls it again. Verified: after one handler failure with a raising
  `on_error`, fresh connections are refused. Fix: wrap user callbacks in
  `try … with`, or document the must-not-raise contract loudly.

### S7. Typed-field parser findings (`fetch/lib/header.ml`)

Downstream-consumer risks (nothing in-repo makes decisions on these outputs):

- **`Vary: *, Accept` mis-parsed** (`header.ml:656-665`): `*` in a
  multi-member list yields `` `Fields ["*"; "accept"] ``; RFC 9111 §4.1
  requires "never match" for any `*` member. A cache on this codec serves
  responses the origin marked unreusable.
- **HSTS uses the last header occurrence** (`header.ml:15-20,1064-1100`);
  RFC 6797 §8.1 mandates the first. An attacker who can append a header can
  strip HSTS with `max-age=0`.
- **`WWW-Authenticate`**: `Basic abc"def` accepted as a token68 blob (the
  `param_of rest = None` fallback at `header.ml:947` bypasses the check);
  orphaned params before any scheme silently dropped.
- **`Authorization: Basic`** base64 is non-canonical (mid-blob `=` accepted)
  and falls back to `` `Other `` instead of rejecting.
- **`Link`** splits on commas inside `<...>` targets, silently dropping
  comma-containing link values (`header.ml:1175-1181`) — pagination/prefetch
  loss for JMAP-style consumers.
- **`Content-Language`** accepts arbitrary garbage (`header.ml:238-241`).

## New low/informational findings

- SSE server `retry:` value replaces client backoff with no floor or ceiling:
  `retry: 0` drives a reconnect storm, a huge value wedges the subscriber for
  days (`fetch.ml:955-967`).
- jsont has no nesting-depth cap; decodes survive 8M-deep nesting at the
  default 16 MiB limit (~2x margin), but an application raising `~limit`
  converts deep nesting into `Stack_overflow`
  (`jsont/bytesrw/jsont_bytesrw.ml:424,461`).
- 100 Continue is written before the body-size check, so an oversized
  request gets 100 then 413 (`proffer_httpz.ml:376-384`). Legal, wasteful,
  no desync.
- Refusals (400/408/413) close with the client body unread — Linux RST can
  destroy the error response before the client reads it
  (`proffer_httpz.ml:204-208,447-452,517-526`). A bounded drain would help.
- No graceful shutdown: `Proffer_httpz.run` does not accept `~stop`
  (cancellation-only, truncates in-flight writes).
- `fetch_curl` DNS may block the Eio loop domain if the libcurl build uses
  the synchronous resolver (`NOSIGNAL` only fixes SIGALRM); threaded/c-ares
  builds are fine. `fetch_httpz` resolves in a systhread pool — clean.
- `on_event` raising is swallowed on the routed path but kills the
  connection fiber on the refusal path — inconsistent, both safe.
- `negotiate.ml:20-29` accepts `q=nan`/`q=inf`/`q=2`/hex floats; benign
  ordering effects only (`nan` sorts below everything, `inf`/`2` rank
  top — self-inflicted), but non-finite and >1 values should be rejected.
- `proffer/example/9-auth/auth.ml:16` compares credentials with plain string
  equality — a timing oracle in code users will copy.
- punycode decode has no label-length cap and is O(n²) (`punycode.ml:241-354`)
  — public-API hazard only, not reachable with remote input in-tree.
- `Buf_write.int`/`hex` misbehave on negative input (documented precondition,
  all in-tree callers honor it; the module is publicly exposed).
- `Req.forwarded_for` takes the *first* X-Forwarded-For entry — spoofable
  unless the trusted proxy strips client-supplied XFF (mli warns; the warning
  could name "strips" explicitly).

## Swept clean in the second pass

- Every other unchecked read (`peek`/`unsafe_get`/`unsafe_get_int32/64`)
  across `parser.ml`, `span.ml`, `chunk.ml`, `target.ml`, `etag.ml`,
  `range.ml`, `scan_portable.ml`, `uriz_raw.ml`, `header_name.ml`, the other
  date forms, punycode, pubsuffix, cookie dates — all guarded; date.ml
  rfc850 remains the only gap.
- Every other limit-vs-parsed-region check (trailers, interim 1xx, curl
  header cap, SSE block accounting, fetch_httpz head/body caps counting
  decoded bytes) — correct pattern everywhere else.
- Every other Int64↔int conversion and numeric parser (`range.ml`,
  `span.ml`, `chunk.ml`, `fetch_curl.ml:953`, `proffer_httpz.ml:381`,
  header.ml `dec_int*`/`Age` saturation/`Retry-After`, cache_control
  `checked_seconds`) — overflow-safe.
- Cookie §5.2/§5.3/§5.4 state transitions (Max-Age, Domain stripping, Path
  defaulting, creation-time preservation, ordering, Secure-over-HTTPS,
  prefixes) — conformant; only the public-suffix step-5 bug deviates.
- Concurrency: timeout race, pipelined boundaries (verified by wire repros
  including Expect+coalesced), `max_connections` accounting exactly once per
  accept, crash isolation per connection fiber, fetch_httpz socket release on
  every exit path, curl engine poisoning/pinning/cancellation, `with_limits`
  semaphore fairness, shared `drain_buf` — all sound.
- proffer: conditional GET/HEAD semantics (69 test checks), bodyless-status
  enforcement, `Resp.v` validation unbypassable (private record — verified
  compile failure), Cache-Control is a *generator* not a parser, Location
  validation, no sniffing, mid-stream producer exceptions safely close.
- httnope-reported ETag/Range/Accept/Content-Digest failures were checked
  and are correct strict rejections, not bugs; Content-Digest duplicate-key
  last-wins is RFC 9651-conformant.

# Third pass (2026-09-02, same day)

Remaining unaudited surfaces: gzip/compression decode path, media-codec
framework, fetch client internals (credential/form/retry/SSE parser/protocol
edges), proffer mounting/query/target-form semantics, and the supply-chain
angle (PSL freshness, dependency bounds, vendoring, CI).

## New confirmed findings

### T1. Vendored Public Suffix List is 268 days stale (cross-tenant cookie leak)

- `httpz/pubsuffix/data/public_suffix_list.dat` is VERSION
  `2025-12-08_08-06-01_UTC`. Diffed against upstream master today: **252 rules
  added, 67 removed**. Missing high-impact multi-tenant additions include
  `claude.app`, `chatgpt.site`, `codepen.app`, `file.core.windows.net`,
  `*.aivencloud.com`, regional `elasticbeanstalk.com`, `*.rds.amazonaws.com`,
  `appwrite.network`.
- Impact: `cookie.ml:373-383` rejects `Domain=<public suffix>` only when the
  PSL knows the suffix. For any suffix added after the snapshot, a response
  from `foo.claude.app` can set `Domain=claude.app` and the jar attaches the
  cookie to every `*.claude.app` tenant — the exact cross-tenant leak the PSL
  check exists to prevent. The 67 removals cause over-rejection (availability
  only).
- Regeneration is fully manual (`httpz/pubsuffix/README.md:41-46`), no
  automation or staleness alert exists, and the cram test pins
  `Total rules: 10064`, adding churn friction to each refresh. Upstream PR
  publicsuffix/list#2959 (adds `cloud.run`) is not yet merged — a tracked
  update cadence is needed, not a one-off bump.

### T2. fetch-httpz rejects any gzip response whose fixed header is split across reads

- decompress 1.6.0 `Gz.Inf.header` requires all 10 fixed-header bytes in one
  input window and returns `` `Await `` without consuming partial bytes;
  `fetch/httpz/fetch_httpz.ml:160-169` follows the documented contract and
  refills from offset 0, discarding the unconsumed prefix → the next window
  parses as a header from the wrong offset → `Protocol_error`. Verified:
  dribbling 1–9 header bytes into a separate TCP segment fails all nine
  splits. Decode correctness depends on the peer's write segmentation — this
  almost certainly explains both httnope gzip failures. Availability, not
  integrity (every split errors; no wrong bytes).
- Companion decompress 1.6.0 bugs (upstream, mirage/decompress): FEXTRA XLEN
  read big-endian (RFC 1952: little-endian) → valid BGZF/dictzip traffic
  rejected; FHCRC compared against the wrong bytes → all valid FHCRC traffic
  rejected; reserved FLG bits not rejected (zlib rejects).
- Parity note: libcurl silently ACCEPTS a gzip body truncated before its
  CRC/ISIZE trailer (verified via fetch-curl and the curl CLI) — integrity
  gap; fetch-httpz rejects all truncations. fetch-curl.mli documents decoder
  behavior as inherited from libcurl.

### T3. No CI, and the tree is permanently red

- No `.github/` or any CI config. The two intentionally-red regression tests
  for first-pass finding 3 make `dune runtest` exit 1, so a permanently-red
  suite will mask every future regression. Mark them expected-fail and add
  minimal CI (`dune build @install && dune runtest`).
- Fuzzing covers only punycode (crowbar, manual `@fuzz` alias, `crowbar` not
  declared in any opam file — a fresh `--deps-only --with-test` build fails).
  No fuzz harness for the HTTP parser, chunk decoder, or cookie parser.
- Correction to pass 1: mdx tests DO run under `@runtest` in the current
  tree (verified empirically); the earlier note about `cookies.md` was wrong.

### T4. Vendored forks with no provenance or sync process

- `httpz/cookie`, `httpz/punycode`, `httpz/pubsuffix` have no recorded
  upstream source or version; git history begins at the reorganize commit and
  local patches have landed since — effectively untracked forks. Upstream
  security fixes would go unnoticed. Add a `VENDORED.md` (upstream repo +
  commit per component). The PSL *data* is the honorable exception
  (VERSION+COMMIT embedded).

### T5. Smaller code findings

- `on_event` hands credentials to the logging callback: the event record
  carries the full raw target (query tokens included) and ALL request headers
  incl. `Authorization`/`Cookie` (`proffer_httpz.ml:40-51,421-433`); the
  shipped `3-log` example logs `event.target`. No redaction helper.
- `Credential.Query` secrets appear unredacted in `pp_request`/trace output
  (`middleware.ml:117-122,204-211`); header credentials are redacted, query
  credentials are not.
- SSE: leading U+FEFF BOM not stripped (`fetch.ml:681-855`) — a
  BOM-producing server silently loses the first event (WHATWG requires
  stripping one BOM). No UTF-8 validation of event data (spec wants U+FFFD).
- `with_retry` ignores `Retry-After` in HTTP-date form (`fetch.ml:302-305` —
  only `` `Seconds`` honored); docs claim Retry-After is honored.
- `with_retry` drain failure escapes instead of retrying (`fetch.ml:363-372`):
  a truncated retryable-body propagates raw; the SSE path guards exactly this
  (fetch.ml:1000-1002) — inconsistent.
- `Authorization: Basic` builder doesn't reject a `:` in the username
  (`header.ml:855`, RFC 7617) — credentials silently authenticate as a
  different user/pass pair.
- proffer mock backend skips wire-level validation (`proffer_mock.ml:35-41`):
  `%zz` targets, CR in headers, 1 MB bodies, absolute-form targets all behave
  differently than the real backend — security-sensitive tests green against
  the mock say nothing about production. Document the divergence.
- `206 Partial Content` freely constructible with no Content-Range
  validation (`resp.ml` blocks only CL/TE/Connection) — a 206 without valid
  Content-Range confuses caches. `range.ml` is confirmed not wired into
  proffer at all.
- Status mapping: `Uri_too_long` → 400 (RFC wants 414),
  `Unsupported_transfer_encoding` → 400 (RFC wants 501), and HTTP/1.0
  parse-error rejections answered with an `HTTP/1.1` status line
  (`proffer_httpz.ml:516-527`).
- `with_auth` stale scope after remounting fails open silently
  (`site.ml:65-97`); `Req.target` returns the raw target, so absolute-form
  + naive redirect building = open redirect by misuse; duplicate
  `Authorization` fields are first-wins here while some intermediaries take
  the last.
- `Expect`/`TE`/`Upgrade` are not reserved headers; an app can set them via
  `with_headers` (no desync — backend forces close and rejects 101 — but
  degraded semantics). CONNECT is constructible but sent origin-form; dead
  functionality.
- Dependency hygiene: `httpz`/`proffer-httpz` leave `base`/`uunf`/`fmt`/
  `ptime`/`eio`/`cstruct` unconstrained while siblings pin; cmarkit `>=
  0.4.0` and 0.4.0 is still the newest release — the S1 exponential DoS has
  no fixed upstream to pin against (file upstream, ship the fuel wrapper);
  `jsont` 0.4.0 exists while pinned `>= 0.3.0`; this machine's opam repo
  snapshot is dated 2025-08-14 (12.5 months stale) — run `opam update`
  before trusting local freshness queries.
- `FOR-JMAP.md` is stale (says SSE doesn't exist; it landed).

## Swept clean in the third pass

- gzip: multi-member decode byte-exact (incl. cross-buffer and empty
  members), trailing junk always rejected in the current tree, CRC/ISIZE
  verified per member, cap sits above the decoder (decoded bytes counted),
  `x-gzip` handling safe, header stripping correct below the framing layer.
- `media.ml`: matching, `*+json` suffix rule, parameter CRLF checks,
  `Fetch.decode` limit plumbing (no string-vs-reader bypass), cmarkit
  benchmarks across 23 other pathological construct families all linear.
- credential attach path, multipart boundary scan (total, all part types,
  split-boundary safe), retry jitter (`env#secure_random`), retried-error
  set, SSE parser beyond BOM (line splitting, max_event accounting,
  Last-Event-ID persistence), HEAD/204/304/1xx bodyless handling,
  absolute-form never emitted client-side, request line pinned to 1.1,
  duplicate-Location first-wins parity across backends, no auto-Referer.
- proffer: mounting cannot shadow routes, auth gate wraps 404/405 under
  scope (no route/method inference), `/%61dmin` does not dodge scope, query
  duplicate keys first-wins consistently, CONNECT/OPTIONS-*/absolute-form
  handled sanely (the absolute-form/Host cross-check landed in-tree during
  this pass — the old known finding is fixed), HTTP/1.0 keep-alive correct,
  generated error bodies never reflect request bytes, etag/date generation
  safe.
- PSL generator embeds upstream VERSION+COMMIT; PRIVATE section included;
  decompress/uri/ocurl at newest opam releases; TLS caller-injected by
  design; no binary blobs; generated opam files in sync.

# Resolution ledger (2026-09-02)

Status of every finding above after the fix pass. "Fixed" means the code
changed and a regression test was added in the package's own suite; the
whole tree builds and every suite passes.

## First pass

- **F1** Fixed. `Cookie.check_public_suffix` stores host-only when the
  Domain attribute is a public suffix equal to the host, rejects otherwise,
  and fails closed on a lookup error. The test that codified the old
  behaviour was corrected.
- **F2** Fixed. New `Httpz.Ip` parses every `inet_aton(3)` spelling;
  `Fetch.Url` canonicalizes hosts to lowercase A-labels without a root dot
  and IPv4 to the dotted quad, and rejects malformed IPv6 literals. DNS
  rebinding and `::ffff:` mapping are documented as `~connect`'s job.
- **F3** Fixed. `Httpz.parse` checks `body_off` against `max_header_size`
  after parsing the head, as `Res.parse` does.
- **F4** Fixed. `Fetch_httpz.v` takes `?clock`, `?connect_timeout` (30 s)
  and `?idle_timeout` (60 s); `std` wires the environment's clock. Without a
  clock nothing is bounded, and the interface says so.
- **F5** Fixed. `Negotiate` parses q-values by the RFC 9110 grammar, drops
  `q=0`, honours wildcard ranges, and answers 406 when an `Accept` field
  matches nothing.
- **F6** Fixed. `Cache.create ?max_entries ~ttl ()` bounds the cache (default
  1024) with LRU eviction; lookups are logarithmic.
- **F7** Fixed. `parse_rfc850` guards the remaining length. A second bug
  surfaced by the truncation sweep: `parse_day` compared an offset to a
  length, so every asctime date at a non-zero offset was Invalid; fixed.
- **F8** Fixed. Empty `Host` is rejected; an absolute-form target whose
  authority differs from `Host` is rejected as an invalid header.
- **F9** Refuted after a wire test: libcurl decodes every coding it was
  built with regardless of what was negotiated and fails the transfer on
  one it lacks, so a coded body never reaches the caller. The predicate is
  unchanged and now documented; a brotli case is in `curl.md`.
- **F10** Public-suffix fail-closed, jar byte cap over name+value+path+
  domain, `Cookie.v` validation, TLS check before connect, server
  `write_timeout`, `Range` boundary from a self-seeded state, and
  `Buf_write.int`/`hex` rejecting negatives: fixed. Router normalization,
  HTTP/1.x minor versions and leading zeros: documented as deliberate.
  32-bit `Max-Age`, `looks_like_ipv6` (replaced by the strict parser), and
  decode-error logging: documented.

## Second pass

- **S1** Mitigated. `Markdown.markdown ?max_bracket_depth` (default 16)
  refuses a body whose bracket nesting exceeds the bound before cmarkit
  runs. An upstream fix to cmarkit's inline parser is being prepared
  separately.
- **S2** Fixed. The proffer-httpz sink charges the declared budget before
  each write and aborts on the first excess byte.
- **S3** Fixed. `Cookie.is_ip` is `Httpz.Ip.is_literal`, and a Domain that is
  an IP literal must equal the host.
- **S4** Fixed. `Backend` evaluates If-Match, If-Unmodified-Since,
  If-None-Match and If-Modified-Since in RFC 9110 §13.2.2 order for every
  method, answers 412 on failure, and ignores a future If-Modified-Since
  when the backend supplies `~now`. If-Range remains unevaluated.
- **S5** Fixed. `Sse.subscribe` delivers `` `End `` only when the stream has
  room, on every shutdown path.
- **S6** Fixed. `on_error` and `on_event` are wrapped; a raising callback is
  reported and the listener keeps serving.
- **S7** Fixed. `Vary` with any `*` member is `` `Any ``; single-valued
  codecs decode the first occurrence; `WWW-Authenticate` rejects non-token68
  blobs and orphan parameters; `Authorization: Basic` requires canonical
  base64; `Link` splits outside `<...>`; `Content-Language` validates tags.
- **Low** SSE `retry:` clamped to `[0.1 s, backoff_max]`; 100 Continue after
  the size check; bounded drain before closing on a refusal; `?stop` on
  `Proffer_httpz.run`; `Route.int` strict; `Req.forwarded_for` doc;
  constant-time compare in the auth example; punycode label cap on
  `to_unicode`; negotiate q validation. jsont depth and the curl
  synchronous-resolver caveat are documented.

## Third pass

- **T1** Fixed. PSL refreshed to `2026-09-02_06-03-53_UTC` (10064 → 10249
  rules; `claude.app` and the other named suffixes now recognized). A
  freshness script (`httpz/pubsuffix/check_psl_freshness.sh`, alias
  `@psl-check`) and a weekly workflow (`.github/workflows/psl.yml`) flag
  drift.
- **T2** Fixed. The inflater keeps the decoder's unconsumed suffix across
  refills, so a gzip member split at any byte decodes; a member whose input
  ends exactly at the header is retried rather than rejected. The remaining
  decompress 1.6.0 defects (FEXTRA, FHCRC, reserved flags) fail closed and
  are documented.
- **T3** Fixed. `.github/workflows/ci.yml` builds, tests and builds docs on
  OCaml 5.5; the previously red tests pass; crowbar harnesses for
  `Httpz.parse`, chunk decoding, cookie parsing and date parsing live in
  `httpz/fuzz` (alias `@fuzz`), and ran ~20 s each without a finding.
- **T4** Fixed. `VENDORED.md` records upstream repositories for the three
  components and states that the imported commit is unknown.
- **T5** Fixed: event header redaction and `path`-based logging example;
  query credentials redacted in traces; SSE BOM stripped; `Retry-After`
  HTTP-date honoured when `with_retry` has a wall clock; drain failures
  retried; `Basic` builder rejects `:` in the user; reserved `Expect`/`TE`/
  `Upgrade`/`Connection`; CONNECT refused; 414 and 501 mappings; 206 requires
  a valid `Content-Range`; mock divergence and `Req.target`/duplicate
  `Authorization` documented; `mount` already refuses a gated sub-site (test
  added); dependency lower bounds; `FOR-JMAP.md` refreshed. The HTTP/1.1
  status line on a 1.0 rejection is permitted by RFC 9112 §2.5 and is
  documented rather than changed.

## Form support (same date)

Not a finding, but done in the same pass: `Httpz.Urlencoded` and
`Httpz.Media.form` implement the WHATWG serializer and parser;
`Httpz.Multipart` parses `multipart/form-data` with header and part caps;
`Fetch.Form` escapes names and filenames per the WHATWG algorithm, checks
the boundary against part headers, and salts it per body; `Proffer.Req`
gains `is_form` and `form_result`, and `Proffer.Multipart` exposes uploads
to handlers within the backend's request window.

## Upstream

cmarkit's exponential bracket nesting is fixed on branch
`fix-bracket-nesting-blowup` of a local clone (`../cmarkit`, commit
`6a64f63`), pending an upstream pull request. The in-tree bracket-depth
guard stays until a release carrying that fix can be required.
