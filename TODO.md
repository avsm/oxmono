# Remaining OxCaml porting work

State as of 2026-08-22, branch `minus39`. Every claim here was compiler-probed
or measured in the session that wrote it. The per-field reasons in
`avsm/arod/lib_handlers/arod_render.mli` and `arod_env.mli` are probe
transcripts and are the authority if this file drifts.

Where things stand. Proffer is a server layer over httpz and **all 91 values
in its interface are `@@ portable`**. Arod serves `listing`, `entry`,
`entry_markdown`, `paper_bib`, `blogroll` and `sitemap` as portable handlers
over an immutable `Arod.Ctx.t`. `Arod_env.t` holds 11 fields: three are data
(`ctx`, `config`, `cache`) and eight are closures, of which four are renders
and four are domain-bound capabilities.

Both axes are now as far as they go without a design change. **Portable**:
four closures are left in `Arod_env.t`. `feed`, `pagination` and `search`
render through jsont, whose codecs cannot be given a crossing kind; `report`
holds a database handle.
**Local**: a complete request and response cycle allocates 2 words, where
`Req.v` alone cost 175 when this started.

## Closures

### 1. `sitemap` (done)

One hunk, a floating `@@ portable` on `vendor/sitemap/sitemap.mli`. Nothing
needed a kind, because `type url` is a private record rather than an abstract
type, so the compiler reads `immutable_data` off its fields. `sitemap.ml` is
unpatched: it holds no module-level table of the kind `vendor/xmlm` had to
replace with code. `avsm/arod/test/test_sitemap.ml` is the guard, and
`Arod_render.sitemap` is `@@ portable`, so the field is gone from
`Arod_env.t`. `render_capture.sh` reports `/sitemap.xml` byte-identical.

### 2. `feed`, `pagination` and `search` (blocked in jsont)

All three render through jsont: `feed` through jsonfeed's document model,
`pagination` and `search` through `Arod_json.stream`. A codec is a
module-level `Jsont.t`, and a portable handler reaches a module-level value
only if its type carries a crossing kind. `Jsont.t` cannot be given one, and
the ruling below says exactly where that stops.

A hand-rolled JSON writer was built and then removed. It worked, and the
escaping was differentially correct against jsont over 126823 comparisons,
but a bespoke JSON encoder in a site is the wrong thing to own. One closure in
`Arod_env.t` is cheaper than that, and that record exists for exactly this.

## The floor for portable

Four closures are left in `Arod_env.t`. `feed`, `pagination` and `search`
render through jsont. `report` holds a SQLite handle, which is bound to the
domain that opened it, so annotating the renderer alone would not free the
route. The rest of the record is either data (`ctx`, `config`, `cache`) or
genuinely domain-bound: `now` (clock), `log_search` (log source), `read_image`
and `read_paper` (confined Eio capabilities).

Off the render path, three things are still nonportable and none of them costs
anything where it sits. `Bushel.Md.extract_all_links` is declared
`@@ nonportable` in `bushel_md.mli` and is the one whole-document conversion
left, with no render consumer. `Astring` survives in `bushel_sync`. `fmt`
survives in `avsm/arod/lib_search/arod_search.ml`, which is behind the
domain-bound `search` closure anyway.

## What is left for local

The request and response paths are done. A complete cycle for a literal route
allocates **2 words**, where `Req.v` alone cost 175 when this started.

| | words |
| --- | --- |
| full serve, literal route, content type only | 2 |
| the same with an entity-tag | 8 |
| the same with a header field in a `stack_` block | 2 |
| a streamed body, written | 8 |
| a 404 | 2 |
| `Req.v` (record only, now on the stack) | 0 |
| one `Req.query_param` | 4, or 0 when absent |

`bleeding/proffer/bench/bench_alloc.exe` is where those come from, so they can
be rechecked rather than believed. It is a tool, not a test: the figures move
with the compiler and pinning them would fail a build for a reason nobody
wants to chase. Everything below was measured the same way and, where it
changed behaviour, differentially tested against what it replaced.

### 1. What was done

`Pct` decodes over a range of the target rather than over pieces cut out of it
first, and takes a range holding no escape with one `String.sub`. The query is
parsed on demand, with `query_param` scanning for its key. Dispatch walks the
path, so a literal segment is compared in place and only a capture allocates,
and its scan takes what it needs as arguments and hands its result to a
continuation, so route selection itself allocates nothing.
A request reaches a handler at `local` with `global_` strings, so it costs no
heap and cannot be stashed. And every header field on the response path is
built with `Headers.h_local`, which allocates in the caller's region.

### 2. `local` permits stack allocation, it does not cause it

The finding that made the rest work, and the one to remember. Annotating a
parameter `@ local` says the callee will not let the value escape; it says
nothing about where the caller puts it, and the caller's default is the heap.
Three things have to line up, and none is visible at a call site that omits
them:

- the caller writes `stack_`;
- the call is not in tail position, so it is written `let () = ... in ()`;
- the parameter is not optional, since an optional argument is passed as an
  allocated `Some` the block cannot cross. This one is silent: without
  `stack_` the call compiles and quietly heap-allocates.

And `stack_` covers the literal it is applied to, not the calls inside it, so
`stack_ [ h n v ]` needs `h` to return `exclave_` for the record to move too.
That is what `Headers.h_local` is.

`Resp.v` takes `~headers` required so it can be given a stack block; the sugar
constructors take `?headers` and cannot. Paths that answer every request use
the former, and say so at the call site.

### 3. What is left, attributed

An earlier version of this section guessed, and guessed wrong: it named the
description record, the outcome record and `Backend.run`'s two `ref`s as the
cost. Measured, that machinery is **0 words**. The compiler keeps all of it
in the region. Nothing in this file is worth believing that the bench does not
print.

`bench_alloc.exe` prints the attribution. Every word left is a box the
interface asks for:

| | words |
| --- | --- |
| `Backend.run` machinery, cheapest response | 0 |
| a string body and its content length | 2 |
| `~content_type` | 0 |
| dispatch, any number of routes, none matched | 0 |
| a route match | 0 |
| a capture pattern's partial application | 4 |
| `Backend.sink`, with or without `emit_sub` | 3 |

Three rules came out of this, and all three generalise past proffer.

**A closure that captures is a heap block, and a higher-order library
function forces one.** This was the largest cost in two separate places and
neither was visible at the call site. `Backend`'s route scan closed over the
method and the path, costing 13 words on every request. `Resp.v`'s
overlap check went through `Headers.exists` with a closure over the field
name, costing 5 on every response naming a content type. Both became direct
recursive scans taking what they need as arguments. Before reaching for a
mode, look for a closure.

**A `global_` field forces its whole block to the heap; a `global` modality
on a payload does not.** `Backend.outcome` held its body as a polymorphic
variant in a `global_` field, which cost 8 words. What a socket needs at
global is the string and the writer, not the block naming which of them it
is, so the body became a declared variant carrying `@@ global` on its
payloads and the field lost `global_`. The same reasoning applies anywhere a
local record has to hand a heap value onward.

**`or_null` is free where `option` is two words.** A value that can never be
null, which a closure or any block is, does not need a box to say so.
`Route.run` returns `'env handler or_null`, so a route match allocates
nothing. `Body.Sink` holds its `emit_sub` the same way, with the fallback
written at the use site rather than built as a defaulting closure, which took
a sink from 8 words to 3.

**An optional argument's payload arrives local, and that is what costs, not
the calling convention.** The `Some` itself is stack-allocated when the callee
does not let it escape, measured at 0. What forces it to the heap is landing
in a `global_` field, and a header value has to be global because it reaches
a socket. So `Resp.v` takes `~content_type` required and as `string or_null`,
alongside the already-required `~headers`. Worth 2 words whenever the content
type is a runtime value; a module-level constant was already static.

Two things were tried and reverted because they measured nothing, which is
worth recording so they are not tried again. Putting `@@ global` on `Body.t`'s
payloads and dropping `global_` from `Resp.description.body` changed no
figure. Replacing `Option.iter (check_value "content_type")` in `Resp.v`,
a partial application and so a closure, changed no figure either. A measured
figure decides these, not the shape of the code.

What is left is three things, and none is worth a redesign.

1. **A string body and its content length, 2 words.** This is the whole cost
   of an ordinary response now.
2. **`Backend.sink`, 3 words.** The record itself, on streamed responses only.
   Removing it means taking the sink at `local` in `Body.Stream`'s writer,
   which changes every producer's signature for three words.
3. **A capture pattern, 4 words.** The partial application binding what the
   segment decoded to, inherent to the final encoding routes use.

`~etag`, `~cache` and `~last_modified` stay optional and cost a couple of
words each when given, for the reason in the third rule above. Making them
required would buy that back and is not worth the ergonomics on arguments
most routes never pass.

A streamed body costs 12 words over a string one, for the sink record and the
closure the outcome carries. Beyond that the remaining allocation is the
response body itself, which is what the next section is about.

### 4. The body is three orders of magnitude bigger than any of this

Worth stating plainly, because it decides where effort goes next. Measured
from the render capture, the routes that keep an `Arod_env.t` closure answer:

| Route | Body |
| --- | --- |
| `/api/entries?collection=network` | 1.35 MB |
| `/api/entries?collection=entries` | 503 KB |
| `/notes/feed.json` | 3.3 MB |
| `/news.xml` | 3.4 MB |
| a listing page | 768 KB |

Against that, everything under **What is left for local** above is about 1 KB
a request. So moding those closures, or the values they return, is not the
lever. Two things are.

**The copy on the way out (done).** Every string body used to leave as one
`Cstruct.of_string`, which mallocs and copies the whole body: a burst of
concurrent requests for the feed held a 3.3 MB bigstring each.
`proffer-httpz` now writes through a 64 KB scratch owned by the connection,
so that is bounded per connection rather than per request. A body that fits
in the scratch is still a single `writev` carrying the head with it, which is
most routes. Measured at 20 to 60 microseconds either way, so this buys
memory, not speed. All 1588 routes byte-identical.

**Building the string at all (JSON done, HTML not).** `pagination` and
`search` render a fresh body per request; `feed` and the pages are memoised,
so they pay once per TTL. Both now answer through `Resp.stream`.
`Arod_json.stream` drives `Jsont_bytesrw.encode` into a `Bytes.Writer.t` that
forwards each slice to `Body.Sink.write_sub`, so the JSON goes from the
encoder to the socket with nothing copied on the way and the encoded body
never exists as a string. Measured on a 111 KB search response: 9725 words
allocated, against 40077 through `encode_string`. All 1588 captured routes
byte-identical.

The two routes are framed chunked now, since the length is not known before
the encode runs. `render_capture.sh` cannot see that: curl decodes the
framing, so the capture compares bodies and a framing change is invisible to
it. Confirmed instead against a live server, which also showed a listing page
still carrying its Content-Length, HEAD reporting neither field, and two
requests answered on one connection, so keep-alive survives the change.

The stats views still go through `Arod_json.encode`. They answer from an
access log rather than the corpus, their bodies are small, and the route is
behind a database handle either way, so they are not the lever.

What is left of this is the HTML. `page_json`'s `html` member is still an
`El.to_string`, and on `collection=network` that member is most of the
1.35 MB. Streaming it means an incremental writer inside htmlit and a jsont
codec that can take a producer rather than a string for a member, which is a
larger change than the one above and is deliberately not taken yet.

## Settled questions. Do not reopen without new facts.

- **logs**: structurally blocked. `format_reporter`'s closure captures
  `Format.formatter`s, which can never be portable, and 20+ in-tree callers
  install reporters. The env-closure pattern is the answer. The compiler
  error is reproducible in minutes if doubted.
- **opam uri in `arod_ctx.ml`**: permanent. `normalise_url`'s
  decode-then-re-encode semantics are load-bearing for persisted feed
  annotation keys (`test_feed_annotations` pins it). Startup-only, never on
  a render path.
- **Re**: never vendor for portability. Compiled `Re.re` values carry
  internally mutable DFA caches mutated on execution. The references
  precompute at `Arod.Ctx` build made it startup-only.
- **jsont**: four of the five blockers fall to small, principled changes. The
  fifth is the kind system, not jsont. This entry replaces two earlier ones,
  the first saying it was structurally impossible and the second that it was
  merely expensive. Both were wrong in ways worth recording.

  To capture a module-level codec in a portable handler, `Jsont.t` needs a
  kind that crosses portability and contention. Probed against vendored jsont
  in a scratch workspace, these are what stand in the way and what each costs:

  1. `Repr.Rec : 'a t Lazy.t`. `lazy_t` has kind `value non_float` and can
     never cross contention, since two domains forcing one cell would race.
     **Solved**: `Basement.Portable_lazy.t` is declared
     `value mod contended portable` and is a drop-in.
  2. `Repr.String_map = Map.Make (String)`, whose `t` carries no kind.
     **Solved**: `Map.MakePortable`.
  3. Fifteen function-typed fields across nine record types. **Solved**: a
     `@@ portable` modality on each, which the compiler accepts, including on
     a GADT constructor payload.
  4. `Type.Id.t`, jsont's own pre-5.1 shim: a first-class module holding an
     extension constructor, which does not cross contention. **Solved**, and
     this one should go upstream: OxCaml is 5.2, `Stdlib.Type.Id.t` is already
     declared `immutable_data`, and jsont's own comment on the shim says
     "Can be removed once we require OCaml 5.1".
  5. **Not solved.** With all of the above in place, `Repr.t`'s kind computes
     as `immutable_data with 'a any_map with ('a, 'a) object_map with ...`,
     nine component type applications the solver will not discharge. They are
     records in one mutually recursive group with existential parameters, and
     annotating them individually reproduces the same conditional-kind problem
     one level down. This is not a defect in jsont: it is that a recursive
     GADT with existential intermediates cannot currently be given a crossing
     kind. No patch to jsont fixes it.

  So the codecs stay where they are and the routes that use them keep a
  closure. Reopen if the kind solver learns to discharge recursive component
  kinds, at which point items 1 to 4 are the patch and they are all small.

  bytesrw was probed too and has its own four blockers, two fixable and two
  not: `Slice.make_or_eod` returns `t @ contended` from the module-level `eod`
  sentinel; `Slice.pp` needs `bytesrw_fmt.mli` annotated; `Slice.tracer`
  defaults its `ppf` to `Format.err_formatter`, which the stdlib declares
  `@@ nonportable`, and a per-`val` override does not reach it because it sits
  two modules deep, where the enclosing floating annotation wins; and
  `Stream.error` raises a module-level exception whose payload is an
  extensible variant. All of that is moot while item 5 stands.

  Earlier work in this repository is not a head start. `opam/bytesrw` on
  `main`, last present at `761947088`, carries 18 `@ local` annotations and no
  `portable` at all. Local is stack allocation, a different axis, and none of
  it lifts a closure.

- **sqlite at render time**: declined by ruling. A db handle in a portable
  handler reintroduces the env-closure pattern. Precomputed immutable
  structures are the house answer for build-once data.

## Two upstream bugs found while vendoring

Both are in bytesrw 0.3.0, both reproduced against a pristine build from the
release sources, and both are fixed in `vendor/bytesrw` and pinned by
`avsm/arod/test/test_bytesrw.ml`. Neither has been reported upstream.

- `Bytes.Slice.equal` and `Bytes.Slice.compare` never read the last byte of
  two slices of equal length. `equal` answers `true` for `"a"` and `"b"`. The
  loop runs `while !cmp = 0 && !i < max` with `max = len - 1`.
- The `Bytes.Slice` formatters test the head cut with `len - 1 > max` and the
  empty case with `max < 0`, forgetting that a slice may start away from zero,
  so a truncated slice with `first > 0` printed without its ellipsis.

## Adjacent opportunities

- **Proffer on the stack** (done). A handler is given a `Resp.respond` and
  calls it rather than returning a value, so the description, its header block
  and the backend's outcome are all `local`. Three lessons worth keeping,
  since they will come up again. A curried function used at `local` groups its
  arrows, so an application reads as complete after the first argument: one
  record argument has no arrows to group. An optional argument is passed as an
  allocated `Some`, which a local block cannot cross, so the primitive takes
  its block required and the sugar forwards. And a local value cannot be an
  argument in a tail call, so every call handing one on is written
  `let () = ... in ()`.
- **Proffer over httpz** (done). Methods, statuses, header names and dates are
  httpz's types rather than copies, so `proffer-httpz` has no conversion left
  in it at all. `Method.t` lost its `` `Other `` case with it. What remains of
  the request path is under **What is left for local** above.
- **Multi-domain serving**: the strategic payoff of all the portability
  work, and the original PROFFER.md goal. `Proffer_httpz.run` through
  `Domain_manager` plus the queue-based log bridge. The comment in
  `avsm/arod/lib/server/arod_server.ml` marks the spot. Everything the
  render path touches now crosses domains. Verify with the same
  live-differential methodology (`avsm/arod/test/render_capture.sh`).
- **sortal_web onto htmlit** (S-M, deletes ~650 lines):
  `avsm/sortal/lib/web/html.ml` (258) and `pages.ml` (392) hand-roll HTML in
  `Buffer` solely because htmlit was nonportable when they were written. It is
  portable now, and sortal itself is already fully ported: no module-level
  mutable state, portable handlers, and on the continuation API. So this is a
  cleanup rather than a port. One consequence to plan for: sortal escapes `'`
  as `&#39;` and htmlit writes `&apos;`, so every page carrying an apostrophe
  changes. `test_web`'s 91 checks are substring-based and mostly survive
  that.
- **Off-path stragglers, low priority**: listed under **The floor for
  portable** above, with their current locations.

## Where the methodology lives

- Vendoring and annotation playbook: the READMEs under `vendor/base64`,
  `vendor/htmlit`, `vendor/ptime`, `vendor/xmlm`, `vendor/cmarkit`,
  `vendor/syndic`, `vendor/sitemap`, `vendor/jsonfeed`, `vendor/bytesrw` and
  `vendor/jsont`. Each carries a hunk inventory graded by provenance and a
  re-vendoring checklist.
- Behaviour oracles: `avsm/arod/test/test_md_golden.ml` (golden renders,
  never regenerate to make a test pass), `render_capture.sh` (full-site
  byte differential, 1588 routes, noise floor documented in its header),
  `link_predicate_diff.ml` (URL predicate corpus differential),
  `test_json.ml` (the search and pagination JSON byte for byte, including
  the escaping rule, on the routes `render_capture.sh` does not reach),
  `test_bytesrw.ml` (the two vendored bytesrw fixes, against oracles outside
  bytesrw).
- Allocation: `bleeding/proffer/bench/bench_alloc.exe`, which is where the
  word counts under **What is left for local** come from. A tool, not a test,
  for the reason given there.
- Portability guards: `test_payload_kinds.ml`, `test_cmarkit_portable.ml`
  and siblings. Guards must capture module-level values inside `@ portable`
  closures. Parameter-shaped ascriptions prove nothing.
