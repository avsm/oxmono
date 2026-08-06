# Proffer

A portable, declarative Eio layer for serving HTTP responses. Proffer is to
servers what `fetch` is to clients: a small capability-style core with
composable wrappers, independent of any HTTP implementation, with
interchangeable backends. Where fetch narrows a client before requests leave,
proffer decorates a site value before responses leave.

## Packages

- `proffer`. The portable core. Depends on Eio for types only. Contains no
  wire code and no sockets. Every export is `portable`.
- `proffer-httpz`. Backend over the `httpz` core parser and writer. The
  performance target. `httpz.route` and `httpz.eio_server` retire into this
  backend once its consumers (arod, bushel_web, httpz-perma-proxy) migrate.
- `proffer-cohttp`. Backend over cohttp-eio.
- `proffer.mock`. Runs a compiled site against synthetic requests with no
  sockets, in the manner of `fetch_mock`. Route handlers become directly
  testable.

## Design

### Responses are data

A handler returns a description of a response. It does not write one.

```ocaml
type 'env handler = 'env -> Req.t -> Resp.t

Resp.html ~cache:(Cache.public ~max_age:(`Hours 1)) html

Resp.v ~status:`OK
  ~etag:(Strong hash) ~last_modified:mtime
  ~cache:Cache.(public ~max_age:(`Days 365) ~immutable:true)
  (Body.string s)
```

The backend implements the protocol mechanics against that description:

- HEAD. The router matches HEAD to GET routes. The backend sends headers
  with the correct Content-Length and no body. A `Delayed` body is never
  generated for HEAD.
- Conditional GET (RFC 7232). When a response carries `etag` or
  `last_modified`, the backend compares If-None-Match and If-Modified-Since
  and answers 304 itself, without forcing a delayed body.
- Cache-Control. `Cache.t` is typed data (`no_store`, `private'`,
  `public ~max_age ?s_maxage ?stale_while_revalidate ?must_revalidate
  ?immutable`), serialized once at compile time.
- Vary. Negotiation combinators add it automatically.

Compression is out of scope. Deployment fronts the origin with Caddy, which
compresses. The origin's job is correct validators and cache policy.

### Bodies

```ocaml
type body =
  | Empty
  | String of string
  | Delayed of { length : int64 or_null; gen : unit -> string }
  | Stream of { length : int64 or_null; write : Sink.t -> unit }
```

`Stream` is sent chunked when `length` is unknown. `Sink.t` is a small
abstract writer each backend provides over its own output path. SSE rides on
it: `Resp.sse (fun send -> ...)` sets `text/event-stream` and `no_store` and
holds the fiber open.

### Routing

Patterns use a final encoding so captures become curried handler arguments,
in the style of the `routes` library, replacing `Httpz_route`'s nested
tuples.

```ocaml
let routes = [
  get nil                          (fun env _req -> Pages.index env);
  get (s "papers" / str /? nil)    (fun id env req -> Pages.paper env id req);
  get (s "static" /* rest)         (fun segs env req -> ...);
  post (s "api" / s "search" /? nil) (fun env req -> Api.search env req);
]
```

`('f, 'r) pat` threads the handler type, so `s "papers" / str /? nil` demands
a handler `string -> 'env -> Req.t -> Resp.t`. Converters are `str`, `int'`,
`conv ~name of_string`, and `rest` for tail capture. Compilation flattens
patterns into a segment trie. Literal matching in the httpz backend walks
spans in the parse buffer without allocating. A capture is the only copy.

A generic `route meth pat handler` covers unusual methods. The WebDAV
constructors of `Httpz_route` have no current consumer and are not carried
over.

### Sites and wrappers

Wrappers scope by path prefix, in deliberate symmetry with fetch's URL
prefix scopes.

```ocaml
Site.of_routes routes
|> Site.mount ~at:["api"] api_site
|> Site.static ~at:["assets"] ~cache:Cache.(public ~max_age:(`Days 365) ~immutable:true) `Embedded
|> Site.with_cache ~scope:[[]] shared_cache
|> Site.with_auth ~scope:[["stats"]] ~realm:"stats" ~check
|> Site.with_headers [security_headers]
|> Site.with_fallback (fun env req -> Pages.not_found env req)
```

`Site.static` is data in the site. Each backend maps it to its native
implementation. The httpz backend already has one with subtree confinement,
ETag, Range, 206 and 416. The cohttp backend ports the same contract.

Content negotiation is a combinator:

```ocaml
get (s "notes" / str /? nil) (fun slug -> Negotiate.v [
  `Html,     (fun env req -> Resp.html (render_html env slug));
  `Markdown, (fun env req -> Resp.media "text/markdown" (render_md env slug));
])
```

The library parses q-values once, adds `Vary: Accept`, and caches each
variant under its own key.

`with_cache` composes with the conditional machinery. A cached entry stores
the body and its etag, so revalidation costs a hash compare and a 304, never
a re-render.

### Compilation and domains

A compiled site is shared across domains, so handlers must be `portable`
closures. Application state that cannot be portable, such as Eio
capabilities, arrives through the `'env` parameter instead of capture. The
env thunk runs once per domain. State that must be genuinely shared lives in
`Proffer.Cache.t`, a portable, domain-safe memoization cache with TTL and
hit and miss counters, which replaces `Arod_cache`.

```ocaml
val compile : 'env Site.t -> 'env Compiled.t @ portable

val run :  (* per backend *)
  sw:Switch.t -> net:_ Eio.Net.t ->
  ?domains:(_ Eio.Domain_manager.t * int) ->
  env:(unit -> 'env) ->
  on_event:(Log.event @ local -> unit) ->
  config -> 'env Compiled.t -> unit
```

Compilation does real work. Constant responses get their ETag computed and
their full header block serialized once, so the httpz fast path for them is
a buffer blit. Dynamic requests allocate one small `Resp.t`, which is noise
next to page rendering.

`Log.event` is the portable successor of `Httpz_eio_server.request_info`.
Backends emit it to `on_event`, and arod's sqlite access log plugs in there.
`Req.t` exposes forwarded-for and forwarded-proto accessors, since
deployment sits behind Caddy.

Typed request-header access reuses the `Header` GADT approach from fetch so
both libraries read the same way.

### Error handling

A handler exception becomes a 500 through an `on_error` hook.
`Site.with_fallback` supplies the 404. Handlers never see sockets, so
transport errors belong entirely to backends.

## Porting arod

Handlers become `'env -> Req.t -> Resp.t` functions. The port deletes, in
`lib_handlers`, the `send_*` helper block, `mime_type_of_path`,
`check_stats_auth`, `wants_markdown`, the `cached` wrapper, and every
`is_head` test. `Arod_server.run` shrinks to a `Proffer_httpz.run` call
whose `on_event` feeds `Arod_log`. Dynamic pages gain ETag and 304 support
through `with_cache`, which they have never had. Handler tests run against
`proffer.mock` without sockets.

Migration order: proffer core with mock and tests, then proffer-httpz, then
the arod port, then proffer-cohttp, then bushel_web and httpz-perma-proxy,
after which `httpz.route` and `httpz.eio_server` fold into the backend.

## Portability prerequisites in Eio

The design requires, under the oxcaml mode system:

1. `compile` returns a value usable from any domain, so the route trie,
   policies, and handlers must cross. Handlers are portable closures over
   nothing, so this needs no Eio annotations.
2. Backends spawn per-domain accept loops through `Eio.Domain_manager`. For
   the type system to enforce rule 1, `Domain_manager.run` must demand a
   `portable` closure.
3. `Proffer.Cache` needs a domain-safe mutex. Either `Stdlib.Mutex` with
   oxcaml's `portable`/`contended` story, or an annotated `Eio.Mutex`.
4. Eio resources (`Flow`, `Path`, `Net`) stay per-domain, reached through
   `'env`, and need no annotations for the core design. Any annotation work
   there only widens what an env may share, it does not block proffer.

### Findings, second pass (2026-08-06): annotating the fiber core and beyond

A deep annotation pass over vendored Eio main established that the fiber
core and most of the surface API can be made fully `portable` on the
`5.2.0+ox` compiler. The checked spike (`bleeding/proffer/spike`) now
crosses domains through the enforcing `Domain_manager.run`, and the
per-domain accept loop calls `Switch.run`, `Net.accept` and
`Flow.copy_string` from checked-portable code. The single assertion left
in user code is `Obj.magic_portable` on the listening socket.

What the oxcaml compiler sources say about effects (`../oxcaml`):
upstream is moving to `Effect.Safe.perform`, which takes a
`Handler.t @ local` capability token (a zero-width type that crosses all
axes) handed down by `Deep.Safe.match_with`, plus a future `yielding`
mode axis for effects. Portability is not the axis that polices
effects. `Domain.Safe.spawn` and `Multicore.spawn_on` demand
`@ portable once (unyielding)` closures, which is exactly the contract
proffer's `Domain_manager.run` annotation enforces. Eio's `Peff` shim
(`external perform : 'a Effect.t -> 'a @@ portable = "%perform"`) is the
bridge until the workspace compiler ships `Effect.Safe`, at which point
the scheduler's `match_with` migrates to the token-passing form.

Annotated portable in vendored Eio: the whole core (`Switch`, `Fiber`,
`Promise`, `Cancel`, `Exn`, `Private`, `Cells`, `Broadcast`,
`Single_waiter`, `Trace`, `Debug.traceln`), and on the surface `Flow`,
`Net` (except `run_server`, `setsockopt` and the printers), `Time`,
`Resource`, `Stream`, `Eio_mutex`, `Semaphore`, `Condition`, `Pool`,
`Waiters`, `Sync`, `Hook` and `Executor_pool`'s dependencies. `Buf_read`,
`Buf_write`, `Path`, `Fs`, `File` and `Process` are not yet annotated.
They are blocked only on `cstruct` and `fmt` carrying annotations, not
on anything structural.

Dependency libraries vendored and annotated: `optint`, `lwt-dllist` and
`hmap` (ported to dune, key counter made atomic, switched to the
stdlib's `Map.MakePortable`). Vendored unmodified so the workspace links
one copy of optint: `uring`, `multibase`, `checkseum`, `decompress`,
`progress`. Still unannotated and shimmed at the Eio boundary instead:
`cstruct` (rebound in `flow.ml`), `mtime` (rebound in `time.ml`), `fmt`
(portable code uses `Format` or `Printf`, printers are opted out).

The backends need no annotations for this architecture. Backend
implementations are reached through resource vtables, and the crossing
assertion lives on `Resource.handler` (via
`[@@unsafe_allow_any_mode_crossing]`, justified because vtable slots
hold module-level functions while per-resource state stays inside the
mode-tracked `T` pair). A socket crossed by assertion then works from
any domain because posix `accept(2)` is thread-safe and each operation
suspends on the calling domain's own scheduler. `eio_posix`'s use of
`Domain.spawn` stays internal to the backend.

Assertions added inside Eio, each with a soundness argument in a
comment: `Peff.perform` and `Peff.dls_get_int`/`dls_set_int`, the
runtime-events writer and descriptors, `Exn.create`/`combine`/
`add_context`/`reraise_with_context` (the `Io` exception carries the
extensible `err` type, which can never cross), `Debug.traceln`, the
`reject` sentinel and `Hmap.empty` constants, and monomorphic error
helpers in `net.ml`.

Compiler behaviour worth reporting upstream, each isolated as a
standalone test case in `oxcaml-repro/` whose dune rules assert the
current behaviour and start failing when a compiler fix lands:

- Item-level `@@ nonportable` cannot override a file-level `@@ portable`
  default inside a nested signature, and a nested `sig @@ ...` default
  does not help. This forced `net.mli`, `time.mli` and the `Exn` sig
  onto item-by-item annotation (`oxcaml-repro/01`).
- `[@@unsafe_allow_any_mode_crossing]` is silently inert unless the
  declaration also repeats an explicit kind annotation. It neither
  satisfies an abstract kind nor crosses at use sites on its own
  (`oxcaml-repro/02`).
- Module-level constants of abstract types read as `contended` from
  portable functions, and only the defining library can declare the
  crossing kind. This is what walls off `Mtime.Span.zero`, `Hmap.empty`
  and the runtime-events descriptors (`oxcaml-repro/03`).
- An exception whose payload includes an extensible type can never be
  matched or constructed in portable code, which makes Eio's
  error-context idiom around `Exn.Io` unannotatable without assertions
  (`oxcaml-repro/04`).
- `Effect.perform` is nonportable in `5.2.0+ox`, so effect-performing
  code cannot be annotated without re-declaring the primitive, pending
  `Effect.Safe` (`oxcaml-repro/05`).

Two earlier suspicions did not survive minimisation and are withdrawn:
units without an mli do export inferred portability in their cmi, and a
module-level partial application of a portable function over crossing
arguments is inferred portable. The failures attributed to those causes
were really the abstract-constant case above.

### Findings, first pass (2026-08-06)

Upstream Eio main (commit `af471df`, 2026-08-05) is vendored in
`vendor/eio`, and cohttp-eio 6.2.1 in `vendor/cohttp-eio` so that it
rebuilds against the vendored Eio rather than the opam one. The whole
workspace builds and its tests pass on the `5.2.0+ox` compiler with no
compatibility patches. The `eio.1.3+ox` overlay patch set is obsolete.

Local modifications to `vendor/eio`:

- `Domain_manager.run` and `run_raw` now require a `portable` closure.
  A documented `unsafe_run` escape keeps the old contract. Eio's two
  internal callers, `Executor_pool.create` and `Net.run_server` with
  `additional_domains`, are grandfathered onto `unsafe_run`.
- `Resource.get`, `Resource.get_opt` and `Net.accept` carry `@@ portable`.
  Their implementations were already inferrable as portable, so the
  markers cost nothing and show the shallow half of the annotation work
  is mechanical.

Verified by `bleeding/proffer/spike`:

- A record whose function fields carry the `@@ portable` modality, the
  shape of `Compiled.t`, crosses into `Domain_manager.run` and is checked.
  A closure capturing a `Buffer.t` is rejected with a clear
  contended-versus-uncontended diagnostic.
- The backend architecture runs end to end today on real sockets: a
  backend whose signature demands `(mk_env @ portable)` and a portable
  site serves connections from two spawned domains. The second pass
  above upgraded this crossing from `unsafe_run` to the checked `run`.

Limits found, and why they do not block proffer:

- Eio API functions are `nonportable` by default, so checked-portable
  per-domain code cannot yet call `Switch.run`, `Fiber`, or `Flow`.
  `Switch.run` is not inferrable as portable because its implementation
  reaches `Cancel`, effects, and `Trace`. Annotating the scheduler core
  is a real upstream campaign, not a local patch.
- Eio resource values do not cross domains. `Resource.t` packs a vtable
  of unannotated function fields, and backend state would need kind
  assertions. Sharing one listening socket across domains therefore
  stays inside the audited backend escape, or each domain listens on its
  own `reuse_port` socket built from its env.

Conclusion: the design works now. Proffer's own signatures enforce
portability of handlers, compiled sites, and env factories, which is the
user-facing guarantee. The Eio annotation campaign only shrinks the
audited region inside backends, so it can proceed upstream at its own
pace.
