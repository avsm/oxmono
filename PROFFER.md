# Proffer

A portable, declarative Eio layer for serving HTTP responses. Proffer is to
servers what `fetch` is to clients: a small capability-style core with
composable wrappers, independent of any HTTP implementation, with
interchangeable backends. Where fetch narrows a client before requests
leave, proffer decorates a site value before responses leave.

The design has been validated against the oxcaml mode system on the
vendored, annotated Eio in `vendor/eio`. A compiled site crosses domains
under a compiler-checked `Domain_manager.run`, and the spikes in
`bleeding/proffer/spike` exercise the whole shape on real sockets. See
the Portability groundwork section for what was proved and how.

## Packages

- `proffer`. The portable core. Depends on Eio for types only. Contains no
  wire code and no sockets. Every export is `portable`.
- `proffer-httpz`. Backend over the `httpz` core parser and writer. The
  performance target. `httpz.route` and `httpz.eio_server` retire into
  this backend once its consumers (arod, bushel_web, httpz-perma-proxy)
  migrate.
- `proffer-cohttp`. Backend over cohttp-eio.
- `proffer.mock`. Runs a compiled site against synthetic requests with no
  sockets, in the manner of `fetch_mock`. Route handlers become directly
  testable, including their conditional-request behaviour.

## Design

### Responses are data

A handler returns a description of a response. It does not write one.

```ocaml
type 'env handler = 'env -> Req.t -> Resp.t

Resp.html ~cache:(Cache_control.public ~max_age:(`Hours 1)) html

Resp.v ~status:`OK
  ~etag:(`Strong hash) ~last_modified:mtime
  ~cache:Cache_control.(public ~max_age:(`Days 365) ~immutable:true)
  (Body.string s)
```

The backend implements the protocol mechanics against that description:

- HEAD. The router matches HEAD to GET routes. The backend sends headers
  with the correct Content-Length and no body. A `Delayed` body is never
  generated for HEAD.
- Conditional GET per RFC 9110. When a response carries `etag` or
  `last_modified`, the backend evaluates `If-None-Match` (list and `*`
  forms, weak comparison) and, failing that, `If-Modified-Since`, and
  answers 304 itself. A 304 carries the validators, `Cache-Control` and
  `Vary` of the full response, and never forces a `Delayed` body.
- Cache-Control. `Cache_control.t` is typed data (`no_store`, `private'`,
  `public ~max_age ?s_maxage ?stale_while_revalidate ?must_revalidate
  ?immutable`), serialized to its header value once, not per request.
- Vary. Negotiation combinators add it automatically.

Compression is out of scope. Deployment fronts the origin with Caddy,
which compresses. The origin's job is correct validators and cache
policy. `Req.t` exposes forwarded-for and forwarded-proto accessors for
the same reason.

Typed request-header access reuses the `Header` GADT approach from fetch
so both libraries read the same way.

### Bodies

```ocaml
type body =
  | Empty
  | String of string
  | Delayed of { length : int64 or_null; gen : unit -> string }
  | Stream of { length : int64 or_null; write : Sink.t -> unit }
```

`Stream` is sent chunked when `length` is unknown. `Sink.t` is a small
abstract writer each backend provides over its own output path. It is
valid only during the `write` callback and must not escape it. SSE rides
on it: `Resp.sse (fun send -> ...)` sets `text/event-stream` and
`no_store` and holds the fiber open until the handler returns or the
client disconnects, which surfaces as an exception from `send`.

A `Resp.t` lives and dies on the domain that built it, so its closures
need no mode discipline. Only responses stored in the compiled site
cross domains, and those are pre-rendered to closure-free data at
compile time (see Compilation and domains).

### Routing

Patterns use a final encoding so captures become curried handler
arguments, in the style of the `routes` library, replacing
`Httpz_route`'s nested tuples.

```ocaml
let routes = [
  get nil                          (fun env _req -> Pages.index env);
  get (s "papers" / str /? nil)    (fun id env req -> Pages.paper env id req);
  get (s "static" /* rest)         (fun segs env req -> ...);
  post (s "api" / s "search" /? nil) (fun env req -> Api.search env req);
]
```

`('f, 'r) pat` threads the handler type, so `s "papers" / str /? nil`
demands a handler `string -> 'env -> Req.t -> Resp.t`. Converters are
`str`, `int'`, `conv ~name of_string`, and `rest` for tail capture.

Route constructors take the handler at `portable`, so a site is portable
by construction and `compile` needs no separate check. The compiler
rejects a handler that captures unshareable state at the point of
registration, which is where the fix belongs.

Compilation flattens patterns into a segment trie. Literal matching in
the httpz backend walks spans in the parse buffer without allocating. A
capture is the only copy.

A generic `route meth pat handler` covers unusual methods. The WebDAV
constructors of `Httpz_route` have no current consumer and are not
carried over.

### Sites and wrappers

Wrappers scope by path prefix, in deliberate symmetry with fetch's URL
prefix scopes.

```ocaml
Site.of_routes routes
|> Site.mount ~at:["api"] api_site
|> Site.static ~at:["assets"] ~cache:Cache_control.(public ~max_age:(`Days 365) ~immutable:true) `Embedded
|> Site.with_cache ~scope:[[]] shared_cache
|> Site.with_auth ~scope:[["stats"]] ~realm:"stats" ~check
|> Site.with_headers [security_headers]
|> Site.with_fallback (fun env req -> Pages.not_found env req)
```

`Site.static` is data in the site. Each backend maps it to its native
implementation. The httpz backend already has one with subtree
confinement, ETag, Range, 206 and 416. The cohttp backend ports the same
contract. Range handling for dynamic bodies is out of scope for v1.

Content negotiation is a combinator:

```ocaml
get (s "notes" / str /? nil) (fun slug -> Negotiate.v [
  `Html,     (fun env req -> Resp.html (render_html env slug));
  `Markdown, (fun env req -> Resp.media "text/markdown" (render_md env slug));
])
```

The library parses q-values once, adds `Vary: Accept`, and caches each
variant under its own key.

`with_cache` composes with the conditional machinery. A cached entry
stores the body and its etag, so revalidation costs a hash compare and a
304, never a re-render.

### Environments as capabilities

Handlers are portable closures, so they cannot capture Eio resources or
other domain-bound state. They receive it instead: the `'env` value is
an argument, and the mode system constrains what a function captures,
not what it is given. A portable handler may therefore call any closure
reached through `env`, including ones built over unannotated APIs.

This makes `env` a capability record in the fetch style. Each domain
builds its own at startup:

```ocaml
type env = {
  ctx : Arod.Ctx.t;                              (* immutable site data *)
  cache : Proffer.Cache.t;                       (* shared, crosses domains *)
  read_asset : string -> string option;          (* over Eio.Path, this domain *)
  search : string -> Arod_search.result list;    (* over this domain's handle *)
}
```

Two styles coexist. Operations from the annotated Eio surface (`Net`,
`Flow`, `Time`, `Switch`, `Fiber`, `Promise`, `Stream`, `Mutex`) can be
called directly from handler code. Anything not yet annotated
(`Eio.Path`, `Buf_read`, third-party bindings) is wrapped in an env
operation instead. Widening the annotated surface shrinks env, it never
blocks the design.

### Compilation and domains

```ocaml
val compile : 'env Site.t -> 'env Compiled.t
(** [compile site] builds the dispatch trie and pre-renders every
    constant response: embedded assets and fixed pages become strings
    with their ETag computed and their full header block serialized
    once. [Compiled.t] holds only closure-free data and portable
    handlers, so it crosses domains by construction. *)
```

The per-backend entry point:

```ocaml
val run :
  sw:Switch.t -> net:_ Eio.Net.t ->
  ?domains:(_ Eio.Domain_manager.t * int) ->
  caps:'caps ->
  env:('caps -> int -> 'env) @ portable ->
  on_event:(Log.event -> unit) @ portable ->
  config -> 'env Compiled.t -> unit
```

The backend listens once, then starts each additional domain through the
checked `Eio.Domain_manager.run`. Exactly two values cross by audited
assertion inside the backend: the listening socket, which posix sockets
support because accept(2) is thread-safe and each operation suspends on
the calling domain's own scheduler, and `caps`, the application's bundle
of capabilities for `env`. Everything else the accept loop touches is
compiler-checked. The `env` factory runs once per domain with the domain
index, so per-domain resources are cheap and uncontended.

`on_event` is called on the domain that served the request, so it must
be portable. An application whose sink is domain-bound, such as arod's
sqlite access log, bridges through a shared queue drained by a fiber on
the starting domain.

### The shared cache

`Proffer.Cache` replaces `Arod_cache` and is fully checked, with no
assertions:

```ocaml
type t                          (* crosses domains *)
type entry = { body : string; etag : string; expires : float }

val create : ttl:float -> t
val memoize : t -> now:float -> key:string -> (unit -> entry) -> entry
val stats : t -> int * int      (* hits, misses *)
```

The implementation is an `Atomic.t` holding an immutable map built with
the stdlib's `Map.MakePortable`, updated by compare-and-set. Keys and
entries are strings and floats, so the whole value crosses every axis
without an unsafe cast. Two domains racing on a miss render twice and
one result wins, which is the right trade for a memoization cache.
Hit and miss counters are atomics.

### Errors and observability

A handler exception becomes a 500 through the backend's `on_error` hook,
which also receives transport errors. `Site.with_fallback` supplies the
404. Handlers never see sockets, so no transport concern leaks into
them.

`Log.event` is the portable successor of
`Httpz_eio_server.request_info`: remote address, method, target, status,
response size, duration, negotiated content type, cache status, and the
forwarded headers. All fields are immutable data, so events cross
domains freely.

## Backends

A backend consumes a `Compiled.t` and owns the wire. Its obligations:
parse requests, dispatch through the trie, apply the conditional and
HEAD logic against `Resp.t` metadata, write bodies, and emit one
`Log.event` per request.

- `proffer-httpz` keeps httpz's zero-allocation interior: per-connection
  buffers, trie matching over parse-buffer spans, and a fast path that
  answers pre-rendered constant responses with a single buffer blit of
  the precomputed header block and body. `Site.static` maps to the
  existing `Httpz_eio_server.Static` with its ETag and Range support.
- `proffer-cohttp` is a plain mapping onto cohttp-eio, single-domain
  first. It exists to keep the core honest about backend independence.
- `proffer.mock` dispatches a synthetic request through the same
  conditional machinery and returns the `Resp.t` plus the rendered
  header set, so a test can assert on a 304 or a `Vary` header without
  a socket.

The dynamic request path allocates one small `Resp.t`, which is noise
next to page rendering. Zero-allocation purism stays confined to the
httpz backend's parse and write paths.

## Porting arod

Handlers become `'env -> Req.t -> Resp.t` functions over the env record
sketched above. The port deletes, in `lib_handlers`, the `send_*` helper
block, `mime_type_of_path`, `check_stats_auth`, `wants_markdown`, the
`cached` wrapper, and every `is_head` test. `Arod_server.run` shrinks to
a `Proffer_httpz.run` call whose `on_event` feeds `Arod_log` through the
queue bridge. Dynamic pages gain ETag and 304 support through
`with_cache`, which they have never had. Handler tests run against
`proffer.mock` without sockets.

## Status and plan

Done: the Eio groundwork. Eio main is vendored with portable
annotations across the fiber core and most of the surface, its small
dependencies (`optint`, `lwt-dllist`, `hmap`) are vendored and
annotated, and the spikes prove the compiled-site crossing and the
checked multi-domain accept loop.

Also done, in a v1 form: the interface, the core with `proffer.mock`,
and the `proffer-httpz` backend, all tested. `Cache`, `Sse`,
`Negotiate`, `Site.mount` and the other combinators described above are
not implemented, and serving is single domain, so no caps cross and the
backend asserts nothing. `sortal serve` is the first consumer, with
`avsm/sortal/lib/web` a site whose portable handlers reach the store
through an env record of closures.

Next, in order:

1. Fill in the deferred core: `Cache`, `Site.mount` and the wrappers,
   `Negotiate`, `Sse`, `Range`.
2. Take `proffer-httpz` multi-domain, and port arod.
3. Implement `proffer-cohttp`.
4. Migrate bushel_web and httpz-perma-proxy, then fold `httpz.route`
   and `httpz.eio_server` into the backend.

In parallel and without blocking: upstream the Eio annotation patch,
and annotate `cstruct` and `fmt` so `Buf_read`, `Buf_write`, `Path` and
`Fs` join the portable surface and env records shrink.

## Portability groundwork in Eio

The design needs, under the oxcaml mode system: a compiled site that
crosses domains, an enforcing `Domain_manager.run`, and a domain-safe
cache. All three are established. The record below is the log of how,
kept because the findings shape the annotation idioms used above.

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

Compiler behaviour worth reporting upstream, each verified in
isolation:

- Item-level `@@ nonportable` cannot override a file-level `@@ portable`
  default inside a nested signature, and a nested `sig @@ ...` default
  does not help. This forced `net.mli`, `time.mli` and the `Exn` sig
  onto item-by-item annotation.
- `[@@unsafe_allow_any_mode_crossing]` is silently inert unless the
  declaration also repeats an explicit kind annotation. It neither
  satisfies an abstract kind nor crosses at use sites on its own.
- Module-level constants of abstract types read as `contended` from
  portable functions, and only the defining library can declare the
  crossing kind. This is what walls off `Mtime.Span.zero`, `Hmap.empty`
  and the runtime-events descriptors.
- An exception whose payload includes an extensible type can never be
  matched or constructed in portable code, which makes Eio's
  error-context idiom around `Exn.Io` unannotatable without assertions.
- `Effect.perform` is nonportable in `5.2.0+ox`, so effect-performing
  code cannot be annotated without re-declaring the primitive, pending
  `Effect.Safe`.

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

Verified by `bleeding/proffer/spike`:

- A record whose function fields carry the `@@ portable` modality, the
  shape of `Compiled.t`, crosses into `Domain_manager.run` and is
  checked. A closure capturing a `Buffer.t` is rejected with a clear
  contended-versus-uncontended diagnostic.
- The backend architecture runs end to end on real sockets: a backend
  whose signature demands a portable env factory and a portable site
  serves connections from two spawned domains. The second pass above
  upgraded this crossing from `unsafe_run` to the checked `run`.
