## v0.1.0 (unreleased)

First release, of the `proffer`, `proffer.mock` and `proffer-httpz` libraries.

- A handler is given a `Resp.respond` and calls it, rather than returning a
  response, over a `Req.t` whose path, query, headers and form body are
  decoded once. Nothing describing a response travels back up, so the
  description, its header block and the backend's outcome all live at `local`
  in the region `Backend.handle` runs the handler in, and answering a request
  allocates on the heap only what the body is made of.
- A header field name is a constructor rather than a string. `Headers.name`
  closes over the fields RFC 9110 names, with `Other of string @@ global` for
  a site's own. Comparison is constructor equality rather than a case-folding
  walk over two strings, a known name needs no validation because it is a
  token by construction, and a name cannot be misspelled at a call site.
  `Req.header` and `Headers.find` take a `name`; `Site.with_headers` and
  `Req.v` still take strings and map them through `Headers.of_string`.
- A header field is a record whose `value` is `global_`, so a block can sit on
  the stack while its values stay readable at the mode a socket write wants.
  `Headers.cat` and `Headers.vary` extend a block in the caller's region,
  which is how `Site.with_headers` and `Negotiate.v` add a field on the way
  past.
- `Backend.handle` reports three handler mistakes to `on_error`: returning
  without responding, which is answered 500; responding twice, where the
  second is dropped; and raising after responding, where nothing further is
  written. `Backend.run` is the same machinery without dispatch, which is what
  `proffer.mock`'s `describe` uses to exercise one response with no site.
- Path patterns built from `s`, `str`, `int`, `conv` and `rest` give each
  capture to the handler as a curried argument. Handlers are taken at
  `portable`, so a compiled site crosses domains by construction.
- Conditional GET per RFC 9110 and HEAD are decided in the core, so every
  backend agrees. A 304 or a HEAD never runs a delayed body generator.
- `Cache_control.t` is typed data, serialised to its header value when the
  policy is described rather than once per response.
- `Resp.v` raises `Invalid_argument` for a header name that is not a token, a
  value holding CR, LF or NUL, or an entity-tag holding a double quote.
- `proffer.mock` dispatches synthetic requests through the code a socket
  backend runs, and collects a streaming body into a buffer.
- `proffer-httpz` serves a compiled site over HTTP/1.1 on the httpz parser and
  Eio, a fibre per connection, and reports one log event per request.
- Proffer is a server layer over httpz rather than a library independent of
  any wire implementation. Methods, statuses, header names and HTTP dates are
  httpz's types, so `Proffer.Method.t = Httpz.Method.t`,
  `Proffer.Status.t = Httpz.Res.status` and
  `Proffer.Headers.name = Httpz.Header_name.t`. `proffer-httpz` has no
  conversion left in it at all: what used to be a thirty-one arm status match,
  a method match, and a header name mapped both ways through a string is now
  nothing.
- `Method.t` loses its `` `Other `` case. httpz's parser accepts the methods it
  names and rejects anything else, so a method that reaches a handler is
  always one of them, and a method the set lacks is added to httpz rather than
  modelled around. `Method.equal` is constructor equality.
- A request's block is built from the parser's fields in one pass instead of
  three, and a response's block is written where it lies, since httpz's
  writers take their strings at `local` and can emit a name it knows from a
  precomputed byte sequence. Measured on eight request fields and four
  response fields: 152 words to 80 per request for the block, and 24 words to
  0 per response, before counting the name string per known field that is no
  longer copied out of the parse buffer. The association list a log event
  carries is built only when a site asked for events.
- The request path decodes in place. `Pct` works over a range of the target
  rather than over pieces cut out of it first, and takes a range that holds no
  escape with one `String.sub` instead of a `Buffer`; and the query is parsed
  on demand, with `Req.query_param` scanning for its key rather than building
  the association list. Measured on
  `/notes/hello-there?tag=ocaml&limit=25` with three header fields, `Req.v`
  falls from 175 words to 33, one `query_param` costs 4 words and one that
  finds nothing costs none. Both changes are differentially tested against the
  implementations they replaced: 168392 comparisons for the decoder and 840406
  for the lookup, over an adversarial corpus, every byte value, and randomised
  input.
- Dispatch walks the path rather than a `string list` built for every
  request. A literal segment is compared where it lies and allocates nothing,
  only a capture allocates, and `rest` is the one arm that materialises a
  list. `Req.segments` stays as an on-demand accessor and no handler signature
  changed. Differentially tested against the segment-list semantics over 40046
  paths, comparing which route won and what it captured, with no differences.
- A request reaches a handler at `local`, so it costs no heap and cannot be
  stashed. Its strings are `global_` within the record, so a handler still has
  them at the mode the stdlib wants.
- `Headers.h_local` and `other_local` build a field in the caller's region.
  `stack_` on a list literal covers its cons cells and not the calls inside
  it, so a block written `stack_ [ h n v ]` still put every record on the
  heap. Every field the backend renders, and every one proffer's own
  constructors build, now goes through the local form.
- Route selection allocates nothing. `Backend`'s scan took its method and
  path from closures and returned a `(handler option * method list)` tuple,
  which cost 13 words on every request before a route was even looked at. It
  now takes what it needs as arguments and hands the result to a local
  continuation, so nothing is boxed on the way out. A route match still costs
  the `Some` `Route.run` returns.
- `Backend.outcome`'s body is a declared variant carrying `@@ global` on its
  payloads, rather than a polymorphic variant in a `global_` field. A socket
  needs the string and the writer at global, not the block naming which of
  them it is, and `global_` on the field forced the whole thing to the heap.
  Worth 8 words a response. `` `String s `` becomes `String s` and
  `` `Stream (length, write) `` becomes `Stream { length; write }`, which is a
  breaking change for anyone who has written a backend.
- A route match allocates nothing. `Route.run` returns `'env handler or_null`
  rather than `'env handler option`: a handler is a closure and so never null,
  and `or_null` needs no box to say so.
- `Resp.v`'s validation no longer builds a closure. The check that a typed
  argument does not collide with a field in the block went through a
  `Headers.exists` taking a closure over the name, which was a heap block on
  every response naming a content type. It is a direct scan now, and worth 5
  words a response.
- `Resp.v` takes `~content_type` required, as `string or_null` rather than
  `?content_type:string`. Same reason `~headers` is required: an optional
  argument's payload arrives local and cannot reach the `global_` field a
  header value lives in. Worth 2 words whenever the content type is a runtime
  value rather than a module-level constant, which is every `Resp.media` and
  every cached page. The sugar constructors are unchanged.
- `Body.Sink` holds its `emit_sub` as an `or_null` and writes the
  string-copying fallback at the use site, rather than building a defaulting
  closure over `emit`. A sink is 3 words whether or not a backend supplies
  `emit_sub`, where it used to be 8 without.
- Together with the decoder work: a complete request and response cycle for a
  literal route allocates **2 words**, where `Req.v` alone used to cost 175.
  A 404 is 2 and a streamed body 8.
  `bleeding/proffer/bench/bench_alloc.exe` prints these and attributes them.
- A response body goes to the socket through a 64 KB scratch owned by the
  connection rather than one `Cstruct.of_string` of the whole body, so the
  bigstring a large response needs is bounded per connection instead of per
  request. A body that fits in the scratch is still a single `writev` carrying
  the head with it. Streamed chunks take the same path.
- `Body.Sink` takes bytes as well as strings. `Sink.write_sub t b ~off ~len`
  is the way in for a producer that writes through a buffer, which is every
  encoder: it hands over the encoder's own slice rather than making a string
  per slice. A backend that can only take strings omits `Backend.sink`'s new
  `?emit_sub` and pays the copy, which is what the mock does.
- `Resp.stream respond ct write` responds with whatever `write` emits, for a
  body that is produced rather than held. Without a `?length` the backend
  frames it chunked, since an encoder does not know its size before it runs.
- Two behaviour changes come with httpz's date handling. The obsolete RFC 850
  and asctime forms of a date are now accepted, as RFC 9110 section 5.6.7
  requires and proffer's own parser did not. And `Date.representable` stops at
  0001-01-01 rather than the proleptic 0000-01-01, which is where httpz's
  formatter clamps.
- 422 is spelled `Unprocessable Entity` on the status line, httpz's phrase,
  where proffer used RFC 9110's `Unprocessable Content`. The reason phrase is
  advisory and no client parses it.
- Reads in `proffer-httpz` carry an idle and a request deadline, and the number
  of connections open at once is capped, so a slow client cannot hold a fibre
  and its buffers indefinitely.
- `Mime.of_path` is the Content-Type a filename's extension names, with case
  folded, and `application/octet-stream` for an extension it does not know.
- `Static.confine` joins the segments of a tail capture only when none of them
  can leave the subtree, and `Static.v` describes a served directory as a root
  label and a cache policy for a backend to resolve.
- `Cache.memoize` returns a rendered body and a weak entity-tag over it, keyed
  by string and expiring after the cache's TTL, so a revalidation costs a tag
  compare rather than a re-render. `Cache.stats` is the hit and miss counts. A
  cache crosses domains. Every miss drops the entries that have expired, so a
  cache under request-derived keys costs what the distinct keys of one TTL
  window cost rather than growing for the life of the process.
- `Negotiate.v` answers with the variant the client's Accept header ranks
  highest, falling back to the first variant offered, and adds `Vary: Accept`.
  `Negotiate.of_accept` is the parsed preference order on its own.
- `Route.moved` and `Route.found` answer GET, and HEAD, at a capture-free
  pattern with a 301 or a 302 to a fixed location.
- `Site.mount` adds another site's routes under a path prefix. It raises
  `Invalid_argument` for a sub-site that has been through `with_auth` or
  `with_headers`, since mounting takes the routes alone and would drop the
  wrapper.
- `Site.with_auth` gates every path under a prefix behind a check on the
  Authorization field, and answers 401 where that scope would have given a 404
  or a 405, so credentials are needed to learn which paths name a route.
  `Site.with_headers` adds fields to every response the site gives.
  `Site.with_auth` raises `Invalid_argument` for an empty scope, which would
  otherwise gate nothing and serve the site open. Pass `[[]]` for the whole
  site.
- `Resp.vary` adds a name to a response's Vary field and `Resp.add_headers`
  appends fields the response does not already set.
- `Proffer_httpz.event` gains the request path, every request field in the
  order it arrived, the response Content-Type and the handler's `X-Cache`
  value, which is what an access log records.
- `proffer-httpz` hands a handler the request fields in arrival order, so a
  repeated field such as Authorization reads through `Req.header` as the first
  one sent, as it does on every other backend.
