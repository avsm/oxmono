## v0.1.0 (unreleased)

First release, of the `proffer`, `proffer.mock` and `proffer-httpz` libraries.

- A handler returns a `Resp.t` describing a response rather than writing one,
  over a `Req.t` whose path, query, headers and form body are decoded once.
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
