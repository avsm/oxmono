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
