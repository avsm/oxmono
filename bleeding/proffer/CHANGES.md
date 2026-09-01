## v0.1.0 (unreleased)

First release.

- Add declarative response construction, typed path routing, site wrappers,
  content negotiation, cache policies, and conditional GET and HEAD handling.
- Add `proffer.mock` for exercising sites without opening sockets.
- Add `proffer-httpz`, an Eio HTTP/1.1 server with bounded concurrency, request
  deadlines, streaming responses, and structured request events.
- Validate response fields and entity-tags before they reach the wire.
- Add helpers for static-file confinement, MIME types, redirects, and a
  concurrent expiring response cache.
- Write routes as paths joined by `( / )` from `root`, ending in `rest` to
  capture the remainder. Sites are served as they are, so `Compiled` is gone,
  and `Proffer_httpz.run` takes the Eio environment with defaults for the
  address, listening message and error reporting.
- Add typed bodies through `Httpz.Media` codecs: `Req.decode`, `Resp.encode`,
  `Resp.encode_seq`, `Route.with_body`, `Negotiate.select` and
  `Negotiate.encode`. Codecs and Jsont descriptions are portable values that
  routes can capture directly; domain-bound application state remains in the
  environment.
- Include Jsont, JSON Lines, CommonMark, and HTML codecs in the main library.
- Prove the response path free of heap allocation with OxCaml's checker under
  the `release-check` profile: `Backend.handle_unboxed`, `Resp.v`, and the
  `proffer-httpz` request loop are `[@zero_alloc]`. Conditional requests are
  evaluated over the request block in place, routing runs the matcher without
  an accumulator, decorators run the handler rather than wrap it, and
  `Backend.outcome` carries `last_modified` as a time for the backend to
  write. `Proffer_httpz` events reach `on_event` at `local`;
  `globalize_event` copies one that must outlive the callback. Request
  strings, route captures, the optional arguments of `Resp.v`, content
  negotiation and the `with_auth` check are local to the request as well;
  `Req.globalize` copies a string a handler keeps. Only the `bytes`/`Cstruct`
  boundary, the application's own callbacks, and the Allow value of a 405
  stay on the heap.
