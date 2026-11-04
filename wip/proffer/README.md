# Proffer

Proffer defines HTTP sites, routes and response handlers. `proffer-httpz`
serves them through Eio. `proffer.mock` dispatches requests in memory.

The optional [`proffer.dav`](dav/README.md) library serves explicitly mounted
WebDAV exports. `proffer.dav.eio` supplies confined read-only directories and a
private transactional writer. No DAV route is enabled by default. Outbound
WebDAV operations belong to [`fetch.dav`](../fetch/dav/README.md).

The [`proffer.session`](session/proffer_session.mli) library keeps browser
sessions on the server. The browser holds only a 256-bit random ID in a
`__Host-` cookie that is `Secure`, `HttpOnly` and `SameSite=Lax`, and the
store is keyed by the ID's SHA-256, so a copy of the store cannot be replayed
as cookies. `create` always mints a new ID, so call it on login and `destroy`
the request's old session. A request carrying the cookie twice has no
session. Sessions end after two idle hours or twelve hours in all, by
default.

State-changing requests go through `check_unsafe`. It needs the session's
CSRF token, in the `_csrf` form field or an `X-CSRF-Token` field, and a request
whose `Sec-Fetch-Site` and `Origin` fields do not name another site. An
older browser that sends neither passes on the token alone. `Login` binds an
off-site login, such as an OAuth authorization, to the browser that began it
with a ten-minute nonce cookie. That cookie is `Lax`, not `Strict`, because the
return from the authorization server is a cross-site top-level navigation.

A site behind a TLS-terminating proxy sees plaintext connections. It still
sets `Secure` cookies, since its browsers use HTTPS, and it takes its origin
from configuration. `Host` and `X-Forwarded-Proto` are chosen by the client
and must not decide what requests are checked against.

Start with the [server and client examples](../../examples/README.md).
The [first Proffer lesson](../../examples/proffer/1-hello/README.md)
contains a complete program and its run command.

The main library includes JSON, JSON Lines, Markdown and HTML codecs.
Timeout and delay values use the external `Duration.t`. Add `duration` to
your Dune libraries when constructing these values.

`Mime.of_path` uses Proffer's built-in extension registry, with no heap
allocation per lookup. The private table in [mime_data.ml](lib/mime_data.ml)
preserves the mappings previously provided by magic-mime 1.3.1, including
Proffer's JavaScript, Markdown, AVIF and webmanifest overrides. To add a
mapping, insert its lowercase extension in sorted order and cover it in
[test_mime.ml](test/test_mime.ml).

The [public interface](lib/proffer.mli) describes the API. The
[repository guide](../../docs/http.md) describes library selection, setup
and backend limits. All examples are under the top-level `example/` directory.

Measure framework allocations with
`dune exec --profile release-check wip/proffer/bench/bench_alloc.exe`.
The writer discards output, so this excludes socket I/O. The
`Req.query_param_local` rows measure lookup after subtracting request creation.

Measure a real keep-alive connection with
`dune exec --profile release-check wip/proffer/bench/bench_httpz_alloc.exe -- 1000`.
This includes both the loopback client and server, Eio, clocks and socket I/O.
Connection setup and 100 warmup requests are excluded.

Request strings can be borrowed directly by `Resp.text`, `Resp.html`,
`Resp.media`, stream callbacks and delayed generators. These bodies finish
before the responder returns. A handler can echo `Req.body req` without
`Req.globalize`. Keep the response call non-tail when it borrows values from
that handler's region. Handoff callbacks retain global ownership because
connection takeover happens later.

`@ local` describes lifetimes, not a blanket allocation guarantee. Use
`stack_` for local wrappers and `Headers.h_local` or `Resp.h_local` for local
fields. Allocation checks exclude application callbacks at documented
boundaries. The borrowed-query response benchmark measures the synchronous
framework path separately from socket I/O.
