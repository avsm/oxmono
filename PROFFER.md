# Proffer in OxMono

Proffer defines HTTP sites and response policies. The `proffer-httpz` backend
serves them through Eio; `proffer.mock` exercises the same handlers in memory.
Arod and Sortal use this stack. The old HTTPz router, Eio server and static-server
executable have been retired after the permanent caching proxy was removed.

Start with the [HTTP guide](HTTPZ.md), the
[Proffer examples](example/README.md#proffer) and the
[package README](bleeding/proffer/README.md).

## Interfaces

- [Proffer](bleeding/proffer/lib/proffer.mli): routes, request access, responses,
  cache and authentication policies, streaming, SSE and media codecs.
- [HTTPz backend](bleeding/proffer/httpz/proffer_httpz.mli): Eio listener,
  deadlines, request bounds, TLS, response streaming and request events.
- [Mock backend](bleeding/proffer/mock/proffer_mock.mli): in-memory dispatch.

Portable handlers receive application resources through their environment.
Responses remain inside the handler callback and streams are consumed while
that callback owns the response. The HTTPz backend owns protocol framing and
connection cleanup. See the interfaces for the supported limits and lifecycle.

`Duration.t` represents timeouts and cache lifetimes. OxMono vendors portable
Duration and Mtime interfaces, so Proffer's cache and Eio clocks call them
directly. The cache no longer needs a portability assertion for duration
conversion.

## Validation and synchronization

Use `opam exec --switch=5.2.0+ox -- dune` with `--profile release-check` to
retain allocation checks. The Proffer and Fetch tests include real backend
exchanges and in-memory handler tests. Shared examples live in `example/`.

[HTTPZ_SYNC.md](HTTPZ_SYNC.md) records the standalone revision and intentional
monorepo differences. [HTTPZ_COMPARISON.md](HTTPZ_COMPARISON.md) records the
2026-09-07 comparison and removal of the legacy server stack. This document
supersedes the original implementation sketch; its earlier versions remain
in Git history.
