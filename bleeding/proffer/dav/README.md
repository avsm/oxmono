# proffer.dav

`Proffer_dav` is an explicitly mounted WebDAV server. The outbound client is
[`Fetch_dav`](../../fetch/dav/README.md). Applications using the former client
alias must change `Proffer_dav` to `Fetch_dav` and link `fetch.dav`.
[`Httpz_dav`](../../httpz/dav/README.md) supplies the pure protocol codecs.

Installing these libraries, linking them or starting an ordinary Proffer site
exports nothing. A caller must construct a reader or writer, choose an origin,
mount and security policy, and call `Proffer_dav.mount`. Constructors do not
open a listener. `proffer.dav.eio` adds the Eio storage adapters.

## Capabilities and activation

An export receives a `Reader.t` or a `Writer.t`, never a global filesystem or
network environment. The Eio constructors consume an explicitly supplied
`Eio.Path.t` and switch. Keep that switch alive throughout serving.

A read-only mount can be assembled as follows. `security` is an application's
credential verifier and export authorization policy. The listener must use
TLS with a certificate for the configured origin.

```ocaml
let reader = Proffer_dav_eio.reader ~sw export_path in
let export = Proffer_dav.read_only
  ~origin:"https://files.example.org" ~at:["files"] ~security reader in
let site = Proffer.Site.of_routes []
  |> Proffer_dav.mount ~at:["files"] (fun export -> export) in
Proffer_httpz.run ~sw ~tls stdenv ~env:export site
```

`Reader.t` has stat, listing and scoped reading operations. It has no mutation
callback. `read_only` denies every write method before reading its body or
calling the store. `Security.public_read_only` is an explicit alternative to
authentication. Public writable exports cannot be constructed.

`Security.authenticated ~realm ~authenticate ~authorize` receives exactly one
Authorization field. The application returns a stable principal and its
`Read_only` or `Read_write` grant, or rejects access. Use a bounded verifier
with constant-time credential comparison. Callback failures produce a generic
503 and do not expose exception text. Authorization is checked again under
the write transaction. The current grant covers the whole export. Use separate
exports for distinct access sets. Locks are bound to the creating principal.

`Site.with_auth` gates endpoint admission and execution. `Site.with_headers`
wraps endpoint responses. Prefixes cannot overlap other admitted endpoints.
An admitted endpoint reserves its subtree ahead of ordinary routes.

Transport provenance comes from `Proffer.Req.transport`, which the HTTPz
backend derives from the accepted connection. HTTPS exports require actual
TLS. `Forwarded`, `X-Forwarded-Proto`, Host and the target cannot grant it.
The explicit `allow_insecure_loopback` option also requires a loopback peer
and a loopback HTTP origin. TLS termination at a proxy is not supported by
this adapter. Run TLS through to Proffer.

## Confined storage

The Eio adapter currently requires Linux `openat2`, a native Eio directory and
procfs descriptor reopening. It fails during construction when unavailable.
Requests use paths relative to the opened directory descriptor with
`RESOLVE_BENEATH`, `RESOLVE_NO_SYMLINKS` and `RESOLVE_NO_XDEV`. Special files,
symlinks and multiply linked regular files are rejected before content reads.
No request-derived pathname is resolved against the process working directory.

The ordinary-directory reader issues weak metadata ETags and streams through
a scoped descriptor. It does not hash every listed file. An externally
modified directory is a live view, not a snapshot. Publish an immutable
snapshot when consistent enumeration and stable file contents are required.

The writer uses a **private managed store**, not an in-place writable export
of a normal directory. Pass an empty directory with mode 0700, owned by the
server's OS user, with `create:true` exactly once. Later starts use
`create:false`. Keep other local writers out of it.

Immutable content objects and one atomic XML manifest represent the public
namespace, dead properties and leases. An upload streams into an exclusive
private object while hashing it. Under one Eio mutex, the server rechecks
permissions, conditions, all affected locks and quotas, then publishes the
manifest after syncing the object. Failed conditions leave public state
unchanged. COPY can share an immutable object without creating filesystem
hardlinks. Readers keep their opened object alive across replacement.

An OS file lease prevents a second cooperating writer. Recovery validates the
manifest and referenced content hashes before listening, preserves unexpired
leases and removes owned abandoned staging objects. Unknown files and corrupt
committed data cause startup failure. A directory-sync failure after publication
poisons the running writer. The client must reconcile an uncertain result.
The manifest is versioned as `urn:proffer:dav:store:1`.

Provide explicit `quota` and server `limits`. File bytes, aggregate logical
storage, concurrent staging bytes, entries, metadata, active requests, XML
nodes/depth/bytes and response expansion are bounded. Current lock caps are
1024 per store, 64 per principal, a 4096-byte owner and a 3600-second lease.
The lease clock is monotonic while running. Startup refuses a wall clock
older than the last committed timestamp.

## Protocol behavior

Read-only exports support OPTIONS, GET on files, HEAD, and PROPFIND at depth
0 or 1. Writers additionally support PUT, MKCOL without a body, DELETE, COPY,
MOVE, ordered atomic PROPPATCH, exclusive LOCK, refresh and UNLOCK. OPTIONS
advertises DAV class 1, plus class 2 for a lock-capable store. These tests are
interoperability evidence, not certification against every RFC requirement.

HTTP conditions and DAV `If` lists are evaluated against current state.
Mutation conditions run inside the publication transaction. Strong file ETags
come back on PUT. COPY/MOVE require one same-origin, same-export Destination.
No operation follows a redirect, contacts an outbound URL or trusts owner XML
as a location to fetch. Resource names are decoded once and reject dot
segments, encoded separators, invalid UTF-8, controls and ambiguous empty
segments. Resource queries and fragments are rejected.

PROPFIND and PROPPATCH preserve XML namespaces, language and mixed content.
A missing property is an individual 404. Protected properties fail the whole
patch with 403/424 propstats. XML parsing disables DTDs and external entities,
and response generation has its own bound. Unbounded PROPFIND receives 403
with `DAV:propfind-finite-depth`.

The HTTPz backend admits these requests before reading a body or sending
100 Continue. PUT accepts fixed and chunked bodies larger than the parser
window. Inputs become invalid when the handler returns. Unread or malformed
input closes the connection. Ordinary routes retain buffered-body behavior.
The backend's request and socket-write deadlines also apply. Authorization,
Cookie, DAV `If` and Lock-Token values are redacted in backend events.

## Tests

From the monorepo root, using the OxCaml switch:

```sh
opam exec --switch=5.2.0+ox -- dune build \
  bleeding/proffer/dav/test/fixture.exe \
  bleeding/fetch/dav/integration/test_proffer.exe
opam exec --switch=5.2.0+ox -- dune runtest --force \
  bleeding/httpz/dav/test bleeding/fetch/dav/test \
  bleeding/proffer/dav/test bleeding/proffer/test
python3 bleeding/proffer/dav/test/run_docker.py
```

In either standalone HTTPz tree omit `bleeding/`. Use its normal switch for
the stock OCaml port. The runner builds a pinned Python image and runs
`webdavclient3` plus independent raw HTTP/XML checks over Linux host networking.
Containers run without capabilities, with a read-only root filesystem and a
bounded temporary directory. Only a generated public certificate is mounted
for the TLS phase. The tests never use production credentials or directories.

The runner exercises disabled defaults, read-only access, independent file
operations, conditions, properties, lock ownership and expiry, traversal,
links and special files, chunked streaming, concurrent create-only writes,
Fetch-to-Proffer integration, verified TLS, restart persistence, orphan cleanup,
second-writer refusal and corrupt-store refusal. `--keep` retains logs and
private disposable stores. Failures retain them automatically.

The [validation record](test/VALIDATION.md) lists the tested builds, checks
and verification limits.

## Remaining work

- Shared locks, infinite-depth PROPFIND, ranges, resumable upload and collection
  GET representations are not implemented. GET ignores Range and returns the
  whole file. HEAD on collections supports existence checks.
- REPORT, sync tokens, CalDAV, CardDAV, ACL, versioning and extended MKCOL are
  client-side features or future server extensions. They are not advertised.
- Authentication is supplied by the application. There is no credential
  database, Digest/Bearer challenge negotiation, per-path ACL, dedicated
  verifier deadline, per-principal request rate limiter or admin unlock API.
- The writer is single-process and assumes exclusive local ownership. There
  is no shared database, multi-process transaction service, migration/import
  tool for ordinary directories, or non-Linux hardened adapter.
- Quotas count logical file lengths conservatively even when COPY shares a
  physical object. No physical-block reservation protects against an unrelated
  process filling the volume. An administrator must provide storage headroom.
- The suite does not yet inject every filesystem failure or power loss between
  fsync/rename steps, mount namespace races, reparenting races, clock rollback,
  or domain-level contention. Independent clients currently cover Python and
  Fetch, not Rust, litmus, Finder or Windows Explorer. These are release-review
  tasks, especially before offering a writable public service.

The [design specification](../DAV_SERVER_SPEC.md) records the broader target.
The [review ledger](../DAV_REVIEW.md) tracks completed fixes and remaining work.
