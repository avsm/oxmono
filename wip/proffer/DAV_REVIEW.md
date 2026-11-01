# DAV integration review and unfinished work

Updated 2026-09-08. The [server guide](dav/README.md) is the current API and
feature record. The historical findings below describe the pre-server baseline.

| Finding | Current result |
| --- | --- |
| R1, Destination authority | Every stacked Fetch URL restriction checks raw COPY/MOVE Destination before transport. The server checks both paths within its export. |
| R2, DAV read-only client | `Fetch_dav.read_only` permits discovery and downloads and rejects mutation. Server readers carry no writer callbacks. |
| Discovery fallback and response selection | Relative discovery locations resolve against the request URL. Authentication/errors propagate. Missing, foreign or duplicate matching resources are rejected. |
| Client facade and dependency | Removed. Fetch owns the client and Proffer owns the server. Proffer no longer depends on Fetch at runtime. |
| Server gating and capabilities | Explicit mounts, TLS provenance, admission before body intake, Eio path confinement and a private managed writer are implemented. |
| Incoming client additions | Preserved stock Mirror and opt-in lenient href parsing through dc18b69. Hardened mirror path/index validation, collection scope, collisions, atomic replacement, interrupted rebuilds and token progress. |
| Coverage | Pure codecs, mock policies, real HTTP, independent Python, Fetch, TLS and recovery tests. See the server guide for untested failure and platform cases. |

The broader [server specification](DAV_SERVER_SPEC.md) remains a design target.
Unfinished server and test work is listed in the server guide's remaining-work
section. Client mirror limits and ownership requirements are recorded in
[the Fetch DAV guide](../fetch/dav/README.md). The rest of this file preserves
the evidence and rationale behind the original findings.

The review covers OxMono at d0cd32b9b, standalone OxCaml at
9d59fac5d8f877969824d030c26c44d2898df98d, and the subsequent stock OCaml
extension at 38af268e089a09cec9b3e6eb6b08b1845e3883f7. The extension received
a source review of its integration and discovery behavior, not a full review
of every new codec.

## Exposure and package boundaries at the reviewed baseline

There is **no DAV server in Proffer today**. Linking proffer.dav does not
register handlers, advertise DAV, open a directory, select a transport or
start a listener. Its implementation is a single include of Fetch_dav.
Recognizing DAV method names in HTTPz does not make a resource accessible.

| Library | Existing responsibility | Authority |
| --- | --- | --- |
| [httpz.dav](../httpz/dav/README.md) | Bounded XML, DAV values, request encoders and response decoders | None; no Eio or transport dependency |
| [fetch.dav](../fetch/dav/README.md) | Outbound DAV client | Supplied Fetch client, narrowed by a declared DAV root |
| [proffer.dav](dav/README.md) | Re-export of that same client | Exactly the Fetch client authority |
| [proffer](lib/dune) | Routes, responses and dispatch | Application-supplied handler environment |
| [proffer.httpz](httpz/proffer_httpz.mli) | Eio HTTP listener and connection handling | Network and clocks; handler environment supplied separately |

The core Proffer library does not link Fetch or DAV. However, its opam package
now has an unconditional Fetch dependency in [dune-project](dune-project),
because it also ships the client facade. The library is optional to link;
the dependency is not optional to install. Keep that distinction explicit.

## Findings

### R1 — high: source-only Fetch restrictions are not DAV destination authority

[Fetch.restrict](../fetch/lib/client.ml) checks the request URL.
[Fetch_dav.transfer](../fetch/dav/fetch_dav.ml) checks Destination against its
own root, but cannot inspect restrictions hidden inside its supplied client.
A narrower inner Fetch prefix and a wider outer DAV root therefore compose
differently for sources and destinations:

~~~ocaml
let delegated =
  Fetch.restrict ~under:["https://example.test/allowed/"] backend
in
let dav = Fetch_dav.v ~root:"https://example.test/" delegated in
Fetch_dav.copy dav ~src:"allowed/file" ~dst:"outside/file" ()
~~~

An isolated mock-backend probe delivered both COPY and MOVE with this
out-of-prefix Destination. No real server was contacted. The DAV root still
works as documented; this is a delegation hazard, not a bypass of Fetch's
documented request-URL-only contract. The old investigation and current
[SPEC](../httpz/dav/SPEC.md) acknowledge source-only restrictions but the API
does not make safe delegation easy.

Before recommending writer delegation, mint a DAV-aware capability at the
authority owner's boundary. Its mandatory request filter must validate source
and Destination, reject ambiguous/repeated Destination fields, and be retained
by every subsequent narrowing. Passing only the narrower opaque DAV capability
prevents callers choosing a wider root. Alternatively, extend Fetch restrictions
to validate auxiliary request targets consistently, including raw COPY/MOVE;
do not let callers evade that check by omitting optional target metadata.
Select the smallest design that preserves composition and test it first.

A preflight GET/OPTIONS is not authorization for a later remote write.
A receiver must still independently authorize both affected resources.

### R2 — medium: the client has no DAV read-only helper

Fetch.read_only deliberately allows GET, HEAD and OPTIONS; PROPFIND is denied.
Fetch_dav.t exposes reads and mutations together. The probe confirmed the
PROPFIND rejection before the backend was called.

Add a DAV reader capability with an enforced method policy and a reader-only
surface. An immediate application pattern can use Fetch.restrict with exactly
GET, HEAD, OPTIONS and PROPFIND. Do not broaden Fetch.read_only globally.
Authorize future reports by the operations and resources they actually access;
a REPORT method name alone is not a complete resource policy.

### R3 — high server prerequisite: conditional writes cannot currently run

[Conditional.reject_conditional_write](lib/conditional.ml) and
[Backend.run_core](lib/backend.ml) reject non-GET/HEAD requests carrying
If-Match, If-None-Match or a valid If-Unmodified-Since before dispatch.
The probe got 412 without invoking its conditional PUT handler.

This is an intentional safeguard for generic handlers. A DAV server needs an
explicit transaction-aware dispatch contract that takes responsibility for
checking conditions against pre-state before committing a mutation. Removing
the generic safeguard, or checking validators after the handler, is unsafe.
The DAV mount must also authenticate before disclosing resource conditions;
the current global rejection occurs before route authentication.

### R4 — high server prerequisite: there is no streaming request-body contract

[proffer.httpz](httpz/proffer_httpz.ml) bounds the request head and body
together to 32,767 bytes. It buffers/dechunks the body before dispatch.
The client can stream uploads, but that does not make Proffer able to receive
ordinary file-sized uploads.

Add a scoped, bounded input stream and an admission phase before body intake.
Keep the current bounded string interface for ordinary handlers. Specify
Expect: 100-continue, early denial, draining/closing, cancellation, temporary
file ownership and atomic publication before implementing PUT.

### R5 — medium before serving DAV: telemetry retains lock-bearing headers

[Proffer's event redaction](httpz/proffer_httpz.ml) removes Authorization,
Proxy-Authorization and Cookie values, but not If or Lock-Token.
An application serving custom DAV routes and recording request_headers would
record those values. The Fetch DAV client already marks both sensitive.

Redact them case-insensitively in the backend's event copy, including repeated
fields. Keep real fields available to handlers. Use allowlisted DAV events,
without XML bodies, credentials, native filenames or tokens in exceptions.
This is not evidence of an existing automatically exposed DAV endpoint.

### R6 — medium: the three copies have already diverged

Before this documentation change, check_sync.py checked 38 shared files and
found ten differences in stock OCaml; the OxCaml copy matched OxMono.
Stock commit 38af268 adds property helpers, REPORT, sync-collection, extended
MKCOL, MKCALENDAR, PUT response ETags, bounded GET and principal/home discovery.
PUT changes its return type from int to a record.

Review and reconcile that committed extension before adding APIs independently
in OxMono. Do not overwrite it with the older tree. Apply the existing
[synchronization procedure](../httpz/dav/SYNC.md), including API consumers and
both compiler variants. These monorepo review documents sit outside the shared
DAV trees; plan their standalone documentation location when porting.

### R7 — medium, stock extension only: discovery hides errors and misresolves redirects

In stock [context_path](../../../ocaml-httpz/fetch/dav/fetch_dav.ml),
the wildcard branch returns the root for 401, 403 and 5xx, and also for a redirect
without Location. Callers receive an apparent discovery result after a failed
exchange. Its relative Location is resolved against the DAV root, rather than
the well-known request URL. For example, Location: service/ from
/.well-known/carddav should resolve under /.well-known/, not under /.

Specify explicit absent-service fallback, preserve authentication/server
failures, require one valid Location for a redirect, and resolve it against the
response URL before checking authority. Discovery is a separate bootstrap grant;
requiring an origin-wide operational writer merely to visit a well-known path
is too broad. RFC 6764 explicitly allows authentication at that path.
See [service discovery](https://www.rfc-editor.org/rfc/rfc6764.html#section-5)
and [reference resolution](https://www.rfc-editor.org/rfc/rfc3986.html#section-5.2).

The extension's response_for also falls back to the first resource when the
requested resource is absent. Require an unambiguous matching response and
retain failed/missing/duplicate property distinctions in discovery helpers.
Returned external hrefs are data; no later request may acquire authority from
them. These observations are from source inspection, not a new stock test run.

## Missing features and acceptance criteria

“Absent here” describes the reviewed monorepo revision. It does not mean that
the feature is absent in the stock extension.

| Area | State | Required next evidence |
| --- | --- | --- |
| DAV routes and filesystem service | Absent in all reviewed integrations | Explicit opt-in mount, authenticated reader first, method/exposure tests |
| Eio export boundary | No DAV filesystem adapter | open_subtree, opaque reader/writer interfaces, confinement and race tests |
| Server protocol direction | Client encoders/decoders only | Typed request decoders, If parser/evaluator and bounded response writers |
| Server properties and locks | Absent | Durable property transactions; principal-bound leases; restart policy |
| Conditional, atomic uploads | Client conditions exist; Proffer rejects conditional writes | Admission and transaction contract, interrupted-upload and crash tests |
| Large listings | Bounded materialization, no streaming resource fold | Explicit incomplete-result semantics, cancellation and peak-memory tests |
| Friendly stat/list API | Raw PROPFIND here; stock has property readers | Preserve partial failures and distinguish collection from child entries |
| Managed client leases | Explicit lock/refresh/unlock only | Switch-scoped helper, deterministic expiry/renewal, bounded release failures |
| Client authentication variants | Scoped Basic/Bearer/custom credentials; no dedicated Digest/Negotiate DAV workflow | Add challenge handling only with a concrete interoperability requirement |
| Sync/REPORT/extended collection creation | Absent here; stock implementation exists | Review and port; independent sync-capable fixture and checkpoint tests |
| CalDAV/CardDAV applications | Some stock building blocks, no full application claim | Typed domain reports, data formats, discovery policy and interoperability |
| Prefer, quota, SEARCH, ACL editing, DeltaV, bindings | No dedicated complete APIs here | Add only with a consumer and separate authority/conformance tests |
| Resume/ranges | Client can use Fetch headers/206; no Proffer DAV range service | Bounded single-range server support or explicit full-response fallback |
| Alternative client transports | DAV fixture selects fetch-httpz | DAV wire tests through fetch-curl; macOS backend on its supported platform |
| Independent client/server conformance | Apache fixture and recorded live observations | New server tested by independent client; second disposable implementation |
| Live deviations | Recorded ignored PUT preconditions and two other deviations | Keep fixture regressions; investigate separately without weakening defaults |
| Continuous cross-repository parity | Read-only local script, currently reports drift | Automated checks and explicit revision records after ports |

## Review evidence and implementation order

An ephemeral OCaml probe under /tmp was compiled with switch 5.2.0+ox against
the local library artifacts. It checked COPY/MOVE destination composition,
Fetch.read_only, ordinary Proffer dispatch and conditional PUT. For an ordinary
GET site, PROPFIND, PUT, COPY and LOCK each returned 405 without a DAV header.
This verifies dispatch behavior, not wire parsing, filesystem isolation or
complete default-exposure coverage.

Source inspection covered package dependencies, dispatch/authentication,
request buffering, telemetry, DAV client policy and the vendored Eio Path/Fs
interfaces. No Docker or live-account test was run for this review. The
ephemeral probe has not been installed as a repository regression test.

Proposed order:

1. Reconcile the stock extension and fix R1/R2/R7 with focused regressions.
2. Agree the server capability and explicit mounting contracts below, including
   authentication before body intake and generic conditional rejection; add
   tests that prove disabled and read-only operation.
3. Build bounded server codecs and the confined Eio reader.
4. Add authenticated read-only serving and independent wire tests.
5. Add admission, transaction handling and persistent writer state before
   enabling any writes; fix R3/R4/R5 as prerequisites.
6. Add locks, recursive operations and extensions only after their security
   and crash-recovery criteria pass.

The detailed method policy, example programs, resource budgets and release
gates are specified in [DAV_SERVER_SPEC.md](DAV_SERVER_SPEC.md).
