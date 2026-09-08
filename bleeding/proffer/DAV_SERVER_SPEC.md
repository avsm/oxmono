# Proffer DAV server specification

Status: implementation underway, 2026-09-08. The shipped API and its tested
scope are documented in [proffer.dav](dav/README.md). This document remains the
broader design target. Sketches below are not exact signatures.

The client facade has been removed as requested. `Fetch_dav` is the client,
`Proffer_dav` is the server, and `Proffer_dav_eio` supplies storage. The first
implementation includes read-only exports and a private managed writer with
an atomic manifest, rather than in-place writes to arbitrary directories.
Explicit admission, connection provenance, bounded streaming, conditions,
properties, leases and restart validation are implemented. The README's
remaining-work list takes precedence over the planned delivery milestones.

## 1. Goals, scope and trust model

The security boundary is an export: a stable identity, a URL mount, a confined
store and its access policy. A request must never increase its authority by
choosing a path, Host field, Destination, property value, lock token or report.
No generic DAV operation opens an outbound connection.

Required invariants:

1. Installation, linking and ordinary Proffer startup expose no DAV resource.
2. Every resource operation is bounded by the export capability and the
   authenticated principal's grant. Either can further restrict access.
3. A read-only export has no reachable store mutation operations. Policy
   denial happens before calling the store.
4. Authentication and authorization precede resource-dependent errors.
5. URI parsing, routing, authorization, locking and storage use one validated
   resource identity; no layer decodes a path for a second time.
6. Source and destination are authorized independently and belong to the same
   export in the first implementation.
7. Preconditions are evaluated against pre-state under the same serialization
   boundary as publication. Failed conditions do not modify public state.
8. Every input, traversal, queued request, temporary object and lease is bounded.
9. Cancellation and restart never publish partial uploads or discard live
   locking authority silently.
10. Logs contain the operational facts needed for diagnosis without credentials,
    lock tokens, file contents or native filesystem paths.

Treat unauthenticated clients, authenticated clients, filenames, XML, HTTP
fields and uploaded bytes as hostile. A credential may authorize a principal
without granting access to every export. Existing links and mounts in an
exported directory also require an explicit storage policy.

The application assembling capabilities, the Eio provider and the OS kernel
are trusted. Eio capability passing is not a sandbox for arbitrary OCaml code
that calls Unix or C directly. Run under a dedicated OS account with only the
required directories accessible; use OS isolation for untrusted extensions.

Initially support one process owning each writable store. Local writers that
bypass the server, another server process and administrator filesystem changes
are outside its transaction/locking guarantee. Refuse a second cooperating
writer using an exclusive store lease. Read-only snapshots are recommended
when publishing a tree changed by other processes. A live externally changed
tree cannot promise snapshot enumeration or stable validators without an
additional backend contract.

## 2. Package boundaries and explicit activation

Keep httpz.dav as pure protocol code and fetch.dav as the outbound client.
The current implementation replaces the former proffer.dav client facade.
Do not preserve an alias whose name suggests server authority. Superseded plan:
its meaning into a server module.

Proposed new opam package: **proffer-dav**.

| Library/module | Responsibility | Direct architectural dependencies |
| --- | --- | --- |
| httpz.dav / Httpz_dav | Add typed server request decoders and response encoders | httpz.uri and private XML codec |
| proffer-dav / Proffer_dav_server | Mounting, authentication, permissions, method semantics, store contract | proffer, httpz.dav; no Fetch or Eio requirement |
| proffer-dav.eio / Proffer_dav_eio | Confined storage, scoped I/O, clocks, transactions and persistence | proffer-dav, Eio and narrowly chosen persistence/hash dependencies |
| Example executable | Acquire capabilities, credentials and listener, choose exports | Explicitly selected libraries and proffer-httpz |

Resolve exact names during implementation, preserving the dependency direction.
A separate package keeps the server optional at installation as well as linking.
The existing Proffer package's Fetch dependency is a separate cleanup decision;
it is not evidence that DAV server routes are active.

Do not add a global enable_dav boolean to the HTTP parser or listener.
Construct an export and register its complete endpoint explicitly:

~~~ocaml
(* Proposed interface outline; not executable with the current libraries. *)
type endpoint
type export
type security

val read_only : reader -> export
val read_write : writer -> writes:write_policy -> export
val endpoint :
  at:string list -> origin:origin -> security:security ->
  limits:limits -> export -> endpoint
val mount : endpoint -> app_site -> (app_site, mount_error) result
~~~

There is deliberately no default directory or default security argument.
Constructing a reader does not open a listener. A directory configuration or
environment variable alone never enables the endpoint.

Reserve the whole mount subtree. Reject overlapping exports and ambiguous
overlap with ordinary routes; a preceding catch-all must not bypass DAV
authentication. Mounting must preserve security and apply it to final decoded
paths. The current Site.mount rejects wrapped subsites; the new integration
must respect that behavior and return a fully protected final site. Do not
ask every caller to remember a fragile mount-then-wrap ordering.

Outside an export, ordinary routing and headers remain unchanged. No global
DAV, Allow or well-known registration is added. Unknown extension methods
must not fall through to an application writer. At a DAV path, run the
security gate even when the requested method is unsupported.

## 3. Eio capabilities and storage interface

### 3.1 Acquiring an export

Trusted startup code opens a directory with Eio.Path.open_subtree and keeps
its switch alive for the server's lifetime. Appending a pathname to env#fs
does not attenuate that filesystem capability. Eio.Path.native/native_exn
must not be used to feed untrusted paths to string-based filesystem APIs.
These distinctions are explicit in the [vendored Path interface](../../vendor/eio/lib_eio/path.mli)
and [Eio's Path documentation](https://ocaml-multicore.github.io/eio/eio/Eio/Path/index.html).

The Eio adapter's constructor should itself call open_subtree on its supplied
path, producing a fresh confined root even if the caller accidentally passes
a composite path over a broad directory capability. After construction it
retains only that root. This confines access to the selected tree; it cannot
judge whether selecting that tree was the operator's intention.

Do not pass env#fs, env#cwd, env#net, a credential file path or the whole Eio
environment to DAV handlers. Network authority belongs to the listener.
The application passes handlers only their prepared export state. Authentication
can be an injected verifier capability; any network-backed verifier must have
its own narrowly scoped client, deadline and concurrency budget.

Current Eio directory types contain both read and write operations. A
polymorphic variant annotation on Eio.Path.t does **not** make a directory
read-only. The server needs an opaque adapter that exposes fewer operations.
The directory handle must never be recoverable from the reader interface.
OS read-only mounts or filesystem permissions can reinforce this boundary.

### 3.2 Store capabilities

Use one small store contract with an optional, separate writer extension.
Avoid building a generic virtual filesystem framework.

| Capability | Operations visible to the DAV layer |
| --- | --- |
| reader | Bounded metadata lookup, bounded child enumeration, scoped immutable read view, readable property lookup |
| writer | Reader plus private upload staging, guarded publication, collection creation, guarded namespace changes, atomic property update |
| lock manager | Principal-bound lease acquisition/check/refresh/release; requires an explicit writer export |
| administrator | Local startup/recovery/revocation only; never constructible from a DAV request |

Only the writer implementation can mint writer capabilities. Attenuating a
writer to a reader must return an interface with no way to recover the writer.
Grant values carry export identity; paths and leases from another export are
rejected even when their relative strings happen to match.

Public resource names are opaque validated relative-segment values, not Eio
paths or host filenames. A scoped read callback receives metadata and a
read-only flow for the same opened object. Streaming must not re-open a name
after producing its ETag. Resources cannot escape their callback or switch.

For OxCaml, portable route descriptions access domain-bound Eio state through
the handler environment; do not mark handles portable to make examples compile.
The stock implementation keeps the same ownership and authorization behavior.

### 3.3 Filesystem policy

The default adapter serves regular files and collections only. It never
executes files, follows a DAV URL to a network target, or opens devices,
FIFOs or sockets as file content.

Default to rejecting symlinks, including intermediate components, and reject
mount traversal and multiply linked regular files in the hardened adapter.
These checks must be tied to descriptor-relative operations, not a separate
lstat followed by a pathname open. Eio's current subtree confinement is a
strong base but does not itself provide all those stricter policies.

On Linux, investigate an Eio/provider extension for openat2 with beneath,
no-symlinks and no-cross-device resolution, plus safe creation and parent
resolution. The [Linux interface](https://man7.org/linux/man-pages/man2/openat2.2.html)
distinguishes those flags: beneath alone does not enforce the other two.
Unsupported protection must produce a clear construction error, never a
silent weaker fallback. Require separate proof and race tests before claiming
the same policy on other Eio backends.

Check the opened object's kind and link count, and reject unsafe objects.
For creation and replacement, resolve and retain the parent descriptor and
validate the final component using the same policy. An administrator changing
mounts or hard links concurrently is outside the stated trust boundary; a
startup scan alone is not a security boundary.

A deliberately weaker confined-symlink mode, if added later, needs an explicit
constructor and documented alias/locking semantics. Do not ship it initially.
Never expose a symlink creation method through generic properties.

Keep credential files, journals, temporary uploads, locks and private metadata
outside the exported namespace. Use separate confined capabilities supplied
at startup. Private staging must share the required filesystem for atomic
publication; reject a configuration that would require a copy fallback.
Dotfiles in an exported tree are ordinary resources unless a documented policy
excludes them. Do not imply that a directory containing secrets is safe merely
because common secret names are hidden.

## 4. Authentication, grants and read-only behavior

There are two explicit security constructors:

- authenticated: a mandatory verifier returns a stable principal identity,
  then an authorization function returns a grant for this export;
- public_read_only: explicit anonymous publication, usable only with a reader.

There is no anonymous writer constructor. Missing credentials fail closed.
Constructing an endpoint with public_read_only security and a writer export
is an error, even if the current caller promises to issue only GET requests.
Verifier exceptions and timeouts deny access; they do not trigger a public
fallback. Never log the supplied credential. Use maintained authentication
primitives and constant-time secret verification where applicable.

The existing Site.with_auth boolean check is useful for a uniform gate but
does not identify principals or authorize separate operations. The DAV gate
must produce an authenticated context once and reuse it consistently for the
request, including destination and lease checks.

Initial grants cover a whole export and select read-only or read-write.
For separate users or authority boundaries, provision disjoint exports.
Do not implement per-path ACLs initially: filtered enumeration, recursive
operations, ancestor locks and namespace changes would all need additional
rules. Any later ACL feature must extend the store's transaction contract.

Effective permission is the intersection of export mode, authenticated grant,
enabled operation set and store capability. Authentication never upgrades a
reader. A valid lock token never upgrades a principal's grant.

| Operation | Read-only export | Enabled writable export | Authority checked |
| --- | --- | --- | --- |
| OPTIONS | Yes after gate | Yes after gate | Export visibility |
| GET / HEAD | Files; bounded collection behavior | Same | Read target |
| PROPFIND | Depth 0/1 and allowlisted properties | Same | Read target and enumerated entries |
| PUT | Deny before store call | Explicit file write | Replace file or bind new child |
| MKCOL | Deny | Explicit collection creation | Bind child in parent |
| DELETE | Deny | Explicit namespace mutation | Remove target and parent binding |
| COPY | Deny, even if source is readable | Explicit copy | Read source; create/replace destination and binding |
| MOVE | Deny | Explicit move | Remove source binding; create/replace destination binding |
| PROPPATCH | Deny | Explicit property write | Update target's permitted dead properties |
| LOCK / refresh / UNLOCK | Deny; locking changes state | Separate lock feature | Write grant, lease scope and owner |
| REPORT, SEARCH, ACL, POST, MKCALENDAR and other extensions | Disabled | Disabled until individually implemented | Explicit future operation contract |

For authenticated requests, use 403 when an implemented operation is denied
by the grant; use 405 with a correct Allow when this export does not implement
it. Anonymous requests receive the authentication challenge first. Do not
consult target existence, locks or validators to choose an unauthenticated
response. Syntactically invalid HTTP can still be rejected before routing.

Read-only means no DAV-induced change to contents, directory entries, dead
properties or lock state, including temporary upload creation. Reads may still
update OS access times or write a separately granted audit sink; stronger
physical immutability requires an OS read-only/noatime snapshot.

Default to an explicit HTTPS origin. The example executable must refuse
password authentication over a public plaintext listener. Permit local HTTP
testing only through an explicit loopback/test configuration. TLS termination
behind a proxy requires a separately configured trusted peer boundary; never
trust arbitrary Forwarded, X-Forwarded-* or identity headers.

Browser access is not an initial feature. Do not install cookie authentication,
CORS or method-override handling implicitly. An eventual cookie-authenticated
adapter needs CSRF protection for all mutations and an explicit origin policy.
Serve untrusted uploads on a dedicated DAV origin with nosniff and conservative
content disposition, to avoid granting uploaded HTML an application origin.

## 5. Resource identity, mounting and secondary targets

Parse and validate the request head and authority first. Compare Host against
the configured external origin; an origin is not inferred from user-controlled
forwarding fields. Reject ambiguity in authority and framing fields.

Use the same identity pipeline for request targets, Destination and tagged If
resources:

1. Parse URI structure with httpz.uri; validate allowed reference form.
2. Require the configured origin for absolute references. Reject userinfo,
   fragments and query strings in filesystem resource targets.
3. Split the encoded path at literal slashes, then decode each segment once.
4. Reject malformed escapes, decoded slash/backslash, NUL/control bytes, dot
   and dot-dot segments; bound segment count and encoded/decoded lengths.
5. Match the decoded mount prefix and obtain a relative Resource_path.
6. Resolve the opaque path only through this export's store.

Proffer currently collapses empty path segments in routing. The DAV mount must
apply the same gate to such aliases; the initial filesystem policy then rejects
interior repeated slashes rather than creating another spelling for a lock key.
Trailing slash represents a collection spelling, not a different lock identity.
The export root is a valid resource with zero relative segments, unlike the
current Static.confine helper's None for an empty capture.

Do not double-decode: a filename literally containing %2e is distinct from
an encoded dot. Do not Unicode-normalize or case-fold names independently of
the store. Initially require a case-sensitive, byte-preserving filename backend
and valid UTF-8 names; other filesystems need an explicit alias identity policy
before supporting locks or conditional writes. Invalid stored names must
produce an explicit listing failure rather than a false complete listing.

Return percent-encoded absolute-path hrefs consistently, with collection
slashes and without native filenames. Use the configured external mount for
Location values. Never redirect a mutation to “fix” its spelling after taking
an action; reject a noncanonical collection target with a documented error.

For COPY/MOVE, parse exactly one Destination and reject a target outside the
same export before any store effect. No DNS lookup or Fetch request is needed.
Validate source/destination ancestry: reject self-copy, self-move and recursive
operations that place a collection into its own descendant. Protect the export
root from deletion, renaming or replacement.

Tagged If resources name conditions, not new authority. Validate their form
and require they name the request's affected resources or relevant ancestor
collections within this export. Evaluate them locally. A property containing
an external href is inert XML; serializers and property readers must not fetch
its contents or treat it as a host path.

## 6. Proffer admission and request-body changes

The server needs two explicit additions to Proffer, driven by regression tests:

- endpoint admission after routing/authentication but before accepting a body;
- an endpoint contract for precondition-aware mutations.

The protected mount and early security gate are prerequisites even for the
read-only milestone. In particular, a conditional PUT to a protected reader
must not be intercepted by today's global 412 before authentication. Large
streaming bodies and guarded writer commits can follow the read-only delivery;
the reader's bounded XML input still needs admission before intake.

Retain existing behavior for ordinary handlers. Do not create a global switch
that disables conditional-write protection. DAV registration declares a handler
that owns conditional evaluation; its store commit operation requires evaluated
guards. The dispatch code must prevent the generic response-stage evaluator
from converting an already committed mutation into a late 412.

Proposed request lifecycle:

~~~text
bounded HTTP head
  -> validated origin and protected mount
  -> authentication and operation grant
  -> target/destination validation and header conditions
  -> quota/concurrency reservation
  -> body admission (send 100 Continue only here)
  -> bounded XML parse OR streamed private upload
  -> recheck grant/conditions/locks under store serialization
  -> commit or discard
  -> bounded response and sanitized event
~~~

Admission can reject obviously false conditions early, but those checks never
replace evaluation immediately before commit. Authorization changes during
an upload must be rechecked at commit. Do not hold a namespace transaction
while waiting for a slow client to upload.

Add a request-body source owned by the request's switch and consumed at most
once. The handler may retain neither it nor a local request value. Existing
Req.body remains available only through bounded materialization; it must not
silently collect a multi-gigabyte upload.

Known Content-Length is checked before admission. Count bytes for unknown
length/chunked uploads as they arrive. Reject excess bytes or malformed/truncated
framing and discard staging. Content-Encoding is rejected initially; enabling
decompression later requires separate compressed and expanded byte limits.

If denying before consuming a body, close the connection or use a strictly
bounded drain policy; never parse its bytes as a pipelined next request.
Backpressure and idle/total deadlines apply to both input and output.
Expect: 100-continue tests must establish that failed authentication does not
cause a client to send its upload first.

## 7. Preconditions, publication and persistence

### 7.1 Serialization contract

Start with one mutation coordinator per export. This is simpler to audit than
fine-grained locks and can be optimized after correctness is established.
Namespace and property checks plus commit run under it; uploads and expensive
preparation happen outside it. Reading and listing use stable views or bounded
read transactions so they cannot observe metadata half committed.

The writer accepts a structured operation with target identities, expected
validators, overwrite policy and evaluated DAV conditions. It revalidates
these against the current state while holding the coordinator. The raw
filesystem writer is private to this implementation.

Follow HTTP condition ordering, including strong If-Match comparison and
If-None-Match: * for creation. A failed conditional mutation returns 412 with
no public mutation. Follow [HTTP conditional semantics](https://www.rfc-editor.org/rfc/rfc9110.html#section-13.2);
the current unconditional pre-dispatch 412 is a prerequisite to replace,
not the eventual DAV implementation.

Parse/evaluate the DAV If expression separately from HTTP validators.
Bound tokens, tagged resources, lists and terms. A submitted token must
satisfy both lock coverage and principal ownership; negation and alternative
condition lists must not bypass required token submission.

Strong ETags identify stored representation bytes. A timestamp/length pair
is not a strong validator. Use a collision-resistant content digest or a
persistent generation
whose contract changes on every representation change, with no reuse after
restart. Do not claim strong validators for externally mutable files without
stable snapshots or equivalent validation.

### 7.2 Upload transactions

Stage uploads using unpredictable names, exclusive creation and restrictive
permissions in the private staging capability. Never truncate the public
destination while receiving bytes. Staging ownership belongs to this request;
cleanup may delete only objects whose successful creation is recorded.

After complete framing and quota checks, close/flush the staging writer and
prepare metadata. Under serialization, recheck current target, permissions,
ETag/If conditions, locks and overwrite rules. Publish by an atomic operation
that implements the requested collision behavior.

If-None-Match: * and Overwrite: F need atomic no-replace publication; an
existence check followed by an overwriting rename is not sufficient.
Add the necessary descriptor-based primitive to the adapter if Eio's existing
rename does not expose it. Do not simulate it by temporarily removing a target.

For replacement, publish new bytes atomically and preserve or update
properties/validators according to the defined operation. Distinguish
atomic visibility from durable persistence. The production writer must define
its fsync/directory-fsync and journal ordering and acknowledge success only
after its selected durability contract is satisfied.

Once committed, a lost HTTP reply leaves the client uncertain; it does not
authorize rollback of a completed operation. Cancellation before publication
discards staging. Keep any cancellation-shielded commit/rollback section short
and bounded, and preserve the original error if cleanup also fails.

### 7.3 Metadata and crash recovery

Persistent dead properties and public filesystem changes form one logical
transaction. Choose and implement a small journal with recovery, or a backend
that can atomically store content references and metadata. An in-memory map
plus filesystem rename cannot satisfy that contract across restart.

Specify commit points for PUT, MKCOL, DELETE, MOVE, COPY and PROPPATCH,
including quota accounting. Keep transaction identifiers and recovery records
in the separate state capability. Recovery completes or rolls back an
interrupted transaction before the export becomes available. Corrupt or
ambiguous state leaves the writer unavailable; do not reset it to empty.

Startup cleanup identifies owned staging entries from journal records and
validates each identity. Never recursively delete an arbitrary directory
because its name resembles a temporary prefix. Recovery and garbage collection
have independent bounds and cancellation policy.

First production write delivery is gated on a chosen persistence design and
fault injection at each publication/journal boundary. This is a substantive
missing component, not an implementation detail to leave until after PUT works.

## 8. DAV operations and protocol completeness

Add server-facing functions to httpz.dav with explicit directions, such as
decode_propfind, decode_proppatch, decode_lockinfo, decode_if and
encode_multistatus. Keep the existing client API stable. Reuse the corrected
private XML codec and namespace-preservation model rather than another parser.
The generic encode_xml operation alone is not a validated DAV response builder.

The first server parser accepts the existing supported XML encodings and
media types, with bounded input, node/attribute counts and depth. DTDs and
external entities stay disabled. No XML callback receives filesystem or
network authority. Apply output limits as well as parser limits.

Protocol anchors: [RFC 4918](https://www.rfc-editor.org/rfc/rfc4918.html)
sections 8–10, 13–18 define DAV request/response semantics and compliance.
In particular, authorization errors precede resource-dependent errors;
PROPPATCH is ordered and atomic; multistatus preserves individual outcomes;
and supported depth values are method-specific. The implementation needs a
requirement-by-requirement checklist before advertising a compliance class.

### 8.1 Read-only delivery

Implement OPTIONS, file GET/HEAD and PROPFIND with explicit-property, propname
and allprop forms. Advertise only implemented behavior at the selected export.
Do not infer a DAV compliance class merely because PROPFIND works. A read-only
resource can deny writes by policy, but the server still needs its protocol
and error behavior audited before advertising DAV: 1 or DAV: 1, 3.

PROPFIND supports depth 0 and 1. Disable infinite traversal initially.
Treat missing Depth according to DAV semantics and reject unsupported infinite
traversal with the appropriate finite-depth error; do not silently reinterpret
it as depth zero. Bound both requested properties and enumerated children.

Readable live properties initially include resourcetype, getcontentlength,
getcontenttype, getlastmodified, getetag and displayname where meaningful.
Do not invent creation times or advertise write/lock/report properties that
the backend cannot supply. Unknown or unavailable properties retain their
property-level failure status rather than being returned as successfully empty.
Only expose configured property namespaces; filesystem ownership, native paths
and private transaction metadata are never implicit DAV properties.

Read an ordinary file from one stable opened view with matching length and
ETag. Collections use PROPFIND for membership; a GET collection representation
is a deliberately defined bounded representation or a documented rejection,
not an automatic HTML directory browser.

Authenticated file responses use private cache policy by default; DAV metadata
and error responses use no-store. Never key shared Proffer caches solely by URL
when responses depend on credentials or export state. Do not execute uploaded
content or run it through a template interpreter.

Single-range downloads are useful but may follow the first read-only milestone.
Until then, document that Range is ignored and a complete 200 representation
is returned where HTTP permits; never emit a fabricated 206 or advertise
Accept-Ranges: bytes. If implemented, test suffix/open-ended ranges, If-Range,
unsatisfiable ranges and exact lengths. Multiple ranges remain deferred.

Large directory responses must fail explicitly when the configured complete
listing limit is reached. The simplest initial design materializes a bounded,
complete result before sending its head; never end a truncated 207 with a
successful closing element and imply that omitted children do not exist.
A later streaming listing needs an explicit partial-failure contract.

### 8.2 Writer delivery

Enable methods only as their complete semantics and persistence tests pass.

| Method | Required behavior before enablement |
| --- | --- |
| PUT | Atomic conditional publication, exact received representation, bounded staging, committed ETag, safe cancellation |
| MKCOL | Parent must exist, collision must fail, resource type is a collection; initial bodyless form, explicit rejection of unsupported extended bodies |
| PROPPATCH | Validate the entire request, preserve update order, atomically update permitted dead properties, report protected-property and dependent failures |
| DELETE | Protect export root; check relevant parent/descendant locks and report recursive failures accurately |
| COPY | Read source properties/content, enforce destination policy, preserve appropriate dead properties, support required collection depth behavior before advertising full support |
| MOVE | Check both namespaces, change resource identity consistently, maintain property state and lock semantics without transient public loss |

Allprop is not a promise to compute every expensive live property. Dead
property values retain namespace context and mixed content. Quotas apply to
their encoded size and aggregate stored size. Protected live properties cannot
be changed through PROPPATCH or smuggled through unknown namespaces.

The client's safe Overwrite: F default must not be copied into server parsing:
an omitted Overwrite header has the protocol's default behavior. An export
may require explicit replacement permission, but must reject a denied request
honestly rather than silently reinterpret it. Likewise, a server must not
copy client defaults for method depth.

Recursive operations have a bounded preflight over the affected tree,
followed by serialized validation. The initial whole-export authorization
model avoids hidden per-user subtrees. Before any change, reject a source
containing a denied special file, unsupported link, forbidden destination or
an operation exceeding traversal limits.

Do not promise that every recursive DAV operation is one atomic filesystem
rename. Preserve and report partial failures where the method permits them.
When an operation requires atomic behavior, an unsupported filesystem/backend
returns an explicit error rather than emulating it with a destructive sequence.
Cross-export and cross-filesystem moves/copies remain disabled initially.

### 8.3 Lock delivery

Locks are an explicit writer feature, disabled for readers. Begin with
exclusive write locks; defer shared locks. Supporting locks requires the
complete lifetime: acquisition, discovery, refresh, submission, expiry,
unlock and restart handling, including collection depth and unmapped URLs.
Do not advertise class 2 during a partial implementation.

Keep the authenticated principal, export identity, canonical lock root, depth,
token, creation/expiry metadata and scope in the lock store. Mint opaque tokens
with at least 128 bits of cryptographic randomness from an explicitly supplied
random capability. Client-supplied owner XML is bounded descriptive data, not
authentication evidence.

The lock creator and the current write grant are checked in addition to a
matching token. An administrator's unlock ability is a distinct local
capability; possession of a token does not create it. This follows DAV's
[lock ownership model](https://www.rfc-editor.org/rfc/rfc4918.html#section-6.4).

Use an injected monotonic clock for active leases and cap granted lifetimes.
Return the actual timeout and make refresh observe the same ownership checks.
Failed refresh leaves the existing expiry unchanged. Check ancestor and
destination locks under the mutation coordinator, including creation in a
locked collection and replacement at a locked destination.

Namespace operations must use explicit rules for removing, retaining or
inheriting locks; never simply move a path-keyed map entry with a file.
Do not emit lock tokens to telemetry or identify the authorized principal from
the XML owner field.

Persist granted leases before acknowledging them. On restart, restore their
coverage before accepting mutations. A monotonic timestamp cannot simply be
reused across boots: persist enough information for a documented conservative
recovery policy. If time/state is uncertain, deny affected writes pending
recovery; do not silently declare every lock expired. Test clock rollback,
corrupt records and restart during renewal.

Shared locks, remote administrative unlock and multi-process lock coordination
are deferred. A nonpersistent lock manager may exist for disposable fixtures
only, with a constructor and label that cannot be confused with production.

## 9. Bounds, overload and information disclosure

The following are proposed initial defaults, subject to measurement. They are
local admission policies, not claims about DAV protocol maxima. Export
constructors validate every number and reject inconsistent budgets.

| Budget | Proposed initial policy |
| --- | --- |
| Request head | Existing 16 KiB maximum; never increased by enabling DAV |
| Path | At most 4 KiB encoded, 64 segments, 255 decoded bytes per segment; also obey backend limits |
| DAV XML | 1 MiB input, depth 32, 20,000 nodes plus attributes |
| Conditions | 16 KiB within head limit; 64 tagged resources, 128 lists, 512 total terms |
| Property request/update | 128 names/instructions; 64 KiB per dead-property value; 1 MiB stored properties per resource |
| Complete PROPFIND result | 10,000 resources and 8 MiB serialized output, whichever is reached first |
| Traversal | Depth 0/1 for PROPFIND; recursive writes require explicit bounded budget |
| Uploads | Read-only: no staging; writer requires explicit max file size and total staging/storage quotas |
| Running work | At most 16 admitted DAV operations per export and 4 per principal; at most 2 uploads per principal |
| Waiting work | At most 32 queued operations per export with a bounded wait; reject overflow |
| Leases | At most 64 per principal and 1,024 per export; 600 seconds default, 3,600 maximum |
| Time | Five seconds for authentication, 15 seconds for metadata, 30 seconds idle transfer timeout; total upload deadline required in writer configuration |

Include queued and in-flight reservations in quota accounting, not just
committed files. Bound quotas across exports sharing a storage volume.
Reserve aggregate memory for XML and materialized responses as well as disk
space: multiplying per-request limits by concurrency must fit the process
budget. Refuse admission before allocating those buffers when it does not.
Use overflow-checked arithmetic for lengths, counters and deadlines.
Constrain unauthenticated work with connection and peer budgets before a
principal is known; do not trust a forwarded client address by default.

Return appropriate HTTP errors for too-large, unsupported and overloaded
requests, with bounded retry information where useful. A server-enforced
traversal limit must not look like a complete successful response.
Resource access and cleanup loops yield/cancel within bounded intervals.

Use an allowlisted DAV event: request ID, principal audit identifier, export
ID, operation, sanitized relative resource, outcome, byte count and duration.
The default log policy may omit names where filenames are sensitive.
Escape control characters in all client-derived display text.

Redact Authorization, Proxy-Authorization, Cookie, If and Lock-Token before
calling Proffer's general telemetry hook, case-insensitively and across
duplicates. Do not log arbitrary headers or request targets containing queries.
Do not log response lockdiscovery XML or bounded error bodies just because
they are bounded. Sanitize filesystem exceptions before handing them to
Proffer's default on_error printer.

Authentication/authorization errors precede existence, validator and lock
details. Once authorized, expose stable protocol errors rather than host
errno text, native paths, SQL messages or stack traces. A configured audit sink
has bounded backpressure; a failing sink cannot acquire filesystem authority
or crash unrelated exports.

## 10. Planned example programs

Each example should be a complete executable with Dune dependencies and an
independent test once its required APIs exist. The following snippets describe
the desired composition and are **pseudocode**, not current API examples.
They omit routine TLS/verifier construction but specify where those
capabilities originate.

### A. Disabled by default

Extend an ordinary Proffer example with optional DAV configuration. Without
an explicit enable flag, it constructs no export, opens no DAV directory,
creates no state directory and registers no DAV subtree. Linking proffer-dav
and setting DAV_ROOT alone must leave this behavior unchanged.

When enabled, require an explicit path, URL mount, external origin and verifier;
default that explicit export to read-only. A separate write flag also requires
state/staging directories, quotas and successful writer recovery. Invalid
configuration fails startup, with no partially mounted endpoint.

### B. Authenticated read-only directory

~~~ocaml
(* Proposed composition. D and FS refer to proposed modules. *)
Eio_main.run @@ fun env ->
Eio.Switch.run @@ fun sw ->
let reader =
  FS.reader ~sw ~policy:FS.Policy.hardened
    Eio.Path.(env#fs / configured_directory)
in
let export = D.read_only reader in
let security =
  D.authenticated ~verify:prepared_verifier ~grant:(fun principal ->
    if may_read principal then Some D.Grant.read_only else None)
in
let dav =
  D.endpoint ~at:["dav"] ~origin:configured_https_origin
    ~security ~limits:D.Limits.default export
in
let site = D.mount dav ordinary_site |> or_startup_error in
(* The listener gets network/clocks; handlers get only prepared export state. *)
run_prepared_site ~sw ~listener_env:env ~tls:prepared_tls site
~~~

FS.reader narrows the supplied path using open_subtree internally. The
prepared-site runner is an integration sketch for storing the handler
environment; it must not pass the listener environment to handlers.
The verifier is prepared by trusted startup code, with no password in an
argument vector or log.

Demonstrate OPTIONS, depth-one listing and streamed download using Fetch_dav.
Then try PUT, DELETE, COPY, MOVE, PROPPATCH and LOCK and prove no store mutation
or staging occurs. Include an unauthenticated listing and an outside-root link.

For a publish-only directory, a separate example uses
D.public_read_only explicitly and requires no credential handling. It should
explain that everyone who reaches the endpoint can read the selected export.

### C. Explicit writable scratch export

Build on B with separately confined data, state and staging directories.
Require positive file/staging/storage quotas and a total transfer deadline.
Use only a new disposable directory created by the example harness:

~~~ocaml
(* Proposed API; requires completed transaction/recovery implementation. *)
let writer =
  FS.writer ~sw ~data ~state ~staging ~quota ~clock ~random ()
in
let export =
  D.read_write writer ~writes:D.Writes.files_and_properties
in
(* Enable locks separately after acquiring a durable lock manager. *)
~~~

Demonstrate create-only PUT, stale ETag rejection, conditional replacement,
no-overwrite COPY, MOVE and cleanup. A second phase enables locks and shows
that another authenticated user cannot mutate or refresh a lease by copying
its token. Run interruption and restart cases before presenting success.

The example must not derive a writable export by setting a boolean on the
reader from B. It obtains and grants a different capability.

### D. Two isolated users

Prepare two disjoint exports with distinct roots, export IDs and grants.
Show that the users can read their own trees, cannot discover each other's
resources, and cannot COPY/MOVE across mounts even on the same origin.
Use an independent client and verify both trees after each rejected request.
A later multi-tenant resolver must preserve the same model; URL usernames
must not be converted into directory paths using an ambient filesystem.

### E. Read-only remote-to-local copy

Keep this as an outbound client example, separate from DAV serving. Give
the operation a DAV reader capability for one remote collection and a writer
for a private local destination. Do not give it the raw network capability.
Until the new reader exists, a supplied Fetch method allowlist can allow
GET, HEAD, OPTIONS and PROPFIND only. Generic Fetch.read_only blocks PROPFIND.

Check every returned href when making a request, construct local names from
validated segments, and stage downloads before publication. Discovery gets a
separate narrow bootstrap capability; it does not widen the operational client.
No incoming DAV Destination should ever invoke this example as an implicit
proxy or remote-copy service.

## 11. Security tests and acceptance evidence

Security-sensitive behavior must run through the real proffer.httpz backend.
Proffer mocks are useful for method policy and store-call assertions, but they
do not validate framing, streaming, Host checks or filesystem races.

| Test group | Required assertions |
| --- | --- |
| Disabled | Ordinary executable with server library linked and unmounted: no DAV methods/headers, well-known route, directory opens or state creation |
| Read-only | Every mutation and method-override attempt denied; instrumented reader has no writer calls; data, properties and locks unchanged |
| Authentication | Missing/repeated/mixed-case credentials, verifier exceptions, denied/unknown paths, denied methods and failed conditions reveal no resource state |
| Mounts | Prefix lookalikes, repeated slashes, percent-encoded prefix, dot traversal, nested mounts, catch-all shadowing and ambiguous routes |
| URI/path | Encoded separators, double escapes, NUL, malformed UTF-8, long names, trailing slashes, authority/userinfo/query/fragment confusion |
| Secondary resources | Cross-origin/export Destination, repeated fields, descendant cycles, outside-root tagged If and external property hrefs cause zero outbound connections/effects |
| Filesystem | Symlink/parent replacement races, hard links, bind mounts, special files, root rename, denied permissions and unsupported confinement primitives |
| Streaming | Fragmented/chunked bodies, 100-continue, length mismatch, early denial, oversized/slow input, pipelining, disconnects and backpressure |
| Conditions | Absent/current/stale/weak ETags, create races, no-overwrite races, If alternatives/negation/tagged conditions and ancestor locks |
| Isolation | Distinct principals/export IDs, token theft, stale grants, forged owner XML and cross-export lease/path reuse |
| Persistence | Kill/fault at every journal/publication boundary, rollback failure, partial fsync, disk full, stale staging, corrupt state and second writer startup |
| Lock lifecycle | Expiry/renewal with deterministic clocks, restart/clock rollback, unmapped URLs, collection operations and failed refresh |
| XML/properties | DTD/entity refusal, namespace shadowing, mixed content, protected properties, repeated ordered updates and atomic failure |
| Bounds/logs | Quota reservations, concurrency/queue exhaustion, response limits, cancellation latency and complete secret redaction |
| Compatibility | Independent wire client, accurate statuses/Allow/DAV fields, conditional downloads and external mount URL handling |

Use a temporary in-memory store for deterministic transaction/fault tests and
a real confined Eio store for filesystem tests. Eio mocks cannot prove kernel
confinement. A test may skip a platform-specific primitive only if the runtime
also refuses construction on that unsupported platform.

Build a separate Docker fixture for the Proffer server with throwaway data,
state and staging volumes, an unprivileged UID, explicit loopback publication
and a fixture CA. Run an independent Python/raw HTTP oracle and an established
DAV client against it, then run Fetch_dav. This avoids accepting a shared client
and server misunderstanding as evidence of correctness. Evaluate a pinned litmus
suite as additional conformance coverage; document skips and do not equate one
suite's success with the security requirements above.

Retain Apache as an independent target for client tests. Add another disposable
server for extension interoperability when sync/CalDAV/CardDAV work is ported.
Ordinary tests never contact a live account. Existing live observations are
regression context, especially servers that ignore PUT preconditions.

## 12. Delivery sequence and unresolved work

| Milestone | Deliverable | Release gate |
| --- | --- | --- |
| S0 | Reconcile stock DAV extensions, reader/delegation policy and documentation | Shared API decisions, regression probes installed, parity established |
| S1 | Server codecs, protected mount, early head admission and reader-only store API | Authentication precedes body intake and generic condition rejection; disabled/method/path tests; no write path reachable |
| S2 | Hardened Eio reader and authenticated read-only example | Real confinement/race tests, independent downloads/listings, bounded resources |
| S3 | Extend admission to streaming uploads and add the mutation contract | Ordinary handler behavior preserved; framing/cancellation and conditional-write tests |
| S4 | Persistent writer, atomic PUT/MKCOL/PROPPATCH and namespace operations | Chosen journal design, fault/restart tests, conditional and quota correctness |
| S5 | Durable exclusive locks and complete recursive method behavior | Ownership, ancestor/destination, expiry/restart and independent DAV conformance |
| S6 | Range convenience, selected reports/sync and application extensions | Separate capability, resource-limit and interoperability evidence for each |

S2 is independently useful; S4/S5 must not be enabled merely because S2 ships.
Read-only operation must continue to work without acquiring private writable
state or enabling a lock manager.

The following remain explicit design/implementation work:

- An Eio/provider API for the hardened no-symlink/no-mount policy and atomic
  no-replace publication. Determine the supported backend matrix by tests.
- The persistent store/journal format and exact fsync/recovery ordering.
  Choose this before implementing the writer, not after exposing PUT.
- Proffer's concrete admitted-body and precondition-owner APIs, including
  how a protected endpoint owns its handler environment and mount subtree.
- Stable read views and validators for externally changed directories;
  the first supported production model uses managed stores or snapshots.
- A per-method RFC checklist and measured initial limits before setting DAV
  compliance headers.
- Range/resume support, streaming enumeration, shared locks, per-path ACLs,
  multi-process writers and online export reconfiguration.
- Server REPORT/sync, extended MKCOL, CalDAV/CardDAV, quota properties,
  Prefer, SEARCH and ACL editing. Client codecs in a sibling do not implement
  these server features.

For an initial configuration change, stop/drain the endpoint and reopen it
under a fresh export configuration. Dynamic revocation must later define what
happens to already-open downloads and admitted uploads; do not imply a grant
change retroactively retracts bytes already sent.

Implementation and examples must be ported deliberately to both standalone
trees, preserving pure-OCaml behavior and OxCaml capability ownership. Follow
the [DAV synchronization procedure](../httpz/dav/SYNC.md), expand its checked
scope for the new server files, run both compiler suites and package installation
checks, and record exact source revisions. Resolve the existing stock extension
first so later ports do not discard its work.
