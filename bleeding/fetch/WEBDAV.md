WebDAV client over Fetch — investigation and implementation plan, 2026-09-07.

Implementation follow-up: the [Docker fixture](test/webdav/README.md) now works
locally over HTTP and verified HTTPS. The subsequent [httpz.dav specification](../httpz/dav/SPEC.md)
supersedes the packaging and initial-deliverable choices in this investigation.

The proposed first deliverable is a file-oriented client: inspect collections,
transfer files, change properties, create collections, copy, move, delete and
manage write locks. Collection synchronization follows on the same protocol
model. Calendar and address-book clients can reuse it later. This is a plan;
no WebDAV implementation or conformance claim accompanies it.

Use an optional `fetch-webdav` companion package with a `Fetch_webdav` module,
following the existing `fetch-signature` packaging pattern. Keep transport,
credentials and generic HTTP policy in Fetch. The substantive new work is XML
handling, property and operation results, and DAV-aware policy. No new network
backend is indicated by the source review.

The source review used OxMono revision `210521fa8`. Links below name the current
implementation rather than committing to its present line numbers.

The standards to implement and consult are:

| Specification | Relevance and proposed scope |
| --- | --- |
| [RFC 4918](https://www.rfc-editor.org/rfc/rfc4918.html) | Core properties, collections, namespace operations, locks and XML responses. Replaces RFC 2518. Read sections 4–10 and 13–18 first. |
| [RFC 9110](https://www.rfc-editor.org/rfc/rfc9110.html), [RFC 9111](https://www.rfc-editor.org/rfc/rfc9111.html) | Current HTTP semantics, conditional requests, redirects and caching; use alongside DAV's older HTTP references. |
| [RFC 7303](https://www.rfc-editor.org/rfc/rfc7303.html) | XML media types and encoding rules. Relevant to the initial decoder. |
| [RFC 5689](https://www.rfc-editor.org/rfc/rfc5689.html) | Extended MKCOL; updates RFC 4918. Add after basic collection creation. |
| [RFC 8144](https://www.rfc-editor.org/rfc/rfc8144.html) | DAV use of Prefer, including reduced responses and depth-noroot. An optimization after ordinary responses work. |
| [RFC 6578](https://www.rfc-editor.org/rfc/rfc6578.html) | Collection synchronization with sync tokens. The next substantial feature after file operations. |
| [RFC 3253](https://www.rfc-editor.org/rfc/rfc3253.html) | REPORT and supported-report-set are useful for synchronization. Full DeltaV version control is a separate undertaking. |
| [RFC 4331](https://www.rfc-editor.org/rfc/rfc4331.html) | Quota properties; a small, useful extension. |
| [RFC 3744](https://www.rfc-editor.org/rfc/rfc3744.html), [RFC 5397](https://www.rfc-editor.org/rfc/rfc5397.html) | Privilege/principal discovery and current-user-principal. Defer ACL editing until a consumer needs it. |
| [RFC 5323](https://www.rfc-editor.org/rfc/rfc5323.html) | SEARCH. A separate optional query API, not a prerequisite for directory listing. |
| [RFC 4791](https://www.rfc-editor.org/rfc/rfc4791.html), [RFC 6352](https://www.rfc-editor.org/rfc/rfc6352.html), [RFC 6764](https://www.rfc-editor.org/rfc/rfc6764.html) | CalDAV, CardDAV and their service discovery. These add domain-specific reports, properties and data formats. The discovery RFC does not define a general file-WebDAV well-known endpoint. |
| [RFC 3648](https://www.rfc-editor.org/rfc/rfc3648.html), [RFC 4437](https://www.rfc-editor.org/rfc/rfc4437.html), [RFC 5842](https://www.rfc-editor.org/rfc/rfc5842.html) | Ordered collections, redirect references and bindings. Defer, while retaining unknown property and error names. |
| [RFC 4709](https://www.rfc-editor.org/rfc/rfc4709.html), [RFC 5995](https://www.rfc-editor.org/rfc/rfc5995.html) | DAV mounting documents and server-assigned member creation. Optional application features. |

Use RFC 4918 as the normative reference and check its errata when making
fixtures. The RFC Editor's [informative rendering with verified errata](https://www.rfc-editor.org/rfc/inline-errata/rfc4918.html)
identifies EIDs 1068, 1430 and 1519; that rendering is a reading aid, not a
replacement specification. The [IANA method registry](https://www.iana.org/assignments/http-methods/)
is useful for checking safety and idempotence separately: PROPFIND, REPORT and
SEARCH are safe/idempotent; COPY, MOVE, MKCOL, PROPPATCH and UNLOCK are
idempotent but unsafe; LOCK is neither. These classifications do not establish
whether a particular failed exchange can usefully be retried.

Existing foundations and concrete gaps:

| Foundation | What is already available | DAV work |
| --- | --- | --- |
| [Fetch requests and responses](lib/fetch.mli) | Arbitrary token methods via `fetch`; scoped response lifetime; streaming request/response bodies; cancellation; status and final URL access. | Method wrappers and response interpretation. Use `Http.Method.of_string` for DAV methods. |
| [HTTPz methods](../httpz/lib/method.mli), [header names](../httpz/lib/header_name.mli) | Core DAV methods and several header names are already recognized. | These names are not DAV header-value parsers or client operations. |
| [Header codecs](lib/header.mli) | Custom codecs, portable codec constructors, conditional request fields and range support. | Add DAV codecs in the companion library: Depth, Destination, Overwrite, DAV, If, Lock-Token and Timeout. Preserve unknown capability tokens. |
| [Media codecs](../httpz/media/media.mli) | Streaming Bytesrw readers/writers, structured errors and configurable accepted media types. | Bounded XML adapters and structured DAV results. Media-type matching currently ignores parameters, so encoding selection needs explicit response-header handling. |
| [HTTPz URI](../httpz/uri/httpz_uri.mli), [Fetch URLs](lib/url.mli) | URI parsing, encoded paths, reference resolution, normalization and canonical HTTP origin comparison. | DAV href validation, collection-child construction and destination checks. Keep encoded resource identity separate from display names. |
| [Credentials](lib/credential.mli) | Scoped Basic and Bearer credentials, dynamic credentials and custom headers. | Start with these over HTTPS. Challenge-based Digest/Negotiate is not exposed as a portable credential facility; scope that separately if required. Basic currently restricts credentials to printable ASCII. |
| [Retry](lib/retry.mli), [redirect](lib/redirect.mli), [policy implementation](lib/client.ml) | Method allowlists, a request-level retry narrowing predicate, replay checks, redirect callbacks and request filters. | Supply explicit DAV policies; account for clients that already carry middleware. |
| [Xmlm](../../vendor/xmlm/xmlm.mli) | Vendored streaming parser/writer, expanded namespace names and portable interfaces. | Verify property fidelity, bound parser buffers and adapt buffered byte input. |
| [Fetch mock](lib/mock/fetch_mock.mli) | Request assertions and scripted status/body responses. | DAV fixtures plus real transport and server interoperability tests. |

The protocol model must preserve information that a file listing convenience
API would otherwise discard. A 207 response can describe success, failure or
mixed outcomes, with resource-level status or property-level propstat groups.
PROPPATCH instructions are ordered and atomic at the resource. An allprop
request does not promise every live property. DAV hrefs use absolute URIs or
absolute-path references, with a consistent form within a multistatus.
Depth has method-specific rules. Locks and ETags solve different aspects of
concurrent editing. These constraints come from [RFC 4918](https://www.rfc-editor.org/rfc/rfc4918.html).

For the implementation, start with one library under `webdav/`, with private
modules for XML, properties, multistatus, conditions and exchange handling.
Its direct dependencies should be `fetch`, `http`, `uriz`, `xmlm`, `bytesrw`
and `eio`, adding `ptime`/`duration` where the exported types require them.
The dependency on Xmlm belongs to this optional package. Expose immutable
protocol values with checked portability; keep parser state, flows and lock
refresh fibers scoped to their exchange or Eio switch. Bounded memory and
avoiding whole-response copies are the performance goals; parsed XML values
will allocate.

The proposed API surface is:

| API area | Shape and design decision |
| --- | --- |
| Client | `v ~root fetch` retains a supplied Fetch capability and explicit DAV root policy. No ambient client, credential store or automatic network discovery in the constructor. |
| Properties | An expanded name `(namespace, local_name)` plus typed decoders for common values. Retain bounded XML fragments for unknown values, attributes and mixed content. Preserve namespace context where values contain qualified names. |
| Results | A multistatus contains resource outcomes and propstat groups, including DAV error names, descriptions and locations. Keep absent, failed and successfully empty properties distinct. Expose the complete low-level result and derive friendly file entries from it. |
| Reads | `options`, `propfind`, `stat`, `list`, `with_download`. `stat` requests explicit properties at depth zero; `list` requests depth one and separates the collection itself from its children. Recursive walking is explicit and bounded. |
| Mutations | `put`, `mkcol`, `proppatch`, `copy`, `move`, `delete`. Use ordinary Fetch bodies for uploads and explicit conditions for writes. Default high-level copy/move to no overwrite, with an explicit replacement option. |
| Conditions | Typed HTTP ETags plus a DAV If AST for tagged and untagged lists, token/ETag terms and negation. Encode method-specific depth types rather than allowing every depth on every operation. |
| Locks | `lock`, `refresh_lock`, `unlock`, followed by `with_lock ~sw ~clock`. A lease records its root, token, scope, depth and granted timeout. Refresh failure is observable; teardown performs bounded best-effort release without hiding the original failure. |
| Streaming | A fold/callback API consumes resource outcomes within `with_response`. A bounded materializing convenience API uses that same decoder. An early stop closes the response; it cannot return a completed sync checkpoint. |
| Errors | Separate transport/decode exceptions, whole-request HTTP rejection and resource/property failures. Keep bounded non-XML error diagnostics, since an authentication gateway may return HTML. Never convert every 2xx into application success. |

The XML spike is the first correctness gate. Feed Xmlm from a buffered
Bytesrw/Eio adapter, with byte accounting on decoded response bytes. Use
`strip:false`, expanded-name matching and a writer instead of XML string
concatenation. Plan independent limits for total bytes, nesting, element/
attribute counts, names, text and preserved property fragments. Reject DTDs,
leave external-entity resolution disabled, and require end-of-input after the
single root. Counting events after parsing is insufficient to bound one giant
token: Xmlm exposes a `Make` functor with a replaceable buffer implementation
that can cap growth. Test the bound at allocation time.

There is a specific fidelity issue to settle before implementing arbitrary
dead-property round trips: Xmlm's documentation says it collapses whitespace
in all attributes, and `p_attr_value` in [xmlm.ml](../../vendor/xmlm/xmlm.ml)
calls `addc_data_strip`. Add an attribute/mixed-content preservation probe;
if it demonstrates loss of required property information, make a focused
Xmlm correction or select an adapter that preserves that information. Do not
claim lossless round trips based only on `strip:false`. Likewise, test HTTP
charset, BOM and XML-declaration combinations against
[RFC 7303](https://www.rfc-editor.org/rfc/rfc7303.html). A generic media codec
that ignores Content-Type parameters cannot make that choice on its own.

Policy needs explicit design rather than just additional method wrappers:

1. **Authority includes destinations.** Fetch's `restrict ~under` checks the
   request URL. A COPY/MOVE Destination header describes another affected
   URL. Provide a DAV-aware `restrict ~root` request filter which checks both
   with Fetch's canonical origin/path semantics. Keep destinations within the
   root by default. A previously narrowed opaque capability does not reveal
   its hidden restrictions, so it cannot automatically authorize destinations
   by proxy: require an explicit DAV policy at capability construction, and
   document that limitation. Revalidate server-returned hrefs before use;
   reading a reference must never itself cause a request.

2. **Read-only and retry policies are distinct.** Fetch's `read_only` permits
   GET, HEAD and OPTIONS only. Supply a DAV-specific method allowlist with
   PROPFIND, adding REPORT/SEARCH only for implemented read operations. Fetch
   retry defaults also omit DAV methods. Recommend retries for replayable
   reads, with the existing `retry_request` predicate to veto conditional
   writes where outcome ambiguity matters. Do not automatically retry LOCK
   acquisition. Do not stack independent retry loops. An already-wrapped
   Fetch capability cannot have its retry policy removed by the DAV client;
   build the recommended stack explicitly. Consider a generic per-request
   retry veto only if opaque preconfigured clients become a concrete blocker.

3. **Redirects must preserve the operation.** Fetch can rewrite a 303 to GET
   and replay a string body on 307/308. Initially stop redirects on mutations
   and allow only explicitly authorized, method-preserving read redirects.
   Discover collection trailing-slash URLs before sending a one-shot upload.
   Mark If and Lock-Token sensitive for tracing, and do not forward lock
   conditions to another resource. Keep origin credentials scoped. Parse
   redirected child results as data rather than following them automatically.

4. **A write failure can leave an unknown outcome.** Preserve that fact when
   a connection fails after a mutation might have reached the server. A
   conditional retry returning 412 does not prove that the first attempt
   failed. Let the application reconcile by inspecting current state. Streams
   remain one-shot; a future replayable-upload API would need a fresh-source
   factory, not an attempt to reuse an exhausted flow.

5. **Encoding and resource identity stay explicit.** Validate response hrefs
   against DAV's reference forms, then use Uriz resolution against the final
   response URL. Do not decode a whole path before splitting it: test `%2F`,
   `%25`, spaces, non-ASCII names, query strings and collection trailing
   slashes. Store the raw href alongside its validated target. Compatibility
   with non-conforming relative hrefs should be an explicit tested option.

6. **Avoid gratuitous Fetch changes.** DAV header codecs fit the existing
   extension API. For an intentionally empty DAV request body, use
   `Fetch.String ""` when explicit zero-length framing is wanted: current
   transports automatically frame `Empty` this way only for POST/PUT/PATCH.
   XML encoding selection and DAV responses belong in the companion. Core
   changes should be driven by a failing transport/policy test.

Collection synchronization is a second protocol feature, not a GET loop.
[RFC 6578](https://www.rfc-editor.org/rfc/rfc6578.html) defines initial and
incremental sync-collection reports, opaque collection-scoped tokens,
removals represented by resource 404s, invalidated-token recovery and truncated
results with a continuation token. Its truncation signal includes a 507 for
the request URL inside multistatus; this must not be confused with every
other use of 507.

Propose `sync_page ~token ~limit` returning changes, deletions, a next token
and a completion/truncation indication. A fold can stream one page, but only
publish the next token after successful parsing and application of the page.
The application owns persistence: store changes and the token in one
transaction. On truncation, continue with the returned token; on invalidation,
perform a complete relisting/reconciliation. Where sync reports are absent,
offer an explicit ETag-based polling fallback. Never infer deletions from a
partial or failed listing, and do not treat a collection's own ETag as a
recursive change journal.

Implementation sequence and acceptance criteria:

| Milestone | Work | Evidence required |
| --- | --- | --- |
| 1. Protocol foundation | Package skeleton, property/result types, XML reader/writer, header codecs, href handling. | Namespace-prefix independence, unknown extensions, mixed content, UTF-8/UTF-16, malformed/truncated XML, entity/DTD refusal and pre-allocation limits. Resolve the Xmlm fidelity question. |
| 2. Useful read client | OPTIONS, explicit-property PROPFIND, stat/list, streamed GET, DAV read-only/redirect policies. | Mock request assertions; mixed 207/propstat results; out-of-root href handling; depth-zero/one behavior; early-stop/cancellation cleanup; real transport framing. |
| 3. File mutations | Conditional uploads, basic MKCOL, COPY/MOVE, DELETE, ordered PROPPATCH. | Destination confinement; overwrite controls; partial collection failures; stale ETags; upload replay refusal and uncertain outcomes; no hidden retry amplification. |
| 4. Locks | Discovery, acquisition, refresh, release, then scoped refresh helper. | Expiry and renewal using deterministic clocks; tagged If conditions; parent/destination locks; server-granted timeouts; cancellation and release failures; no duplicate acquisition retries. |
| 5. Synchronization | REPORT support, supported-report discovery, sync pages and application checkpoint contract. | Initial/incremental changes, removals, invalid tokens, truncated pages, partial XML, interrupted application and ETag-polling fallback. |
| 6. Extensions and adoption | Extended MKCOL, quota and Prefer first; then a concrete CalDAV/CardDAV or SEARCH consumer. | Each extension is capability-tested and has server interoperability fixtures; base behavior remains available when unsupported. |

Run codec and method tests from the first milestone using Fetch.mock. Add
socket tests against both fetch-httpz and fetch-curl as soon as the read client
exists: fragmented/chunked and compressed XML, custom methods with XML bodies,
empty bodies, limits, connection cleanup and cancellation. Include macOS
transport coverage on macOS CI when the package claims that backend.

Use two independent server implementations for the first file-client release:
[Apache mod_dav](https://httpd.apache.org/docs/2.4/mod/mod_dav.html) and
[Nextcloud's file WebDAV API](https://docs.nextcloud.com/server/latest/developer_manual/client_apis/WebDAV/basic.html).
Keep service-specific behavior, such as resumable upload protocols, in explicit
extensions. A self-written test server alone would risk confirming the same
misinterpretation in both sides. Pin fixture versions and record supported
operations rather than making a blanket server compatibility claim.

Build the OxCaml version under `opam exec --switch=5.2.0+ox --` in the
development and `release-check` profiles. Keep XML fixtures usable by stock
OCaml and follow the existing sibling-port process once this API settles.
Benchmark peak memory on large listings and cancellation latency, as well as
throughput; zero allocation is not an appropriate contract for constructing
arbitrary property values.

The read client is the smallest independently useful release. Generic dead
properties and their XML fidelity, reliable lock lifecycle, and durable sync
checkpoints account for most of the correctness work. The practical first
implementation step is milestone 1 plus a depth-one PROPFIND against both
target servers, before committing to a larger public API.
