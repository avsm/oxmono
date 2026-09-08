# davz and proffer.dav

`davz` implements the transport-independent portion of a file-oriented WebDAV
client. `proffer.dav` exposes it as a Fetch client capability. The latter name
is intentional: it is the requested client integration, not a Proffer server
handler. Neither library selects a transport or constructs credentials.

The first version implements RFC 4918 request XML, multistatus and lock discovery
decoding, DAV header values and href validation. The integration supports
OPTIONS, PROPFIND, PROPPATCH, MKCOL, COPY, MOVE, DELETE, PUT, scoped downloads,
LOCK, refresh and UNLOCK. HTTP representation features use Fetch's existing
headers and streaming bodies. The [standards investigation](../fetch/WEBDAV.md)
records the extension roadmap and the [Docker fixture](../fetch/test/webdav/README.md)
provides a locally verified Apache interoperability target.

## Protocol model

XML names are expanded `(namespace URI, local name)` pairs. Property values
retain attributes, mixed text/element content, inherited namespace bindings and
`xml:lang`; namespace prefixes embedded in property text must remain meaningful
when a fragment is written back. XML lexical details such as CDATA boundaries,
quote style and comments are not property identity. Attribute normalization
must follow XML's CDATA rules, preserving repeated spaces and character
references. DTDs and unresolved entities are rejected.

A multistatus is a list of resource responses plus an optional description.
Each response contains hrefs and either a whole-resource status or property
status groups, never an invented aggregate success flag. Property groups retain
their XML values, numeric status, DAV:error children and description. Resource
responses also retain their location. Unknown extension elements outside these
defined fields are ignored as RFC 4918 requires. Missing, malformed or duplicate
mandatory structural fields are errors. An empty multistatus is valid.
Repeated property names are retained: ordered PROPPATCH instructions may
produce several reports for the same name. `property_results` returns every
occurrence; the singular `property` accessor rejects ambiguous results.
Servers may group reports by status, so response order need not match request
order. Read the property again to learn its final value after a successful patch.

Href spelling is retained for diagnostics. Only absolute HTTP(S) URLs or
absolute-path references are accepted, with one form throughout a multistatus.
Resolution uses the response's URL. Percent escapes are normalized by Uriz for
comparison, never decoded into path separators. Returned hrefs confer no new
authority: each later request is checked against the client root again.

Lock tokens are validated opaque absolute URIs, not restricted to a UUID scheme.
Lock discovery retains scope, depth, timeout, owner, root and token. The granted
timeout is server data and may differ from the requested value. DAV If uses an
AST for OR-ed condition lists, AND-ed terms, negation, entity tags and state
tokens, with tagged and untagged forms kept distinct. Empty lists and malformed
tokens cannot be serialized. If-Match accepts strong HTTP ETags only.

## Bounds and ownership

The first decoder materializes bounded XML responses. Defaults are 8 MiB of
input, depth 64 and 100,000 XML nodes/attributes. A byte limit bounds parser
tokens as well as the document; depth is checked before recursive descent.
These are configurable local limits, not protocol restrictions. The same limits
apply to successful XML responses and XML error bodies. XML encodings use Xmlm;
the integration honors supported HTTP charset labels and BOM precedence.

The first version does not promise constant-memory collection enumeration.
A streaming resource fold is a future extension; adding it must keep complete
document validation separate from delivery of partial results. File uploads
and downloads already stream through Fetch. A download callback owns the
response only for its dynamic extent, including error and cancellation paths.

Protocol data is immutable and checked portable. Parser and transport state is
exchange-local. XML errors are explicit results in davz; the integration raises
a distinct protocol exception. HTTP rejection contains status, response headers
and a bounded body, with a parsed DAV error when available. A 207 remains a
structured outcome even when it contains failures.

## Client policy

Construction takes a Fetch capability and an absolute collection root ending
in `/`. Requests and COPY/MOVE destinations must remain beneath that root and
on its origin; source-only Fetch restrictions cannot protect Destination.
Redirects stop for every DAV operation in this initial API. The caller may
explicitly construct another root after inspecting an HTTP redirect result.
The integration does not add retries. Supplied Fetch middleware remains active:
applications requiring single-attempt mutations must supply a capability with
retries disabled. A lost reply leaves a mutation's outcome uncertain.

Depth types are method-specific. PROPFIND defaults to zero; COPY accepts zero
or infinity; MOVE is infinity; LOCK accepts zero or infinity. COPY/MOVE default
to `Overwrite: F`. XML request bodies are UTF-8, and empty DAV request bodies use
`Fetch.String ""` for explicit zero-length framing. Lock refresh sends an empty
body and an untagged DAV If; UNLOCK sends Lock-Token. `lock_condition` produces
a tagged condition naming the lease URL, including when the lease covers a
COPY/MOVE destination or a parent collection. A session does not silently refresh or
release locks in a background fiber.

## Validation and subsequent work

Pure tests cover hostile/invalid XML, namespace and whitespace fidelity,
mixed propstat results, empty documents, href forms, lock and If syntax and
resource bounds. Fetch mocks check request serialization, failures, redirects,
root/destination restrictions and download lifetime. An explicit Docker client
test repeats property, file and lock workflows against Apache over HTTP and
HTTPS, trusting only the fixture's CA.

RFC 6578 sync, RFC 5689 extended MKCOL, RFC 8144 Prefer, automatic lock leases,
CalDAV/CardDAV, ACL editing, search and server-side Proffer handlers are later
features. No general WebDAV conformance claim accompanies this initial client.

Normative references: [RFC 4918](https://www.rfc-editor.org/rfc/rfc4918.html),
[RFC 9110](https://www.rfc-editor.org/rfc/rfc9110.html),
[RFC 3986](https://www.rfc-editor.org/rfc/rfc3986.html),
[RFC 7303](https://www.rfc-editor.org/rfc/rfc7303.html) and
[XML 1.0](https://www.w3.org/TR/REC-xml/).
