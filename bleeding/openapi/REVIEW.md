# OpenAPI generator review

Reviewed 2026-09-07 against the Fetch implementation in this checkout, including
`Fetch.Json`, `Fetch.decode`, `Fetch.Form`, scoped credentials, restrictions,
retry/limit middleware, and `Fetch.mock`. The review covers spec parsing, schema
analysis, client generation, runtime helpers, CLI output and the Karakeep,
PeerTube and Immich consumers. Fixes below are applied; the remaining findings
are follow-up work, not claims of complete OpenAPI conformance.

## Fixed

| Priority | Finding and correction |
| --- | --- |
| P1 | Every operation read its entire body with `Eio.Flow.read_all`, then decoded JSON without checking the media type. Generated operations now use `Fetch.encode` and `Fetch.decode` inside `Fetch.with_response`. Success bodies default to a configurable 16 MiB limit; JSON nesting is bounded by Fetch. |
| P1 | API error bodies were unbounded, and the status could be lost during decoding. Diagnostics are capped at 64 KiB; oversized bodies produce a marker while preserving the HTTP status. Typed-error decoding falls back to bounded JSON/raw diagnostics. |
| P1 | Automatic write redirects could forward private request bodies. Mutating methods and requests with bodies now use `redirects:0`; bodyless GET/HEAD retain Fetch's redirect policy. |
| P1 | Path values were interpolated without escaping and could change the path or query. Rendering now substitutes once, percent-encodes values, rejects dot segments and missing parameters, and escapes query names as well as values. Base URLs are validated and normalized; error URLs omit queries. |
| P1 | Arrays of component responses were decoded as a single object; only 200/201 were examined. Wire shapes retain array/nullable wrappers, all declared 2xx responses are considered, empty responses return `unit`, and mixed empty/value responses return an option. Explicit statuses take precedence over wildcard empty responses. |
| P1 | DELETE/HEAD/OPTIONS request bodies were silently dropped, optional bodies were mandatory, and non-JSON bodies were mislabeled as JSON. Declared bodies now drive generation: JSON codecs, `Fetch.Form.urlencoded`, `Fetch.Form.multipart`, or `Fetch.body`. Optional bodies can be omitted. Non-JSON responses return bounded strings. |
| P1 | Wildcard typed errors generated invalid OCaml patterns, default errors were omitted, and error-codec dependencies were missing. Error definitions now use status strings with exact/range/default precedence and include the required module dependencies. An exact untyped definition also overrides a typed range. |
| P1 | Path-level parameters were concatenated with overrides, producing duplicate labels. Parameters now merge by `(name, location)`. Local parameter, request-body and response aliases resolve with cycle detection. Unresolved references in these locations fail generation. Header parameters are emitted; the spec's special Accept/Content-Type/Authorization parameters are ignored. |
| P1 | JSON tree conversion serialized and parsed again, and failed encoding silently returned JSON `null`. Runtime and reference codecs now use `Jsont.Json` directly and propagate encoding failures. |
| P1 | Required nullable members accepted omission and were omitted on encoding. They now require presence and encode `None` as JSON null. Nullable validation and nullable array items are retained. Optional nullable members now use two options to distinguish absence, null and a value; defaults do not erase nullable presence. |
| P1 | Numeric codecs accepted truncated fractions, non-finite numbers, and large-integer strings despite declaring JSON numeric types. Strict numeric wrappers reject those conversions. Values outside Jsont's exact JSON-number encoding range fail explicitly; full-width integer support remains open. |
| P2 | Validation counted UTF-8 bytes, silently ignored invalid regexes, ran only on decoding, and compared integer bounds through a lossy float conversion. Validation now runs both ways, counts code points, reports regex construction errors, preserves integer widths, and compares integers to bounds without rounding the integer. Array uniqueness uses JSON equality, ignoring metadata and object member order, with sorting rather than quadratic membership scans. Float bounds are emitted with round-trip precision. |
| P2 | Undiscriminated `oneOf` used the same first-success behavior as `anyOf`. It now requires exactly one successful decoder. Whole-schema guards now enforce exclusivity on encoding as well, using every branch's constraints. |
| P2 | Cyclic `allOf` expansion could overflow the stack. It now fails with a cycle diagnostic. Referenced composed schemas retain their direct properties as well as inherited properties. The original `allOf` graph is retained for validation, so every conjunct is checked even when properties are flattened for the OCaml record. |
| P2 | Client construction required a curl environment even when the caller already had Fetch. `of_fetch` accepts any Fetch capability and retains its middleware. `create ?session` remains available. No separate authentication or retry implementation was added. |
| P2 | The old single-phase generator duplicated the active two-phase generator. Its unused implementation was removed. HTTP/error handling is shared in `Runtime.Client`, eliminating roughly 10,000 lines across regenerated consumers. |
| P2 | Spec comments could terminate or nest generated OCaml comments, and output package names could contain paths. Comment delimiters are escaped, names are checked, regeneration dependencies are quoted, and file channels close on exceptions. CLI examples now use the installed `openapi-gen` name. Package dependencies and outdated documentation were corrected. |
| P1 | The schema codec rejected 3.1 type arrays/boolean schemas and 3.0 boolean exclusive bounds. Parsing now preserves these forms and reference siblings. Validation distinguishes 3.0 reference sibling rules and nullable semantics from 3.1 numeric bounds and type unions. Unsupported specification versions fail generation. |
| P1 | Primitive/array component schemas and aliases lost their types or constraints. They now get scalar/list codecs; numeric and mixed enums, const, decimal `multipleOf`, object property limits, additional properties, nested composition and inline operation schemas are validated on decode and encode. Decimal multiples use Zarith rationals derived from the shortest round-trip decimal spelling. |
| P1 | Recursive and forward references silently erased validation. An immutable schema graph resolves component aliases and nested JSON pointers, including escaped names. Recursive fields retain list/option wrappers; mutual recursion uses an explicit JSON representation. Both paths validate the referenced schemas. Invalid or unsupported references fail before output. Validation depth is bounded at 128 schema/data steps. |
| P1 | Different successful response schemas fell back to unchecked JSON. Generated clients now validate the selected schema for the actual response status even when the return type is generic JSON. Invalid inline JSON request bodies fail before I/O. |
| P1 | Discriminators used snake-case tags instead of implicit schema names. Tags now use exact names or explicit mappings, and encode/decode check them. Whole-value discriminator codecs compose with schema guards without requiring a Jsont object-map representation. |
| P2 | Name normalization could shadow functions or generate conflicting declarations. Schema modules, fields, enum/union constructors and operation names are checked before output. Path-derived operation names retain parameter segments; Karakeep's list/get-one backup functions now have distinct names. Reserved accessor names are escaped. Global forward-reference/schema state was replaced with a per-generation context. Sibling array dependencies are included in module ordering. |
| P2 | Defaults could truncate integers, lose float precision or generate ill-typed null/enum literals. Only representable defaults are materialized; floats use round-trip precision. Materialized defaults are checked against their schema. Required/nullable properties retain presence semantics, and unsupported defaults remain annotations. |
| P2 | Spec validation and writes were partial. Duplicate map/response definitions, fractional/negative schema counts, unsupported versions, invalid status/media keys, malformed paths and missing required path declarations now fail early. Upstream path declarations absent from a template are diagnosed and omitted from generated arguments. Output names are all checked before writes; each file is atomically replaced after a successful flush, with temporary cleanup on failure. CLI validation and filesystem errors produce normal diagnostics. |

Operation parameter overrides and response selection were checked against the
[OpenAPI 3.1.2 parameter definition](https://spec.openapis.org/oas/v3.1.2.html#parameter-object)
and [response rules](https://spec.openapis.org/oas/v3.1.2.html#responses-object).
The implementation is reviewed primarily against the repository's own Fetch
interfaces, rather than assumptions about an external HTTP library.

## Remaining findings

### 1. P1 — Directional schemas and complete JSON Schema conformance

The new guards enforce the supported schema vocabulary, but this is still a
subset implementation. Separate request/response interpretation of
`readOnly`/`writeOnly` is not implemented. Formats are not generally assertions;
regular expressions use Re's supported PCRE syntax rather than a full ECMAScript
engine. Custom dialects, vocabularies, external references and dynamic references
need a broader resolver. Known unsupported validating keywords (for example
`prefixItems`, `contains`, conditionals and `unevaluatedProperties`) fail
explicitly instead of being silently treated as implemented.

The [3.0 Schema Object](https://spec.openapis.org/oas/v3.0.4.html#schema-object)
and [3.1 Schema Object](https://spec.openapis.org/oas/v3.1.2.html#schema-object)
have different rules. Supporting their common forms does not establish full
conformance to either version.

### 2. P2 — Typed representation and round-trip preservation

Recursive fields and mutually recursive components are validated but can use
`Jsont.json`; nullable object components also use an opaque nullable value.
Record codecs still discard unmodeled additional members after validating them.
`allOf` constraints are intersected by the guard, but property flattening can
produce a less expressive OCaml representation than the full intersection.
Typed recursive modules, extensible records and structured type/codec expressions
remain improvements. Generation still localizes some references through strings.
Pure alias and cyclic `allOf` expansion are diagnosed; they do not produce
recursive OCaml definitions.

### 3. P2 — Parameter serialization and representation alternatives

Scalar path/query/header arguments remain strings. Cookie parameters,
matrix/label/deepObject, array/object styles, `explode`, `allowReserved` and
parameter `content` codecs are not implemented. Request selection prefers JSON,
otherwise the first media entry. Form/multipart callers supply fields and parts;
schema `encoding` metadata is not applied automatically. Wildcard request media
still need an API for a concrete Content-Type.

Different JSON success shapes return validated `Jsont.json`; mixed JSON/non-JSON
representations return bounded raw strings. Explicit request/response variants,
response headers/status metadata, root/path/operation server overrides, security
requirements and TRACE need further work. Callers supply the intended base URL
and scoped Fetch credential stack.

### 4. P2 — Streaming responses need a scoped API

Binary responses are buffered. SSE/NDJSON, large downloads, pagination headers
and response metadata need callback-based operations that consume responses
inside `Fetch.with_response`. Fetch already provides the necessary flow,
JSON-lines and SSE interfaces.

### 5. P2 — Full-width JSON numbers and complete spec validation

Jsont represents JSON numbers through floats and uses strings for large OCaml
integers. Strict wrappers reject fractional integers, non-finite values and the
outbound type change, but cannot provide the complete int64 JSON numeric range.
Exact original numeric lexemes and bounds remain unavailable. Rational
`multipleOf` validation fixes represented decimal arithmetic, not lost input
precision.

Preflight covers the invariants exercised by generated code, rather than every
OpenAPI invariant or annotation. Nonstandard vendor paths still need correction
in the supplied specification: PeerTube's snapshot includes literal `:logoType`
segments, plus an unused `id` parameter on a list operation. Generation diagnoses
these unused path declarations; it does not infer a replacement path. Unsupported defaults remain annotations.
Replacement is atomic per file, not for the entire output directory. A later
filesystem failure can leave an earlier file updated.

## Compatibility and validation

Corrected generated signatures can change: array responses return lists; empty responses return unit; mixed empty
responses return options; optional bodies are optional; form, multipart and raw
bodies use their Fetch representations. JSON responses now require a declared
Content-Type. Required nullable fields must be present in responses. Optional nullable fields
use `None` for absence, `Some None` for explicit null and `Some (Some value)`
for a value. For a string field, constructors expose `?field:string option`;
`~field:None` explicitly clears it. Defaults do not replace absent nullable
fields. Primitive/array component aliases now expose their actual scalar/list
types. Operations without an `operationId` include path parameters in their names,
for example `get_bookmarks_by_bookmark_id`. Request validation runs before I/O. Invalid/unsupported regexes now fail instead of
silently dropping their checks. Large numeric values can fail instead of being
sent as JSON strings.

Generated sources for Karakeep, PeerTube and Immich were regenerated by their
Dune rules. Their libraries and CLI executables compile. Karakeep's handwritten calls were
updated for parameter-derived names, and its title display flattens nullable
presence with `Option.join`. All three CLI `--help=plain` startup checks pass.

Validation uses `opam exec --switch=5.2.0+ox --`:

- `dune build --profile release @bleeding/openapi/runtest`: 24 existing tests,
  6 runtime regression groups, 9 Fetch compiled-client groups and 12 schema
  correctness groups: 51 total.
- The generated fixture covers component aliases, overriding parameters,
  headers, typed/nested arrays, empty/wildcard responses, vendor JSON, optional
  and DELETE bodies, forms, multipart and raw bodies, error precedence,
  credentials across redirects, byte/depth limits, cancellation, validation,
  and exchange closure on success and exceptions.
- A separate compiled OpenAPI 3.1 fixture covers boolean/type-union schemas,
  scalar/array aliases and ordering, nullable presence, field/union/intersection
  validation, recursive and mutually recursive values, decimal multiples,
  discriminator tags, nested pointers, per-status responses, invalid requests
  before I/O, defaults, generated-name collisions and atomic file failures.
  Direct codec tests cover 3.0/3.1 exclusive-bound/reference semantics and
  bidirectional schema-model round trips.
- Consumer executables: `bleeding/karakeep/bin/main.exe`,
  `bleeding/peertube/bin/main.exe`, `bleeding/immich/bin/main.exe`.
- Package metadata: `bleeding/openapi/openapi.opam`.

The generated-client tests use Fetch's mock backend. They establish generator
behavior at the Fetch boundary, not libcurl framing or live server compatibility.
The default development profile remains blocked by the previously observed
unrelated fatal unused-value warning in `bleeding/httpz/uri/uri_template.ml:66`;
validation uses the release profile.

`@bleeding/openapi/doc` was also attempted. The installed odoc rejects dependency
artifacts (including `bytesrw.cmti`, `jsont__.cmt`, and `ptime.cmti`) with
`not an interface` / `not an implementation`. Documentation generation is
therefore unverified under this switch; the test and executable targets build
independently of that failure.
