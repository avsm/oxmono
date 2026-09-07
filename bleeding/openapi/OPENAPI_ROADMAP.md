# OpenAPI generator roadmap

The [2026-09-07 review](REVIEW.md) records implemented correctness fixes,
compatibility changes, remaining limits and validation evidence.

## Implemented

- Fetch injection, typed JSON encoding/decoding, checked Content-Type, bounded
  responses/diagnostics, scoped response lifetime and write redirect policy.
- Fetch forms, multipart parts and raw bodies, with caller-owned credentials,
  retries, limits and backend policy.
- Dialect-aware schema guards for 3.0/3.1 bounds, nullability, type unions,
  boolean schemas and reference siblings.
- Component scalars/arrays/aliases, optional nullable presence, encode/decode
  constraints, union exclusivity and status-specific response validation.
- Immutable reference validation graph, nested JSON pointers, validated opaque
  recursive values and per-generation naming/order context.
- Generation preflight, default handling, atomic file replacement and normal
  CLI error diagnostics.
- Compiled 3.0/3.1 regression clients and regenerated Karakeep, PeerTube and
  Immich consumers.

## 1. Extend schema fidelity

Model request/response directions for readOnly/writeOnly. Preserve additional
record members. Generate typed recursive definitions and more faithful allOf
representations. Replace remaining type/codec string manipulation with structured
expressions. Expand dialect, vocabulary, external reference, format and regex
support with explicit diagnostics and conformance tests.

## 2. Preserve exact JSON numbers

Retain original numeric lexemes and full-width integer JSON numbers through
parsing and encoding. Current codecs reject lossy outgoing representations;
rational decimal multipleOf validation cannot recover input precision already
lost by float parsing.

## 3. Model parameters and representation alternatives

Generate typed scalar/array/object parameters with style/explode rules, cookie
parameters and content codecs. Apply form/multipart encoding metadata. Generate
explicit variants for alternative request media and success status/media shapes,
including headers and concrete Content-Type for wildcard requests. Apply server
overrides deliberately and expose security requirements while retaining Fetch
credential injection.

## 4. Add scoped streaming operations

Use Fetch flows, JSON Lines and SSE decoders through callback-based operations.
Include download/status/header access and configurable stream limits while
keeping response consumption inside Fetch.with_response.

## 5. Expand diagnostics and validation

Check more OpenAPI invariants and make opaque representation choices visible.
Keep the compiled fixture corpus and consumer builds alongside each change.
Directory-wide transactional output remains separate from the implemented atomic
replacement of individual files.
