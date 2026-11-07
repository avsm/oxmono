# Changes

- `Form.multipart` draws its boundary through `Httpz.Range.generate_boundary`
  from an optional `random` flow, or from the domain's default `Random`
  generator, instead of a `Random` state of its own shared across domains.

- Borrow request bodies through middleware, retries, redirects and HTTPZ.
  Generate explicit owned request copies with ppx_globalize for asynchronous
  backends and retaining mocks.

- Clarify opam descriptions of package functionality and interfaces.

- Update CLI rendering to the current Console View and terminal APIs.

- Make Link encoding/decoding portable and match relation tokens in
  multi-relation Link values. Add typed Content-Location decoding.

- Remove fetch-httpz's decompress upper bound. The workspace uses its vendored
  decoder and verifies the gzip shim when refreshing it.

- Allow decompress 1.6.1 in fetch-httpz, retaining the gzip-header compatibility
  shim and requiring review before accepting 1.6.2 or later.
