# Changes

- Seed the `Form.multipart` boundary generator from the system on each
  call instead of keeping one `Random` state per domain.

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
