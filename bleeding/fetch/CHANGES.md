# Changes

- Remove fetch-httpz's decompress upper bound. The workspace uses its vendored
  decoder and verifies the gzip shim when refreshing it.

- Allow decompress 1.6.1 in fetch-httpz, retaining the gzip-header compatibility
  shim and requiring review before accepting 1.6.2 or later.
