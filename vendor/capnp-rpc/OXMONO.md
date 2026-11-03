# OxMono import

Imported capnp-rpc 2.1.2, the Eio version, from its Git tag. The
`capnp-rpc`, `capnp-rpc-net` and `unix` libraries are kept. Tests, examples,
fuzzing and benchmarks are omitted.

Local patches:

- `capnp-rpc/rpc_schema.ml` and `capnp-rpc/persistent.ml`, with their
  interfaces, are generated from the upstream schemas and checked in. Their
  Dune rules, which run the C++ `capnp` tool, are removed. See
  [capnp's notes](../capnp/OXMONO.md) for regenerating them.
- `capnp-rpc-net/endpoint.ml` drops its Prometheus connection and message
  counters, so the library does not need Prometheus. The opam files no longer
  list it or `conf-capnproto`.
- `capnp-rpc-net/auth.ml` builds its certificate name with
  `X509.Distinguished_name.Common_name.v`, as the workspace's X.509 types it.

On refresh, reapply these patches, regenerate the schema code, and build
`vendor/capnp-rpc/unix/capnp_rpc_unix.cmxa` and `@wip/ocluster-client/all`.
A live check submits a job with `ocluster-submit`.
