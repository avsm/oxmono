# OxMono import

Imported capnp-ocaml 3.6.0 from its Git tag. `src/runtime`, `src/unix` and
`src/compiler` are unchanged. Tests, examples and benchmarks, which need the
C++ `capnp` tool at build time, are omitted.

The `capnpc-ocaml` plugin built here generates the schema code checked in by
capnp-rpc and ocluster-client. The C++ `capnp` tool only parses a schema, so
it can run in a container and pass its request on standard output:

```sh
docker run --rm -v "$PWD":/w -w /w debian:bookworm-slim sh -c \
  'apt-get update -qq && apt-get install -y -qq capnproto >/dev/null &&
   capnp compile -o- schema.capnp' > schema.req
_build/default/vendor/capnp/src/compiler/main.exe < schema.req
```

The checked-in code was generated with Cap'n Proto 0.9.2 from Debian 12.

On refresh, compare the three directories with the new tag, rebuild the
plugin, regenerate the checked-in schema code and rebuild its users.
