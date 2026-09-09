# Import provenance

Imported the shared types and MQTT 3.1.1/5.0 packet implementation from
`../monopampam/ocaml-mqtte` at monorepo commit
`a59a113719c994e5e95479cc3964b2ed7ccc68a5` on 2026-09-09.
The original ISC notice is retained. The upstream package source identifies
`https://tangled.org/anil.recoil.org/ocaml-mqtte`.

## Interface changes

- `Mqtte` becomes `Mqttz`. `V3.Packet` and `V5.Packet` retain the source packet
  types with PUBLISH payloads changed from strings to byte slices.
- Bytesrw packet readers and writers become exact-frame `decode`, segmented
  `encode`, and copying `to_bytes`. Scalar parsing uses bounded local cursors.
- `Mqttz_eio` exposes the client directly. The old transport and partially
  implemented protocol/session internals are private or replaced.
- Callback delivery becomes bounded `receive`. Acknowledged operations wait
  for their complete QoS exchange. Connection failures release waiters.
- The unused connection-pool dependency and unimplemented pooled/TLS paths
  are removed. `mqttz.tls` provides verified TLS independently.
- The former Cmdliner/XDG/pool configuration layer becomes `mqttz.config`.
  It accepts explicit strict TOML settings without implicit discovery.

The new core has no external libraries. Eio uses a direct Unix bytes path for
TCP and the existing Bytesrw flow adapter for TLS or other generic flows.
The TOML subset is recorded in [the vendor manifest](../../vendor/upstreams.json).

## Refresh

Compare upstream changes against the recorded monorepo commit before applying
them. Preserve binary-field distinctions, property contexts, buffer ownership,
local allocation contracts and failure cleanup. Run the codec and fault suites,
Docker interoperability harness and TOML differential from the README. Update
this record only when incorporating a reviewed source snapshot.
