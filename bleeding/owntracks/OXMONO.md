# Import provenance

Ported `../monopampam/ocaml-owntracks` from monorepo commit
`a59a113719c994e5e95479cc3964b2ed7ccc68a5` on 2026-09-09. Upstream identifies
<https://tangled.org/anil.recoil.org/ocaml-owntracks>. The ISC license and
copyright notices are retained.

The source already embeds a GeoJSON codec derived from the Jsont programmers'
example. The port retains its Point/LineString feature and collection export
behaviour in `Owntracks.Geojson`. Unused geometry parsers, nested aliases and
the redundant external `geojson` dependency are removed. No additional vendor
package is needed. JSON uses the workspace's `jsont` and `jsont.bytesrw`.
TOML uses the previously vendored `ocaml-codec` subset.

## Interfaces

- `Owntracks.Message.decode` and `Owntracks.Mqtt.of_mqtt` accept mqttz byte
  slices. `Message.of_string` is the explicit string convenience interface.
  The old duplicate MQTT message record is removed.
- Known message codecs share one body and check their `_type` discriminator.
  Unsupported message types return errors. The unusable `Unknown` constructor
  is removed. `Waypoints` contains a list of waypoint messages.
- Waypoint geographic coordinates and radius are optional. Beacon fields are
  exposed. Transition coordinates are optional.
- `Owntracks.Geojson` exposes the OwnTracks export subset directly. Encoding
  returns a result. A LineString requires two locations from one MQTT topic.
- `Owntracks_eio` works on an existing mqttz connection. Callback clients,
  connection pools, polling loops and nested `Eio_main.run` calls are removed.
- Recorder I/O is separate from pure response codecs and accepts a Fetch
  client. Failures are explicit. Query parameters are encoded.
- The CLI configuration composes `Mqttz_config.codec`. The old `[pool]` table,
  insecure TLS flags and HTTP debug-logging flag are removed. Unknown options
  and keys are errors. Device aliases and the five original commands remain.

## Refresh

Compare source changes against the recorded revision. Preserve borrowed input
lifetimes, standalone type-tag checks, optional beacon coordinates, query
encoding and separation of device tracks. Update tests for any supported
message fields before refreshing codecs. Run the scoped build, forced tests,
format alias and Docker harness from the README.
