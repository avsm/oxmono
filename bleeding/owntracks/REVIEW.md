# Review

Reviewed on 2026-09-09 against the
[OwnTracks JSON format](https://owntracks.org/booklet/tech/json/), the imported
Recorder client and [RFC 7946](https://www.rfc-editor.org/rfc/rfc7946.html).

| Finding | Change and evidence |
|---|---|
| MQTT payloads were copied and edited to insert a topic. Embedded topics could override the transport. | Decode borrowed bytes, then set the location topic. Tests cover whitespace, escaped topic names, offsets, unchanged input and result ownership. |
| Standalone codecs ignored their type tag. `waypoints` decoded as one waypoint. | Shared bodies with checked discriminators and a real `Waypoints` list. Round trips cover all six supported message types. |
| Waypoints and transitions required geographic coordinates. | Accept omitted coordinates, including beacon-only waypoints. Preserve beacon identifiers and region ID. |
| General JSON number codecs admitted null, nonfinite values and truncated integers. | Require finite numbers and exact JSON integers. Tests reject null coordinates, overflow, fractional timestamps and wrong tags. |
| Live tracks combined device locations and emitted short LineStrings. | Group by complete MQTT topic, sort timestamps, emit Points for singleton groups. Tests include identical device IDs under different users. |
| Recorder errors appeared as empty results and bodies were copied into strings. | Stream through a bounded reader, select response shape once, preserve errors and scope authentication. Unit and HTTP fixture tests cover query escaping, response shapes, Basic auth, failures and limits. |
| Configuration errors were ignored and help loaded configuration. | Explicit loading after command parsing, strict TOML composition, CLI overrides and private initial file permissions. CLI fixtures cover these behaviours. |

## Allocation and cleanup

The mqttz payload reaches Jsont without a whole-payload copy or generic JSON
tree. Returned strings and records allocate and outlive the input buffer.
The compiler accepts a local MQTT slice at the decoder boundary. Topic level
scanning uses `let mutable` and passes `[@zero_alloc]` checking, allocating
only the returned user/device substrings and their wrappers. GeoJSON output
uses typed records instead of constructing generic JSON property trees. Message
encoding uses 1 KiB scratch space rather than the streaming writer's 64 KiB
default. One hundred small location encodes allocate 180,080 bytes in the
release-check build. A heap-allocation guard and a multi-flush card round trip
cover this.

Duplicate codec bodies, unused generic geometry code, dead `Unknown` encoding,
MQTT wrapper records, pool configuration and repeated runtime entry points are
removed. mqttz's accompanying review removes duplicate PUBLISH preparation,
uses bytes for final encoded headers, removes an unused parser type and uses
unboxed mutable CONNECT flag accumulators. Pure request preparation happens
before registering an exchange. Oversized publication, subscription and
unsubscribe requests leave the connection usable. Configuration now validates
the full version-specific CONNECT packet before opening a socket.

## Verification

The OxCaml `release-check` build, scoped formatting aliases and all 31 forced
unit cases across mqttz and OwnTracks pass. The local slice tests compile
against the public interface. The Docker harness passes for both MQTT versions,
password/TLS checks, OwnTracks QoS delivery and the CLI/HTTP fixtures. See the
README for reproducible commands.

## Limits

Supported messages are location, transition, waypoint, waypoints, card and
last will. Extra fields are ignored. Configuration commands, encrypted
payloads, beacons as ranging reports, status and steps are unsupported.
Historical permissiveness is retained for missing card tracker IDs and
transition accuracy/waypoint timestamps. Application enum values and geographic
ranges are preserved rather than validated. The number codec rejects values
outside JSON's exact integer range.

The GeoJSON interface is an OwnTracks export schema, not a general GeoJSON
library. It supports Point/LineString features and FeatureCollection. It does
not split tracks crossing the antimeridian. MQTT reconnect and persistence
remain subject to mqttz's documented clean-session limits. The CLI reports a
connection failure and exits. Long Recorder histories materialise decoded
location records within the response byte limit.
