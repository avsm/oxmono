# mqttz

MQTT 3.1.1 and 5.0 packet codecs and an Eio client for OxCaml. The core uses
OCaml `bytes` and has no library dependencies. The port derives from `mqtte`.
[OXMONO.md](OXMONO.md) records the import and interface changes.

| Library | Interface | Purpose |
| --- | --- | --- |
| `mqttz` | [Mqttz](lib/mqttz.mli) | Packet types, bounded decoding, encoding and topic matching |
| `mqttz.eio` | [Mqttz_eio](eio/mqttz_eio.mli) | TCP client with QoS 0, 1 and 2 |
| `mqttz.tls` | [Mqttz_tls](tls/mqttz_tls.mli) | Certificate-verified TLS connections |
| `mqttz.config` | [Mqttz_config](config/mqttz_config.mli) | Strict TOML connection settings |

## Buffer ownership

`Slice.make bytes` borrows its buffer. `Packet.decode slice` requires exactly
one complete frame and returns PUBLISH payloads that share those bytes.
The caller must keep them unchanged while using the decoded packet.
`Slice.copy payload` makes an independent copy. `Slice.of_string` also copies.

`Packet.encode packet` returns header and payload slices. The PUBLISH payload
shares the caller's bytes. It is never concatenated into a packet buffer.
`Packet.to_bytes` is the explicit copying convenience function.

`Mqttz_eio.publish` borrows the payload until it returns. `receive` returns a
view into a fresh received frame that remains valid after subsequent reads.
The TCP path reads directly into OCaml bytes. Generic flows, including TLS,
use Bytesrw adapters and can copy at the flow boundary. Encryption and kernel
I/O are not covered by a zero-copy guarantee. There are no Cstruct references
or direct Cstruct dependencies in mqttz.

Parser cursors, property-presence bitmaps and temporary frame views use local
allocation. `release-check` verifies zero heap allocation for scalar byte
readers, variable integers, framing, UTF-8 validation and topic matching.
Returned packet records, metadata strings and retained payload views allocate.

## Client

```ocaml
let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let config = Mqttz_eio.default_config ~client_id:"example" in
  let client = Mqttz_eio.connect ~sw
    ~net:(Eio.Stdenv.net env) ~clock:(Eio.Stdenv.mono_clock env)
    ~config ~host:"127.0.0.1" ~port:1883 () in
  Mqttz_eio.subscribe ~qos:`Exactly_once client ["sensors/+"];
  let payload = Mqttz.Slice.make (Bytes.of_string "22.5") in
  Mqttz_eio.publish ~qos:`Exactly_once client
    ~topic:"sensors/temperature" payload;
  let message = Mqttz_eio.receive client in
  Printf.printf "%s: %s\n" message.topic
    (Mqttz.Slice.to_string message.payload);
  Mqttz_eio.disconnect client
```

Use `Mqttz_tls.connect` with the same arguments for TLS, usually on port 8883.
It checks DNS names and IP subjectAltNames. An optional authenticator supplies
private trust anchors. The default uses system trust anchors.

Each client implements one clean session. Acknowledged operations are
serialized, respecting even a broker receive maximum of one. Inbound QoS 2
duplicates are suppressed until PUBREL. Negative acknowledgements are raised
as `Rejected`. EOF, malformed packets, interrupted exchanges and missing
acknowledgements close the connection and release waiters. `disconnect` sends
DISCONNECT before closing. `close` closes immediately, allowing a Will.

Call `receive` regularly. The bounded incoming queue defaults to 32 messages.
An overflowing queue closes the connection instead of silently dropping
messages or blocking the acknowledgement reader. Packet size defaults to
16 MiB and an operation deadline defaults to 30 seconds.

## Configuration

Pass TOML text explicitly to `Mqttz_config.of_string`. It neither discovers
files nor reads ambient environment variables. Unknown keys are errors.

```toml
client_id = "sensor-reader"
host = "localhost"
version = "5.0"
tls = false
keep_alive = 60
operation_timeout = 30.0
message_capacity = 32
max_packet_size = 16777216
```

`port` defaults to 1883 or 8883 according to `tls`. `username` and `password`
are optional. The codec is a vendored TOML subset of `ocaml-codec`.

## Build and verification

```sh
opam exec --switch=5.2.0+ox -- dune build --profile release-check @bleeding/mqttz/all
opam exec --switch=5.2.0+ox -- dune runtest --profile release-check --force bleeding/mqttz
bleeding/mqttz/test/docker/run.sh
python3 vendor/ocaml-codec/check-port.py ../ocaml-codec
```

The Docker harness needs Docker Compose, OpenSSL and the build switch. It
starts a digest-pinned Mosquitto 2.0.22 broker with anonymous, authenticated
and TLS listeners, publishes ephemeral ports on loopback, and creates fresh
test credentials and certificates. It removes its containers, network and
temporary files on exit. `MQTTZ_SWITCH` overrides the build switch.

[REVIEW.md](REVIEW.md) maps fixes and tests to the specifications. These tests
are evidence for the implemented behavior, not a complete MQTT certification.
Persistent-session recovery, reconnect/retransmit storage, enhanced AUTH,
ACE-MQTT, outbound topic aliases, WebSockets and broker operation are outside
the client interface. The pure codecs represent all MQTT 5 packet kinds,
including AUTH. TLS buffering remains in the existing Eio TLS stack.
