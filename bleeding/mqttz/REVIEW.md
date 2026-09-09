# Protocol review and validation

Reviewed on 2026-09-09 against
[MQTT 3.1.1 plus Errata 01](https://docs.oasis-open.org/mqtt/mqtt/v3.1.1/errata01/os/mqtt-v3.1.1-errata01-os-complete.html),
[MQTT 5.0](https://docs.oasis-open.org/mqtt/mqtt/v5.0/os/mqtt-v5.0-os.html)
and [RFC 3629](https://www.rfc-editor.org/rfc/rfc3629.html).
Exact reference copies and hashes are in [spec/](spec/README.md).

## Findings addressed

| Area | Finding in the imported implementation | Port behavior and evidence |
| --- | --- | --- |
| Framing, §2.2 | Remaining-length arithmetic and the four-byte limit were incorrect. Reserved flags and trailing bytes were not consistently rejected. | Bounded exact-frame decoding, minimal varints, early size checks, fixed-header validation and mandatory body exhaustion. Malformed and incremental-frame tests. |
| Strings, §1.5 | UTF-8 validation was absent. Passwords, Will payloads and several binary properties used string readers. | RFC 3629 validation plus MQTT's NUL prohibition for strings. Binary fields remain arbitrary bytes. Binary round trips and malformed UTF-8 tests. |
| Packet identifiers, §2.3 / §2.2 | Zero identifiers and inconsistent PUBLISH QoS/identifier combinations could pass. | Nonzero identifier checks, QoS validation and exact packet lengths. Malformed-wire tests. |
| MQTT 5 properties, §2.2.2 | Context, singleton, boolean, nonzero and range checks were missing. | Named property/context rules and stack presence bitmaps. Repeated User Properties and PUBLISH Subscription Identifiers remain legal. Property rejection and round-trip tests. |
| MQTT 5 acknowledgements, §§3.2–3.15 | Packet-specific reason codes were not checked. CONNACK could omit Property Length. Zero-length AUTH was rejected. | Named reason-code cases, mandatory CONNACK properties, valid abbreviated ACK/DISCONNECT/AUTH encodings. Exact wire tests. |
| Topics, §4.7 | Root wildcards incorrectly matched `$` topics. Matching allocated split strings and lists. | Allocation-free topic scans, empty-level matching and shared-subscription validation. Topic regressions and compiler contracts. |
| QoS 2, §4.3.3 | Duplicate PUBLISH packets were delivered repeatedly. | Deliver once per inbound identifier, resend PUBREC for duplicates, finish PUBREL/PUBCOMP, handle repeated PUBREL. Scripted duplicate-exchange test. |
| Client acknowledgement handling | Negative acknowledgements were ignored and unsubscribe returned before UNSUBACK. | Check response type, identifier, count and reason. Wait for each full exchange. Negative PUBACK/SUBACK tests and broker unsubscribe tests. |
| Connection lifecycle | Reader failure could strand promises and the writer queue. DISCONNECT could be lost before shutdown. | Serialized writes, connection-ending promise, bounded deadlines and cleanup. EOF, cancellation, timeout and flushed-DISCONNECT tests. |
| Negotiation and keep-alive | Broker capabilities and missing ping responses were ignored. | Honor broker packet size, maximum QoS, retain, wildcard/shared availability and Server Keep Alive. One outbound acknowledged exchange respects Receive Maximum. Negotiated-limit and idle-broker tests. |
| TLS | The transport entry point was a stub. | Verified DNS/IP names and system or caller-supplied trust anchors. Docker tests for trusted TLS and rejected untrusted certificates. |
| Payload copies | Full packet bodies and PUBLISH payloads passed through intermediate buffers and strings. | Borrowed `bytes` slices and separate output header/payload views. Physical buffer-identity checks and allocation measurements. |

The client advertises no topic aliases and rejects unsolicited aliases. Its
clean session and lack of enhanced authentication are explicit interface
choices. The codecs validate packet-local rules. They cannot validate rules
that depend on an earlier connection or on which peer sent a packet.

## Verification results

The `5.2.0+ox` switch with `--profile release-check` passed the scoped build
and 19 test cases: nine codec cases, eight client-failure cases and two TOML
configuration cases. The codec suite includes 10,000 deterministic mutations,
all control packet kinds, all QoS levels and payload sizes crossing remaining
length boundaries. Client tests use real local sockets and bounded deadlines.

One hundred decodes allocated 16,080 bytes for both a 16-byte payload and a
1 MiB payload. This measures the decoder after the input frame exists. It
does not include receiving or allocating that frame. Local scalar readers,
framing, variable integers, UTF-8 scans and topic matching pass compiler
zero-allocation checks. Returned metadata and packet records still allocate.

The [Docker harness](test/docker/run.sh) passed with digest-pinned Mosquitto
2.0.22. Both MQTT versions passed QoS 0/1/2, binary data, retained publication,
unsubscribe, a 1 MiB message, idle keep-alive and Will delivery. MQTT 5 tests
also checked binary correlation data. Password acceptance/refusal and TLS
DNS/IP verification passed. Independent `mosquitto_pub` and `mosquitto_sub`
clients checked both directions. Harness resources were removed after testing.

The TOML port matched pristine `ocaml-codec` in 19 differential cases under
stock OCaml 5.5 and OxCaml 5.2. The only source patch replaces two uses of
`Result.Syntax` with equivalent local operators.

The scoped Dune formatting alias passes. Dune disables OCaml formatting in
this tree without an `.ocamlformat` file, and the installed stock formatter
cannot parse OxCaml modes. Standard-syntax client/configuration/test files
were formatted explicitly. OxCaml syntax was checked by the compiler and
reviewed manually.

## Limits

This is a reviewed client and packet codec, not an exhaustive conformance
certification or a broker. Persistent-session recovery, automatic reconnection,
durable QoS state, enhanced AUTH, ACE-MQTT and WebSocket transport are absent.
QoS 2 guarantees apply within the active clean session. The bounded queue
disconnects a consumer that falls behind instead of retaining unbounded data.

The default packet limit is 16 MiB. Applications may raise it within MQTT's
wire limit. Non-minimal variable integers are rejected in both protocol
versions. The code is intended for the repository's OxCaml compiler and
vendored dependency interfaces, rather than stock OCaml package installation.
