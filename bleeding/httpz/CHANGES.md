# Changes

## Unreleased

- Reject extra response Upgrade protocols, answer pings while closing, and
  acknowledge client close code 1010. Stop resumed I/O after connection failure.

- Add `httpz.websocket` with version-13 handshakes, bounded byte messages and
  allocation-checked framing. Verify server upgrades through Proffer over TCP.
