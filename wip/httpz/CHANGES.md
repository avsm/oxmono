# Changes

- Clarify opam descriptions of package functionality and interfaces.

## Unreleased

- `Httpz_websocket.send` accepts a message of any length its payload holds.
  `max_message` bounds received messages only, as documented, so a server
  that limits what clients may send can still deliver longer frames.

- `Range.generate_boundary` draws each character through a `random` closure
  the caller passes instead of a generator of its own, so the caller decides
  where boundaries come from and no state is shared between domains.

- Install the media interface needed by its public re-export, preserving
  nested modules and portability guarantees for standalone consumers.

- Allow JSON and JSON Lines codec construction inside portable handlers.

- Report WebSocket read activity and expose rejected upgrade status codes.

- Add an Eio WS/WSS client with verified upgrades, TLS, bounded I/O and
  cancellation. Preserve frames buffered after the HTTP upgrade.

- Reject extra response Upgrade protocols, answer pings while closing, and
  acknowledge client close code 1010. Stop resumed I/O after connection failure.

- Add `httpz.websocket` with version-13 handshakes, bounded byte messages and
  allocation-checked framing. Verify server upgrades through Proffer over TCP.
