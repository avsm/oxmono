(** Bounded WS/WSS client connections using HTTPz and Eio. *)
val with_connection
  :  ?tls:Httpz_tls.client
  -> ?max_message:int
  -> < net : _ Eio.Net.t
     ; clock : _ Eio.Time.clock
     ; mono_clock : _ Eio.Time.Mono.t
     ; secure_random : _ Eio.Flow.source
     ; .. >
  -> string
  -> (Httpz_websocket.t -> 'a)
  -> 'a
(** [with_connection env url f] connects, verifies the HTTP upgrade and calls
    [f socket]. WS and WSS URLs must have a host and no userinfo or fragment.
    TLS defaults to system trust. Redirects, compression and subprotocols are
    not accepted. Buffered bytes after the upgrade are retained. Connection
    and handshake take at most 15 seconds, reads 120 seconds and writes 10
    seconds. A ping is sent every 30 seconds. Returning or cancelling [f]
    closes the connection. Frame payloads use the borrowed-byte WebSocket API.
    The Eio byte adapter copies through its buffered string reads. *)
