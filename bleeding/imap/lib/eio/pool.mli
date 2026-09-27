(** A bounded Eio pool of authenticated IMAP connections.

    Connections belong to the creation switch. Closed or failed connections
    are replaced on the next checkout. Callers should dedicate a connection
    outside this pool for long IDLE waits when ordinary work must continue. *)

type t

val create :
  sw:Eio.Switch.t -> max_connections:int ->
  connect:(sw:Eio.Switch.t -> (Client.t, Error.t) result) -> t
(** [connect] is called lazily, at most [max_connections] live clients at a
    time. A failed connection attempt returns its IMAP error from [use] and
    does not consume capacity. The pool closes its clients with [sw]. *)

val max_connections : t -> int

val use : t -> (Client.t -> ('a, Error.t) result) -> ('a, Error.t) result
(** Borrow one client for a bounded operation. A protocol, transport or
    uncertain-outcome error closes the borrowed client before returning it to
    the pool; a tagged rejection leaves it reusable. Exceptions and
    cancellation close the client and propagate. Never retain [Client.t]
    beyond the callback. Once the pool's switch is released, [use] returns
    [Error.Closed], including for a caller already waiting for a client. *)
