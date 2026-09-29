@@ portable

(** A bounded pool of IMAP clients, documented in [Imap_eio.Pool]. *)

type t

val create :
  sw:Eio.Switch.t -> max_connections:int ->
  connect:(sw:Eio.Switch.t -> (Client.t, Error.t) result) -> t

val max_connections : t -> int
val use : t -> (Client.t -> ('a, Error.t) result) -> ('a, Error.t) result
