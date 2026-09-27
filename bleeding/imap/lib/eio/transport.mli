(** IMAP endpoints and their owned byte flows, documented in
    [Imap_eio.Transport]. *)

type tls = [ `Implicit | `Required_starttls | `Plain ]
type t

val v :
  net:_ Eio.Net.t ->
  host:string ->
  ?port:int ->
  ?tls:tls ->
  ?authenticator:X509.Authenticator.t @ portable ->
  unit -> t

val host : t -> string
val port : t -> int
val tls : t -> tls

type flow

val read : flow -> Cstruct.t -> int
val write : flow -> Cstruct.t list -> unit

val close : flow -> unit
(** [close flow] closes the owned resource once, protected from
    cancellation. *)

val compressed : flow -> bool

val compress_deflate : flow -> unit
(** [compress_deflate flow] wraps the current layers in DEFLATE for the rest
    of the connection, and a later call to it or to {!upgrade} raises
    [Invalid_argument]. *)

val of_flow :
  [> Eio.Flow.two_way_ty | Eio.Resource.close_ty ] Eio.Resource.t -> flow
val connect : sw:Eio.Switch.t -> t -> flow

val upgrade : t -> flow -> unit
(** [upgrade endpoint flow] installs TLS on [flow], closing the resource and
    re-raising if the handshake fails. *)
