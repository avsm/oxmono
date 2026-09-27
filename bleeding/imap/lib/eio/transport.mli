(** An IMAP endpoint and its network authority. *)

type tls = [ `Implicit | `Required_starttls | `Plain ]
type t

val v :
  net:_ Eio.Net.t ->
  host:string ->
  ?port:int ->
  ?tls:tls ->
  ?authenticator:X509.Authenticator.t @ portable ->
  unit -> t
(** [Plain] is for an explicitly trusted test server. *)

val host : t -> string
val port : t -> int
val tls : t -> tls

type flow
val read : flow -> Cstruct.t -> int
val write : flow -> Cstruct.t list -> unit
val close : flow -> unit
(** [close flow] closes the owned resource once, with cancellation protected. *)
val compressed : flow -> bool
val compress_deflate : flow -> unit
(** Wrap the current transport, including any TLS layer, after COMPRESS OK.
    Takes ownership of closing the same underlying resource. Compression
    cannot be disabled; STARTTLS upgrades after this call are forbidden. *)

val of_flow : [> Eio.Flow.two_way_ty | Eio.Resource.close_ty ] Eio.Resource.t -> flow
val connect : sw:Eio.Switch.t -> t -> flow
val upgrade : t -> flow -> unit
(** [upgrade endpoint flow] installs TLS on [flow]. Handshake failure closes
    the resource and propagates the original exception. *)
