(** An RFC 4978 raw DEFLATE flow with one continuous stream per direction,
    whose operations are serialized per direction. *)

type t

type Eio.Exn.err += Deflate of string
(** [Deflate message] reports a malformed or final block, more than 16 MiB of
    compressed input without decoded output, or use of a closed flow. *)

val create :
  [> Eio.Flow.two_way_ty | Eio.Resource.close_ty] Eio.Resource.t -> t
(** [create flow] takes ownership of [flow] at an exact compressed-stream
    boundary. *)

val read : t -> Cstruct.t -> int
val write : t -> Cstruct.t list -> unit

val close : t -> unit
(** [close t] closes the original resource once, as does a codec or I/O
    failure or a cancelled operation, and an operation running in another
    fiber then fails. *)
