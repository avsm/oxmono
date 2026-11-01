(** Incremental MQTT framing. *)

exception Malformed of string

val length : ?max_size:int -> Slice.t @ local -> int [@@zero_alloc]
(** [length buffer] is the complete frame size from its fixed header, or
    [0] if that header is incomplete. The body need not yet be present.
    [max_size] bounds the entire packet and defaults to 16 MiB.
    Malformed, overlong or oversized headers raise [Malformed]. *)


val default_max_size : int
