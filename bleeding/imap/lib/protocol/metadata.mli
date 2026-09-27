(** RFC 5464 GETMETADATA options. *)

type depth =
  | Zero  (** The named entries only. *)
  | One  (** The named entries and their immediate children. *)
  | Infinity  (** The named entries and all their descendants. *)

val depth_to_wire : depth -> string
(** [depth_to_wire d] is the DEPTH value of [d], one of [0], [1] or
    [infinity]. *)
