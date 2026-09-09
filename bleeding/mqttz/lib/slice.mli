(** Views into byte buffers. Views share storage and do not copy it.
    Do not mutate backing bytes while a decoder or writer is using them. *)

type t = private { global_ bytes : bytes; off : int; len : int }
val make : ?off:int -> ?len:int -> bytes -> t
val make_local : bytes -> off:int -> len:int -> t @ local [@@zero_alloc]
(** [make_local bytes ~off ~len] is a bounds-checked stack view. *)

val of_string : string -> t
(** [of_string s] copies [s] into a new byte buffer. *)

val empty : t
val sub : t -> int -> int -> t
val sub_local : t @ local -> int -> int -> t @ local
val length : t @ local -> int [@@zero_alloc]
val get_uint8 : t @ local -> int -> int [@@zero_alloc]
val to_string : t @ local -> string
val copy : t @ local -> t
(** [copy s] returns a view owning a fresh copy of [s]. *)
