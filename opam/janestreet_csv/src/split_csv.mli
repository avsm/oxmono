open! Core

type t

val kv_maps : t -> (string String.Map.t * string String.Map.t) list
val header_map : t -> string Int.Map.t
val create : (string String.Map.t * string String.Map.t) list -> string Int.Map.t -> t
val of_mapped_csv : Mapped_csv.t -> key_spec:Key_specifier.t -> t
val to_mapped_csv : t -> Mapped_csv.t
val by_key : ?header:bool -> key:string -> Csvlib.Csv.t list -> t list
val by_key_from_files : ?header:bool -> key:string -> string list -> t list
