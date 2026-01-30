open! Core

type t

val row_maps : t -> string String.Map.t list
val header_map : t -> string Int.Map.t
val create : string String.Map.t list -> string Int.Map.t -> t
val of_csv : Csvlib.Csv.t -> t
val to_csv : t -> Csvlib.Csv.t
