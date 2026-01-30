open Csvlib.Csv

val diff : ?header:bool -> key:string -> t -> t -> t
val diff_from_files : ?header:bool -> key:string -> string -> string -> t
