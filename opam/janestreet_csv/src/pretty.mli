open! Core
open Csvlib

type t =
  { header_lines : string list
  ; row_lines : string list
  }

val prettify : space:int -> suppress_header:bool -> Csv.t -> (t, string) Result.t
val lines : t -> string list
val print : t -> unit
