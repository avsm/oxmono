open! Core

module Insertion : sig
  type t =
    { placement_cnum : int
    ; value : string
    }

  val apply_all : original:string -> t list -> string
end

type t

val create : filename_rel_to_project_root:string -> unit
val add_insertion : filename_rel_to_project_root:string -> Insertion.t -> unit
val make_corrected_file : filename_rel_to_project_root:string -> unit
