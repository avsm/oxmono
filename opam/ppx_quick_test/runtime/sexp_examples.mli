open! Core

module One : sig
  type t =
    { sexp_string : string (** Evaluated string expression *)
    ; body : string (** Syntactic string (including escape characters) *)
    ; body_location : Location.t
    (** Location of syntactic string, not including delimiters *)
    ; delimiters : string option
    (** [None] for ["asdf"], [Some "xxx"] for [{xxx|asdf|xxx}] *)
    }
end

type t =
  | Ignore
  | Empty of Location.t (** The location of the [[@sexp_examples]] attribute *)
  | List of One.t Nonempty_list.t

val get_parsed_examples : t -> ('tup, _, _) Params.t -> 'tup list Or_error.t
val insertion_for_new_example : t -> Sexp.t -> File_corrections.Insertion.t option

module For_testing : sig
  val string_to_ocaml_string : string -> string
end
