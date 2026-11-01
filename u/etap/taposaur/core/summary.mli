(** A paper's searchable bibliography and file availability.
    This read model carries no archive paths or resolver policy. *)

type holdings = { pdf : bool; xml : bool; tei : bool; s2orc : bool }
type t = {
  uid : Uid.t;
  bibliography : Bibliography.t;
  holdings : holdings;
}

val of_index : Index.t -> t
(** [of_index index] describes the metadata and holdings in an imported row. *)
val jsont : t Jsont.t
val of_string : string -> (t, string) result
val to_string : t -> (string, string) result
(** [to_string summary] encodes the public search result as JSON. *)
