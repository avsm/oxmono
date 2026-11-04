(** A paper's searchable bibliography and file availability.
    This read model carries no archive paths or resolver policy. *)

type holdings = {
  pdf : bool; xml : bool; tei : bool; s2orc : bool;
  text : bool option;
      (** Whether a converted document holds body prose, once the
          catalogue has resolved the paper's documents; [None] before
          that or when no reader could tell. *)
}
type t = {
  uid : Uid.t;
  bibliography : Bibliography.t;
  holdings : holdings;
}

val of_index : Index.t -> t
(** [of_index index] describes the metadata and holdings in an imported
    row. Body text is unknown there. *)
val full_text : holdings -> bool
(** [full_text h] is whether the paper's text is held: a PDF or S2ORC
    document, or a converted document with body prose. Before the body
    is known, any publisher XML or TEI counts. *)
val jsont : t Jsont.t
val of_string : string -> (t, string) result
val to_string : t -> (string, string) result
(** [to_string summary] encodes the public search result as JSON. *)
