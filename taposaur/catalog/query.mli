(** Search predicates, independent of database handles. *)
type holdings = Any | Pdf | Xml | Tei | S2orc | Unconverted
type metadata_field = Title | Abstract | Publisher
type availability = Present | Missing
type t = {
  text : string; scheme : Taposaur.Uid.Scheme.t option; publisher : string;
  journal : string; year_from : int option; year_to : int option;
  holdings : holdings; metadata : (metadata_field * availability) list;
  after : string option; limit : int;
}
val make : ?text:string -> unit -> t
(** [make ()] selects the first 25 papers with no filters. *)
