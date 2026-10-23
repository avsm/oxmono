(** Bibliographic values, independent of storage and source format.
    Empty records represent missing metadata, not missing papers. *)

type t = {
  title : string option;
  authors : Author.t list;
  journal : string option;
  publisher : string option;
  published : string option;
  year : int option;
  abstract : string option;
  volume : string option;
  issue : string option;
  pages : string option;
  issn : string option;
  doi : string option;
  pmcid : string option;
  pmid : string option;
}

val empty : t
(** [empty] has no bibliographic values. *)
val of_index : Index.t -> t
(** [of_index index] extracts the metadata recorded in a corpus index row. *)

type presence = { title : bool; abstract : bool; publisher : bool }
val presence : t -> presence
(** [presence bibliography] reports nonblank title, abstract and publisher
    availability. *)

val jsont : t Jsont.t
val of_string : string -> (t, string) result
val to_string : t -> (string, string) result
(** [of_string json] decodes bibliographic values without interpreting
    their sources or performing I/O. *)
