(** Versioned definitions and frozen values. No I/O or database access. *)

val encode : 'a Jsont.t -> 'a -> string
val decode : 'a Jsont.t -> string -> ('a, string) result
val digest : string -> string
(** [digest bytes] is the lowercase SHA-256 of the exact bytes. *)

module Definition : sig
  type format = Doi_lines | Metadata_jsonl
  type kind = List | Catalogue | Recorded_search
  type holdings = Any | Pdf | Xml | Tei | S2orc | Unconverted
  type field = Title | Abstract | Publisher
  type availability = Present | Missing
  type predicate = { field : field; availability : availability }
  type local = {
    text : string; journal : string; publisher : string;
    year_from : int option; year_to : int option;
    holdings : holdings; metadata : predicate list;
  }
  val empty_local : local
  type source = {
    kind : kind; format : format; blob : string option;
    local : local option; provider : string option; query : string option;
    parameters : Jsont.json option;
  }
  type input = { key : string; label : string; source : source }
  type t = {
    version : int; inputs : input list;
    include_any : string list; require_all : string list;
    exclude_any : string list;
  }
  val validate : t -> (unit, string) result
  (** [validate definition] checks unique keys, references, source shapes,
      supported query semantics and limits. Membership is the union of
      [include_any], intersected with each [require_all] input, minus the
      union of [exclude_any]. Inputs always name DOI sets. *)
  val jsont : t Jsont.t
  val of_string : string -> (t, string) result
  val to_string : t -> string
end

type record = {
  doi : string; bibliography : Taposaur.Bibliography.t; line : int;
}
type diagnostic = { line : int; message : string }
type parsed = { records : record list; diagnostics : diagnostic list }
val parse : Definition.format -> string -> parsed
(** [parse format bytes] preserves duplicate occurrences and reports invalid
    lines. Metadata JSONL rows have a DOI and optional bibliographic fields. *)
val record_jsont : record Jsont.t
val diagnostic_jsont : diagnostic Jsont.t
type supplied = { input : string; record : record }
type member = { doi : string; origins : string list; supplied : supplied list }
val member_jsont : member Jsont.t
val select : Definition.t -> (string * record list) list -> member list
(** [select definition results] deduplicates and sorts membership by DOI.
    Input order controls fallback metadata precedence. *)

type evidence = { field : string; source : string }
type observation = {
  doi : string; bibliography : Taposaur.Bibliography.t;
  holdings : Taposaur.Summary.holdings; in_catalogue : bool;
  origins : string list; evidence : evidence list;
  catalogue_stamp : string option;
  stale : bool; error : string option; diagnostics : string list;
}
val observation_jsont : observation Jsont.t
val freeze : member -> summary:Taposaur.Summary.t option ->
  catalogue_stamp:string option -> stale:bool -> error:string option ->
  diagnostics:string list -> observation
(** [freeze member ~summary ...] prefers nonblank catalogue fields, then
    supplied records in input order. It freezes values and field provenance.
    It does not rerun the paper resolver. *)

type export_format = Dois | Csv | Jsonl
val export_name : export_format -> string
val export_type : export_format -> string
val export : export_format -> observation list -> string
(** [export format rows] sorts by DOI and includes missing metadata.
    CSV contains doi,title,abstract with exact values and CRLF records. *)

module Statistics : sig
  type field = Title | Abstract | Catalogue | Full_text | Pdf | Xml | Tei
    | S2orc | Publisher | Stale | Error
  val fields : field list
  val label : field -> string
  val of_label : string -> field option
  val publisher : observation -> string
  val values : observation -> (field * bool) list
  type coverage = { total : int; present : (string * int) list }
  type t = { overall : coverage; publishers : (string * coverage) list }
  val jsont : t Jsont.t
  val collect : ((observation -> unit) -> unit) -> t
  (** [collect iter] counts frozen metadata and recorded holdings once per
      observation. Formats overlap. An empty publisher denotes unknown. *)
end
