(** Manifest projections. Event logs remain authoritative. *)

type id = Uid.t

(** {1 Artifact vocabulary and records} *)

module Source : sig
  type t = Publisher | Oa | Core | Url | Tei | S2orc

  val jsont : t Jsont.t @@ portable
  val to_string : t -> string
  val of_string : string -> (t, string) result
end

module Format : sig
  type t = Xml | Pdf | Tei | Json

  val jsont : t Jsont.t @@ portable
  val to_string : t -> string
  val of_string : string -> (t, string) result
end

module Version : sig
  type t = Published | Accepted | Preprint | Unknown

  val jsont : t Jsont.t @@ portable
  val to_string : t -> string
  val of_string : string -> (t, string) result
end

module Confidence : sig
  type t = Observed | Assumed

  val jsont : t Jsont.t @@ portable
  val to_string : t -> string
  val of_string : string -> (t, string) result
end

module Route : sig
  type t = Pub2tei | Grobid | S2orc

  (* Pub2TEI reads the article's own markup where GROBID reads a
     rendered page, so it leads wherever a paper offers both. The
     s2orc route rewrites Semantic Scholar's parse of a page we may
     not hold ourselves, which is why it ranks after both of ours. *)
  val jsont : t Jsont.t @@ portable
  val to_string : t -> string
  val of_string : string -> (t, string) result
end

module Artifact : sig
  type t = {
    file : string;
    format : Format.t;
    source : Source.t;
    version : Version.t option;
    converter : string option;
    from : string option;
    strategy : string option;
    status : int option;
    bytes : int64;
    sha256 : string option;
    at : string;
    url : string option;
    origin : string option;
    confidence : Confidence.t option;
  }

  val jsont : t Jsont.t @@ portable
end

(** {1 Human corrections} *)

module Field : sig
  type t = Title | Authors | Journal | Publisher | Year | Abstract

  val jsont : t Jsont.t @@ portable
  val to_string : t -> string
  val of_string : string -> (t, string) result
end

module Correction : sig
  type t = {
    field : Field.t;
    value : string;
    by : string;
    at : string;
    note : string option;
  }

  val jsont : t Jsont.t @@ portable
end

(** {1 Conversion failures} *)

module Failure : sig
  type t = {
    route : Route.t;
    at : string;  (** When the last attempt failed. *)
    attempts : int;  (** How many have failed, over the whole log. *)
    reason : string;  (** What the converter said, verbatim. *)
    permanent : bool;
        (** Whether the converter was refusing this document rather than
            failing at its own end. A permanent refusal is not asked
            again by an ordinary sweep, because asking spends the
            converter to be told the same thing; only a converter new
            enough to answer differently, named by [--retry-failed
            --since], gets another go. Absent in a manifest written
            before the distinction existed, which reads as false, so
            those are re-asked once and classified properly. *)
  }

  val jsont : t Jsont.t @@ portable
end

(** {1 Paper manifests} *)


type t = {
  v : int;
  id : id;
  doi : string option;
  pmcid : string option;
  pmid : string option;
  title : string option;
  publisher : string option;
  year : int option;
  corrections : Correction.t list;
  artifacts : Artifact.t list;
  conversions_failed : Failure.t list;
  updated : string;
}

val jsont : t Jsont.t @@ portable
val of_string : string -> (t, string) result
(** [of_string s] decodes a version 1 manifest. Optional arrays default to
    empty. A missing failure [permanent] defaults to false. Unknown
    members are ignored. Timestamps are retained verbatim. No record is
    written or reconstructed. *)

val to_string : t -> (string, string) result
(** [to_string t] encodes a manifest as JSON without writing a file. *)

val correction : t -> Field.t -> string option
(** [correction t field] is the last correction for [field]. *)
