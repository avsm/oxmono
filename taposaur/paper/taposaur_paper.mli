(** Pure paper metadata resolution. This library has no I/O, parser,
    database or HTML dependency. *)

(** {1 Evidence and field selection} *)

type source =
  | Correction of Taposaur.Manifest.Correction.t
  | Manifest
  | Index
  | Artifact of Taposaur.Manifest.Artifact.t

type 'a evidence = { value : 'a; source : source }

module Field : sig
  type 'a t = private {
    selected : 'a evidence option;
    alternatives : 'a evidence list;
  }
  val value : 'a t -> 'a option
  (** [value field] returns its selected value without provenance. *)
  val candidates : 'a t -> 'a evidence list
  (** [candidates field] returns the selected evidence followed by every
      alternative, including agreement from independent sources. *)
end

val source_name : source -> string
(** [source_name source] labels a correction, projection or artifact. *)

type metadata = private {
  title : string Field.t;
  authors : Taposaur.Author.t list Field.t;
  journal : string Field.t;
  publisher : string Field.t;
  published : string Field.t;
  year : int Field.t;
  abstract : string Field.t;
  volume : string Field.t;
  issue : string Field.t;
  pages : string Field.t;
  issn : string Field.t;
  doi : string Field.t;
  pmcid : string Field.t;
  pmid : string Field.t;
}

(** {1 Document observations and resolved papers} *)

type document = {
  artifact : Taposaur.Manifest.Artifact.t;
  bibliography : (Taposaur.Bibliography.t, string) result;
}

type t = private {
  manifest : Taposaur.Manifest.t;
  index : Taposaur.Index.t option;
  metadata : metadata;
  documents : document list;
  preferred_tei : Taposaur.Manifest.Artifact.t option;
  preferred_pdf : Taposaur.Manifest.Artifact.t option;
}

val resolve :
  ?index:Taposaur.Index.t ->
  Taposaur.Manifest.t -> document list -> t
(** [resolve manifest documents] selects each field independently. Newest
    explicit corrections outrank manifest values, publisher XML, Pub2TEI,
    GROBID, S2ORC, other documents and the index, in that order. Within a
    source class, published versions precede accepted versions, preprints
    and unknown versions. Ties retain manifest order. Blank values are
    absent. The raw inputs, disagreements and parse errors remain in [t].
    Identifiers never change the manifest UID. An index with another UID
    is rejected, as is a document not listed in the manifest. A derived
    year retains its date's source.

    [preferred_tei] selects the highest-ranked successfully parsed TEI,
    or the highest-ranked TEI when every header failed. [preferred_pdf]
    is the first PDF in manifest order. *)

(** {1 Catalogue projection} *)

val version : int
(** [version] identifies the resolver rules for invalidating derived caches. *)

val bibliography : t -> Taposaur.Bibliography.t
(** [bibliography paper] returns its selected values without provenance.
    It does not alter the manifest or the original index record. *)

val presence : t -> Taposaur.Bibliography.presence
(** [presence paper] reports selected title, abstract and publisher
    availability. *)
