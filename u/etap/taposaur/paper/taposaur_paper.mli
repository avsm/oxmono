(** Pure paper metadata resolution. This library has no I/O, parser,
    database or HTML dependency. *)

(** {1 Evidence and field selection} *)

type source =
  | Correction of Taposaur.Manifest.Correction.t
  | Manifest
  | Index
  | Artifact of Taposaur.Manifest.Artifact.t
  | Mirror of Taposaur.Publisher_evidence.t
  | External of Taposaur.Abstract_evidence.t
      (** An abstract from an external metadata service, used only when
          no archive document or index row supplies one. *)

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
  text : bool option;
      (** Whether the document body holds prose, when its reader can
          tell. TEI readers report it; others leave it unknown. *)
}

type t = private {
  publisher_evidence : Taposaur.Publisher_evidence.t option;
  abstract_evidence : Taposaur.Abstract_evidence.t option;
  manifest : Taposaur.Manifest.t;
  index : Taposaur.Index.t option;
  metadata : metadata;
  documents : document list;
  preferred_tei : Taposaur.Manifest.Artifact.t option;
  preferred_pdf : Taposaur.Manifest.Artifact.t option;
  text : bool option;
      (** [Some true] when any document holds prose, [Some false] when
          every document that could tell holds none, [None] otherwise. *)
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
    or [None] when every header failed. [preferred_pdf]
    is the first PDF in manifest order. *)

(** {1 Catalogue projection} *)

val version : int
(** [version] identifies the resolver rules for invalidating derived
    caches. Raise it whenever a document reader or the selection rules
    change, so every catalogue row is resolved again and search, exports
    and snapshots see what the readers now see. Version 2 derives
    abstracts from TEI bodies and records body text. *)

val bibliography : t -> Taposaur.Bibliography.t
(** [bibliography paper] returns its selected values without provenance.
    It does not alter the manifest or the original index record. *)

val presence : t -> Taposaur.Bibliography.presence
(** [presence paper] reports selected title, abstract and publisher
    availability. *)

val cache_string : t -> (string, string) result
(** [cache_string paper] encodes its manifest and document observations.
    The original index row is stored separately by the catalogue. *)
val of_cache_string :
  index:Taposaur.Index.t -> string -> (t, string) result
(** [of_cache_string ~index json] reconstructs complete resolved details
    without I/O. It checks the cache version, identity and artifact references.
    Source freshness is checked separately by the indexing workflow. *)

val with_publisher : Taposaur.Publisher_evidence.t -> t -> t
(** Fill a missing publisher from exact-DOI evidence. Existing sources win. *)
val with_abstract : Taposaur.Abstract_evidence.t -> t -> t
(** Fill a missing abstract from exact-DOI evidence. Existing sources win,
    so an abstract read from a document or index row is never replaced.
    The evidence is cached with the paper and reapplied by
    {!of_cache_string}. *)
