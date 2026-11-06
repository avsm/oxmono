(** Snapshot corpora: frozen review rows joined with archive body text.

    A corpus row carries the bibliographic values a snapshot froze for one
    member, the provenance of every field that has one, the holdings
    recorded at capture, and the prose of the paper's preferred
    TEI document read from the archive when the row is exported. Rows are
    flat so that a JSON Lines file converts directly to columnar formats. *)

type provenance = {
  kind : string;
  (** Where a value came from: [grobid], [pub2tei] or [s2orc] for a
      converted document, [publisher_xml] for publisher markup,
      [index] for the original corpus index row, [manifest],
      [correction] (a human correction), [crossref] or [datacite]
      (publisher mirrors), [openalex] (work or abstract evidence),
      [review_input] (a review's own supplied metadata) or [catalogue]
      when the catalogue supplied the value but its own resolution is
      unavailable or has since changed. *)
  provider : string option;
  (** The acquisition strategy or archive source of an artifact, the
      service for external evidence, the review input key, or the
      corrector. *)
  artifact : string option;  (** Archive file, or the input location. *)
  converter : string option;
  version : string option;  (** published, accepted, preprint or unknown. *)
  digest : string option;  (** SHA-256 of the file or retained response. *)
  retrieved : string option;  (** RFC 3339 time of conversion or retrieval. *)
}
val provenance_jsont : provenance Jsont.t

type acquisition = {
  file : string;  (** The PDF, XML or JSON the text was converted from. *)
  format : string;
  source : string;  (** publisher, oa, core, url, tei or s2orc. *)
  strategy : string option;  (** The acquisition strategy, when recorded. *)
  origin : string option;  (** The path or service it was copied from. *)
  url : string option;
  status : int option;  (** HTTP status of the acquisition. *)
  acquired_version : string option;
  confidence : string option;  (** observed or assumed. *)
  acquired_digest : string option;
  acquired_at : string;
}
val acquisition_jsont : acquisition Jsont.t

type text = {
  file : string;  (** The TEI artifact inside the paper directory. *)
  converter : string option;
  sha256 : string;  (** Of the TEI bytes; empty when they could not be read. *)
  bytes : int;
  body : string option;  (** Body prose, or [None] when the body has none. *)
  error : string option;  (** Why no prose could be read. *)
  entry : Taposaur.Manifest.Artifact.t option;  (** The TEI's manifest entry. *)
  acquired : Taposaur.Manifest.Artifact.t option;
  (** The artifact the TEI was converted from: the one its entry names,
      else the first of the format its converter reads. *)
}

type row = {
  snapshot : int;
  doi : string;
  uid : string;
  title : string option;
  abstract : string option;
  authors : string list;  (** Given name then family name. *)
  journal : string option;
  publisher : string option;
  year : int option;
  published : string option;
  volume : string option;
  issue : string option;
  pages : string option;
  issn : string option;
  pmid : string option;
  pmcid : string option;
  title_source : string option;
  abstract_source : string option;
  publisher_source : string option;
  sources : (string * string) list;
  (** Field name to source label for every field with provenance, sorted.
      The three columns above repeat the most used ones. *)
  title_provenance : provenance option;
  abstract_provenance : provenance option;
  authors_provenance : provenance option;
  journal_provenance : provenance option;
  publisher_provenance : provenance option;
  year_provenance : provenance option;
  pmid_provenance : provenance option;
  pmcid_provenance : provenance option;
  journal_openalex_id : string option;  (** OpenAlex source id, such as S30982003. *)
  journal_openalex_name : string option;
  journal_openalex_match : string option;
  (** [work] when the member's own OpenAlex work named the source, [issn]
      when the row's ISSN matched a known venue, [name] when its journal
      name did. *)
  in_catalogue : bool;
  pdf : bool;
  xml : bool;
  tei : bool;
  s2orc : bool;
  tei_file : string option;
  tei_converter : string option;
  tei_sha256 : string option;
  tei_bytes : int option;
  text : string option;
  text_error : string option;
  text_provenance : provenance option;
  (** The TEI the text came from, with its digest as [digest]. *)
  text_acquisition : acquisition option;
  (** How the document behind that TEI entered the archive. *)
}

val row_jsont : row Jsont.t
(** Every member is present; absent values encode as [null]. *)
val encode_row : row -> string

type evidence = {
  abstract : Taposaur.Doi.t -> Taposaur.Abstract_evidence.t option;
  work : Taposaur.Doi.t -> Taposaur.Work_evidence.t option;
  venue_by_issn : string -> Taposaur.Work_evidence.venue option;
  (** Keyed by {!issn_key} form, [NNNN-NNNX]. *)
  venue_by_name : string -> Taposaur.Work_evidence.venue option;
  (** Keyed by {!name_key} form. *)
}
val no_evidence : evidence
val issn_key : string -> string option
(** [issn_key s] is the first ISSN-shaped run in [s] as [NNNN-NNNX], so
    that a hyphenless ISSN or one a converter embedded in other text
    still keys a venue; [None] when there is none. *)
val name_key : string -> string
(** [name_key s] folds a venue name: ASCII lower case, punctuation and
    spacing removed, a leading "the" dropped. *)

val of_observation : snapshot:int -> ?text:text -> ?paper:Taposaur_paper.t ->
  ?evidence:evidence -> Taposaur_review.observation -> row
(** [of_observation ~snapshot o] flattens a frozen observation. Source
    labels come from the observation's field evidence; each provenance
    refines its label: a [catalogue] label becomes the resolver source
    that [paper] (the live resolution) selects for the field when it still
    selects the frozen value, an evidence label gains the retrieval time
    from [evidence] when the digests agree, and an input label names its
    key and location. The OpenAlex venue comes from the member's work
    evidence, else from [evidence] by ISSN, else by journal name. *)

val text_of_paper : ?max_bytes:int -> ?max_output:int ->
  Taposaur_archive.paper -> text option
(** [text_of_paper paper] reads the paper's TEI documents in the resolver's
    preference order (Pub2TEI before GROBID before others, published
    versions first) and returns the first with body prose, otherwise the
    best-ranked one that parsed, otherwise the first failure. [None] when
    the manifest lists no TEI. Limits follow
    {!Taposaur_document.Tei.body_text}. *)

type summary = {
  rows : int; held : int; texts : int; errors : int; characters : int;
}
val summary_jsont : summary Jsont.t

val refresh : ?catalogue:_ Taposaur_catalog.t -> ?evidence:evidence ->
  Taposaur_review.observation -> Taposaur_review.observation
(** [refresh ~catalogue ~evidence o] supersedes frozen values with the
    live catalogue's nonblank fields, holdings and presence, with evidence
    source "catalogue", fills a still-blank abstract from [evidence] with
    the evidence label as source, and then fills the other blank
    bibliographic fields from its work evidence through
    {!Taposaur_review.fill}. Without any argument the observation is
    returned unchanged. The snapshot itself is never modified. *)
val paper_of : _ Taposaur_catalog.t -> Taposaur.Uid.t -> Taposaur_paper.t option
(** [paper_of catalogue uid] is the catalogue's cached live resolution. *)

val export : ?pool:Eio.Executor_pool.t -> ?fibers:int -> ?max_bytes:int ->
  ?max_output:int -> ?catalogue:_ Taposaur_catalog.t -> ?evidence:evidence ->
  ?provenance:_ Taposaur_catalog.t ->
  archive:Taposaur_archive.t -> store:_ Taposaur_review_store.t ->
  snapshot:int -> (row -> unit) -> summary
(** [export ~archive ~store ~snapshot emit] reads the snapshot's frozen
    rows in DOI order, applies {!refresh} when [catalogue] or [evidence]
    is given, reads each catalogued member's live resolution from
    [provenance] for field provenance, looks up members found in the catalogue in the archive,
    extracts body text with up to [fibers] concurrent readers (1..256,
    default 8) and calls [emit] for each row in order. Reads run on [pool]
    when given: regular-file reads block the calling domain, so a pool of
    domains is what overlaps archive latency. Bibliographic values and
    holdings are the snapshot's unless refreshed; text and its digest are
    the archive's at export time. Archive read failures become
    [text_error] and never stop the export. *)
