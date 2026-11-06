(** Snapshot corpora: frozen review rows joined with archive body text.

    A corpus row carries the bibliographic values a snapshot froze for one
    member, the provenance of every field that has one, the holdings
    recorded at capture, and the prose of the paper's preferred
    TEI document read from the archive when the row is exported. Rows are
    flat so that a JSON Lines file converts directly to columnar formats. *)

type text = {
  file : string;  (** The TEI artifact inside the paper directory. *)
  converter : string option;
  sha256 : string;  (** Of the TEI bytes; empty when they could not be read. *)
  bytes : int;
  body : string option;  (** Body prose, or [None] when the body has none. *)
  error : string option;  (** Why no prose could be read. *)
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
  (** Field name to source for every field with provenance, sorted. The
      three columns above repeat the most used ones. *)
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
}

val row_jsont : row Jsont.t
(** Every member is present; absent values encode as [null]. *)
val encode_row : row -> string
val of_observation : snapshot:int -> ?text:text ->
  Taposaur_review.observation -> row
(** [of_observation ~snapshot o] flattens a frozen observation. Sources
    come from the observation's field evidence. *)

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

val refresh : ?catalogue:_ Taposaur_catalog.t ->
  ?abstracts:(Taposaur.Doi.t -> Taposaur.Abstract_evidence.t option) ->
  ?works:(Taposaur.Doi.t -> Taposaur.Work_evidence.t option) ->
  Taposaur_review.observation -> Taposaur_review.observation
(** [refresh ~catalogue ~abstracts ~works o] supersedes frozen values with
    the live catalogue's nonblank fields, holdings and presence, with
    evidence source "catalogue", fills a still-blank abstract from
    [abstracts] with the evidence label as source, and then fills the
    other blank bibliographic fields from [works] through
    {!Taposaur_review.fill}. Without any argument the observation is
    returned unchanged. The snapshot itself is never modified. *)

val export : ?pool:Eio.Executor_pool.t -> ?fibers:int -> ?max_bytes:int ->
  ?max_output:int -> ?catalogue:_ Taposaur_catalog.t ->
  ?abstracts:(Taposaur.Doi.t -> Taposaur.Abstract_evidence.t option) ->
  ?works:(Taposaur.Doi.t -> Taposaur.Work_evidence.t option) ->
  archive:Taposaur_archive.t -> store:_ Taposaur_review_store.t ->
  snapshot:int -> (row -> unit) -> summary
(** [export ~archive ~store ~snapshot emit] reads the snapshot's frozen
    rows in DOI order, applies {!refresh} when [catalogue], [abstracts]
    or [works] is given, looks up members found in the catalogue in the archive,
    extracts body text with up to [fibers] concurrent readers (1..256,
    default 8) and calls [emit] for each row in order. Reads run on [pool]
    when given: regular-file reads block the calling domain, so a pool of
    domains is what overlaps archive latency. Bibliographic values and
    holdings are the snapshot's unless refreshed; text and its digest are
    the archive's at export time. Archive read failures become
    [text_error] and never stop the export. *)
