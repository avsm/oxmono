(** A read-only Proffer browser. The executable owns sockets and databases. *)

type t
module Journal_links = Journal_links
type storage = {
  audit_file : string option;
  mirrors_file : string option;
  abstracts_file : string option;
  archive_root : string;
  catalog_file : string;
  reviews_file : string option;
  jobs_file : string option;
  journal_links_file : string option;
  working_directory : string;
  executable : string;
}
val create :
  ?publisher_lookup:(Taposaur.Doi.t -> Taposaur.Publisher_evidence.t option) ->
  ?abstract_lookup:(Taposaur.Doi.t -> Taposaur.Abstract_evidence.t option) ->
  ?audit:Taposaur_audit.read Taposaur_audit.t ->
  ?storage:storage ->
  ?reviews:Taposaur_review_store.read Taposaur_review_store.t ->
  ?jobs:Duraml.plain ->
  ?mono_clock:_ Eio.Time.Mono.t ->
  ?journal_links:Journal_links.t ->
  archive:Taposaur_archive.t ->
  catalog:Taposaur_catalog.read Taposaur_catalog.t -> unit -> t
(** [create ~archive ~catalog ()] supplies read capabilities. Optional
    [reviews] enables review pages and cached exports. Optional [jobs]
    enables the [/jobs] and [/job] inspection pages and must be a store
    opened read-only: the pages list and describe jobs and never submit,
    cancel or settle work. All databases must be opened read-only. No
    review query executes in a request handler. [mono_clock] times
    requests for the [/activity] page, which lists recent website requests
    held in memory together with new audit records and queue events;
    without it durations read as zero.
    [storage] supplies explicitly selected host paths for the debug panel.
    No credentials or environment-variable values are inspected. *)
module Docs : sig
  val corpus_columns : string list
  (** [corpus_columns] names every corpus export column the [/docs/corpus]
      page documents, in page order. *)
end
val site : t Proffer.Site.t
(** [site] serves review, overview, search, paper, event and TEI pages, plus
    manifest-listed artifact streams, and the [/docs] reference pages.
    GET routes also support HEAD.
    PDF streams support one byte range. No mutation routes are installed. *)
