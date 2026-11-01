(** A read-only Proffer browser. The executable owns sockets and databases. *)

type t
module Journal_links = Journal_links
type storage = {
  audit_file : string option;
  mirrors_file : string option;
  archive_root : string;
  catalog_file : string;
  reviews_file : string option;
  journal_links_file : string option;
  working_directory : string;
  executable : string;
}
val create :
  ?publisher_lookup:(Taposaur.Doi.t -> Taposaur.Publisher_evidence.t option) ->
  ?audit:Taposaur_audit.read Taposaur_audit.t ->
  ?storage:storage ->
  ?reviews:Taposaur_review_store.read Taposaur_review_store.t ->
  ?journal_links:Journal_links.t ->
  archive:Taposaur_archive.t ->
  catalog:Taposaur_catalog.read Taposaur_catalog.t -> unit -> t
(** [create ~archive ~catalog ()] supplies read capabilities. Optional
    [reviews] enables review pages and cached exports. Both databases must
    be opened read-only. No review query executes in a request handler.
    [storage] supplies explicitly selected host paths for the debug panel.
    No credentials or environment-variable values are inspected. *)
val site : t Proffer.Site.t
(** [site] serves review, overview, search, paper, event and TEI pages, plus
    manifest-listed artifact streams. GET routes also support HEAD.
    PDF streams support one byte range. No mutation routes are installed. *)
