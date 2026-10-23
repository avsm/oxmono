(** A read-only Proffer browser. The executable owns sockets and databases. *)

type t
val create :
  archive:Taposaur_archive.t ->
  catalog:Taposaur_catalog.read Taposaur_catalog.t -> t
(** [create ~archive ~catalog] supplies a browser with its read capabilities.
    Open the catalogue with [Taposaur_catalog.open_read]. *)
val site : t Proffer.Site.t
(** [site] serves overview, search, paper, event and TEI pages, plus
    manifest-listed artifact streams. GET routes also support HEAD.
    PDF streams support one byte range. No mutation routes are installed. *)
