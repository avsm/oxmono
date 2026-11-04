(** Exact-DOI abstract evidence from an external metadata service.

    A record names the service response it was decoded from by SHA-256, so
    the retained body can be reread. It never replaces an abstract the
    archive itself provides; see {!Taposaur_paper.with_abstract}. *)
type t = {
  doi : string;  (** Canonical DOI the response matched. *)
  abstract : string;  (** Nonblank abstract text, reconstructed if needed. *)
  provider : string;  (** Service identity, such as [openalex]. *)
  endpoint : string;  (** Service base URL. *)
  sha256 : string;  (** Digest of the retained response body. *)
  retrieved : float;  (** Unix time of retrieval. *)
}
val jsont : t Jsont.t
val label : t -> string
(** [label e] names the provider and response digest for provenance. *)
