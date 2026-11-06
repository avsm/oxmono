(** Exact-DOI bibliographic evidence from an external metadata service.

    A record carries the nonblank bibliographic fields one service response
    supplied for one DOI and names that response by SHA-256, so the retained
    body can be reread. The abstract travels separately as
    {!Abstract_evidence}. Evidence fills fields the archive and the review
    inputs left blank; it never replaces a value they supply. *)
type venue = {
  id : string;  (** The service's identifier, such as an OpenAlex source id. *)
  name : string option;
  issn_l : string option;
  issns : string list;
  kind : string option;  (** journal, repository, conference, ... *)
  host : string option;  (** Publisher or host organisation. *)
}
val venue_jsont : venue Jsont.t
type t = {
  doi : string;  (** Canonical DOI the response matched. *)
  bibliography : Bibliography.t;
  (** Nonblank fields only; [abstract] is always [None]. *)
  venue : venue option;  (** The work's primary source, when identified. *)
  provider : string;  (** Service identity, such as [openalex]. *)
  endpoint : string;  (** Service base URL. *)
  sha256 : string;  (** Digest of the retained response body. *)
  retrieved : float;  (** Unix time of retrieval. *)
}
val jsont : t Jsont.t
val label : t -> string
(** [label e] names the provider and response digest for provenance. *)
val fields : t -> string list
(** [fields e] names the bibliographic fields [e] supplies, using the
    field names of {!Bibliography.jsont}, in that order. *)
