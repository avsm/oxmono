(** The CORE v3 API ({{:https://core.ac.uk/}core.ac.uk}).

    Authentication is a bearer token. {!download} redirects to the
    repository holding the file, and a search result's [downloadUrl]
    names a host of its own, so {!client} scopes the token to
    {!origin} without restricting the client to it. Pace requests 1-3
    seconds apart ([Fetch.with_limits ~min_interval:2.0]). *)

type t
(** The authority to call the CORE v3 API, minted by {!client}. It is
    abstract, so {!client} is the only way to make one and another
    service's capability cannot stand in for it. *)

val origin : string
(** [origin] is ["https://api.core.ac.uk"], the URL prefix the API
    answers on. *)

val metadata_url : id:string -> string
(** [metadata_url ~id] is the work record endpoint for the CORE id
    [id]. *)

val download_url : id:string -> string
(** [download_url ~id] is the file endpoint for [id]. *)

val search_url : doi:Taposaur.Doi.t -> string
(** [search_url ~doi] is the works search for [doi], which is how a
    work is found when only the DOI is known. Its path ends in the
    slash the API redirects the bare path to. *)

val metadata : sw:Eio.Switch.t -> t -> string -> Fetch.response
(** [metadata ~sw t id] is the work record for the CORE id [id], asked
    for as JSON and decoded with {!Output.jsont}.

    A search result's id and the same number under {!metadata_url} have
    been seen to name different works, so a file is worth reading this
    for before it is fetched: it says which paper the file endpoint
    will actually serve. *)

val client : token:(unit -> string) -> _ Fetch.t -> t
(** [client ~token t] is [t] restricted to the safe methods, with
    [token ()] attached as a bearer token to each request bound for
    {!origin}. Every wrapper goes on [t], as the result is not a
    client. *)


val download : sw:Eio.Switch.t -> t -> string -> Fetch.response
(** [download ~sw t id] fetches [id]'s file, following the redirect to
    the repository holding it. A work found by {!search} is reached
    through its [downloadUrl] instead: the file is filed under an
    identifier of its own, which this endpoint does not answer for. *)

val search : sw:Eio.Switch.t -> t -> Taposaur.Doi.t -> Fetch.response
(** [search ~sw t doi] searches the index for [doi] as JSON. The
    [results] array holds the works matched, each with an [id] and a
    [downloadUrl] locating its file on [core.ac.uk], which is a
    different host from {!origin} and takes no token. A DOI the index
    does not hold is a 200 with no results rather than a 404. *)

(** {1 Reading the answers} *)

(** The fields of a {!search} answer a caller fetches a file by. *)
module Search : sig
  type result = {
    id : string option;
        (** The work's [id], which the API writes as a number here and
            takes as a string in {!val-metadata_url} and
            {!val-download_url}. *)
    download_url : string option;
        (** The work's [downloadUrl], which is where the file is. It is
            filed under an identifier of its own on a host that takes no
            token, so it is not {!val-download_url} for the [id] beside
            it. *)
  }
  (** One matched work. Both members are optional: a result naming
      neither is one no file can be fetched from, which is a caller's
      decision rather than a parse failure. *)

  type t = {
    total_hits : int;  (** The [totalHits] of the search, [0] if absent. *)
    results : result list;  (** The [results], most relevant first. *)
  }
  (** A search answer. *)

  val jsont : t Jsont.t
  (** [jsont] reads a search answer, ignoring every member of a result
      but the two above. *)
end

(** A work record, as {!metadata} returns it. *)
module Output : sig
  type t = { doi : string option; title : string option }
  (** What the record says the file is. Both are optional: a work
      deposited without either is still a work. *)

  val jsont : t Jsont.t
end
